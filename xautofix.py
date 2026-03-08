#!/usr/bin/env python3
"""Compile-driven first-pass Fortran autofixer.

Workflow:
1. Compile source(s).
2. Parse compiler diagnostics.
3. Apply one small plausible fix.
4. Recompile, repeat.

Current conservative fixes:
- undeclared symbol with implicit none -> insert a scalar declaration
- `if var then` -> `if (var) then`
"""

from __future__ import annotations

import argparse
import difflib
import re
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Optional, Tuple

import xformat_mismatch as xfmm


LOC_RE = re.compile(r"^(.*?):(\d+):(\d+):")
UNDECL_RE = re.compile(r"Error:\s+Symbol '([A-Za-z_][A-Za-z0-9_]*)'.*has no IMPLICIT type")
FUNC_IMPLICIT_RE = re.compile(r"Error:\s+Function '([A-Za-z_][A-Za-z0-9_]*)'.*has no IMPLICIT type")
RUNTIME_LOC_RE = re.compile(r"^\s*At line\s+(\d+)\s+of file\s+(.+?)\s+\(unit\s*=", re.IGNORECASE | re.MULTILINE)
RUNTIME_FMT_MISMATCH_RE = re.compile(
    r"Fortran runtime error:\s+Expected\s+([A-Za-z]+)(?:\s+or\s+[A-Za-z]+)?\s+for item\s+(\d+)\s+in formatted transfer,\s+got\s+([A-Za-z]+)",
    re.IGNORECASE,
)
DECL_ALLOC_RE = re.compile(
    r"^\s*(?:integer|real|logical|character|complex|type\s*\([^)]*\)|class\s*\([^)]*\))\b.*\ballocatable\b.*::\s*(.*)$",
    re.IGNORECASE,
)
ALLOCATE_STMT_RE = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)
DEALLOCATE_STMT_RE = re.compile(r"^\s*deallocate\s*\((.*)\)\s*$", re.IGNORECASE)
IF_GUARDED_DEALLOC_RE = re.compile(
    r"^\s*if\s*\(\s*allocated\s*\([^)]*\)\s*\)\s*deallocate\s*\(",
    re.IGNORECASE,
)
DO_START_RE = re.compile(r"^\s*do\b", re.IGNORECASE)
END_DO_RE = re.compile(r"^\s*end\s*do\b|^\s*enddo\b", re.IGNORECASE)
DO_ITER_RE = re.compile(r"^\s*do\b(?:\s+\d+)?\s*([a-z_][a-z0-9_]*)\s*=", re.IGNORECASE)
SUBSCRIPTED_ARRAY_CTOR_RE = re.compile(
    r"^\s*([a-z_][a-z0-9_]*(?:\s*\([^)]*\))?)\s*=\s*(\[[^]]+\])\s*\(([^()]*)\)\s*(?:!\s*(.*))?$",
    re.IGNORECASE,
)
NUM_REAL_LIT_RE = re.compile(
    r"^[+-]?(?:\d+\.\d*|\.\d+|\d+[eEdD][+-]?\d+|\d+\.\d*[eEdD][+-]?\d+)(?:_([a-z0-9_]+))?$",
    re.IGNORECASE,
)
NUM_INT_LIT_RE = re.compile(r"^[+-]?\d+(?:_([a-z0-9_]+))?$", re.IGNORECASE)
CHAR_LIT_RE = re.compile(r"^(['\"])(.*)\1$", re.DOTALL)
PRINT_MISSING_COMMA_RE = re.compile(r'^(\s*print\s*"[^"]*")\s+([^,].*?)\s*(?:!\s*(.*))?$', re.IGNORECASE)
IF_NO_THEN_RE = re.compile(r"^(\s*)if\s*\((.+)\)\s*(?:!\s*(.*))?$", re.IGNORECASE)
PROC_START_RE = re.compile(
    r"^\s*(?:pure\s+|elemental\s+|recursive\s+|impure\s+)*(subroutine|function)\s+[a-z_][a-z0-9_]*\b",
    re.IGNORECASE,
)


def _q(path: Path) -> str:
    return f'"{path}"'


def _run_compile(cmd: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        cmd,
        shell=True,
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
    )


def _emit_cp_output(cp: subprocess.CompletedProcess[str], prefix: str = "") -> None:
    def _safe_emit(txt: str) -> None:
        if not txt:
            return
        s = txt.rstrip()
        enc = sys.stdout.encoding or "utf-8"
        safe = s.encode(enc, errors="replace").decode(enc, errors="replace")
        print(safe)

    if cp.stdout:
        if prefix:
            print(f"{prefix}stdout:")
        _safe_emit(cp.stdout)
    if cp.stderr:
        if prefix:
            print(f"{prefix}stderr:")
        _safe_emit(cp.stderr)


def _safe_print_text(text: str) -> None:
    if not text:
        return
    s = text.rstrip()
    enc = sys.stdout.encoding or "utf-8"
    safe = s.encode(enc, errors="replace").decode(enc, errors="replace")
    print(safe)


def _build_and_run_program(srcs: list[Path], tag: str, tee_both: bool = False) -> int:
    exe = Path(tempfile.gettempdir()) / f"{tag}_xautofix_final.exe"
    srcs_str = " ".join(_q(s) for s in srcs)
    bcmd = f"gfortran {srcs_str} -o {_q(exe)}"
    print(f"Final build: {bcmd}")
    cpb = _run_compile(bcmd)
    if tee_both:
        _emit_cp_output(cpb, prefix="Final build ")
    if cpb.returncode != 0:
        print("Final build: FAIL")
        err = cpb.stderr or cpb.stdout or ""
        if err.strip():
            _safe_print_text(err)
        return cpb.returncode or 1
    print(f"Final run: {exe}")
    cpr = subprocess.run(
        [str(exe)],
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
    )
    if tee_both:
        _emit_cp_output(cpr, prefix="Final run ")
    else:
        if cpr.stdout:
            print(cpr.stdout.rstrip())
        if cpr.stderr:
            print(cpr.stderr.rstrip())
    if cpr.returncode == 0:
        print("Final run: PASS")
    else:
        print(f"Final run: FAIL (exit {cpr.returncode})")
    return cpr.returncode


def _read_lines(path: Path) -> list[str]:
    return path.read_text(encoding="utf-8", errors="replace").splitlines()


def _print_unified_diff(path: Path, before: list[str], after: list[str]) -> None:
    diff = list(
        difflib.unified_diff(
            before,
            after,
            fromfile=f"{path} (before)",
            tofile=f"{path} (after)",
            lineterm="",
        )
    )
    if not diff:
        return
    print("Diff:")
    for ln in diff:
        print(ln)


def _split_top_level_commas(text: str) -> list[str]:
    out: list[str] = []
    cur: list[str] = []
    depth = 0
    in_single = False
    in_double = False
    for ch in text:
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def _base_ident(expr: str) -> str:
    m = re.match(r"^\s*([a-z_][a-z0-9_]*)", expr, re.IGNORECASE)
    return m.group(1).lower() if m else ""


def _declared_allocatables(line: str) -> set[str]:
    m = DECL_ALLOC_RE.match(line)
    if not m:
        return set()
    rhs = m.group(1).strip()
    names: set[str] = set()
    for chunk in _split_top_level_commas(rhs):
        c = chunk.strip()
        if "=" in c and "=>" not in c:
            c = c.split("=", 1)[0].strip()
        nm = _base_ident(c)
        if nm:
            names.add(nm)
    return names


def _parse_allocate_targets(line: str) -> list[str]:
    m = ALLOCATE_STMT_RE.match(line)
    if not m:
        return []
    out: list[str] = []
    for chunk in _split_top_level_commas(m.group(1)):
        s = chunk.strip()
        if re.match(r"^[a-z_][a-z0-9_]*\s*=", s, re.IGNORECASE):
            continue
        nm = _base_ident(s)
        if nm:
            out.append(nm)
    return out


def _parse_deallocate_targets(line: str) -> list[str]:
    m = DEALLOCATE_STMT_RE.match(line)
    if not m:
        return []
    out: list[str] = []
    for chunk in _split_top_level_commas(m.group(1)):
        s = chunk.strip()
        if re.match(r"^[a-z_][a-z0-9_]*\s*=", s, re.IGNORECASE):
            continue
        nm = _base_ident(s)
        if nm:
            out.append(nm)
    return out


def _fix_unallocated_deallocate_guards(path: Path, drop_deallocate: bool = False) -> int:
    """Guard definitely unsafe DEALLOCATE on allocatables."""
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    drop_marker = "__XAUTOFIX_DROP_DEALLOC_LINE__"
    allocatables: set[str] = set()
    allocated_now: set[str] = set()
    edits = 0

    for i, old in enumerate(lines):
        code = old.split("!", 1)[0]
        low = code.strip().lower()
        if not low:
            continue
        for nm in _declared_allocatables(code):
            allocatables.add(nm)
        for nm in _parse_allocate_targets(code):
            if nm in allocatables:
                allocated_now.add(nm)

        if IF_GUARDED_DEALLOC_RE.match(code):
            guarded = re.findall(r"allocated\s*\(\s*([a-z_][a-z0-9_]*)\s*\)", code, re.IGNORECASE)
            for g in guarded:
                allocated_now.discard(g.lower())
            continue

        dnames = _parse_deallocate_targets(code)
        if not dnames:
            continue
        unsafe = [nm for nm in dnames if nm in allocatables and nm not in allocated_now]
        if unsafe:
            if drop_deallocate:
                # Keep any trailing comment as a comment-only line for traceability.
                if "!" in old:
                    indent = re.match(r"^\s*", old).group(0)
                    comment = old[old.find("!") :]
                    lines[i] = f"{indent}{comment}"
                else:
                    lines[i] = drop_marker
            else:
                indent = re.match(r"^\s*", old).group(0)
                comment = ""
                if "!" in old:
                    comment = old[old.find("!") :]
                guards = [f"if (allocated({nm})) deallocate({nm})" for nm in dnames]
                lines[i] = f"{indent}{'; '.join(guards)}{(' ' + comment) if comment else ''}"
            edits += 1
        for nm in dnames:
            allocated_now.discard(nm)

    if edits:
        if drop_deallocate:
            lines = [ln for ln in lines if ln != drop_marker]
        path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return edits


def _fix_subscripted_array_constructor(path: Path, line_no: int) -> bool:
    """Rewrite invalid `x = [..](idx)` into an ASSOCIATE block."""
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no < 1 or line_no > len(lines):
        return False
    raw = lines[line_no - 1]
    m = SUBSCRIPTED_ARRAY_CTOR_RE.match(raw)
    if not m:
        return False
    lhs = m.group(1).strip()
    ctor = m.group(2).strip()
    idx = m.group(3).strip()
    comment = m.group(4)
    indent = raw[: len(raw) - len(raw.lstrip(" "))]
    out = [
        f"{indent}associate(xautofix_tmp => {ctor})",
        f"{indent}   {lhs} = xautofix_tmp({idx})",
        f"{indent}end associate",
    ]
    if comment:
        out[0] = f"{out[0]} ! {comment}"
    lines[line_no - 1 : line_no] = out
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _split_code_comment(line: str) -> tuple[str, str]:
    i = line.find("!")
    if i < 0:
        return line, ""
    return line[:i], line[i:]


def _rewrite_mixed_numeric_items(items: list[str]) -> list[str] | None:
    kinds: list[str] = []
    parsed: list[tuple[str, str, str]] = []  # (kind, token, suffix)
    for it in items:
        tok = it.strip()
        mr = NUM_REAL_LIT_RE.match(tok)
        if mr:
            suf = mr.group(1) or ""
            parsed.append(("real", tok, suf))
            if suf:
                kinds.append(suf)
            continue
        mi = NUM_INT_LIT_RE.match(tok)
        if mi:
            suf = mi.group(1) or ""
            parsed.append(("int", tok, suf))
            if suf:
                kinds.append(suf)
            continue
        return None
    has_real = any(k == "real" for k, _, _ in parsed)
    has_int = any(k == "int" for k, _, _ in parsed)
    if not (has_real and has_int):
        return None
    chosen = kinds[0] if kinds else "dp"
    out: list[str] = []
    for k, tok, _ in parsed:
        t = tok
        if k == "int":
            m = re.match(r"^([+-]?\d+)", t)
            if not m:
                return None
            base = m.group(1)
            t = f"{base}.0_{chosen}"
        else:
            if "_" not in t:
                t = f"{t}_{chosen}"
        out.append(t)
    return out


def _char_lit_len(tok: str) -> int | None:
    m = CHAR_LIT_RE.match(tok.strip())
    if not m:
        return None
    q = m.group(1)
    s = m.group(2)
    esc = q * 2
    return len(s.replace(esc, q))


def _fix_mixed_type_array_constructor(path: Path, line_no_1: int) -> tuple[bool, str]:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False, ""
    old = lines[line_no_1 - 1]
    code, comment = _split_code_comment(old)
    m = re.search(r"\[([^\]]*)\]", code)
    if not m:
        return False, ""
    inner = m.group(1).strip()
    if not inner:
        return False, ""
    items = _split_top_level_commas(inner)
    if len(items) < 2:
        return False, ""
    nums = _rewrite_mixed_numeric_items(items)
    if nums is not None:
        rep = "[" + ", ".join(nums) + "]"
        new_code = code[: m.start()] + rep + code[m.end() :]
        if new_code != code:
            lines[line_no_1 - 1] = new_code + comment
            path.write_text("\n".join(lines) + "\n", encoding="utf-8")
            return True, "Fix: homogenized mixed int/real array constructor to real literals."
        return False, ""
    char_lens = [_char_lit_len(it) for it in items]
    if all(v is not None for v in char_lens):
        mx = max(char_lens) if char_lens else 0
        mn = min(char_lens) if char_lens else 0
        if mx != mn and "::" not in inner:
            rep = "[character(len=" + str(mx) + ") :: " + ", ".join(it.strip() for it in items) + "]"
            new_code = code[: m.start()] + rep + code[m.end() :]
            if new_code != code:
                lines[line_no_1 - 1] = new_code + comment
                path.write_text("\n".join(lines) + "\n", encoding="utf-8")
                return True, "Fix: added CHARACTER(len=...) type-spec to mixed-length character constructor."
    return False, ""


def _infer_decl_typespec_for_name(lines: list[str], line_no_1: int, name: str) -> str | None:
    nm = name.lower()
    for ln in lines[: max(0, line_no_1 - 1)]:
        core = ln.split("!", 1)[0]
        if "::" not in core:
            continue
        left, right = core.split("::", 1)
        l = left.strip()
        m = re.match(r"^(integer|real|logical|character|complex)(\s*\([^)]*\))?", l, re.IGNORECASE)
        if not m:
            continue
        tname = m.group(1)
        tmod = m.group(2) or ""
        decl_items = _split_top_level_commas(right)
        for d in decl_items:
            b = d.split("=", 1)[0].strip()
            mv = re.match(r"^([a-z_][a-z0-9_]*)", b, re.IGNORECASE)
            if mv and mv.group(1).lower() == nm:
                return f"{tname}{tmod}"
    return None


def _fix_empty_array_constructor(path: Path, line_no_1: int) -> tuple[bool, str | None]:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False, None
    old = lines[line_no_1 - 1]
    code, comment = _split_code_comment(old)
    m = re.match(r"^(\s*)([a-z_][a-z0-9_]*(?:\s*\([^)]*\))?)\s*=\s*\[\s*\]\s*$", code, re.IGNORECASE)
    if not m:
        return False, None
    lhs = m.group(2).strip()
    base = _base_ident(lhs)
    ts = _infer_decl_typespec_for_name(lines, line_no_1, base)
    if not ts:
        return False, (
            f"Warning: could not infer LHS type for empty constructor at {path}:{line_no_1}; "
            "left `x = []` unchanged."
        )
    lines[line_no_1 - 1] = f"{m.group(1)}{lhs} = [{ts} ::]{comment}"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True, "Fix: rewrote empty constructor using inferred LHS type."


def _fix_missing_closers(path: Path, line_no_1: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    old = lines[line_no_1 - 1]
    code, comment = _split_code_comment(old)
    stack: list[str] = []
    in_s = False
    in_d = False
    for ch in code:
        if ch == "'" and not in_d:
            in_s = not in_s
            continue
        if ch == '"' and not in_s:
            in_d = not in_d
            continue
        if in_s or in_d:
            continue
        if ch in "([":
            stack.append(ch)
        elif ch == ")" and stack and stack[-1] == "(":
            stack.pop()
        elif ch == "]" and stack and stack[-1] == "[":
            stack.pop()
    if not stack:
        return False
    closers = "".join(")" if c == "(" else "]" for c in reversed(stack))
    lines[line_no_1 - 1] = code.rstrip() + closers + comment
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _fix_missing_contains(path: Path, line_no_1: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    cur = lines[line_no_1 - 1].split("!", 1)[0]
    if not PROC_START_RE.match(cur):
        return False
    scope_idx = -1
    scope_kind = ""
    for i in range(line_no_1 - 1, -1, -1):
        s = lines[i].split("!", 1)[0].strip().lower()
        if re.match(r"^module\s+[a-z_][a-z0-9_]*\b", s) and not s.startswith("module procedure"):
            scope_idx = i
            scope_kind = "module"
            break
        if re.match(r"^program\s+[a-z_][a-z0-9_]*\b", s):
            scope_idx = i
            scope_kind = "program"
            break
    if scope_idx < 0:
        return False
    for j in range(scope_idx + 1, line_no_1 - 1):
        sj = lines[j].split("!", 1)[0].strip().lower()
        if sj == "contains":
            return False
        if scope_kind == "module" and sj.startswith("end module"):
            return False
        if scope_kind == "program" and sj.startswith("end program"):
            return False
    indent = re.match(r"^\s*", lines[scope_idx]).group(0)
    lines.insert(line_no_1 - 1, f"{indent}contains")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _fix_use_after_implicit_none(path: Path, line_no_1: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    use_idx = -1
    for i in range(max(0, line_no_1 - 4), min(len(lines), line_no_1 + 1)):
        if re.match(r"^\s*use\b", lines[i], re.IGNORECASE):
            use_idx = i
            break
    if use_idx < 0:
        return False
    imp_idx = -1
    for i in range(use_idx - 1, -1, -1):
        if re.match(r"^\s*implicit\s+none\b", lines[i], re.IGNORECASE):
            imp_idx = i
            break
        s = lines[i].split("!", 1)[0].strip().lower()
        if re.match(r"^(program|module|subroutine|function)\b", s):
            break
    if imp_idx < 0 or imp_idx > use_idx:
        return False
    last_use = use_idx
    for i in range(use_idx + 1, len(lines)):
        s = lines[i].split("!", 1)[0].strip().lower()
        if re.match(r"^use\b", s):
            last_use = i
            continue
        if not s:
            continue
        break
    imp_line = lines.pop(imp_idx)
    if imp_idx < last_use:
        last_use -= 1
    lines.insert(last_use + 1, imp_line)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _fix_print_missing_comma(path: Path, line_no_1: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    old = lines[line_no_1 - 1]
    m = PRINT_MISSING_COMMA_RE.match(old)
    if not m:
        return False
    cmt = f" ! {m.group(3)}" if m.group(3) else ""
    lines[line_no_1 - 1] = f"{m.group(1)}, {m.group(2).strip()}{cmt}"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _fix_if_missing_then(path: Path, line_no_1: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    old = lines[line_no_1 - 1]
    if re.search(r"\bthen\b", old, re.IGNORECASE):
        return False
    m = IF_NO_THEN_RE.match(old)
    if not m:
        return False
    # Require a block IF context nearby.
    has_end_if = any(re.match(r"^\s*end\s*if\b", lines[k], re.IGNORECASE) for k in range(line_no_1, min(len(lines), line_no_1 + 25)))
    if not has_end_if:
        return False
    cmt = f" ! {m.group(3)}" if m.group(3) else ""
    lines[line_no_1 - 1] = f"{m.group(1)}if ({m.group(2).strip()}) then{cmt}"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _fix_missing_end_associate(path: Path) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    open_n = 0
    close_n = 0
    for ln in lines:
        s = ln.split("!", 1)[0].strip().lower()
        if re.match(r"^associate\s*\(", s):
            open_n += 1
        elif re.match(r"^end\s+associate\b", s):
            close_n += 1
    if open_n <= close_n:
        return False
    miss = open_n - close_n
    insert_at = len(lines)
    for i, ln in enumerate(lines):
        s = ln.split("!", 1)[0].strip().lower()
        if re.match(r"^end\s+(program|module|subroutine|function)\b|^end$", s):
            insert_at = i
            break
    indent = re.match(r"^\s*", lines[insert_at - 1] if insert_at > 0 else "").group(0)
    for _ in range(miss):
        lines.insert(insert_at, f"{indent}end associate")
        insert_at += 1
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _line_has_guard_for_var(line: str, var: str) -> bool:
    code = line.split("!", 1)[0]
    return bool(
        re.match(
            rf"^\s*if\s*\(\s*allocated\s*\(\s*{re.escape(var)}\s*\)\s*\)\s*deallocate\s*\(\s*{re.escape(var)}\s*\)\s*$",
            code,
            re.IGNORECASE,
        )
    )


def _fix_allocate_inside_loop_with_guard(path: Path) -> int:
    """Insert guarded DEALLOCATE before ALLOCATE on allocatables inside loops."""
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    allocatables: set[str] = set()
    out: list[str] = []
    edits = 0
    loop_depth = 0

    for old in lines:
        code = old.split("!", 1)[0]
        low = code.strip().lower()
        if not low:
            out.append(old)
            continue

        for nm in _declared_allocatables(code):
            allocatables.add(nm)

        if END_DO_RE.match(low):
            loop_depth = max(0, loop_depth - 1)
            out.append(old)
            continue

        alloc_targets = _parse_allocate_targets(code)
        if loop_depth > 0 and alloc_targets:
            first_target = alloc_targets[0]
            if first_target in allocatables:
                prev_nonblank = ""
                for ln in reversed(out):
                    if ln.strip():
                        prev_nonblank = ln
                        break
                if not _line_has_guard_for_var(prev_nonblank, first_target):
                    indent = re.match(r"^\s*", old).group(0)
                    out.append(f"{indent}if (allocated({first_target})) deallocate({first_target})")
                    edits += 1

        out.append(old)
        if DO_START_RE.match(low):
            loop_depth += 1

    if edits:
        path.write_text("\n".join(out) + "\n", encoding="utf-8")
    return edits


def _prev_nonblank_idx(lines: list[str], i: int) -> int:
    j = i - 1
    while j >= 0:
        if lines[j].strip():
            return j
        j -= 1
    return -1


def _next_nonblank_idx(lines: list[str], i: int, hi: int) -> int:
    j = i
    while j <= hi:
        if lines[j].strip():
            return j
        j += 1
    return -1


def _allocate_single_target(line: str) -> tuple[str, str] | None:
    m = ALLOCATE_STMT_RE.match(line.split("!", 1)[0])
    if not m:
        return None
    objs: list[str] = []
    for chunk in _split_top_level_commas(m.group(1)):
        s = chunk.strip()
        if not s:
            continue
        if re.match(r"^[a-z_][a-z0-9_]*\s*=", s, re.IGNORECASE):
            continue
        objs.append(s)
    if len(objs) != 1:
        return None
    obj = objs[0]
    mm = re.match(r"^\s*([a-z_][a-z0-9_]*)\s*\((.*)\)\s*$", obj, re.IGNORECASE)
    if not mm:
        return None
    return mm.group(1).lower(), mm.group(2).strip()


def _hoist_loop_allocate(path: Path, drop_deallocate: bool = False) -> int:
    """Hoist simple loop-local allocate patterns out of DO loops.

    Pattern (conservative):
      do i = ...
         [if (allocated(x)) deallocate(x)]
         allocate(x(...))
         ...
      end do

    Rewrites to:
      [if (allocated(x)) deallocate(x)]
      allocate(x(...))
      do i = ...
         ...
      end do
    """
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    edits = 0
    while True:
        allocatables: set[str] = set()
        stack: list[tuple[int, str]] = []
        found = None
        for i, old in enumerate(lines):
            code = old.split("!", 1)[0]
            low = code.strip().lower()
            if not low:
                continue
            for nm in _declared_allocatables(code):
                allocatables.add(nm)
            mdo = DO_ITER_RE.match(low)
            if mdo:
                stack.append((i, mdo.group(1).lower()))
                continue
            if END_DO_RE.match(low) and stack:
                do_start, loop_var = stack.pop()
                body_lo = do_start + 1
                body_hi = i - 1
                if body_lo > body_hi:
                    continue
                first = _next_nonblank_idx(lines, body_lo, body_hi)
                if first < 0:
                    continue
                guard_idx = -1
                alloc_idx = -1
                first_code = lines[first].split("!", 1)[0]
                d0 = _parse_deallocate_targets(first_code)
                if d0 and len(d0) == 1 and _line_has_guard_for_var(first_code, d0[0]):
                    guard_idx = first
                    alloc_idx = _next_nonblank_idx(lines, first + 1, body_hi)
                else:
                    alloc_idx = first
                if alloc_idx < 0:
                    continue
                alloc_pair = _allocate_single_target(lines[alloc_idx])
                if not alloc_pair:
                    continue
                var, spec = alloc_pair
                if var not in allocatables:
                    continue
                if re.search(rf"\b{re.escape(loop_var)}\b", spec, re.IGNORECASE):
                    continue
                bad = False
                for t in range(body_lo, body_hi + 1):
                    if t == alloc_idx or t == guard_idx:
                        continue
                    code_t = lines[t].split("!", 1)[0]
                    if var in _parse_allocate_targets(code_t) or var in _parse_deallocate_targets(code_t):
                        bad = True
                        break
                if bad:
                    continue
                found = (do_start, i, guard_idx, alloc_idx, var)
                break
        if found is None:
            break
        do_start, do_end, guard_idx, alloc_idx, var = found
        do_indent = re.match(r"^\s*", lines[do_start]).group(0)
        alloc_line = lines[alloc_idx].lstrip()
        hoist_lines: list[str] = []
        pidx = _prev_nonblank_idx(lines, do_start)
        if (not drop_deallocate) and (pidx < 0 or not _line_has_guard_for_var(lines[pidx], var)):
            hoist_lines.append(f"{do_indent}if (allocated({var})) deallocate({var})")
        hoist_lines.append(f"{do_indent}{alloc_line}")
        lines[do_start:do_start] = hoist_lines
        shift = len(hoist_lines)
        do_end += shift
        if guard_idx >= 0:
            del lines[guard_idx + shift]
            do_end -= 1
            if alloc_idx > guard_idx:
                alloc_idx -= 1
        del lines[alloc_idx + shift]
        edits += 1

    if edits:
        path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return edits


def _already_declared(lines: list[str], name: str) -> bool:
    pat = re.compile(rf"\b{name}\b", re.IGNORECASE)
    for ln in lines:
        s = ln.split("!")[0]
        if "::" in s and pat.search(s):
            return True
    return False


def _guess_decl_type(lines: list[str], line_no_1: int, name: str) -> str:
    if line_no_1 < 1 or line_no_1 > len(lines):
        return "integer"
    ln = lines[line_no_1 - 1]
    low = ln.lower()
    if ".true." in low or ".false." in low:
        return "logical"
    if re.search(rf"\b{name}\b\s*=\s*['\"]", ln, re.IGNORECASE):
        return "character(len=256)"
    if re.search(rf"\b{name}\b\s*=", ln, re.IGNORECASE):
        rhs = ln.split("=", 1)[1]
        if re.search(r"\d\.\d|[eEdD][+-]?\d+", rhs):
            return "real"
    return "integer"


def _insert_decl_near_implicit_none(path: Path, line_no_1: int, decl: str) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if _already_declared(lines, decl.split("::", 1)[1].strip()):
        return False
    idx = None
    for i in range(min(line_no_1 - 1, len(lines) - 1), -1, -1):
        if lines[i].strip().lower() == "implicit none":
            idx = i
            break
    if idx is None:
        return False
    indent = re.match(r"^\s*", lines[idx]).group(0)
    lines.insert(idx + 1, f"{indent}{decl}")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _fix_if_missing_paren(path: Path, line_no_1: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    old = lines[line_no_1 - 1]
    m = re.match(r"^(\s*)if\s+(.+?)\s+then(\s*!.*)?\s*$", old, re.IGNORECASE)
    if not m:
        return False
    cond = m.group(2).strip()
    if cond.startswith("(") and cond.endswith(")"):
        return False
    cmt = m.group(3) or ""
    lines[line_no_1 - 1] = f"{m.group(1)}if ({cond}) then{cmt}"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _fix_bare_call_stmt(path: Path, line_no_1: int, name: str) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    old = lines[line_no_1 - 1]
    m = re.match(r"^(\s*)([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)\s*(?:!.*)?$", old)
    if not m:
        return False
    if m.group(2).lower() != name.lower():
        return False
    if re.match(r"^\s*call\b", old, re.IGNORECASE):
        return False
    lines[line_no_1 - 1] = f"{m.group(1)}call {m.group(2)}({m.group(3)})"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _fix_do_while_paren(path: Path, line_no_1: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    old = lines[line_no_1 - 1]
    m = re.match(r"^(\s*)do\s+while\s+(.+?)\s*(?:!.*)?$", old, re.IGNORECASE)
    if not m:
        return False
    cond = m.group(2).strip()
    if cond.startswith("(") and cond.endswith(")"):
        return False
    lines[line_no_1 - 1] = f"{m.group(1)}do while ({cond})"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _fix_percent_mod(path: Path, line_no_1: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    old = lines[line_no_1 - 1]
    if "%%" not in old:
        return False
    # Conservative single replacement per pass: a %% b -> mod(a, b)
    new = re.sub(r"(\S+)\s*%%\s*(\S+)", r"mod(\1, \2)", old, count=1)
    if new == old:
        return False
    lines[line_no_1 - 1] = new
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _strip_comment(ln: str) -> str:
    i = ln.find("!")
    return (ln if i < 0 else ln[:i]).strip()


def _infer_missing_end_line(path: Path) -> str:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    stack: list[tuple[str, str | None]] = []

    for raw in lines:
        s = _strip_comment(raw).lower()
        if not s:
            continue

        # Closers first.
        if re.match(r"^end\s+if\b", s):
            if stack and stack[-1][0] == "if":
                stack.pop()
            continue
        if re.match(r"^end\s+do\b|^enddo\b", s):
            if stack and stack[-1][0] == "do":
                stack.pop()
            continue
        if re.match(r"^end\s+select\b", s):
            if stack and stack[-1][0] == "select":
                stack.pop()
            continue
        if re.match(r"^end\s+block\b", s):
            if stack and stack[-1][0] == "block":
                stack.pop()
            continue
        if re.match(r"^end\s+type\b", s):
            if stack and stack[-1][0] == "type":
                stack.pop()
            continue
        if re.match(r"^end\s+interface\b", s):
            if stack and stack[-1][0] == "interface":
                stack.pop()
            continue
        if re.match(r"^end\s+associate\b", s):
            if stack and stack[-1][0] == "associate":
                stack.pop()
            continue
        if re.match(r"^end\s+function\b", s):
            if stack and stack[-1][0] == "function":
                stack.pop()
            continue
        if re.match(r"^end\s+subroutine\b", s):
            if stack and stack[-1][0] == "subroutine":
                stack.pop()
            continue
        if re.match(r"^end\s+module\b", s):
            if stack and stack[-1][0] == "module":
                stack.pop()
            continue
        if re.match(r"^end\s+program\b", s):
            if stack and stack[-1][0] == "program":
                stack.pop()
            continue
        if s == "end":
            if stack:
                stack.pop()
            continue

        # Openers.
        m = re.match(r"^program\s+([a-z_]\w*)\b", s)
        if m:
            stack.append(("program", m.group(1)))
            continue
        m = re.match(r"^module\s+([a-z_]\w*)\b", s)
        if m and not s.startswith("module procedure"):
            stack.append(("module", m.group(1)))
            continue
        m = re.match(r"^(?:pure\s+|elemental\s+|recursive\s+|impure\s+)*subroutine\s+([a-z_]\w*)\b", s)
        if m:
            stack.append(("subroutine", m.group(1)))
            continue
        m = re.match(r"^(?:pure\s+|elemental\s+|recursive\s+|impure\s+)*function\s+([a-z_]\w*)\b", s)
        if m:
            stack.append(("function", m.group(1)))
            continue
        if re.match(r"^if\s*\(.*\)\s*then\b", s):
            stack.append(("if", None))
            continue
        if re.match(r"^do\b", s):
            stack.append(("do", None))
            continue
        if re.match(r"^select\s+case\s*\(", s):
            stack.append(("select", None))
            continue
        if re.match(r"^block\b", s):
            stack.append(("block", None))
            continue
        if re.match(r"^interface\b", s):
            stack.append(("interface", None))
            continue
        if re.match(r"^associate\s*\(", s):
            stack.append(("associate", None))
            continue
        if re.match(r"^type\s*::\s*[a-z_]\w*\b", s):
            stack.append(("type", None))
            continue

    if not stack:
        return "end"
    k, nm = stack[-1]
    if k in {"program", "module", "subroutine", "function"} and nm:
        return f"end {k} {nm}"
    if k == "if":
        return "end if"
    if k == "do":
        return "end do"
    if k == "select":
        return "end select"
    if k == "block":
        return "end block"
    if k == "type":
        return "end type"
    if k == "interface":
        return "end interface"
    if k == "associate":
        return "end associate"
    return "end"


def _fix_unexpected_eof(path: Path) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    end_ln = _infer_missing_end_line(path)
    if lines and lines[-1].strip().lower() == end_ln.lower():
        return False
    lines.append(end_ln)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _replacement_descriptor_for_got(got: str) -> str | None:
    g = got.strip().upper()
    if g in {"INTEGER", "UNSIGNED"}:
        return "i0"
    if g == "REAL":
        return "g0.6"
    if g == "LOGICAL":
        return "l1"
    if g == "CHARACTER":
        return "a"
    if g == "COMPLEX":
        return "2g0.6"
    return None


def _expected_descriptor_pattern(expected: str) -> re.Pattern[str] | None:
    e = expected.strip().upper()
    if e == "REAL":
        return re.compile(r"\b[fegd]\s*\d+(?:\.\d+)?(?:e\d+)?\b", re.IGNORECASE)
    if e == "INTEGER":
        return re.compile(r"\bi\s*\d+\b", re.IGNORECASE)
    if e == "LOGICAL":
        return re.compile(r"\bl\s*\d+\b", re.IGNORECASE)
    if e == "CHARACTER":
        return re.compile(r"\ba\b", re.IGNORECASE)
    if e == "COMPLEX":
        return re.compile(r"\b(?:[fegd]\s*\d+(?:\.\d+)?(?:e\d+)?|2[fegd]\s*\d+(?:\.\d+)?)\b", re.IGNORECASE)
    return None


def _fix_runtime_format_mismatch(path: Path, line_no_1: int, expected: str, got: str, item: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    repl = _replacement_descriptor_for_got(got)
    pat = _expected_descriptor_pattern(expected)
    if repl is None or pat is None:
        return False

    # Match all common scalar data edit descriptors that consume one list item.
    any_data_desc = re.compile(
        r"\b(?:i\s*\d+|[fegd]\s*\d+(?:\.\d+)?(?:e\d+)?|l\s*\d+|a)\b",
        re.IGNORECASE,
    )

    # Search around the reported line for a WRITE/PRINT format descriptor line.
    lo = max(0, line_no_1 - 6)
    hi = min(len(lines), line_no_1 + 10)
    for i in range(lo, hi):
        s = lines[i]
        if ("write" not in s.lower()) and ("print" not in s.lower()) and ("&" not in s):
            continue
        matches = list(any_data_desc.finditer(s))
        if not matches:
            continue
        # Prefer the descriptor corresponding to runtime "item N" when possible.
        idx = min(max(item - 1, 0), len(matches) - 1)
        m = matches[idx]
        # If the selected descriptor class is clearly incompatible with the
        # runtime "expected" class, fall back to first matching expected token.
        tok = s[m.start():m.end()]
        if not pat.fullmatch(tok.strip()):
            exp_matches = list(pat.finditer(s))
            if exp_matches:
                m = exp_matches[0]
        lines[i] = s[:m.start()] + repl + s[m.end():]
        path.write_text("\n".join(lines) + "\n", encoding="utf-8")
        return True

    # Fallback: edit the exact reported line if it carries a matching descriptor.
    s = lines[line_no_1 - 1]
    m = pat.search(s)
    if not m:
        return False
    lines[line_no_1 - 1] = s[:m.start()] + repl + s[m.end():]
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _split_top_level_commas(s: str) -> list[str]:
    out: list[str] = []
    cur: list[str] = []
    depth = 0
    for ch in s:
        if ch == "(":
            depth += 1
        elif ch == ")" and depth > 0:
            depth -= 1
        if ch == "," and depth == 0:
            tok = "".join(cur).strip()
            if tok:
                out.append(tok)
            cur = []
        else:
            cur.append(ch)
    tok = "".join(cur).strip()
    if tok:
        out.append(tok)
    return out


def _safe_eval_int(expr: str, env: dict[str, int]) -> int | None:
    e = expr.strip().lower().replace(" ", "")
    if not re.fullmatch(r"[a-z0-9_+\-*/()]+", e):
        return None
    try:
        v = eval(e, {"__builtins__": {}}, env)
    except Exception:
        return None
    if isinstance(v, int):
        return v
    return None


def _param_int_env(lines: list[str]) -> dict[str, int]:
    env: dict[str, int] = {}
    re_par = re.compile(r"^\s*integer\s*,\s*parameter\s*::\s*(.+)$", re.IGNORECASE)
    for ln in lines:
        m = re_par.match(ln.split("!")[0])
        if not m:
            continue
        for part in _split_top_level_commas(m.group(1)):
            if "=" not in part:
                continue
            k, v = part.split("=", 1)
            name = k.strip().lower()
            iv = _safe_eval_int(v, env)
            if name and iv is not None:
                env[name] = iv
    return env


def _var_kind_and_count(lines: list[str], upto_line_1: int, name: str) -> tuple[str, int] | None:
    nlow = name.lower()
    env = _param_int_env(lines[:max(0, upto_line_1 - 1)])
    re_decl = re.compile(r"^\s*(integer|real|logical|character)\b.*::\s*(.+)$", re.IGNORECASE)
    for ln in lines[:max(0, upto_line_1 - 1)]:
        core = ln.split("!")[0]
        m = re_decl.match(core)
        if not m:
            continue
        kind = m.group(1).lower()
        rhs = m.group(2)
        for part in _split_top_level_commas(rhs):
            base = part.split("=", 1)[0].strip()
            mvar = re.match(r"^([a-z_]\w*)\s*(\(([^)]*)\))?$", base, re.IGNORECASE)
            if not mvar:
                continue
            vname = mvar.group(1).lower()
            if vname != nlow:
                continue
            dims = mvar.group(3)
            if not dims:
                return kind, 1
            # Handle simple 1-D explicit shape like (n) or (10).
            dparts = _split_top_level_commas(dims)
            if len(dparts) != 1:
                return kind, 1
            cnt = _safe_eval_int(dparts[0], env)
            if cnt is None or cnt < 1:
                return kind, 1
            return kind, cnt
    return None


def _fmt_desc_for_kind(kind: str) -> str:
    k = kind.lower()
    if k == "integer":
        return "i0"
    if k == "real":
        return "g0.6"
    if k == "logical":
        return "l1"
    if k == "character":
        return "a"
    return "g0.6"


def _fmt_part_no_trailing_space(desc: str, cnt: int) -> str:
    """Format one argument kind/count without trailing 1x at end."""
    if cnt <= 1:
        return desc
    # Example: 3 values -> 2(g0.6,1x), g0.6
    return f"{cnt - 1}({desc},1x), {desc}"


def _normalize_format_comma_spacing(fmt: str) -> str:
    # Keep formatting simple and consistent: comma followed by one space.
    s = re.sub(r"\s*,\s*", ", ", fmt)
    s = re.sub(r"\(\s+", "(", s)
    s = re.sub(r"\s+\)", ")", s)
    return s.strip()


def _fix_runtime_rebuild_print_format(path: Path, line_no_1: int) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    old = lines[line_no_1 - 1]
    m = re.match(r'^(\s*)print\s*"([^"]*)"\s*,\s*(.+?)\s*$', old, re.IGNORECASE)
    if not m:
        return False
    args_s = m.group(3).strip()
    args = _split_top_level_commas(args_s)
    if not args:
        return False
    parts: list[str] = []
    for a in args:
        if not re.fullmatch(r"[A-Za-z_]\w*", a):
            return False
        kc = _var_kind_and_count(lines, line_no_1, a)
        if kc is None:
            return False
        kind, cnt = kc
        desc = _fmt_desc_for_kind(kind)
        parts.append(_fmt_part_no_trailing_space(desc, cnt))
    new_fmt = _normalize_format_comma_spacing("(" + ", 1x, ".join(parts) + ")")
    new_line = f'{m.group(1)}print "{new_fmt}", {args_s}'
    if new_line == old:
        return False
    lines[line_no_1 - 1] = new_line
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _expr_likely_real(expr: str) -> bool:
    e = expr.strip().lower()
    if not e:
        return False
    if "/" in e and not re.fullmatch(r"[a-z_]\w*\s*/\s*[a-z_]\w*", e):
        return True
    if re.search(r"\d\.\d|[de][-+]?\d+", e):
        return True
    if "sum(" in e and "size(" in e:
        return True
    if re.search(r"\breal\s*\(", e):
        return True
    return False


def _fix_scalar_i_format_for_real_expr(path: Path) -> bool:
    """Fix `print "(i...)", <real expr>` patterns missed by strict static typing."""
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    any_data_desc = re.compile(r"\bi\s*\d+\b", re.IGNORECASE)
    changed = False
    for i, old in enumerate(lines):
        m = re.match(r'^\s*print\s*"([^"]*)"\s*,\s*(.+?)\s*$', old, re.IGNORECASE)
        if not m:
            continue
        fmt = m.group(1).strip()
        args = _split_top_level_commas(m.group(2).strip())
        if len(args) != 1:
            continue
        if not _expr_likely_real(args[0]):
            continue
        if not any_data_desc.search(fmt):
            continue
        new_fmt = any_data_desc.sub("g0.6", fmt, count=1)
        if new_fmt != fmt:
            lines[i] = old.replace(f'"{fmt}"', f'"{new_fmt}"', 1)
            changed = True
            break
    if changed:
        path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return changed


def _static_format_mismatches(path: Path) -> list[Tuple[int, str, int | None, str | None]]:
    infos, _ = xfmm.fscan.load_source_files([path])
    if not infos:
        return []
    finfo = infos[0]
    findings = []
    for u in xfmm.collect_units(finfo):
        findings.extend(xfmm.analyze_unit(u))
    findings = [f for f in findings if f.certainty == "definite"]
    findings.sort(key=lambda f: (f.line, f.detail))
    out: list[Tuple[int, str, int | None, str | None]] = []
    seen: set[Tuple[int, str, int | None, str | None]] = set()
    for f in findings:
        item_no: int | None = None
        got_type: str | None = None
        mm = re.search(r"item\s+(\d+)\s+type\s+([a-z]+)", f.detail, re.IGNORECASE)
        if mm:
            try:
                item_no = int(mm.group(1))
            except ValueError:
                item_no = None
            got_type = mm.group(2).upper()
        k = (f.line, f.detail, item_no, got_type)
        if k in seen:
            continue
        seen.add(k)
        out.append(k)
    return out


def _expected_kind_from_descriptor_token(tok: str) -> str | None:
    t = tok.strip().lower()
    if not t:
        return None
    if t.startswith("i"):
        return "INTEGER"
    if t.startswith("l"):
        return "LOGICAL"
    if t.startswith("a"):
        return "CHARACTER"
    if t.startswith("f") or t.startswith("e") or t.startswith("d") or t.startswith("g"):
        return "REAL"
    return None


def _expected_kind_at_item_in_line(line: str, item_no: int) -> str | None:
    any_data_desc = re.compile(
        r"\b(?:i\s*\d+|[fegd]\s*\d+(?:\.\d+)?(?:e\d+)?|l\s*\d+|a)\b",
        re.IGNORECASE,
    )
    matches = list(any_data_desc.finditer(line))
    if not matches:
        return None
    idx = min(max(item_no - 1, 0), len(matches) - 1)
    tok = line[matches[idx].start():matches[idx].end()]
    return _expected_kind_from_descriptor_token(tok)


def _fix_static_format_mismatch_with_xfmm(path: Path, line_no_1: int) -> bool:
    infos, _ = xfmm.fscan.load_source_files([path])
    if not infos:
        return False
    finfo = infos[0]
    units = xfmm.collect_units(finfo)
    target_unit = None
    target_stmt = None
    for u in units:
        for ln, stmt in u.body:
            if ln == line_no_1:
                target_unit = u
                target_stmt = stmt
                break
        if target_stmt is not None:
            break
    if target_unit is None or target_stmt is None:
        return False
    code = xfmm.strip_comment(target_stmt).strip()
    parsed = xfmm.parse_print_stmt(code)
    is_print = True
    if parsed is None:
        parsed = xfmm.parse_write_stmt(code)
        is_print = False
    if parsed is None:
        return False
    fmt_expr, iolist = parsed
    named_fmts = xfmm.parse_named_format_constants(target_unit)
    fmt_text = xfmm.normalize_format_expr(fmt_expr, named_fmts)
    if fmt_text is None:
        return False
    types = xfmm.build_local_types(target_unit)
    sizes = xfmm.build_local_sizes(target_unit)
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if line_no_1 < 1 or line_no_1 > len(lines):
        return False
    # Prefer declaration-driven arg grouping for simple variable lists.
    arg_names = _split_top_level_commas(iolist)
    simple = True
    fmt_parts: list[str] = []
    for a in arg_names:
        if not re.fullmatch(r"[A-Za-z_]\w*", a.strip()):
            simple = False
            break
        kc = _var_kind_and_count(lines, line_no_1, a.strip())
        if kc is None:
            simple = False
            break
        kind, cnt = kc
        desc = _fmt_desc_for_kind(kind)
        fmt_parts.append(_fmt_part_no_trailing_space(desc, cnt))
    if not simple:
        arg_kinds = xfmm.iolist_arg_kinds(iolist, types, sizes)
        if not arg_kinds:
            return False
        if any(k == "unknown" for k in arg_kinds):
            return False
        if len(arg_kinds) > 64:
            return False
        fmt_parts = []
        i = 0
        while i < len(arg_kinds):
            k = arg_kinds[i]
            j = i + 1
            while j < len(arg_kinds) and arg_kinds[j] == k:
                j += 1
            cnt = j - i
            desc = _fmt_desc_for_kind(k)
            fmt_parts.append(_fmt_part_no_trailing_space(desc, cnt))
            i = j
    new_fmt = _normalize_format_comma_spacing("(" + ", 1x, ".join(fmt_parts) + ")")

    old = lines[line_no_1 - 1]
    if is_print:
        m = re.match(r'^(\s*print\s*)"([^"]*)"(.*)$', old, re.IGNORECASE)
    else:
        m = re.match(r'^(\s*write\s*\(\s*\*\s*,\s*)"([^"]*)"(.*)$', old, re.IGNORECASE)
    if not m:
        return False
    new_line = f'{m.group(1)}"{new_fmt}"{m.group(3)}'
    if new_line == old:
        return False
    lines[line_no_1 - 1] = new_line
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _normalize_format_spacing(path: Path) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    changed_any = False
    # Conservative token set we emit/use often.
    tok_re = r"(?:i0|g0\.\d+|f0\.\d+|l1|a)"
    for i, old in enumerate(lines):
        m = re.match(r'^(\s*(?:print|write\s*\(\s*\*\s*,)\s*)"([^"]+)"(.*)$', old, re.IGNORECASE)
        if not m:
            continue
        prefix, fmt, suffix = m.group(1), m.group(2), m.group(3)
        if "1x" in fmt.lower():
            continue
        if not re.search(tok_re, fmt, re.IGNORECASE):
            continue
        new_fmt = fmt
        # (i0) -> (i0,1x)
        new_fmt = re.sub(rf"\(\s*({tok_re})\s*\)", r"(\1,1x)", new_fmt, flags=re.IGNORECASE)
        # 3i0 -> 3(i0,1x)
        new_fmt = re.sub(rf"(?<![\w\)])(\d+)\s*({tok_re})\b", r"\1(\2,1x)", new_fmt, flags=re.IGNORECASE)
        # bare token -> (token,1x)
        new_fmt = re.sub(rf"(?<![\w\)])\b({tok_re})\b", r"(\1,1x)", new_fmt, flags=re.IGNORECASE)
        if new_fmt != fmt:
            lines[i] = f'{prefix}"{new_fmt}"{suffix}'
            changed_any = True
    if changed_any:
        path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return changed_any


def _normalize_redundant_format_parens(path: Path) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    changed_any = False
    tok_re = r"(?:i0|g0\.\d+|f0\.\d+|l1|a)"
    for i, old in enumerate(lines):
        m = re.match(r'^(\s*(?:print|write\s*\(\s*\*\s*,)\s*)"([^"]+)"(.*)$', old, re.IGNORECASE)
        if not m:
            continue
        prefix, fmt, suffix = m.group(1), m.group(2), m.group(3)
        # Remove redundant singleton grouping: (g0.6,1x) -> g0.6,1x
        # Keep repeated groups such as 3(i0,1x).
        new_fmt = re.sub(
            rf"(?<!\d)\(\s*({tok_re}\s*,\s*1x)\s*\)",
            r"\1",
            fmt,
            flags=re.IGNORECASE,
        )
        new_fmt = re.sub(r"\s+", " ", new_fmt).strip()
        new_fmt = _normalize_format_comma_spacing(new_fmt)
        # Explicit Fortran format strings must keep outer parentheses.
        if new_fmt and not new_fmt.startswith("("):
            new_fmt = f"({new_fmt})"
        if new_fmt != fmt:
            lines[i] = f'{prefix}"{new_fmt}"{suffix}'
            changed_any = True
    if changed_any:
        path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return changed_any


def _parse_runtime_fmt_mismatch(log_text: str) -> dict[str, object] | None:
    txt = (log_text or "").lstrip("\ufeff")
    ml = RUNTIME_LOC_RE.search(txt)
    mm = RUNTIME_FMT_MISMATCH_RE.search(txt)
    if not ml or not mm:
        return None
    try:
        line_no = int(ml.group(1))
        item_no = int(mm.group(2))
    except ValueError:
        return None
    return {
        "kind": "runtime_fmt_mismatch",
        "file": Path(ml.group(2).strip()),
        "line": line_no,
        "expected": mm.group(1).strip(),
        "got": mm.group(3).strip(),
        "item": item_no,
    }


def _find_first_action(stderr: str, default_src: Path) -> dict[str, object] | None:
    cur_file = default_src
    cur_line = 1
    for ln in stderr.splitlines():
        mloc = LOC_RE.match(ln.strip())
        if mloc:
            cur_file = Path(mloc.group(1))
            cur_line = int(mloc.group(2))
            continue
        mu = UNDECL_RE.search(ln)
        if mu:
            name = mu.group(1)
            return {"kind": "undeclared", "file": cur_file, "line": cur_line, "name": name}
        mf = FUNC_IMPLICIT_RE.search(ln)
        if mf:
            name = mf.group(1)
            return {"kind": "func_implicit", "file": cur_file, "line": cur_line, "name": name}
        if "Error: Missing '(' in IF-expression" in ln:
            return {"kind": "if_paren", "file": cur_file, "line": cur_line}
        if "Error: Unexpected '%' for nonderived-type variable" in ln:
            return {"kind": "percent_mod", "file": cur_file, "line": cur_line}
        if "Error: Unclassifiable statement" in ln:
            return {"kind": "unclassifiable", "file": cur_file, "line": cur_line}
        if "Error: Missing ')' in statement" in ln or "Expected a right parenthesis" in ln:
            return {"kind": "missing_closer", "file": cur_file, "line": cur_line}
        if "Error: Missing THEN in IF statement" in ln:
            return {"kind": "if_missing_then", "file": cur_file, "line": cur_line}
        if "Error: Expecting END ASSOCIATE statement" in ln:
            return {"kind": "missing_end_associate", "file": cur_file, "line": cur_line}
        if "cannot follow IMPLICIT NONE" in ln and "USE statement" in ln:
            return {"kind": "implicit_after_use", "file": cur_file, "line": cur_line}
        if "Unexpected end of file" in ln:
            return {"kind": "unexpected_eof", "file": cur_file, "line": cur_line}
    return None


def _first_error_loc(stderr: str, default_src: Path) -> tuple[Path, int]:
    for ln in stderr.splitlines():
        m = LOC_RE.match(ln.strip())
        if m:
            return Path(m.group(1)), int(m.group(2))
    return default_src, 1


def _try_general_compile_fix(path: Path, line_no: int, err_text: str) -> tuple[bool, str | None]:
    changed, msg = _fix_mixed_type_array_constructor(path, line_no)
    if changed:
        return True, msg
    changed, warn = _fix_empty_array_constructor(path, line_no)
    if changed:
        return True, "Fix: rewrote empty array constructor from LHS type."
    if warn:
        return False, warn
    if _fix_subscripted_array_constructor(path, line_no):
        return True, "Fix: rewrote subscripted array constructor."
    if _fix_print_missing_comma(path, line_no):
        return True, "Fix: inserted missing comma in PRINT statement."
    if _fix_if_missing_then(path, line_no):
        return True, "Fix: inserted missing THEN in IF statement."
    if _fix_missing_contains(path, line_no):
        return True, "Fix: inserted missing CONTAINS before internal procedures."
    if _fix_use_after_implicit_none(path, line_no):
        return True, "Fix: moved IMPLICIT NONE after USE statements."
    if _fix_missing_closers(path, line_no):
        return True, "Fix: appended missing closing parenthesis/bracket."
    if ("expecting end associate" in err_text.lower()) or ("unexpected end of file" in err_text.lower()):
        if _fix_missing_end_associate(path):
            return True, "Fix: inserted missing END ASSOCIATE."
    return False, None


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Compile-driven first-pass Fortran autofixer.",
    )
    ap.add_argument("sources", nargs="+", help="Fortran source files (first file is the one autofixed)")
    ap.add_argument(
        "--compile-cmd",
        default='gfortran -c {srcs}',
        help='Compilation command template (default: "gfortran -c {srcs}"). '
        "Placeholders: {src}, {srcs}",
    )
    ap.add_argument("--max-iter", type=int, default=8, help="Maximum compile-fix iterations")
    ap.add_argument(
        "--out",
        default="temp.f90",
        help="Output path for transformed source (default: temp.f90). Ignored by --in-place.",
    )
    ap.add_argument(
        "--in-place",
        action="store_true",
        help="Modify first input source in place and create a backup.",
    )
    ap.add_argument(
        "--runtime",
        action="store_true",
        help="Runtime autofix mode: compile and run, then fix supported runtime formatted-transfer mismatches.",
    )
    ap.add_argument(
        "--compile-fix",
        action="store_true",
        help="Enable compile-error-driven autofixes (for example, invalid expression subscripting).",
    )
    ap.add_argument(
        "--run",
        action="store_true",
        help="After fixes succeed, build executable and run once.",
    )
    ap.add_argument(
        "--diff",
        action="store_true",
        help="Print unified diff for each applied edit.",
    )
    ap.add_argument(
        "--tee-orig",
        action="store_true",
        help="Print original source of the target Fortran file.",
    )
    ap.add_argument(
        "--tee",
        action="store_true",
        help="Print transformed source of the target Fortran file.",
    )
    ap.add_argument(
        "--tee-both",
        action="store_true",
        help="Print original+transformed source and both stdout/stderr from build/run steps.",
    )
    ap.add_argument(
        "--hoist-loop-allocate",
        action="store_true",
        help="Conservatively hoist simple allocatable ALLOCATE statements out of DO loops.",
    )
    ap.add_argument(
        "--drop-deallocate",
        action="store_true",
        help="When a DEALLOCATE is definitely unsafe/redundant, drop it instead of guarding with allocated(...).",
    )
    ap.add_argument(
        "--normalize-format-style",
        action="store_true",
        help="Apply format-string style normalization (spacing/parentheses). Off by default.",
    )
    args = ap.parse_args()
    tee_orig = args.tee_orig or args.tee_both
    tee_src = args.tee or args.tee_both

    def _emit_source(label: str, p: Path) -> None:
        print(f"{label} ({p}):")
        try:
            txt = p.read_text(encoding="utf-8", errors="replace")
        except Exception as ex:
            print(f"<unable to read source: {ex}>")
            return
        print(txt.rstrip("\n"))

    # Runtime mode is primarily intended to patch the target source directly.
    # Keep --out available when explicitly requested, but default to in-place.
    if args.runtime and (not args.in_place) and args.out == "temp.f90":
        args.in_place = True

    srcs = [Path(s) for s in args.sources]
    for s in srcs:
        if not s.exists():
            print(f"Missing file: {s}")
            return 1

    compile_fix_enabled = args.compile_fix or (not args.runtime)

    if args.runtime:
        tgt_idx = -1
        for i in range(len(srcs) - 1, -1, -1):
            if srcs[i].suffix.lower() in {".f90", ".f95", ".f03", ".f08", ".f", ".for"}:
                tgt_idx = i
                break
        if tgt_idx < 0:
            print("Runtime mode requires at least one Fortran source file in inputs.")
            return 1
        target = srcs[tgt_idx]
        target_index = tgt_idx
    else:
        tgt_idx = -1
        for i, s in enumerate(srcs):
            if s.suffix.lower() in {".f90", ".f95", ".f03", ".f08", ".f", ".for"}:
                tgt_idx = i
                break
        if tgt_idx < 0:
            print("No Fortran source file found in inputs.")
            return 1
        target = srcs[tgt_idx]
        target_index = tgt_idx
    if tee_orig:
        _emit_source("Original source", target)
    if args.in_place:
        stem = target.name + ".bak"
        bak = target.with_name(stem)
        k = 1
        while bak.exists():
            bak = target.with_name(f"{stem}.{k}")
            k += 1
        bak.write_text(target.read_text(encoding="utf-8", errors="replace"), encoding="utf-8")
        print(f"Backup: {bak}")
        work_target = target
    else:
        out = Path(args.out)
        out.write_text(target.read_text(encoding="utf-8", errors="replace"), encoding="utf-8")
        work_target = out
        print(f"Wrote: {work_target}")
    build_inputs = list(srcs)
    build_inputs[target_index] = work_target

    def _ret(code: int) -> int:
        if tee_src and work_target.exists():
            _emit_source("Transformed source", work_target)
        return code

    edits = 0
    before_guard = _read_lines(work_target) if args.diff else []
    guard_edits = _fix_unallocated_deallocate_guards(work_target, drop_deallocate=args.drop_deallocate)
    if guard_edits:
        if args.diff:
            _print_unified_diff(work_target, before_guard, _read_lines(work_target))
        edits += guard_edits
        if args.drop_deallocate:
            print(f"Fix: dropped {guard_edits} unsafe/redundant DEALLOCATE line(s) via static analysis.")
        else:
            print(f"Fix: added {guard_edits} allocatable DEALLOCATE guard(s) via static analysis.")
    if args.hoist_loop_allocate:
        before_hoist = _read_lines(work_target) if args.diff else []
        hoist_edits = _hoist_loop_allocate(work_target, drop_deallocate=args.drop_deallocate)
        if hoist_edits:
            if args.diff:
                _print_unified_diff(work_target, before_hoist, _read_lines(work_target))
            edits += hoist_edits
            print(f"Fix: hoisted {hoist_edits} loop-local ALLOCATE pattern(s).")
    before_loop_guard = _read_lines(work_target) if args.diff else []
    loop_guard_edits = _fix_allocate_inside_loop_with_guard(work_target)
    if loop_guard_edits:
        if args.diff:
            _print_unified_diff(work_target, before_loop_guard, _read_lines(work_target))
        edits += loop_guard_edits
        print(
            f"Fix: inserted {loop_guard_edits} guarded DEALLOCATE line(s) before ALLOCATE in loop context."
        )

    for it in range(1, args.max_iter + 1):
        src = work_target
        srcs_str = " ".join(_q(s) for s in build_inputs)

        if args.runtime:
            exe = Path(tempfile.gettempdir()) / f"{work_target.stem}_xautofix_runtime.exe"
            cmd = f"gfortran {srcs_str} -o {_q(exe)}"
            print(f"[{it}] Build: {cmd}")
            cpb = _run_compile(cmd)
            if args.tee_both:
                _emit_cp_output(cpb, prefix=f"[{it}] Build ")
            if cpb.returncode != 0:
                err = cpb.stderr or cpb.stdout or ""
                action = _find_first_action(err, src)
                if compile_fix_enabled:
                    if action is not None:
                        af0 = Path(action["file"])  # type: ignore[index]
                        ln0 = int(action["line"])  # type: ignore[index]
                    else:
                        af0, ln0 = _first_error_loc(err, src)
                    if not af0.exists():
                        af0 = src
                    before0 = _read_lines(af0) if args.diff else []
                    changed0, msg0 = _try_general_compile_fix(af0, ln0, err)
                    if changed0:
                        if args.diff:
                            _print_unified_diff(af0, before0, _read_lines(af0))
                        edits += 1
                        print(msg0 or f"Fix: applied compile-fix rewrite in {af0}:{ln0}.")
                        continue
                    if msg0 and msg0.lower().startswith("warning:"):
                        print(msg0)
                if action is None:
                    print("Build: FAIL; no supported autofix for current error.")
                    _safe_print_text(err)
                    return _ret(cpb.returncode or 1)
                if compile_fix_enabled:
                    afile = Path(action["file"])  # type: ignore[index]
                    if not afile.exists():
                        afile = src
                    if action["kind"] == "unclassifiable":
                        line_no = int(action["line"])
                        before = _read_lines(afile) if args.diff else []
                        changed = _fix_subscripted_array_constructor(afile, line_no)
                        if changed:
                            if args.diff:
                                _print_unified_diff(afile, before, _read_lines(afile))
                            edits += 1
                            print(f"Fix: rewrote subscripted array constructor in {afile}:{line_no}.")
                            continue
                afile = Path(action["file"])  # type: ignore[index]
                if not afile.exists():
                    afile = src
                if action["kind"] == "undeclared":
                    name = str(action["name"])
                    line_no = int(action["line"])
                    lines = afile.read_text(encoding="utf-8", errors="replace").splitlines()
                    dtype = _guess_decl_type(lines, line_no, name)
                    decl = f"{dtype} :: {name}"
                    before = _read_lines(afile) if args.diff else []
                    changed = _insert_decl_near_implicit_none(afile, line_no, decl)
                    if changed:
                        if args.diff:
                            _print_unified_diff(afile, before, _read_lines(afile))
                        edits += 1
                        print(f"Fix: declared `{name}` in {afile}.")
                        continue
                elif action["kind"] == "if_paren":
                    line_no = int(action["line"])
                    before = _read_lines(afile) if args.diff else []
                    changed = _fix_if_missing_paren(afile, line_no)
                    if changed:
                        if args.diff:
                            _print_unified_diff(afile, before, _read_lines(afile))
                        edits += 1
                        print(f"Fix: parenthesized IF condition in {afile}:{line_no}.")
                        continue
                elif action["kind"] == "func_implicit":
                    line_no = int(action["line"])
                    name = str(action["name"])
                    before = _read_lines(afile) if args.diff else []
                    changed = _fix_bare_call_stmt(afile, line_no, name)
                    if changed:
                        if args.diff:
                            _print_unified_diff(afile, before, _read_lines(afile))
                        edits += 1
                        print(f"Fix: rewrote bare call to `call {name}(...)` in {afile}:{line_no}.")
                        continue
                elif action["kind"] == "percent_mod":
                    line_no = int(action["line"])
                    before = _read_lines(afile) if args.diff else []
                    changed = _fix_percent_mod(afile, line_no)
                    if changed:
                        if args.diff:
                            _print_unified_diff(afile, before, _read_lines(afile))
                        edits += 1
                        print(f"Fix: rewrote `%%` to `mod(...)` in {afile}:{line_no}.")
                        continue
                elif action["kind"] == "unclassifiable":
                    line_no = int(action["line"])
                    before = _read_lines(afile) if args.diff else []
                    changed = _fix_do_while_paren(afile, line_no)
                    if changed:
                        if args.diff:
                            _print_unified_diff(afile, before, _read_lines(afile))
                        edits += 1
                        print(f"Fix: parenthesized DO WHILE condition in {afile}:{line_no}.")
                        continue
                elif action["kind"] == "unexpected_eof":
                    before = _read_lines(afile) if args.diff else []
                    changed = _fix_unexpected_eof(afile)
                    if changed:
                        if args.diff:
                            _print_unified_diff(afile, before, _read_lines(afile))
                        edits += 1
                        print(f"Fix: appended missing closing `end` in {afile}.")
                        continue
                print("Build: FAIL; matched rule but no edit was applied.")
                _safe_print_text(err)
                return _ret(cpb.returncode or 1)

            print(f"[{it}] Run: {exe}")
            cpr = subprocess.run(
                [str(exe)],
                capture_output=True,
                text=True,
                encoding="utf-8",
                errors="replace",
            )
            if args.tee_both:
                _emit_cp_output(cpr, prefix=f"[{it}] Run ")
            if cpr.returncode == 0:
                if args.normalize_format_style:
                    before_norm = _read_lines(work_target) if args.diff else []
                    norm_changed = _normalize_format_spacing(work_target)
                    if norm_changed:
                        if args.diff:
                            _print_unified_diff(work_target, before_norm, _read_lines(work_target))
                        edits += 1
                        print(f"Fix: normalized explicit format spacing in {work_target}.")
                        continue
                    before_par = _read_lines(work_target) if args.diff else []
                    par_changed = _normalize_redundant_format_parens(work_target)
                    if par_changed:
                        if args.diff:
                            _print_unified_diff(work_target, before_par, _read_lines(work_target))
                        edits += 1
                        print(f"Fix: normalized redundant format parentheses in {work_target}.")
                        continue
                print(f"Run: PASS after {edits} edit(s).")
                if args.run:
                    rr = _build_and_run_program(build_inputs, work_target.stem, tee_both=args.tee_both)
                    return _ret(0 if rr == 0 else rr)
                return _ret(0)

            run_log = (cpr.stdout or "") + "\n" + (cpr.stderr or "")
            action = _parse_runtime_fmt_mismatch(run_log)
            if action is None:
                print("Run: FAIL; no supported runtime autofix for current error.")
                _safe_print_text(run_log)
                return _ret(cpr.returncode or 1)
            afile_from_log = Path(action["file"])  # type: ignore[index]
            if args.in_place and afile_from_log.exists():
                afile = afile_from_log
            else:
                afile = work_target
            rline = int(action["line"])
            before_rebuild = _read_lines(afile) if args.diff else []
            rebuilt = _fix_runtime_rebuild_print_format(afile, rline)
            if rebuilt:
                if args.diff:
                    _print_unified_diff(afile, before_rebuild, _read_lines(afile))
                edits += 1
                print(f"Fix: rebuilt format string from declarations in {afile}:{rline}.")
                continue
            before = _read_lines(afile) if args.diff else []
            changed = _fix_runtime_format_mismatch(
                afile,
                rline,
                str(action["expected"]),
                str(action["got"]),
                int(action["item"]),
            )
            if changed:
                if args.diff:
                    _print_unified_diff(afile, before, _read_lines(afile))
                edits += 1
                print(
                    "Fix: runtime formatted-transfer mismatch "
                    f"({action['expected']} -> {action['got']}, item {action['item']}) in {afile}:{action['line']}."
                )
                continue
            print("Run: FAIL; matched runtime rule but no edit was applied.")
            _safe_print_text(run_log)
            return _ret(cpr.returncode or 1)

        cmd = args.compile_cmd.format(src=_q(src), srcs=srcs_str)
        print(f"[{it}] Build: {cmd}")
        cp = _run_compile(cmd)
        if args.tee_both:
            _emit_cp_output(cp, prefix=f"[{it}] Build ")
        if cp.returncode == 0:
            static_mismatches = _static_format_mismatches(src)
            if static_mismatches:
                fixed_one = False
                for line_no, detail, item_no, got_type in static_mismatches:
                    before_static = _read_lines(src) if args.diff else []
                    changed = _fix_static_format_mismatch_with_xfmm(src, line_no)
                    if not changed:
                        changed = _fix_runtime_rebuild_print_format(src, line_no)
                    if (not changed) and item_no is not None and got_type is not None:
                        src_lines = src.read_text(encoding="utf-8", errors="replace").splitlines()
                        if 1 <= line_no <= len(src_lines):
                            exp = _expected_kind_at_item_in_line(src_lines[line_no - 1], item_no)
                            if exp is not None:
                                changed = _fix_runtime_format_mismatch(src, line_no, exp, got_type, item_no)
                    if changed:
                        if args.diff:
                            _print_unified_diff(src, before_static, _read_lines(src))
                        edits += 1
                        print(f"Fix: static format/type mismatch at {src}:{line_no} ({detail}).")
                        fixed_one = True
                        break
                if fixed_one:
                    continue
            before_expr = _read_lines(src) if args.diff else []
            expr_changed = _fix_scalar_i_format_for_real_expr(src)
            if expr_changed:
                if args.diff:
                    _print_unified_diff(src, before_expr, _read_lines(src))
                edits += 1
                print(f"Fix: corrected integer format descriptor for real-valued expression in {src}.")
                continue
            if args.normalize_format_style:
                before_par = _read_lines(src) if args.diff else []
                par_changed = _normalize_redundant_format_parens(src)
                if par_changed:
                    if args.diff:
                        _print_unified_diff(src, before_par, _read_lines(src))
                    edits += 1
                    print(f"Fix: normalized redundant format parentheses in {src}.")
                    continue
            print(f"Build: PASS after {edits} edit(s).")
            if args.run:
                rr = _build_and_run_program(build_inputs, work_target.stem, tee_both=args.tee_both)
                return _ret(0 if rr == 0 else rr)
            return _ret(0)

        err = cp.stderr or cp.stdout or ""
        action = _find_first_action(err, src)
        if compile_fix_enabled:
            if action is not None:
                af0 = Path(action["file"])  # type: ignore[index]
                ln0 = int(action["line"])  # type: ignore[index]
            else:
                af0, ln0 = _first_error_loc(err, src)
            if not af0.exists():
                af0 = src
            before0 = _read_lines(af0) if args.diff else []
            changed0, msg0 = _try_general_compile_fix(af0, ln0, err)
            if changed0:
                if args.diff:
                    _print_unified_diff(af0, before0, _read_lines(af0))
                edits += 1
                print(msg0 or f"Fix: applied compile-fix rewrite in {af0}:{ln0}.")
                continue
            if msg0 and msg0.lower().startswith("warning:"):
                print(msg0)
        if action is None:
            print("Build: FAIL; no supported autofix for current error.")
            _safe_print_text(err)
            return _ret(cp.returncode or 1)
        if compile_fix_enabled:
            afile = Path(action["file"])  # type: ignore[index]
            if not afile.exists():
                afile = src
            if action["kind"] == "unclassifiable":
                line_no = int(action["line"])
                before = _read_lines(afile) if args.diff else []
                changed = _fix_subscripted_array_constructor(afile, line_no)
                if changed:
                    if args.diff:
                        _print_unified_diff(afile, before, _read_lines(afile))
                    edits += 1
                    print(f"Fix: rewrote subscripted array constructor in {afile}:{line_no}.")
                    continue

        afile = Path(action["file"])  # type: ignore[index]
        if not afile.exists():
            afile = src
        if action["kind"] == "undeclared":
            name = str(action["name"])
            line_no = int(action["line"])
            lines = afile.read_text(encoding="utf-8", errors="replace").splitlines()
            dtype = _guess_decl_type(lines, line_no, name)
            decl = f"{dtype} :: {name}"
            before = _read_lines(afile) if args.diff else []
            changed = _insert_decl_near_implicit_none(afile, line_no, decl)
            if changed:
                if args.diff:
                    _print_unified_diff(afile, before, _read_lines(afile))
                edits += 1
                print(f"Fix: declared `{name}` in {afile}.")
                continue
        elif action["kind"] == "if_paren":
            line_no = int(action["line"])
            before = _read_lines(afile) if args.diff else []
            changed = _fix_if_missing_paren(afile, line_no)
            if changed:
                if args.diff:
                    _print_unified_diff(afile, before, _read_lines(afile))
                edits += 1
                print(f"Fix: parenthesized IF condition in {afile}:{line_no}.")
                continue
        elif action["kind"] == "func_implicit":
            line_no = int(action["line"])
            name = str(action["name"])
            before = _read_lines(afile) if args.diff else []
            changed = _fix_bare_call_stmt(afile, line_no, name)
            if changed:
                if args.diff:
                    _print_unified_diff(afile, before, _read_lines(afile))
                edits += 1
                print(f"Fix: rewrote bare call to `call {name}(...)` in {afile}:{line_no}.")
                continue
        elif action["kind"] == "percent_mod":
            line_no = int(action["line"])
            before = _read_lines(afile) if args.diff else []
            changed = _fix_percent_mod(afile, line_no)
            if changed:
                if args.diff:
                    _print_unified_diff(afile, before, _read_lines(afile))
                edits += 1
                print(f"Fix: rewrote `%%` to `mod(...)` in {afile}:{line_no}.")
                continue
        elif action["kind"] == "unclassifiable":
            line_no = int(action["line"])
            before = _read_lines(afile) if args.diff else []
            changed = _fix_do_while_paren(afile, line_no)
            if changed:
                if args.diff:
                    _print_unified_diff(afile, before, _read_lines(afile))
                edits += 1
                print(f"Fix: parenthesized DO WHILE condition in {afile}:{line_no}.")
                continue
        elif action["kind"] == "unexpected_eof":
            before = _read_lines(afile) if args.diff else []
            changed = _fix_unexpected_eof(afile)
            if changed:
                if args.diff:
                    _print_unified_diff(afile, before, _read_lines(afile))
                edits += 1
                print(f"Fix: appended missing closing `end` in {afile}.")
                continue

        print("Build: FAIL; matched rule but no edit was applied.")
        _safe_print_text(err)
        return _ret(cp.returncode or 1)

    print(f"Stopped after max iterations ({args.max_iter}); edits={edits}.")
    return _ret(1)


if __name__ == "__main__":
    raise SystemExit(main())
