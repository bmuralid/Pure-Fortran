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
    if cp.stdout:
        if prefix:
            print(f"{prefix}stdout:")
        print(cp.stdout.rstrip())
    if cp.stderr:
        if prefix:
            print(f"{prefix}stderr:")
        print(cp.stderr.rstrip())


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
            print(err.strip())
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
        if cnt <= 1:
            parts.append(f"{desc},1x")
        else:
            parts.append(f"{cnt}({desc},1x)")
    new_fmt = _normalize_format_comma_spacing("(" + ", ".join(parts) + ")")
    new_line = f'{m.group(1)}print "{new_fmt}", {args_s}'
    if new_line == old:
        return False
    lines[line_no_1 - 1] = new_line
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _static_format_mismatches(path: Path) -> list[Tuple[int, str]]:
    infos, _ = xfmm.fscan.load_source_files([path])
    if not infos:
        return []
    finfo = infos[0]
    findings = []
    for u in xfmm.collect_units(finfo):
        findings.extend(xfmm.analyze_unit(u))
    findings = [f for f in findings if f.certainty == "definite"]
    findings.sort(key=lambda f: (f.line, f.detail))
    out: list[Tuple[int, str]] = []
    seen: set[Tuple[int, str]] = set()
    for f in findings:
        k = (f.line, f.detail)
        if k in seen:
            continue
        seen.add(k)
        out.append(k)
    return out


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
        if cnt <= 1:
            fmt_parts.append(f"{desc},1x")
        else:
            fmt_parts.append(f"{cnt}({desc},1x)")
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
            if cnt == 1:
                fmt_parts.append(f"{desc},1x")
            else:
                fmt_parts.append(f"{cnt}({desc},1x)")
            i = j
    new_fmt = _normalize_format_comma_spacing("(" + ", ".join(fmt_parts) + ")")

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
        if "Unexpected end of file" in ln:
            return {"kind": "unexpected_eof", "file": cur_file, "line": cur_line}
    return None


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
        "--tee-both",
        action="store_true",
        help="Print both stdout and stderr from build/run steps each iteration.",
    )
    args = ap.parse_args()

    # Runtime mode is primarily intended to patch the target source directly.
    # Keep --out available when explicitly requested, but default to in-place.
    if args.runtime and (not args.in_place) and args.out == "temp.f90":
        args.in_place = True

    srcs = [Path(s) for s in args.sources]
    for s in srcs:
        if not s.exists():
            print(f"Missing file: {s}")
            return 1

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
        target = srcs[0]
        target_index = 0
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

    edits = 0
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
                if action is None:
                    print("Build: FAIL; no supported autofix for current error.")
                    print(err.strip())
                    return cpb.returncode or 1
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
                print(err.strip())
                return cpb.returncode or 1

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
                    return 0 if rr == 0 else rr
                return 0

            run_log = (cpr.stdout or "") + "\n" + (cpr.stderr or "")
            action = _parse_runtime_fmt_mismatch(run_log)
            if action is None:
                print("Run: FAIL; no supported runtime autofix for current error.")
                print(run_log.strip())
                return cpr.returncode or 1
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
            print(run_log.strip())
            return cpr.returncode or 1

        cmd = args.compile_cmd.format(src=_q(src), srcs=srcs_str)
        print(f"[{it}] Build: {cmd}")
        cp = _run_compile(cmd)
        if args.tee_both:
            _emit_cp_output(cp, prefix=f"[{it}] Build ")
        if cp.returncode == 0:
            static_mismatches = _static_format_mismatches(src)
            if static_mismatches:
                fixed_one = False
                for line_no, detail in static_mismatches:
                    before_static = _read_lines(src) if args.diff else []
                    changed = _fix_static_format_mismatch_with_xfmm(src, line_no)
                    if not changed:
                        changed = _fix_runtime_rebuild_print_format(src, line_no)
                    if changed:
                        if args.diff:
                            _print_unified_diff(src, before_static, _read_lines(src))
                        edits += 1
                        print(f"Fix: static format/type mismatch at {src}:{line_no} ({detail}).")
                        fixed_one = True
                        break
                if fixed_one:
                    continue
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
                return 0 if rr == 0 else rr
            return 0

        err = cp.stderr or cp.stdout or ""
        action = _find_first_action(err, src)
        if action is None:
            print("Build: FAIL; no supported autofix for current error.")
            print(err.strip())
            return cp.returncode or 1

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
        print(err.strip())
        return cp.returncode or 1

    print(f"Stopped after max iterations ({args.max_iter}); edits={edits}.")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
