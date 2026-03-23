import re
import sys
import argparse
import subprocess
import shlex
import time
import difflib
from pathlib import Path
from datetime import datetime
from dataclasses import dataclass


def split_fortran_comment(line: str) -> tuple[str, str]:
    in_str = False
    quote = ""
    out: list[str] = []
    for ch in line:
        if in_str:
            out.append(ch)
            if ch == quote:
                in_str = False
        else:
            if ch in ("'", '"'):
                in_str = True
                quote = ch
                out.append(ch)
            elif ch == "!":
                code = "".join(out).rstrip()
                comment = line[len("".join(out)) + 1 :].strip()
                return code, comment
            else:
                out.append(ch)
    return "".join(out).rstrip(), ""


@dataclass
class UseSpec:
    module: str
    only_items: list[str] | None
    intrinsic: bool = False




def _replace_identifier_outside_strings(code: str, old: str, new: str) -> str:
    out: list[str] = []
    i = 0
    n = len(code)
    in_str = False
    quote = ""
    pat = re.compile(rf"(?i)\b{re.escape(old)}\b")
    while i < n:
        ch = code[i]
        if in_str:
            out.append(ch)
            if ch == quote:
                in_str = False
            i += 1
            continue
        if ch in ("'", '"'):
            in_str = True
            quote = ch
            out.append(ch)
            i += 1
            continue
        j = i
        while j < n and code[j] not in ("'", '"'):
            j += 1
        out.append(pat.sub(new, code[i:j]))
        i = j
    return "".join(out)


def _choose_fresh_identifier(src: str, base: str) -> str:
    ids: set[str] = set()
    for raw in src.splitlines():
        code, _comment = split_fortran_comment(raw)
        ids.update(m.group(0).lower() for m in re.finditer(r"\b[a-z_]\w*\b", code, re.I))
    cand = base
    while cand.lower() in ids:
        cand += "_"
    return cand


def _preprocess_fortran_source(src: str) -> str:
    ids: set[str] = set()
    for raw in src.splitlines():
        code, _comment = split_fortran_comment(raw)
        ids.update(m.group(0).lower() for m in re.finditer(r"\b[a-z_]\w*\b", code, re.I))
    if "lambda" not in ids:
        return src
    repl = _choose_fresh_identifier(src, "lambda_")
    out_lines: list[str] = []
    for raw in src.splitlines():
        code, comment = split_fortran_comment(raw)
        new_code = _replace_identifier_outside_strings(code, "lambda", repl)
        if comment:
            if new_code:
                out_lines.append(f"{new_code} ! {comment}")
            else:
                out_lines.append(f"! {comment}")
        else:
            out_lines.append(new_code)
    return "\n".join(out_lines)

def _clean_fortran_code_lines(src: str) -> list[str]:
    out: list[str] = []
    for raw in src.splitlines():
        code, _c = split_fortran_comment(raw)
        s = code.strip()
        if s:
            out.append(s)
    return out


def _parse_file_interface(src: str) -> tuple[list[str], list[str], list[UseSpec]]:
    """Return (defined_modules, defined_symbols, use_specs) for one source."""
    code_lines = _clean_fortran_code_lines(src)
    defs: list[str] = []
    mods: list[str] = []
    uses: list[UseSpec] = []

    in_module = False
    in_module_contains = False

    for s in code_lines:
        sl = s.lower()

        # module declarations (skip 'module procedure')
        mm = re.match(r"^module\s+([a-z_]\w*)\b", sl, re.I)
        if mm and not re.match(r"^module\s+procedure\b", sl, re.I):
            mods.append(mm.group(1))
            in_module = True
            in_module_contains = False
            continue

        if re.match(r"^end\s+module\b", sl, re.I):
            in_module = False
            in_module_contains = False
            continue

        if in_module and sl == "contains":
            in_module_contains = True
            continue

        # type declarations
        mt = re.match(r"^type\s*(?:,\s*[^:]*)?::\s*([a-z_]\w*)\b", sl, re.I)
        if mt:
            defs.append(mt.group(1))
            continue

        # module declarative-part variables/parameters
        if in_module and not in_module_contains:
            pd = parse_decl(s)
            if pd:
                _ftype, attrs, rest = pd
                for nm, _shape, _init in parse_decl_items(rest, parse_decl_attr_dimension(attrs)):
                    defs.append(nm)
                continue
            td = re.match(r"^type\s*\(\s*([a-z_]\w*)\s*\)\s*(.*?)::\s*(.*)$", s, re.I)
            if td:
                attrs = td.group(2).strip()
                for nm, _shape, _init in parse_decl_items(td.group(3).strip(), parse_decl_attr_dimension(attrs)):
                    defs.append(nm)
                continue

        # function declarations
        mf = re.match(
            r"^(?!\s*end\s+function\b)\s*(?:(?:pure|elemental|recursive)\s+)*(?:\w+(?:\s*\([^)]*\))?\s+)*function\s+([a-z_]\w*)\s*\(",
            sl,
            re.I,
        )
        if mf:
            defs.append(mf.group(1))
            continue

        # subroutine declarations
        ms = re.match(
            r"^(?!\s*end\s+subroutine\b)\s*(?:(?:pure|elemental|recursive)\s+)*subroutine\s+([a-z_]\w*)\s*\(",
            sl,
            re.I,
        )
        if ms:
            defs.append(ms.group(1))
            continue

        # use statements
        mu = re.match(r"^use\s*(?:,\s*intrinsic\s*)?(?:::)?\s*([a-z_]\w*)\s*(.*)$", sl, re.I)
        if mu:
            mod = mu.group(1)
            tail = mu.group(2).strip()
            intrinsic = bool(re.search(r"\bintrinsic\b", sl, re.I))
            only_items: list[str] | None = None
            mo = re.search(r"\bonly\s*:\s*(.+)$", tail, re.I)
            if mo:
                only_raw = mo.group(1).strip()
                only_items = []
                for it in split_args(only_raw):
                    nm = it.strip()
                    if not nm:
                        continue
                    # For USE renames, keep the local imported name (a => b -> keep a).
                    if "=>" in nm:
                        nm = nm.split("=>", 1)[0].strip()
                    nm = nm.strip()
                    if re.match(r"^[a-z_]\w*$", nm, re.I):
                        only_items.append(nm)
            uses.append(UseSpec(module=mod, only_items=only_items, intrinsic=intrinsic))
            continue

    return unique_preserve(mods), unique_preserve(defs), uses


def _insert_imports(py_text: str, import_lines: list[str]) -> str:
    if not import_lines:
        return py_text
    lines = py_text.splitlines()
    insert_at = 0
    while insert_at < len(lines):
        s = lines[insert_at].strip()
        if s.startswith("import ") or s.startswith("from "):
            insert_at += 1
            continue
        if s == "":
            insert_at += 1
            break
        break
    merged = lines[:insert_at] + import_lines + ([""] if import_lines and (insert_at < len(lines) and lines[insert_at].strip() != "") else []) + lines[insert_at:]
    return "\n".join(merged).rstrip() + "\n"


def collapse_fortran_continuations(raw_lines: list[tuple[str, str]]) -> list[tuple[str, str]]:
    """Collapse free-form continuation lines joined with trailing/leading '&'."""
    out: list[tuple[str, str]] = []
    i = 0
    n = len(raw_lines)
    while i < n:
        code, comment = raw_lines[i]
        cur_code = code.rstrip()
        cur_comment = comment
        while cur_code.rstrip().endswith("&"):
            cur_code = cur_code.rstrip()
            cur_code = cur_code[:-1].rstrip()
            i += 1
            if i >= n:
                break
            ncode, ncomment = raw_lines[i]
            s = ncode.lstrip()
            if s.startswith("&"):
                s = s[1:].lstrip()
            if cur_code and s:
                cur_code = f"{cur_code} {s}"
            else:
                cur_code = cur_code + s
            if ncomment.strip():
                if cur_comment.strip():
                    cur_comment = f"{cur_comment.strip()} | {ncomment.strip()}"
                else:
                    cur_comment = ncomment
        out.append((cur_code, cur_comment))
        i += 1
    return out


def find_matching_paren(text: str, open_pos: int) -> int:
    depth = 0
    in_str = False
    q = ""
    for p in range(open_pos, len(text)):
        ch = text[p]
        if in_str:
            if ch == q:
                in_str = False
            continue
        if ch in ("'", '"'):
            in_str = True
            q = ch
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return p
    return -1


def split_args(s: str) -> list[str]:
    args: list[str] = []
    buf: list[str] = []
    pdepth = 0
    bdepth = 0
    in_str = False
    quote = ""
    for ch in s:
        if in_str:
            buf.append(ch)
            if ch == quote:
                in_str = False
            continue
        if ch in ("'", '"'):
            in_str = True
            quote = ch
            buf.append(ch)
            continue
        if ch == "(":
            pdepth += 1
            buf.append(ch)
            continue
        if ch == ")":
            pdepth = max(0, pdepth - 1)
            buf.append(ch)
            continue
        if ch == "[":
            bdepth += 1
            buf.append(ch)
            continue
        if ch == "]":
            bdepth = max(0, bdepth - 1)
            buf.append(ch)
            continue
        if ch == "," and pdepth == 0 and bdepth == 0:
            arg = "".join(buf).strip()
            if arg:
                args.append(arg)
            buf = []
            continue
        buf.append(ch)
    tail = "".join(buf).strip()
    if tail:
        args.append(tail)
    return args

def _fortran_unquote(s: str) -> str:
    if len(s) >= 2 and s[0] == s[-1] and s[0] in ("'", "\""):
        q = s[0]
        return s[1:-1].replace(q + q, q)
    return s


def _strip_one_outer_paren(s: str) -> str:
    s = s.strip()
    if len(s) >= 2 and s[0] == "(" and s[-1] == ")" and find_matching_paren(s, 0) == len(s) - 1:
        return s[1:-1].strip()
    return s


def _fortran_implied_do_expr(raw: str, translate_expr, arrays_1d: set[str]) -> str | None:
    """Translate an I/O implied-DO item, including nested implied-DOs."""
    s = raw.strip()
    if not (len(s) >= 2 and s[0] == "(" and s[-1] == ")" and find_matching_paren(s, 0) == len(s) - 1):
        return None
    inner = s[1:-1].strip()
    parts = [p.strip() for p in split_args(inner) if p.strip()]
    if len(parts) < 3:
        return None

    obj_parts: list[str] | None = None
    var = lo = hi = step = None

    if len(parts) >= 4:
        mm = re.fullmatch(r"([a-z_]\w*)\s*=\s*(.+)", parts[-3], re.I)
        if mm:
            obj_parts = parts[:-3]
            var = mm.group(1)
            lo = mm.group(2).strip()
            hi = parts[-2]
            step = parts[-1]
    if obj_parts is None:
        mm = re.fullmatch(r"([a-z_]\w*)\s*=\s*(.+)", parts[-2], re.I)
        if mm:
            obj_parts = parts[:-2]
            var = mm.group(1)
            lo = mm.group(2).strip()
            hi = parts[-1]
            step = None
    if obj_parts is None or not obj_parts or var is None or lo is None or hi is None:
        return None

    lo_py = translate_expr(lo, arrays_1d)
    hi_py = translate_expr(hi, arrays_1d)
    if step is None:
        step_py = "1"
    else:
        step_py = translate_expr(step, arrays_1d)

    body_parts: list[str] = []
    for p in obj_parts:
        nested_py = _fortran_implied_do_expr(p, translate_expr, arrays_1d)
        if nested_py is not None:
            body_parts.append(nested_py)
        else:
            expr_py = translate_expr(p, arrays_1d)
            body_parts.append(f"[{expr_py}]")

    if len(body_parts) == 1:
        body_py = body_parts[0]
    else:
        body_py = " + ".join(body_parts)
    return f"_xf2p_implied_do(lambda {var}: {body_py}, {lo_py}, {hi_py}, {step_py})"


def _is_recyclable_io_iterable(raw: str, decl_array_types: dict[str, str]) -> bool:
    s = raw.strip()
    return (
        s.startswith('[')
        or s.startswith('(/')
        or _fortran_implied_do_expr(s, lambda x, _a: x, set()) is not None
        or (re.fullmatch(r"[a-z_]\w*", s, flags=re.I) and s.lower() in decl_array_types)
    )


def _fortran_format_arg_count(fmt_literal: str) -> int | None:
    """Return the number of data items consumed by one pass of a limited format."""
    try:
        fmt_text = _strip_one_outer_paren(_fortran_unquote(fmt_literal.strip()))
    except Exception:
        return None

    def count_token(tok: str) -> int | None:
        tok = tok.strip()
        if not tok:
            return 0
        low = tok.lower()

        if tok[0] in ("'", '"') and tok[-1] == tok[0]:
            return 0

        mm = re.fullmatch(r"\*\((.*)\)", tok, re.I)
        if mm:
            return None

        mm = re.fullmatch(r"(\d+)\((.*)\)", tok, re.I)
        if mm:
            rep = int(mm.group(1))
            inner_total = 0
            for sub in split_args(mm.group(2).strip()):
                nsub = count_token(sub)
                if nsub is None:
                    return None
                inner_total += nsub
            return rep * inner_total

        mm = re.fullmatch(r"(\d*)/", low)
        if mm:
            return 0

        if low == ":":
            return 0

        mm = re.fullmatch(r"(\d*)x", low)
        if mm:
            return 0

        mm = re.fullmatch(r"(\d*)a(\d+)?", low)
        if mm:
            return int(mm.group(1) or "1")

        mm = re.fullmatch(r"(\d*)l(\d+)", low)
        if mm:
            return int(mm.group(1) or "1")

        mm = re.fullmatch(r"(\d*)i(\d+)(?:\.\d+)?", low)
        if mm:
            return int(mm.group(1) or "1")

        mm = re.fullmatch(r"(\d*)(es|en|e|d|g|f)(\d+)(?:\.(\d+))?(?:e(\d+))?", low)
        if mm:
            return int(mm.group(1) or "1")

        if tok.startswith("(") and tok.endswith(")") and find_matching_paren(tok, 0) == len(tok) - 1:
            total = 0
            for sub in split_args(tok[1:-1]):
                nsub = count_token(sub)
                if nsub is None:
                    return None
                total += nsub
            return total

        return None

    total = 0
    for item in split_args(fmt_text):
        nitem = count_token(item)
        if nitem is None:
            return None
        total += nitem
    return total



def _fortran_format_recycled_expr(fmt_literal: str, iterable_expr: str) -> str | None:
    """Return a Python expression for one-argument finite-format recycling over an iterable."""
    if _fortran_format_arg_count(fmt_literal) != 1:
        return None
    item_expr = _fortran_format_expr(fmt_literal, ["_xf2p_item"])
    if item_expr is None:
        return None
    # For finite format reversion during output, each new cycle starts a new record.
    return f"'\\n'.join({item_expr} for _xf2p_item in {iterable_expr})"


def _single_iterable_formatted_arg_plan(fmt_literal: str, raw_args: list[str], decl_array_types: dict[str, str]) -> tuple[int, int] | None:
    """Plan finite-format expansion when exactly one I/O argument is an iterable."""
    total = _fortran_format_arg_count(fmt_literal)
    if total is None:
        return None
    iterable_pos = [i for i, raw in enumerate(raw_args) if _is_recyclable_io_iterable(raw, decl_array_types)]
    if len(iterable_pos) != 1:
        return None
    pos = iterable_pos[0]
    needed = total - (len(raw_args) - 1)
    if needed <= 0:
        return None
    return pos, needed


def _fortran_format_expr(fmt_literal: str, arg_exprs: list[str]) -> str | None:
    """Return a Python expression for a limited Fortran character format string."""
    try:
        fmt_text = _strip_one_outer_paren(_fortran_unquote(fmt_literal.strip()))
    except Exception:
        return None

    parts: list[str] = []
    arg_i = 0
    STOP = "__xf2p_stop__"

    def take_arg() -> str | None:
        nonlocal arg_i
        if arg_i >= len(arg_exprs):
            return None
        out = arg_exprs[arg_i]
        arg_i += 1
        return out

    def add_token(tok: str) -> str:
        nonlocal arg_i
        tok = tok.strip()
        if not tok:
            return "ok"
        low = tok.lower()

        mm = re.fullmatch(r"\*\((.*)\)", tok, re.I)
        if mm:
            inner = mm.group(1).strip()
            if arg_i >= len(arg_exprs):
                return "ok"
            iterable = arg_exprs[arg_i]
            arg_i += 1
            inner_items = split_args(inner)
            if ":" in [it.strip().lower() for it in inner_items]:
                colon_i = next(i for i, it in enumerate(inner_items) if it.strip().lower() == ":")
                left = ",".join(inner_items[:colon_i]).strip()
                right = ",".join(inner_items[colon_i + 1:]).strip()
                if left and right:
                    left_n = _fortran_format_arg_count(repr("(" + left + ")"))
                    right_n = _fortran_format_arg_count(repr("(" + right + ")"))
                    if left_n == 1 and right_n == 0:
                        item_expr = _fortran_format_expr(repr("(" + left + ")"), ["_xf2p_item"])
                        sep_expr = _fortran_format_expr(repr("(" + right + ")"), [])
                        if item_expr is not None and sep_expr is not None:
                            parts.append(f"({sep_expr}).join({item_expr} for _xf2p_item in {iterable})")
                            return "ok"
            inner_expr = _fortran_format_expr(repr("(" + inner + ")"), ["_xf2p_item"])
            if inner_expr is None:
                return "fail"
            parts.append(f"''.join({inner_expr} for _xf2p_item in {iterable})")
            return "ok"

        if tok[0] in ("'", '"') and tok[-1] == tok[0]:
            parts.append(repr(_fortran_unquote(tok)))
            return "ok"

        mm = re.fullmatch(r"(\d+)\((.*)\)", tok, re.I)
        if mm:
            rep = int(mm.group(1))
            inner = mm.group(2).strip()
            inner_items = split_args(inner)
            for _ in range(rep):
                for sub in inner_items:
                    status = add_token(sub)
                    if status == "fail":
                        return "fail"
                    if status == STOP:
                        return STOP
            return "ok"

        mm = re.fullmatch(r"(\d*)/", low)
        if mm:
            rep = int(mm.group(1) or "1")
            parts.append(repr("\n" * rep))
            return "ok"

        if low == ":":
            if arg_i >= len(arg_exprs):
                return STOP
            return "ok"

        mm = re.fullmatch(r"(\d*)x", low)
        if mm:
            rep = int(mm.group(1) or "1")
            if rep == 1:
                parts.append(repr(" "))
            else:
                parts.append(f"{rep}*' '")
            return "ok"

        mm = re.fullmatch(r"(\d*)a(\d+)?", low)
        if mm:
            rep = int(mm.group(1) or "1")
            width = mm.group(2)
            for _ in range(rep):
                a = take_arg()
                if a is None:
                    return STOP
                if width is None:
                    parts.append(f"str({a})")
                else:
                    parts.append(f"str({a}).rjust({int(width)})")
            return "ok"

        mm = re.fullmatch(r"(\d*)l(\d+)", low)
        if mm:
            rep = int(mm.group(1) or "1")
            width = int(mm.group(2))
            for _ in range(rep):
                a = take_arg()
                if a is None:
                    return STOP
                parts.append(f"str(bool({a})).upper().replace('TRUE', 'T').replace('FALSE', 'F').rjust({width})")
            return "ok"

        mm = re.fullmatch(r"(\d*)i(\d+)(?:\.\d+)?", low)
        if mm:
            rep = int(mm.group(1) or "1")
            width = int(mm.group(2))
            for _ in range(rep):
                a = take_arg()
                if a is None:
                    return STOP
                if width == 0:
                    parts.append(f"str(int({a}))")
                else:
                    parts.append(f"format(int({a}), '{width}d')")
            return "ok"

        mm = re.fullmatch(r"(\d*)(es|en|e|d|g|f)(\d+)(?:\.(\d+))?(?:e(\d+))?", low)
        if mm:
            rep = int(mm.group(1) or "1")
            code = mm.group(2)
            width = int(mm.group(3))
            prec = mm.group(4)
            py_code = {"d": "E", "e": "E", "es": "E", "en": "E", "f": "f", "g": "G"}[code]
            spec = f"{width}.{int(prec)}{py_code}" if prec is not None else f"{width}{py_code}"
            for _ in range(rep):
                a = take_arg()
                if a is None:
                    return STOP
                parts.append(f"format(float({a}), '{spec}')")
            return "ok"

        if tok.startswith("(") and tok.endswith(")") and find_matching_paren(tok, 0) == len(tok) - 1:
            for sub in split_args(tok[1:-1]):
                status = add_token(sub)
                if status == "fail":
                    return "fail"
                if status == STOP:
                    return STOP
            return "ok"

        return "fail"

    items = split_args(fmt_text)
    if not items:
        return repr("")
    for item in items:
        status = add_token(item)
        if status == "fail":
            return None
        if status == STOP:
            break
    if arg_i < len(arg_exprs):
        for a in arg_exprs[arg_i:]:
            parts.append(f"str({a})")
    if not parts:
        return repr("")
    return " + ".join(parts)



def unique_preserve(items: list[str]) -> list[str]:
    seen: set[str] = set()
    out: list[str] = []
    for it in items:
        key = it.strip().lower()
        if key in seen:
            continue
        seen.add(key)
        out.append(it)
    return out


def _find_dups_ci(items: list[str]) -> list[str]:
    seen: set[str] = set()
    dups: list[str] = []
    for it in items:
        key = it.strip().lower()
        if key in seen and key not in dups:
            dups.append(key)
        seen.add(key)
    return dups


_type_scalar_hint = {
    "integer": "int",
    "real": "np.float64",
    "logical": "bool",
    "complex": "complex",
    "character": "str",
}

_type_default_scalar_value = {
    "integer": "0",
    "real": "np.float64(0.0)",
    "logical": "False",
    "complex": "0j",
    "character": repr(""),
}

_type_dtype = {
    "integer": "np.int_",
    "real": "np.float64",
    "logical": "np.bool_",
    "complex": "np.complex128",
    "character": "object",
}

_type_ndarray_hint = {
    "integer": "npt.NDArray[np.int_]",
    "real": "npt.NDArray[np.float64]",
    "logical": "npt.NDArray[np.bool_]",
    "complex": "npt.NDArray[np.complex128]",
    "character": "npt.NDArray[object]",
}

_LOCAL_RUNTIME_HELPERS = {
    "_f_size",
    "_f_spread",
    "_f_assign_array",
    "mean_1d",
    "var_1d",
    "argsort_real",
    "random_normal_vec",
    "random_choice2",
    "random_choice_prob",
    "random_choice_norep",
}


def infer_function_result_ftype(header: str) -> str | None:
    """Infer scalar Fortran result type from function header when explicit.

    Examples:
      pure integer function f(...)
      real(kind=dp) pure function g(...)
    """
    h = header.strip()
    m = re.search(r"\b(integer|real|logical|complex)\b(?:\s*\([^)]*\))?\b[^!\n]*\bfunction\b", h, re.I)
    if not m:
        return None
    return m.group(1).lower()

_decl_re = re.compile(r"^(integer|real|logical|complex|character(?:\s*\([^)]*\))?)\b(.*)::(.*)$", re.I)


def parse_decl(line: str):
    m = _decl_re.match(line.strip())
    if not m:
        return None
    ftype = m.group(1).lower()
    if ftype.startswith("character"):
        ftype = "character"
    attrs = m.group(2).strip()
    rest = m.group(3).strip()
    return ftype, attrs, rest


def parse_decl_attr_dimension(attrs: str) -> str | None:
    m = re.search(r"\bdimension\s*\(\s*(.*?)\s*\)", attrs, re.I)
    if not m:
        return None
    return m.group(1).strip()


def parse_decl_items(rest: str, default_shape: str | None = None):
    items = []
    for part in split_args(rest):
        part = part.strip()
        if not part:
            continue
        init = None
        if "=" in part:
            left, init = part.split("=", 1)
            left = left.strip()
            init = init.strip()
        else:
            left = part.strip()

        shape = None
        mm = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)\s*$", left, re.I)
        if mm:
            name = mm.group(1)
            shape = mm.group(2).strip()
        else:
            name = left
            shape = default_shape

        items.append((name, shape, init))
    return items


class basic_f2p:
    def __init__(self) -> None:
        self.out: list[str] = []
        self.indent = 0
        self.seen_parameter = False
        self._code_emit_count = 0
        self._block_code_start: list[int] = []
        self._decl_types: dict[str, str] = {}
        self._decl_array_types: dict[str, str] = {}
        self._subr_sigs: dict[str, dict[str, list[str]]] = {}
        self._current_result_name: str | None = None
        self._derived_types: set[str] = set()
        self._seen_scipy_special = False

    def _kind_alias_value(self, remote: str) -> str | None:
        r = remote.strip().lower()
        mapping = {
            "real32": "4",
            "real64": "8",
            "int8": "1",
            "int16": "2",
            "int32": "4",
            "int64": "8",
        }
        return mapping.get(r)

    def _emit_intrinsic_use_aliases(self, lines: list[tuple[str, str]]) -> None:
        for code, _comment in lines:
            s = code.strip()
            mu = re.match(r"^use\s*,\s*intrinsic\s*::\s*([a-z_]\w*)\s*(.*)$", s, re.I)
            if not mu:
                continue
            mod = mu.group(1).strip().lower()
            tail = mu.group(2).strip()
            if mod != "iso_fortran_env":
                continue
            mo = re.search(r"\bonly\s*:\s*(.+)$", tail, re.I)
            if not mo:
                continue
            for it in split_args(mo.group(1).strip()):
                nm = it.strip()
                if not nm:
                    continue
                if "=>" in nm:
                    local_name, remote_name = [p.strip() for p in nm.split("=>", 1)]
                else:
                    local_name = nm.strip()
                    remote_name = nm.strip()
                alias_val = self._kind_alias_value(remote_name)
                if alias_val is not None and re.match(r"^[a-z_]\w*$", local_name, re.I):
                    self.emit(f"{local_name} = {alias_val}")

    def _type_hint(self, ftype: str, type_name: str | None = None, is_array: bool = False) -> str:
        if ftype == "type":
            if is_array:
                return "npt.NDArray[object]"
            return type_name if type_name else "SimpleNamespace"
        if is_array:
            return _type_ndarray_hint.get(ftype, "npt.NDArray[np.float64]")
        return _type_scalar_hint.get(ftype, "int")

    def _parse_decl_line(self, s: str):
        pd = parse_decl(s)
        if pd:
            ftype, attrs, rest = pd
            return ftype, None, attrs.lower(), parse_decl_items(rest, parse_decl_attr_dimension(attrs))
        td = re.match(r"^type\s*\(\s*([a-z_]\w*)\s*\)\s*(.*?)::\s*(.*)$", s, re.I)
        if td:
            attrs = td.group(2).strip()
            return "type", td.group(1), attrs.lower(), parse_decl_items(td.group(3).strip(), parse_decl_attr_dimension(attrs))
        return None

    def _validate_unit_symbols(self, unit_kind: str, unit_name: str, args: list[str], body_lines: list[tuple[str, str]]) -> None:
        dups = _find_dups_ci(args)
        if dups:
            raise ValueError(f"{unit_kind} {unit_name}: duplicate formal argument(s): {', '.join(dups)}")

        # Track declarations by lexical scope. A `block ... end block` introduces
        # a nested scope that may legitimately redeclare names used elsewhere.
        # Use a scope-id stack (not just depth), because sibling blocks share depth.
        seen_decl: dict[tuple[tuple[int, ...], str], tuple] = {}
        scope_stack: list[int] = [0]
        next_scope_id = 1
        for i, (code, _comment) in enumerate(body_lines, start=1):
            s = code.strip()
            sl = s.lower()
            if re.match(r"^block\b", sl):
                scope_stack.append(next_scope_id)
                next_scope_id += 1
                continue
            if re.match(r"^end\s+block\b", sl):
                if len(scope_stack) > 1:
                    scope_stack.pop()
                continue
            parsed = self._parse_decl_line(s)
            if not parsed:
                continue
            ftype, type_name, attrs_l, items = parsed
            for name, shape, _init in items:
                key = (tuple(scope_stack), name.lower())
                sig = (
                    ftype,
                    (type_name or "").lower(),
                    shape is not None,
                    (shape or "").replace(" ", "").lower(),
                    attrs_l.replace(" ", ""),
                )
                if key in seen_decl:
                    prev = seen_decl[key]
                    if prev != sig:
                        raise ValueError(
                            f"{unit_kind} {unit_name}: conflicting declaration for '{name}' at body line {i}"
                        )
                    raise ValueError(
                        f"{unit_kind} {unit_name}: duplicate declaration for '{name}' at body line {i}"
                    )
                seen_decl[key] = sig


    def _is_function_header(self, s: str) -> bool:
        return bool(re.match(r"^(?!\s*end\s+function\b)\s*(?:(?:pure\s+)?\w+(?:\s*\([^)]*\))?\s+)*function\b", s, re.I))

    def _is_subroutine_header(self, s: str) -> bool:
        return bool(re.match(r"^(?!\s*end\s+subroutine\b)\s*(?:pure\s+)?subroutine\b", s, re.I))

    def _collect_subprogram_body(self, lines: list[tuple[str, str]], start_idx: int, outer_kind: str) -> tuple[list[tuple[str, str]], int]:
        body: list[tuple[str, str]] = []
        i = start_idx
        n = len(lines)
        in_contains = False
        internal_depth = 0
        while i < n:
            code = lines[i][0]
            s = code.strip()
            sl = s.lower()
            if sl == "contains":
                in_contains = True
                body.append(lines[i])
                i += 1
                continue
            if in_contains:
                if self._is_function_header(s) or self._is_subroutine_header(s):
                    internal_depth += 1
                    body.append(lines[i])
                    i += 1
                    continue
                if re.match(r"^\s*end\s+function\b", s, re.I):
                    if outer_kind == "function" and internal_depth == 0:
                        break
                    if internal_depth > 0:
                        internal_depth -= 1
                    body.append(lines[i])
                    i += 1
                    continue
                if re.match(r"^\s*end\s+subroutine\b", s, re.I):
                    if outer_kind == "subroutine" and internal_depth == 0:
                        break
                    if internal_depth > 0:
                        internal_depth -= 1
                    body.append(lines[i])
                    i += 1
                    continue
            else:
                if re.match(rf"^\s*end\s+{re.escape(outer_kind)}\b", s, re.I):
                    break
            body.append(lines[i])
            i += 1
        return body, i

    def _split_internal_subprograms(self, body_lines: list[tuple[str, str]]) -> tuple[list[tuple[str, str]], list[tuple[str, str, list[tuple[str, str]]]]]:
        contains_idx = None
        for i, (code, _comment) in enumerate(body_lines):
            if code.strip().lower() == "contains":
                contains_idx = i
                break
        if contains_idx is None:
            return body_lines, []
        main_lines = body_lines[:contains_idx]
        tail = body_lines[contains_idx + 1 :]
        internals: list[tuple[str, str, list[tuple[str, str]]]] = []
        i = 0
        n = len(tail)
        while i < n:
            line = tail[i][0].strip()
            if not line:
                i += 1
                continue
            if self._is_function_header(line):
                header = line
                body, end_idx = self._collect_subprogram_body(tail, i + 1, "function")
                internals.append(("function", header, body))
                i = end_idx + 1
                continue
            if self._is_subroutine_header(line):
                header = line
                body, end_idx = self._collect_subprogram_body(tail, i + 1, "subroutine")
                internals.append(("subroutine", header, body))
                i = end_idx + 1
                continue
            i += 1
        return main_lines, internals

    def transpile_derived_type(self, header: str, body_lines: list[tuple[str, str]]) -> None:
        h = header.strip()
        m = re.match(r"^type\b(?:\s*,[^:]*)?(?:\s*::\s*|\s+)([a-z_]\w*)\s*$", h, re.I)
        if not m:
            return
        tname = m.group(1)
        self._derived_types.add(tname)

        self.emit("@dataclass")
        self.emit(f"class {tname}:")
        self.indent += 1
        had_field = False
        for code, comment in body_lines:
            s = code.strip()
            pd = parse_decl(s)
            if pd:
                ftype, attrs, rest = pd
                attrs_l = attrs.lower()
                items = parse_decl_items(rest, parse_decl_attr_dimension(attrs))
                type_name = None
            else:
                td = re.match(r"^type\s*\(\s*([a-z_]\w*)\s*\)\s*(.*?)::\s*(.*)$", s, re.I)
                if not td:
                    continue
                ftype = "type"
                attrs_l = td.group(2).strip().lower()
                items = parse_decl_items(td.group(3).strip(), parse_decl_attr_dimension(td.group(2).strip()))
                type_name = td.group(1)
            for name, shape, init in items:
                had_field = True
                cmt = f"  # {comment.strip()}" if comment.strip() else ""
                if shape is not None:
                    if "allocatable" in attrs_l:
                        self.emit(f"{name}: {self._type_hint(ftype, type_name, is_array=True)} | None = None{cmt}")
                        continue
                    shape_py = self._shape_to_py(shape, set())
                    if ftype == "type":
                        self.emit(f"{name}: {self._type_hint(ftype, type_name, is_array=True)} = field(default_factory=lambda: np.empty({shape_py}, dtype=object)){cmt}")
                    else:
                        dtype = _type_dtype[ftype]
                        self.emit(f"{name}: {self._type_hint(ftype, type_name, is_array=True)} = field(default_factory=lambda: np.empty({shape_py}, dtype={dtype})){cmt}")
                    continue
                if "allocatable" in attrs_l:
                    self.emit(f"{name}: {self._type_hint(ftype, type_name, is_array=False)} | None = None{cmt}")
                    continue
                if ftype == "type":
                    self.emit(f"{name}: {self._type_hint(ftype, type_name, is_array=False)} = field(default_factory={type_name}){cmt}")
                    continue
                hint = self._type_hint(ftype, None, is_array=False)
                if init is None:
                    default_val = _type_default_scalar_value.get(ftype, "0")
                else:
                    default_val = self.translate_expr(init, set())
                self.emit(f"{name}: {hint} = {default_val}{cmt}")
        if not had_field:
            self.emit("pass")
        self.indent = max(0, self.indent - 1)
        self.emit("")

    @staticmethod
    def _remove_top_level_def(lines: list[str], name: str) -> list[str]:
        start = None
        for i, ln in enumerate(lines):
            if re.match(rf"^def\s+{re.escape(name)}\s*\(", ln):
                start = i
                break
        if start is None:
            return lines
        end = len(lines)
        for j in range(start + 1, len(lines)):
            if re.match(r"^(def|class)\s+\w+\s*\(|^@dataclass\b|^if __name__ == ", lines[j]):
                end = j
                break
        out = lines[:start]
        out.extend(lines[end:])
        while out and out[-1] == "":
            out.pop()
        return out

    @staticmethod
    def _drop_unused_runtime(lines: list[str]) -> list[str]:
        helper_names = [
            "_f_size",
            "_f_spread",
            "_f_assign_array",
            "mean_1d",
            "var_1d",
            "argsort_real",
            "random_normal_vec",
            "random_choice2",
            "random_choice_prob",
            "random_choice_norep",
        ]
        changed = True
        while changed:
            changed = False
            text = "\n".join(lines)
            for name in helper_names:
                # one reference means definition-only; remove it
                if len(re.findall(rf"\b{name}\s*\(", text)) <= 1:
                    new_lines = basic_f2p._remove_top_level_def(lines, name)
                    if len(new_lines) != len(lines):
                        lines = new_lines
                        text = "\n".join(lines)
                        changed = True

        # prune imports that are no longer needed
        def _drop_line(prefix: str) -> None:
            nonlocal lines
            lines = [ln for ln in lines if not ln.startswith(prefix)]

        text = "\n".join(lines)
        if "@dataclass" not in text:
            _drop_line("from dataclasses import dataclass")
        if "SimpleNamespace(" not in text:
            _drop_line("from types import SimpleNamespace")
        text = "\n".join(lines)
        if re.search(r"\bnpt\.", text) is None:
            _drop_line("import numpy.typing as npt")
        text = "\n".join(lines)
        if re.search(r"\bnp\.", text) is None:
            _drop_line("import numpy as np")
        text = "\n".join(lines)
        if re.search(r"\bsps\.", text) is None:
            _drop_line("import scipy.special as sps")
        return lines

    def emit(self, s: str = "") -> None:
        if s and not s.lstrip().startswith("#"):
            self._code_emit_count += 1
        self.out.append((" " * 4 * self.indent) + s if s else "")

    def emit_comment(self, c: str) -> None:
        cc = c.strip()
        if cc:
            self.emit(f"# {cc}")

    def _shape_to_py(self, shape: str, arrays_1d: set[str]) -> str:
        dims: list[str] = []
        for d in split_args(shape):
            ds = d.strip()
            if ":" in ds:
                p = ds.split(":", 1)
                lo = p[0].strip()
                hi = p[1].strip()
                if lo and hi:
                    if re.fullmatch(r"[+-]?\d+", lo) and lo == "1":
                        dims.append(self.translate_expr(hi, arrays_1d))
                    else:
                        lo_py = self.translate_expr(lo, arrays_1d)
                        hi_py = self.translate_expr(hi, arrays_1d)
                        dims.append(f"(({hi_py}) - ({lo_py}) + 1)")
                elif hi:
                    dims.append(self.translate_expr(hi, arrays_1d))
                else:
                    dims.append(ds)
            else:
                dims.append(self.translate_expr(ds, arrays_1d))
        if len(dims) == 1:
            return dims[0]
        return "(" + ", ".join(dims) + ")"

    def translate_expr(self, expr: str, arrays_1d: set[str]) -> str:
        s = expr.strip()
        s = s.replace("%", ".")
        had_concat = False

        # Fortran string concatenation operator.
        if "//" in s:
            out_concat: list[str] = []
            i = 0
            in_str = False
            quote = ""
            while i < len(s):
                ch = s[i]
                if in_str:
                    out_concat.append(ch)
                    if ch == quote:
                        in_str = False
                    i += 1
                    continue
                if ch in ("'", '"'):
                    in_str = True
                    quote = ch
                    out_concat.append(ch)
                    i += 1
                    continue
                if i + 1 < len(s) and s[i : i + 2] == "//":
                    out_concat.append(" + ")
                    had_concat = True
                    i += 2
                    continue
                out_concat.append(ch)
                i += 1
            s = "".join(out_concat)
            if had_concat and re.match(r"^\s*\+", s):
                s = '"" ' + s

        # kind(...) used as a kind selector
        # basic rule: if it uses a d exponent constant, treat it as double precision -> 8
        s = re.sub(r"\bkind\s*\(\s*[^)]*[dD][^)]*\)\s*", "8", s, flags=re.I)
        s = re.sub(r"\breal64\b", "8", s, flags=re.I)
        s = re.sub(r"\breal32\b", "4", s, flags=re.I)

        # fortran d exponent literal -> python e exponent literal
        s = re.sub(
            r"(?i)(\d+(?:\.\d*)?|\.\d+)[d]([+-]?\d+)",
            r"\1e\2",
            s,
        )
        # fortran kind suffix literal -> plain python numeric literal
        # e.g. 1.0_dp, 2_ikind, 3.5_rk
        s = re.sub(r"(?i)\b((?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)\s*_[a-z_]\w*\b", r"\1", s)

        s = re.sub(r"\.true\.", "True", s, flags=re.I)
        s = re.sub(r"\.false\.", "False", s, flags=re.I)
        s = re.sub(r"\.eq\.", " == ", s, flags=re.I)
        s = re.sub(r"\.ne\.", " != ", s, flags=re.I)
        s = re.sub(r"\.lt\.", " < ", s, flags=re.I)
        s = re.sub(r"\.le\.", " <= ", s, flags=re.I)
        s = re.sub(r"\.gt\.", " > ", s, flags=re.I)
        s = re.sub(r"\.ge\.", " >= ", s, flags=re.I)
        s = s.replace("/=", " != ")
        s = re.sub(r"\.and\.", " and ", s, flags=re.I)
        s = re.sub(r"\.or\.", " or ", s, flags=re.I)
        s = re.sub(r"\.not\.", " not ", s, flags=re.I)

        s = re.sub(r"\bsqrt\s*\(", "np.sqrt(", s, flags=re.I)
        s = re.sub(r"\bacos\s*\(", "np.arccos(", s, flags=re.I)
        s = re.sub(r"\bcos\s*\(", "np.cos(", s, flags=re.I)
        s = re.sub(r"\bsin\s*\(", "np.sin(", s, flags=re.I)
        s = re.sub(r"\blog\s*\(", "np.log(", s, flags=re.I)
        s = re.sub(r"\bexp\s*\(", "np.exp(", s, flags=re.I)
        s = re.sub(r"\bmax\s*\(", "np.maximum(", s, flags=re.I)
        s = re.sub(r"\bmin\s*\(", "np.minimum(", s, flags=re.I)
        s = re.sub(r"\breshape\s*\(", "np.reshape(", s, flags=re.I)
        s = re.sub(r"\bspread\s*\(", "_f_spread(", s, flags=re.I)
        s = re.sub(r"\bmod\s*\(", "np.mod(", s, flags=re.I)
        s = re.sub(r"\bmaxval\s*\(", "np.max(", s, flags=re.I)
        s = re.sub(r"\bminval\s*\(", "np.min(", s, flags=re.I)
        s = re.sub(r"\bcount\s*\(", "np.count_nonzero(", s, flags=re.I)
        s = re.sub(r"\bsum\s*\(", "np.sum(", s, flags=re.I)
        s = re.sub(
            r"\ballocated\s*\(\s*([a-z_]\w*)\.([a-z_]\w*)\s*\)",
            r"(hasattr(\1, '\2') and (getattr(\1, '\2') is not None))",
            s,
            flags=re.I,
        )
        s = re.sub(r"\ballocated\s*\(\s*([^)]+?)\s*\)", r"(\1 is not None)", s, flags=re.I)
        s = re.sub(r"\bpresent\s*\(\s*([a-z_]\w*)\s*\)", r"(\1 is not None)", s, flags=re.I)
        s = re.sub(r"\btiny\s*\(\s*[^)]*\)", "np.finfo(float).tiny", s, flags=re.I)
        s = re.sub(r"\bhuge\s*\(\s*[^)]*\)", "np.finfo(float).max", s, flags=re.I)
        s = s.replace("np.np.", "np.")

        s = re.sub(r"\bsize\s*\(\s*([a-z_]\w*)\s*\)", r"_f_size(\1)", s, flags=re.I)
        s = re.sub(r"\bsize\s*\(\s*([^)]+)\s*\)", r"_f_size(\1)", s, flags=re.I)
        s = s.replace("np.np.", "np.")

        def _rewrite_np_sum_calls(txt: str) -> str:
            out: list[str] = []
            i = 0
            n = len(txt)
            while i < n:
                k = txt.lower().find("np.sum(", i)
                if k == -1:
                    out.append(txt[i:])
                    break
                out.append(txt[i:k])
                p0 = k + len("np.sum")
                if p0 >= n or txt[p0] != "(":
                    out.append(txt[k:k + 1])
                    i = k + 1
                    continue
                p1 = find_matching_paren(txt, p0)
                if p1 == -1:
                    out.append(txt[k:])
                    break
                inner = txt[p0 + 1 : p1]
                parts = [p.strip() for p in split_args(inner) if p.strip()]
                if not parts:
                    out.append("np.sum()")
                    i = p1 + 1
                    continue
                arr = parts[0]
                dim_expr = None
                mask_expr = None
                for ptxt in parts[1:]:
                    mk = re.match(r"^([a-z_]\w*)\s*=\s*(.+)$", ptxt, re.I)
                    if mk:
                        key = mk.group(1).lower()
                        val = mk.group(2).strip()
                        if key == "dim":
                            dim_expr = val
                        elif key == "mask":
                            mask_expr = val
                    elif dim_expr is None:
                        dim_expr = ptxt
                if mask_expr is not None:
                    arr_eff = f"np.where({mask_expr}, {arr}, 0.0)"
                else:
                    arr_eff = arr
                if dim_expr is not None:
                    repl = f"np.sum({arr_eff}, axis=({dim_expr}) - 1)"
                else:
                    repl = f"np.sum({arr_eff})"
                out.append(repl)
                i = p1 + 1
            return "".join(out)

        s = _rewrite_np_sum_calls(s)

        def _translate_special_call(name: str, inner: str) -> str | None:
            lname = name.lower()
            if lname not in {
                "gamma",
                "log_gamma",
                "erf",
                "erfc",
                "bessel_j0",
                "bessel_j1",
                "bessel_jn",
                "bessel_yn",
                "asinh",
                "acosh",
                "atanh",
                "hypot",
                "norm2",
                "acosd",
                "asind",
                "atand",
                "cosd",
                "sind",
                "tand",
                "acospi",
                "asinpi",
                "atanpi",
                "cospi",
                "sinpi",
                "tanpi",
                "trim",
                "real",
                "int",
            }:
                return None
            parts = [p.strip() for p in split_args(inner)]
            args_py = [self.translate_expr(p, arrays_1d) for p in parts]
            if lname == "trim" and len(args_py) == 1:
                return f"str({args_py[0]}).rstrip()"
            if lname == "real":
                if len(parts) >= 1:
                    arg0_raw = parts[0].strip()
                    arg0_py = args_py[0]
                    if (
                        arg0_raw.startswith("[")
                        or arg0_raw.startswith("(/")
                        or arg0_raw.lower() in arrays_1d
                    ):
                        return f"np.asarray({arg0_py}, dtype=np.float64)"
                    return f"np.float64({arg0_py})"
                return None
            if lname == "int":
                if len(parts) >= 1:
                    arg0_raw = parts[0].strip()
                    arg0_py = args_py[0]
                    if (
                        arg0_raw.startswith("[")
                        or arg0_raw.startswith("(/")
                        or arg0_raw.lower() in arrays_1d
                    ):
                        return f"np.asarray({arg0_py}, dtype=int)"
                    return f"int({arg0_py})"
                return None
            if lname == "gamma" and len(args_py) == 1:
                return f"sps.gamma({args_py[0]})"
            if lname == "log_gamma" and len(args_py) == 1:
                return f"sps.gammaln({args_py[0]})"
            if lname == "erf" and len(args_py) == 1:
                return f"sps.erf({args_py[0]})"
            if lname == "erfc" and len(args_py) == 1:
                return f"sps.erfc({args_py[0]})"
            if lname == "bessel_j0" and len(args_py) == 1:
                return f"sps.j0({args_py[0]})"
            if lname == "bessel_j1" and len(args_py) == 1:
                return f"sps.j1({args_py[0]})"
            if lname == "bessel_jn":
                if len(args_py) == 2:
                    return f"sps.jv({args_py[0]}, {args_py[1]})"
                if len(args_py) == 3:
                    return f"sps.jv(np.arange(int({args_py[0]}), int({args_py[1]}) + 1), {args_py[2]})"
            if lname == "bessel_yn":
                if len(args_py) == 2:
                    return f"sps.yn({args_py[0]}, {args_py[1]})"
                if len(args_py) == 3:
                    return f"sps.yn(np.arange(int({args_py[0]}), int({args_py[1]}) + 1), {args_py[2]})"
            if lname == "asinh" and len(args_py) == 1:
                return f"np.asinh({args_py[0]})"
            if lname == "acosh" and len(args_py) == 1:
                return f"np.acosh({args_py[0]})"
            if lname == "atanh" and len(args_py) == 1:
                return f"np.atanh({args_py[0]})"
            if lname == "hypot" and len(args_py) == 2:
                return f"np.hypot({args_py[0]}, {args_py[1]})"
            if lname == "norm2" and len(args_py) == 1:
                return f"np.linalg.norm({args_py[0]})"
            if lname == "acosd" and len(args_py) == 1:
                return f"np.degrees(np.arccos({args_py[0]}))"
            if lname == "asind" and len(args_py) == 1:
                return f"np.degrees(np.arcsin({args_py[0]}))"
            if lname == "atand" and len(args_py) == 1:
                return f"np.degrees(np.arctan({args_py[0]}))"
            if lname == "cosd" and len(args_py) == 1:
                return f"np.cos(np.radians({args_py[0]}))"
            if lname == "sind" and len(args_py) == 1:
                return f"np.sin(np.radians({args_py[0]}))"
            if lname == "tand" and len(args_py) == 1:
                return f"np.tan(np.radians({args_py[0]}))"
            if lname == "acospi" and len(args_py) == 1:
                return f"(np.arccos({args_py[0]}) / np.pi)"
            if lname == "asinpi" and len(args_py) == 1:
                return f"(np.arcsin({args_py[0]}) / np.pi)"
            if lname == "atanpi" and len(args_py) == 1:
                return f"(np.arctan({args_py[0]}) / np.pi)"
            if lname == "cospi" and len(args_py) == 1:
                return f"np.cos(np.pi * ({args_py[0]}))"
            if lname == "sinpi" and len(args_py) == 1:
                return f"np.sin(np.pi * ({args_py[0]}))"
            if lname == "tanpi" and len(args_py) == 1:
                return f"np.tan(np.pi * ({args_py[0]}))"
            return None

        # 1d array element: a(i) -> a[(i)-1] (assume 1-based Fortran indexing)
        # Use a scanner (not regex) so nested references like x(idx(i)+1) work.
        def _convert_refs(txt: str) -> str:
            out: list[str] = []
            i = 0
            n = len(txt)
            while i < n:
                m = re.match(r"[A-Za-z_]\w*(?:\.[A-Za-z_]\w*)*", txt[i:])
                if not m:
                    out.append(txt[i])
                    i += 1
                    continue
                name = m.group(0)
                j = i + len(name)
                k = j
                while k < n and txt[k].isspace():
                    k += 1
                if k < n and txt[k] == "(":
                    pclose = find_matching_paren(txt, k)
                    if pclose != -1:
                        inner = txt[k + 1 : pclose]
                        inner_py = self.translate_expr(inner, arrays_1d)
                        root = name.split(".", 1)[0].lower()
                        dotted_array_ref = ("." in name) and (root not in {"np", "math", "random", "sps"})
                        special_call = None if "." in name else _translate_special_call(name, inner)
                        if special_call is not None:
                            out.append(special_call)
                            i = pclose + 1
                            continue
                        if name in arrays_1d or dotted_array_ref:
                            if "," in inner:
                                parts = [p.strip() for p in split_args(inner)]
                                idx_parts: list[str] = []
                                for ptxt in parts:
                                    if ptxt == ":":
                                        idx_parts.append(":")
                                    elif ":" in ptxt:
                                        lo, hi = ptxt.split(":", 1)
                                        lo = lo.strip()
                                        hi = hi.strip()
                                        lo_py = self.translate_expr(lo, arrays_1d) if lo else ""
                                        hi_py = self.translate_expr(hi, arrays_1d) if hi else ""
                                        start = f"(int({lo_py}) - 1)" if lo_py else ""
                                        stop = f"int({hi_py})" if hi_py else ""
                                        idx_parts.append(f"{start}:{stop}")
                                    else:
                                        idx_parts.append(f"({self.translate_expr(ptxt, arrays_1d)}) - 1")
                                out.append(f"{name}[{', '.join(idx_parts)}]")
                                i = pclose + 1
                                continue
                            if ":" in inner:
                                lo, hi = inner.split(":", 1)
                                lo = lo.strip()
                                hi = hi.strip()
                                lo_py = self.translate_expr(lo, arrays_1d) if lo else ""
                                hi_py = self.translate_expr(hi, arrays_1d) if hi else ""
                                start = f"(int({lo_py}) - 1)" if lo_py else ""
                                stop = f"int({hi_py})" if hi_py else ""
                                out.append(f"{name}[{start}:{stop}]")
                            else:
                                out.append(f"{name}[({inner_py}) - 1]")
                        else:
                            if ":" in inner and "," not in inner:
                                lo, hi = inner.split(":", 1)
                                lo = lo.strip()
                                hi = hi.strip()
                                lo_py = self.translate_expr(lo, arrays_1d) if lo else ""
                                hi_py = self.translate_expr(hi, arrays_1d) if hi else ""
                                start = f"(int({lo_py}) - 1)" if lo_py else ""
                                stop = f"int({hi_py})" if hi_py else ""
                                out.append(f"{name}[{start}:{stop}]")
                            else:
                                out.append(f"{name}({inner_py})")
                        i = pclose + 1
                        continue
                root = name.split(".", 1)[0].lower()
                if "." in name and root in arrays_1d and root not in {"np", "math", "random", "sps"}:
                    root_name, path_name = name.split(".", 1)
                    out.append(f"_xf2p_component_array({root_name}, {path_name!r})")
                else:
                    out.append(name)
                i = j
            return "".join(out)

        s = _convert_refs(s)
        return s

    def transpile_assignment(self, lhs: str, rhs_py: str, arrays_1d: set[str]) -> None:
        lhs = lhs.replace("%", ".")
        lhs_base = lhs.split(".", 1)[0].strip().lower()
        if self._decl_types.get(lhs_base) == "integer":
            rhs_py = f"int({rhs_py})"
        mname = re.match(r"^\s*([a-z_]\w*(?:\.[a-z_]\w*)*)\s*\(", lhs, re.I)
        idx_name = None
        idx = None
        if mname:
            name_try = mname.group(1)
            open_pos = lhs.find("(", mname.end(1))
            if open_pos >= 0:
                close_pos = find_matching_paren(lhs, open_pos)
                if close_pos == len(lhs) - 1:
                    idx_name = name_try
                    idx = lhs[open_pos + 1 : close_pos].strip()
        if idx_name is not None and idx is not None:
            name = idx_name
            root = name.split(".", 1)[0].lower()
            dotted_array_ref = ("." in name) and (root not in {"np", "math", "random", "sps"})
            if name in arrays_1d or dotted_array_ref:
                if "," in idx:
                    parts = [p.strip() for p in split_args(idx)]
                    idx_parts: list[str] = []
                    for ptxt in parts:
                        if ptxt == ":":
                            idx_parts.append(":")
                        elif ":" in ptxt:
                            lo, hi = ptxt.split(":", 1)
                            lo = lo.strip()
                            hi = hi.strip()
                            lo_py = self.translate_expr(lo, arrays_1d) if lo else ""
                            hi_py = self.translate_expr(hi, arrays_1d) if hi else ""
                            start = f"(int({lo_py}) - 1)" if lo_py else ""
                            stop = f"int({hi_py})" if hi_py else ""
                            idx_parts.append(f"{start}:{stop}")
                        else:
                            idx_parts.append(f"({self.translate_expr(ptxt, arrays_1d)}) - 1")
                    self.emit(f"{name}[{', '.join(idx_parts)}] = {rhs_py}")
                    return
                if ":" in idx:
                    lo, hi = idx.split(":", 1)
                    lo = lo.strip()
                    hi = hi.strip()
                    lo_py = self.translate_expr(lo, arrays_1d) if lo else ""
                    hi_py = self.translate_expr(hi, arrays_1d) if hi else ""
                    start = f"(int({lo_py}) - 1)" if lo_py else ""
                    stop = f"int({hi_py})" if hi_py else ""
                    self.emit(f"{name}[{start}:{stop}] = {rhs_py}")
                else:
                    idx_py = self.translate_expr(idx, arrays_1d)
                    self.emit(f"{name}[({idx_py}) - 1] = {rhs_py}")
                return
        if lhs in arrays_1d:
            self.emit(f"{lhs} = _f_assign_array({lhs}, {rhs_py})")
            return
        self.emit(f"{lhs} = {rhs_py}")

    def transpile_simple_stmt(self, stmt: str, arrays_1d: set[str]) -> None:
        s = stmt.strip()
        if self.handle_exec_line(s, arrays_1d):
            return
        mm = re.match(r'error\s+stop\s+(.+)$', s, re.I)
        if mm:
            self.emit(f"raise RuntimeError({mm.group(1).strip()})")
            return
        if s.lower() == "return":
            if self._current_result_name:
                self.emit(f"return {self._current_result_name}")
            else:
                self.emit("return")
            return
        if "=" in s and "::" not in s:
            lhs, rhs = s.split("=", 1)
            lhs = lhs.strip()
            rhs_py = self.translate_expr(rhs, arrays_1d)
            if lhs in arrays_1d and rhs_py in ("True", "False"):
                self.emit(f"{lhs}[:] = {rhs_py}")
                return
            self.transpile_assignment(lhs, rhs_py, arrays_1d)
        else:
            self.emit("# unsupported inline statement")
            self.emit("pass")

    def emit_parameters_from_decl(self, ftype: str, attrs_l: str, rest: str, arrays_1d: set[str]) -> set[str]:
        names: set[str] = set()
        if "parameter" not in attrs_l:
            return names
        if ftype == "type":
            return names
        for name, shape, init in parse_decl_items(rest):
            if init is None:
                continue
            # parameters are named constants: method (2) -> Final
            hint = _type_scalar_hint.get(ftype, "int")
            val = self.translate_expr(init, arrays_1d)
            self.emit(f"{name}: Final[{hint}] = {val}")
            names.add(name)
        return names

    def emit_var_inits_from_sym(
        self,
        sym: dict[str, dict],
        arrays_1d: set[str],
        parameter_names: set[str],
        skip_names: set[str] | None = None,
    ) -> None:
        # allocate explicit-shape arrays and initialize scalars so python is always valid
        if skip_names is None:
            skip_names = set()
        for name, info in sym.items():
            if name in parameter_names:
                continue
            if name in skip_names:
                continue
            ftype = info["ftype"]
            ftype_name = info.get("type_name")
            is_array = info["is_array"]
            shape = info["shape"]
            init = info["init"]
            alloc = info["alloc"]

            if is_array:
                # only allocate explicit-shape, non-allocatable arrays
                if alloc:
                    self.emit(f"{name} = None")
                    continue
                if shape is None:
                    continue
                if shape.strip() == ":":
                    continue
                if ftype == "type":
                    shape_py = self._shape_to_py(shape, arrays_1d)
                    self.emit(f"{name} = np.empty({shape_py}, dtype=object)")
                    continue
                dtype = _type_dtype[ftype]
                shape_py = self._shape_to_py(shape, arrays_1d)
                hint = _type_ndarray_hint[ftype]
                if init is None:
                    self.emit(f"{name}: {hint} = np.empty({shape_py}, dtype={dtype})")
                else:
                    init_py = self.translate_expr(init, arrays_1d)
                    self.emit(f"{name}: {hint} = np.full({shape_py}, {init_py}, dtype={dtype})")
            else:
                # scalar
                if ftype == "type":
                    cls = ftype_name if ftype_name else "SimpleNamespace"
                    self.emit(f"{name} = {cls}()")
                    continue
                hint = _type_scalar_hint.get(ftype, "int")
                if init is None:
                    default_val = _type_default_scalar_value.get(ftype, "0")
                    self.emit(f"{name}: {hint} = {default_val}")
                else:
                    init_py = self.translate_expr(init, arrays_1d)
                    self.emit(f"{name}: {hint} = {init_py}")

    def handle_exec_line(self, s: str, arrays_1d: set[str]) -> bool:
        sl = s.lower()

        # ignore some non-exec lines
        if sl in ("implicit none", "contains"):
            return True
        if sl.startswith("use "):
            return True
        if sl.startswith("end function") or sl.startswith("end program") or sl.startswith("end module"):
            return True
        if sl == "end":
            return True

        # if (...) then
        mm = re.match(r"if\s*\(\s*(.+)\s*\)\s*then$", s, re.I)
        if mm:
            cond = self.translate_expr(mm.group(1), arrays_1d)
            self.emit(f"if {cond}:")
            self.indent += 1
            self._block_code_start.append(self._code_emit_count)
            return True

        mm = re.match(r"else\s+if\s*\(\s*(.+)\s*\)\s*then$", s, re.I)
        if mm:
            cond = self.translate_expr(mm.group(1), arrays_1d)
            self.indent = max(0, self.indent - 1)
            self.emit(f"elif {cond}:")
            self.indent += 1
            return True

        if sl == "else":
            self.indent = max(0, self.indent - 1)
            self.emit("else:")
            self.indent += 1
            return True

        if sl.startswith("end if"):
            if self._block_code_start:
                start = self._block_code_start.pop()
                if self._code_emit_count == start:
                    self.emit("pass")
            self.indent = max(0, self.indent - 1)
            return True

        # do while (...)
        mm = re.match(r"do\s+while\s*\(\s*(.+)\s*\)$", s, re.I)
        if mm:
            cond = self.translate_expr(mm.group(1), arrays_1d)
            self.emit(f"while {cond}:")
            self.indent += 1
            self._block_code_start.append(self._code_emit_count)
            return True

        # do i = a, b
        mm = re.match(r"do\s+([a-z_]\w*)\s*=\s*(.+?)\s*,\s*(.+)$", s, re.I)
        if mm:
            var = mm.group(1)
            a = self.translate_expr(mm.group(2), arrays_1d)
            b = self.translate_expr(mm.group(3), arrays_1d)
            self.emit(f"for {var} in range({a}, ({b}) + 1):")
            self.indent += 1
            self._block_code_start.append(self._code_emit_count)
            return True

        if sl.startswith("end do"):
            if self._block_code_start:
                start = self._block_code_start.pop()
                if self._code_emit_count == start:
                    self.emit("pass")
            self.indent = max(0, self.indent - 1)
            return True

        # allocate(a(n))
        mm = re.match(r"allocate\s*\(\s*([a-z_]\w*)\s*\(\s*(.+)\s*\)\s*\)\s*$", s, re.I)
        if mm:
            name = mm.group(1)
            sz = self._shape_to_py(mm.group(2), arrays_1d)
            ftype = self._decl_types.get(name.lower(), "")
            if not ftype:
                ftype = self._decl_array_types.get(name.lower(), "")
            if ftype == "integer":
                self.emit(f"{name} = np.empty({sz}, dtype=int)")
            elif ftype == "logical":
                self.emit(f"{name} = np.empty({sz}, dtype=bool)")
            elif ftype == "real":
                self.emit(f"{name} = np.empty({sz}, dtype=np.float64)")
            else:
                self.emit(f"{name} = np.empty({sz})")
            return True

        # open(newunit=fp, file="temp.txt", ...) or open(unit=iu, file="temp.txt", ...)
        mm = re.match(r"open\s*\(\s*(.+)\s*\)\s*$", s, re.I)
        if mm:
            spec = mm.group(1).strip()
            unit_m = re.search(r"\b(?:newunit|unit)\s*=\s*([^,]+)", spec, re.I)
            file_m = re.search(r"\bfile\s*=\s*(.+?)(?:,\s*[a-z_]\w*\s*=|$)", spec, re.I)
            status_m = re.search(r"\bstatus\s*=\s*(.+?)(?:,\s*[a-z_]\w*\s*=|$)", spec, re.I)
            action_m = re.search(r"\baction\s*=\s*(.+?)(?:,\s*[a-z_]\w*\s*=|$)", spec, re.I)
            position_m = re.search(r"\bposition\s*=\s*(.+?)(?:,\s*[a-z_]\w*\s*=|$)", spec, re.I)
            unit_var = unit_m.group(1).strip() if unit_m else None
            file_expr = file_m.group(1).strip() if file_m else None
            status = status_m.group(1).strip() if status_m else None
            action = action_m.group(1).strip() if action_m else None
            position = position_m.group(1).strip() if position_m else None
            if unit_var is None or file_expr is None:
                self.emit(f"# unsupported open form: {s}")
                return True
            mode = "r"
            st = status.strip().strip("'\"").lower() if status else ""
            act = action.strip().strip("'\"").lower() if action else ""
            pos = position.strip().strip("'\"").lower() if position else ""
            if act == "write" or st == "replace":
                mode = "w"
            if pos == "append":
                mode = "a"
            self.emit(f"{unit_var} = open({self.translate_expr(file_expr, arrays_1d)}, {mode!r})")
            return True

        # close(fp)
        mm = re.match(r"close\s*\(\s*(.+?)\s*\)\s*$", s, re.I)
        if mm:
            unit = self.translate_expr(mm.group(1), arrays_1d)
            self.emit(f"{unit}.close()")
            return True

        # call random_number(x)
        mm = re.match(r"call\s+random_number\s*\(\s*([a-z_]\w*)\s*\)\s*$", s, re.I)
        if mm:
            name = mm.group(1)
            if name in arrays_1d:
                self.emit(f"{name} = _f_assign_array({name}, np.random.random(size={name}.shape))")
            else:
                self.emit(f"{name} = np.float64(np.random.random())")
            return True

        # open(newunit=fp, file="temp.txt", ...) or open(unit=iu, file="temp.txt", ...)
        mm = re.match(r"open\s*\(\s*(.+)\s*\)\s*$", s, re.I)
        if mm:
            spec = mm.group(1).strip()
            unit_m = re.search(r"\b(?:newunit|unit)\s*=\s*([^,]+)", spec, re.I)
            file_m = re.search(r"\bfile\s*=\s*(.+?)(?:,\s*[a-z_]\w*\s*=|$)", spec, re.I)
            status_m = re.search(r"\bstatus\s*=\s*(.+?)(?:,\s*[a-z_]\w*\s*=|$)", spec, re.I)
            action_m = re.search(r"\baction\s*=\s*(.+?)(?:,\s*[a-z_]\w*\s*=|$)", spec, re.I)
            position_m = re.search(r"\bposition\s*=\s*(.+?)(?:,\s*[a-z_]\w*\s*=|$)", spec, re.I)
            unit_var = unit_m.group(1).strip() if unit_m else None
            file_expr = file_m.group(1).strip() if file_m else None
            status = status_m.group(1).strip() if status_m else None
            action = action_m.group(1).strip() if action_m else None
            position = position_m.group(1).strip() if position_m else None
            if unit_var is None or file_expr is None:
                self.emit(f"# unsupported open form: {s}")
                return True
            mode = "r"
            st = status.strip().strip("'\"").lower() if status else ""
            act = action.strip().strip("'\"").lower() if action else ""
            pos = position.strip().strip("'\"").lower() if position else ""
            if act == "write" or st == "replace":
                mode = "w"
            if pos == "append":
                mode = "a"
            self.emit(f"{unit_var} = open({self.translate_expr(file_expr, arrays_1d)}, {mode!r})")
            return True

        # close(fp)
        mm = re.match(r"close\s*\(\s*(.+?)\s*\)\s*$", s, re.I)
        if mm:
            unit = self.translate_expr(mm.group(1), arrays_1d)
            self.emit(f"{unit}.close()")
            return True

        # read(unit, "(a)", iostat=ios) text
        if sl.startswith("read"):
            p0 = s.find("(")
            if p0 != -1:
                p1 = find_matching_paren(s, p0)
                if p1 != -1:
                    spec = s[p0 + 1 : p1].strip()
                    rest = s[p1 + 1 :].strip()
                    parts = split_args(spec)
                    if len(parts) >= 2:
                        unit = self.translate_expr(parts[0].strip(), arrays_1d)
                        fmt = parts[1].strip()
                        kws = {}
                        for p in parts[2:]:
                            mk = re.match(r"^\s*([a-z_]\w*)\s*=\s*(.+?)\s*$", p, re.I)
                            if mk:
                                kws[mk.group(1).lower()] = mk.group(2).strip()
                        if fmt != "*":
                            fmt_txt = _fortran_unquote(fmt).strip().lower() if (len(fmt) >= 2 and fmt[0] in ("'", '"') and fmt[-1] == fmt[0]) else ""
                            if fmt_txt in {"(a)", "a"} and rest:
                                ios_var = kws.get("iostat")
                                lhs = self.translate_expr(rest, arrays_1d)
                                self.emit(f"__xf2p_line = {unit}.readline()")
                                if ios_var is not None:
                                    self.emit(f"{ios_var} = 0 if __xf2p_line != '' else -1")
                                self.emit(f"{lhs} = __xf2p_line.rstrip('\\n')")
                                return True
        # read(fp,*) a, b, arr(i,:), ...
        mm = re.match(r"read\s*\(\s*([a-z_]\w*)\s*,\s*\*\s*\)\s*(.+)$", s, re.I)
        if mm:
            unit = mm.group(1)
            rest = mm.group(2).strip()
            items = [a.strip() for a in split_args(rest) if a.strip()]
            if len(items) == 1:
                tgt = items[0]
                # row-slice target: mat(i,:)
                mrow = re.match(r"^([a-z_]\w*)\s*\(\s*([^,]+)\s*,\s*:\s*\)\s*$", tgt, re.I)
                if mrow:
                    arr = mrow.group(1)
                    ridx = self.translate_expr(mrow.group(2), arrays_1d)
                    ftype = self._decl_array_types.get(arr.lower(), "")
                    cast = "int" if ftype == "integer" else "float"
                    self.emit(f"__read_vals = [{cast}(v) for v in {unit}.readline().split()]")
                    self.emit(f"{arr}[({ridx}) - 1, :] = np.asarray(__read_vals[:{arr}.shape[1]], dtype={arr}.dtype)")
                    return True
                # scalar target
                lhs = self.translate_expr(tgt, arrays_1d)
                base = re.match(r"^([a-z_]\w*)", tgt, re.I)
                bnm = base.group(1).lower() if base else ""
                ftype = self._decl_types.get(bnm, "")
                cast = "int" if ftype == "integer" else ("bool" if ftype == "logical" else "float")
                self.emit(f"{lhs} = {cast}({unit}.readline().split()[0])")
                return True
            # multiple scalar targets
            self.emit(f"__read_parts = {unit}.readline().split()")
            for i, tgt in enumerate(items):
                lhs = self.translate_expr(tgt, arrays_1d)
                base = re.match(r"^([a-z_]\w*)", tgt, re.I)
                bnm = base.group(1).lower() if base else ""
                ftype = self._decl_types.get(bnm, "")
                cast = "int" if ftype == "integer" else ("bool" if ftype == "logical" else "float")
                self.emit(f"{lhs} = {cast}(__read_parts[{i}])")
            return True

        # call foo(...)
        mm = re.match(r"call\s+([a-z_]\w*)\s*\((.*)\)\s*$", s, re.I)
        if mm:
            cname = mm.group(1)
            cname_l = cname.lower()
            argtxt = mm.group(2).strip()
            args_py: list[str] = []
            kwargs_py: dict[str, str] = {}
            if argtxt:
                for a in split_args(argtxt):
                    a = a.strip()
                    mk = re.match(r"^([a-z_]\w*)\s*=\s*(.+)$", a, re.I)
                    if mk:
                        kwargs_py[mk.group(1).lower()] = self.translate_expr(mk.group(2), arrays_1d)
                    else:
                        args_py.append(self.translate_expr(a, arrays_1d))
            # Fortran random_seed adapters for valid Python
            if cname.lower() == "random_seed":
                if "size" in kwargs_py:
                    self.emit(f"{kwargs_py['size']} = 1")
                elif "put" in kwargs_py:
                    self.emit(f"np.random.seed(int(np.sum({kwargs_py['put']})) % (2**32 - 1))")
                else:
                    self.emit("np.random.seed(None)")
                return True
            merged = list(args_py)
            for k, v in kwargs_py.items():
                merged.append(f"{k}={v}")
            sig = self._subr_sigs.get(cname_l)
            if sig:
                formal_args = sig.get("args", [])
                out_formals = sig.get("out", [])
                if kwargs_py == {} and len(formal_args) > 0 and len(args_py) > len(formal_args):
                    # Robustness for malformed Fortran with duplicated actual arguments.
                    args_py = args_py[: len(formal_args)]
                    merged = list(args_py)
                actual_by_formal: dict[str, str] = {}
                for i, formal in enumerate(formal_args):
                    if i < len(args_py):
                        actual_by_formal[formal] = args_py[i]
                for k, v in kwargs_py.items():
                    actual_by_formal[k] = v
                out_actuals = [actual_by_formal.get(fm, "") for fm in out_formals]
                if out_actuals and all(x != "" for x in out_actuals):
                    self.emit(f"{', '.join(out_actuals)} = {cname}({', '.join(merged)})")
                    return True
            self.emit(f"{cname}({', '.join(merged)})")
            return True

        # return
        if sl == "return":
            if self._current_result_name:
                self.emit(f"return {self._current_result_name}")
            else:
                self.emit("return")
            return True
        if sl == "exit":
            self.emit("break")
            return True
        mm = re.match(r"(?:error\s+)?stop(?:\s+(.+))?$", s, re.I)
        if mm:
            msg = (mm.group(1) or '"stop"').strip()
            self.emit(f"raise RuntimeError({msg})")
            return True

        # print *, ...
        mm = re.match(r"print\s*\*\s*,\s*(.+)$", s, re.I)
        if mm:
            raw_args = [a.strip() for a in split_args(mm.group(1))]
            if len(raw_args) == 1:
                a0 = raw_args[0].strip()
                implied_py = _fortran_implied_do_expr(a0, self.translate_expr, arrays_1d)
                if implied_py is not None:
                    self.emit(f"print(*{implied_py})")
                    return True
                if re.fullmatch(r"[a-z_]\w*", a0, flags=re.I) and a0.lower() in self._decl_array_types:
                    self.emit(f"print(*np.ravel({a0}, order='F'))")
                    return True
            args2 = []
            for a in raw_args:
                if a.startswith(("'", '"')):
                    args2.append(a)
                else:
                    implied_py = _fortran_implied_do_expr(a, self.translate_expr, arrays_1d)
                    if implied_py is not None:
                        args2.append(f"*{implied_py}")
                    else:
                        args2.append(self.translate_expr(a, arrays_1d))
            self.emit(f"print({', '.join(args2)})")
            return True

        # print "fmt", ...
        mm = re.match(r"print\s*((\"|').*?\2)\s*,\s*(.+)$", s, re.I)
        if mm:
            fmt = mm.group(1)
            raw_args = [a.strip() for a in split_args(mm.group(3))]
            args2 = []
            for a in raw_args:
                if a.startswith(("'", "\"")):
                    args2.append(a)
                else:
                    implied_py = _fortran_implied_do_expr(a, self.translate_expr, arrays_1d)
                    if implied_py is not None:
                        args2.append(implied_py)
                    else:
                        args2.append(self.translate_expr(a, arrays_1d))
            if len(raw_args) == 1:
                a0 = raw_args[0].strip()
                if _is_recyclable_io_iterable(a0, self._decl_array_types):
                    fmt_expr = _fortran_format_recycled_expr(fmt, args2[0])
                    if fmt_expr is not None:
                        self.emit(f"print({fmt_expr})")
                        return True
            if len(raw_args) > 1:
                plan = _single_iterable_formatted_arg_plan(fmt, raw_args, self._decl_array_types)
                if plan is not None:
                    iter_pos, iter_need = plan
                    iter_raw = raw_args[iter_pos].strip()
                    iter_tmp = f"_xf2p_fmt_items_{self._code_emit_count + 1}"
                    if re.fullmatch(r"[a-z_]\w*", iter_raw, flags=re.I) and iter_raw.lower() in self._decl_array_types:
                        self.emit(f"{iter_tmp} = list(np.ravel({args2[iter_pos]}, order='F'))")
                    else:
                        self.emit(f"{iter_tmp} = list({args2[iter_pos]})")
                    expanded_args = args2[:iter_pos] + [f"{iter_tmp}[{k}]" for k in range(iter_need)] + args2[iter_pos + 1:]
                    fmt_expr = _fortran_format_expr(fmt, expanded_args)
                    if fmt_expr is not None:
                        self.emit(f"print({fmt_expr})")
                        return True
            fmt_expr = _fortran_format_expr(fmt, args2)
            if fmt_expr is not None:
                self.emit(f"print({fmt_expr})")
            else:
                self.emit(f"print({', '.join(args2)})")
            return True

        # write(*,*) ...
        mm = re.match(r"write\s*\(\s*\*\s*,\s*\*\s*\)\s*(.+)$", s, re.I)
        if mm:
            raw_args = [a.strip() for a in split_args(mm.group(1))]
            if len(raw_args) == 1:
                a0 = raw_args[0].strip()
                implied_py = _fortran_implied_do_expr(a0, self.translate_expr, arrays_1d)
                if implied_py is not None:
                    self.emit(f"print(*{implied_py})")
                    return True
                if re.fullmatch(r"[a-z_]\w*", a0, flags=re.I) and a0.lower() in self._decl_array_types:
                    self.emit(f"print(*np.ravel({a0}, order='F'))")
                    return True
            args2 = []
            for a in raw_args:
                if a.startswith(("'", "\"")):
                    args2.append(a)
                else:
                    implied_py = _fortran_implied_do_expr(a, self.translate_expr, arrays_1d)
                    if implied_py is not None:
                        args2.append(f"*{implied_py}")
                    else:
                        args2.append(self.translate_expr(a, arrays_1d))
            self.emit(f"print({', '.join(args2)})")
            return True

        # write(unit,*) ...
        mm = re.match(r"write\s*\(\s*([a-z_]\w*)\s*,\s*\*\s*\)\s*(.*)$", s, re.I)
        if mm:
            unit = mm.group(1)
            rest = mm.group(2).strip()
            if not rest:
                self.emit(f"print(file={unit})")
                return True
            args2 = []
            raw_args = [a.strip() for a in split_args(rest)]
            if len(raw_args) == 1:
                implied_py = _fortran_implied_do_expr(raw_args[0], self.translate_expr, arrays_1d)
                if implied_py is not None:
                    self.emit(f"print(*{implied_py}, file={unit})")
                    return True
            for a in raw_args:
                if a.startswith(("'", "\"")):
                    args2.append(a)
                else:
                    implied_py = _fortran_implied_do_expr(a, self.translate_expr, arrays_1d)
                    if implied_py is not None:
                        args2.append(f"*{implied_py}")
                    else:
                        args2.append(self.translate_expr(a, arrays_1d))
            self.emit(f"print({', '.join(args2)}, file={unit})")
            return True

        # write(*,"fmt") ...
        mm = re.match(r"write\s*\(\s*\*\s*,\s*(([\"']).*?\2)\s*\)\s*(.*)$", s, re.I)
        if mm:
            fmt = mm.group(1)
            rest = mm.group(3).strip()
            if not rest:
                self.emit("print()")
                return True
            raw_args = [a.strip() for a in split_args(rest)]
            args2 = []
            for a in raw_args:
                if a.startswith(("'", "\"")):
                    args2.append(a)
                else:
                    implied_py = _fortran_implied_do_expr(a, self.translate_expr, arrays_1d)
                    if implied_py is not None:
                        args2.append(implied_py)
                    else:
                        args2.append(self.translate_expr(a, arrays_1d))
            if len(raw_args) == 1:
                a0 = raw_args[0].strip()
                if _is_recyclable_io_iterable(a0, self._decl_array_types):
                    fmt_expr = _fortran_format_recycled_expr(fmt, args2[0])
                    if fmt_expr is not None:
                        self.emit(f"print({fmt_expr})")
                        return True
            if len(raw_args) > 1:
                plan = _single_iterable_formatted_arg_plan(fmt, raw_args, self._decl_array_types)
                if plan is not None:
                    iter_pos, iter_need = plan
                    iter_raw = raw_args[iter_pos].strip()
                    iter_tmp = f"_xf2p_fmt_items_{self._code_emit_count + 1}"
                    if re.fullmatch(r"[a-z_]\w*", iter_raw, flags=re.I) and iter_raw.lower() in self._decl_array_types:
                        self.emit(f"{iter_tmp} = list(np.ravel({args2[iter_pos]}, order='F'))")
                    else:
                        self.emit(f"{iter_tmp} = list({args2[iter_pos]})")
                    expanded_args = args2[:iter_pos] + [f"{iter_tmp}[{k}]" for k in range(iter_need)] + args2[iter_pos + 1:]
                    fmt_expr = _fortran_format_expr(fmt, expanded_args)
                    if fmt_expr is not None:
                        self.emit(f"print({fmt_expr})")
                        return True
            fmt_expr = _fortran_format_expr(fmt, args2)
            if fmt_expr is not None:
                self.emit(f"print({fmt_expr})")
            else:
                self.emit(f"print({', '.join(args2)})")
            return True

        # write(unit,"fmt") ...
        mm = re.match(r"write\s*\(\s*([^,]+?)\s*,\s*(([\"']).*?\3)\s*\)\s*(.*)$", s, re.I)
        if mm:
            unit = mm.group(1).strip()
            fmt = mm.group(2)
            rest = mm.group(4).strip()
            if not rest:
                self.emit(f"print(file={unit})")
                return True
            raw_args = [a.strip() for a in split_args(rest)]
            args2 = []
            for a in raw_args:
                if a.startswith(("'", "\"")):
                    args2.append(a)
                else:
                    implied_py = _fortran_implied_do_expr(a, self.translate_expr, arrays_1d)
                    if implied_py is not None:
                        args2.append(implied_py)
                    else:
                        args2.append(self.translate_expr(a, arrays_1d))
            if len(raw_args) == 1:
                a0 = raw_args[0].strip()
                if _is_recyclable_io_iterable(a0, self._decl_array_types):
                    fmt_expr = _fortran_format_recycled_expr(fmt, args2[0])
                    if fmt_expr is not None:
                        self.emit(f"print({fmt_expr}, file={unit})")
                        return True
            if len(raw_args) > 1:
                plan = _single_iterable_formatted_arg_plan(fmt, raw_args, self._decl_array_types)
                if plan is not None:
                    iter_pos, iter_need = plan
                    iter_raw = raw_args[iter_pos].strip()
                    iter_tmp = f"_xf2p_fmt_items_{self._code_emit_count + 1}"
                    if re.fullmatch(r"[a-z_]\w*", iter_raw, flags=re.I) and iter_raw.lower() in self._decl_array_types:
                        self.emit(f"{iter_tmp} = list(np.ravel({args2[iter_pos]}, order='F'))")
                    else:
                        self.emit(f"{iter_tmp} = list({args2[iter_pos]})")
                    expanded_args = args2[:iter_pos] + [f"{iter_tmp}[{k}]" for k in range(iter_need)] + args2[iter_pos + 1:]
                    fmt_expr = _fortran_format_expr(fmt, expanded_args)
                    if fmt_expr is not None:
                        self.emit(f"print({fmt_expr}, file={unit})")
                        return True
            fmt_expr = _fortran_format_expr(fmt, args2)
            if fmt_expr is not None:
                self.emit(f"print({fmt_expr}, file={unit})")
            else:
                self.emit(f"print({', '.join(args2)}, file={unit})")
            return True

        # generic write(...) ... fallback
        mm = re.match(r"write\s*\(\s*(.*?)\s*\)\s*(.*)$", s, re.I)
        if mm:
            ctl = mm.group(1).strip().lower()
            rest = mm.group(2).strip()
            ctl_parts = split_args(ctl)
            unit_expr = "*"
            if ctl_parts:
                unit_expr = ctl_parts[0].strip()
            args2 = []
            if rest:
                for a in split_args(rest):
                    a = a.strip()
                    if a.startswith(("'", '"')):
                        args2.append(a)
                    else:
                        args2.append(self.translate_expr(a, arrays_1d))
            end_txt = ', end=""' if "advance" in ctl and "'no'" in ctl else ""
            file_txt = ""
            if unit_expr != "*":
                file_txt = f", file={unit_expr}"
            if args2:
                self.emit(f"print({', '.join(args2)}{end_txt}{file_txt})")
            else:
                if file_txt:
                    self.emit(f"print({file_txt[2:]})")
                else:
                    self.emit("print()")
            return True

        # single-line if: if (cond) stmt
        if sl.startswith("if") and not sl.endswith("then"):
            p0 = s.find("(")
            if p0 != -1:
                p1 = find_matching_paren(s, p0)
                if p1 != -1:
                    cond_txt = s[p0 + 1 : p1].strip()
                    stmt = s[p1 + 1 :].strip()
                    if stmt:
                        cond = self.translate_expr(cond_txt, arrays_1d)
                        self.emit(f"if {cond}:")
                        self.indent += 1
                        self.transpile_simple_stmt(stmt, arrays_1d)
                        self.indent = max(0, self.indent - 1)
                        return True

        # assignment
        if "=" in s and "::" not in s:
            lhs, rhs = s.split("=", 1)
            lhs = lhs.strip()
            rhs_py = self.translate_expr(rhs, arrays_1d)
            if lhs in arrays_1d:
                self.emit(f"{lhs} = _f_assign_array({lhs}, {rhs_py})")
                return True
            self.transpile_assignment(lhs, rhs_py, arrays_1d)
            return True

        return False

    def transpile_function(self, header: str, body_lines: list[tuple[str, str]]) -> None:
        hdr = header.strip()
        # tolerate arbitrary prefixes such as:
        # "pure real(kind=dp) function f(...)" or "real(kind=dp) pure function f(...)"
        m = re.search(r"\bfunction\s+(\w+)\s*\(\s*([^\)]*)\s*\)\s*result\s*\(\s*(\w+)\s*\)", hdr, re.I)
        if m:
            fname = m.group(1)
            args = [a.strip() for a in m.group(2).split(",") if a.strip()]
            result_name = m.group(3)
        else:
            m2 = re.search(r"\bfunction\s+(\w+)\s*\(\s*([^\)]*)\s*\)", hdr, re.I)
            if not m2:
                return
            fname = m2.group(1)
            args = [a.strip() for a in m2.group(2).split(",") if a.strip()]
            result_name = fname

        main_lines, internals = self._split_internal_subprograms(body_lines)
        self._validate_unit_symbols("function", fname, args, main_lines)

        sym: dict[str, dict] = {}
        arrays_1d: set[str] = set()
        arg_hints: dict[str, str] = {}
        header_ftype = infer_function_result_ftype(hdr)
        result_hint = _type_scalar_hint.get(header_ftype, "int")
        result_is_scalar = True
        result_is_derived = False
        result_type_name: str | None = None

        self._emit_intrinsic_use_aliases(main_lines)

        # declarations pass
        for code, _comment in main_lines:
            s = code.strip()
            pd = parse_decl(s)
            if pd:
                ftype, attrs, rest = pd
                attrs_l = attrs.lower()
                items = parse_decl_items(rest, parse_decl_attr_dimension(attrs))
                type_name = None
            else:
                td = re.match(r"^type\s*\(\s*([a-z_]\w*)\s*\)\s*(.*?)::\s*(.*)$", s, re.I)
                if not td:
                    continue
                ftype = "type"
                attrs_l = td.group(2).strip().lower()
                items = parse_decl_items(td.group(3).strip(), parse_decl_attr_dimension(td.group(2).strip()))
                type_name = td.group(1)

            for name, shape, init in items:
                is_array = shape is not None
                is_alloc = "allocatable" in attrs_l
                sym[name] = {
                    "ftype": ftype,
                    "type_name": type_name,
                    "is_array": is_array,
                    "shape": shape,
                    "init": init,
                    "alloc": is_alloc,
                    "attrs_l": attrs_l,
                }
                if is_array:
                    arrays_1d.add(name)

                if name in args and "intent(in" in attrs_l:
                    if is_array:
                        arg_hints[name] = self._type_hint(ftype, type_name, is_array=True)
                    else:
                        arg_hints[name] = self._type_hint(ftype, type_name, is_array=False)

                if name == result_name:
                    if is_array:
                        result_hint = _type_ndarray_hint.get(ftype, "npt.NDArray[np.float64]")
                        result_is_scalar = False
                    else:
                        if ftype == "type":
                            result_hint = self._type_hint(ftype, type_name, is_array=False)
                            result_is_scalar = False
                            result_is_derived = True
                            result_type_name = type_name
                        else:
                            result_hint = _type_scalar_hint[ftype]
                            result_is_scalar = True

        args_annot = []
        for a in args:
            hint = arg_hints.get(a, "int")
            default_none = False
            info = sym.get(a)
            if info and "optional" in info.get("attrs_l", ""):
                default_none = True
            if default_none:
                args_annot.append(f"{a}: {hint} = None")
            else:
                args_annot.append(f"{a}: {hint}")

        self.emit(f"def {fname}({', '.join(args_annot)}) -> {result_hint}:")
        self.indent += 1

        # If the first non-code lines are comments (just after signature),
        # emit them as a Python docstring.
        lead_doc: list[str] = []
        lead_idx = 0
        for code, comment in main_lines:
            if code.strip():
                break
            if comment.strip():
                lead_doc.append(comment.strip())
            lead_idx += 1
        if lead_doc:
            if len(lead_doc) == 1:
                self.emit(f'"""{lead_doc[0]}"""')
            else:
                self.emit('"""')
                for ln in lead_doc:
                    self.emit(ln)
                self.emit('"""')

        for kind, header, ibody in internals:
            if kind == "function":
                self.transpile_function(header, ibody)
            else:
                self.transpile_subroutine(header, ibody)

        # parameters inside function (if any)
        parameter_names: set[str] = set()
        for code, _comment in main_lines:
            s = code.strip()
            pd = parse_decl(s)
            if not pd:
                continue
            ftype, attrs, rest = pd
            attrs_l = attrs.lower()
            if "parameter" in attrs_l:
                parameter_names |= self.emit_parameters_from_decl(ftype, attrs_l, rest, arrays_1d)

        # allocate/init explicit-shape arrays and scalars
        self.emit_var_inits_from_sym(sym, arrays_1d, parameter_names, skip_names=set(args))
        # initialize derived-type/component bases that appear as name%field
        comp_bases: set[str] = set()
        for code, _comment in main_lines:
            for mbase in re.finditer(r"\b([a-z_]\w*)\s*%", code, re.I):
                comp_bases.add(mbase.group(1))
        known_names = {k.lower() for k in sym} | {a.lower() for a in args}
        for b in sorted(comp_bases):
            if b.lower() not in known_names and b not in parameter_names:
                self.emit(f"{b} = SimpleNamespace()")
        result_has_components = any(b.lower() == result_name.lower() for b in comp_bases)
        if (result_has_components or result_is_derived) and result_name.lower() not in {k.lower() for k in sym}:
            cls = result_type_name if result_type_name else "SimpleNamespace"
            self.emit(f"{result_name} = {cls}()")
        self._decl_types = {k.lower(): v["ftype"] for k, v in sym.items()}
        self._decl_array_types = {k.lower(): v["ftype"] for k, v in sym.items() if v.get("is_array")}
        self._decl_types = {k.lower(): v["ftype"] for k, v in sym.items()}
        self._decl_array_types = {k.lower(): v["ftype"] for k, v in sym.items() if v.get("is_array")}

        # exec pass
        prev_result_name = self._current_result_name
        self._current_result_name = result_name
        for idx, (code, comment) in enumerate(main_lines):
            s = code.strip()
            if idx < lead_idx and not s and comment.strip():
                continue
            self.emit_comment(comment)
            if not s:
                continue
            if self._parse_decl_line(s):
                continue
            self.handle_exec_line(s, arrays_1d)

        # basic default return (safe)
        self.emit(f"return {result_name}")
        self._current_result_name = prev_result_name
        self.indent = max(0, self.indent - 1)
        self.emit("")

    def transpile_program_body(self, body_lines: list[tuple[str, str]]) -> None:
        # gather decls and arrays
        self._emit_intrinsic_use_aliases(body_lines)
        sym: dict[str, dict] = {}
        arrays_1d: set[str] = set()

        for code, _comment in body_lines:
            s = code.strip()
            if not s:
                continue
            parsed = self._parse_decl_line(s)
            if not parsed:
                continue
            ftype, type_name, attrs_l, items = parsed
            if "parameter" in attrs_l:
                continue
            for name, shape, init in items:
                is_array = shape is not None
                is_alloc = "allocatable" in attrs_l
                sym[name] = {
                    "ftype": ftype,
                    "type_name": type_name,
                    "is_array": is_array,
                    "shape": shape,
                    "init": init,
                    "alloc": is_alloc,
                    "attrs_l": attrs_l,
                }
                if is_array:
                    arrays_1d.add(name)

        # parameters
        parameter_names: set[str] = set()
        for code, _comment in body_lines:
            s = code.strip()
            if not s:
                continue
            pd = parse_decl(s)
            if not pd:
                continue
            ftype, attrs, rest = pd
            attrs_l = attrs.lower()
            if "parameter" in attrs_l:
                parameter_names |= self.emit_parameters_from_decl(ftype, attrs_l, rest, arrays_1d)

        # declare/init variables (explicit-shape arrays + scalars)
        self.emit_var_inits_from_sym(sym, arrays_1d, parameter_names)
        self._decl_types = {k.lower(): v["ftype"] for k, v in sym.items()}
        self._decl_array_types = {k.lower(): v["ftype"] for k, v in sym.items() if v.get("is_array")}

        # exec statements
        for code, comment in body_lines:
            s = code.strip()
            self.emit_comment(comment)
            if not s:
                continue
            if parse_decl(s):
                continue
            self.handle_exec_line(s, arrays_1d)


    def transpile_module(self, header: str, body_lines: list[tuple[str, str]]) -> None:
        # Flatten a Fortran module into top-level Python definitions:
        # declarative-part parameters/variables/types become module globals,
        # and contained procedures are emitted as top-level defs.
        decl_lines = body_lines
        tail: list[tuple[str, str]] = []
        contains_idx = None
        for i, (code, _comment) in enumerate(body_lines):
            if code.strip().lower() == "contains":
                contains_idx = i
                break
        if contains_idx is not None:
            decl_lines = body_lines[:contains_idx]
            tail = body_lines[contains_idx + 1 :]

        # Emit derived types from the module declarative part.
        filtered_decl: list[tuple[str, str]] = []
        i = 0
        ndecl = len(decl_lines)
        while i < ndecl:
            line = decl_lines[i][0].strip()
            if re.match(r"^type\b", line, re.I) and not re.match(r"^type\s*\(", line, re.I):
                dtype_header = line
                dtype_body: list[tuple[str, str]] = []
                i += 1
                while i < ndecl and not re.match(r"\s*end\s+type\b", decl_lines[i][0], re.I):
                    dtype_body.append(decl_lines[i])
                    i += 1
                if i < ndecl:
                    i += 1
                self.transpile_derived_type(dtype_header, dtype_body)
                continue
            filtered_decl.append(decl_lines[i])
            i += 1
        decl_lines = filtered_decl

        self._emit_intrinsic_use_aliases(decl_lines)

        sym: dict[str, dict] = {}
        arrays_1d: set[str] = set()

        for code, _comment in decl_lines:
            s = code.strip()
            if not s:
                continue
            parsed = self._parse_decl_line(s)
            if not parsed:
                continue
            ftype, type_name, attrs_l, items = parsed
            for name, shape, init in items:
                is_array = shape is not None
                is_alloc = "allocatable" in attrs_l
                sym[name] = {
                    "ftype": ftype,
                    "type_name": type_name,
                    "is_array": is_array,
                    "shape": shape,
                    "init": init,
                    "alloc": is_alloc,
                    "attrs_l": attrs_l,
                }
                if is_array:
                    arrays_1d.add(name)

        parameter_names: set[str] = set()
        for code, _comment in decl_lines:
            s = code.strip()
            if not s:
                continue
            pd = parse_decl(s)
            if not pd:
                continue
            ftype, attrs, rest = pd
            attrs_l = attrs.lower()
            if "parameter" in attrs_l:
                parameter_names |= self.emit_parameters_from_decl(ftype, attrs_l, rest, arrays_1d)

        self.emit_var_inits_from_sym(sym, arrays_1d, parameter_names)
        if sym or parameter_names:
            self.emit("")

        if contains_idx is not None:
            i = 0
            n = len(tail)
            while i < n:
                line = tail[i][0].strip()
                if not line:
                    i += 1
                    continue
                if re.match(r"^(?!\s*end\s+function\b)\s*(?:(?:pure\s+)?\w+(?:\s*\([^)]*\))?\s+)*function\b", line, re.I):
                    func_header = line
                    fbody: list[tuple[str, str]] = []
                    i += 1
                    while i < n and not re.match(r"\s*end\s+function\b", tail[i][0], re.I):
                        fbody.append(tail[i])
                        i += 1
                    if i < n:
                        i += 1
                    self.transpile_function(func_header, fbody)
                    continue
                if re.match(r"^(?!\s*end\s+subroutine\b)\s*(?:pure\s+)?subroutine\b", line, re.I):
                    sub_header = line
                    sbody: list[tuple[str, str]] = []
                    i += 1
                    while i < n and not re.match(r"\s*end\s+subroutine\b", tail[i][0], re.I):
                        sbody.append(tail[i])
                        i += 1
                    if i < n:
                        i += 1
                    self.transpile_subroutine(sub_header, sbody)
                    continue
                i += 1

    def transpile_program(self, body_lines: list[tuple[str, str]]) -> None:
        # Handle internal procedures after "contains".
        main_lines = body_lines
        tail: list[tuple[str, str]] = []
        contains_idx = None
        for i, (code, _comment) in enumerate(body_lines):
            if code.strip().lower() == "contains":
                contains_idx = i
                break
        if contains_idx is not None:
            main_lines = body_lines[:contains_idx]
            tail = body_lines[contains_idx + 1 :]

        # Extract derived types declared in the program declarative part and
        # emit them as top-level dataclasses before internal procedures that may
        # reference those types in annotations.
        filtered_main: list[tuple[str, str]] = []
        i = 0
        nmain = len(main_lines)
        while i < nmain:
            line = main_lines[i][0].strip()
            if re.match(r"^type\b", line, re.I) and not re.match(r"^type\s*\(", line, re.I):
                header = line
                tbody: list[tuple[str, str]] = []
                i += 1
                while i < nmain and not re.match(r"\s*end\s+type\b", main_lines[i][0], re.I):
                    tbody.append(main_lines[i])
                    i += 1
                if i < nmain:
                    i += 1
                self.transpile_derived_type(header, tbody)
                continue
            filtered_main.append(main_lines[i])
            i += 1
        main_lines = filtered_main

        if contains_idx is not None:
            i = 0
            n = len(tail)
            while i < n:
                line = tail[i][0].strip()
                if not line:
                    i += 1
                    continue
                if re.match(r"^(?!\s*end\s+function\b)\s*(?:(?:pure\s+)?\w+(?:\s*\([^)]*\))?\s+)*function\b", line, re.I):
                    header = line
                    fbody: list[tuple[str, str]] = []
                    i += 1
                    while i < n and not re.match(r"\s*end\s+function\b", tail[i][0], re.I):
                        fbody.append(tail[i])
                        i += 1
                    if i < n:
                        i += 1
                    self.transpile_function(header, fbody)
                    continue
                if re.match(r"^(?!\s*end\s+subroutine\b)\s*(?:pure\s+)?subroutine\b", line, re.I):
                    header = line
                    sbody: list[tuple[str, str]] = []
                    i += 1
                    while i < n and not re.match(r"\s*end\s+subroutine\b", tail[i][0], re.I):
                        sbody.append(tail[i])
                        i += 1
                    if i < n:
                        i += 1
                    self.transpile_subroutine(header, sbody)
                    continue
                i += 1

        self.emit("def main() -> None:")
        self.indent += 1
        code0 = self._code_emit_count
        self.transpile_program_body(main_lines)
        if self._code_emit_count == code0:
            self.emit("pass")
        self.indent = max(0, self.indent - 1)
        self.emit("")

    def transpile_subroutine(self, header: str, body_lines: list[tuple[str, str]]) -> None:
        hdr = header.strip()
        m = re.match(r"(?:pure\s+)?subroutine\s+(\w+)\s*\(\s*([^\)]*)\s*\)", hdr, re.I)
        if not m:
            return
        sname = m.group(1)
        args = [a.strip() for a in m.group(2).split(",") if a.strip()]

        main_lines, internals = self._split_internal_subprograms(body_lines)
        self._validate_unit_symbols("subroutine", sname, args, main_lines)

        sym: dict[str, dict] = {}
        arrays_1d: set[str] = set()
        arg_hints: dict[str, str] = {}

        self._emit_intrinsic_use_aliases(main_lines)

        for code, _comment in main_lines:
            s = code.strip()
            pd = parse_decl(s)
            if pd:
                ftype, attrs, rest = pd
                attrs_l = attrs.lower()
                items = parse_decl_items(rest, parse_decl_attr_dimension(attrs))
                type_name = None
            else:
                td = re.match(r"^type\s*\(\s*([a-z_]\w*)\s*\)\s*(.*?)::\s*(.*)$", s, re.I)
                if not td:
                    continue
                ftype = "type"
                attrs_l = td.group(2).strip().lower()
                items = parse_decl_items(td.group(3).strip(), parse_decl_attr_dimension(td.group(2).strip()))
                type_name = td.group(1)
            for name, shape, init in items:
                is_array = shape is not None
                is_alloc = "allocatable" in attrs_l
                sym[name] = {
                    "ftype": ftype,
                    "type_name": type_name,
                    "is_array": is_array,
                    "shape": shape,
                    "init": init,
                    "alloc": is_alloc,
                    "attrs_l": attrs_l,
                }
                if is_array:
                    arrays_1d.add(name)
                if name in args:
                    if is_array:
                        arg_hints[name] = self._type_hint(ftype, type_name, is_array=True)
                    else:
                        arg_hints[name] = self._type_hint(ftype, type_name, is_array=False)

        args_annot = []
        for a in args:
            hint = arg_hints.get(a, "float")
            default_none = False
            info = sym.get(a)
            if info and "optional" in info.get("attrs_l", ""):
                default_none = True
            if default_none:
                args_annot.append(f"{a}: {hint} = None")
            else:
                args_annot.append(f"{a}: {hint}")
        out_formals: list[str] = []
        for a in args:
            info = sym.get(a)
            if not info:
                continue
            attrs_l = info.get("attrs_l", "")
            if "intent(out" in attrs_l or "intent(inout" in attrs_l:
                out_formals.append(a)
        self._subr_sigs[sname.lower()] = {"args": list(args), "out": list(out_formals)}

        self.emit(f"def {sname}({', '.join(args_annot)}):")
        self.indent += 1

        for kind, header, ibody in internals:
            if kind == "function":
                self.transpile_function(header, ibody)
            else:
                self.transpile_subroutine(header, ibody)

        parameter_names: set[str] = set()
        for code, _comment in main_lines:
            s = code.strip()
            pd = parse_decl(s)
            if not pd:
                continue
            ftype, attrs, rest = pd
            attrs_l = attrs.lower()
            if "parameter" in attrs_l:
                parameter_names |= self.emit_parameters_from_decl(ftype, attrs_l, rest, arrays_1d)

        self.emit_var_inits_from_sym(sym, arrays_1d, parameter_names, skip_names=set(args))
        self._decl_types = {k.lower(): v["ftype"] for k, v in sym.items()}
        self._decl_array_types = {k.lower(): v["ftype"] for k, v in sym.items() if v.get("is_array")}

        for code, comment in main_lines:
            s = code.strip()
            self.emit_comment(comment)
            if not s:
                continue
            if parse_decl(s):
                continue
            self.handle_exec_line(s, arrays_1d)

        if out_formals:
            if len(out_formals) == 1:
                self.emit(f"return {out_formals[0]}")
            else:
                self.emit("return " + ", ".join(out_formals))

        self.indent = max(0, self.indent - 1)
        self.emit("")

    def transpile(self, src: str) -> str:
        raw = [split_fortran_comment(l) for l in src.splitlines()]
        raw = collapse_fortran_continuations(raw)
        self.seen_parameter = any(re.search(r"\bparameter\b", code, re.I) for code, _c in raw)
        self._seen_scipy_special = any(
            re.search(r"\b(?:gamma|log_gamma|erf|erfc|bessel_j0|bessel_j1|bessel_jn|bessel_yn)\s*\(", code, re.I)
            for code, _c in raw
        )

        self.out = []
        self.indent = 0
        self._code_emit_count = 0
        self._block_code_start = []

        self.emit("import numpy as np")
        self.emit("import numpy.typing as npt")
        self.emit("from dataclasses import dataclass, field")
        self.emit("from types import SimpleNamespace")
        self.emit("from fortran_py_runtime import *")
        if self._seen_scipy_special:
            self.emit("import scipy.special as sps")
        if self.seen_parameter:
            self.emit("from typing import Final")
        self.emit("")
        self.emit("def _xf2p_flatten(x):")
        self.indent += 1
        self.emit('"""Flatten nested list/tuple values emitted by transpiled implied-DOs."""')
        self.emit("if isinstance(x, (list, tuple)):")
        self.indent += 1
        self.emit("for item in x:")
        self.indent += 1
        self.emit("yield from _xf2p_flatten(item)")
        self.indent -= 1
        self.indent -= 1
        self.emit("else:")
        self.indent += 1
        self.emit("yield x")
        self.indent -= 1
        self.indent -= 1
        self.emit("")
        self.emit("def _xf2p_implied_do(func, lo, hi, step=1):")
        self.indent += 1
        self.emit('"""Expand a Fortran I/O implied-DO into a flat Python list."""')
        self.emit("ilo = int(lo)")
        self.emit("ihi = int(hi)")
        self.emit("istep = int(step)")
        self.emit("stop = ihi + (1 if istep > 0 else -1)")
        self.emit("out = []")
        self.emit("for _xf2p_i in range(ilo, stop, istep):")
        self.indent += 1
        self.emit("out.extend(_xf2p_flatten(func(_xf2p_i)))")
        self.indent -= 1
        self.emit("return out")
        self.indent -= 1
        self.emit("")
        self.emit("def _xf2p_component_array(obj, path):")
        self.indent += 1
        self.emit('"""Project a component path over an array of derived-type values."""')
        self.emit("attrs = path.split('.')")
        self.emit("out = []")
        self.emit("for _xf2p_item in obj:")
        self.indent += 1
        self.emit("val = _xf2p_item")
        self.emit("for _xf2p_attr in attrs:")
        self.indent += 1
        self.emit("val = getattr(val, _xf2p_attr)")
        self.indent -= 1
        self.emit("out.append(val)")
        self.indent -= 1
        self.emit("return np.asarray(out)")
        self.indent -= 1
        self.emit("")

        i = 0
        n = len(raw)

        in_module = False
        found_program = False
        loose_main: list[tuple[str, str]] = []

        while i < n:
            line = raw[i][0].strip()
            if not line:
                self.emit_comment(raw[i][1])
                i += 1
                continue

            if re.match(r"module\b", line, re.I) and not re.match(r"module\s+procedure\b", line, re.I):
                in_module = True
                header = line
                mbody: list[tuple[str, str]] = []
                i += 1
                while i < n and not re.match(r"\s*end\s+module\b", raw[i][0], re.I):
                    mbody.append(raw[i])
                    i += 1
                if i < n:
                    i += 1
                self.transpile_module(header, mbody)
                in_module = False
                continue

            if re.match(r"^type\b", line, re.I) and not re.match(r"^type\s*\(", line, re.I):
                header = line
                tbody: list[tuple[str, str]] = []
                i += 1
                while i < n and not re.match(r"\s*end\s+type\b", raw[i][0], re.I):
                    tbody.append(raw[i])
                    i += 1
                if i < n:
                    i += 1
                self.transpile_derived_type(header, tbody)
                continue

            if re.match(r"^(?!\s*end\s+function\b)\s*(?:(?:pure\s+)?\w+(?:\s*\([^)]*\))?\s+)*function\b", line, re.I):
                header = line
                body: list[tuple[str, str]] = []
                i += 1
                while i < n and not re.match(r"\s*end\s+function\b", raw[i][0], re.I):
                    body.append(raw[i])
                    i += 1
                if i < n:
                    i += 1
                self.transpile_function(header, body)
                continue

            if re.match(r"^(?!\s*end\s+subroutine\b)\s*(?:pure\s+)?subroutine\b", line, re.I):
                header = line
                body: list[tuple[str, str]] = []
                i += 1
                while i < n and not re.match(r"\s*end\s+subroutine\b", raw[i][0], re.I):
                    body.append(raw[i])
                    i += 1
                if i < n:
                    i += 1
                self.transpile_subroutine(header, body)
                continue

            if re.match(r"program\b", line, re.I):
                found_program = True
                body: list[tuple[str, str]] = []
                i += 1
                while i < n and not re.match(r"\s*end\s+program\b", raw[i][0], re.I):
                    body.append(raw[i])
                    i += 1
                if i < n:
                    i += 1
                self.transpile_program(body)
                continue

            if not in_module:
                loose_main.append(raw[i])

            i += 1

        # unnamed main program (no "program" statement)
        if not found_program:
            if any(code.strip() or comment.strip() for code, comment in loose_main):
                self.transpile_program(loose_main)

        lines = "\n".join(self.out).rstrip().splitlines()
        lines = self._drop_unused_runtime(lines)
        out_lines: list[str] = []
        i = 0
        while i < len(lines):
            if lines[i].strip() == 'if __name__ == "__main__":':
                if i + 1 < len(lines) and lines[i + 1].strip() == "main()":
                    i += 2
                    continue
            out_lines.append(lines[i])
            i += 1
        has_main = any(ln.startswith("def main(") for ln in out_lines)
        if has_main:
            if out_lines and out_lines[-1].strip() != "":
                out_lines.append("")
            out_lines.append('if __name__ == "__main__":')
            out_lines.append("    main()")
        return "\n".join(out_lines).rstrip() + "\n"


def main() -> int:
    ap = argparse.ArgumentParser(description="Partial Fortran-to-Python transpiler")
    ap.add_argument("input_f90", nargs="+", help="input .f90 source file(s)")
    ap.add_argument("--mode-program", action="store_true", help="treat all input files as one program (default)")
    ap.add_argument("--mode-each", action="store_true", help="transpile each input file independently")
    ap.add_argument("--out", help="output .py path (single target; valid for program mode or one-file each mode)")
    ap.add_argument("--out-dir", help="output directory for --mode-each (default: source file directories)")
    ap.add_argument("--run", action="store_true", help="run translated Python script after writing it")
    ap.add_argument("--compile", action="store_true", help="compile original Fortran source")
    ap.add_argument("--run-both", action="store_true", help="run original Fortran and translated Python (no timing)")
    ap.add_argument("--run-diff", action="store_true", help="run Fortran and Python and compare outputs")
    ap.add_argument("--tee", action="store_true", help="print transformed output (run output, or transpiled source when not running)")
    ap.add_argument("--tee-both", action="store_true", help="print both original and transformed outputs (run output, or source when not running)")
    ap.add_argument("--time", action="store_true", help="time transpile/compile/run stages (implies --run)")
    ap.add_argument("--time-both", action="store_true", help="time both original Fortran and translated Python (implies --run-both)")
    ap.add_argument(
        "--compiler",
        default="gfortran -O3 -march=native",
        help='compiler command, e.g. "gfortran -O2 -Wall"',
    )
    args = ap.parse_args()

    if args.time_both:
        args.run_diff = True
    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
        args.compile = True
    if args.time_both:
        args.time = True
    if args.time:
        args.run = True
    if args.tee_both:
        args.tee = True

    show_fortran_output = bool(args.run_both or args.tee_both)
    show_python_output = bool(args.run_both or args.tee or args.tee_both)

    if args.mode_program and args.mode_each:
        print("Transpile: FAIL (choose at most one of --mode-program and --mode-each)")
        return 1
    mode_each = bool(args.mode_each)

    in_paths = [Path(p) for p in args.input_f90]
    for p in in_paths:
        if not p.exists():
            print(f"Missing file: {p}")
            return 1
    if mode_each and args.out and len(in_paths) > 1:
        print("Transpile: FAIL (--out with --mode-each is allowed only for one input file)")
        return 1
    if (not mode_each) and len(in_paths) > 1 and args.out:
        print("Transpile: FAIL (--out is allowed only for a single input in --mode-program)")
        return 1

    def _norm(s: str):
        lines = s.replace("\r\n", "\n").replace("\r", "\n").split("\n")
        lines = [" ".join(ln.split()) for ln in lines]
        while lines and lines[-1] == "":
            lines.pop()
        return lines

    def _ensure_runtime_file(dst_dir: Path) -> None:
        rt = Path(__file__).with_name("fortran_py_runtime.py")
        if rt.exists():
            dst = dst_dir / "fortran_py_runtime.py"
            if not dst.exists():
                dst.write_text(rt.read_text(encoding="utf-8"), encoding="utf-8")

    def process_one(src_paths: list[Path], out_path: Path) -> int:
        timings = {}
        ft_run = None
        ft_exe = out_path.with_suffix(".orig.exe")
        compiler_parts = shlex.split(args.compiler)

        if args.compile or args.run_both:
            if args.time:
                if len(compiler_parts) > 1:
                    print("Compile options:", " ".join(compiler_parts[1:]))
                else:
                    print("Compile options: <none>")
            build_cmd = compiler_parts + [str(p) for p in src_paths] + ["-o", str(ft_exe)]
            print("Build (original-fortran):", " ".join(build_cmd))
            t0_build = time.perf_counter()
            cp = subprocess.run(build_cmd, text=True, capture_output=True)
            timings["compile"] = time.perf_counter() - t0_build
            if cp.returncode != 0:
                print(f"Build (original-fortran): FAIL (exit {cp.returncode})")
                if cp.stdout.strip():
                    print(cp.stdout.rstrip())
                if cp.stderr.strip():
                    print(cp.stderr.rstrip())
                return cp.returncode
            print("Build (original-fortran): PASS")

        if args.run_both:
            t0_ft = time.perf_counter()
            ft_run = subprocess.run([str(ft_exe)], text=True, capture_output=True)
            timings["fortran_run"] = time.perf_counter() - t0_ft
            if ft_run.returncode != 0:
                print(f"Run (original-fortran): FAIL (exit {ft_run.returncode})")
                if ft_run.stdout.strip():
                    print(ft_run.stdout.rstrip())
                if ft_run.stderr.strip():
                    print(ft_run.stderr.rstrip())
                return ft_run.returncode
            print("Run (original-fortran): PASS")
            if show_fortran_output and ft_run.stdout.strip():
                if args.run_both and not args.tee_both:
                    print("--- output (original-fortran) ---")
                print(ft_run.stdout.rstrip())
            if show_fortran_output and ft_run.stderr.strip():
                if args.run_both and not args.tee_both and not ft_run.stdout.strip():
                    print("--- output (original-fortran) ---")
                print(ft_run.stderr.rstrip())

        t0_transpile = time.perf_counter()
        src = _preprocess_fortran_source("\n\n".join(p.read_text(encoding="utf-8") for p in src_paths))
        t = basic_f2p()
        try:
            py = t.transpile(src)
        except ValueError as e:
            print(f"Transpile: FAIL ({e})")
            return 1
        stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        src_tag = ", ".join(p.name for p in src_paths)
        py = f"# transpiled by xf2p.py from {src_tag} on {stamp}\n" + py
        out_path.write_text(py, encoding="utf-8")
        _ensure_runtime_file(out_path.parent)
        if args.tee_both:
            try:
                src_text = src_paths[0].read_text(encoding="utf-8") if len(src_paths) == 1 else "\n\n".join(
                    p.read_text(encoding="utf-8") for p in src_paths
                )
                src_tag = src_paths[0].name if len(src_paths) == 1 else ", ".join(p.name for p in src_paths)
                print(f"--- original: {src_tag} ---")
                print(src_text.rstrip())
            except OSError:
                pass
        if args.tee:
            try:
                out_text = out_path.read_text(encoding="utf-8")
                print(f"--- transpiled: {out_path} ---")
                print(out_text.rstrip())
            except OSError:
                pass
        timings["transpile"] = time.perf_counter() - t0_transpile

        if args.run:
            cmd = [sys.executable, str(out_path)]
            t0_py = time.perf_counter()
            rp = subprocess.run(cmd, text=True, capture_output=True)
            timings["python_run"] = time.perf_counter() - t0_py
            if rp.returncode != 0:
                if "SyntaxError" in (rp.stderr or ""):
                    print(f"Run: FAIL (translated Python is invalid: syntax error in {out_path})")
                else:
                    print(f"Run: FAIL (exit {rp.returncode})")
                if rp.stdout.strip():
                    print(rp.stdout.rstrip())
                if rp.stderr.strip():
                    print(rp.stderr.rstrip())
                return rp.returncode
            if args.run_both:
                print("Run (translated-python): PASS")
            if show_python_output and rp.stdout.strip():
                if args.run_both and not args.tee_both:
                    print("--- output (translated-python) ---")
                print(rp.stdout.rstrip())
            if show_python_output and rp.stderr.strip():
                if args.run_both and not args.tee_both and not rp.stdout.strip():
                    print("--- output (translated-python) ---")
                print(rp.stderr.rstrip())

            if args.run_diff and ft_run is not None:
                ft_lines = _norm((ft_run.stdout or "") + (("\n" + ft_run.stderr) if ft_run.stderr else ""))
                py_lines = _norm((rp.stdout or "") + (("\n" + rp.stderr) if rp.stderr else ""))
                if ft_lines == py_lines:
                    print("Run diff: MATCH")
                else:
                    print("Run diff: DIFF")
                    first = None
                    nmin = min(len(ft_lines), len(py_lines))
                    for i in range(nmin):
                        if ft_lines[i] != py_lines[i]:
                            first = i
                            break
                    if first is None:
                        first = nmin
                    print(f"  first mismatch line: {first + 1}")
                    if first < len(ft_lines):
                        print(f"  fortran: {ft_lines[first]}")
                    else:
                        print("  fortran: <no line>")
                    if first < len(py_lines):
                        print(f"  python : {py_lines[first]}")
                    else:
                        print("  python : <no line>")
                    for dl in difflib.unified_diff(ft_lines, py_lines, fromfile="fortran", tofile="python", n=1):
                        print(dl)
                        if dl.startswith("@@"):
                            break

        if args.time:
            fortran_total = timings.get("compile", 0.0) + timings.get("fortran_run", 0.0)
            print("")
            print("Timing summary (seconds):")
            base = timings.get("python_run", 0.0)

            def _ratio(v):
                if base > 0.0:
                    return f"{(v / base):.6f}"
                return "n/a"

            rows = []
            rows.append(("transpile", timings.get("transpile", 0.0)))
            if "python_run" in timings:
                rows.append(("python run", timings["python_run"]))
            if "compile" in timings:
                rows.append(("compile", timings["compile"]))
            if "fortran_run" in timings:
                rows.append(("fortran run", timings["fortran_run"]))
            if "compile" in timings or "fortran_run" in timings:
                rows.append(("fortran total", fortran_total))

            print("  stage            seconds    ratio(vs python run)")
            for name, val in rows:
                print(f"  {name:<14} {val:>8.6f}    {_ratio(val)}")
        return 0

    if not mode_each:
        if len(in_paths) == 1:
            out_path = Path(args.out) if args.out else in_paths[-1].with_name(f"{in_paths[-1].stem}_f.py")
            return process_one(in_paths, out_path)

        # Multi-file program mode: emit one Python module per Fortran input file.
        # This preserves separate namespaces and wires USE-based imports.
        timings: dict[str, float] = {}
        ft_run = None
        compiler_parts = shlex.split(args.compiler)
        if args.out_dir:
            ft_exe = Path(args.out_dir) / f"{in_paths[-1].stem}.orig.exe"
        else:
            ft_exe = in_paths[-1].with_suffix(".orig.exe")
        ft_exe.parent.mkdir(parents=True, exist_ok=True)

        if args.compile or args.run_both:
            if args.time:
                if len(compiler_parts) > 1:
                    print("Compile options:", " ".join(compiler_parts[1:]))
                else:
                    print("Compile options: <none>")
            build_cmd = compiler_parts + [str(p) for p in in_paths] + ["-o", str(ft_exe)]
            print("Build (original-fortran):", " ".join(build_cmd))
            t0_build = time.perf_counter()
            cp = subprocess.run(build_cmd, text=True, capture_output=True)
            timings["compile"] = time.perf_counter() - t0_build
            if cp.returncode != 0:
                print(f"Build (original-fortran): FAIL (exit {cp.returncode})")
                if cp.stdout.strip():
                    print(cp.stdout.rstrip())
                if cp.stderr.strip():
                    print(cp.stderr.rstrip())
                return cp.returncode
            print("Build (original-fortran): PASS")

        if args.run_both:
            t0_ft = time.perf_counter()
            ft_run = subprocess.run([str(ft_exe)], text=True, capture_output=True)
            timings["fortran_run"] = time.perf_counter() - t0_ft
            if ft_run.returncode != 0:
                print(f"Run (original-fortran): FAIL (exit {ft_run.returncode})")
                if ft_run.stdout.strip():
                    print(ft_run.stdout.rstrip())
                if ft_run.stderr.strip():
                    print(ft_run.stderr.rstrip())
                return ft_run.returncode
            print("Run (original-fortran): PASS")
            if show_fortran_output and ft_run.stdout.strip():
                if args.run_both and not args.tee_both:
                    print("--- output (original-fortran) ---")
                print(ft_run.stdout.rstrip())
            if show_fortran_output and ft_run.stderr.strip():
                if args.run_both and not args.tee_both and not ft_run.stdout.strip():
                    print("--- output (original-fortran) ---")
                print(ft_run.stderr.rstrip())

        t0_transpile = time.perf_counter()
        file_src: dict[Path, str] = {p: _preprocess_fortran_source(p.read_text(encoding="utf-8")) for p in in_paths}
        file_mods: dict[Path, list[str]] = {}
        file_defs: dict[Path, list[str]] = {}
        file_uses: dict[Path, list[UseSpec]] = {}
        for p in in_paths:
            mods, defs, uses = _parse_file_interface(file_src[p])
            file_mods[p] = mods
            file_defs[p] = defs
            file_uses[p] = uses

        module_provider: dict[str, Path] = {}
        for p in in_paths:
            for m in file_mods[p]:
                module_provider[m.lower()] = p

        generated: dict[Path, Path] = {}
        rc = 0
        for p in in_paths:
            out_path = (Path(args.out_dir) / f"{p.stem}_f.py") if args.out_dir else p.with_name(f"{p.stem}_f.py")
            out_path.parent.mkdir(parents=True, exist_ok=True)
            print(f"[mode-program] {p} -> {out_path}")
            src = file_src[p]
            t = basic_f2p()
            try:
                py = t.transpile(src)
            except ValueError as e:
                print(f"Transpile: FAIL ({e})")
                return 1
            stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            py = f"# transpiled by xf2p.py from {p.name} on {stamp}\n" + py
            out_path.write_text(py, encoding="utf-8")
            _ensure_runtime_file(out_path.parent)
            if not args.run:
                if args.tee_both:
                    print(f"--- original: {p} ---")
                    print(src.rstrip())
                if args.tee:
                    print(f"--- transpiled: {out_path} ---")
                    print(py.rstrip())
            generated[p] = out_path

        # Add inter-file imports according to USE dependencies.
        for p in in_paths:
            out_path = generated[p]
            local_defs = {x.lower() for x in file_defs[p]}
            imports: list[str] = []
            for us in file_uses[p]:
                if us.intrinsic:
                    continue
                provider = module_provider.get(us.module.lower())
                if provider is None or provider == p:
                    continue
                mod_py = generated[provider].stem
                names: list[str]
                if us.only_items is not None:
                    names = [
                        nm
                        for nm in us.only_items
                        if nm.lower() not in local_defs and nm.lower() not in _LOCAL_RUNTIME_HELPERS
                    ]
                else:
                    # import all known symbols from provider, excluding local collisions
                    names = [
                        nm
                        for nm in file_defs[provider]
                        if nm.lower() not in local_defs and nm.lower() not in _LOCAL_RUNTIME_HELPERS
                    ]
                names = unique_preserve(names)
                if not names:
                    continue
                imports.append(f"from {mod_py} import {', '.join(names)}")
            if imports:
                py = out_path.read_text(encoding="utf-8")
                py = _insert_imports(py, unique_preserve(imports))
                out_path.write_text(py, encoding="utf-8")
        timings["transpile"] = time.perf_counter() - t0_transpile

        if args.run:
            main_out = generated[in_paths[-1]]
            cmd = [sys.executable, str(main_out)]
            t0_py = time.perf_counter()
            rp = subprocess.run(cmd, text=True, capture_output=True)
            timings["python_run"] = time.perf_counter() - t0_py
            if rp.returncode != 0:
                if "SyntaxError" in (rp.stderr or ""):
                    print(f"Run: FAIL (translated Python is invalid: syntax error in {main_out})")
                else:
                    print(f"Run: FAIL (exit {rp.returncode})")
                if rp.stdout.strip():
                    print(rp.stdout.rstrip())
                if rp.stderr.strip():
                    print(rp.stderr.rstrip())
                return rp.returncode
            if args.run_both:
                print("Run (translated-python): PASS")
            else:
                print("Run: PASS")
            if show_python_output and rp.stdout.strip():
                if args.run_both and not args.tee_both:
                    print("--- output (translated-python) ---")
                print(rp.stdout.rstrip())
            if show_python_output and rp.stderr.strip():
                if args.run_both and not args.tee_both and not rp.stdout.strip():
                    print("--- output (translated-python) ---")
                print(rp.stderr.rstrip())

            if args.run_diff and ft_run is not None:
                ft_lines = _norm((ft_run.stdout or "") + (("\n" + ft_run.stderr) if ft_run.stderr else ""))
                py_lines = _norm((rp.stdout or "") + (("\n" + rp.stderr) if rp.stderr else ""))
                if ft_lines == py_lines:
                    print("Run diff: MATCH")
                else:
                    print("Run diff: DIFF")
                    first = None
                    nmin = min(len(ft_lines), len(py_lines))
                    for i in range(nmin):
                        if ft_lines[i] != py_lines[i]:
                            first = i
                            break
                    if first is None:
                        first = nmin
                    print(f"  first mismatch line: {first + 1}")
                    if first < len(ft_lines):
                        print(f"  fortran: {ft_lines[first]}")
                    else:
                        print("  fortran: <no line>")
                    if first < len(py_lines):
                        print(f"  python : {py_lines[first]}")
                    else:
                        print("  python : <no line>")
                    for dl in difflib.unified_diff(ft_lines, py_lines, fromfile="fortran", tofile="python", n=1):
                        print(dl)
                        if dl.startswith("@@"):
                            break

        if args.time:
            fortran_total = timings.get("compile", 0.0) + timings.get("fortran_run", 0.0)
            print("")
            print("Timing summary (seconds):")
            base = timings.get("python_run", 0.0)

            def _ratio(v):
                if base > 0.0:
                    return f"{(v / base):.6f}"
                return "n/a"

            rows = []
            rows.append(("transpile", timings.get("transpile", 0.0)))
            if "python_run" in timings:
                rows.append(("python run", timings["python_run"]))
            if "compile" in timings:
                rows.append(("compile", timings["compile"]))
            if "fortran_run" in timings:
                rows.append(("fortran run", timings["fortran_run"]))
            if "compile" in timings or "fortran_run" in timings:
                rows.append(("fortran total", fortran_total))

            print("  stage            seconds    ratio(vs python run)")
            for name, val in rows:
                print(f"  {name:<14} {val:>8.6f}    {_ratio(val)}")
        return rc

    rc = 0
    for p in in_paths:
        print(f"[mode-each] {p}")
        if args.out:
            out_path = Path(args.out)
        elif args.out_dir:
            out_path = Path(args.out_dir) / f"{p.stem}_f.py"
        else:
            out_path = p.with_name(f"{p.stem}_f.py")
        out_path.parent.mkdir(parents=True, exist_ok=True)
        rc = process_one([p], out_path)
        if rc != 0:
            break
    return rc


if __name__ == "__main__":
    raise SystemExit(main())
