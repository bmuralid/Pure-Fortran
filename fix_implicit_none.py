#!/usr/bin/env python3
"""
fix_implicit_none.py
--------------------
Reads gfortran/ifort compiler errors of the form:
    filename.f90:LINE:COL: Error: Symbol 'X' at (1) has no IMPLICIT type
and auto-inserts variable declarations into the corresponding subroutines/functions.

Also handles missing function interfaces:
    filename.f90:LINE:COL: Error: Function 'X' at (1) has no IMPLICIT type
    filename.f90:LINE:COL: Error: Return type mismatch of function 'X' at (1) (UNKNOWN/INTEGER(4))
For these, an INTERFACE block is inserted after the variable declarations, with return type
inferred from the "Return type mismatch" message or Fortran implicit typing rules (I-N → INTEGER).
Argument types in the interface are inferred from the dummy argument names at the call site.

Type inference uses Fortran's classic implicit typing rules (I-N → INTEGER, else REAL),
but you can override with --default-type or --type-map.

Usage:
    # 1. Capture compiler errors to a file first:
    gfortran -c myfile.f90 2> errors.txt

    # 2. Run this script:
    python fix_implicit_none.py myfile.f90 errors.txt

    # 3. Dry-run (no file changes, just show what would be inserted):
    python fix_implicit_none.py myfile.f90 errors.txt --dry-run

    # 4. Override default real type:
    python fix_implicit_none.py myfile.f90 errors.txt --real-type "real(8)"

    # 5. Provide explicit type overrides for specific variables:
    python fix_implicit_none.py myfile.f90 errors.txt --type-map "n:integer,x:real(8),flag:logical"
"""

import re
import sys
import argparse
import shutil
from pathlib import Path
from collections import defaultdict


# ---------------------------------------------------------------------------
# Fortran implicit typing: I, J, K, L, M, N → INTEGER; everything else REAL
# ---------------------------------------------------------------------------
INTEGER_LETTERS = set("ijklmn")

def implicit_type(varname: str, real_type: str = "real", int_type: str = "integer") -> str:
    first = varname[0].lower()
    return int_type if first in INTEGER_LETTERS else real_type


# ---------------------------------------------------------------------------
# Parse compiler error output
# ---------------------------------------------------------------------------
# Handles two gfortran formats:
#
# Single-line (older gfortran / ifort):
#   foo.f90:42:10: Error: Symbol 'x' at (1) has no IMPLICIT type
#
# Multi-line (modern gfortran):
#   foo.f90:126:34:
#     126 |     read(...) (dum,k=1,3),sd
#         |                  1
#   Error: Symbol 'dum' at (1) has no IMPLICIT type

# Header line: path:line:col:  (with nothing after the last colon on that line)
HEADER_RE = re.compile(r"^(?P<file>.+?):(?P<line>\d+):\d+:\s*$")
# Inline single-line format
INLINE_RE = re.compile(
    r"^(?P<file>.+?):(?P<line>\d+):\d+:.*?Symbol\s+\W?(?P<var>\w+)\W?\s+at\s+\([^)]+\)\s+has\s+no\s+IMPLICIT\s+type",
    re.IGNORECASE,
)
# The actual error message line (may appear 1-3 lines after the header)
MSG_RE = re.compile(
    r"Error:\s+Symbol\s+\W?(?P<var>\w+)\W?\s+at\s+\([^)]+\)\s+has\s+no\s+IMPLICIT\s+type",
    re.IGNORECASE,
)
# Function has no IMPLICIT type  (different keyword: "Function" not "Symbol")
FUNC_NO_IMPLICIT_RE = re.compile(
    r"Error:\s+Function\s+\W?(?P<func>\w+)\W?\s+at\s+\([^)]+\)\s+has\s+no\s+IMPLICIT\s+type",
    re.IGNORECASE,
)
# Return type mismatch — gives us the actual return type gfortran expected
# e.g. "Error: Return type mismatch of function 'lv_g' at (1) (UNKNOWN/INTEGER(4))"
# Note: gtype can contain parentheses itself (e.g. INTEGER(4)), so we match
# an optional (N) kind parameter explicitly.
RETURN_TYPE_MISMATCH_RE = re.compile(
    r"Error:\s+Return\s+type\s+mismatch\s+of\s+function\s+\W?(?P<func>\w+)\W?\s+at\s+\([^)]+\)\s+"
    r"\(UNKNOWN/(?P<gtype>[A-Za-z][A-Za-z0-9 ]*(?:\(\d+\))?)\)",
    re.IGNORECASE,
)

def parse_errors(error_text: str) -> dict[str, dict[int, set[str]]]:
    """
    Returns { filename: { line_number: {var, ...} } }
    Only matches "Symbol '...' has no IMPLICIT type" (variables, not functions).
    """
    result: dict[str, dict[int, set[str]]] = defaultdict(lambda: defaultdict(set))
    lines = error_text.splitlines()
    i = 0
    while i < len(lines):
        line = lines[i]

        # Try single-line format first
        m = INLINE_RE.match(line.strip())
        if m:
            result[m.group("file")][int(m.group("line"))].add(m.group("var").lower())
            i += 1
            continue

        # Try multi-line format: header line followed within 4 lines by Error: message
        m = HEADER_RE.match(line)
        if m:
            fname, lineno = m.group("file"), int(m.group("line"))
            # Scan the next up to 4 lines for the Error: message
            for j in range(i + 1, min(i + 5, len(lines))):
                mm = MSG_RE.search(lines[j])
                if mm:
                    result[fname][lineno].add(mm.group("var").lower())
                    i = j + 1
                    break
            else:
                i += 1
            continue

        i += 1
    return result


def gfortran_type_to_fortran(gtype: str) -> str:
    """Map gfortran internal type strings (e.g. 'INTEGER(4)') to Fortran type declarations."""
    g = gtype.strip().upper()
    mapping = {
        "INTEGER":          "integer",
        "INTEGER(1)":       "integer(1)",
        "INTEGER(2)":       "integer(2)",
        "INTEGER(4)":       "integer",
        "INTEGER(8)":       "integer(8)",
        "REAL":             "real",
        "REAL(4)":          "real",
        "REAL(8)":          "real(8)",
        "DOUBLE PRECISION": "double precision",
        "COMPLEX":          "complex",
        "COMPLEX(4)":       "complex",
        "COMPLEX(8)":       "complex(8)",
        "LOGICAL":          "logical",
        "LOGICAL(4)":       "logical",
        "CHARACTER":        "character(len=*)",
    }
    return mapping.get(g, gtype.lower())


def parse_function_errors(error_text: str) -> tuple[dict, dict]:
    """
    Parse function-related compiler errors.

    Returns:
        func_lines  – { filename: { line_number: set(func_name_lower) } }
                      sourced from "Function '...' has no IMPLICIT type" messages
        func_types  – { func_name_lower: fortran_type_str }
                      sourced from "Return type mismatch … (UNKNOWN/TYPE)" messages
    """
    func_lines: dict = defaultdict(lambda: defaultdict(set))
    func_types: dict = {}

    lines = error_text.splitlines()
    i = 0
    while i < len(lines):
        line = lines[i]

        m_hdr = HEADER_RE.match(line)
        if m_hdr:
            fname  = m_hdr.group("file")
            lineno = int(m_hdr.group("line"))
            # Scan the next few lines for a function error message
            for j in range(i + 1, min(i + 6, len(lines))):
                mm = RETURN_TYPE_MISMATCH_RE.search(lines[j])
                if mm:
                    func_name = mm.group("func").lower()
                    func_types[func_name] = gfortran_type_to_fortran(mm.group("gtype"))
                    i = j + 1
                    break
                mm = FUNC_NO_IMPLICIT_RE.search(lines[j])
                if mm:
                    func_name = mm.group("func").lower()
                    func_lines[fname][lineno].add(func_name)
                    i = j + 1
                    break
            else:
                i += 1
            continue

        i += 1

    return dict(func_lines), func_types


# ---------------------------------------------------------------------------
# Argument extraction helpers for building interface blocks
# ---------------------------------------------------------------------------

def split_args(args_str: str) -> list[str]:
    """Split a comma-separated Fortran argument list, respecting nested parentheses."""
    args: list[str] = []
    depth = 0
    current: list[str] = []
    for c in args_str:
        if c == "(":
            depth += 1
            current.append(c)
        elif c == ")":
            depth -= 1
            current.append(c)
        elif c == "," and depth == 0:
            args.append("".join(current).strip())
            current = []
        else:
            current.append(c)
    if current:
        args.append("".join(current).strip())
    return [a for a in args if a]


def primary_identifier(expr: str) -> str:
    """
    Extract the leading identifier from a Fortran expression for type inference.
    E.g. "MAX(kbn,1)" → "max", "arr(i,j)" → "arr", "kbn" → "kbn".
    """
    expr = expr.strip()
    m = re.match(r"^([A-Za-z]\w*)", expr)
    return m.group(1) if m else expr


def find_func_args_in_proc(
    lines: list[str], func_name: str, proc_start: int, proc_end: int
) -> list[str]:
    """
    Find the first call to *func_name* within the procedure (proc_start..proc_end,
    0-based) and return the raw argument expressions.  Returns [] if not found.
    """
    pat = re.compile(r"\b" + re.escape(func_name) + r"\s*\(", re.IGNORECASE)
    for i in range(proc_start, proc_end + 1):
        line = lines[i]
        m = pat.search(line)
        if not m:
            continue
        start = m.end()          # position just after the opening '('
        depth = 1
        pos   = start
        while pos < len(line) and depth > 0:
            if line[pos] == "(":
                depth += 1
            elif line[pos] == ")":
                depth -= 1
            pos += 1
        if depth == 0:           # found the matching ')'
            return split_args(line[start : pos - 1])
    return []


def build_interface_block(
    func_names: list[str],
    func_types: dict[str, str],
    lines: list[str],
    proc_start: int,
    proc_end: int,
    real_type: str,
    int_type: str,
    indent: str,
) -> list[str]:
    """
    Build a Fortran INTERFACE block for the given list of function names.
    Returns a list of text lines (each ending with '\\n').
    """
    result = [f"{indent}interface\n"]

    for func_name in sorted(func_names):
        # Return type: prefer the compiler-reported type, fall back to implicit rules
        ret_type = func_types.get(func_name, implicit_type(func_name, real_type, int_type))

        # Extract argument expressions from the first call site in the procedure
        raw_args = find_func_args_in_proc(lines, func_name, proc_start, proc_end)

        if raw_args:
            # Build dummy argument names; use the call-site identifier when it is
            # a plain variable, otherwise fall back to a positional name a1, a2, …
            dummy_names: list[str] = []
            for k, expr in enumerate(raw_args):
                ident = primary_identifier(expr)
                dummy_names.append(ident if re.match(r"^[A-Za-z]\w*$", ident) else f"a{k + 1}")

            result.append(f"{indent}  {ret_type} function {func_name}({', '.join(dummy_names)})\n")

            # Group dummies by their implicit type
            by_type: dict[str, list[str]] = defaultdict(list)
            for dummy in dummy_names:
                by_type[implicit_type(dummy, real_type, int_type)].append(dummy)
            for arg_type, arg_list in sorted(by_type.items()):
                result.append(f"{indent}    {arg_type}, intent(in) :: {', '.join(arg_list)}\n")
        else:
            # No call found — emit a no-argument stub (rare; user can refine)
            result.append(f"{indent}  {ret_type} function {func_name}()\n")

        result.append(f"{indent}  end function {func_name}\n")

    result.append(f"{indent}end interface\n")
    return result


# ---------------------------------------------------------------------------
# Identify subroutine/function boundaries in source
# ---------------------------------------------------------------------------
PROC_START_RE = re.compile(
    r"^\s*((?:pure|elemental|recursive|impure)\s+)*"
    r"(subroutine|function|program|module\s+procedure)\s+(\w+)",
    re.IGNORECASE,
)
PROC_END_RE = re.compile(
    r"^\s*end\s*(?:(subroutine|function|program|module\s+procedure)\s*(\w+)?)?\s*$",
    re.IGNORECASE,
)
# Where to insert declarations: after the last non-blank USE/IMPLICIT/variable-decl line
# inside the procedure header block.
DECL_HEADER_RE = re.compile(
    r"^\s*(use\s|implicit\s|integer|real|double|complex|character|logical|type\s*\()",
    re.IGNORECASE,
)
# Interface block delimiters — needed to avoid misidentifying interface specs as procedures
INTERFACE_START_RE = re.compile(r"^\s*(?:abstract\s+)?interface(?:\s+\w+)?\s*$", re.IGNORECASE)
INTERFACE_END_RE   = re.compile(r"^\s*end\s+interface(?:\s+\w+)?\s*$",           re.IGNORECASE)


def find_procedures(lines: list[str]) -> list[dict]:
    """
    Returns a list of dicts:
        { 'name': str, 'start': int, 'end': int }   (0-based line indices)

    Interface blocks inside procedures are ignored so that 'end function' lines
    inside an interface spec don't prematurely close the enclosing procedure.
    """
    procs = []
    stack: list[tuple[str, int]] = []
    iface_depth = 0  # nesting depth inside interface blocks

    for i, line in enumerate(lines):
        stripped = line.rstrip()

        # Track interface block nesting
        if INTERFACE_START_RE.match(stripped):
            iface_depth += 1
            continue
        if INTERFACE_END_RE.match(stripped):
            iface_depth = max(iface_depth - 1, 0)
            continue

        # Ignore everything inside interface blocks
        if iface_depth > 0:
            continue

        if PROC_START_RE.match(stripped):
            m = PROC_START_RE.match(stripped)
            name = m.group(3)
            stack.append((name, i))
        elif PROC_END_RE.match(stripped) and stack:
            name, start = stack.pop()
            procs.append({"name": name, "start": start, "end": i})

    return procs


def find_insert_line(lines: list[str], proc_start: int, proc_end: int) -> int:
    """
    Find the best line to insert declarations:
    After the last USE / IMPLICIT / existing declaration line (including any
    existing interface blocks and their continuation lines) in the header block,
    but before the first executable statement.
    Falls back to just after the SUBROUTINE/FUNCTION line.
    """
    insert_at = proc_start + 1  # default: right after the header
    in_decl   = False           # True while inside a multi-line declaration (&)
    i = proc_start + 1
    while i < proc_end:
        raw     = lines[i]
        stripped = raw.strip()

        # Blank lines and pure comments don't break the declaration section
        if not stripped or stripped.startswith("!"):
            i += 1
            continue

        # Continuation of a previous declaration line (&…)
        is_continuation = stripped.startswith("&")
        if in_decl and is_continuation:
            in_decl = raw.rstrip().endswith("&")
            insert_at = i + 1
            i += 1
            continue

        if DECL_HEADER_RE.match(stripped):
            in_decl   = raw.rstrip().endswith("&")
            insert_at = i + 1
            i += 1
        elif INTERFACE_START_RE.match(stripped):
            # Skip the entire existing interface block, treating it as a declaration
            i += 1
            while i < proc_end and not INTERFACE_END_RE.match(lines[i].strip()):
                i += 1
            i += 1          # step past 'end interface'
            insert_at = i   # insert after the existing interface block
            in_decl = False
        else:
            break           # hit first executable statement
    return insert_at


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def make_decl_lines(indent: str, type_str: str, varnames: list[str], max_len: int = 100) -> list[str]:
    """
    Build one or more Fortran declaration lines for *varnames* of *type_str*,
    wrapping with a continuation character ('&') whenever a line would exceed
    *max_len* characters.

    Example output (max_len=40, indent='  ', type_str='integer'):
        '  integer :: alpha, beta, &\n'
        '             gamma\n'
    """
    prefix      = f"{indent}{type_str} :: "
    cont_indent = " " * len(prefix)   # align continuations under the first variable

    lines_out: list[str] = []
    current    = prefix
    first      = True

    for var in varnames:
        if first:
            current += var
            first = False
        else:
            # Check if ", var" fits on the current line
            addition = f", {var}"
            if len(current) + len(addition) > max_len:
                # End the current line with a trailing comma + continuation marker
                lines_out.append(current + ", &\n")
                current = cont_indent + var
            else:
                current += addition

    lines_out.append(current + "\n")
    return lines_out


# ---------------------------------------------------------------------------
# Core fix routine
# ---------------------------------------------------------------------------
def fix_file(
    source_path: str,
    line_vars: dict[int, set[str]],
    line_funcs: dict[int, set[str]] | None = None,
    func_types: dict[str, str] | None = None,
    real_type: str = "real",
    int_type: str = "integer",
    type_map: dict[str, str] | None = None,
    dry_run: bool = False,
    backup: bool = True,
) -> None:
    """
    line_vars   – { 1-based line: set(variable names) } from "Symbol has no IMPLICIT type"
    line_funcs  – { 1-based line: set(function names) } from "Function has no IMPLICIT type"
    func_types  – { func_name_lower: fortran_type } from "Return type mismatch" errors
    """
    path = Path(source_path)
    if not path.exists():
        print(f"[SKIP] File not found: {source_path}")
        return

    lines = path.read_text(errors="replace").splitlines(keepends=True)
    procs = find_procedures(lines)

    if line_funcs is None:
        line_funcs = {}
    if func_types is None:
        func_types = {}
    if type_map is None:
        type_map = {}

    # ------------------------------------------------------------------
    # Build per-procedure sets of undeclared variables and missing functions
    # ------------------------------------------------------------------
    proc_vars:  dict[int, set[str]] = defaultdict(set)
    proc_funcs: dict[int, set[str]] = defaultdict(set)

    def assign_to_proc(lineno: int, data: set[str], target: dict) -> bool:
        idx = lineno - 1
        for pi, proc in enumerate(procs):
            if proc["start"] <= idx <= proc["end"]:
                target[pi].update(data)
                return True
        return False

    for lineno, vars_ in line_vars.items():
        if not assign_to_proc(lineno, vars_, proc_vars):
            print(f"  [WARN] Line {lineno}: vars {vars_} not inside any known procedure — skipping")

    for lineno, funcs_ in line_funcs.items():
        if not assign_to_proc(lineno, funcs_, proc_funcs):
            print(f"  [WARN] Line {lineno}: funcs {funcs_} not inside any known procedure — skipping")

    all_procs = set(proc_vars) | set(proc_funcs)
    if not all_procs:
        print(f"  [INFO] Nothing to fix in {source_path}")
        return

    # ------------------------------------------------------------------
    # Build insertions: { 0-based line index: [text lines to insert] }
    # Variables first, then an interface block (so they end up in that order)
    # ------------------------------------------------------------------
    insertions: dict[int, list[str]] = defaultdict(list)

    for pi in sorted(all_procs):
        proc = procs[pi]
        insert_at = find_insert_line(lines, proc["start"], proc["end"])

        # Determine indentation from the first non-blank, non-comment line
        indent = "  "
        for i in range(proc["start"] + 1, proc["end"]):
            s = lines[i]
            stripped = s.lstrip()
            if stripped and not stripped.startswith("!"):
                indent = s[: len(s) - len(stripped)]
                break

        # --- Variable declarations ---
        vars_ = proc_vars.get(pi, set())
        if vars_:
            by_type: dict[str, list[str]] = defaultdict(list)
            for v in sorted(vars_):
                t = type_map.get(v.lower()) or implicit_type(v, real_type, int_type)
                by_type[t].append(v)

            decl_lines: list[str] = []
            for t, vlist in sorted(by_type.items()):
                decl_lines.extend(make_decl_lines(indent, t, sorted(vlist)))

            insertions[insert_at].extend(decl_lines)
            print(f"  [{proc['name']}] vars at line {insert_at + 1}: " +
                  ", ".join(
                      f"{v}({type_map.get(v.lower()) or implicit_type(v, real_type, int_type)})"
                      for v in sorted(vars_)))

        # --- Interface block for missing functions ---
        funcs_ = proc_funcs.get(pi, set())
        if funcs_:
            iface_lines = build_interface_block(
                func_names=sorted(funcs_),
                func_types=func_types,
                lines=lines,
                proc_start=proc["start"],
                proc_end=proc["end"],
                real_type=real_type,
                int_type=int_type,
                indent=indent,
            )
            insertions[insert_at].extend(iface_lines)
            print(f"  [{proc['name']}] interface at line {insert_at + 1}: " +
                  ", ".join(
                      f"{f}({func_types.get(f, implicit_type(f, real_type, int_type))})"
                      for f in sorted(funcs_)))

    if dry_run:
        print("\n--- DRY RUN: No files modified ---")
        for insert_at in sorted(insertions):
            print(f"\n  >> Would insert before line {insert_at + 1}:")
            for dl in insertions[insert_at]:
                print("    " + dl.rstrip())
        return

    # Apply insertions (in reverse order so line numbers stay valid)
    new_lines = list(lines)
    for insert_at in sorted(insertions.keys(), reverse=True):
        for decl in reversed(insertions[insert_at]):
            new_lines.insert(insert_at, decl)

    if backup:
        shutil.copy2(path, path.with_suffix(path.suffix + ".bak"))

    path.write_text("".join(new_lines))
    print(f"  [DONE] Written: {source_path}")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------
def parse_type_map(s: str) -> dict[str, str]:
    result = {}
    for entry in s.split(","):
        parts = entry.strip().split(":")
        if len(parts) == 2:
            result[parts[0].strip().lower()] = parts[1].strip()
    return result


def main():
    parser = argparse.ArgumentParser(
        description="Auto-declare implicitly typed variables after adding IMPLICIT NONE."
    )
    parser.add_argument("source", help="Fortran source file (.f90 / .f)")
    parser.add_argument("errors", help="File containing compiler error output")
    parser.add_argument("--real-type", default="real",
                        help="Fortran type for real vars (default: 'real'). E.g. 'real(8)' or 'double precision'")
    parser.add_argument("--int-type", default="integer",
                        help="Fortran type for integer vars (default: 'integer'). E.g. 'integer(4)'")
    parser.add_argument("--type-map", default="",
                        help="Explicit overrides: 'varname:type,...' e.g. 'flag:logical,n:integer(8)'")
    parser.add_argument("--dry-run", action="store_true",
                        help="Print what would be inserted without modifying files")
    parser.add_argument("--no-backup", action="store_true",
                        help="Skip creating .bak backup of the original file")
    args = parser.parse_args()

    error_text = Path(args.errors).read_text(errors="replace")
    all_errors = parse_errors(error_text)
    all_func_errors, func_types = parse_function_errors(error_text)

    type_map = parse_type_map(args.type_map) if args.type_map else {}
    source_path = Path(args.source)

    def find_key(mapping: dict, src: Path) -> str | None:
        """Return the key in *mapping* that matches *src* by path, name, or stem."""
        for key in mapping:
            if Path(key).resolve() == src.resolve() or Path(key).name == src.name:
                return key
        for key in mapping:
            if Path(key).stem == src.stem:
                return key
        return None

    matched_key      = find_key(all_errors,      source_path)
    func_matched_key = find_key(all_func_errors,  source_path)

    if matched_key is None and func_matched_key is None:
        all_keys = sorted(set(all_errors) | set(all_func_errors))
        print(f"No errors found for '{args.source}' in error file.")
        print(f"Files mentioned in errors: {all_keys}")
        sys.exit(1)

    line_vars  = all_errors.get(matched_key,      {}) if matched_key      else {}
    line_funcs = all_func_errors.get(func_matched_key, {}) if func_matched_key else {}

    print(f"\nProcessing: {args.source}")
    fix_file(
        source_path=str(source_path),
        line_vars=line_vars,
        line_funcs=line_funcs,
        func_types=func_types,
        real_type=args.real_type,
        int_type=args.int_type,
        type_map=type_map,
        dry_run=args.dry_run,
        backup=not args.no_backup,
    )


if __name__ == "__main__":
    main()
