#!/usr/bin/env python3
"""xproc_dep.py

Analyze a Fortran module and report intra-module procedure dependencies.

For each procedure defined in the chosen module (module-scope procedures after
CONTAINS), this script finds which other procedures in the same module it calls.

Output is sorted by increasing number of dependencies (callees).

Notes / limitations (conservative, but practical):
- Detects subroutine calls via CALL <designator>
- Detects function-style calls by identifier followed by '('
- Also detects type-bound calls like obj%proc(...)
- Does NOT infer implicit calls through overloaded operators, assignments, etc.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Set, Tuple


# Allow running next to the helper scripts in py.zip (fortran_scan.py, etc.)
THIS_DIR = Path(__file__).resolve().parent
IMPORT_DIRS = [THIS_DIR, THIS_DIR / 'py']
for _d in IMPORT_DIRS:
    if _d.is_dir() and str(_d) not in sys.path:
        sys.path.insert(0, str(_d))

try:
    import fortran_scan  # type: ignore
except Exception as e:
    raise SystemExit(
        "Failed to import fortran_scan.py. Put xproc_dep.py in the same folder as fortran_scan.py.\n"
        f"Import error: {e}"
    )


_MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
_END_MODULE_RE = re.compile(r"^\s*end\s+module\b(?:\s+([a-z][a-z0-9_]*))?", re.IGNORECASE)
_CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)
_MODULE_PROCEDURE_STMT_RE = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)

# CALL statement designator: call a%b%c(...)  -> we take the last component.
_CALL_DESIG_RE = re.compile(
    r"\bcall\s+([a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)*)\b", re.IGNORECASE
)

# Function-style invocation (or array ref): name( ... )
_NAME_PAREN_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)

# Type-bound invocation: obj%name( ... )
_TYPEBOUND_PAREN_RE = re.compile(r"%\s*([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)


def _strip_string_literals(stmt: str) -> str:
    """Replace content inside single/double quotes with spaces (keep length)."""
    out = list(stmt)
    in_single = False
    in_double = False
    i = 0
    while i < len(out):
        ch = out[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            i += 1
            continue
        if in_single or in_double:
            # Keep quotes, blank out interior.
            if ch not in ("'", '"'):
                out[i] = " "
        i += 1
    return "".join(out)


def _find_module_span(
    stmts: Sequence[Tuple[int, str]],
    module_name: Optional[str],
) -> Tuple[str, int, int, Optional[int]]:
    """Return (module_name, start_line, end_line, contains_line)."""
    target = module_name.lower() if module_name else None
    current: Optional[str] = None
    start_line = -1
    contains_line: Optional[int] = None

    for lineno, stmt in stmts:
        low = stmt.strip().lower()
        if not low:
            continue

        m_start = _MODULE_START_RE.match(low)
        if m_start and not _MODULE_PROCEDURE_STMT_RE.match(low):
            name = m_start.group(1).lower()
            # skip "module procedure" (already handled) and "module function/subroutine" won't match
            # because MODULE_START_RE expects 'module <name>' only.
            if current is None:
                if target is None or name == target:
                    current = name
                    start_line = lineno
                    contains_line = None
            continue

        if current is not None and contains_line is None and _CONTAINS_RE.match(low):
            contains_line = lineno
            continue

        if current is not None:
            m_end = _END_MODULE_RE.match(low)
            if m_end:
                end_name = (m_end.group(1) or "").lower()
                if not end_name or end_name == current:
                    return current, start_line, lineno, contains_line

    raise ValueError(
        "Could not find a complete module span in the file"
        + (f" for module '{module_name}'." if module_name else ".")
    )


def _module_scope_procs(
    finfo: "fortran_scan.SourceFileInfo",  # type: ignore
    contains_line: Optional[int],
    end_line: int,
) -> List["fortran_scan.Procedure"]:  # type: ignore
    """Procedures that are module-scope (parent is None) and inside CONTAINS..END MODULE."""
    if contains_line is None:
        return []
    out = []
    for p in finfo.procedures:
        if p.parent is not None:
            continue
        if p.start > contains_line and p.start < end_line:
            out.append(p)
    return out


def _extract_deps_for_proc(
    proc: "fortran_scan.Procedure",  # type: ignore
    module_proc_names: Set[str],
) -> Set[str]:
    deps: Set[str] = set()
    self_name = proc.name.lower()

    for _lineno, stmt in proc.body:
        # Comments are already stripped by fortran_scan.join_continued_lines, but
        # we still strip strings to avoid matching identifiers inside literals.
        s = _strip_string_literals(stmt).lower()
        if not s:
            continue

        # CALL statements
        for m in _CALL_DESIG_RE.finditer(s):
            desig = m.group(1)
            parts = [p.strip() for p in desig.split("%") if p.strip()]
            if not parts:
                continue
            callee = parts[-1]
            if callee != self_name and callee in module_proc_names:
                deps.add(callee)

        # Function-style calls: name(...)
        for m in _NAME_PAREN_RE.finditer(s):
            callee = m.group(1).lower()
            if callee != self_name and callee in module_proc_names:
                deps.add(callee)

        # Type-bound calls: obj%name(...)
        for m in _TYPEBOUND_PAREN_RE.finditer(s):
            callee = m.group(1).lower()
            if callee != self_name and callee in module_proc_names:
                deps.add(callee)

    return deps


def analyze_module(path: Path, module_name: Optional[str]) -> Tuple[str, Dict[str, Set[str]]]:
    infos, any_missing = fortran_scan.load_source_files([path])
    if any_missing or not infos:
        raise FileNotFoundError(str(path))
    finfo = infos[0]

    stmts = fortran_scan.iter_fortran_statements(finfo.parsed_lines)
    mod_name, _mod_start, mod_end, contains_line = _find_module_span(stmts, module_name)
    procs = _module_scope_procs(finfo, contains_line, mod_end)

    module_proc_names = {p.name.lower() for p in procs}

    deps: Dict[str, Set[str]] = {}
    for p in procs:
        deps[p.name.lower()] = _extract_deps_for_proc(p, module_proc_names)
    return mod_name, deps


def reverse_deps(deps: Dict[str, Set[str]]) -> Dict[str, Set[str]]:
    rev: Dict[str, Set[str]] = {name: set() for name in deps}
    for caller, callees in deps.items():
        for callee in callees:
            rev.setdefault(callee, set()).add(caller)
    return rev


def format_report(module_name: str, deps: Dict[str, Set[str]]) -> str:
    rev = reverse_deps(deps)
    names = sorted(deps.keys(), key=lambda n: (len(deps[n]), len(rev[n]), n))
    lines: List[str] = []
    lines.append(f"module: {module_name}")
    lines.append(f"procedures: {len(names)}")
    edges = sum(len(deps[n]) for n in names)
    lines.append(f"dependency edges: {edges}")
    lines.append("")
    lines.append("ndep  nrev  procedure: callees")
    lines.append("----  ----  -------------------")
    for n in names:
        callees = sorted(deps[n])
        if callees:
            lines.append(f"{len(callees):4d}  {len(rev[n]):4d}  {n}: {', '.join(callees)}")
        else:
            lines.append(f"{0:4d}  {len(rev[n]):4d}  {n}:")
    return "\n".join(lines) + "\n"


def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(
        prog="xproc_dep.py",
        description="List Fortran module procedures sorted by number of intra-module callees, and show reverse dependency counts.",
    )
    ap.add_argument("path", type=Path, help="Fortran source file containing the module")
    ap.add_argument(
        "--module",
        dest="module_name",
        default=None,
        help="Module name to analyze (defaults to first module in file)",
    )
    ap.add_argument(
        "--out",
        type=Path,
        default=None,
        help="Write report to this path (defaults to stdout)",
    )
    ns = ap.parse_args(argv)

    mod, deps = analyze_module(ns.path, ns.module_name)
    report = format_report(mod, deps)

    if ns.out is not None:
        ns.out.write_text(report, encoding="utf-8")
    else:
        sys.stdout.write(report)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
