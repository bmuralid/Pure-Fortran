#!/usr/bin/env python3
"""xsort_module.py

Reorder the module-scope procedures of one or more Fortran modules into a
better display order while preserving the number of procedures in each module.

Order used within each module:
1. collapse strongly connected components (SCCs), so mutually recursive
   procedures stay together
2. order SCCs by increasing helper-side level, where level 0 means the SCC has
   no outgoing same-module dependencies and higher levels depend on lower ones
3. within the same level, sort SCCs by increasing number of dependencies,
   then increasing number of reverse dependencies, then original source order
4. within an SCC, preserve original source order

By default, if the source file contains multiple modules, the script sorts the
procedures in each module. Other program units, including a main program, are
left unchanged.
"""

from __future__ import annotations

import argparse
import sys
import re
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Set, Tuple

this_dir = Path(__file__).resolve().parent
for d in (this_dir, this_dir / "py"):
    if d.is_dir() and str(d) not in sys.path:
        sys.path.insert(0, str(d))

try:
    import fortran_scan  # type: ignore
except Exception as e:
    raise SystemExit(
        "failed to import fortran_scan.py. put xsort_module.py in the same folder as fortran_scan.py\n"
        f"import error: {e}"
    )

_module_start_re = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
_end_module_re = re.compile(r"^\s*end\s+module\b(?:\s+([a-z][a-z0-9_]*))?", re.IGNORECASE)
_contains_re = re.compile(r"^\s*contains\b", re.IGNORECASE)
_module_procedure_stmt_re = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)
_call_desig_re = re.compile(
    r"\bcall\s+([a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)*)\b", re.IGNORECASE
)
_name_paren_re = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
_typebound_paren_re = re.compile(r"%\s*([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)


ModuleSpan = Tuple[str, int, int, Optional[int]]


def strip_string_literals(stmt: str) -> str:
    """Replace content inside quotes with spaces."""
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
            if ch not in ("'", '"'):
                out[i] = " "
        i += 1
    return "".join(out)


def find_module_spans(
    stmts: Sequence[Tuple[int, str]],
) -> List[ModuleSpan]:
    """Return all complete top-level module spans in source order."""
    spans: List[ModuleSpan] = []
    current: Optional[str] = None
    start_line = -1
    contains_line: Optional[int] = None

    for lineno, stmt in stmts:
        low = stmt.strip().lower()
        if not low:
            continue

        m_start = _module_start_re.match(low)
        if m_start and not _module_procedure_stmt_re.match(low):
            name = m_start.group(1).lower()
            if current is None:
                current = name
                start_line = lineno
                contains_line = None
            continue

        if current is not None and contains_line is None and _contains_re.match(low):
            contains_line = lineno
            continue

        if current is not None:
            m_end = _end_module_re.match(low)
            if m_end:
                end_name = (m_end.group(1) or "").lower()
                if not end_name or end_name == current:
                    spans.append((current, start_line, lineno, contains_line))
                    current = None
                    start_line = -1
                    contains_line = None

    return spans


def module_scope_procs(
    finfo: "fortran_scan.SourceFileInfo",  # type: ignore
    contains_line: Optional[int],
    end_line: int,
) -> List["fortran_scan.Procedure"]:  # type: ignore
    """Return top-level procedures in one module subprogram part."""
    if contains_line is None:
        return []
    out = []
    for p in finfo.procedures:
        if p.parent is None and contains_line < p.start < end_line:
            out.append(p)
    out.sort(key=lambda p: p.start)
    return out


def extract_deps_for_proc(
    proc: "fortran_scan.Procedure",  # type: ignore
    module_proc_names: Set[str],
) -> Set[str]:
    """Return same-module procedure names referenced from one procedure body."""
    deps: Set[str] = set()
    self_name = proc.name.lower()

    for _lineno, stmt in proc.body:
        s = strip_string_literals(stmt).lower()
        if not s:
            continue

        for m in _call_desig_re.finditer(s):
            parts = [p.strip() for p in m.group(1).split("%") if p.strip()]
            if parts:
                callee = parts[-1]
                if callee != self_name and callee in module_proc_names:
                    deps.add(callee)

        for m in _name_paren_re.finditer(s):
            callee = m.group(1).lower()
            if callee != self_name and callee in module_proc_names:
                deps.add(callee)

        for m in _typebound_paren_re.finditer(s):
            callee = m.group(1).lower()
            if callee != self_name and callee in module_proc_names:
                deps.add(callee)

    return deps


def reverse_deps(deps: Dict[str, Set[str]]) -> Dict[str, Set[str]]:
    """Return the reverse dependency graph."""
    rev = {name: set() for name in deps}
    for caller, callees in deps.items():
        for callee in callees:
            rev.setdefault(callee, set()).add(caller)
    return rev


def tarjan_scc(graph: Dict[str, Set[str]]) -> List[List[str]]:
    """Return strongly connected components of a directed graph."""
    index = 0
    indices: Dict[str, int] = {}
    lowlink: Dict[str, int] = {}
    stack: List[str] = []
    on_stack: Set[str] = set()
    comps: List[List[str]] = []

    sys.setrecursionlimit(max(4000, 10 * len(graph) + 1000))

    def strongconnect(v: str) -> None:
        nonlocal index
        indices[v] = index
        lowlink[v] = index
        index += 1
        stack.append(v)
        on_stack.add(v)

        for w in graph.get(v, set()):
            if w not in indices:
                strongconnect(w)
                lowlink[v] = min(lowlink[v], lowlink[w])
            elif w in on_stack:
                lowlink[v] = min(lowlink[v], indices[w])

        if lowlink[v] == indices[v]:
            comp: List[str] = []
            while True:
                w = stack.pop()
                on_stack.remove(w)
                comp.append(w)
                if w == v:
                    break
            comps.append(comp)

    for v in sorted(graph):
        if v not in indices:
            strongconnect(v)

    return comps


def scc_order(
    deps: Dict[str, Set[str]],
    rev: Dict[str, Set[str]],
    name_to_start: Dict[str, int],
) -> List[List[str]]:
    """Return SCCs ordered helper-side first, then by tie-breakers."""
    comps = tarjan_scc(deps)
    comp_id: Dict[str, int] = {}
    for i, comp in enumerate(comps):
        for name in comp:
            comp_id[name] = i

    dag: Dict[int, Set[int]] = {i: set() for i in range(len(comps))}
    for caller, callees in deps.items():
        a = comp_id[caller]
        for callee in callees:
            b = comp_id[callee]
            if a != b:
                dag[a].add(b)

    memo: Dict[int, int] = {}

    def level(i: int) -> int:
        if i in memo:
            return memo[i]
        if not dag[i]:
            memo[i] = 0
        else:
            memo[i] = 1 + max(level(j) for j in dag[i])
        return memo[i]

    for i in range(len(comps)):
        level(i)

    def comp_key(i: int) -> Tuple[int, int, int, int, str]:
        comp = comps[i]
        starts = sorted(name_to_start[n] for n in comp)
        total_deps = sum(len(deps[n]) for n in comp)
        total_rev = sum(len(rev[n]) for n in comp)
        first_name = min(comp)
        return (memo[i], total_deps, total_rev, starts[0], first_name)

    ordered = []
    for i in sorted(range(len(comps)), key=comp_key):
        ordered.append(sorted(comps[i], key=lambda n: name_to_start[n]))
    return ordered


def build_proc_blocks(
    lines: Sequence[str],
    procs: Sequence["fortran_scan.Procedure"],  # type: ignore
    module_end_line: int,
) -> Dict[str, List[str]]:
    """Return exact text block for each top-level procedure, including trailing trivia."""
    blocks: Dict[str, List[str]] = {}
    procs_sorted = sorted(procs, key=lambda p: p.start)
    for i, proc in enumerate(procs_sorted):
        start0 = proc.start - 1
        if i + 1 < len(procs_sorted):
            end0 = procs_sorted[i + 1].start - 2
        else:
            end0 = module_end_line - 2
        blocks[proc.name.lower()] = list(lines[start0 : end0 + 1])
    return blocks


def render_sorted_module(
    lines: Sequence[str],
    contains_line: Optional[int],
    module_end_line: int,
    ordered_sccs: Sequence[Sequence[str]],
    blocks: Dict[str, List[str]],
) -> str:
    """Return rewritten module text with procedures reordered."""
    if contains_line is None:
        return "".join(lines)

    before = list(lines[: contains_line - 1])
    subprogram_intro = [lines[contains_line - 1]]
    end_module = list(lines[module_end_line - 1 :])

    body: List[str] = []
    for i, comp in enumerate(ordered_sccs):
        for name in comp:
            body.extend(blocks[name])
            if body and body[-1] and not body[-1].endswith("\n"):
                body[-1] += "\n"
        if i + 1 < len(ordered_sccs):
            if not body or body[-1].strip():
                body.append("\n")

    out_lines = before + subprogram_intro + body + end_module
    return "".join(out_lines)


def default_out_path(path: Path) -> Path:
    """Return default output path next to the input file."""
    return path.with_name(path.stem + "_sorted" + path.suffix)


def default_report_path(path: Path) -> Path:
    """Return default report path next to the input file."""
    return path.with_name(path.stem + "_sorted_order.txt")


def load_finfo(path: Path):
    """Load one Fortran source file."""
    infos, any_missing = fortran_scan.load_source_files([path])
    if any_missing or not infos:
        raise FileNotFoundError(str(path))
    return infos[0]


def analyze_module_span(
    finfo: "fortran_scan.SourceFileInfo",  # type: ignore
    span: ModuleSpan,
):
    """Compute procedure dependency information for one module span."""
    mod_name, mod_start, mod_end, contains_line = span
    procs = module_scope_procs(finfo, contains_line, mod_end)
    name_to_start = {p.name.lower(): p.start for p in procs}
    module_proc_names = set(name_to_start)
    deps = {p.name.lower(): extract_deps_for_proc(p, module_proc_names) for p in procs}
    rev = reverse_deps(deps)
    ordered_sccs = scc_order(deps, rev, name_to_start)
    return {
        "name": mod_name,
        "start": mod_start,
        "end": mod_end,
        "contains": contains_line,
        "procs": procs,
        "deps": deps,
        "rev": rev,
        "name_to_start": name_to_start,
        "ordered_sccs": ordered_sccs,
    }


def format_module_report(info) -> str:
    """Format one module order report."""
    name = info["name"]
    procs = info["procs"]
    deps = info["deps"]
    rev = info["rev"]
    name_to_start = info["name_to_start"]
    ordered_sccs = info["ordered_sccs"]

    out: List[str] = []
    out.append(f"module: {name}\n")
    out.append(f"procedures: {len(procs)}\n")
    out.append(f"scc groups: {len(ordered_sccs)}\n")
    out.append("order:\n")
    for comp in ordered_sccs:
        if len(comp) == 1:
            proc = comp[0]
            out.append(
                f"  {proc:30s} deps={len(deps[proc]):3d} rev={len(rev[proc]):3d} line={name_to_start[proc]}\n"
            )
        else:
            names = ", ".join(comp)
            ndep = sum(len(deps[n]) for n in comp)
            nrev = sum(len(rev[n]) for n in comp)
            line0 = min(name_to_start[n] for n in comp)
            out.append(f"  [{names}] deps={ndep:3d} rev={nrev:3d} line={line0}\n")
    return "".join(out)


def main() -> int:
    """Run the command-line interface."""
    parser = argparse.ArgumentParser(
        description="reorder procedures within each Fortran module in dependency-aware display order"
    )
    parser.add_argument("source", help="path to a Fortran source file")
    parser.add_argument(
        "--module",
        help="only sort this one module; default is to sort all complete modules in the file",
    )
    parser.add_argument("--out", help="output source file path")
    parser.add_argument("--report", help="optional text report path")
    parser.add_argument(
        "--check-only",
        action="store_true",
        help="analyze and print the proposed order without writing an output file",
    )
    args = parser.parse_args()

    path = Path(args.source)
    finfo = load_finfo(path)
    stmts = list(fortran_scan.iter_fortran_statements(finfo.parsed_lines))
    spans = find_module_spans(stmts)
    if not spans:
        raise SystemExit("no complete modules found in source file")

    if args.module:
        target = args.module.lower()
        spans = [span for span in spans if span[0] == target]
        if not spans:
            raise SystemExit(f"module '{args.module}' not found")

    module_infos = [analyze_module_span(finfo, span) for span in spans]

    report_parts: List[str] = []
    for i, info in enumerate(module_infos):
        proc_count_in = len(info["procs"])
        proc_count_out = sum(len(comp) for comp in info["ordered_sccs"])
        if proc_count_out != proc_count_in:
            raise SystemExit(
                f"internal error in module {info['name']}: input has {proc_count_in} procedures "
                f"but output order has {proc_count_out}"
            )
        if i:
            report_parts.append("\n")
        report_parts.append(format_module_report(info))

    report_text = "".join(report_parts)
    sys.stdout.write(report_text)

    if args.check_only:
        return 0

    out_lines = list(finfo.lines)
    for info in sorted(module_infos, key=lambda d: d["start"], reverse=True):
        if info["contains"] is None:
            continue
        blocks = build_proc_blocks(out_lines, info["procs"], info["end"])
        new_module_text = render_sorted_module(
            out_lines[info["start"] - 1 : info["end"]],
            None if info["contains"] is None else info["contains"] - info["start"] + 1,
            info["end"] - info["start"] + 1,
            info["ordered_sccs"],
            blocks,
        )
        out_lines[info["start"] - 1 : info["end"]] = new_module_text.splitlines(True)

    out_path = Path(args.out) if args.out else default_out_path(path)
    out_path.write_text("".join(out_lines), encoding="utf-8")
    print(f"wrote {out_path}")

    if args.report:
        report_path = Path(args.report)
        report_path.write_text(report_text, encoding="utf-8")
        print(f"wrote {report_path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
