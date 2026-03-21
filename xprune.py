#!/usr/bin/env python3
"""Prune likely unused Fortran procedures with optional compile validation."""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Dict, Iterable, List, Optional, Set, Tuple, Union

import cli_paths as cpaths
import fortran_scan as fscan

PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
PROC_END_RE = re.compile(r"^\s*end(?:\s+(?:function|subroutine))?\b", re.IGNORECASE)
CALL_RE = re.compile(r"\bcall\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
USE_RE = re.compile(
    r"^\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?"
    r"(?:\s*::\s*|\s+)([a-z][a-z0-9_]*)(.*)$",
    re.IGNORECASE,
)
FUNC_REF_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
PUBLIC_RE = re.compile(r"^\s*public\b(.*)$", re.IGNORECASE)
PROGRAM_START_RE = re.compile(r"^\s*program\s+[a-z][a-z0-9_]*\b", re.IGNORECASE)


@dataclass
class ProcRef:
    """Store removable procedure location metadata."""

    path: Path
    name: str
    kind: str
    start: int
    end: int


@dataclass
class InterfaceRef:
    """Store removable generic interface location metadata."""

    path: Path
    name: str
    start: int
    end: int
    members: Set[str]


@dataclass(frozen=True)
class RemovalCandidate:
    """One prune candidate that can be removed in a batch."""

    path: Path
    name: str
    start: int
    end: int
    kind: str
    ref: Union[ProcRef, InterfaceRef]


def quote_cmd_arg(arg: str) -> str:
    """Quote one shell argument for command construction."""
    return subprocess.list2cmdline([arg])


def parse_use_only_names(rest: str) -> Optional[Set[str]]:
    """Return names imported by USE, ONLY or None for wildcard use."""
    m = re.search(r"\bonly\s*:\s*(.*)$", rest, re.IGNORECASE)
    if not m:
        return None
    rhs = m.group(1).strip()
    if not rhs:
        return set()
    names: Set[str] = set()
    for tok in rhs.split(","):
        t = tok.strip()
        if not t:
            continue
        if "=>" in t:
            t = t.split("=>", 1)[1].strip()
        if t.lower().startswith(("operator(", "assignment(")):
            continue
        mm = re.match(r"^([a-z][a-z0-9_]*)$", t, re.IGNORECASE)
        if mm:
            names.add(mm.group(1).lower())
    return names


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one line into code and trailing comment text."""
    in_single = False
    in_double = False
    for i, ch in enumerate(line):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return line[:i], line[i:]
    return line, ""


def get_eol(line: str) -> str:
    """Return the input line ending sequence."""
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


def split_top_level_commas(text: str) -> List[str]:
    """Split text on top-level commas outside parentheses and strings."""
    out: List[str] = []
    cur: List[str] = []
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


def strip_name_from_public_stmt(stmt: str, target: str) -> Tuple[str, bool]:
    """Remove one name from a PUBLIC statement if present."""
    m = PUBLIC_RE.match(stmt.strip())
    if not m:
        return stmt, False
    rest = m.group(1).strip()
    if not rest:
        return stmt, False
    if rest.startswith("::"):
        rest = rest[2:].strip()
    elif rest.startswith(","):
        rest = rest[1:].strip()
    if not rest:
        return stmt, False
    names = split_top_level_commas(rest)
    kept: List[str] = []
    removed = False
    for n in names:
        token = n.strip().lower()
        if token == target.lower():
            removed = True
        else:
            kept.append(n.strip())
    if not removed:
        return stmt, False
    indent = re.match(r"^\s*", stmt).group(0)
    if kept:
        return f"{indent}public :: {', '.join(kept)}", True
    return "", True


def remove_name_from_public_lines(lines: List[str], target: str) -> List[str]:
    """Remove target symbol from PUBLIC accessibility statements across a file."""
    out: List[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        code0, _comment0 = split_code_comment(line.rstrip("\r\n"))
        if not PUBLIC_RE.match(code0.strip()):
            out.append(line)
            i += 1
            continue

        # Collect continuation block for this PUBLIC statement.
        block_idx = [i]
        segs: List[str] = []
        seg0 = code0.rstrip()
        cont = seg0.endswith("&")
        if cont:
            seg0 = seg0[:-1].rstrip()
        if seg0.strip():
            segs.append(seg0)

        j = i + 1
        while j < len(lines):
            codej, _commentj = split_code_comment(lines[j].rstrip("\r\n"))
            lead = codej.lstrip()
            if not (cont or lead.startswith("&")):
                break
            if lead.startswith("&"):
                segj = lead[1:].lstrip()
            else:
                segj = lead
            cont = segj.rstrip().endswith("&")
            if cont:
                segj = segj[:-1].rstrip()
            if segj.strip():
                segs.append(segj)
            block_idx.append(j)
            j += 1

        combined = " ".join(segs).strip()
        changed_any = False
        kept_parts: List[str] = []
        for stmt in fscan.split_fortran_statements(combined):
            new_stmt, changed = strip_name_from_public_stmt(stmt, target)
            changed_any = changed_any or changed
            if new_stmt.strip():
                kept_parts.append(new_stmt.strip())

        if changed_any:
            eol = get_eol(lines[i]) or "\n"
            rebuilt = "; ".join(kept_parts).strip()
            if rebuilt:
                indent = re.match(r"^\s*", lines[i]).group(0)
                out.append(f"{indent}{rebuilt}{eol}")
            else:
                out.append(eol)
            for _ in block_idx[1:]:
                out.append(eol)
        else:
            for k in block_idx:
                out.append(lines[k])
        i = j
    return out


def run_compile(command: str, files: List[Path], phase: str, cwd: Path) -> bool:
    """Run compile command in cwd and print pass/fail diagnostics."""
    file_args = " ".join(quote_cmd_arg(str(p.name)) for p in files)
    if "{files}" in command:
        cmd = command.replace("{files}", file_args)
    else:
        cmd = f"{command} {file_args}".strip()
    print(f"Compile ({phase}): {cmd}")
    proc = subprocess.run(cmd, shell=True, capture_output=True, text=True, cwd=str(cwd))
    if proc.returncode == 0:
        print(f"Compile ({phase}): PASS")
        return True
    print(f"Compile ({phase}): FAIL (exit {proc.returncode})")
    if proc.stdout:
        print(proc.stdout.rstrip())
    if proc.stderr:
        print(proc.stderr.rstrip())
    return False


def choose_files(args_files: List[Path], exclude: List[str]) -> List[Path]:
    """Resolve source files from CLI args or current directory defaults."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def copy_to_out_dir(files: List[Path], out_dir: Path) -> List[Path]:
    """Copy source files into out_dir and return copied paths."""
    out_dir.mkdir(parents=True, exist_ok=True)
    copied: List[Path] = []
    for p in files:
        dst = out_dir / p.name
        shutil.copy2(p, dst)
        copied.append(dst)
    return copied


def collect_procedures(infos: List[fscan.SourceFileInfo]) -> List[ProcRef]:
    """Collect top-level procedures that are candidates for pruning."""
    out: List[ProcRef] = []
    for finfo in infos:
        for proc in finfo.procedures:
            if proc.parent is not None:
                continue
            out.append(ProcRef(finfo.path, proc.name.lower(), proc.kind, proc.start, proc.end))
    return out


def collect_interfaces(infos: List[fscan.SourceFileInfo]) -> List[InterfaceRef]:
    """Collect named generic interface blocks that are candidates for pruning."""
    out: List[InterfaceRef] = []
    iface_start_re = re.compile(r"^\s*interface\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
    iface_end_re = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)
    member_re = re.compile(r"^\s*module\s+procedure\s+(.+)$", re.IGNORECASE)

    for finfo in infos:
        active_name: str | None = None
        active_start = 0
        active_members: Set[str] = set()
        for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
            low = stmt.strip().lower()
            m_start = iface_start_re.match(low)
            if m_start and not low.startswith("abstract interface"):
                active_name = m_start.group(1).lower()
                active_start = lineno
                active_members = set()
                continue
            if active_name is not None:
                m_member = member_re.match(low)
                if m_member:
                    for raw in split_top_level_commas(m_member.group(1)):
                        nm = raw.strip().lower()
                        if re.match(r"^[a-z][a-z0-9_]*$", nm):
                            active_members.add(nm)
                    continue
                if iface_end_re.match(low):
                    out.append(
                        InterfaceRef(
                            path=finfo.path,
                            name=active_name,
                            start=active_start,
                            end=lineno,
                            members=set(active_members),
                        )
                    )
                    active_name = None
                    active_start = 0
                    active_members = set()
    return out


def collect_used_names(
    infos: List[fscan.SourceFileInfo], known_names: Set[str], whole_program: bool = False
) -> Set[str]:
    """Collect referenced procedure names from use/call/function-reference patterns."""
    used: Set[str] = set()
    for finfo in infos:
        for _lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
            low = stmt.strip().lower()
            if not low:
                continue
            if PROC_START_RE.match(low) or PROC_END_RE.match(low):
                continue

            m_use = USE_RE.match(low)
            if m_use:
                only_names = parse_use_only_names(m_use.group(2) or "")
                if only_names is None:
                    # In library mode, wildcard USE could expose any public entry point.
                    # In whole-program mode we can rely on actual references instead.
                    if not whole_program:
                        used.update(known_names)
                else:
                    used.update(n for n in only_names if n in known_names)
                continue

            for m in CALL_RE.finditer(low):
                n = m.group(1).lower()
                if n in known_names:
                    used.add(n)

            for m in FUNC_REF_RE.finditer(low):
                n = m.group(1).lower()
                if n in known_names:
                    used.add(n)
    return used


def has_program_unit(infos: List[fscan.SourceFileInfo]) -> bool:
    """Return True when the loaded sources include a complete-program entry point."""
    for finfo in infos:
        for _lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
            if PROGRAM_START_RE.match(stmt.strip()):
                return True
    return False


def remove_symbol_block_from_lines(lines: List[str], start: int, end: int, name: str) -> List[str]:
    """Delete one symbol block from source lines without reloading the file."""
    start_idx = max(0, start - 1)
    end_idx = min(len(lines), end)
    del lines[start_idx:end_idx]
    return remove_name_from_public_lines(lines, name)


def remove_procedure_block(path: Path, proc: ProcRef) -> str:
    """Delete a procedure block from a source file and return previous text."""
    old_text = path.read_text(encoding="utf-8", errors="ignore")
    lines = old_text.splitlines(keepends=True)
    lines = remove_symbol_block_from_lines(lines, proc.start, proc.end, proc.name)
    path.write_text("".join(normalize_blank_lines(lines)), encoding="utf-8", newline="")
    return old_text


def remove_interface_block(path: Path, iface: InterfaceRef) -> str:
    """Delete a named generic interface block from a source file and return previous text."""
    old_text = path.read_text(encoding="utf-8", errors="ignore")
    lines = old_text.splitlines(keepends=True)
    lines = remove_symbol_block_from_lines(lines, iface.start, iface.end, iface.name)
    path.write_text("".join(normalize_blank_lines(lines)), encoding="utf-8", newline="")
    return old_text


def make_interface_candidates(items: List[InterfaceRef]) -> List[RemovalCandidate]:
    """Convert interface refs into batch-removal candidates."""
    return [
        RemovalCandidate(
            path=item.path,
            name=item.name,
            start=item.start,
            end=item.end,
            kind="interface",
            ref=item,
        )
        for item in items
    ]


def make_procedure_candidates(items: List[ProcRef]) -> List[RemovalCandidate]:
    """Convert procedure refs into batch-removal candidates."""
    return [
        RemovalCandidate(
            path=item.path,
            name=item.name,
            start=item.start,
            end=item.end,
            kind=item.kind,
            ref=item,
        )
        for item in items
    ]


def describe_candidate(candidate: RemovalCandidate) -> str:
    """Return a short human-readable description for one candidate."""
    return f"{candidate.path.name} {candidate.kind} {candidate.name} [{candidate.start}-{candidate.end}]"


def apply_candidate_batch(candidates: List[RemovalCandidate]) -> Dict[Path, str]:
    """Apply a candidate batch and return the original file texts for rollback."""
    snapshots: Dict[Path, str] = {}
    by_path: Dict[Path, List[RemovalCandidate]] = {}
    for candidate in candidates:
        by_path.setdefault(candidate.path, []).append(candidate)

    for path, path_candidates in by_path.items():
        old_text = path.read_text(encoding="utf-8", errors="ignore")
        snapshots[path] = old_text
        lines = old_text.splitlines(keepends=True)
        for candidate in sorted(path_candidates, key=lambda x: (-x.start, -x.end, x.name)):
            lines = remove_symbol_block_from_lines(lines, candidate.start, candidate.end, candidate.name)
        path.write_text("".join(normalize_blank_lines(lines)), encoding="utf-8", newline="")
    return snapshots


def restore_candidate_batch(snapshots: Dict[Path, str]) -> None:
    """Restore files after a failed batch prune attempt."""
    for path, old_text in snapshots.items():
        path.write_text(old_text, encoding="utf-8", newline="")


def prune_candidates_with_backtracking(
    candidates: List[RemovalCandidate],
    compiler: str,
    compile_files: List[Path],
    cwd: Path,
    verbose: bool = False,
    compile_runner: Callable[[str, List[Path], str, Path], bool] = run_compile,
) -> List[RemovalCandidate]:
    """Try removing many candidates at once, splitting batches when compile fails."""
    if not candidates:
        return []
    candidates = sorted(candidates, key=lambda x: (x.path.name.lower(), -x.start, -x.end, x.name))

    snapshots = apply_candidate_batch(candidates)
    if compile_runner(compiler, compile_files, "trial", cwd):
        if verbose:
            if len(candidates) == 1:
                print(f"Removed: {describe_candidate(candidates[0])}")
            else:
                names = " ".join(c.name for c in candidates[:8])
                more = "" if len(candidates) <= 8 else " ..."
                print(f"Removed batch ({len(candidates)}): {names}{more}")
        return candidates

    restore_candidate_batch(snapshots)
    if len(candidates) == 1:
        if verbose:
            print(f"Kept: {describe_candidate(candidates[0])}")
        return []

    mid = len(candidates) // 2
    accepted: List[RemovalCandidate] = []
    accepted.extend(
        prune_candidates_with_backtracking(
            candidates[:mid],
            compiler=compiler,
            compile_files=compile_files,
            cwd=cwd,
            verbose=verbose,
            compile_runner=compile_runner,
        )
    )
    accepted.extend(
        prune_candidates_with_backtracking(
            candidates[mid:],
            compiler=compiler,
            compile_files=compile_files,
            cwd=cwd,
            verbose=verbose,
            compile_runner=compile_runner,
        )
    )
    return accepted


def normalize_blank_lines(lines: List[str]) -> List[str]:
    """Collapse blank-line runs to at most one blank line."""
    out: List[str] = []
    blank_run = 0
    for ln in lines:
        if ln.strip() == "":
            blank_run += 1
            if blank_run > 1:
                continue
        else:
            blank_run = 0
        out.append(ln)
    return out


def print_progress(iteration: int, removed_this_iter: int, removed_total: int) -> None:
    """Print one progress line for the current prune iteration."""
    print(
        f"Progress: iteration {iteration} removed {removed_this_iter} "
        f"entit{'y' if removed_this_iter == 1 else 'ies'} this iteration; "
        f"{removed_total} cumulative."
    )


def main() -> int:
    """Run pruning to remove likely unused procedures."""
    parser = argparse.ArgumentParser(description="Prune likely unused Fortran procedures")
    parser.add_argument("fortran_files", type=Path, nargs="*", help="Source files (default: *.f90/*.F90)")
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Glob pattern to exclude files (can be repeated)",
    )
    parser.add_argument(
        "--compiler",
        type=str,
        default="gfortran -c -Wfatal-errors {files}",
        help='Compilation command (default: "gfortran -c -Wfatal-errors {files}")',
    )
    parser.add_argument("--out-dir", type=Path, default=Path("pruned"), help="Output directory (default: pruned)")
    parser.add_argument(
        "--out",
        type=Path,
        default=None,
        help="Output file for a single input source (implies non-in-place output)",
    )
    parser.add_argument("--in-place", action="store_true", help="Modify sources in place (default: off)")
    parser.add_argument("--fix", action="store_true", help="Alias for --in-place")
    parser.add_argument(
        "--no-compile",
        action="store_true",
        help="Use static call-graph pruning only (skip compiler validation).",
    )
    parser.add_argument("--max-iter", type=int, default=1000, help="Maximum prune passes (default: 1000)")
    parser.add_argument("--progress", action="store_true", help="Print per-iteration removal progress")
    parser.add_argument("--verbose", action="store_true", help="Print accepted/rejected removals")
    args = parser.parse_args()
    if args.fix:
        args.in_place = True

    if args.max_iter < 1:
        print("--max-iter must be >= 1.")
        return 2

    src_files = choose_files(args.fortran_files, args.exclude)
    if not src_files:
        print("No source files remain after applying --exclude filters.")
        return 2

    if args.out is not None:
        if args.in_place:
            print("--out cannot be combined with --in-place/--fix.")
            return 2
        if len(src_files) != 1:
            print("--out requires exactly one input source file.")
            return 2
        if args.out.exists() and args.out.is_dir():
            print("--out exists but is a directory.")
            return 2
        args.out.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src_files[0], args.out)
        work_files = [args.out]
        work_dir = args.out.parent.resolve()
        print(f"Wrote working source to: {args.out}")
    elif args.in_place:
        work_files = src_files
        work_dir = Path(".").resolve()
    else:
        work_files = copy_to_out_dir(src_files, args.out_dir)
        work_dir = args.out_dir.resolve()
        print(f"Wrote working sources to: {work_dir}")

    infos, any_missing = fscan.load_source_files(work_files)
    if not infos:
        return 2 if any_missing else 1
    ordered_infos, _ = fscan.order_files_least_dependent(infos)
    compile_files, _ = fscan.build_compile_closure(ordered_infos)
    compile_files = [p for p in compile_files if p.parent.resolve() == work_dir]
    if not compile_files:
        compile_files = [f.path for f in ordered_infos]

    if not args.no_compile:
        if not run_compile(args.compiler, compile_files, phase="baseline", cwd=work_dir):
            return 1

    removed: List[ProcRef] = []
    removed_interfaces: List[InterfaceRef] = []
    for it in range(1, args.max_iter + 1):
        infos, any_missing = fscan.load_source_files(work_files)
        if not infos:
            return 2 if any_missing else 1
        whole_program = has_program_unit(infos)
        procedures = collect_procedures(infos)
        interfaces = collect_interfaces(infos)
        if not procedures and not interfaces:
            break
        known_names = {p.name for p in procedures} | {iface.name for iface in interfaces}
        used_names = collect_used_names(infos, known_names, whole_program=whole_program)
        interface_candidates = [iface for iface in interfaces if iface.name not in used_names]
        procedure_candidates = [p for p in procedures if p.name not in used_names]
        if not interface_candidates and not procedure_candidates:
            break

        changed_this_iter = 0
        interface_batch = make_interface_candidates(
            sorted(interface_candidates, key=lambda x: (x.path.name.lower(), -x.start))
        )
        if args.no_compile:
            if interface_batch:
                apply_candidate_batch(interface_batch)
                removed_interfaces.extend(iface.ref for iface in interface_batch if isinstance(iface.ref, InterfaceRef))
                changed_this_iter += len(interface_batch)
                if args.verbose:
                    print(f"Removed batch ({len(interface_batch)}): {' '.join(i.name for i in interface_batch[:8])}"
                          f"{'' if len(interface_batch) <= 8 else ' ...'}")
        else:
            accepted_interfaces = prune_candidates_with_backtracking(
                interface_batch,
                compiler=args.compiler,
                compile_files=compile_files,
                cwd=work_dir,
                verbose=args.verbose,
            )
            removed_interfaces.extend(
                iface.ref for iface in accepted_interfaces if isinstance(iface.ref, InterfaceRef)
            )
            changed_this_iter += len(accepted_interfaces)

        if changed_this_iter > 0:
            if args.progress:
                print_progress(it, changed_this_iter, len(removed) + len(removed_interfaces))
            continue

        procedure_batch = make_procedure_candidates(
            sorted(procedure_candidates, key=lambda x: (x.path.name.lower(), -x.start))
        )
        if args.no_compile:
            if procedure_batch:
                apply_candidate_batch(procedure_batch)
                removed.extend(proc.ref for proc in procedure_batch if isinstance(proc.ref, ProcRef))
                changed_this_iter += len(procedure_batch)
                if args.verbose:
                    print(f"Removed batch ({len(procedure_batch)}): {' '.join(p.name for p in procedure_batch[:8])}"
                          f"{'' if len(procedure_batch) <= 8 else ' ...'}")
        else:
            accepted_procedures = prune_candidates_with_backtracking(
                procedure_batch,
                compiler=args.compiler,
                compile_files=compile_files,
                cwd=work_dir,
                verbose=args.verbose,
            )
            removed.extend(proc.ref for proc in accepted_procedures if isinstance(proc.ref, ProcRef))
            changed_this_iter += len(accepted_procedures)

        if args.progress:
            print_progress(it, changed_this_iter, len(removed) + len(removed_interfaces))
        if changed_this_iter == 0:
            break

    if not args.no_compile:
        if not run_compile(args.compiler, compile_files, phase="final", cwd=work_dir):
            print("Final compile failed; no further changes applied.")
            return 1

    # Final formatting normalization (also useful when no procedures were removed).
    for p in work_files:
        txt = p.read_text(encoding="utf-8", errors="ignore")
        lines = txt.splitlines(keepends=True)
        norm = normalize_blank_lines(lines)
        if norm != lines:
            p.write_text("".join(norm), encoding="utf-8", newline="")

    print(f"\nRemoved {len(removed)} procedure(s).")
    if removed:
        by_file: Dict[str, List[str]] = {}
        for p in removed:
            by_file.setdefault(p.path.name, []).append(p.name)
        for fname in sorted(by_file.keys(), key=str.lower):
            names = by_file[fname]
            print(f"{fname} {len(names)}: {' '.join(names)}")
    print(f"Removed {len(removed_interfaces)} interface block(s).")
    if removed_interfaces:
        by_file_if: Dict[str, List[str]] = {}
        for iface in removed_interfaces:
            by_file_if.setdefault(iface.path.name, []).append(iface.name)
        for fname in sorted(by_file_if.keys(), key=str.lower):
            names = by_file_if[fname]
            print(f"{fname} {len(names)} interface(s): {' '.join(names)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
