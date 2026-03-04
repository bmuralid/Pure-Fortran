#!/usr/bin/env python3
"""Batch runner for xp2f.py over explicit files and glob patterns.

Each selected Python source is transpiled and run via xp2f.py.
"""

from __future__ import annotations

import argparse
import glob
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List


@dataclass
class CaseResult:
    source: str
    ok: bool
    rc: int
    status: str


def _has_glob_meta(s: str) -> bool:
    return any(ch in s for ch in "*?[]")


def _is_generated_typed_file(p: Path) -> bool:
    stem = p.stem.lower()
    if stem.endswith("_typed"):
        return True
    if "_typed_typed" in stem:
        return True
    return False


def _expand_inputs(items: List[str], *, exclude_generated_typed: bool = False) -> List[Path]:
    out: List[Path] = []
    seen = set()
    for it in items:
        matches: List[str]
        if _has_glob_meta(it):
            matches = glob.glob(it, recursive=True)
        else:
            matches = [it]
        for m in matches:
            p = Path(m)
            if p.is_dir():
                for q in sorted(p.rglob("*.py")):
                    k = str(q.resolve()).lower()
                    if k not in seen:
                        seen.add(k)
                        out.append(q)
                continue
            if p.suffix.lower() != ".py":
                continue
            if p.exists():
                if exclude_generated_typed and _is_generated_typed_file(p):
                    continue
                k = str(p.resolve()).lower()
                if k not in seen:
                    seen.add(k)
                    out.append(p)
    return sorted(out, key=lambda p: str(p).lower())


def main() -> int:
    t0 = time.perf_counter()
    ap = argparse.ArgumentParser(
        description="Run xp2f.py on multiple Python files/globs (transpile + run each)."
    )
    ap.add_argument("inputs", nargs="+", help="Python files, directories, and/or glob patterns.")
    ap.add_argument(
        "--helpers",
        nargs="*",
        default=[],
        help="Zero or more helper .f90 files passed to xp2f.py.",
    )
    ap.add_argument(
        "--compiler",
        default="gfortran -O3 -march=native -flto",
        help='Compiler command forwarded to xp2f.py --compiler.',
    )
    ap.add_argument("--flat", action="store_true", help="Forward --flat to xp2f.py.")
    ap.add_argument("--type", action="store_true", help="Forward --type to xp2f.py.")
    ap.add_argument("--comment", action="store_true", help="Forward --comment to xp2f.py.")
    ap.add_argument("--run-diff", action="store_true", help="Forward --run-diff to xp2f.py.")
    ap.add_argument("--time-both", action="store_true", help="Forward --time-both to xp2f.py.")
    ap.add_argument("--pretty", action="store_true", help="Forward --pretty to xp2f.py.")
    ap.add_argument("--maxfail", type=int, default=0, help="Stop after this many failures (0 = no limit).")
    ap.add_argument("--verbose", action="store_true", help="Print full xp2f output for PASS cases too.")
    args = ap.parse_args()

    py_files = _expand_inputs(args.inputs, exclude_generated_typed=args.type)
    if not py_files:
        print("No Python files matched the provided inputs.")
        return 1

    xp2f_path = Path(__file__).with_name("xp2f.py")
    if not xp2f_path.exists():
        print(f"Missing script: {xp2f_path}")
        return 1

    results: List[CaseResult] = []
    failures = 0
    total = len(py_files)

    for i, pyf in enumerate(py_files, start=1):
        rel = str(pyf)
        cmd = [sys.executable, str(xp2f_path), rel, *args.helpers, "--run", "--compiler", args.compiler]
        if args.flat:
            cmd.append("--flat")
        if args.type:
            cmd.append("--type")
        if args.comment:
            cmd.append("--comment")
        if args.run_diff:
            cmd.append("--run-diff")
        if args.time_both:
            cmd.append("--time-both")
        if args.pretty:
            cmd.append("--pretty")

        print(f"[{i}/{total}] {rel}")
        cp = subprocess.run(cmd, text=True, capture_output=True, encoding="utf-8", errors="ignore")
        ok = cp.returncode == 0

        if ok:
            status = "PASS"
            show_pass_output = args.verbose or args.time_both or args.run_diff
            if show_pass_output and cp.stdout.strip():
                print(cp.stdout.rstrip())
            if show_pass_output and cp.stderr.strip():
                print(cp.stderr.rstrip())
        else:
            status = "FAIL"
            failures += 1
            print(f"  FAIL (exit {cp.returncode})")
            if cp.stdout.strip():
                print(cp.stdout.rstrip())
            if cp.stderr.strip():
                print(cp.stderr.rstrip())
            if args.maxfail > 0 and failures >= args.maxfail:
                results.append(CaseResult(source=rel, ok=ok, rc=cp.returncode, status=status))
                print(f"Stopped at maxfail={args.maxfail}.")
                break

        results.append(CaseResult(source=rel, ok=ok, rc=cp.returncode, status=status))

    print("")
    print("Summary:")
    src_w = max(len("source"), *(len(r.source) for r in results)) if results else len("source")
    st_w = max(len("status"), *(len(r.status) for r in results)) if results else len("status")
    rc_w = max(len("rc"), *(len(str(r.rc)) for r in results)) if results else len("rc")
    header = f"{'source':<{src_w}}  {'status':<{st_w}}  {'rc':>{rc_w}}"
    print(header)
    for r in results:
        print(f"{r.source:<{src_w}}  {r.status:<{st_w}}  {r.rc:>{rc_w}}")
    n_pass = sum(1 for r in results if r.ok)
    n_fail = len(results) - n_pass
    print(f"Totals: {len(results)} files, {n_pass} pass, {n_fail} fail")
    elapsed = time.perf_counter() - t0
    print(f"Elapsed: {elapsed:.3f} s")
    return 0 if n_fail == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
