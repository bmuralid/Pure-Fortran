#!/usr/bin/env python3
from __future__ import annotations

import argparse
import glob
import re
import shlex
import subprocess
import tempfile
from pathlib import Path
from typing import List

import fortran_scan as fscan


def _expand_inputs(items: List[str]) -> List[Path]:
    out: List[Path] = []
    for it in items:
        matches = glob.glob(it)
        if matches:
            out.extend(Path(m) for m in matches)
        else:
            out.append(Path(it))
    seen = set()
    uniq: List[Path] = []
    for p in out:
        key = str(p.resolve()) if p.exists() else str(p)
        if key not in seen:
            seen.add(key)
            uniq.append(p)
    return uniq


def _safe_wrap_lines(lines: List[str], max_len: int) -> List[str]:
    """Wrap only low-risk lines to avoid introducing invalid continuations."""
    decl_like_re = re.compile(
        r"^\s*(integer|real|logical|complex|character|type\s*\([^)]+\))\b.*::",
        re.IGNORECASE,
    )
    use_like_re = re.compile(r"^\s*use\b", re.IGNORECASE)
    out: List[str] = []
    for ln in lines:
        if len(ln) <= max_len:
            out.append(ln)
            continue
        # Risky for generic wrappers: string literals, explicit concatenation,
        # and pre-existing continuations.
        if ("!" in ln) or ("'" in ln) or ('"' in ln) or ("//" in ln) or ("&" in ln):
            out.append(ln)
            continue
        if ("**" in ln) or (";" in ln):
            out.append(ln)
            continue
        # Be conservative: wrap only declaration/use-like statements.
        if decl_like_re.match(ln):
            wrapped = fscan.wrap_long_declaration_lines([ln], max_len=max_len)
            out.extend(wrapped)
            continue
        if use_like_re.match(ln):
            wrapped = fscan.wrap_long_fortran_lines([ln], max_len=max_len)
            out.extend(wrapped)
            continue
        out.append(ln)
    return out


def _max_leading_spaces(lines: List[str]) -> int:
    m = 0
    for ln in lines:
        if not ln:
            continue
        n = len(ln) - len(ln.lstrip(" "))
        if n > m:
            m = n
    return m


def main() -> int:
    ap = argparse.ArgumentParser(
        description=(
            "Indent Fortran code: no extra body indent for program/module/procedures; "
            "3-space indent for block constructs and derived-type components; "
            "wrap overlong lines."
        )
    )
    ap.add_argument("inputs", nargs="+", help="Fortran source files (supports globs)")
    ap.add_argument("--fix", action="store_true", help="Rewrite files in place")
    ap.add_argument("--out", help="Write output to this file (single input only)")
    ap.add_argument("--indent", type=int, default=3, help="Indent size (default: 3)")
    ap.add_argument("--indent-proc", action="store_true", help="Indent function/subroutine bodies")
    ap.add_argument("--indent-module", action="store_true", help="Indent module bodies")
    ap.add_argument("--indent-program", action="store_true", help="Indent main program bodies")
    ap.add_argument("--indent-contains", action="store_true", help="Indent bodies under CONTAINS")
    ap.add_argument("--max-len", type=int, default=80, help="Maximum line length (default: 80)")
    ap.add_argument("--compile", action="store_true", help="Compile transformed output")
    ap.add_argument("--compile-both", action="store_true", help="Compile original input and transformed output")
    ap.add_argument(
        "--compiler",
        default="gfortran -c -Wfatal-errors -w -std=legacy",
        help='compiler command, e.g. "gfortran -c -Wfatal-errors -w -std=legacy"',
    )
    args = ap.parse_args()

    paths = _expand_inputs(args.inputs)
    missing = [p for p in paths if not p.exists()]
    for p in missing:
        print(f"Missing file: {p}")
    if missing:
        return 1

    if args.out and len(paths) != 1:
        print("--out requires exactly one input file.")
        return 2
    if args.max_len < 20:
        print("--max-len should be at least 20.")
        return 2
    if args.compile_both:
        args.compile = True

    changed = 0
    transformed = {}
    for path in paths:
        src = fscan.read_text_flexible(path)

        # Stash preprocessor directive lines (#ifdef, #endif, etc.) so they
        # are neither re-indented nor allowed to confuse the block-level
        # tracker.  Replaced with Fortran comment tokens that the indenter
        # ignores for level tracking (strip_comment returns "").
        preproc_keep: dict = {}
        src_for_indent_lines = src.splitlines(keepends=True)
        for i, ln in enumerate(src_for_indent_lines):
            if re.match(r"^\s*#", ln):
                tok = f"!__XINDENT_PREPROC_{len(preproc_keep)}__"
                preproc_keep[tok] = ln.rstrip("\r\n")
                eol = "\r\n" if ln.endswith("\r\n") else "\n"
                src_for_indent_lines[i] = tok + eol
        src_for_indent = "".join(src_for_indent_lines)

        dst = fscan.indent_fortran_blocks(
            src_for_indent,
            indent_step=args.indent,
            indent_proc=args.indent_proc,
            indent_module=args.indent_module,
            indent_program=args.indent_program,
            indent_contains=args.indent_contains,
        )
        src_lines = src.splitlines()
        lines = dst.splitlines()

        # Guardrail: avoid pathological indentation blow-ups on some legacy
        # sources (e.g., ENDIF/ENDDO forms not fully recognized by indenter).
        src_max_indent = _max_leading_spaces(src_lines)
        dst_max_indent = _max_leading_spaces(lines)
        if dst_max_indent > max(512, src_max_indent + 120):
            print(
                f"Warning: skipping indentation for {path} due to runaway indent "
                f"(max leading spaces {src_max_indent} -> {dst_max_indent})."
            )
            lines = src_lines

        # Keep runtime export metadata lines intact; wrapping can otherwise
        # break "!@pyapi ..." comments into invalid continuation lines.
        pyapi_keep = {}
        for i, ln in enumerate(lines):
            if "!@pyapi" in ln:
                tok = f"!__XINDENT_PYAPI_KEEP_{len(pyapi_keep)}__"
                pyapi_keep[tok] = ln
                lines[i] = tok
        lines = _safe_wrap_lines(lines, max_len=args.max_len)
        if pyapi_keep:
            lines = [pyapi_keep.get(ln, ln) for ln in lines]
        # Restore preprocessor directives verbatim (column-1, no wrapping).
        if preproc_keep:
            lines = [preproc_keep.get(ln.strip(), ln) for ln in lines]
        dst = "\n".join(lines) + ("\n" if src.endswith("\n") and lines else "")
        transformed[path] = dst
        if dst != src:
            changed += 1
        if args.fix:
            path.write_text(dst, encoding="utf-8")
        elif args.out:
            Path(args.out).write_text(dst, encoding="utf-8")
        elif len(paths) == 1 and not args.compile:
            print(dst, end="")

    if args.fix:
        print(f"Applied indentation to {changed} file(s).")
    elif not args.out and len(paths) > 1:
        print(f"Checked {len(paths)} file(s); {changed} would change. Use --fix to apply.")

    if args.compile:
        cc = shlex.split(args.compiler)
        if not cc:
            print("Empty compiler command.")
            return 2

        temp_paths: List[Path] = []
        try:
            out_paths = {}
            if args.fix:
                for p in paths:
                    out_paths[p] = p
            elif args.out:
                out_paths[paths[0]] = Path(args.out)
            else:
                for p in paths:
                    tf = tempfile.NamedTemporaryFile(
                        prefix=f"{p.stem}_ind_",
                        suffix=p.suffix,
                        dir=str(p.parent) if str(p.parent) else None,
                        delete=False,
                    )
                    tpath = Path(tf.name)
                    tf.close()
                    tpath.write_text(transformed[p], encoding="utf-8")
                    temp_paths.append(tpath)
                    out_paths[p] = tpath

            def _run_compile(label: str, src_path: Path, src_hint: Path | None = None) -> int:
                cmd = cc + [str(src_path)]
                if src_hint is not None:
                    print(f"File: {src_hint}")
                print(f"Build ({label}): {' '.join(cmd)}")
                cp = subprocess.run(cmd, text=True, capture_output=True)
                if cp.returncode != 0:
                    print(f"Build ({label}): FAIL (exit {cp.returncode})")
                    if cp.stdout.strip():
                        print(cp.stdout.rstrip())
                    if cp.stderr.strip():
                        print(cp.stderr.rstrip())
                    return cp.returncode
                print(f"Build ({label}): PASS")
                return 0

            if args.compile_both:
                baseline_fail = 0
                for p in paths:
                    rc = _run_compile("original", p, src_hint=p)
                    if rc != 0:
                        baseline_fail += 1
                        continue
                    rc = _run_compile("transformed", out_paths[p], src_hint=p)
                    if rc != 0:
                        print("First original-pass/transformed-fail case encountered; stopping.")
                        return rc
                if baseline_fail > 0:
                    print(f"Skipped {baseline_fail} file(s) where original compile failed.")
            else:
                for p in paths:
                    rc = _run_compile("transformed", out_paths[p], src_hint=p)
                    if rc != 0:
                        return rc
        finally:
            for tp in temp_paths:
                try:
                    tp.unlink()
                except OSError:
                    pass
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
