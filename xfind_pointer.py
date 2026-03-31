#!/usr/bin/env python3
"""Find likely modern Fortran pointer usage in source files."""

from __future__ import annotations

import argparse
import glob
import os
import re
import sys
from dataclasses import dataclass
from typing import Iterable, List, Optional, Sequence, Tuple

free_form_exts = {
    ".f90",
    ".f95",
    ".f03",
    ".f08",
    ".f18",
    ".fpp",
    ".fypp",
}

fixed_form_exts = {".f", ".for", ".ftn", ".f77"}

decl_starters = {
    "integer",
    "real",
    "double",
    "complex",
    "character",
    "logical",
    "type",
    "class",
    "procedure",
}

skip_arrow_starters = (
    "use ",
    "use,",
    "public ",
    "public,",
    "private ",
    "private,",
    "procedure ",
    "procedure,",
    "generic ",
    "generic,",
    "associate ",
    "associate(",
    "select type ",
    "select type(",
    "select rank ",
    "select rank(",
    "import ",
    "import,",
)

word_re = re.compile(r"[a-z_][a-z0-9_]*", re.IGNORECASE)


@dataclass
class Hit:
    path: str
    line_no: int
    kind: str
    statement: str



def guess_free_form(path: str) -> bool:
    ext = os.path.splitext(path)[1].lower()
    if ext in free_form_exts:
        return True
    if ext in fixed_form_exts:
        return False
    return True



def split_code_comment_free(line: str) -> Tuple[str, str]:
    code = []
    i = 0
    in_single = False
    in_double = False
    while i < len(line):
        ch = line[i]
        if in_single:
            code.append(ch)
            if ch == "'":
                if i + 1 < len(line) and line[i + 1] == "'":
                    code.append(line[i + 1])
                    i += 1
                else:
                    in_single = False
        elif in_double:
            code.append(ch)
            if ch == '"':
                if i + 1 < len(line) and line[i + 1] == '"':
                    code.append(line[i + 1])
                    i += 1
                else:
                    in_double = False
        else:
            if ch == "!":
                return "".join(code), line[i:]
            code.append(ch)
            if ch == "'":
                in_single = True
            elif ch == '"':
                in_double = True
        i += 1
    return "".join(code), ""



def strip_strings(s: str) -> str:
    out = []
    i = 0
    in_single = False
    in_double = False
    while i < len(s):
        ch = s[i]
        if in_single:
            if ch == "'":
                if i + 1 < len(s) and s[i + 1] == "'":
                    i += 1
                else:
                    in_single = False
            i += 1
            continue
        if in_double:
            if ch == '"':
                if i + 1 < len(s) and s[i + 1] == '"':
                    i += 1
                else:
                    in_double = False
            i += 1
            continue
        if ch == "'":
            in_single = True
            out.append(" ")
        elif ch == '"':
            in_double = True
            out.append(" ")
        else:
            out.append(ch)
        i += 1
    return "".join(out)



def normalize_space(s: str) -> str:
    return " ".join(s.split())



def logical_lines_free(lines: Sequence[str]) -> Iterable[Tuple[int, str]]:
    buf = ""
    start_line = 1
    for lineno, raw in enumerate(lines, start=1):
        code, _comment = split_code_comment_free(raw.rstrip("\n"))
        if not buf:
            start_line = lineno
            work = code.lstrip()
        else:
            work = code.lstrip()
            if work.startswith("&"):
                work = work[1:]
        cont = work.rstrip().endswith("&")
        if cont:
            work = work.rstrip()[:-1]
        if buf:
            buf += " " + work.strip()
        else:
            buf = work.strip()
        if not cont:
            if buf:
                yield start_line, buf
            buf = ""
    if buf:
        yield start_line, buf



def is_fixed_comment(raw: str) -> bool:
    if not raw:
        return False
    first = raw[0]
    return first in "cC*!"



def logical_lines_fixed(lines: Sequence[str]) -> Iterable[Tuple[int, str]]:
    buf = ""
    start_line = 1
    for lineno, raw in enumerate(lines, start=1):
        line = raw.rstrip("\n")
        if not line.strip():
            continue
        if is_fixed_comment(line):
            continue
        padded = line + " " * max(0, 6 - len(line))
        cont = len(padded) >= 6 and padded[5] not in {" ", "0"}
        text = padded[6:72] if len(padded) > 6 else ""
        code, _comment = split_code_comment_free(text)
        work = code.strip()
        if not cont:
            if buf:
                yield start_line, buf
            buf = work
            start_line = lineno
        else:
            if not buf:
                buf = work
                start_line = lineno
            else:
                buf += " " + work
    if buf:
        yield start_line, buf



def iter_logical_lines(path: str) -> Iterable[Tuple[int, str]]:
    with open(path, "r", encoding="utf-8", errors="replace") as f:
        lines = f.readlines()
    if guess_free_form(path):
        yield from logical_lines_free(lines)
    else:
        yield from logical_lines_fixed(lines)



def first_word(s: str) -> str:
    m = word_re.search(s)
    return m.group(0).lower() if m else ""



def lhs_has_pointer_attr(lhs: str) -> bool:
    tokens = [t.lower() for t in word_re.findall(lhs)]
    if not tokens:
        return False
    if tokens[0] not in decl_starters:
        return False
    if "pointer" not in tokens:
        return False
    return True



def is_cray_pointer_stmt(code: str) -> bool:
    s = code.lstrip().lower()
    return bool(re.match(r"pointer\s*\(", s))



def is_nullify_stmt(code: str) -> bool:
    return bool(re.match(r"\s*nullify\s*\(", code, flags=re.IGNORECASE))



def has_associated_call(code: str) -> bool:
    return bool(re.search(r"\bassociated\s*\(", code, flags=re.IGNORECASE))



def has_c_f_pointer(code: str) -> bool:
    return bool(re.search(r"\bc_f_pointer\s*\(", code, flags=re.IGNORECASE))



def is_pointer_decl(code: str) -> bool:
    if "::" not in code:
        return False
    lhs = code.split("::", 1)[0]
    return lhs_has_pointer_attr(lhs)



def is_pointer_assignment(code: str) -> bool:
    if "=>" not in code:
        return False
    s = normalize_space(code).lower()
    for prefix in skip_arrow_starters:
        if s.startswith(prefix):
            return False
    if s.startswith("type(") and "::" not in s:
        return False
    if s.startswith("class(") and "::" not in s:
        return False
    if re.match(r"\s*where\b", s):
        return False
    return True



def find_hits(path: str) -> List[Hit]:
    hits: List[Hit] = []
    for line_no, stmt in iter_logical_lines(path):
        code = strip_strings(stmt)
        if not code.strip():
            continue
        if is_cray_pointer_stmt(code):
            hits.append(Hit(path, line_no, "cray pointer statement", normalize_space(stmt)))
            continue
        if is_pointer_decl(code):
            hits.append(Hit(path, line_no, "pointer declaration", normalize_space(stmt)))
            continue
        if is_nullify_stmt(code):
            hits.append(Hit(path, line_no, "nullify", normalize_space(stmt)))
            continue
        if has_associated_call(code):
            hits.append(Hit(path, line_no, "associated()", normalize_space(stmt)))
            continue
        if has_c_f_pointer(code):
            hits.append(Hit(path, line_no, "c_f_pointer", normalize_space(stmt)))
            continue
        if is_pointer_assignment(code):
            hits.append(Hit(path, line_no, "pointer association", normalize_space(stmt)))
            continue
    return hits



def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="report likely Fortran pointer usage without matching comments or names containing 'pointer'"
    )
    parser.add_argument("files", nargs="+", help="Fortran source files")
    parser.add_argument(
        "-q",
        "--quiet",
        action="store_true",
        help="print nothing; use exit status only (0=any match, 1=no matches, 2=error)",
    )
    parser.add_argument(
        "-l",
        "--files-with-matches",
        action="store_true",
        help="print only file names that contain pointer usage",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="print all hits per file instead of stopping after the first hit",
    )
    return parser.parse_args(argv)



def expand_paths(paths: Sequence[str]) -> List[str]:
    out: List[str] = []
    seen = set()
    for path in paths:
        matches = glob.glob(path)
        if matches:
            for match in matches:
                norm = os.path.normpath(match)
                key = os.path.normcase(norm)
                if key not in seen:
                    seen.add(key)
                    out.append(norm)
        else:
            norm = os.path.normpath(path)
            key = os.path.normcase(norm)
            if key not in seen:
                seen.add(key)
                out.append(norm)
    return out


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = parse_args(argv)
    had_error = False
    any_match = False
    paths = expand_paths(args.files)

    for path in paths:
        try:
            hits = find_hits(path)
        except OSError as exc:
            had_error = True
            if not args.quiet:
                print(f"{path}: error: {exc}", file=sys.stderr)
            continue

        if not hits:
            continue

        any_match = True
        if args.quiet:
            continue
        if args.files_with_matches:
            print(path)
            continue

        show_hits = hits if args.all else [hits[0]]
        for hit in show_hits:
            print(f"{hit.path}:{hit.line_no}: {hit.kind}: {hit.statement}")

    if had_error:
        return 2
    return 0 if any_match else 1


if __name__ == "__main__":
    raise SystemExit(main())
