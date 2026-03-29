from __future__ import annotations

import argparse
import glob
import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

import fortran_build as fbuild
import fortran_scan as fscan


@dataclass
class DeclLine:
    line_no: int
    indent: str
    prefix: str
    entities: list[str]
    names: list[str]
    comment: str


@dataclass
class BlockSpec:
    name: str
    items: list[str]
    unit_name: str


def _expand_inputs(patterns: list[str]) -> list[Path]:
    out: list[Path] = []
    for item in patterns:
        matches = [Path(p) for p in glob.glob(item)]
        if matches:
            out.extend(matches)
        else:
            out.append(Path(item))
    return out


def _split_code_comment(line: str) -> tuple[str, str]:
    in_single = False
    in_double = False
    i = 0
    while i < len(line):
        ch = line[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(line) and line[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(line) and line[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if ch == "!" and not in_single and not in_double:
            return line[:i], line[i:]
        i += 1
    return line, ""


def _split_top_level_commas(text: str) -> list[str]:
    out: list[str] = []
    cur: list[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                cur.append("''")
                i += 2
                continue
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                cur.append('""')
                i += 2
                continue
            in_double = not in_double
            cur.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                item = "".join(cur).strip()
                if item:
                    out.append(item)
                cur = []
                i += 1
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def _strip_top_level_init(entity: str) -> str:
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(entity):
        ch = entity[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(entity) and entity[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(entity) and entity[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "=" and depth == 0:
                if i + 1 < len(entity) and entity[i + 1] == ">":
                    return entity
                return entity[:i].rstrip()
        i += 1
    return entity.rstrip()


def _entity_name(entity: str) -> str | None:
    m = re.match(r"^\s*([a-z][a-z0-9_]*)\b", entity, re.IGNORECASE)
    return m.group(1).lower() if m else None


def _entity_has_declarator(entity: str) -> bool:
    stripped = _strip_top_level_init(entity).strip()
    name = _entity_name(stripped)
    if not name:
        return False
    rest = stripped[len(name) :].lstrip()
    return rest.startswith("(")


def _entity_with_common_shape(entity: str, common_item: str) -> str:
    base = _strip_top_level_init(entity).strip()
    if _entity_has_declarator(base):
        return base
    m = re.match(r"^\s*([a-z][a-z0-9_]*)(\s*\(.*\))\s*$", common_item.strip(), re.IGNORECASE)
    if not m:
        return base
    name = _entity_name(base)
    if not name or name != m.group(1).lower():
        return base
    return f"{base}{m.group(2)}"


def _parse_decl_line(raw: str, line_no: int) -> DeclLine | None:
    code, comment = _split_code_comment(raw.rstrip("\r\n"))
    if not code.strip():
        return None
    m = re.match(
        r"^(\s*)(?P<prefix>(?:double\s+precision|integer(?:\s*\([^)]*\))?|real(?:\s*\([^)]*\))?|logical(?:\s*\([^)]*\))?|complex(?:\s*\([^)]*\))?|character(?:\s*\([^)]*\)|\*\s*[^,\s]+)?|type\s*\([^)]*\)|class\s*\([^)]*\))(?:\s*,\s*[^:]+?)*)\s*::\s*(?P<rhs>.+)$",
        code,
        re.IGNORECASE,
    )
    if not m:
        m = re.match(
            r"^(\s*)(?P<prefix>(?:double\s+precision|integer(?:\s*\([^)]*\))?|real(?:\s*\([^)]*\))?|logical(?:\s*\([^)]*\))?|complex(?:\s*\([^)]*\))?|character(?:\s*\([^)]*\)|\*\s*[^,\s]+)?|type\s*\([^)]*\)|class\s*\([^)]*\)))\s+(?P<rhs>.+)$",
            code,
            re.IGNORECASE,
        )
    if not m:
        return None
    rhs = m.group("rhs").strip()
    entities = _split_top_level_commas(rhs)
    if not entities:
        return None
    names: list[str] = []
    for entity in entities:
        name = _entity_name(_strip_top_level_init(entity))
        if not name:
            return None
        names.append(name)
    return DeclLine(
        line_no=line_no,
        indent=m.group(1),
        prefix=m.group("prefix").strip(),
        entities=entities,
        names=names,
        comment=comment,
    )


def _parse_common_stmt(stmt: str, unit_name: str) -> list[BlockSpec]:
    low = fscan.strip_comment(stmt).strip()
    m = re.match(r"^\s*common\b(.*)$", low, re.IGNORECASE)
    if not m:
        return []
    text = m.group(1).strip()
    if not text:
        raise ValueError(f"{unit_name}: empty COMMON statement")
    out: list[BlockSpec] = []
    i = 0
    while i < len(text):
        while i < len(text) and text[i].isspace():
            i += 1
        if i >= len(text):
            break
        block_name = ""
        if text[i] == "/":
            j = text.find("/", i + 1)
            if j < 0:
                raise ValueError(f"{unit_name}: malformed COMMON block name")
            block_name = text[i + 1 : j].strip().lower()
            i = j + 1
        seg: list[str] = []
        depth = 0
        in_single = False
        in_double = False
        while i < len(text):
            ch = text[i]
            if ch == "'" and not in_double:
                if in_single and i + 1 < len(text) and text[i + 1] == "'":
                    seg.append("''")
                    i += 2
                    continue
                in_single = not in_single
                seg.append(ch)
                i += 1
                continue
            if ch == '"' and not in_single:
                if in_double and i + 1 < len(text) and text[i + 1] == '"':
                    seg.append('""')
                    i += 2
                    continue
                in_double = not in_double
                seg.append(ch)
                i += 1
                continue
            if not in_single and not in_double:
                if ch == "(":
                    depth += 1
                elif ch == ")" and depth > 0:
                    depth -= 1
                elif ch == "/" and depth == 0:
                    break
            seg.append(ch)
            i += 1
        items = _split_top_level_commas("".join(seg))
        if not items:
            raise ValueError(f"{unit_name}: COMMON block has no items")
        out.append(BlockSpec(name=block_name, items=items, unit_name=unit_name))
    return out


def _module_name(block_name: str) -> str:
    return "common_blank_mod" if not block_name else f"common_{block_name}_mod"


def _collect_module_decl(
    unit_name: str,
    common_item: str,
    decls_by_name: dict[str, DeclLine],
) -> tuple[str, int]:
    name = _entity_name(common_item)
    if not name:
        raise ValueError(f"{unit_name}: cannot parse COMMON item '{common_item}'")
    decl = decls_by_name.get(name)
    if decl is None:
        raise ValueError(f"{unit_name}: COMMON variable '{name}' needs an explicit declaration")
    idx = decl.names.index(name)
    entity = _entity_with_common_shape(decl.entities[idx], common_item)
    return f"{decl.prefix} :: {entity}", decl.line_no


def _render_decl_line(decl: DeclLine, keep_names: set[str]) -> str | None:
    kept: list[str] = []
    for entity, name in zip(decl.entities, decl.names):
        if name in keep_names:
            kept.append(entity.strip())
    if not kept:
        return None
    line = f"{decl.indent}{decl.prefix} :: {', '.join(kept)}"
    if decl.comment:
        line += decl.comment
    return line


def _rewrite_common_text(text: str) -> str:
    if re.search(r"^\s*block\s+data\b", text, re.IGNORECASE | re.MULTILINE):
        raise ValueError("BLOCK DATA is not supported yet")
    if re.search(r"^\s*equivalence\b", text, re.IGNORECASE | re.MULTILINE):
        raise ValueError("EQUIVALENCE with COMMON is not supported yet")

    lines = text.splitlines()
    units = fscan.split_fortran_units_simple(text)
    if not units:
        return text

    module_blocks: dict[str, list[str]] = {}
    module_block_names: dict[str, list[str]] = {}
    line_rewrites: dict[int, str | None] = {}
    insert_before: dict[int, list[str]] = {}

    for unit in units:
        unit_name = str(unit["name"])
        body_line_nos = [int(x) for x in unit.get("body_line_nos", [])]
        decls_by_name: dict[str, DeclLine] = {}
        decls_by_line: dict[int, DeclLine] = {}
        used_blocks: list[str] = []
        common_names_by_decl_line: dict[int, set[str]] = {}

        for line_no in body_line_nos:
            raw = lines[line_no - 1]
            decl = _parse_decl_line(raw, line_no)
            if decl:
                decls_by_line[line_no] = decl
                for name in decl.names:
                    decls_by_name[name] = decl

        for line_no in body_line_nos:
            raw = lines[line_no - 1]
            stmt = fscan.strip_comment(raw).strip()
            if not re.match(r"^\s*common\b", stmt, re.IGNORECASE):
                continue
            line_rewrites[line_no] = None
            for block in _parse_common_stmt(stmt, unit_name):
                if block.name not in used_blocks:
                    used_blocks.append(block.name)
                names = [_entity_name(item) for item in block.items]
                if any(name is None for name in names):
                    raise ValueError(f"{unit_name}: malformed COMMON item in block '{block.name or 'blank'}'")
                name_list = [name for name in names if name is not None]
                decl_lines: list[str] = []
                for item in block.items:
                    decl_line, decl_line_no = _collect_module_decl(unit_name, item, decls_by_name)
                    decl_lines.append(decl_line)
                    common_names_by_decl_line.setdefault(decl_line_no, set()).add(_entity_name(item) or "")
                prior_lines = module_blocks.get(block.name)
                prior_names = module_block_names.get(block.name)
                if prior_lines is None:
                    module_blocks[block.name] = decl_lines
                    module_block_names[block.name] = name_list
                else:
                    if prior_names != name_list or [ln.lower() for ln in prior_lines] != [ln.lower() for ln in decl_lines]:
                        label = block.name or "blank"
                        raise ValueError(
                            f"{unit_name}: COMMON block '{label}' has a declaration/layout conflict across units"
                        )

        for decl_line_no, names_to_remove in common_names_by_decl_line.items():
            decl = decls_by_line[decl_line_no]
            keep_names = {name for name in decl.names if name not in names_to_remove}
            line_rewrites[decl_line_no] = _render_decl_line(decl, keep_names)

        if used_blocks:
            header_line_no = int(unit["header_line_no"])
            use_lines = [f"   use {_module_name(name)}, only: {', '.join(module_block_names[name])}" for name in used_blocks]
            pos = header_line_no + 1
            while pos <= len(lines):
                code = fscan.strip_comment(lines[pos - 1]).strip().lower()
                if not code:
                    pos += 1
                    continue
                if code.startswith("use "):
                    pos += 1
                    continue
                break
            insert_before.setdefault(pos, []).extend(use_lines)

    if not module_blocks:
        return text

    out: list[str] = []
    for block_name, decl_lines in module_blocks.items():
        mod_name = _module_name(block_name)
        out.append(f"module {mod_name}")
        out.append("   implicit none")
        for decl_line in decl_lines:
            out.append(f"   {decl_line.strip()}")
        out.append(f"end module {mod_name}")
        out.append("")

    for idx in range(1, len(lines) + 1):
        for use_line in insert_before.get(idx, []):
            out.append(use_line)
        rewritten = line_rewrites.get(idx, lines[idx - 1])
        if rewritten is None:
            continue
        out.append(rewritten)

    return "\n".join(out).rstrip() + "\n"


def _rewrite_file(src: Path, dst: Path) -> None:
    text = fscan.read_text_flexible(src)
    rewritten = _rewrite_common_text(text)
    dst.write_text(rewritten, encoding="utf-8", newline="\n")


def _build_and_run(
    source: Path,
    *,
    label: str,
    show_output: bool,
    keep_exe: bool = False,
) -> tuple[bool, str, str]:
    exe = source.with_suffix("")
    if exe.suffix.lower() == ".orig":
        exe = exe.with_suffix("")
    exe = exe.with_name(exe.name + ".exe")
    compile_cmd = ["gfortran", str(source), "-o", str(exe)]
    print(f"Build ({label}): {' '.join(fbuild.quote_cmd_arg(x) for x in compile_cmd)}")
    cp = subprocess.run(compile_cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(fbuild.format_linker_stderr(cp.stderr).rstrip())
        return False, cp.stdout or "", cp.stderr or ""
    print(f"Build ({label}): PASS")
    rp = subprocess.run([str(exe)], capture_output=True, text=True)
    try:
        if rp.returncode != 0:
            print(f"Run ({label}): FAIL (exit {rp.returncode})")
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
            return False, rp.stdout or "", rp.stderr or ""
        print(f"Run ({label}): PASS")
        if show_output:
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
        return True, rp.stdout or "", rp.stderr or ""
    finally:
        if not keep_exe:
            try:
                exe.unlink(missing_ok=True)
            except Exception:
                pass


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Rewrite a conservative COMMON-block subset into module variables. "
            "By default, each input file foo.f90 is written as a sibling file "
            "foo_nocommon.f90."
        ),
    )
    parser.add_argument("inputs", nargs="+", help="Input Fortran files or glob patterns.")
    parser.add_argument("--out", help="Output file path for a single input.")
    parser.add_argument("--stdout", action="store_true", help="Write rewritten source to stdout for a single input.")
    parser.add_argument("--in-place", action="store_true", help="Rewrite files in place.")
    parser.add_argument("--tee", action="store_true", help="Print transformed source, or transformed run output with --run-both.")
    parser.add_argument("--tee-both", action="store_true", help="Print original and transformed source, or both run outputs with --run-both.")
    parser.add_argument("--run-both", action="store_true", help="Build/run original and rewritten Fortran sources.")
    parser.add_argument("--run-diff", action="store_true", help="With --run-both, compare stdout/stderr of original and rewritten runs.")
    parser.add_argument("--keep-exe", action="store_true", help="Keep generated executables when using --run-both/--run-diff.")
    parser.add_argument(
        "--suffix",
        default="_nocommon",
        help="Suffix added before the extension when writing sibling output files. Default: %(default)s",
    )
    args = parser.parse_args(argv)
    if args.run_diff:
        args.run_both = True
    if args.tee_both:
        args.tee = True

    inputs = _expand_inputs(args.inputs)
    if not inputs:
        print("No input files found.", file=sys.stderr)
        return 1

    if args.stdout and len(inputs) != 1:
        print("--stdout requires exactly one input file.", file=sys.stderr)
        return 2
    if args.out and len(inputs) != 1:
        print("--out requires exactly one input file.", file=sys.stderr)
        return 2
    if args.run_both and len(inputs) != 1:
        print("--run-both/--run-diff require exactly one input file.", file=sys.stderr)
        return 2
    if args.out and args.in_place:
        print("--out and --in-place cannot be used together.", file=sys.stderr)
        return 2
    if args.out and args.stdout:
        print("--out and --stdout cannot be used together.", file=sys.stderr)
        return 2
    if args.stdout and args.run_both:
        print("--stdout cannot be used with --run-both/--run-diff.", file=sys.stderr)
        return 2

    try:
        if args.stdout:
            src = inputs[0]
            sys.stdout.write(_rewrite_common_text(fscan.read_text_flexible(src)))
            return 0

        for src in inputs:
            if not src.exists():
                print(f"Missing input: {src}", file=sys.stderr)
                return 1
            if args.in_place:
                dst = src
            elif args.out:
                dst = Path(args.out)
            else:
                dst = src.with_name(f"{src.stem}{args.suffix}{src.suffix}")
            orig_text = fscan.read_text_flexible(src)
            rewritten = _rewrite_common_text(orig_text)
            dst.write_text(rewritten, encoding="utf-8", newline="\n")
            print(f"Wrote {dst}")
            if not args.run_both:
                if args.tee_both:
                    print(f"--- original: {src.name} ---")
                    print(orig_text, end="" if orig_text.endswith("\n") else "\n")
                if args.tee:
                    print(f"--- transformed: {dst.name} ---")
                    print(rewritten, end="" if rewritten.endswith("\n") else "\n")
            else:
                show_orig = bool(args.tee_both)
                show_new = bool(args.tee)
                orig_build_path = src
                if src.resolve() == dst.resolve():
                    orig_build_path = src.with_name(f"{src.stem}.orig{src.suffix}")
                    orig_build_path.write_text(orig_text, encoding="utf-8", newline="\n")
                ok_orig, orig_out, orig_err = _build_and_run(
                    orig_build_path,
                    label="original",
                    show_output=show_orig,
                    keep_exe=args.keep_exe,
                )
                try:
                    if not ok_orig:
                        return 1
                    ok_new, new_out, new_err = _build_and_run(
                        dst,
                        label="rewritten",
                        show_output=show_new,
                        keep_exe=args.keep_exe,
                    )
                    if not ok_new:
                        return 1
                finally:
                    if orig_build_path != src:
                        orig_build_path.unlink(missing_ok=True)
                if args.run_diff:
                    if orig_out == new_out and orig_err == new_err:
                        print("Run diff: MATCH")
                    else:
                        print("Run diff: DIFFER")
                        if not show_orig:
                            print("--- original output ---")
                            if orig_out:
                                print(orig_out.rstrip())
                            if orig_err:
                                print(orig_err.rstrip())
                        if not show_new:
                            print("--- rewritten output ---")
                            if new_out:
                                print(new_out.rstrip())
                            if new_err:
                                print(new_err.rstrip())
                        return 1
        return 0
    except ValueError as exc:
        print(str(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
