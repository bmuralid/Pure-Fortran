#!/usr/bin/env python3
"""Remove two conservative no-need data-pointer cases from free-form Fortran."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan
import xunused

DECL_START_RE = re.compile(
    r"^\s*(?:integer(?:\s*\([^)]*\))?|real(?:\s*\([^)]*\))?|logical|character(?:\s*\([^)]*\))?|complex(?:\s*\([^)]*\))?|double\s+precision|type\s*\([^)]*\)|class\s*\([^)]*\))\b",
    re.IGNORECASE,
)
POINTER_ASSIGN_RE_TMPL = r"^\s*{name}\s*=>\s*(.+?)\s*$"
ALLOCATE_RE_TMPL = r"\ballocate\s*\(\s*{name}(?:\b|\s*\()"
DEALLOCATE_RE_TMPL = r"\bdeallocate\s*\(\s*{name}(?:\b|\s*\()"
NULLIFY_RE_TMPL = r"\bnullify\s*\(\s*{name}(?:\b|\s*\()"
ASSOCIATED_RE_TMPL = r"\bassociated\s*\(\s*{name}(?:\b|\s*\()"
CALL_RE = re.compile(r"^\s*call\b", re.IGNORECASE)
LEADING_LABEL_RE = re.compile(r"^\s*\d+\b")
GOTO_RE = re.compile(r"\bgo\s*to\b|\bgoto\b", re.IGNORECASE)
ARITH_IF_RE = re.compile(r"^\s*if\s*\([^)]*\)\s*\d+\s*,\s*\d+\s*,\s*\d+\s*$", re.IGNORECASE)


@dataclass
class PointerDecl:
    name: str
    decl_line: int
    decl_stmt_index: int
    lhs: str
    entity: str
    init_target: Optional[str] = None


@dataclass
class PlannedEdit:
    replace_lines: Dict[int, str] = field(default_factory=dict)
    delete_lines: Set[int] = field(default_factory=set)
    notes: List[str] = field(default_factory=list)

    def overlaps(self, lines: Iterable[int]) -> bool:
        line_set = set(lines)
        if self.delete_lines & line_set:
            return True
        return any(ln in self.replace_lines for ln in line_set)

    def claim_replace(self, lineno: int, new_text: str) -> None:
        self.replace_lines[lineno] = new_text

    def claim_delete(self, lineno: int) -> None:
        self.delete_lines.add(lineno)


@dataclass
class FileResult:
    path: Path
    changed: bool
    notes: List[str]
    out_path: Optional[Path] = None
    backup_path: Optional[Path] = None


@dataclass
class AliasAnalysis:
    assoc_line: int
    assoc_stmt_index: int
    target_expr: str
    target_base: str
    use_lines: List[Tuple[int, str]]
    use_stmt_indices: List[int]


def choose_files(args_files: Sequence[Path]) -> List[Path]:
    """Expand CLI file arguments."""
    return cpaths.expand_source_inputs(args_files, extensions=(".f90", ".F90", ".f", ".F"))


def split_top_level_commas(text: str) -> List[str]:
    """Split text on top-level commas."""
    out: List[str] = []
    cur: List[str] = []
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
                out.append("".join(cur).strip())
                cur = []
                i += 1
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def normalize_attr_list(lhs: str, *, replace_pointer: bool = False, remove_target: bool = False) -> str:
    """Normalize declaration attrs after conservative edits."""
    parts = split_top_level_commas(lhs)
    new_parts: List[str] = []
    saw_allocatable = False
    for part in parts:
        low = part.strip().lower()
        if low == "pointer":
            if replace_pointer:
                new_parts.append("allocatable")
                saw_allocatable = True
            continue
        if low == "allocatable":
            saw_allocatable = True
            new_parts.append(part.strip())
            continue
        if low == "target" and remove_target:
            continue
        new_parts.append(part.strip())
    if replace_pointer and not saw_allocatable:
        new_parts.append("allocatable")
    return ", ".join(p for p in new_parts if p)


def parse_pointer_decl(stmt: str, stmt_index: int, lineno: int) -> Optional[PointerDecl]:
    """Parse one simple data-pointer declaration."""
    code = fscan.strip_comment(stmt).strip()
    if not code or "::" not in code or not DECL_START_RE.match(code):
        return None
    lhs, rhs = code.split("::", 1)
    if "pointer" not in lhs.lower():
        return None
    if lhs.strip().lower().startswith("procedure"):
        return None
    entities = split_top_level_commas(rhs)
    if len(entities) != 1:
        return None
    entity = entities[0].strip()
    m = re.match(r"^([a-z][a-z0-9_]*)", entity, re.IGNORECASE)
    if not m:
        return None
    name = m.group(1).lower()
    rest = entity[m.end() :].strip()
    init_target: Optional[str] = None
    if "=>" in rest:
        before, after = rest.split("=>", 1)
        entity = f"{m.group(1)}{before.rstrip()}"
        init_target = after.strip()
    return PointerDecl(
        name=name,
        decl_line=lineno,
        decl_stmt_index=stmt_index,
        lhs=lhs.strip(),
        entity=entity.strip(),
        init_target=init_target,
    )


def is_simple_raw_line(raw: str) -> bool:
    """Keep only single-line, single-statement code."""
    code = fscan.strip_comment(raw)
    return ";" not in code and "&" not in code


def token_present(code: str, name: str) -> bool:
    """True when code contains name as an identifier token."""
    return re.search(rf"\b{re.escape(name)}\b", code, re.IGNORECASE) is not None


def is_null_initializer(expr: Optional[str]) -> bool:
    """True for => null()."""
    if expr is None:
        return True
    return re.fullmatch(r"null\s*\(\s*\)", expr.strip(), re.IGNORECASE) is not None


def split_code_comment_eol(line: str) -> Tuple[str, str, str]:
    """Split one raw line into code, comment, and eol."""
    eol = xunused.get_eol(line)
    body = line[:-len(eol)] if eol else line
    code, comment = xunused.split_code_comment(body)
    return code, comment, eol


def replace_identifier_outside_strings(code: str, name: str, replacement: str) -> str:
    """Replace identifier tokens outside strings."""
    out: List[str] = []
    i = 0
    in_single = False
    in_double = False
    low_name = name.lower()
    while i < len(code):
        ch = code[i]
        if ch == "'" and not in_double:
            out.append(ch)
            if in_single and i + 1 < len(code) and code[i + 1] == "'":
                out.append("'")
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            out.append(ch)
            if in_double and i + 1 < len(code) and code[i + 1] == '"':
                out.append('"')
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if in_single or in_double:
            out.append(ch)
            i += 1
            continue
        if re.match(r"[a-z_]", ch, re.IGNORECASE):
            j = i + 1
            while j < len(code) and re.match(r"[a-z0-9_]", code[j], re.IGNORECASE):
                j += 1
            tok = code[i:j]
            if tok.lower() == low_name:
                out.append(replacement)
            else:
                out.append(tok)
            i = j
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def simple_target_expr(expr: str) -> bool:
    """Accept whole variables and simple array sections/components."""
    s = expr.strip()
    return re.fullmatch(
        r"[a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)*(?:\s*\([^()]*\))?",
        s,
        re.IGNORECASE,
    ) is not None


def stmt_uses_token_as_call_arg(stmt: str, name: str) -> bool:
    """True if token appears in a CALL statement."""
    if not CALL_RE.match(stmt):
        return False
    return token_present(stmt, name)


def stmt_has_indexed_use(stmt: str, name: str) -> bool:
    """True if token is followed by subscripts."""
    return re.search(rf"\b{re.escape(name)}\s*\(", stmt, re.IGNORECASE) is not None


def stmt_has_component_use(stmt: str, name: str) -> bool:
    """True if token is followed by component selection."""
    return re.search(rf"\b{re.escape(name)}\s*%", stmt, re.IGNORECASE) is not None


def transform_pointer_decl_to_allocatable(raw_line: str) -> str:
    """Rewrite one single-entity pointer declaration to allocatable."""
    code, comment, eol = split_code_comment_eol(raw_line)
    lhs, rhs = code.split("::", 1)
    lhs2 = normalize_attr_list(lhs.strip(), replace_pointer=True)
    entities = split_top_level_commas(rhs)
    entity = entities[0].strip()
    m = re.match(r"^([a-z][a-z0-9_]*)(.*)$", entity, re.IGNORECASE)
    if m is None:
        return raw_line
    tail = m.group(2)
    if "=>" in tail:
        before, after = tail.split("=>", 1)
        if re.fullmatch(r"\s*null\s*\(\s*\)\s*", after, re.IGNORECASE):
            tail = before.rstrip()
    entity2 = f"{m.group(1)}{tail}".rstrip()
    return f"{lhs2} :: {entity2}{comment}{eol}"


def remove_target_attr_from_decl(raw_line: str) -> str:
    """Remove TARGET from one single-entity declaration."""
    code, comment, eol = split_code_comment_eol(raw_line)
    if "::" not in code:
        return raw_line
    lhs, rhs = code.split("::", 1)
    lhs2 = normalize_attr_list(lhs.strip(), remove_target=True)
    return f"{lhs2} :: {rhs.strip()}{comment}{eol}"


def leading_spaces(text: str) -> str:
    """Return leading blanks and tabs."""
    m = re.match(r"^[ \t]*", text)
    return m.group(0) if m is not None else ""


def line_has_control_flow_hazard(raw_line: str) -> bool:
    """Reject obvious cases that are awkward for associate blocks."""
    code, _comment, _eol = split_code_comment_eol(raw_line)
    stripped = code.strip()
    if not stripped:
        return False
    if LEADING_LABEL_RE.match(code):
        return True
    if GOTO_RE.search(stripped):
        return True
    if ARITH_IF_RE.match(stripped):
        return True
    return False


def collect_alias_analysis(
    cand: PointerDecl,
    body_lines: List[str],
    body_line_nos: List[int],
) -> Optional[AliasAnalysis]:
    """Collect conservative data for a single fixed-alias pointer."""
    name = cand.name
    assoc_lines: List[Tuple[int, int, str]] = []
    alloc_lines: List[int] = []
    dealloc_lines: List[int] = []
    use_lines: List[Tuple[int, str]] = []
    use_stmt_indices: List[int] = []
    unsafe = False
    first_use_stmt_index: Optional[int] = None

    for idx, (lineno, stmt) in enumerate(zip(body_line_nos, body_lines)):
        low = stmt.lower().strip()
        if not low:
            continue
        if lineno == cand.decl_line:
            continue
        if not token_present(stmt, name):
            continue
        if first_use_stmt_index is None:
            first_use_stmt_index = idx
        m_assoc = re.match(POINTER_ASSIGN_RE_TMPL.format(name=re.escape(name)), stmt, re.IGNORECASE)
        if m_assoc is not None:
            assoc_lines.append((lineno, idx, m_assoc.group(1).strip()))
            continue
        if re.search(ALLOCATE_RE_TMPL.format(name=re.escape(name)), stmt, re.IGNORECASE):
            alloc_lines.append(lineno)
            continue
        if re.search(DEALLOCATE_RE_TMPL.format(name=re.escape(name)), stmt, re.IGNORECASE):
            dealloc_lines.append(lineno)
            continue
        if re.search(NULLIFY_RE_TMPL.format(name=re.escape(name)), stmt, re.IGNORECASE):
            unsafe = True
            break
        if re.search(ASSOCIATED_RE_TMPL.format(name=re.escape(name)), stmt, re.IGNORECASE):
            unsafe = True
            break
        if stmt_uses_token_as_call_arg(stmt, name):
            unsafe = True
            break
        use_lines.append((lineno, stmt))
        use_stmt_indices.append(idx)

    if unsafe:
        return None
    if len(assoc_lines) != 1 or alloc_lines or dealloc_lines:
        return None

    assoc_line, assoc_stmt_index, target_expr = assoc_lines[0]
    if not simple_target_expr(target_expr):
        return None
    if first_use_stmt_index is not None and first_use_stmt_index < assoc_stmt_index:
        return None

    return AliasAnalysis(
        assoc_line=assoc_line,
        assoc_stmt_index=assoc_stmt_index,
        target_expr=target_expr,
        target_base=(fscan.base_identifier(target_expr) or ""),
        use_lines=use_lines,
        use_stmt_indices=use_stmt_indices,
    )


def maybe_cleanup_target_attr(
    planned: PlannedEdit,
    cand: PointerDecl,
    alias: AliasAnalysis,
    body_lines: List[str],
    body_line_nos: List[int],
    raw_lines: List[str],
) -> None:
    """Remove a now-unused TARGET attr in a simple same-unit case."""
    target_base = alias.target_base
    if not target_base:
        return
    for lineno, stmt in zip(body_line_nos, body_lines):
        if lineno == cand.decl_line or lineno == alias.assoc_line:
            continue
        raw = raw_lines[lineno - 1]
        if not is_simple_raw_line(raw):
            continue
        code = fscan.strip_comment(stmt).strip()
        if "::" not in code or not DECL_START_RE.match(code):
            continue
        names = fscan.parse_declared_names_from_decl(code)
        if names != {target_base.lower()}:
            continue
        lhs = code.split("::", 1)[0]
        if "target" not in lhs.lower():
            continue
        still_targeted = False
        for lineno2, stmt2 in zip(body_line_nos, body_lines):
            if lineno2 == alias.assoc_line:
                continue
            m2 = re.match(r"^\s*[a-z][a-z0-9_]*\s*=>\s*(.+?)\s*$", stmt2, re.IGNORECASE)
            if m2 is None:
                continue
            b2 = fscan.base_identifier(m2.group(1).strip()) or ""
            if b2.lower() == target_base.lower():
                still_targeted = True
                break
        if still_targeted:
            return
        new_line = remove_target_attr_from_decl(raw)
        if new_line != raw and lineno not in planned.delete_lines:
            planned.claim_replace(lineno, new_line)
            planned.notes.append(f"line {lineno}: removed target attr from {target_base}")
        return


def plan_alias_direct_substitution(
    planned: PlannedEdit,
    cand: PointerDecl,
    alias: AliasAnalysis,
    raw_lines: List[str],
    occupied: Set[int],
    body_lines: List[str],
    body_line_nos: List[int],
) -> bool:
    """Plan direct target substitution for one fixed-alias pointer."""
    touched = [cand.decl_line, alias.assoc_line]
    for lineno, stmt in alias.use_lines:
        raw = raw_lines[lineno - 1]
        if not is_simple_raw_line(raw):
            return False
        if lineno in occupied or planned.overlaps([lineno]):
            return False
        if stmt_has_indexed_use(stmt, cand.name) or stmt_has_component_use(stmt, cand.name):
            return False
        touched.append(lineno)
    if any(ln in occupied or planned.overlaps([ln]) for ln in touched):
        return False

    for lineno, _stmt in alias.use_lines:
        raw = raw_lines[lineno - 1]
        code, comment, eol = split_code_comment_eol(raw)
        new_code = replace_identifier_outside_strings(code, cand.name, alias.target_expr)
        if new_code != code:
            planned.claim_replace(lineno, f"{new_code}{comment}{eol}")
    planned.claim_delete(cand.decl_line)
    planned.claim_delete(alias.assoc_line)
    planned.notes.append(
        f"line {cand.decl_line}: removed alias pointer {cand.name} => {alias.target_expr}"
    )
    maybe_cleanup_target_attr(planned, cand, alias, body_lines, body_line_nos, raw_lines)
    return True


def plan_alias_associate(
    planned: PlannedEdit,
    cand: PointerDecl,
    alias: AliasAnalysis,
    raw_lines: List[str],
    occupied: Set[int],
    body_lines: List[str],
    body_line_nos: List[int],
) -> bool:
    """Plan associate-block rewrite for one fixed-alias pointer."""
    if not alias.use_lines:
        return False
    if any(stmt_has_indexed_use(stmt, cand.name) or stmt_has_component_use(stmt, cand.name) for _lineno, stmt in alias.use_lines):
        return False
    if not alias.use_stmt_indices:
        return False
    last_use_stmt_index = alias.use_stmt_indices[-1]
    if last_use_stmt_index <= alias.assoc_stmt_index:
        return False

    stmt_index_to_lineno = {idx: lineno for idx, lineno in enumerate(body_line_nos)}
    block_stmt_indices = list(range(alias.assoc_stmt_index + 1, last_use_stmt_index + 1))
    if not block_stmt_indices:
        return False
    block_lines = [stmt_index_to_lineno[idx] for idx in block_stmt_indices]
    touched = [cand.decl_line, alias.assoc_line] + block_lines
    if any(ln in occupied or planned.overlaps([ln]) for ln in touched):
        return False

    for lineno in block_lines:
        raw = raw_lines[lineno - 1]
        if not is_simple_raw_line(raw):
            return False
        if line_has_control_flow_hazard(raw):
            return False
    if line_has_control_flow_hazard(raw_lines[alias.assoc_line - 1]):
        return False

    assoc_raw = raw_lines[alias.assoc_line - 1]
    assoc_code, assoc_comment, assoc_eol = split_code_comment_eol(assoc_raw)
    if assoc_comment.strip():
        return False
    assoc_indent = leading_spaces(assoc_code)
    body_indent = assoc_indent + "   "
    end_eol = assoc_eol or "\n"

    planned.claim_delete(cand.decl_line)
    planned.claim_replace(
        alias.assoc_line,
        f"{assoc_indent}associate ({cand.name} => {alias.target_expr}){end_eol}",
    )

    for lineno in block_lines:
        raw = raw_lines[lineno - 1]
        repl = f"{body_indent}{raw}"
        if lineno == block_lines[-1]:
            repl = f"{repl}{assoc_indent}end associate{end_eol}"
        planned.claim_replace(lineno, repl)

    planned.notes.append(
        f"line {cand.decl_line}: replaced alias pointer {cand.name} with associate block"
    )
    maybe_cleanup_target_attr(planned, cand, alias, body_lines, body_line_nos, raw_lines)
    return True


def plan_unit_edits(
    unit: Dict[str, object],
    raw_lines: List[str],
    occupied: Set[int],
    *,
    use_associate: bool,
) -> PlannedEdit:
    """Plan conservative edits for one program unit."""
    body_lines: List[str] = list(unit.get("body_lines", []))
    body_line_nos: List[int] = list(unit.get("body_line_nos", []))
    planned = PlannedEdit()
    if not body_lines or not body_line_nos:
        return planned

    candidates: List[PointerDecl] = []
    for idx, (lineno, stmt) in enumerate(zip(body_line_nos, body_lines)):
        raw = raw_lines[lineno - 1]
        if not is_simple_raw_line(raw):
            continue
        cand = parse_pointer_decl(stmt, idx, lineno)
        if cand is not None:
            candidates.append(cand)

    if not candidates:
        return planned

    for cand in candidates:
        if cand.decl_line in occupied or planned.overlaps([cand.decl_line]):
            continue
        if not is_null_initializer(cand.init_target):
            continue

        name = cand.name
        unsafe = False
        alloc_lines: List[int] = []
        assoc_seen = False
        for lineno, stmt in zip(body_line_nos, body_lines):
            if lineno == cand.decl_line:
                continue
            if not token_present(stmt, name):
                continue
            if re.match(POINTER_ASSIGN_RE_TMPL.format(name=re.escape(name)), stmt, re.IGNORECASE):
                assoc_seen = True
                continue
            if re.search(ALLOCATE_RE_TMPL.format(name=re.escape(name)), stmt, re.IGNORECASE):
                alloc_lines.append(lineno)
                continue
            if re.search(NULLIFY_RE_TMPL.format(name=re.escape(name)), stmt, re.IGNORECASE):
                unsafe = True
                break
            if re.search(ASSOCIATED_RE_TMPL.format(name=re.escape(name)), stmt, re.IGNORECASE):
                unsafe = True
                break
            if stmt_uses_token_as_call_arg(stmt, name):
                unsafe = True
                break
        if unsafe:
            continue

        if not assoc_seen and alloc_lines:
            raw_decl = raw_lines[cand.decl_line - 1]
            new_decl = transform_pointer_decl_to_allocatable(raw_decl)
            if new_decl != raw_decl:
                planned.claim_replace(cand.decl_line, new_decl)
                planned.notes.append(f"line {cand.decl_line}: {name} pointer -> allocatable")
            continue

        alias = collect_alias_analysis(cand, body_lines, body_line_nos)
        if alias is None:
            continue

        if use_associate:
            if plan_alias_associate(planned, cand, alias, raw_lines, occupied, body_lines, body_line_nos):
                continue
        plan_alias_direct_substitution(planned, cand, alias, raw_lines, occupied, body_lines, body_line_nos)

    return planned


def apply_planned_edits(raw_lines: List[str], planned: PlannedEdit) -> List[str]:
    """Apply one file's edits."""
    out: List[str] = []
    for lineno, raw in enumerate(raw_lines, start=1):
        if lineno in planned.delete_lines:
            continue
        if lineno in planned.replace_lines:
            out.append(planned.replace_lines[lineno])
        else:
            out.append(raw)
    return out


def transform_lines(raw_lines: List[str], *, use_associate: bool) -> Tuple[List[str], List[str]]:
    """Transform one source file."""
    text = "".join(raw_lines)
    units = fscan.split_fortran_units_simple(text)
    occupied: Set[int] = set()
    all_notes: List[str] = []
    merged = PlannedEdit()
    for unit in units:
        planned = plan_unit_edits(unit, raw_lines, occupied, use_associate=use_associate)
        for ln in planned.delete_lines:
            occupied.add(ln)
        for ln in planned.replace_lines:
            occupied.add(ln)
        merged.delete_lines.update(planned.delete_lines)
        merged.replace_lines.update(planned.replace_lines)
        all_notes.extend(planned.notes)
    if not merged.delete_lines and not merged.replace_lines:
        return raw_lines, []
    return apply_planned_edits(raw_lines, merged), all_notes


def default_out_path_for(path: Path) -> Path:
    """Build default single-file output path."""
    return path.with_name("temp.f90")


def process_file(
    path: Path,
    *,
    inplace: bool,
    backup: bool,
    show_diff: bool,
    out_dir: Optional[Path],
    output_path: Optional[Path],
    use_associate: bool,
) -> FileResult:
    """Process one file."""
    text = fscan.read_text_flexible(path)
    raw_lines = text.splitlines(keepends=True)
    new_lines, notes = transform_lines(raw_lines, use_associate=use_associate)
    changed = new_lines != raw_lines
    result = FileResult(path=path, changed=changed, notes=notes)
    if not changed:
        return result

    if inplace:
        target = path
    elif output_path is not None:
        target = output_path
    elif out_dir is not None:
        out_dir.mkdir(parents=True, exist_ok=True)
        target = out_dir / path.name
    else:
        target = default_out_path_for(path)

    if show_diff:
        diff = difflib.unified_diff(
            raw_lines,
            new_lines,
            fromfile=str(path),
            tofile=str(target),
            lineterm="",
        )
        for line in diff:
            print(line)
    if inplace and backup:
        backup_path = path.with_name(path.name + ".bak")
        shutil.copy2(path, backup_path)
        result.backup_path = backup_path
    target.write_text("".join(new_lines), encoding="utf-8", newline="")
    result.out_path = target
    return result


def build_arg_parser() -> argparse.ArgumentParser:
    """Build CLI parser."""
    parser = argparse.ArgumentParser(
        description=(
            "Remove two conservative no-need data-pointer cases: "
            "(1) pointer-owned storage -> allocatable, "
            "(2) single fixed aliases -> direct target use or associate blocks."
        )
    )
    parser.add_argument("files", nargs="+", type=Path, help="Fortran source files or globs")
    parser.add_argument("--inplace", action="store_true", help="overwrite input files")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True, help="write .bak on --inplace")
    parser.add_argument("--no-backup", dest="backup", action="store_false", help="disable .bak on --inplace")
    parser.add_argument("--diff", action="store_true", help="show unified diff")
    parser.add_argument("--out-dir", type=Path, help="write outputs to this directory")
    parser.add_argument("-o", "--output", type=Path, help="write output to this file")
    parser.add_argument(
        "--assoc",
        action="store_true",
        help="use associate blocks for eligible fixed-alias pointer cases",
    )
    return parser


def main() -> int:
    """Run CLI."""
    parser = build_arg_parser()
    args = parser.parse_args()
    if args.inplace and args.out_dir is not None:
        parser.error("--inplace and --out-dir cannot be used together")
    if args.inplace and args.output is not None:
        parser.error("--inplace and --output cannot be used together")
    if args.out_dir is not None and args.output is not None:
        parser.error("--out-dir and --output cannot be used together")

    files = choose_files(args.files)
    if not files:
        print("No source files matched.")
        return 1

    if args.output is not None and len(files) != 1:
        parser.error("--output requires exactly one input file")
    if not args.inplace and args.out_dir is None and args.output is None and len(files) != 1:
        parser.error(
            "with multiple input files, use --inplace, --out-dir, or --output; "
            "the default output file name temp.f90 is only used for a single input file"
        )

    changed_count = 0
    for path in files:
        if not path.exists():
            print(f"Missing file: {path}")
            continue
        result = process_file(
            path,
            inplace=args.inplace,
            backup=args.backup,
            show_diff=args.diff,
            out_dir=args.out_dir,
            output_path=args.output,
            use_associate=args.assoc,
        )
        if result.changed:
            changed_count += 1
            where = str(result.out_path) if result.out_path is not None else str(path)
            print(f"Changed: {path} -> {where}")
            for note in result.notes:
                print(f"  {note}")
            if result.backup_path is not None:
                print(f"  backup: {result.backup_path}")
        else:
            print(f"No change: {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
