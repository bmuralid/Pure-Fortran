from __future__ import annotations

import re


def reconcile_allocatable_decl_ranks(lines: list[str]) -> list[str]:
    intrinsic_type_re = (
        r"(?:"
        r"integer(?:\s*\([^)]*\)|\*\d+)?|"
        r"real(?:\s*\([^)]*\)|\*\d+)?|"
        r"double\s+precision|"
        r"complex(?:\s*\([^)]*\)|\*\d+)?|"
        r"double\s+complex|"
        r"logical(?:\s*\([^)]*\)|\*\d+)?|"
        r"character(?:\s*\([^)]*\)|\*\d+)?"
        r")"
    )
    decl_re = re.compile(
        rf"^(\s*{intrinsic_type_re}\s*,\s*allocatable\s*::\s*)([A-Za-z_]\w*)\((\s*:\s*(?:,\s*:\s*)+)\)(\s*)$",
        re.IGNORECASE,
    )
    unit_start_re = re.compile(r"^\s*(program|module|subroutine|function)\b", re.IGNORECASE)

    def _paren_occurrences_from_line(line: str, name: str):
        occs = []
        start = 0
        needle = f"{name}("
        low = line.lower()
        while True:
            idx = low.find(needle.lower(), start)
            if idx < 0:
                break
            j = idx + len(name)
            if j >= len(line) or line[j] != "(":
                start = idx + 1
                continue
            depth = 0
            k = j
            while k < len(line):
                ch = line[k]
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        inner = line[j + 1 : k]
                        parts = []
                        inner_depth = 0
                        cur = []
                        for c in inner:
                            if c == "(":
                                inner_depth += 1
                                cur.append(c)
                            elif c == ")":
                                inner_depth -= 1
                                cur.append(c)
                            elif c == "," and inner_depth == 0:
                                parts.append("".join(cur).strip())
                                cur = []
                            else:
                                cur.append(c)
                        parts.append("".join(cur).strip())
                        rank = len(parts)
                        faux_rank1 = rank == 2 and parts[1] == ":" and ":" not in parts[0]
                        occs.append((idx, k, 1 if faux_rank1 else rank, parts, faux_rank1))
                        break
                k += 1
            start = idx + 1
        return occs

    out = list(lines)
    unit_starts = [i for i, line in enumerate(lines) if unit_start_re.match(line)]
    if not unit_starts:
        unit_starts = [0]
    unit_starts.append(len(lines))
    for u in range(len(unit_starts) - 1):
        lo = unit_starts[u]
        hi = unit_starts[u + 1]
        decls = []
        for i in range(lo, hi):
            m = decl_re.match(lines[i])
            if not m:
                continue
            name = m.group(2)
            decl_rank = m.group(3).count(":")
            if decl_rank > 1:
                decls.append((i, name, decl_rank))
        for i_decl, name, _decl_rank in decls:
            observed = []
            faux_lines = []
            for i in range(lo, hi):
                if i == i_decl:
                    continue
                m_decl_other = decl_re.match(lines[i])
                if m_decl_other and m_decl_other.group(2).lower() == name.lower():
                    continue
                occs = _paren_occurrences_from_line(lines[i], name)
                observed.extend([rk for _a, _b, rk, _parts, _faux in occs])
                if any(_faux for _a, _b, _rk, _parts, _faux in occs):
                    faux_lines.append(i)
            if observed and max(observed) == 1:
                out[i_decl] = re.sub(
                    rf"\b{name}\s*\(\s*:\s*(?:,\s*:\s*)+\)",
                    f"{name}(:)",
                    out[i_decl],
                    count=1,
                )
                for i in faux_lines:
                    line = out[i]
                    occs = _paren_occurrences_from_line(line, name)
                    if not occs:
                        continue
                    rebuilt = []
                    pos = 0
                    for a, b, _rk, parts, faux in occs:
                        rebuilt.append(line[pos:a])
                        if faux:
                            rebuilt.append(f"{name}({parts[0]})")
                        else:
                            rebuilt.append(line[a : b + 1])
                        pos = b + 1
                    rebuilt.append(line[pos:])
                    out[i] = "".join(rebuilt)
    return out
