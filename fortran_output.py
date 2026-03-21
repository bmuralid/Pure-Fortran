#!/usr/bin/env python3
"""Shared formatting/normalization helpers for Fortran runtime output."""

from __future__ import annotations

import re
from typing import List

_FLOAT_TOKEN_RE = re.compile(
    r"(?<![A-Za-z0-9_])([+-]?\d+\.\d+(?:[eEdD][+-]?\d+)?)"
)


def _split_float_token(token: str) -> tuple[str, str]:
    """Split token into mantissa and exponent suffix (if any)."""
    mant = token
    exp = ""
    for ch in ("e", "E", "d", "D"):
        if ch in mant:
            p = mant.find(ch)
            exp = mant[p:]
            mant = mant[:p]
            break
    return mant, exp


def _trim_float_token(token: str) -> str:
    """Trim redundant trailing fractional zeros while preserving exponent text."""
    mant, exp = _split_float_token(token)
    if "." not in mant:
        return token
    head, frac = mant.split(".", 1)
    frac = frac.rstrip("0")
    if frac == "":
        mant2 = f"{head}."
    else:
        mant2 = f"{head}.{frac}"
    return mant2 + exp


def _format_float_token(token: str, float_digits: int | None, trim: bool) -> str:
    """Format one decimal float token according to options."""
    if float_digits is None:
        return _trim_float_token(token) if trim else token

    mant, exp = _split_float_token(token)
    try:
        val = float(token.replace("d", "e").replace("D", "E"))
    except ValueError:
        return _trim_float_token(token) if trim else token

    if exp:
        marker = exp[0]
        suffix = f"{val:.{float_digits}e}"
        if marker in {"E", "D"}:
            suffix = suffix.replace("e", "E")
        if marker in {"d", "D"}:
            suffix = suffix.replace("e", "d").replace("E", "D")
        out = suffix
    else:
        out = f"{val:.{float_digits}f}"

    if trim:
        out = _trim_float_token(out)
    return out


def pretty_numeric_text(text: str, *, float_digits: int | None = None, trim: bool = True) -> str:
    """Pretty-format decimal float tokens in free text."""
    return _FLOAT_TOKEN_RE.sub(lambda m: _format_float_token(m.group(1), float_digits, trim), text)


def normalize_output_lines(
    text: str,
    *,
    pretty: bool = False,
    float_digits: int | None = None,
    trim: bool = True,
) -> List[str]:
    """Normalize output text to comparable lines.

    - normalizes line endings
    - optionally trims numeric float noise
    - collapses internal whitespace per line
    - drops trailing blank lines
    """
    s = text.replace("\r\n", "\n").replace("\r", "\n")
    if pretty:
        s = pretty_numeric_text(s, float_digits=float_digits, trim=trim)
    lines = [" ".join(ln.split()) for ln in s.split("\n")]
    while lines and lines[-1] == "":
        lines.pop()
    return lines


def pretty_output_text(text: str, *, float_digits: int | None = None, trim: bool = True) -> str:
    """Return display-oriented pretty output text for Fortran stdout/stderr."""
    lines = normalize_output_lines(text, pretty=True, float_digits=float_digits, trim=trim)
    return "\n".join(lines)


def pretty_output_line(line: str, *, float_digits: int | None = None, trim: bool = True) -> str:
    """Pretty-format one output line for stream printing."""
    return " ".join(pretty_numeric_text(line, float_digits=float_digits, trim=trim).split())
