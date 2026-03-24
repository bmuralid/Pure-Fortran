"""Shared runtime helpers for Python emitted from scientific transpilers.

Used by xf2p.py and xr2p.py outputs.
"""
from __future__ import annotations

import math
import numpy as np

_np_reshape_orig = np.reshape


def _reshape_with_pad(a, newshape, order="C", pad=None):
    """NumPy reshape wrapper with optional Fortran-style pad support."""
    if pad is None and not isinstance(order, (str, bytes)):
        pad = order
        order = "F"
    shp = tuple(int(v) for v in np.asarray(newshape).ravel())
    if pad is None:
        return _np_reshape_orig(a, shp, order=order)
    arr = np.asarray(a)
    target = int(np.prod(np.asarray(shp, dtype=np.int64)))
    src = np.asarray(pad if np.asarray(pad).size > 0 else arr)
    if src.size == 0:
        src = np.zeros(target, dtype=arr.dtype if arr.size else np.float64)
    arr2 = np.resize(src, target)
    return _np_reshape_orig(arr2, shp, order="F")


np.reshape = _reshape_with_pad

__all__ = [
    "_reshape_with_pad",
    "_f_size",
    "_f_spread",
    "_f_assign_array",
    "_f_str_assign",
    "_f_adjustl",
    "_f_len_trim",
    "merge",
    "pack",
    "count",
    "maxval",
    "minval",
    "spread",
    "huge",
    "tiny",
    "floor",
    "nint",
    "ieee_value",
    "mean_1d",
    "var_1d",
    "argsort_real",
    "random_normal_vec",
    "random_choice2",
    "random_choice_prob",
    "random_choice_norep",
    "r_matmul",
    "matmul",
]


def _f_size(a, dim=None):
    """Fortran-like size with optional 1-based dimension argument."""
    if hasattr(a, "__dict__"):
        if dim is None:
            return len(a.__dict__)
        return len(a.__dict__)
    if isinstance(a, dict):
        if not a:
            return 0
        if dim is None:
            first = next(iter(a.values()))
            return int(np.asarray(first).size)
        k = int(dim)
        if k == 1:
            first = next(iter(a.values()))
            return int(np.asarray(first).size)
        if k == 2:
            return int(len(a))
        return 0
    arr = np.asarray(a)
    if dim is None:
        return arr.size
    k = int(dim) - 1
    if k < 0 or k >= arr.ndim:
        return 0
    return arr.shape[k]


def _f_spread(a, dim=None, ncopies=None):
    """Fortran-like spread: replicate array along a 1-based dimension."""
    arr = np.asarray(a)
    d = 1 if dim is None else int(dim)
    ncopy = 1 if ncopies is None else int(ncopies)
    axis = d - 1
    ex = np.expand_dims(arr, axis=axis)
    return np.repeat(ex, ncopy, axis=axis)


def _f_assign_array(lhs, rhs):
    """Assign with array-shape-aware replacement semantics used by transpiled code."""
    try:
        arr = np.array(rhs, copy=True)
    except Exception:
        if isinstance(rhs, (list, tuple)):
            parts = [np.asarray(v).ravel(order="F") for v in rhs]
            arr = np.concatenate(parts) if parts else np.asarray([], dtype=np.float64)
        else:
            raise
    if lhs is None:
        return arr
    lhs_arr = np.asarray(lhs)
    if arr.ndim == 0 and lhs_arr.ndim > 0:
        lhs_arr[...] = arr.item()
        return lhs
    if lhs_arr.shape != arr.shape or lhs_arr.dtype != arr.dtype:
        return arr
    lhs_arr[...] = arr
    return lhs




def _f_str_assign(rhs, n):
    """Fortran CHARACTER assignment: truncate or blank-pad to length n."""
    nn = int(n)
    def _one(v):
        txt = str(v)
        return txt[:nn].ljust(nn)
    if isinstance(rhs, np.ndarray):
        if rhs.ndim == 0:
            return _one(rhs.item())
        vec = np.vectorize(_one, otypes=[object])
        return vec(rhs)
    if isinstance(rhs, (list, tuple)) and not isinstance(rhs, (str, bytes)):
        vec = np.vectorize(_one, otypes=[object])
        return vec(np.asarray(rhs, dtype=object))
    return _one(rhs)


def _f_adjustl(x):
    """Fortran ADJUSTL for a scalar character value."""
    s = str(x)
    t = s.lstrip(" ")
    return t + (" " * (len(s) - len(t)))


def _f_len_trim(x):
    """Fortran LEN_TRIM for a scalar character value."""
    return len(str(x).rstrip(" "))

def merge(tsource, fsource, mask):
    """Fortran MERGE equivalent for scalar/array masks."""
    m = np.asarray(mask)
    if m.ndim == 0:
        return tsource if bool(m) else fsource
    return np.where(m, tsource, fsource)


def pack(x, mask):
    """Fortran PACK equivalent."""
    a = np.asarray(x)
    m = np.asarray(mask, dtype=bool)
    return a[m]


def count(x):
    """Count non-zero/true entries."""
    return int(np.count_nonzero(np.asarray(x)))


def maxval(x, dim=None):
    """Fortran MAXVAL equivalent with 1-based dim."""
    a = np.asarray(x)
    if dim is None:
        return np.max(a)
    return np.max(a, axis=int(dim) - 1)


def minval(x, dim=None):
    """Fortran MINVAL equivalent with 1-based dim."""
    a = np.asarray(x)
    if dim is None:
        return np.min(a)
    return np.min(a, axis=int(dim) - 1)


def spread(a, dim=1, ncopies=1):
    """Alias for spread used by some generated code."""
    return _f_spread(a, dim=dim, ncopies=ncopies)


def huge(x):
    """Return a large representable value for x's dtype."""
    a = np.asarray(x)
    if a.dtype.kind in "iu":
        return np.iinfo(a.dtype).max
    return np.finfo(np.float64).max


def tiny(x):
    """Return tiny positive float."""
    return np.finfo(np.float64).tiny


def floor(x):
    """Floor helper for scalar/array values."""
    a = np.asarray(x)
    if a.ndim == 0:
        return math.floor(float(a))
    return np.floor(a)


def nint(x):
    """Nearest integer (Fortran NINT-like)."""
    return int(np.rint(float(np.asarray(x))))


def ieee_value(x, what):
    """Return IEEE quiet NaN placeholder."""
    del x, what
    return np.nan


def ieee_is_finite(x):
    """IEEE finite-value predicate."""
    return np.isfinite(np.asarray(x, dtype=np.float64))


def mean_1d(x):
    """Mean of 1D input with empty fallback 0.0."""
    a = np.asarray(x, dtype=np.float64)
    return np.float64(np.mean(a)) if a.size else np.float64(0.0)


def var_1d(x):
    """Sample variance of 1D input with safe fallback."""
    a = np.asarray(x, dtype=np.float64)
    return np.float64(np.var(a, ddof=1)) if a.size > 1 else np.float64(0.0)


def argsort_real(x):
    """Argsort for real-valued input."""
    return np.argsort(np.asarray(x, dtype=np.float64))


def random_normal_vec(x):
    """Fill vector with standard normal draws."""
    return _f_assign_array(x, np.random.normal(size=np.asarray(x).shape))


def random_choice2(weights, n, z=None):
    """Sample component labels 0..k-1 by probability weights."""
    p = np.asarray(weights, dtype=np.float64)
    p = p / np.sum(p)
    out = np.random.choice(np.arange(p.size), size=int(n), p=p)
    return _f_assign_array(z, out)


def random_choice_prob(weights, n, z=None):
    """Alias for random_choice2."""
    return random_choice2(weights, n, z)


def random_choice_norep(n, k, out=None):
    """Sample without replacement from 0..n-1."""
    vals = np.random.choice(np.arange(int(n)), size=int(k), replace=False)
    return _f_assign_array(out, vals)


def r_matmul(a, b):
    """Matrix multiply wrapper."""
    return np.matmul(np.asarray(a), np.asarray(b))


def matmul(a, b):
    """Alias for matrix multiplication."""
    return r_matmul(a, b)
