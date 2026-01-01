"""
Core functions wrapping nvschooldata R package via rpy2.
"""

import pandas as pd
from rpy2 import robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter
from rpy2.robjects.packages import importr

# Import the R package (lazy load)
_pkg = None


def _get_pkg():
    """Lazy load the R package."""
    global _pkg
    if _pkg is None:
        _pkg = importr("nvschooldata")
    return _pkg


def fetch_enr(end_year: int) -> pd.DataFrame:
    """
    Fetch Nevada school enrollment data for a single year.

    Parameters
    ----------
    end_year : int
        The ending year of the school year (e.g., 2025 for 2024-25).

    Returns
    -------
    pd.DataFrame
        Enrollment data with columns for school/district identifiers,
        enrollment counts, and demographic breakdowns.

    Examples
    --------
    >>> import pynvschooldata as nv
    >>> df = nv.fetch_enr(2025)
    >>> df.head()
    """
    pkg = _get_pkg()
    with localconverter(robjects.default_converter + pandas2ri.converter):
        r_df = pkg.fetch_enr(end_year)
        if isinstance(r_df, pd.DataFrame):
            return r_df
        return pandas2ri.rpy2py(r_df)


def fetch_enr_multi(end_years: list[int]) -> pd.DataFrame:
    """
    Fetch Nevada school enrollment data for multiple years.

    Parameters
    ----------
    end_years : list[int]
        List of ending years (e.g., [2020, 2021, 2022]).

    Returns
    -------
    pd.DataFrame
        Combined enrollment data for all requested years.

    Examples
    --------
    >>> import pynvschooldata as nv
    >>> df = nv.fetch_enr_multi([2020, 2021, 2022])
    """
    pkg = _get_pkg()
    with localconverter(robjects.default_converter + pandas2ri.converter):
        r_years = robjects.IntVector(end_years)
        r_df = pkg.fetch_enr_multi(r_years)
        if isinstance(r_df, pd.DataFrame):
            return r_df
        return pandas2ri.rpy2py(r_df)


def tidy_enr(df: pd.DataFrame) -> pd.DataFrame:
    """
    Convert enrollment data to tidy (long) format.

    Parameters
    ----------
    df : pd.DataFrame
        Enrollment data from fetch_enr or fetch_enr_multi.

    Returns
    -------
    pd.DataFrame
        Tidy format with one row per school/year/demographic combination.

    Examples
    --------
    >>> import pynvschooldata as nv
    >>> df = nv.fetch_enr(2025)
    >>> tidy = nv.tidy_enr(df)
    """
    pkg = _get_pkg()
    with localconverter(robjects.default_converter + pandas2ri.converter):
        r_df = pandas2ri.py2rpy(df)
        r_result = pkg.tidy_enr(r_df)
        if isinstance(r_result, pd.DataFrame):
            return r_result
        return pandas2ri.rpy2py(r_result)


def get_available_years() -> dict:
    """
    Get the range of available years for enrollment data.

    Returns
    -------
    dict
        Dictionary with 'min_year' and 'max_year' keys.

    Examples
    --------
    >>> import pynvschooldata as nv
    >>> years = nv.get_available_years()
    >>> print(f"Data available from {years['min_year']} to {years['max_year']}")
    """
    pkg = _get_pkg()
    with localconverter(robjects.default_converter + pandas2ri.converter):
        r_result = pkg.get_available_years()
        # Handle both dict-like (NamedList) and R vector (rx2) access
        if isinstance(r_result, dict):
            return {
                "min_year": int(r_result["min_year"]),
                "max_year": int(r_result["max_year"]),
            }
        elif hasattr(r_result, "rx2"):
            return {
                "min_year": int(r_result.rx2("min_year")[0]),
                "max_year": int(r_result.rx2("max_year")[0]),
            }
        else:
            # NamedList or similar - access by key, value may be a list
            min_val = r_result["min_year"]
            max_val = r_result["max_year"]
            # Handle case where values are lists/arrays
            if hasattr(min_val, "__getitem__") and not isinstance(min_val, (int, float)):
                min_val = min_val[0]
            if hasattr(max_val, "__getitem__") and not isinstance(max_val, (int, float)):
                max_val = max_val[0]
            return {
                "min_year": int(min_val),
                "max_year": int(max_val),
            }
