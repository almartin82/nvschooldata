"""
Tests for pynvschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pynvschooldata
    assert pynvschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pynvschooldata
    assert hasattr(pynvschooldata, 'fetch_enr')
    assert callable(pynvschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pynvschooldata
    assert hasattr(pynvschooldata, 'get_available_years')
    assert callable(pynvschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pynvschooldata
    assert hasattr(pynvschooldata, '__version__')
    assert isinstance(pynvschooldata.__version__, str)
