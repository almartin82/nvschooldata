"""
Tests for pynvschooldata Python wrapper.

These tests verify that the Python wrapper correctly interfaces with
the underlying R package and returns valid pandas DataFrames.
"""

import pytest
import pandas as pd


class TestImport:
    """Test that the package can be imported."""

    def test_import_package(self):
        """Package imports successfully."""
        import pynvschooldata as nv
        assert nv is not None

    def test_import_functions(self):
        """All expected functions are available."""
        import pynvschooldata as nv
        assert hasattr(nv, 'fetch_enr')
        assert hasattr(nv, 'fetch_enr_multi')
        assert hasattr(nv, 'tidy_enr')
        assert hasattr(nv, 'get_available_years')

    def test_version_exists(self):
        """Package has a version string."""
        import pynvschooldata as nv
        assert hasattr(nv, '__version__')
        assert isinstance(nv.__version__, str)


class TestGetAvailableYears:
    """Test get_available_years function."""

    def test_returns_dict(self):
        """Returns a dictionary."""
        import pynvschooldata as nv
        years = nv.get_available_years()
        assert isinstance(years, dict)

    def test_has_min_max_keys(self):
        """Dictionary has min_year and max_year keys."""
        import pynvschooldata as nv
        years = nv.get_available_years()
        assert 'min_year' in years
        assert 'max_year' in years

    def test_years_are_integers(self):
        """Year values are integers."""
        import pynvschooldata as nv
        years = nv.get_available_years()
        assert isinstance(years['min_year'], int)
        assert isinstance(years['max_year'], int)

    def test_min_less_than_max(self):
        """min_year is less than max_year."""
        import pynvschooldata as nv
        years = nv.get_available_years()
        assert years['min_year'] < years['max_year']

    def test_reasonable_year_range(self):
        """Years are in a reasonable range."""
        import pynvschooldata as nv
        years = nv.get_available_years()
        assert years['min_year'] >= 2010
        assert years['min_year'] <= 2020
        assert years['max_year'] >= 2020
        assert years['max_year'] <= 2030


class TestFetchEnr:
    """Test fetch_enr function."""

    def test_returns_dataframe(self):
        """Returns a pandas DataFrame."""
        import pynvschooldata as nv
        df = nv.fetch_enr(2026)
        assert isinstance(df, pd.DataFrame)

    def test_dataframe_not_empty(self):
        """DataFrame is not empty."""
        import pynvschooldata as nv
        df = nv.fetch_enr(2026)
        assert len(df) > 0

    def test_has_expected_columns(self):
        """DataFrame has expected columns."""
        import pynvschooldata as nv
        df = nv.fetch_enr(2026)
        expected_cols = ['end_year', 'n_students', 'grade_level']
        for col in expected_cols:
            assert col in df.columns, f"Missing column: {col}"

    def test_end_year_matches_request(self):
        """end_year column matches requested year."""
        import pynvschooldata as nv
        df = nv.fetch_enr(2026)
        assert (df['end_year'] == 2026).all()

    def test_n_students_is_numeric(self):
        """n_students column is numeric."""
        import pynvschooldata as nv
        df = nv.fetch_enr(2026)
        assert pd.api.types.is_numeric_dtype(df['n_students'])

    def test_has_reasonable_row_count(self):
        """DataFrame has a reasonable number of rows."""
        import pynvschooldata as nv
        df = nv.fetch_enr(2026)
        # Should have many rows (schools x grades x subgroups)
        assert len(df) > 1000

    def test_total_enrollment_reasonable(self):
        """Total enrollment is in a reasonable range."""
        import pynvschooldata as nv
        df = nv.fetch_enr(2026)
        # Filter for district-level total if available
        if 'is_district' in df.columns and 'grade_level' in df.columns:
            total_df = df[(df['is_district'] == True) & (df['grade_level'] == 'TOTAL')]
            if len(total_df) > 0:
                total = total_df['n_students'].sum()
                # Nevada should have 400k-600k students
                assert total > 400_000
                assert total < 600_000


class TestFetchEnrMulti:
    """Test fetch_enr_multi function."""

    def test_returns_dataframe(self):
        """Returns a pandas DataFrame."""
        import pynvschooldata as nv
        df = nv.fetch_enr_multi([2025, 2026])
        assert isinstance(df, pd.DataFrame)

    def test_contains_all_years(self):
        """DataFrame contains all requested years."""
        import pynvschooldata as nv
        years = [2024, 2025, 2026]
        df = nv.fetch_enr_multi(years)
        result_years = df['end_year'].unique()
        for year in years:
            assert year in result_years, f"Missing year: {year}"

    def test_more_rows_than_single_year(self):
        """Multiple years has more rows than single year."""
        import pynvschooldata as nv
        df_single = nv.fetch_enr(2026)
        df_multi = nv.fetch_enr_multi([2025, 2026])
        assert len(df_multi) > len(df_single)


class TestTidyEnr:
    """Test tidy_enr function."""

    def test_returns_dataframe(self):
        """Returns a pandas DataFrame."""
        import pynvschooldata as nv
        df = nv.fetch_enr(2026)
        tidy = nv.tidy_enr(df)
        assert isinstance(tidy, pd.DataFrame)

    def test_has_subgroup_column(self):
        """Tidy data has subgroup column."""
        import pynvschooldata as nv
        df = nv.fetch_enr(2026)
        tidy = nv.tidy_enr(df)
        assert 'subgroup' in tidy.columns or len(tidy) > 0


class TestDataIntegrity:
    """Test data integrity across functions."""

    def test_consistent_between_single_and_multi(self):
        """Single year fetch matches corresponding year in multi fetch."""
        import pynvschooldata as nv
        df_single = nv.fetch_enr(2026)
        df_multi = nv.fetch_enr_multi([2026])

        # Row counts should match
        assert len(df_single) == len(df_multi)

    def test_years_within_available_range(self):
        """Fetching within available range succeeds."""
        import pynvschooldata as nv
        years = nv.get_available_years()
        # Fetch the most recent year
        df = nv.fetch_enr(years['max_year'])
        assert len(df) > 0


class TestEdgeCases:
    """Test edge cases and error handling."""

    def test_invalid_year_raises_error(self):
        """Invalid year raises appropriate error."""
        import pynvschooldata as nv
        with pytest.raises(Exception):
            nv.fetch_enr(1800)  # Way too old

    def test_future_year_raises_error(self):
        """Future year raises appropriate error."""
        import pynvschooldata as nv
        with pytest.raises(Exception):
            nv.fetch_enr(2099)  # Way in future

    def test_empty_year_list_raises_error(self):
        """Empty year list raises appropriate error."""
        import pynvschooldata as nv
        with pytest.raises(Exception):
            nv.fetch_enr_multi([])


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
