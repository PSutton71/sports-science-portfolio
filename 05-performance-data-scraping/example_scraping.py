"""
TFRRS Results Scraper

Description
----------
Given a TFRRS results page URL, this script:
- Loads the page.
- Finds all HTML tables that look like individual result sheets.
- Optionally filters rows by a target year column.
- Writes each event's results to its own sheet in a single Excel file.

Usage
-----
1. For new user profiles install dependencies and Playwright:
   pip install playwright pandas lxml openpyxl
   playwright install

2. Edit ONLY the three variables in the "USER SETTINGS" section below:
   - 1) MAIN_URL - pick the race you want data from
   - 2) OUTPUT_EXCEL_PATH - Copy and paste the excel path and name the file 
   - 3) YEAR_FILTER (set to None to disable) - keep only rows where the "Year" column equals what you write

4. Run:
   python tfrrs_results_scraper.py
"""

from playwright.sync_api import sync_playwright
import pandas as pd
from io import StringIO
import re
import time


# =======================
# USER SETTINGS (EDIT ME)
# =======================

# 1) TFRRS results page URL to scrape
MAIN_URL = "https://www.tfrrs.org/results/xc/25571/Cowboy_Preview"

# 2) Full path for the Excel file to be created
OUTPUT_EXCEL_PATH = r"C:\Python Scraping\Example_Meet_Results.xlsx"

# 3) Optional year filter
#    Set to None if you do NOT want to filter by year
YEAR_FILTER = 2025   # e.g., 2025 or None


# ==========================
# HELPER FUNCTIONS (FIXED)
# ==========================

def looks_like_individual_result_sheet(df: pd.DataFrame) -> bool:
    """
    Heuristically decide if a table is an individual results sheet.

    We check for a minimal set of expected columns, allowing variation in order
    and capitalization. This is based on typical TFRRS XC/track result tables.
    """
    required = {"pl", "name", "team", "time"}
    # "year" and "avg. mile" are common but not strictly required so that
    # the script is a bit more flexible across meets.
    cols = {str(c).strip().lower() for c in df.columns}
    return required.issubset(cols)


def extract_event_label(idx: int, all_tables) -> str:
    """
    Build a human-readable sheet name for a given table.

    Strategy:
    - Look upward in the DOM from the table for a preceding element whose text
      contains the word "result".
    - Clean and truncate the text so that it is a valid Excel sheet name.
    """
    table_el = all_tables.nth(idx)
    label = table_el.evaluate(
        """
        el => {
            let prev = el.previousElementSibling;
            while (prev) {
                if (
                    prev.innerText &&
                    prev.innerText.length > 6 &&
                    prev.innerText.toLowerCase().includes('result')
                ) {
                    return prev.innerText.trim();
                }
                prev = prev.previousElementSibling;
            }
            return '';
        }
        """
    )

    # Clean up label to make it safe for Excel sheet names
    label = label.replace("\n", " ").replace("Top↑", "").strip()
    label = re.sub(r"\s+", " ", label)                     # collapse spaces
    label = re.sub(r"[\[\]*:?/\\]", "", label)             # remove invalid chars

    # Fallback generic label if we didn't find anything useful
    return label[:31] if label else f"Event_{idx + 1}"


def filter_by_year_if_needed(df: pd.DataFrame, year_filter) -> pd.DataFrame:
    """
    If YEAR_FILTER is set and a 'Year' column exists, keep only that year's rows.
    Otherwise, return df unchanged.
    """
    if year_filter is None:
        return df

    # Find any column that looks like "year"
    year_col = None
    for c in df.columns:
        if str(c).strip().lower() == "year":
            year_col = c
            break

    if year_col is None:
        return df  # cannot filter without a year column

    # Coerce to numeric when possible, then filter
    df_copy = df.copy()
    df_copy[year_col] = pd.to_numeric(df_copy[year_col], errors="coerce")
    df_filtered = df_copy[df_copy[year_col] == float(year_filter)]

    # If filtering removed everything, fall back to original
    return df_filtered if not df_filtered.empty else df


# ====================
# MAIN SCRIPT LOGIC
# ====================

def main():
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()

        print(f"Loading page: {MAIN_URL}")
        page.goto(MAIN_URL, timeout=60000)

        # Give page time to fully render tables (adjust if needed)
        time.sleep(8)

        all_tables = page.locator("table")
        n_tables = all_tables.count()
        print(f"Found {n_tables} total tables on page.")

        excel_writer = pd.ExcelWriter(OUTPUT_EXCEL_PATH, engine="openpyxl")
        saved_sheets = 0

        for i in range(n_tables):
            # Extract raw HTML from the table and try parsing as a DataFrame
            tbl_html = all_tables.nth(i).evaluate("el => el.outerHTML")
            try:
                df = pd.read_html(StringIO(tbl_html))[0]
            except Exception:
                # Skip tables that are not parsable by pandas.read_html
                continue

            if df.empty:
                continue

            # Check if this table looks like an individual result sheet
            if not looks_like_individual_result_sheet(df):
                continue

            # Optionally apply year filter
            df = filter_by_year_if_needed(df, YEAR_FILTER)
            if df.empty:
                continue

            # Derive spreadsheet sheet name
            label = extract_event_label(i, all_tables)
            print(f"Saving individual event sheet: {label}")

            # Save DataFrame to its own sheet
            df.to_excel(excel_writer, sheet_name=label[:31], index=False)
            saved_sheets += 1

        browser.close()

        if saved_sheets > 0:
            excel_writer.close()
            print(f"Saved {saved_sheets} individual event sheets to: {OUTPUT_EXCEL_PATH}")
        else:
            excel_writer.close()
            print("No individual event sheets were detected or saved.")


if __name__ == "__main__":
    main()
