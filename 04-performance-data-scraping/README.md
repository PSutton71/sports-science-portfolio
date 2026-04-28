# Web Scraping Project

## Overview
This project uses web scraping to collect publicly available sports performance data from an external source and turn it into a clean, analysis-ready dataset. The workflow was built to reduce manual data collection and make performance information easier to use for analysis and modeling.

## Problem
There was a need for external competition data from a publicly available source that could be collected more efficiently and transformed into a format that was easier to analyze.

## Data
- Publicly available performance data scraped from a sports website
- Meet results
- Athlete rankings
- Individual bests
- Contextual information such as date and distance
- Raw HTML pages transformed into a clean dataset ready for analysis

## Tools
- Python
- requests
- pandas
- Excel exports for downstream analysis in R or other environments

## Outcome
Built an automated Python script that scrapes and cleans performance statistics into a structured dataset, reducing manual data collection time and errors.

## Applied Use
- Use scraped performance data to build models that predict race finish time for new events and conditions.
- Estimate critical speed and D' from an athlete’s historical race results, reducing the need for dedicated testing sessions.
- Apply the same scraping and modeling workflow to maintain up-to-date performance profiles, monitor adaptation over a season, and inform individualized pacing and training targets.