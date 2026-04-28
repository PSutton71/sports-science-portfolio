# Daily Training Load Report

## Overview
This project automates the creation of a daily athlete monitoring report using GPS and wellness data. The goal was to provide coaching staff with a quick, visual summary of training load and athlete status for a selected date.

## Problem
Coaching staff needed a daily snapshot of GPS load and wellness data to quickly identify athletes who may be at increased risk, rather than manually reviewing raw data sheets.

## Data
- Polar GPS data, including total distance, high-speed running, and acute to chronic workload ratio
- Athlete wellness questionnaire data

## Tools
- R
- readxl
- dplyr
- lubridate
- R Markdown
- ggplot2
- ggpubr

## Outcome
Created an automated workflow that reads the Excel file, filters the data to the selected date, summarizes daily load and wellness information, and outputs a one-page PDF report with clear visuals.

## Applied Use
- Use color-coded acute to chronic workload ratio and wellness bands to flag athletes who may require closer monitoring.
- Athletes in red zones can be monitored over the following days to determine whether training volume or exposure should be adjusted.
- The daily PDF can be shared with coaches before practice to support fast decision-making about modifications, constraints, and athlete readiness.