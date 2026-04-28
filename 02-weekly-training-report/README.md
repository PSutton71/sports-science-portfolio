# Weekly Training Load Report

## Overview
This project automates the creation of a weekly athlete monitoring report using GPS and wellness data. The purpose was to give coaching staff a high-level overview of team workload and recovery trends to help inform weekly planning and periodization decisions.

## Problem
Coaching staff needed a weekly overview of team workload and practice demands to support periodization decisions, including planning deload weeks and avoiding unwanted spikes in training load.

## Data
- Polar GPS data, including total distance, acute to chronic workload ratio, and monotony
- Athlete wellness questionnaire data

## Tools
- R
- readxl
- dplyr
- ggplot2
- lubridate
- R Markdown

## Outcome
Created an automated workflow that generates a weekly PDF report with team training load plots, over-time trend visuals, and wellness summaries to support monitoring and planning.

## Applied Use
- Review total distance to evaluate whether weekly workload goals were met.
- Use monotony and acute to chronic workload ratio trends to help guide appropriate practice planning.
- Review weekly wellness data at both the individual and team level, including which factors may be contributing to decreases in team wellness.
- Share the weekly PDF with coaches before the following week’s practice planning to support decision-making.