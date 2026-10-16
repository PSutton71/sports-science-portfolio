# Sports Science Portfolio

This repository contains applied sports science and performance analytics projects focused on athlete monitoring, workload and wellness reporting, performance testing, dashboard development, and performance data workflows. The projects were built to support coach-, athlete-, stakeholder-, and medical staff-facing reports for faster, clearer, and more practical decision-making.

## Projects

### 1. Athlete Performance and Injury Risk Reports
`01-testing-report`

An automated R workflow that turns a multi-test performance battery (force plates, dynamometry, isokinetic dynamometer, Wingate, and timing gates) into individualized athlete reports and a team dashboard. Each report includes team-relative performance scores for power, strength, speed, and agility; upper- and lower-body readiness radars built from asymmetry, H:Q ratio, neck ratio, and hip extension force thresholds; and a Dynamic Strength Index vs. fatigue-resistance quadrant chart. Plain-language notes on strengths, development focus, and the weaker side are written automatically, and missing tests are handled by re-weighting scores and logging data coverage.

### 2. Daily Training Load Report
`02-daily-training-report`

An automated daily report that summarizes GPS load and wellness data to help identify athletes who may need closer monitoring. This coach-facing report assists with daily practice planning.

### 3. Weekly Training Load Report
`03-weekly-training-report`

An automated weekly report that provides an overview of team workload, recovery trends, and periodization support. This coach-facing report assists in decision-making across the macrocycle.

### 4. Athlete Monitoring Dashboard
`04-athlete-monitoring-dashboard`

An interactive dashboard built in R Shiny that brings together session-level performance monitoring and athlete comparison tools in one place. This dashboard contains coach- and athlete-facing tabs to support a wide range of conversations.

### 5. Performance Data Scraping
`05-performance-data-scraping`

A Python workflow that collects public performance data and turns it into a clean dataset for analysis and modeling.

### 6. High Speed Running Management Dashboard
`06-hsr-management-dashboard`

An HSR dashboard that filters by session type and date, summarizes positional high-speed running, and flags athletes based on distance from same-day positional means. This dashboard was created for coach-facing decision-making in regard to HSR supplementation.

### 7. Wellness & Training Load Report
`07-wellness-report`

An automated, self-contained HTML report built from a daily athlete morning survey. It compares each athlete's wellness composite (fatigue, muscle soreness, stress, mood, and sleep quality) to their own rolling 28-day baseline using z-scores, flags athletes Green, Yellow, or Red, and pairs wellness with the previous day's session RPE training load. Staff can move between survey dates, view team or individual trends, and see a "check in with" list before practice.

### 8. Dynamic HTML Athlete Monitoring Report
`08-dynamic-html-report`

A dynamic browser-based athlete monitoring report built with R, HTML, CSS, JavaScript, and Chart.js. An R script reads Excel-based monthly, weekly, and schedule data, converts them into JSON, and injects them into an HTML template that displays interactive timelines, ROM comparisons, strength leaderboards, mobility trends, and schedule-linked monitoring visuals. This format is effective for coaches, athletes, stakeholders, or medical staff to support red-flagging athletes and continual monitoring in a dynamic yet simple format.

## Skills Demonstrated

- Data cleaning and wrangling
- Automated reporting
- Interactive dashboard development
- Dynamic, self-contained HTML report generation
- Performance monitoring and workload analysis
- Wellness monitoring with individual baselines and z-score flagging
- Performance testing analysis and injury-risk / readiness screening
- Individualized athlete-facing reports with automated written feedback
- Data visualization with Shiny, Plotly, Chart.js, and custom SVG
- Web scraping and data preparation

## Tools

- **R**: dplyr, tidyr, stringr, readxl, janitor, readr, jsonlite, base64enc, plotly, Shiny
- **Python**: web scraping and data preparation
- **Web**: HTML, CSS, JavaScript, Plotly.js, Chart.js

## Repository Structure

```
sports-science-portfolio/
├── 01-testing-report/
├── 02-daily-training-report/
├── 03-weekly-training-report/
├── 04-athlete-monitoring-dashboard/
├── 05-performance-data-scraping/
├── 06-hsr-managment-dashboard/
├── 07-wellness-report/
└── 08-dynamic-html-report/
```

Each project folder contains:
- A short project README
- The relevant R or Python code
- Supporting files as needed (HTML templates, screenshots)

## Contact

Paige Sutton
paige.sutton17@gmail.com
