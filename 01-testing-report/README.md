# Athlete Performance and Injury Risk Reports

## Overview
This project automates the creation of individualized athlete performance reports and a team dashboard from a multi-test performance testing battery. The goal was to give coaching staff and athletes a clear, visual summary of each athlete's power, strength, speed, agility, and injury-risk readiness, without anyone having to dig through raw test exports. Each report is a single self-contained HTML file with interactive charts that can be opened on any device, shared directly with athletes, or printed.

## Problem
Testing data was spread across separate exports from force plates, dynamometers, an isokinetic dynamometer, a Wingate bike, and timing gates. Staff needed one place to see how each athlete compares to the team, where they have left-to-right imbalances or strength-ratio concerns, and what they should prioritize in training. Athletes also needed an easy-to-read report that explains their results in plain language.

## Data
All data is read from one Excel workbook, with one sheet per test source:
- **Demographics**: roster, position, height, and weight
- **CMJ**: countermovement jump height, peak propulsive force, and left/right force (also includes plyometric push-up trials, tagged "Prone")
- **DJ**: drop jump reactive strength index (RSI) and left/right landing force
- **IMTP**: isometric mid-thigh pull peak force
- **BioDex**: isokinetic quadriceps and hamstring peak torque at 60°/s
- **Dynamo**: grip squeeze, pinch grip, trunk rotation, hip extension, and neck flexion/extension force
- **Wingate**: peak power relative to body mass and rate of fatigue
- **Speed**: acceleration, sprint, and agility times

## Tools
- R
- readxl
- dplyr
- tidyr
- stringr
- janitor
- readr
- jsonlite
- base64enc
- plotly / Plotly.js
- HTML, CSS, and JavaScript

## Outcome
Created an automated workflow that reads the workbook, cleans and standardizes each test, selects each athlete's most recent best effort, and calculates:
- **Performance scores**: team-relative percentiles for power, strength, speed, and agility, combined into an overall score
- **Readiness scores**: upper- and lower-body scores built from asymmetry, hamstring-to-quadriceps ratio, neck flexion-to-extension ratio, and hip extension force thresholds, each flagged Green, Yellow, or Red
- **Dynamic Strength Index (DSI)**: CMJ peak propulsive force divided by IMTP peak force, plotted against Wingate fatigue resistance to place each athlete in a strength vs. explosiveness quadrant

A simple console menu lets staff generate a report for one athlete, or every athlete report plus the team dashboard. Each report includes a performance radar, upper- and lower-body readiness radars, the strength vs. explosiveness chart, and automatically written notes on strengths, development focus, and readiness priorities (including which side is weaker). The script tolerates missing tests: absent data is left blank, scores are re-weighted around it, and a data coverage file lists exactly which tests each athlete is missing.

## Applied Use
- Use the color-coded readiness radars to flag athletes with meaningful left-to-right imbalances, low H:Q ratios, or hip extension force below the injury-risk threshold.
- Athletes flagged Red or Yellow can be prioritized for corrective work and retested to track whether the imbalance resolves.
- The strength vs. explosiveness quadrant helps guide training emphasis, such as more ballistic work for strength-dominant athletes or more heavy strength work for power-dominant athletes.
- The team dashboard gives staff a leaderboard view of every score and a dropdown to review any athlete's readiness profile before planning training.
- Individual reports can be handed directly to athletes so they understand their results and training priorities.

![Athlete performance report screenshot](Image-1.png")
![Athlete performance report screenshot](Image-2.png")
![Athlete performance report screenshot](Image-3.png")
![Athlete performance report screenshot](Image-4.png")
![Athlete performance report screenshot](Image-5.png")
![Athlete performance report screenshot](Image-6.png")
