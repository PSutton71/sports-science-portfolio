# High-Speed Running Session Dashboard

## Project Description

This project is a single-tab Shiny dashboard designed to monitor high-speed running (HSR) for individual athletes on selected game and practice days. The dashboard was created to help practitioners quickly identify which athletes may need additional HSR exposure, recovery considerations, or closer review based on how their session output compared with the positional average for that day.

## Problem

In team sport settings, GPS data can be difficult to interpret quickly when coaches or practitioners need to make same-day or next-day decisions.

Raw session exports may show athlete HSR values, but they do not immediately answer practical questions such as:
- Who was meaningfully below the positional norm?
- Who was meaningfully above the positional norm?
- Does this athlete need additional HSR work?
- Does this athlete need more recovery or modified loading?
- Is this session output appropriate in the context of return to play?

Because of this, there was a need for a simple tool that could organize HSR by session type, compare athletes within their position group, and visually flag athletes whose session output may require follow-up action.

## Reason for Creating the Project

This dashboard was created to make HSR monitoring faster, more practical, and easier to apply in the field.

The goal was to build a simple decision-support tool that allows the user to:
- Select a game or practice session
- Review average HSR by position for that day
- Compare each athlete’s HSR with the same-day positional mean
- Quickly identify athletes who may require supplementation, recovery, or closer review

This project is especially useful in return-to-play and return-to-performance settings, where HSR exposure should be monitored carefully and interpreted within the context of the athlete’s position, recent workload, and upcoming schedule.

## Data Sources

The dashboard uses two Excel sheets:

### GPS Data
Required columns:
- Date
- Name
- Type
- HSR

### Demographics
Required columns:
- Name
- Position

## Flag Logic

Each athlete is compared with the same-day positional mean HSR using the absolute z-score.

Current thresholds:
- Black = within 1.0 SD of the positional mean
- Yellow = between 1.0 and 2.0 SD from the positional mean
- Red = more than 2.0 SD from the positional mean

Because the dashboard uses the absolute z-score, athletes are flagged whether they are above or below the positional mean. A black bar suggests the athlete’s HSR is within an expected range for their position on that selected day. A yellow bar suggests the athlete is moderately above or below the positional mean. This result should prompt the practitioner to consider the athlete’s recent and upcoming workload before deciding whether HSR supplementation is needed. A red bar suggests the athlete is meaningfully above or below the positional mean and likely requires follow-up interpretation.

If the athlete is well above the positional mean, application may include:
- Considering whether extra recovery is needed
- Reviewing upcoming session demands
- Adjusting future playing time, intensity, or load if needed

If the athlete is well below the positional mean, application may include:
- Considering whether HSR supplementation is needed
- Determining whether the athlete accumulated enough high-speed exposure in that session
- Planning additional HSR work to better align with positional demands

## Application

The main purpose of the dashboard is not just to describe HSR output, but to support follow-up decision-making.

## Data Handling Notes

The dashboard reads the GPS sheet as text first and then converts dates manually. This was done to help manage mixed Excel date formats, especially when some dates were stored as Excel serial values and others were stored as text. This was important because earlier practice dates loaded correctly while more recent practice dates did not.

## Main Packages

- shiny
- readxl
- dplyr
- ggplot2
- DT
- lubridate
- stringr
