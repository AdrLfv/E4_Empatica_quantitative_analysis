# Process and Analyze Data

This directory contains several R scripts designed to process and analyze data from various sessions of an experiment. The focus is on handling heart rate (HR) data, calculating its delta (Δ𝐻𝑅(𝑡𝑥 ) = 𝐻𝑅(𝑡𝑥 ) − 𝐻𝑅(𝑡0)), and processing Electrodermal Activity (EDA) and accelerometer (ACC) data. The scripts aim to synchronize all collected data, generate visualizations, and perform statistical analyses.

## Contents of the Directory

### `process_and_draw_graphs.R`
- **Data Synchronization**: This script synchronizes the experiment data with video recordings by matching the first timestamp found in "tags.csv" with corresponding timestamps in "videos_timecodes.csv" and the timestamps in each E4 data stream file. This ensures that the data from different sources are aligned for accurate analysis.
  - **Tags Synchronization**: Aligns the first timestamp in "tags.csv" with the video timecodes.
  - **E4 Data Stream Synchronization**: Aligns the timestamps in each E4 data stream file (HR, EDA, ACC) with the synchronized video timecodes.
- **Comparison Graph Generation**: Utilizing the ggplot2 library, the script generates various graphs to visualize the processed and synchronized data, aiding in the identification of patterns, trends, and relationships.
  - **Heart Rate Graphs**: Visualizes the heart rate data over time, highlighting significant events.
  - **EDA Graphs**: Displays the electrodermal activity data, showing responses to stimuli.
  - **ACC Graphs**: Illustrates the accelerometer data, indicating movement and activity levels.
- **Customization**: Users can customize the generated graphs, adjusting aspects like colors, labels, titles, and axes to suit their analysis needs.
  - **Color Customization**: Change the color schemes of the graphs.
  - **Label Customization**: Modify the labels for axes and data points.
  - **Title Customization**: Set custom titles for the graphs.
  - **Axis Customization**: Adjust the scales and limits of the axes.

### `Tests and statistical summaries`
  - **Wilcoxon Signed-Rank Test**: Compares paired samples to assess whether their population mean ranks differ.
  - **Shapiro-Wilk Test**: Tests the normality of the data distribution.
  - **Friedman Test**: Determines whether there are statistically significant differences between multiple groups.
  - **Kruskal-Wallis Test**: Compares the medians of two or more groups to assess whether they are significantly different.
  - **Spearman Correlation Test**: Evaluates the strength and direction of the relationship between two variables.
  - **Descriptive Statistics**: Calculates mean, median, standard deviation, minimum, and maximum values for heart rate variation.

## Features

- **Data Synchronization**: Ensures that data from different sources are aligned for accurate analysis.
- **Graph Generation**: Visualizes processed and synchronized data to identify patterns, trends, and relationships.
- **Customization**: Allows users to customize graphs to meet their analysis needs.
- **Statistical Analysis**: Performs statistical tests to understand the impact of different conditions on physiological responses.
- **Data Filtering**: Filters data based on specific criteria for targeted analysis.

## Usage

To use the programs, run the R script `process_and_draw_graphs.R` to first generate the clean data from the raw data stored in data_rds and generate comparison graphs.
Then, run the desired statistical test directly by executing the corresponding R script.

## Run tests on your own data

Ensure that all necessary data files (e.g., `tags.csv`, `videos_timecodes.csv`, `participants.csv` and a folder "E4 streams") are present in a folder "data_csv" placed in the working directory if you want to work with your own data.
Then execute the program `read_save_rds.R` to convert all your csv to rds objects, you can then execute `process_and_draw_graphs.R`.

### Data format

The folder "E4 streams" should contains all the folders containing the data of each participant. For example: "P01_25_12_00h" containing at least `ACC.csv`, `EDA.csv`, `HR.csv`, and `tags.csv`.

In our example, the excel file `participants.csv` contains the following columns:
  "ID"; "Order"; "Familiarity"; "A"; "B"; "C"; "D"; "isPianist"; "Gender"; "A_diagram_before"; "A_diagram_after"; "B_diagram_before"; "B_diagram_after"; "Connection_A"; "Connection_B"; "A_diagram_var"; "B_diagram_var"
The column "ID" should match the name of the folder containing the data of the participant.

In the same example, the excel file `videos_timecodes.csv` contains the following columns:
  "Participant"; "A_started"; "A_finished"; "B_started"; "B_finished"; "C_started"; "C_finished"; "D_started"; "D_finished"; "Timestamp"
The column "Participant" should match the name of the folder containing the data of the participant (same as ID).
All the cells in the columns "A_started", "A_finished", "B_started", "B_finished", "C_started", "C_finished", "D_started", "D_finished" should be filled with the time in seconds when the video started and finished.
The column "Timestamp" should contain the corresponding time in seconds of the timestamp since the beginning of the video. This timestamp is then linked to the one in the "tags.csv" file. All the times have to follow the following format: "hh:mm:ss".


## Prerequisites

- R and RStudio installed
- Required R libraries: `ggplot2`, `dplyr`, `ARTool`, etc.