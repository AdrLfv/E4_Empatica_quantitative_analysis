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

### Tests and statistical summaries`
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

To use the programs, run the R script `process_and_draw_graphs.R` to first synchronize data and generate comparison graphs.

Ensure that all necessary data files (e.g., `tags.csv`, `videos_timecodes.csv`, `participants.csv` and E4 data stream files) are present in the working directory.

## Prerequisites

- R and RStudio installed
- Required R libraries: `ggplot2`, `dplyr`, `ARTool`, etc.