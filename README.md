# Process and Draw Graphs

The program `process_and_draw_graphs.R`, is specifically designed to handle data from various sessions of an experiment, focusing on processing heart rate (HR) data, calculating its coefficient of variation, and processing Electrodermal Activity (EDA) and accelerometer (ACC) data. It aims to synchronize all collected data by aligning the first timestamp found in "tags.csv" with timestamps in "videos_timecodes.csv" and each E4 data stream file's timestamp.

## Features

- **Data Synchronization**: It synchronizes the experiment data with video recordings by matching the first timestamp in "tags.csv" with corresponding timestamps in "videos_timecodes.csv" and the timestamps in each E4 data stream file. This ensures that the data from different sources are aligned for accurate analysis.

- **Graph Generation**: Utilizing the ggplot2 library, the program generates various graphs to visualize the processed and synchronized data, aiding in the identification of patterns, trends, and relationships.

- **Customization**: Users can customize the generated graphs, adjusting aspects like colors, labels, titles, and axes to suit their analysis needs.

- **Statistical Analysis**: Additional programs are included to format all the data, perform Wilcoxon and Shapiro tests and obtain other statistics, particularly on HR variations according to the order of passage, session, or other variables. These analyses help in understanding the impact of different conditions on participants' physiological responses.

## Usage

1. Ensure your data is prepared in a compatible format. The "videos_timecodes.csv" file should include a 'participant' column, followed by eight columns for video times in the `hh:mm:ss` format for each session phase (A_started, A_finished, B_started, ...), and a final 'Timestamp' column with all video times in the `hh:mm:ss` format when the E4's visual timestamp appears.

2. Modify the `TO MODIFY` sections in the `process_and_draw_graphs.R` script, specify the input files, output directory, and other necessary parameters.

3. Run the program to process the data and generate graphs, which will be saved in the specified output directory. Additionally, use the included statistical analysis programs as needed to further explore the data.

## Dependencies

The program relies on the following R packages:

- **ggplot2**: For data visualization and graph generation.
- **dplyr**: For data manipulation and transformation.
- **readr**: For reading data from various file formats.
- **stats**: For performing statistical tests, including the Wilcoxon test.

This automation follows the guidelines provided by Empatica on their [support site](https://support.empatica.com/hc/en-us/articles/202800715-Session-start-time-format-and-synchronization) for session start time format and synchronization.
