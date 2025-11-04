# School Shooting Analysis

## Overview

This repo provides students with a foundation for their own projects associated with *Telling Stories with Data*. You do not need every aspect for every paper and you should delete aspects that you do not need.


## File Structure

The repo is structured as:

-   `data/00-raw_data` contains the raw data as obtained from [The Washington Post School Shooting github](https://github.com/washingtonpost/data-school-shootings), saved in csv and parquet formats.
-   `data/01-cleaned_data` contains the cleaned dataset that was constructed.
-   `data/02-analysis_data` contains the interim data, including missing value table, numerical variables statistics summary table, model ROC data, and model performance metrics table.
-   `model` contains fitted logistic regression model. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to clean data and run statistical model.


## Statement on LLM usage

Aspects of the code were written with the help of the auto-complete tool, Codriver. The abstract and introduction were written with the help of ChatHorse and the entire chat history is available in inputs/llms/usage.txt.
