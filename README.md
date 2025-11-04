# School Shooting Analysis

## Overview

This repo provides students with a foundation for their own projects associated with *Telling Stories with Data*. You do not need every aspect for every paper and you should delete aspects that you do not need.


## File Structure

The repo is structured as:

-   `data/00-raw_data` contains the raw data as obtained from [The Washington Post School Shooting github](https://github.com/washingtonpost/data-school-shootings), saved in csv and parquet formats.
-   `data/01-cleaned_data` contains the cleaned dataset that was constructed.
-   `data/02-analysis_data` contains the interim data, including missing value table, numerical variables statistics summary table, model ROC data, and model performance metrics table.
-   `model` contains fitted logistic regression model. 
-   `other` contains relevant literature and project proposal.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to clean data and run statistical model.

## Execution Instructions

1. [OPTIONAL] Run `scripts/00_install_packages.R` to install required R packages.
2. [OPTIONAL] Run `scripts/01-transforming_file_type.R` to transform original csv data into parquet data.
3. Run `scripts/02-data_cleaning.R` to clean the data.
4. [OPTIONAL] Run `scripts/03-test_data.R` to confirm cleaned data has no missing values, and all entries are valid.
5. Run `scrips/04-summary_statistics.R` to summarize the dataset for the model.
6. Run `scripts/05-statistical_model.R` to create model, conduct model diagnostics and evaluations.
7. Run `outputs/paper.qmd` and Render to generate the PDF of this paper.

## Statement on LLM usage

Data visualization and summary table codes were written with the help of the auto-complete tool, Copilot. 
