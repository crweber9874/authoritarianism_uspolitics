
# Authoritarianism and the Transformation of American Politics

## Christopher Federico, Stanley Feldman, and Christopher Weber

### Updated: February 2025

## Data and Analysis Guide

This repository houses the code and general materials to replicate the findings in our book. The repository is organized into three broad sections:

### Data Cleaning and Data Wrangling

The `../dataClean` folder holds the recoded data files used throughout the book. The untouched, original data can be found in Dataverse, along with the cleaned, transformed data.

1. **`..../anes_c_recode.R`**: This large file structures the questions in the cumulative and individual cross-sections in a logical manner, cleaning up missing data, creating categorical variables, etc. Because authoritarianism is not in the cumulative file, we must code the individual cross-sections, then join back to the cumulative data based on the respondent identifier provided by the ANES.

2. **`..../anes_c_scales.R`**: This file executes simple transformations and data joins. It also constructs summary scales, like efficacy, authoritarianism, and traditionalism.

3. **`..../panel_recode.R`**: This file structures and recodes the panel data used in the book.

4. **`..../panel_scales.R`**: This file executes simple transformations and data joins.

5. **`..../anes_ch3_recodes.R`**: This file focuses on differences in expressing authoritarianism for non-Hispanic White, Latino, and Black respondents.

The data cleaning generally applies to three data sources: the ANES cross-sectional data, the ANES panel data, and the VSG panel data. On occasion, additional data cleaning is located in the analysis folder.

---

## Chapter Analysis

Each chapter has its own folder, which contains the code to run the models and post-process the results.

### Chapter 3

- **`..../Chapter Analysis/Chapter 3 Analysis`**: The code in this folder runs the models and post-processes the results for Chapter 3.
  - **`models_and_post_process.ipynb`**: Executes the models and generates the figures and tables reported in this chapter.
  - The "cleaned" data reside in the `clean_data` folder. These "transformed" data are also available on the project Dataverse page.

### Chapter 5

- **`..../Chapter Analysis/Chapter 5 Analysis`**: The code in this folder runs the models and post-processes the results for Chapter 5.
  - **`run_models.ipynb`**: Executes the models.
  - **`post_process.ipynb`**: Generates the figures and tables reported in the book.
  - The "cleaned" data reside in the `clean_data` folder. These "transformed" data are also available on the project Dataverse page.
  - Posterior estimates from all model runs are included: `affect.rda`, `party_model.rda`, and `indirect.rda`.

### Chapter 6

- **`..../Chapter Analysis/Chapter 6 Analysis`**: The code in this folder runs the models and post-processes the results for Chapter 6.
  - **`run_models.ipynb`**: Executes the models.
  - **`post_process.ipynb`**: Generates the figures and tables reported in the book.
  - The "cleaned" data reside in the `clean_data` folder. These "transformed" data are also available on the project Dataverse page.
  - Posterior estimates from all model runs are included: `chapter6.rda` and `chapter5_cohorts.rda`.

### Chapter 7

- **`..../Chapter Analysis/Chapter 7 Analysis`**: The code in this folder runs the models and post-processes the results for Chapter 7.
  - **`exp1.R`** and **`exp2.R`**: Clean the data and execute the analysis.
  - The data for this chapter reside in the original, raw data folder. All data cleaning and transformations are done in the associated R files.

### Chapter 8

- **`..../Chapter Analysis/Chapter 8 Analysis`**: The code in this folder runs the models and post-processes the results for Chapter 8.
  - **`run_models.ipynb`**: Executes the models.
  - **`post_process.ipynb`**: Generates the figures and tables reported in the book.
  - The "cleaned" data reside in the `clean_data` folder. These "transformed" data are also available on the project Dataverse page.
  - Posterior estimates from all model runs are included in the `spatialModel.rda` file, which contains a list of several models.

### Chapter 9

- **`..../Chapter Analysis/Chapter 9 Analysis`**: The code in this folder runs the models and post-processes the results for Chapter 9.
  - **`exp3.R`**: Cleans the data and executes the analysis.
  - The data for this chapter reside in the original, raw data folder in Dataverse. All data cleaning and transformations are done in the associated R file.
