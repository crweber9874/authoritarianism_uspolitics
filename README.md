# Authoritarianism and the Transformation of American Politics

## Christopher Federico, Stanley Feldman and Christopher Weber

### Updated: February 2024

## Data and Analysis Guide

This repository houses the code and general materials to replicate the findings in our book. The repo is organized into three broad sections:

$\textbf{Data Cleaning and Data Wrangling}$ $\texttt{../dataClean}$ This folder holds the recoded data files used throughout the book. The untouched, original data can be found in dataverse, along with the cleaned, transformed data.

1)  $\texttt{..../anes_c_recode.R}$. This large file structures the questions in the cumulative and individual cross sections in a logical manner, cleaning up missing data, creating categorical variables, etc. Because authoritarianism is not in the cumulative file, we must code the individual cross sections, then join back to the cumulative data based on the respondent identifier provided by the ANES.

2)  $\texttt{..../anes_c_scales.R}$. The $\texttt{..../anes_c_recode.R}$ file executes simple transformations and data joins. On the other hand, $\texttt{..../anes_c_scales.R}$ is where we construct summary scales, like efficacy, authoritarianism, and traditionalism.

3)  $\texttt{..../panel_recode.R}$. This file structures and recodes the panel data used i n the book.

4)  $\texttt{..../panel_scales.R}$. The $\texttt{..../anes_c_recode.R}$ file executes simple transformations and data joins.

5)  Chapter 3 focuses on differences in expressing authoritarianism, for non Hispanic White, Latino, and Black respondents. $\texttt{..../anes_ch3_recodes.R}$.

The data cleaning generally applies to three datasources -- the ANES cross sectional data, the ANES panel data, and the VSG panel data. On occasion, additional data cleaning is located in the analysis folder

## Chapter Analysis

Each chapter has its own folder, which contains the code to run the models and post-process the results.

### Chapter 3

$\texttt{..../Chapter Analysis/Chapter 3 Analysis}$. The code in this folder runs the models and post-processes the results for Chapter 3. The code is organized as follows: - $\texttt{models_and_post_process.ipynb}$ executes the models and generates the figures and tables reported in this chapter. - The "cleaned" data reside in the $\texttt{clean_data}$ folder. These "transformed" data are also available in the project dataverse page.

### Chapter 5

-   $\texttt{..../Chapter Analysis/Chapter 5 Analysis}$. The code in this folder runs the models and post-processes the results for Chapter 5. The code is organized as follows:

    \- $\texttt{run_models.ipynb}$ executes the models.

    \- $\texttt{post_process.ipynb}$ generates the figures and tables reported in the book.

The "cleaned" data reside in the $\texttt{clean_data}$ folder. These "transformed" data are also available in the project dataverse page. We also include the posterior estimates from all model runs. They are $\texttt{affect.rda}$, $\texttt{party\_model.rda}$, $\texttt{indirect.rda}$ in dataverse.

### Chapter 6

-   $\texttt{..../Chapter Analysis/Chapter 6 Analysis}$. The code in this folder runs the models and post-processes the results for Chapter 5. The code is organized as follows:

    \- $\texttt{run_models.ipynb}$ executes the models.

    \- $\texttt{post_process.ipynb}$ generates the figures and tables reported in the book.

The "cleaned" data reside in the $\texttt{clean_data}$ folder. These "transformed" data are also available in the project dataverse page. We also include the posterior estimates from all model runs. They are $\texttt{chapter6.rda}$, $\texttt{chapter5_cohorts.rda}$ in dataverse.

### Chapter 7

-   $\texttt{..../Chapter Analysis/Chapter 7 Analysis}$. The code in this folder runs the models and post-processes the results for Chapter 5. The code is organized as follows:

    \- $\texttt{exp1.R}$ and $\texttt{exp2.R}$ cleans the data and executes the analysis

The data for this chapter reside in the original, raw data folder. All data cleaning and transformations are done in the associated R files.


### Chapter 8

-   $\texttt{..../Chapter Analysis/Chapter 6 Analysis}$. The code in this folder runs the models and post-processes the results for Chapter 5. The code is organized as follows:

    \- $\texttt{run_models.ipynb}$ executes the models.

    \- $\texttt{post_process.ipynb}$ generates the figures and tables reported in the book.

The "cleaned" data reside in the $\texttt{clean_data}$ folder. These "transformed" data are also available in the project dataverse page. We also include the posterior estimates from all model runs. They are in the $\texttt{spatialModel.rda}$ file, a list of several models.

### Chapter 9

-   $\texttt{..../Chapter Analysis/Chapter 7 Analysis}$. The code in this folder runs the models and post-processes the results for Chapter 5. The code is organized as follows:

    \- $\texttt{exp3.R}$ cleans the data and executes the analysis

The data for this chapter reside in the original, raw data folder in dataverse. All data cleaning and transformations are done in the associated R file.

