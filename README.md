# The State of Mind: Evaluating Depression Trends in California

This repository contains the code, data, and report for a statistical analysis project titled:

**“The State of Mind: Evaluating Depression Trends in California”**

The goal of this project is to explore patterns and risk factors associated with adult depression in California using public health data and statistical analysis in R.

---

## 📌 Project Overview

Depression is a major public health concern, influenced by demographic, socioeconomic, and lifestyle factors. This project focuses on:

- Estimating **prevalence of adult depression** in California.
- Understanding how depression varies by **age, gender, race/ethnicity, and income**.
- Identifying **key predictors** associated with higher likelihood of reported depression.
- Using **statistical modeling** to explore relationships between depression and other variables.

The project was developed as part of a statistics course final project.

---

## 📂 Repository Contents

> Note: Update filenames below if they differ in your repo.

- `STAT 515 FINAL CODE.R`  
  Main R script containing:
  - Data import and cleaning  
  - Exploratory Data Analysis (EDA)  
  - Visualizations  
  - Statistical modeling and interpretation  

- `adult-depression-lghc-indicator-24.csv`  
  Primary dataset used in the analysis.

- `STAT 515 Document.docx`  
  Full written report describing:
  - Background and motivation  
  - Data description  
  - Methodology  
  - Results and discussion  
  - Conclusions and limitations  

- `Final Project - Team 8 The State of Mind_ Evaluating Depression Trends in California (1).pptx`  
  Presentation slides summarizing:
  - Research questions  
  - Key findings  
  - Visualizations and conclusions  

You can modify the above file names to match your exact files if needed.

---

## 🧮 Methods & Analysis

The analysis in this project includes (typical structure):

### 1. Data Loading & Cleaning
- Reading the CSV dataset into R.
- Selecting relevant variables.
- Handling missing or inconsistent values.
- Recoding variables (e.g., categorical to factors, grouping age, etc.).

### 2. Exploratory Data Analysis (EDA)
- Summary statistics: mean, median, proportions.
- Distribution of depression-related indicators.
- Group-wise summaries, such as:
  - Depression by age group  
  - Depression by gender  
  - Depression by race/ethnicity  
  - Depression by income level  

### 3. Data Visualization
- Histograms and bar charts for key variables.
- Boxplots and grouped bar plots for comparisons across categories.
- Visual summaries used in the report and slides.

### 4. Statistical Modeling
- Regression models (often **logistic regression** for binary depression indicators).
- Model selection and diagnostics.
- Interpretation of coefficients:
  - Which variables are significantly associated with depression?
  - Direction and strength of relationships.

### 5. Interpretation & Conclusions
- Insights on which subgroups show higher prevalence.
- Discussion of social and public health implications.
- Limitations of the data and analysis.

For full technical details, refer to the **R script**, **DOCX report**, and **PPTX slides**.

---

## ▶️ How to Run the Project

### 1. Prerequisites

- **R** (latest version recommended)  
- Optional: **RStudio** for a nicer development environment

You may need to install some common R packages, such as:

```r
install.packages(c(
  "tidyverse",
  "dplyr",
  "ggplot2",
  "readr"
  # Add any other packages used in your script
))
