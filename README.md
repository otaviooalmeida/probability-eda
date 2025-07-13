# 📊 Exploratory Data Analysis – Probability and Statistics

This repository contains an Exploratory Data Analysis (EDA) project developed in R as part of a university course on Probability and Statistics.

## 📁 Project Overview

The goal of this project is to explore and understand the behavior of two key variables:

1. **Corruption Index** – Position of countries in a global corruption ranking  
2. **Annual Income per Capita** – Average income per person in each country

The analysis includes:
- Descriptive statistics (mean, median, mode, variance, etc.)
- Frequency tables and percentage distributions
- Histograms, boxplots, and Q-Q plots
- Logarithmic transformation of skewed data
- Scatterplot to explore the relationship between variables

## 🧰 Tools and Packages

The project was developed in **R** and uses the following packages:
- `dplyr`
- `ggplot2`
- `psych`
- `moments`
- `gridExtra`

## 📂 Files

- `eda_script.R`: Main R script with all analysis steps
- `dataset.txt`: Input dataset (tabular format)
- `summary_table.csv`: Exported table with descriptive statistics
- `corruption_frequency.csv`: Frequency table for the Corruption Index
- `summary_table.png`: Visual summary of statistics

## 📌 Notes

- The dataset is read from a generic directory (`path/to/your/project`) — update this path as needed.
- Log transformation is applied to income data to reduce skewness and improve interpretability.

## 📈 Output Preview

The analysis provides insights into the distribution and relationship between corruption and income across countries, serving as a foundation for further statistical modeling.

---

Feel free to fork, adapt, or expand this project for your own statistical explorations!
