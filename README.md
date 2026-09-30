# Crime Pattern Analysis and Crime Risk Prediction Using R

## 📌 Project Overview

This project analyzes district-level crime data in India from 2014 to 2023 using R. The project identifies crime patterns based on time, state, district, crime type, population, and crime-related outcomes.

Machine learning techniques are also used to predict the crime-risk category for the following year.

## 🎯 Objectives

- Analyze crime trends across different years.
- Identify state-wise, district-wise, and crime-type patterns.
- Perform data preprocessing and exploratory data analysis.
- Create useful features such as conviction rate and chargesheet rate.
- Classify crime risk into Low, Medium, and High categories.
- Use Decision Tree and Random Forest for next-year crime-risk prediction.
- Develop an interactive R Shiny dashboard.

## 📊 Dataset

The dataset contains **30,000 district-level crime records** covering:

- **Years:** 2014–2023
- **States:** 12
- **Districts:** 300
- **Records:** 30,000
- **Variables:** 9 original attributes

### Main Variables

- State
- District
- Year
- Crime_Type
- Cases_Reported
- Chargesheeted
- Convictions
- Population
- Crime_Rate_per_100k

## 🛠️ Technologies Used

- R
- RStudio
- dplyr
- ggplot2
- rpart
- randomForest
- caret
- Shiny
- Plotly

## 🔄 Project Workflow

```text
Crime Dataset
      ↓
Data Import
      ↓
Data Cleaning
      ↓
Exploratory Data Analysis
      ↓
Feature Engineering
      ↓
Data Visualization
      ↓
Machine Learning
      ↓
Model Evaluation
      ↓
Next-Year Crime Risk Prediction
      ↓
R Shiny Dashboard
