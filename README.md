# Prediction of hypotensive episodes from longitudinal high-frequency data collected in ICU Patients : MIMIC II application

## Authors

**Ményssa CHERIFA** 

[Linkedin](https://www.linkedin.com/in/menyssacherifa/) 

## Background

Among the most critical events that occur in intensive care units, acute hypotensive episodes (AHE) require effective, prompt intervention.Acute hypotension is the second leading cause of cardiac arrest and sudden death, and is associated with tissue ischemia and progressive organ failure. Hospital providers should consider any episode of hypotension as evidence of significant shock and illness.That’s why, prognoses of AHE is very important in the management of critical ill patients, and precise prediction models of AHE will give professionals much more precious time to determine a proper treatment for patients. In practice, assess « real-time » prediction model of AHE is a major concern for physicians and a valid prediction system that identifies an imminent event would be a significant benefit to timely support non-invasive and preventive treatments. 
Definition of AHE are based either on the systolic arterial blood pressure (SBP) or the mean arterial pressure (MAP). Several definitions of AHE exist, nevertheless the most frequent definition considers an AHE as an episode of two minutes or more with a MAP below 60 mmHg. MAP is usually calculated using either the generic formula  or directly using the mean of blood pressure waveform obtain from intra arterial blood catheter which is the most accurate blood pressure measurement. The 60 mmHg threshold value has never been scientifically scrutinized, and was merely proposed decades ago based on the fact that the kidneys cease to produce urine at pressures below 60 mmHg.
In 2009, a challenge was released by PhysioNet/computers where some valid machine learning approaches were proposed. This challenge focused on prediction of AHE using only previous measurement of blood pressure, ignoring other patient characteristics and/or time dependent variables. Moreover, to our best knowledge, no study aimed to compare these machine learning approaches against standard regression models.

## Objectives

The aim of this project is to provide dynamic prediction of an acute hypotensive episode for ICU patients whose arterial blood pressure and others features are monitored. We want to compare different statistical approaches that allow forecasting of AHE using standard regression models and machine learning algorithms.

## Methods

Basically, we use a logistic regression model to predict a dependent binary end-point Y using covariates X. Let Y be the binary response variable AHE assignment label and X = (X1, X2, ... , Xk ) a vector of covariates then   is the probability of an acute hypotensive episode according to X covariates.
Logistic regression does not assume a linear relationship between the dependent variable and the independent covariates  but it does assume linear relationship between the logit of the response and the explanatory variables. That’s why, we will compare this standard approach to Machine Learning (ML) classification algorithms. These methods are non parametric and take into account nonlinear relationships between covariates and dependent variable. There are 2 main principal methods in ML classification : 
-  by partitioning data into groups of similar values such as decisions trees
-  by modeling relationship between a set of input signals and an output signal such as artificial neuronal network
In order to predict AHE assignment label with theses methods, we define a study period, All patients observations between ICU admission and ICU discharge are cut into 90 min period. For each patient’s period, we will determine the study end – point (AHE status) as MAP <= 60 mmHg during 2 min at least in prediction window. Then, We will train the model using covariates of observation window to predict the occurrence of an AHE in the prediction window. Finally, we will split data into 2/3 for the training set and 1/3 for the validation set and we will assess to models performances on the validation set by computing the receiver operating characteristic curve (AUC), accuracy and time to obtain the prediction. 

## Data

We apply the results on multiparameter intelligent monitoring in intensive care II (MIMIC-II)[9]. This is a large, single-center database comprising information relating to patients admitted to critical care units at a large tertiary care hospital. This study was designed to provide physiologic waveform and numeric series such as MAP, heart rate, spO2 to explore in ICU patients. Data were collected between 2001 and 2008 from a variety of ICUs (medical, surgical, coronary care, and neonatal) in a single tertiary teaching hospital. 

### Tools

```
* SQL 
* R 
* Python 
* Awk 

```

# Presentation
