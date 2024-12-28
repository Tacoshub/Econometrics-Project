# EU ETS Spot Volume Modeling and Forecasting

This repository contains the implementation and analysis for our project on modeling and forecasting daily trading volumes of carbon emission allowances within the EU ETS (European Union Emission Trading System). The project explores various autoregressive models, including ARCH/GARCH structures, and evaluates the impact of exogenous variables like coal and oil prices on trading volume predictions.

---

## Project Overview

The project focuses on the following:
1. **Modeling EU ETS Trading Volumes**: Using ARMA and GARCH-based models to capture the time series behavior.
2. **Incorporating Exogenous Variables**: Evaluating the impact of external factors such as coal and Brent oil prices on trading volumes.
3. **Forecasting**: Developing a predictive model to forecast out-of-sample trading volumes with an emphasis on accuracy and volatility.

Key findings include:
- The presence of volatility clustering, justifying the use of GARCH models.
- ARMA(1,1)-GARCH(1,1) selected as the optimal model based on Bayesian Information Criterion (BIC).
- Coal price differences as the most significant exogenous variable, though results remain modest.

---

## Repository Contents

### Code
- R scripts for:
  - Data preprocessing and cleaning.
  - Stationarity testing and ARCH effect detection.
  - Model fitting and selection (AR, MA, ARMA, GARCH, and ARMAX-GARCH models).
  - Forecasting analysis and performance evaluation.

### Data
- Time series data for EU ETS trading volumes (2013–2020).
- Exogenous variables: Brent oil, coal prices, and VIX daily differences.

### Results
- Model comparisons and selection criteria.
- Forecasting results, including performance metrics like MAPE, MAE, and MSE.

### Documentation
- A detailed project report explaining the methodology, results, and conclusions.

---

## Methodology

1. **Data Preprocessing**:
   - Removal of outliers and non-trading days.
   - Logarithmic and arc-tangent transformations for stationarity and gaussianity.

2. **Model Selection**:
   - Use of ACF/PACF for lag selection.
   - BIC as the primary criterion for model comparison.

3. **Exogenous Variables**:
   - Inclusion of variables like coal prices and Brent oil prices.
   - Analysis of statistical significance and impact on predictions.

4. **Forecasting**:
   - One-step-ahead predictions using ARMAX(1,1,1)-GARCH(1,1).
   - Evaluation of predicted vs. actual trading volumes.

---

## Authors
- Flavio Salvatore Boccia
- Ludovico Costa
- Michele Facconi
- Alessandro Pigato

---
