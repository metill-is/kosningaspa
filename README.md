# A Dynamic Generalized Linear Model for Predicting the Icelandic Parliamentary Elections

This repository contains the code and data for a dynamic generalized linear model for predicting the outcome of the Icelandic Parliamentary Elections.

## Model Overview

The model combines three key components:
1. **Polling Data**: A dynamic state-space model tracking party support over time
2. **Fundamentals**: Historical election results and economic indicators
3. **Constituency Effects**: Regional variations in party support

### Key Features
- Handles varying numbers of parties across elections
- Accounts for house effects in polling
- Special handling of government collapse periods
- Combines national and constituency-level predictions
- Incorporates economic fundamentals

## Model Structure

### Data Inputs
- **Polling Data**
  - National polls (`y_n`)
  - Constituency polls (`y_k`)
  - House identifiers
  - Time information
  
- **Fundamentals**
  - Historical election results (`y_f`)
  - Previous vote shares (`x_f`)
  - Incumbent status and duration
  - Economic indicators (growth)
  - Vote/No Vote status

### Parameters
1. **Polling Model**
   - Random walk innovations (`z_beta_raw`)
   - House effects (`gamma_raw`)
   - Party-specific volatilities (`sigma`)
   - Correlation structure (`L_Omega`)

2. **Fundamentals Model**
   - Party-specific intercepts (`alpha_f`)
   - Effects of previous results (`beta_lag_f`)
   - Incumbent effects (`beta_inc_years_f`)
   - Economic effects (`beta_growth_f`)

3. **Constituency Effects**
   - Constituency-specific deviations (`delta_raw`)
   - Constituency effect scales (`sigma_delta`)

## File Structure

- `Stan/`
  - `polling_and_fundamentals.stan`: Main model combining all components
  - `polling_and_fundamentals_kjordaemi.stan`: Extended model with constituency effects
  - `polling_and_fundamentals_historic.stan`: Model for historical analysis
  - `base_model_no_polling_bias.stan`: Simplified version without house effects


## Key Model Components

### State Space Evolution
The model evolves party support through time using a random walk with varying volatility:
- Normal periods: Standard random walk
- Government collapse: Increased volatility
- Pre-election period: Additional uncertainty

### House Effects
Polling house biases are modeled as constant offsets with:
- Zero-sum constraints
- House-specific scales
- Reference house for identifiability

### Fundamentals Component
Predicts baseline party support using:
- Previous election results
- Incumbent status and duration
- Economic conditions
- Historical voting patterns

## Contributing

Please read through the Stan models to understand the implementation details. Key sections to focus on:
- Parameter declarations and transformations
- Model specification
- Generated quantities for predictions
