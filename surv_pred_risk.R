# Script to derive predicted risk using linear predictor of a survival function

# Dependencies
library(data.table)
library(survival)

# Set data as data.table
setDT(data) # Replace *data* with dataset of interest

# Create cox PH model for score of interest (*time* = fu time [5 years], *status* = outcome yes/no [at 5 years], *score* = score of interest)
mod_score <- coxph(Surv(time,status) ~ score, data=data)

# Derive s0 (average hazard of outcome of interest in population)
### Determine average risk score value in population
data[,score_avgbeta := mean(score)]

### set km as a survival function using the average level of each factor
km_avg_score <- survfit(mod_score, data=data.frame(x1 = mean(score)),type="kaplan-meier")

# Set s0 as the survival coefficient of km
s0_score <- summary(km_avg_score, times = 5)$surv   # 5 years desired (may need to recode if time is in days)

# Calculate predicted risk using survival equation
data[,pred_risk_score := (1-(s0_score)^exp(score - (score_avgbeta)))*100]
