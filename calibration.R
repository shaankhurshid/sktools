# Script for estimating and plotting calibration of a linear predictor in an external validation set

## Dependencies
library(rms)
library(survival)

# Fit cox model on score (replace *data* with validation set, *time* with follow up time [5 years], *status* with event yes/no [at 5 years])
# Since this is a previously derived model, the calculated score can be directly used as the predictor in the validation set without having to re-calibrate
# May need to replace time.inc=5 and u=5 if FU time is coded in days
fit<-cph(Surv(time,status) ~ score,data=data,surv=TRUE,time.inc=5,u=5,x=T,y=T) 

fit # beta coefficient for score and 95% CI corresponds to the calibration slope

# plot calibration
cal.score=calibrate(fit,u=5,cmethod='hare',B=200) # again, 5 may need to be recoded if FU in days; use B=200 if possible
plot(cal.score)
plot(1-cal.score, xlab="Predicted 5-year risk of AF", ylab="Proportion with AF at 5 years")
