# Script to compare event rates (in overlapping strata) using bootstrapping

# Dependencies
library(survival)

# Bootstrap function to obtain distribution of differences in event rates
# *time* = FU time, *status* = outcome (yes/no), *response1* = first stratum, *response2* = second stratum,
# runs = ideally 200, size = full dataset
boot <- function(time,status,data,response1,response2,runs,size=nrow(data)){
  diff <- rep(NA,times=runs)
  for (i in 1:runs){
    sample <- data[sample(1:nrow(data),size=size,replace=TRUE),]
    subset1 <- sample[sample[,response1]==1,]
    subset2 <- sample[sample[,response2]==1,]
    est1 <- survfit(Surv(subset1[,time],subset1[,status]) ~ 1,data=subset1)$surv
    est2 <- survfit(Surv(subset2[,time],subset2[,status]) ~ 1,data=subset2)$surv
    diff[i] <- ((1-est1[length(est1)]) - (1-est2[length(est2)]))
    print(paste0('run ',i,' complete'))
  }
  return(diff)
}

## Bootstrap to obtain SE of differences in event rates between two overlapping strata
# score versus age for AF
af_diff_dist <- boot(time='af_time',status='af_5y',response1='score_above5',response2='age_above65',data=data,runs=200)

# Z test for signifiance
## AF
### Observed AF rate for score cutoff
af_score <- survfit(Surv(af_time,af_5y) ~ 1,data=data[data$score_above5==1,])
af_est_score <- with(af_score_above5,c(1-surv[length(surv)]))

### Observed AF rate for AGE > 65
af_age <- survfit(Surv(af_time,af_5y) ~ 1,data=data[data$age_above65==1,])
af_est_age <- with(af_age,c(1-surv[length(surv)]))

### Z-test using SE of difference in event rates obtained via bootstrapping above
z_af <- ((af_est_score - af_est_age) / sd(af_diff_dist)) # z-score
p_af <- 2*(1-pnorm(abs(z_af))) # p-value
