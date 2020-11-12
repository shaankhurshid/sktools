#' Script to obtain time-dependent AUC/c-stat and model pseudo-R2 with 95% CI using bootstrapping
#' Dataset must be a data.frame. If given a data.table, a shallow data.frame copy will be made.
#' @export

# Bootstrap function
boot_cr2 <- function(time,status,response,data,runs,size){
  if (data.table::is.data.table(data)==TRUE){data <- as.data.frame(data)}
  out <- cbind(rep(NA,times=runs),rep(NA,times=runs))
  for (i in 1:runs){
    sample <- data[sample(1:nrow(data),size=size,replace=TRUE),]
    out[i,1] <- rms::cph(Surv(sample[,time],sample[,status]) ~ sample[,response],data=sample)$stats['R2']
    out[i,2] <- concordance(coxph(Surv(sample[,time],sample[,status]) ~ sample[,response],data=sample))$concordance
    print(paste0('run ',i,' complete'))
  }
  return(out)
}

## AF
# SCORE ('time' = censored survival time, 'status' = outcome status, 'data' = dataset, 'response' = classifier, 'runs' = ideally 200, 'size' = full dataset)
#boot_score <- boot(time='time',status='status',response='score',data=data,runs=200,size=nrow(data))

# Final R2 and AUC with 95% CI (1st value = estimate, 2nd value = Lower bound of 95% CI, 3rd value = Upper bound of 95% CI)
#r2_score <- c(mean(boot_score[,1]),mean(boot_score[,1])-1.96*sd(boot_score[,1]),mean(boot_score[,1])+1.96*sd(boot_score[,1]))
#auc_score <- c(mean(boot_score[,2]),mean(boot_score[,2])-1.96*sd(boot_score[,2]),mean(boot_score[,2])+1.96*sd(boot_score[,2]))
