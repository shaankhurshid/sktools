# This function computes the cumulative sum of the difference between the next row and the current row
# Optional stratification by categorical variable

library(data.table)

ir <- function(data,strata=NULL,time,status){
  out <- list()
  if (length(strata) == 0){
    events <- nrow(data[get(status)==1])
    pt <- sum(data[,get(time)])
    rate <- events/pt
    out[[1]] <- c("all",events,pt,rate,rate-1.96*(rate/sqrt(events)),rate+1.96*(rate/sqrt(events)))
    return(unlist(out))
  } else {
    for (i in 1:length(unique(data[,get(strata)]))){
        subset <- data[get(strata)==unique(data[,get(strata)])[i]]
        events <- nrow(subset[get(status)==1])
        pt <- sum(subset[,get(time)])
        rate <- events/pt
        out[[i]] <- c(paste0(unique(data[,get(strata)])[i]),events,pt,rate,rate-1.96*(rate/sqrt(events)),rate+1.96*(rate/sqrt(events)))
    }
  }
  return(do.call(rbind,out))
}

# Usage
# af <- ir(data=mydata,time='af_time_variable',status='af_status',strata='groups')
