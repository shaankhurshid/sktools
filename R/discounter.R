# Function to perform annunity based discounting
discounter <- function(time=time, rate=0.03){
  out <- rep(NA,times=time)
  for(i in 1:time){
    out[i] <- 1 + 1*((1-(1+rate)**-(i-1))/rate)
  }
output <- data.frame(time=1:time,discounted_value=out)
print(paste('Discounted using rate of',as.character(rate)))
return(output)
}
