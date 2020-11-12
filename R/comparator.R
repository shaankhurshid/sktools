# This function computes the cumulative sum of the difference between the next row and the current row

comparator <- function(x){
  out <- rep(NA,times=length(x))
  for (i in 1:length(x)){
    if (i==1) {
      out[i] <- as.numeric(x[i+1] - x[i])
    }
    else if (i<length(x)) {
      out[i] <- as.numeric(out[i-1] + as.numeric(x[i+1] - x[i]))
    } else {
      out[length(x)] <- out[length(x)-1]
    }}
  return(out)}
