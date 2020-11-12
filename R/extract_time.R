#' Friendly conversion from date/time format into date 
#' Dataset must be a data.table. If given a data.frame, will quit.
#' @export

extract_time <- function(name, data, width=10, format='%Y-%m-%d'){
  if (data.table::is.data.table(data)==FALSE){print('Error: data must be a data.table')
  } else {
  for (j in (name)){set(data,j=j,value=strtrim(data[[j]],width=width))}
  for (j in (name)){set(data,j=j,value=as.Date(data[[j]],format=format))}
}}


