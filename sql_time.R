# This function strips time from SQL dates and changes the variable to POSIX time
# Requires data.table and replaces data.table object by reference

sql_time <- function(name, data, width=10, format='%Y-%m-%d'){
  for (j in (name)){set(data,j=j,value=strtrim(data[[j]],width=width))}
  for (j in (name)){set(data,j=j,value=as.Date(data[[j]],format=format))}
}


