#' Changes selected columns to date format by reference using data.table
#' @param cols columns to change
#' @param format date format
#' @param data a data table
#' @param trunc OPTIONAL: if string needs to be truncated first, start and end points
#' @import data.table
#' @example
#' calib_charge <- GND.calib(pred=test$charge_pred5/100,tvar=test$af_5y.t, out=test$incd_af_5y,groups=test$pred_risk_quintile_clinical,cens.t=test$censored,adm.cens=5)
#' @export

format_date = function(data, cols, format='%Y-%m-%d', trunc=NULL){
  if (length(trunc)==0){
  for (j in cols){set(data,j=j,value=as.Date(data[[j]],format=format))}
  } else {
    for (j in cols){set(data,j=j,value=as.Date(substr(data[[j]],trunc[1],trunc[2]),format=format))}
  }
}

# calib_charge <- GND.calib(pred=test$charge_pred5/100,tvar=test$af_5y.t,
# out=test$incd_af_5y,groups=test$pred_risk_quintile_clinical,
# cens.t=test$censored,adm.cens=5)