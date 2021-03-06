#' Calculate GND test of calibration
#' @param pred predicted risk
#' @param tvar time variale
#' @param out outcome variable
#' @param cens.t variable denoting censoring (1 = censored, 0 = not censored)
#' @param groups quantiles for comparison (usually 10)
#' @param adm.cens administrative censoring time
#' @example
#' calib_charge <- GND.calib(pred=test$charge_pred5/100,tvar=test$af_5y.t, out=test$incd_af_5y,groups=test$pred_risk_quintile_clinical,cens.t=test$censored,adm.cens=5)
#' @export

GND_calib = function(pred, tvar, out, cens.t, groups, adm.cens){
  output <- list()
  
  tvar.t=ifelse(tvar>adm.cens, adm.cens, tvar)
  out.t=ifelse(tvar>adm.cens, 0, out)
  
  datause=data.frame(pred=pred, tvar=tvar.t, out=out.t, count=1, cens.t=cens.t, dec=groups)
  numcat=length(unique(datause$dec))
  groups=sort(unique(datause$dec))
  
  kmtab=matrix(unlist(lapply(groups,kmdec,"dec",datain=datause, adm.cens)),ncol=5, byrow=TRUE)
  
  if (any(kmtab[,5] == -1)) stop("Stopped because at least one of the groups contains <2 events. Consider collapsing some groups.")
  else if (any(kmtab[,5] == 1)) warning("At least one of the groups contains < 5 events. GND can become unstable.\ 
                                        (see Demler, Paynter, Cook 'Tests of Calibration and Goodness of Fit in the Survival Setting' DOI: 10.1002/sim.6428) \
                                        Consider collapsing some groups to avoid this problem.")
  
  hltab=data.frame(group=kmtab[,4],
                   totaln=tapply(datause$count,datause$dec,sum),
                   censn=tapply(datause$cens.t,datause$dec,sum),
                   numevents=tapply(datause$out,datause$dec,sum),
                   expected=tapply(datause$pred,datause$dec,sum),
                   kmperc=1-kmtab[,1], 
                   kmvar=kmtab[,2]^2, 
                   kmnrisk=kmtab[,3],
                   expectedperc=tapply(datause$pred,datause$dec,mean))
  
  hltab$kmnum=hltab$kmperc*hltab$totaln
  hltab$GND_component=ifelse(hltab$kmvar==0, 0,(hltab$kmperc-hltab$expectedperc)^2/(hltab$kmvar))
  
  print(hltab[c(1,2,3,4,10,5,6,9,7,11)], digits=4)
  output[[1]] <- hltab[c(1,2,3,4,10,5,6,9,7,11)]
  output[[2]] <- c(df=numcat-1, chi2gw=sum(hltab$GND_component),pvalgw=1-pchisq(sum(hltab$GND_component),numcat-1))
  return(output)
}

# calib_charge <- GND.calib(pred=test$charge_pred5/100,tvar=test$af_5y.t,
# out=test$incd_af_5y,groups=test$pred_risk_quintile_clinical,
# cens.t=test$censored,adm.cens=5)