#' GCV2GSD
#'
#' This function converts Geometric Coefficient of Variation to Geometric SD
#' @param GCV Geometric Coefficient of Variation
#' @param percent Is the GCV expressed as a percent? Defaults to FALSE.
#' @keywords GCV, GSD
#' @return GSD
#' @export
#' @examples
#' GCV2GSD()
GCV2GSD <- function(GCV,percent=FALSE){
  if(percent){GCV <- GCV/100.}
  return ( exp (sqrt(  log((GCV)^2 + 1.0)) ))
}

#' GCV2GSD
#'
#' This function converts Geometric Coefficient of Variation to Geometric SD,
#' using the formula of Kirkwood (1982)
#' @param GCV Geometric Coefficient of Variation
#' @param percent Is the GCV expressed as a percent? Defaults to FALSE.
#' @keywords GCV, GSD
#' @return GSD
#' @export
#' @examples
#' kGCV2GSD()
kGCV2GSD <- function(GCV,percent=FALSE){ ## kirkwood version
  if(percent){GCV <- GCV/100.}
  return ( exp (  log((GCV) + 1.0) ))
}

#' GSD2GSEM
#'
#' This function converts Geometric Standard Deviation to Geometric SEM
#' @param GSD Geometric Standard Deviation
#' @param df degrees of freedom (or number of data points)
#' @keywords GSD, GSEM
#' @return GSEM
#' @export
#' @examples
#' GSD2GSEM()
GSD2GSEM <- function(GSD,df){
  return ( exp(log(GSD)/sqrt(df)))
}
#' GCV2GSEM
#'
#' This function converts Geometric Coefficient of Variation to Geometric SEM
#' @param GCV Geometric Coefficient of Variation
#' @param df degrees of freedom (or number of data points)
#' @param percent Is the GCV expressed as a percent? Defaults to FALSE.
#' @keywords GCV, GSEM
#' @return GSEM
#' @export
#' @examples
#' GCV2GSEM()
GCV2GSEM <- function(GCV,df,percent=FALSE){
  if(percent){GCV <- GCV/100.}
  return ( exp(sqrt(  log((GCV)^2 + 1.0)) / sqrt(df) ))
}
#' GCV2CI
#'
#' This function converts Geometric Coefficient of Variation to Geometric SEM
#' @param GCV Geometric Coefficient of Variation
#' @param alpha Type I error rate (0.05 for 95% CI)
#' @param m mean of the log-transformed samples
#' @param df degrees of freedom (or number of data points)
#' @param percent Is the GCV expressed as a percent? Defaults to FALSE.
#' @keywords GCV, GSEM
#' @return GSEM
#' @export
#' @examples
#' GCV2CI()
GCV2CI <- function(GCV,df,m,alpha,percent = FALSE){
  if(percent){GCV <- GCV/100.}
  gsem <- exp(sqrt(  log((GCV)^2 + 1.0)) / sqrt(df))
  qs <- qt(alpha/2. , df=df,lower.tail = TRUE)
  CI <- gsem * c(qs,-qs)
  return(exp(log(m) - CI))
}

#' CI2GCV
#'
#' This function converts Confidence Interval to Geometric Coeficient of Variation
#' @param CI Confidence interval
#' @param alpha Type I error rate (0.05 for 95% CI)
#' @param m mean of the log-transformed samples
#' @param df degrees of freedom (or number of data points)
#' @param percent Is the GCV to be expressed as a percent? Defaults to FALSE.
#' @keywords CI, GCV
#' @return GCV
#' @export
#' @examples
#' CI2GCV()
CI2GCV <- function(CI,df,m,alpha,percent = FALSE){
  ifelse(percent, f = 100.0,f=1.0)
  CIp <- max(CI)/m
  CIn <- m / min(CI)
  qp <- qt(1.0 - alpha/2. , df=df,lower.tail = TRUE)
  qn <- qt(alpha/2. , df=df,lower.tail = TRUE)
  sdp <- sqrt(df)*log(CIp) / qp
  sdn <- sqrt(df)*log(CIn) / qn
  return(  f*c( sqrt(exp(sdn^2)-1.0), sqrt(exp(sdp^2)-1.0) ))
}
