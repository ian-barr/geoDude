#' GMEAN
#'
#' This function calculates Geometric Standard Deviation from a set of data
#' @param x Data points, as a vector-like object
#' @keywords GMEAN
#' @return GMEAN
#' @export
#' @examples
#' GMEAN()
GMEAN <- function(x){
  if (length(x) > 1){
    gmean <- exp(mean(log(x)))
  } else {
    print( "Need 2 or more data points")
  }
  return(gmean)
}

#' GSD
#'
#' This function calculates Geometric Standard Deviation from a set of data
#' @param x Data points, as a vector-like object
#' @keywords GSD
#' @return GSD
#' @export
#' @examples
#' GSD()
GSD <- function(x){
  if (length(x) > 1){
    gsd <- exp(sd(log(x)))
  } else {
    print( "Need 2 or more data points")
  }
  return(gsd)
}

#' GSEM
#'
#' This function calculates Geometric SEM from a set of data
#' @param x Data points, as a vector-like object
#' @keywords GSEM
#' @return GSEM
#' @export
#' @examples
#' GSEM()
GSEM <- function(x){
  l <- length(x)
  if (l > 1){
    gsem <- exp(sd(log(x))/sqrt(l))
  } else {
    print( "Need 2 or more data points")
  }
  return(gsem)
}

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
  return ( exp (sqrt(  log1p(GCV^2)) ))
}

#' kGCV2GSD
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
  return ( exp (  log1p(GCV) ))
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
  return ( exp(sqrt(  log1p(GCV^2)) / sqrt(df) ))
}

#' GCV2GCI
#'
#' This function converts Geometric Coefficient of Variation to Geometric SEM
#' @param GCV Geometric Coefficient of Variation
#' @param alpha Type I error rate (default 0.05 for 95\% CI)
#' @param gmn geometric mean of the samples
#' @param df degrees of freedom (or number of data points)
#' @param percent Is the GCV expressed as a percent? Defaults to FALSE.
#' @keywords GCV, GCI
#' @return GSEM
#' @export
#' @examples
#' GCV2GCI()
GCV2GCI <- function(GCV,df,gmn,alpha = 0.05,percent = FALSE){
  if(percent){GCV <- GCV/100.}
  lgsem <- sqrt( log1p(GCV^2)) / sqrt(df)
  qs <- abs(qt(alpha/2. , df=df,lower.tail = TRUE))
  CI <- lgsem * c(-qs,qs)
  return(exp(log(gmn) + CI))
}

#' GCI2GCV
#'
#' This function converts Confidence Interval to Geometric Coeficient of Variation
#' @param GCI Geometric Confidence interval
#' @param alpha Type I error rate (0.05 for 95\% CI)
#' @param gm geometric mean of the samples
#' @param df degrees of freedom (or number of data points)
#' @param percent Is the GCV to be expressed as a percent? Defaults to FALSE.
#' @keywords GCI, GCV
#' @return GCV
#' @export
#' @examples
#' GCI2GCV()
GCI2GCV <- function(GCI,df,gmn,alpha=0.05, percent = FALSE){
  f <- ifelse(percent, 100.0, 1.0)
  CIp <- max(GCI)/gmn
  CIn <- gmn / min(GCI)
  qp <- qt(1.0 - alpha/2. , df=df,lower.tail = TRUE)
  qn <- qt(alpha/2. , df=df,lower.tail = TRUE)
  sdp <- sqrt(df)*log(CIp) / qp
  sdn <- sqrt(df)*log(CIn) / qn
  return(  f*c( sqrt(expm1(sdn^2)), sqrt(expm1(sdp^2)) ))
}
