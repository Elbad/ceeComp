#'
#' @title Computes Lin's concordance correlation between 2 distinct datasets.
#' @description The function takes 2 datasets and a vector of wilingness-to-pay values.
#' The dataset MUST have 4 columns \code{ID}, \code{RESPONSE}, \code{COST} and \code{TREATMENT}
#' which hold respectively unique subject identifiers, treatment outcome, treatment cost
#' and 2 treatment arms names. If no values are for wilingness-to-pay, 101 default values ranging
#' from 0 to 500,000 are used to estimate the Lins's concordance correlation between the 2 datasets.
#' @param data1 a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @param data2 a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @param will2pay a numeric vector of willingness-to-pay thresholds.
#' @param ccThreshold concordance correlation cut-off, default value is 0.40.
#' @details to be written
#' @return a list which holds the below items:
#' \code{cccNB} Lin's concordance correlation between net benefits.
#' \code{secccNB} standard error of the concordance correlation.
#' \code{lclcccNB} lower limit of the concordance correlation.
#' \code{uclcccNB} upper limit of the concordance correlation.
#' \code{pvaluecccNB1} pvalue of the concordance correlation for the 1st dataset
#' \code{pvaluecccNB2} pvalue of the concordance correlation for the 2nd dataset
#' @export
#' @author Amadou Gaye & Felix Achana
#' @examples {
#' 
#' # load examples datasets
#' data(dataset1); data(dataset2)
#' 
#' # compute concordance correlation using the default willingness-to-pay thresholds
#' results <- linCC(data1=dataset1, data2=dataset2, ccThreshold=0.40)
#' 
#' }
#'
linCC <- function(data1=NULL, data2=NULL, will2pay=NULL, ccThreshold=0.40){
  
  # stop if two datasets are not provided
  if(is.null(data1) | is.null(data2)){
    stop("Please provide 2 valid datasets!", call.=FALSE)
  }
  
  # warn user if willingness to pay threshold not provided
  if(is.null(will2pay)){ 
    wtp <- c(0:100)*5000
    warning("No values provided for 'willingness-to-Pay'; 101 default values (ranging from 0 to 500000) will be used!", call.=FALSE)
  }else{
    wtp <-  will2pay
  }
  
  # check required information (colums) are supplied in both datasets
  ldt <- list(data1, data2)
  for(i in 1:2){
    dt <- ldt[[i]]
    trt <- unique(dt$TREATMENT)
    # stop if a table does not have the required
    # columns or if it does not have 2 treatment arms
    # or if that not have unique identifiers
    idx <- which(colnames(dt) %in% c("ID","RESPONSE","COST","TREATMENT"))
    if(length(idx) < 4){
      stop(paste0("Dataset ", i, " is missing required column(s)!", call.=FALSE))
    }else{
      if(length(trt) != 2){
        stop(paste0("Error in dataset ", i," : only 2 treatment arms are allowed. Please check the table!", call.=FALSE))
      }
      if(length(unique(dt$ID)) < dim(dt)[1]){
        stop(paste0("Error in dataset ", i," :duplicated idenfiers are not allowed. Check column 'ID'!", call.=FALSE))
      }
    }
  }
  
  # generate information required for Lin's concordance correlation calculations:
  # 'rhoNB', 'seNB', 'diffNB' etc...
  nbRes <- vector("list", 2)
  for(i in 1:2){
    nbRes[[i]] <- netbenef(ldt[[i]], wtp)
  }
  diffRes <- diffNB(data1=ldt[[1]], data2=ldt[[2]],  will2pay=wtp)
  covRes <- covNB(data1=ldt[[1]], data2=ldt[[2]], will2pay=wtp, extraOutput=TRUE)

  seNB1 <- nbRes[[1]][,"StandardError"]; seNB2 <- nbRes[[2]][,"StandardError"]
  diff <- diffRes$diffNB
  rhoNB <- covRes$rhoNB
  
  # vector the old estimated values
  cbNB <- vector("numeric", length(wtp))
  cccNB <- vector("numeric", length(wtp))
  mucccNB <- vector("numeric", length(wtp))
  zcccNB <- vector("numeric", length(wtp))
  secccNB  <- vector("numeric", length(wtp))
  lclcccNB <- vector("numeric", length(wtp))
  uclcccNB <- vector("numeric", length(wtp))
  dLoss <- vector("numeric", length(wtp))
  ccc02 <- vector("numeric", length(wtp))
  pvaluecccNB1 <- vector("numeric", length(wtp))
  pvaluecccNB2 <- vector("numeric", length(wtp))
  
  # compute the above values
  pow <- function(a,b){a^b}
  for(i in 1:length(wtp)){
    mucccNB[i] <- abs(diff[i])/sqrt(seNB1[i]^2 * seNB2[i]^2)
    cbNB[i] <- (2*seNB1[i]*seNB2[i]) / (diff[i]^2 + seNB1[i]^2 + seNB2[i]^2)
    cccNB[i] <- rhoNB[i]*cbNB[i]
    zcccNB[i] <- 0.5*log((1+cccNB[i])/(1-cccNB[i]))
    secccNB[i] <- sqrt((1/((dim(data1)[1]/2)-2)) * (1-pow(rhoNB[i],2)) * pow(cccNB[i],2)/(1-pow(cccNB[i],2)) * pow(rhoNB[i],2) + 
                         (2*pow(cccNB[i],3) * (1-cccNB[i]) * pow(mucccNB[i],2)/(rhoNB[i]*(1-pow(cccNB[i],2))^2))
                       -(pow(cccNB[i],4)*pow(mucccNB[i],4)/(2*pow(rhoNB[i],2)*(1-pow(cccNB[i],2))^2)))
    
    lclcccNB[i] <- (exp(2*(zcccNB[i]- 1.96*secccNB[i]))-1)/(exp(2*(zcccNB[i]- 1.96*secccNB[i]))+1)
    uclcccNB[i] <- (exp(2*(zcccNB[i]+ 1.96*secccNB[i]))-1)/(exp(2*(zcccNB[i]+ 1.96*secccNB[i]))+1)
    
    # test hypothesis that cccNB > the concordance correlation cut-off
    ccc01 <- ccThreshold
    pvaluecccNB1[i] <- pnorm(-abs(zcccNB[i]-0.5*log((1+ccc01)/(1-ccc01)))/secccNB[i])
    dLoss[i] <- (0.15*rhoNB[i])^2
    ccc02[i] <- cbNB[i]*sqrt(rhoNB[i]^2-dLoss[i])
    pvaluecccNB2[i] = pnorm(-abs(zcccNB[i]-0.5*log((1+ccc02[i] )/(1-ccc02[i] )))/secccNB[i])
  }
  
  # return output
  output <- list(cccNB, secccNB, lclcccNB, uclcccNB, pvaluecccNB1, pvaluecccNB2)
  names(output) <- c("cccNB", "secccNB", "lclcccNB", "uclcccNB", "pvaluecccNB1", "pvaluecccNB2")
  return(output)
    
}









