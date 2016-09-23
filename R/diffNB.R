#'
#' @title Computes difference between net benefits from 2 distinct datasets.
#' @description The function takes 2 datasets and a vector of wilingness-to-pay values.
#' The dataset MUST have 4 columns \code{ID}, \code{RESPONSE}, \code{COST} and \code{TREATMENT}
#' which hold respectively unique subject identifiers, treatment outcome, treatment cost
#' and 2 treatment arms names. If no values are for wilingness-to-pay, 101 default values ranging
#' from 0 to 500,000 are used to estimate the difference between net benefits from the 2 datasets
#' @param data1 a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @param data2 a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @param will2pay a numeric vector of willingness-to-pay thresholds.
#' @details to be written
#' @return a list which holds the below items; items other than \code{covNB} are optional, set the 
#' the parameter \code{extraOutput} to have those items returned:
#' \code{diffNB} to be written
#' \code{sediffNB} to be written
#' \code{lcldiffNB} to be written
#' \code{ucldiffNB} to be written
#' \code{pvaluediffNB} to be written
#' @export
#' @author Amadou Gaye & Felix Achana
#' @examples {
#' 
#' # load examples datasets
#' data(dataset1); data(dataset2)
#' 
#' # diffNB computation using the default willingness-to-pay thresholds
#' results <- diffNB(data1=dataset1, data2=dataset2)
#' 
#' }
#'

diffNB <- function(data1=NULL, data2=NULL, will2pay=NULL){
  
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
  
  # compute net benefit and covariance between net benefits
  nbRes <- vector("list", 2)
  for(i in 1:2){
    nbRes[[i]] <- netbenef(ldt[[i]], wtp)
  }
  NB1 <- nbRes[[1]][,"NetBenefit"]; NB2 <- nbRes[[2]][,"NetBenefit"]
  seNB1 <- nbRes[[1]][,"StandardError"]; seNB2 <- nbRes[[2]][,"StandardError"]
  covValue <- covNB(ldt[[1]], ldt[[2]], will2pay=wtp)
  
  
  # vectors to hold estimated values
  diffNB <- vector("numeric", length(wtp))
  sediffNB <- vector("numeric", length(wtp))
  lcldiffNB <- vector("numeric", length(wtp))
  ucldiffNB <- vector("numeric", length(wtp))
  pvaluediffNB <- vector("numeric", length(wtp))
  
  for(i in 1:length(wtp)){
    diffNB[i] <- NB1[i] - NB2[i]  
    sediffNB[i] <- round(sqrt(seNB1[i]^2 + seNB2[i]^2 - (2*covValue[[1]][[i]])),4)
    lcldiffNB[i] <- diffNB[i] - 1.96*sediffNB[i]
    ucldiffNB[i] <- diffNB[i] + 1.96*sediffNB[i]  
    pvaluediffNB[i] <- 2*pnorm(-abs(diffNB[i])/sediffNB[i])
  }
  
  output <- list(diffNB, sediffNB, lcldiffNB, ucldiffNB, pvaluediffNB)
  names(output) <- c("diffNB", "sediffNB", "lcldiffNB", "ucldiffNB", "pvaluediffNB")
  return(output)
  
}

  










