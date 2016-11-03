#'
#' @title Computes the covariance between net benefit values from 2 distinct datasets.
#' @description The function takes 2 datasets and a vector of wilingness-to-pay values.
#' The dataset MUST have 4 columns \code{ID}, \code{RESPONSE}, \code{COST} and \code{TREATMENT}
#' which hold respectively unique subject identifiers, treatment outcome, treatment cost
#' and 2 treatment arms names. If no values are for wilingness-to-pay, 101 default values ranging
#' from 0 to 500,000 are used to compute net benefits and covariance between net benefits from the 2 datasets
#' @param data1 a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @param data2 a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @param will2pay a numeric vector of willingness-to-pay thresholds.
#' @param extraOutput a boolean set to FALSE by default, if set to TRUE other information are returned
#' in addition to \code{covNB}, the covariance between net benefits from the two datasets.
#' @treatResponse a character, default is \code{beneficial} i.e. the treatment resulted in beneficial response;
#' otherwise \code{harmful}, the treatement resulted in harmful outcome
#' @details to be written
#' @return a list which holds the below items; items other than \code{covNB} are optional, set the 
#' the parameter \code{extraOutput} to have those items returned:
#' \code{covNB} covariance between net benefits from the two datasets
#' \code{rhoNB} correlation between net benefits from the two datasets
#' \code{covCostA} covariance of costs between the two datasets for the 1st treatment arm
#' \code{covCostB} covariance of costs between the two datasets for the 2nd treatment arm
#' \code{covOutcomeA} covariance of outcomes between the two datasets for the 1st treatment arm
#' \code{covOutcomeB} covariance of outcomes between the two datasets for the 2nd treatment arm
#' @export
#' @author Amadou Gaye & Felix Achana
#' @examples {
#' 
#' # load examples datasets
#' data(dataset1); data(dataset2)
#' 
#' # covNB computation using the default willingness-to-pay thresholds
#' covValues <- covNB(data1=dataset1, data2=dataset2)
#' 
#' # covNB computation using the default willingness-to-pay thresholds 
#' #and request extra information.
#' covValues <- covNB(data1=dataset1, data2=dataset2, extraOutput=TRUE)
#' 
#' }
#'

covNB <- function(data1=NULL, data2=NULL, will2pay=NULL, extraOutput=FALSE, treatResponse="beneficial"){
  
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
  
  # check if datasets are complete, if not remove subject with missing data in both datasets and make sure 
  # both datasets have the same subjects and in the same order.

  # compute net benefit and generates summaries for both datasets
  nbRes <- vector("list", 2)
  stats <- vector("list", 2)
  for(i in 1:2){
    nbRes[[i]] <- netbenef(ldt[[i]], wtp)
    stats[[i]] <- getSummaries(ldt[[i]])
  }
  seNB1 <- nbRes[[1]][,"StandardError"]; seNB2 <- nbRes[[2]][,"StandardError"]
  idxA1 <- stats[[1]][["idxA"]]; idxA2 <- stats[[2]][["idxA"]]
  idxB1 <- stats[[1]][["idxB"]]; idxB2 <- stats[[2]][["idxB"]]
  cost1 <- stats[[1]][["cost"]]; cost2 <- stats[[2]][["cost"]]
  outcome1 <- stats[[1]][["outcome"]]; outcome2 <- stats[[2]][["outcome"]]
  seCostA1 <- stats[[1]][["seCostA"]]; seCostA2 <- stats[[2]][["seCostA"]]
  seCostB1 <- stats[[1]][["seCostB"]]; seCostB2 <- stats[[2]][["seCostB"]]
  seOutcomeA1 <- stats[[1]][["seOutcomeA"]]; seOutcomeA2 <- stats[[2]][["seOutcomeA"]]
  seOutcomeB1 <- stats[[1]][["seOutcomeB"]]; seOutcomeB2 <- stats[[2]][["seOutcomeB"]]
  
  # compute covariance between net benefits obtained above and other statistics
  ndt <- length(ldt)
  covOutcomeA <- vector("list",length(wtp));
  covOutcomeB <- vector("list",length(wtp))
  covCostOutcomeA1 <- vector("list",length(wtp));  covCostOutcomeA2 <- vector("list",length(wtp))
  covCostOutcomeB1 <- vector("list",length(wtp));  covCostOutcomeB2 <- vector("list",length(wtp))
  covDeltaOutcome <- vector("list",length(wtp));  
  covDeltaCostOutcome1 <- vector("list",length(wtp)); covDeltaCostOutcome2 <- vector("list",length(wtp))
  covNB <- vector("list",length(wtp))
  rhoNB <- vector("list",length(wtp))
  
  # get covariance between cost for the 2 treatment arms
  # and then compute covNB for each willingness-to-pay value
  covCostA <- (stats::cov(cbind(cost1[idxA1], cost2[idxA2]))/length(cost2[idxA2]))[1,2]
  covCostB <- (stats::cov(cbind(cost1[idxB1], cost2[idxB2]))/length(cost2[idxB2]))[1,2]
  covDeltaCost <- covCostA + covCostB
  for(i in 1:length(wtp)){
    covOutcomeA[[i]] <- ((wtp[i]^2) * stats::cov(cbind(outcome1[idxA1], outcome2[idxA2]))/length(cost2[idxA2]))[1,2]
    covOutcomeB[[i]] <- ((wtp[i]^2) * stats::cov(cbind(outcome1[idxB1], outcome2[idxB2]))/length(cost2[idxA2]))[1,2] 
    covDeltaOutcome[[i]] <- covOutcomeA[[i]] + covOutcomeB[[i]] 
    
    covCostOutcomeA1[[i]] <- (wtp[i] * stats::cov(cbind(cost1[idxA1], outcome2[idxA2]))/length(cost2[idxA2]))[1,2]
    covCostOutcomeB1[[i]] <- (wtp[i] * stats::cov(cbind(cost1[idxB1], outcome2[idxB2]))/length(cost2[idxA2]))[1,2]
    
    covCostOutcomeA2[[i]] <- (wtp[i] * stats::cov(cbind(cost2[idxA2], outcome1[idxA1]))/length(cost2[idxA2]))[1,2]
    covCostOutcomeB2[[i]] <- (wtp[i] * stats::cov(cbind(cost2[idxB2], outcome1[idxB1]))/length(cost2[idxA2]))[1,2]
    
    if(treatResponse == 'beneficial'){
      covDeltaCostOutcome1[[i]] <- (covCostOutcomeA1[[i]] + covCostOutcomeB1[[i]])
      covDeltaCostOutcome2[[i]] <- (covCostOutcomeA2[[i]] + covCostOutcomeB2[[i]])
    }else{
      covDeltaCostOutcome1[[i]] <- -(covCostOutcomeA1[[i]] + covCostOutcomeB1[[i]])
      covDeltaCostOutcome2[[i]] <- -(covCostOutcomeA2[[i]] + covCostOutcomeB2[[i]])
    }
    
    covNB[[i]] <- (covDeltaOutcome[[i]] - covDeltaCostOutcome1[[i]] - covDeltaCostOutcome2[[i]] + covDeltaCost)
    rhoNB[[i]] <- covNB[[i]] / (seNB1[i] * seNB2[i]) 
  }

  # return a list with 'covNB' or more depending on what the user specified
  if(extraOutput){
    output <- list("covNB"=covNB, "rhoNB"=rhoNB, "covCostA"=covCostA, "covCostB"=covCostB, 
                   "covOutcomeA"=covOutcomeA, "covOutcomeB"=covOutcomeB)
  }else{
    output <- list("covNB"=covNB)
  }
  return(output)
}
