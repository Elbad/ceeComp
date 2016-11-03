#'
#' @title Computes net benefit values across a range of wilingness-to-pay thresholds
#' @description The function takes one dataset and a vector of wilingness-to-pay values.
#' The dataset MUST have 4 columns \code{ID}, \code{RESPONSE}, \code{COST} and \code{TREATMENT}
#' which hold respectively unique subject identifiers, treatment outcome, treatment cost
#' and 2 treatment arms names. If no values are for wilingness-to-pay 101 default values ranging
#' from 0 to 500,000 are used to compute net befit.
#' @param data a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @param will2pay a numeric vector of willingness-to-pay thresholds.
#' @param CI confidence interval.
#' @treatResponse a character, default is \code{beneficial} i.e. the treatment resulted in beneficial response;
#' otherwise \code{harmful}, the treatement resulted in harmful outcome
#' @details to be written
#' @return a table with for columns: net benefit, standard error, upper and lower limit
#' @export
#' @author Amadou Gaye & Felix Achana
#' @examples {
#' 
#' # load examples datasets
#' data(dataset1)
#' 
#' # nebefit computation using the default willingness-to-pay thresholds
#' NB <- netbenef(dataset1)
#' 
#' # display the top 5 records of the output table
#' NB[1:5,]
#' 
#' }
#'

netbenef <- function(data=NULL, will2pay=NULL, CI=0.95, treatResponse="beneficial"){
  
  # stop if two datasets are not provided
  if(is.null(data)){
    stop("Please provide a valid dataset to analyse!", call.=FALSE)
  }
  # warn user is only one dataset is provided 
  # we do not stop because the user might want compute ICER for only one dataset

  if(is.null(will2pay)){ 
    wtp <- c(0:100)*5000
    warning("No values provided for 'willingness-to-Pay'; 101 default values (ranging from 0 to 500000) will be used!", call.=FALSE)
  }else{
    wtp <-  will2pay
  }
  
  # check required information (colums) are supplied and compute 'incCost' and 'incOutcome'
  dt <- data
  trt <- unique(dt$TREATMENT)
  # stop if a table does not have the required
  # columns or if it does not have 2 treatment arms
  # or if that not have unique identifiers
  idx <- which(colnames(dt) %in% c("ID","RESPONSE","COST","TREATMENT"))
  if(length(idx) < 4){
    stop("The input dataset is missing required column(s)!", call.=FALSE)
  }else{
    if(length(trt) != 2){
      stop("Only 2 treatment arms are allowed. Please check input table(s)!", call.=FALSE)
    }
    if(length(unique(dt$ID)) < dim(dt)[1]){
      stop("Duplicated idenfiers are not allowed. Check column 'ID'!", call.=FALSE)
    }
  }
  
  # get mean and se for cost and outcome in both treament arms
  s <- getSummaries(dt)
  cost <- s$cost; outcome <- s$outcome
  idxA <- s$idxA; idxB <- s$idxB
  meanCostA <- s$meanCostA; meanCostB <- s$meanCostB
  meanOutcomeA <- s$meanOutcomeA; meanOutcomeB <- s$meanOutcomeB
  seCostA <- s$seCostA; seCostB <- s$seCostB
  seOutcomeA <- s$seOutcomeA; seOutcomeB <- s$seOutcomeB
  
  # difference between treatment arms
  if(treatResponse == 'beneficial'){
    incCost <- meanCostB - meanCostA
    incOutcome <- meanOutcomeB - meanOutcomeA
  }else{
    incCost <- meanCostA - meanCostB
    incOutcome <- meanOutcomeA - meanOutcomeB
  }
  
  # objects to hold various computed values
  NB <- rep(NA, length(wtp))
  seNB <- rep(NA, length(wtp))
  lclNB <- rep(NA, length(wtp))
  uclNB <- rep(NA, length(wtp))

  # compute net benefit values, standard error, low and upper limit
  for(i in 1:length(wtp)){
    NB[i] <- (wtp[i]*incOutcome) - incCost
    if(treatResponse == 'beneficial'){
      seNB[i] <- sqrt(wtp[i]*wtp[i]*(seOutcomeA^2 + seOutcomeB^2) + (seCostA^2 + seCostB^2) 
                      - 2*wtp[i]*(stats::cor(cost[idxA],outcome[idxA])*seOutcomeA*seCostA)
                      - 2*wtp[i]*(stats::cor(cost[idxB],outcome[idxB])*seOutcomeB*seCostB))
    }else{
      seNB[i] <- sqrt(wtp[i]*wtp[i]*(seOutcomeA^2 + seOutcomeB^2) + (seCostA^2 + seCostB^2) 
                      + 2*wtp[i]*(stats::cor(cost[idxA],outcome[idxA])*seOutcomeA*seCostA)
                      + 2*wtp[i]*(stats::cor(cost[idxB],outcome[idxB])*seOutcomeB*seCostB))
    }
    lclNB[i]    = NB[i] - 1.96*seNB[i]
    uclNB[i]    = NB[i] + 1.96*seNB[i]
  }

  # return a table with NB, seNB, lclNB and uclNB
  output <- cbind(NB, seNB, uclNB, lclNB)
  colnames(output) <- c("NetBenefit", "StandardError", "LowerLimit", "UpperLimit")
  return(output)
}
