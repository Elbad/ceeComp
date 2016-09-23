#'
#' @title Computes incremental cost-effectiveness ratio (ICER)
#' @description The function takes one dataset as input and the dataset
#' MUST have 4 columns \code{ID}, \code{RESPONSE}, \code{COST} and \code{TREATMENT}
#' which hold respectively unique subject identifiers, treatment outcome, treatment cost
#' and 2 treatment arms names.
#' @param data a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @details to be written
#' @return a list which holds to elements: ICER value and other statistics for the given dataset
#' @export
#' @author Amadou Gaye & Felix Achana
#' @examples {
#' 
#' # load examples datasets
#' data(dataset1)
#' 
#' # ICER computation 
#' output <- icer(dataset1)
#' 
#' # display the ICER value
#' names(output)
#' output$ICER
#'
#' # display the other stats 
#' output$Stats
#' }
#'
icer <- function(data=NULL){
  
  # stop if two datasets are not provided
  if(is.null(data)){
    stop("Please provide at a valid dataset to analyse!", call.=FALSE)
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
  meanCostA <- s$meanCostA; meanCostB <- s$meanCostB
  meanOutcomeA <- s$meanOutcomeA; meanOutcomeB <- s$meanOutcomeB
  seCostA <- s$seCostA; seCostB <- s$seCostB
  seOutcomeA <- s$seOutcomeA; seOutcomeB <- s$seOutcomeB
   
  # compute ICER
  incCost <- meanCostB - meanCostA
  incOutcome <- meanOutcomeA - meanOutcomeB
  ICER = incCost/incOutcome
  
  # format and return output
  output <- vector("list", 2); names(output) <- c("ICER","Stats")
  output[[1]] <- ICER
  dx <- cbind(c(meanCostA,meanCostB), c(meanOutcomeA,meanOutcomeA),
              c(seCostA,seCostB), c(seOutcomeA,seOutcomeA))
  colnames(dx) <- c("meanCost", "meanOutcome", "seCost", "seOutcome")
  rownames(dx) <- c(paste0("Treatment_",trt[1]), paste0("Treatment_",trt[2]))
  output[[2]] <- dx
  
  return(output)
}