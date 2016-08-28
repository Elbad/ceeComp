#'
#' @title Computes incremental cost-effectiveness ratio (ICER)
#' @description The function takes at least one dataset as input and each dataset
#' MUST have 4 columns \code{ID}, \code{RESPONSE}, \code{COST} and \code{TREATMENT}
#' which hold respectively unique subject identifiers, treatment outcome, treatment cost
#' and 2 treatment arms names.
#' @param data1 a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @param data1 a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @return a list of 2 lists which hold ICER value and other statistics for each dataset
#' @author Amadou Gaye & Felix Achana
#'
icer <- function(data1=NULL, data2=NULL){
  
  # stop if two datasets are not provided
  if(is.null(data1) & is.null(data2)){
    stop("Please provide at least 1 valid dataset to analyse!", call.=FALSE)
  }
  # warn user is only one dataset is provided 
  # we do not stop because the user might want compute ICER for only one dataset
  if(is.null(data1) | is.null(data2)){
    warning("You provided only 1 dataset, ICER will be computed only for 1 dataset!", call.=FALSE)
  }
  
  # loop through the datasets and compute ICER, mean and SE
  # for ease we keep the 2 datasets in a list and loop through that list
  ldt <- list(data1,data2)
  myresults <- vector("list", 2); names(myresults) <- c("DATASET1", "DATASET2")
  for(i in 1:2){
    output <- vector("list", 2)
    dt <- ldt[[i]]
    if(!is.null(dt)){
      trt <<- unique(dt$TREATMENT)
      # stop if a table does not have the required
      # columns or if it does not have 2 treatment arms
      # or if that not have unique identifiers
      idx <- which(colnames(dt) %in% c("ID","RESPONSE","COST","TREATMENT"))
      if(length(idx) < 4){
        stop("One or more input dataset is missing required column(s)!", call.=FALSE)
      }else{
        if(length(trt) != 2 ){
          stop("Only 2 treatment arms are allowed. Please check input table(s)!", call.=FALSE)
        }
        if(length(unique(dt$ID)) < dim(dt)[1]){
          stop("Duplicated idenfiers are not allowed. Check column 'ID'!", call.=FALSE)
        }
      }
      idxA <- which(dt$TREATMENT==trt[1]); idxB <- which(dt$TREATMENT==trt[2])
      cost <- as.numeric(as.character(dt$COST)); outcome <- as.numeric(as.character(dt$RESPONSE))
      meanCostsA <- mean(cost[idxA],na.rm=T)
      meanCostsB <- mean(cost[idxB],na.rm=T)
      meanOutcomeA <- mean(outcome[idxA],na.rm=T)
      meanOutcomeB <- mean(outcome[idxB],na.rm=T)
      seCostsA <- sqrt(var(cost[idxA],na.rm=T) / length(cost[idxA]))
      seCostsB <- sqrt(var(cost[idxB],na.rm=T) / length(cost[idxB]))
      seOutcomeA <- sqrt(var(outcome[idxA],na.rm=T) / length(outcome[idxA]))
      seOutcomeB <- sqrt(var(outcome[idxB],na.rm=T) / length(outcome[idxB]))
      
      # difference between treatment arms
      incCosts     = meanCostsB-meanCostsA
      incOutcome   = meanOutcomeA-meanOutcomeB
      ICER         = incCosts/incOutcome 
      output[[1]] <- ICER
      dx <- cbind(c(meanCostsA,meanCostsB), c(meanOutcomeA,meanOutcomeA),
                            c(seCostsA,seCostsB), c(seOutcomeA,seOutcomeA))
      colnames(dx) <- c("meanCost", "meanOutcome", "seCost", "seOutcome")
      rownames(dx) <- c(paste0("Treatment_",trt[1]), paste0("Treatment_",trt[2]))
      output[[2]] <- dx
    }else{
      output[[1]] <- NA; output[[2]] <- NA
    }
    names(output) <- c("ICER","Stats")
    myresults[[i]] <- output
  }
  return(myresults)
}