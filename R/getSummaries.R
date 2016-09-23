#'
#' @title Generates various information required within different functions
#' @description This is an internal function that takes as input a dataset with
#' 4 columns \code{ID}, \code{RESPONSE}, \code{COST} and \code{TREATMENT} and extracts and generates
#' a list with the required information.
#' @param data a dataset with 4 columns holding respectively, ID, treatment outcome,
#'        treatment cost and treatment arm.
#' @details to be written
#' @return a list with the following items:
#' \code{idxA} indices of the observation in the first treatment arm
#' \code{idxB} indices of the observation in the second treatment arm
#' \code{cost} cost values
#' \code{meanCostA} mean cost for the first treatment arm
#' \code{meanCostB} mean cost for the second treatment arm
#' \code{meanOutcomeA} mean outome for the first treatment arm
#' \code{meanOutcomeB} mean outome for the second treatment arm
#' \code{seCostA} standard error of the cost of the first treatment arm
#' \code{seCostB} standard error of the cost of the second treatment arm
#' \code{seOutcomeA} standard error of the outcome of the first treatment arm
#' \code{seOutcomeB} standard error of the outcome of the second treatment arm
#' @keywords internal
#' @author Amadou Gaye & Felix Achana
#'
getSummaries <- function(dt){
  trt <- unique(dt$TREATMENT)
  idxA <- which(dt$TREATMENT==trt[1]); idxB <- which(dt$TREATMENT==trt[2])
  cost <- as.numeric(as.character(dt$COST)); outcome <- as.numeric(as.character(dt$RESPONSE))
  meanCostA <- mean(cost[idxA],na.rm=T)
  meanCostB <- mean(cost[idxB],na.rm=T)
  meanOutcomeA <- mean(outcome[idxA],na.rm=T)
  meanOutcomeB <- mean(outcome[idxB],na.rm=T)
  seCostA <- sqrt(stats::var(cost[idxA],na.rm=T) / length(cost[idxA]))
  seCostB <- sqrt(stats::var(cost[idxB],na.rm=T) / length(cost[idxB]))
  seOutcomeA <- sqrt(stats::var(outcome[idxA],na.rm=T) / length(outcome[idxA]))
  seOutcomeB <- sqrt(stats::var(outcome[idxB],na.rm=T) / length(outcome[idxB]))
  
  output <- list(idxA, idxB, cost, outcome, meanCostA, meanCostB, meanOutcomeA,
                 meanOutcomeB, seCostA, seCostB, seOutcomeA, seOutcomeB)
  names(output) <- c("idxA", "idxB", "cost", "outcome", "meanCostA", "meanCostB", "meanOutcomeA",
                 "meanOutcomeB", "seCostA", "seCostB", "seOutcomeA", "seOutcomeB")
  return(output)
}
