% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linCC.R
\name{linCC}
\alias{linCC}
\title{Computes Lin's concordance correlation between 2 distinct datasets.}
\usage{
linCC(data1 = NULL, data2 = NULL, will2pay = NULL, ccThreshold = 0.4,
  CI = 0.95, treatResponse = "beneficial")
}
\arguments{
\item{data1}{a dataset with 4 columns holding respectively, ID, treatment outcome,
treatment cost and treatment arm.}

\item{data2}{a dataset with 4 columns holding respectively, ID, treatment outcome,
treatment cost and treatment arm.}

\item{will2pay}{a numeric vector of willingness-to-pay thresholds.}

\item{ccThreshold}{concordance correlation cut-off, default value is 0.40.}

\item{CI}{confidence interval.}

\item{treatResponse}{a character, default is \code{beneficial} i.e. the treatment resulted in beneficial response;
otherwise \code{harmful}, the treatement resulted in harmful outcome.}
}
\value{
a list which holds the below items:
\code{cccNB} Lin's concordance correlation between net benefits.
\code{secccNB} standard error of the concordance correlation.
\code{lclcccNB} lower limit of the concordance correlation.
\code{uclcccNB} upper limit of the concordance correlation.
\code{pvaluecccNB1} pvalue of the concordance correlation for the 1st dataset
\code{pvaluecccNB2} pvalue of the concordance correlation for the 2nd dataset
}
\description{
The function takes 2 datasets and a vector of wilingness-to-pay values.
The dataset MUST have 4 columns \code{ID}, \code{RESPONSE}, \code{COST} and \code{TREATMENT}
which hold respectively unique subject identifiers, treatment outcome, treatment cost
and 2 treatment arms names. If no values are for wilingness-to-pay, 101 default values ranging
from 0 to 500,000 are used to estimate the Lins's concordance correlation between the 2 datasets.
}
\details{
to be written
}
\examples{
{

# load examples datasets
data(dataset1); data(dataset2)

# compute concordance correlation using the default willingness-to-pay thresholds
results <- linCC(data1=dataset1, data2=dataset2, ccThreshold=0.40)

}

}
\author{
Amadou Gaye & Felix Achana
}

