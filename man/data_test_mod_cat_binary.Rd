% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_test.R
\docType{data}
\name{data_test_mod_cat_binary}
\alias{data_test_mod_cat_binary}
\title{Test Dataset with a Binary Outcome Variable}
\format{
A data frame with 300 rows
and five variables:
\describe{
\item{dv}{Dependent variable, binary: 0, 1}
\item{iv}{Independent variable, continuous}
\item{mod}{Moderator, continuous}
\item{cov1}{Control variable, continuous}
\item{cat1}{String variable with these values: "gp1", "gp2", and "gp3"}
}
}
\usage{
data_test_mod_cat_binary
}
\description{
This dataset has one
predictor, one moderator, one
control variable, one binary dependent
variable, and a categorical variable.
}
\examples{

glm_out <- glm(dv ~ iv * mod + cov1 + cat1, data_test_mod_cat_binary, family = binomial())
summary(glm_out)


}
\keyword{datasets}
