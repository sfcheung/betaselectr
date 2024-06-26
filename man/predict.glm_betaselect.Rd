% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm_betaselect_methods.R
\name{predict.glm_betaselect}
\alias{predict.glm_betaselect}
\title{Predict Method for a 'glm_betaselect' Object}
\usage{
\method{predict}{glm_betaselect}(
  object,
  model_type = c("beta", "standardized", "raw", "unstandardized"),
  newdata,
  ...
)
}
\arguments{
\item{object}{A \code{glm_betaselect}-class
object.}

\item{model_type}{The model from which the
the predicted values are computed.
For
\code{"beta"} or \code{"standardized"}, the
model is the one after selected
variables standardized. For \code{"raw"}
or \code{"unstandardized"}, the model is
the one before standardization was
done.}

\item{newdata}{If set to a data
frame, the predicted values are
computed using this data frame.
The data must be unstandardized.
That is, the variables are of the
same units as in the data frame
used in \code{\link[=glm_betaselect]{glm_betaselect()}}. If
\code{model_type} is \code{"beta"} or
\code{"standardized"}, it will be
standardized using the setting
of \code{to_standardize} when \code{object}
is created in \code{\link[=glm_betaselect]{glm_betaselect()}}.}

\item{...}{Arguments
to be passed to \code{\link[stats:predict.glm]{stats::predict.glm()}}.
Please refer to the help page of
\code{\link[stats:predict.glm]{stats::predict.glm()}}.}
}
\value{
It returns the output of \code{\link[stats:predict.glm]{stats::predict.glm()}}.
}
\description{
Compute the predicted
values in a model fitted by
\code{\link[=glm_betaselect]{glm_betaselect()}}.
}
\details{
It simply passes the model \emph{before}
or \emph{after} selected variables
are standardized to the
\code{predict}-method of a \code{glm} object.
\subsection{IMPORTANT}{

Some statistics, such as prediction
or confidence interval, which make use
of the sampling variances and
covariances of coefficient estimates
\emph{may} not be applicable to the
models with one or more variables
standardized. Therefore, they should
only be used for exploratory purpose.
}
}
\examples{

data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
                              yes = 1,
                              no = 0)
logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
                                  data = data_test_mod_cat,
                                  family = binomial,
                                  to_standardize = "iv")

predict(logistic_beta_x)
predict(logistic_beta_x, model_type = "raw")

}
\seealso{
\code{\link[=glm_betaselect]{glm_betaselect()}} and \code{\link[stats:predict.glm]{stats::predict.glm()}}
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
