% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm_betaselect.R
\name{lm_betaselect}
\alias{lm_betaselect}
\alias{glm_betaselect}
\alias{print.lm_betaselect}
\alias{print.glm_betaselect}
\alias{raw_output}
\title{Standardize Coefficients in a
Regression Model}
\usage{
lm_betaselect(
  ...,
  to_standardize = NULL,
  not_to_standardize = NULL,
  do_boot = TRUE,
  bootstrap = 100L,
  iseed = NULL,
  parallel = FALSE,
  ncpus = parallel::detectCores(logical = FALSE) - 1,
  progress = TRUE,
  load_balancing = TRUE,
  model_call = c("lm", "glm")
)

glm_betaselect(
  ...,
  to_standardize = NULL,
  not_to_standardize = NULL,
  do_boot = TRUE,
  bootstrap = 100L,
  iseed = NULL,
  parallel = FALSE,
  ncpus = parallel::detectCores(logical = FALSE) - 1,
  progress = TRUE,
  load_balancing = TRUE
)

\method{print}{lm_betaselect}(
  x,
  digits = max(3L, getOption("digits") - 3L),
  type = c("beta", "standardized", "raw", "unstandardized"),
  ...
)

\method{print}{glm_betaselect}(
  x,
  digits = max(3L, getOption("digits") - 3L),
  type = c("beta", "standardized", "raw", "unstandardized"),
  ...
)

raw_output(x)
}
\arguments{
\item{...}{For \code{\link[=lm_betaselect]{lm_betaselect()}}.
these arguments will be
passed directly to \code{\link[=lm]{lm()}}. For
\code{\link[=glm_betaselect]{glm_betaselect()}}, these arguments
will be passed to \code{\link[=glm]{glm()}}.
For
the \code{print}-method of \code{lm_betaselect}
or \code{glm_betaselect}
objects, this will be passed to
other methods.}

\item{to_standardize}{A string vector,
which should be the names of the
variables to be standardized.
Default is \code{NULL}, indicating all
variables are to be standardized
(but see \code{skip_categorical}).}

\item{not_to_standardize}{A string
vector, which should be the names
of the variables that should \emph{not} be
standardized. This argument is useful
when most variables, except for a few,
are to be standardized. This argument
cannot be ued with \code{to_standardize}
at the same time. Default is \code{NULL},
and only \code{to_standardize} is used.}

\item{do_boot}{Whether bootstrapping
will be conducted. Default is \code{TRUE}.}

\item{bootstrap}{If \code{do_boot} is
\code{TRUE}, this argument is the number
of bootstrap samples to draw. Default
is 100. Should be set to 5000 or even
10000 for stable results.}

\item{iseed}{If \code{do_boot} is \code{TRUE}
and this argument is not \code{NULL},
it will be used by \code{\link[=set.seed]{set.seed()}} to
set the seed for the random number
generator. Default is \code{NULL}.}

\item{parallel}{If \code{do_boot} is
\code{TRUE} and this argument is \code{TRUE},
parallel processing will be used to
do bootstrapping. Default is \code{FALSE}
because bootstrapping for models fitted
by \code{\link[=lm]{lm()}} or \code{\link[=glm]{glm()}} is rarely slow.}

\item{ncpus}{If \code{do_boot} is \code{TRUE}
and \code{parallel} is also \code{TRUE}, this
argument is the number of processes
to be used in parallel processing.
Default
is \code{parallel::detectCores(logical = FALSE) - 1}}

\item{progress}{Logical. If \code{TRUE},
progress bars will be displayed
for long process.}

\item{load_balancing}{Logical. If
\code{parallel} is \code{TRUE}, this determine
whether load balancing will be used.
Default is \code{TRUE}.}

\item{model_call}{The model function
to be called.
If \code{"lm"}, the default, the model will be fitted
by \code{\link[stats:lm]{stats::lm()}}. If \code{"glm"}, the
model will be fitted by \code{\link[stats:glm]{stats::glm()}}.
Users should call the corresponding
function directly rather than setting
this argument manually.}

\item{x}{An \code{lm_betaselect} or
\code{glm_betaselect} object.}

\item{digits}{The number of significant
digits to be printed for the
coefficients.}

\item{type}{The coefficients to be
printed. For \code{"beta"} or
\code{"standardized"}, the coefficients
after selected variables standardized
will be printed. For \code{"raw"} or
\code{"unstandardized"}, the coefficients
before standardization was done will
be printed.}
}
\value{
The function \code{\link[=lm_betaselect]{lm_betaselect()}}
returns an object of the class \code{lm_betaselect},
The function \code{\link[=glm_betaselect]{glm_betaselect()}}
returns an object of the class
\code{glm_betaselect}. They are similar
in structure to the output of
\code{\link[stats:lm]{stats::lm()}} and \code{\link[stats:glm]{stats::glm()}},
with additional information stored.

The function \code{\link[=raw_output]{raw_output()}} returns
an object of the class \code{lm} or
\code{glm}, which are
the results of fitting the model
to the data by \code{\link[stats:lm]{stats::lm()}}
or \code{\link[stats:glm]{stats::glm()}} without
standardization.
}
\description{
Can fit a linear regression
models with selected variables standardized;
handle product terms correctly and
skip categorical predictors in
standardization.
}
\details{
The functions \code{\link[=lm_betaselect]{lm_betaselect()}}
and \code{\link[=glm_betaselect]{glm_betaselect()}}
let users
select which variables to be
standardized when computing the
standardized solution. They have the
following features:
\itemize{
\item They automatically skip categorical
predictors (i.e., factor or string
variables).
\item They do not standardize a product
term, which is incorrect. Instead,
they
compute the product term with its
component variables standardized,
if requested.
\item They standardize the selected
variables \emph{before} fitting a model.
Therefore, If a model has the term
\code{log(x)} and \code{x} is one of the
selected variables, the model used
the logarithm of the \emph{standardized}
\code{x} in the model, instead of
standardized \code{log(x)} which is
difficult to interpret.
\item They can be used to generate
nonparametric
bootstrap confidence intervals for
the standardized solution. Bootstrap
confidence interval is better than
the default confidence interval
ignoring the standardization
because it
takes into account the sampling
variance of the standard deviations.
Preliminary support for bootstrap
confidence has been found
for forming confidence intervals for
coefficients involving standardized
variables in linear regression
(Jones & Waller, 2013).
}
\subsection{Problems With Common Approaches}{

In some regression programs, users
have limited control on which
variables to standardize when
requesting the so-called "betas".
The solution may be uninterpretable
or misleading in these conditions:
\itemize{
\item Dummy variables are standardized
and cannot be interpreted as the
difference between two groups on the
outcome variables.
\item Product terms (interaction terms)
are standardized and they cannot be
interpreted as the changes in the
effects of focal variables when the
moderators change (Cheung, Cheung,
Lau, Hui, & Vong, 2022).
\item Variables with meaningful units can
be more difficult to interpret when
they are standardized (e.g., age).
}
}

\subsection{How The Function Work}{

They standardize the original variables
\emph{before} they are used in the
model. Therefore, strictly
speaking, they do not standardize
the predictors in model,
but standardize the \emph{input variable}
(Gelman et al., 2021).

The requested model is then fitted to
the dataset with selected variables
standardized. For the ease of
follow-up analysis, both the results
with selected variables standardized
and the results without
standardization are stored. If
required, the results without
standardization can be retrieved
by \code{\link[=raw_output]{raw_output()}}.
}

\subsection{Methods}{

The output of \code{\link[=lm_betaselect]{lm_betaselect()}} is
an \code{lm_betaselect}-class object,
and the output of \code{\link[=glm_betaselect]{glm_betaselect()}}
is a \code{glm_betaselect}-class object.
They have the following methods:
\itemize{
\item A \code{coef}-method for extracting
the coefficients of the model.
(See \code{\link[=coef.lm_betaselect]{coef.lm_betaselect()}}
and \code{\link[=coef.glm_betaselect]{coef.glm_betaselect()}}
for details.)
\item A \code{vcov}-method for extracting the
variance-covariance matrix of the
estimates of the coefficients.
If bootstrapping is requested, it
can return the matrix based on the
bootstrapping estimates.
(See \code{\link[=vcov.lm_betaselect]{vcov.lm_betaselect()}}
and \code{\link[=vcov.glm_betaselect]{vcov.glm_betaselect()}}
for details.)
\item A \code{confint}-method for forming the
confidence intervals of the
estimates of the coefficients.
If bootstrapping is requested, it
can return the bootstrap confidence
intervals.
(See \code{\link[=confint.lm_betaselect]{confint.lm_betaselect()}} and
\code{\link[=confint.glm_betaselect]{confint.glm_betaselect()}}
for details.)
\item A \code{summary}-method for printing the
summary of the results, with additional
information such as the number of
bootstrap samples and which variables
have been standardized.
(See \code{\link[=summary.lm_betaselect]{summary.lm_betaselect()}} and
\code{\link[=summary.glm_betaselect]{summary.glm_betaselect()}}
for details.)
\item An \code{anova}-method for printing the
ANOVA table. Can also be used to
compare two or more outputs of
\code{\link[=lm_betaselect]{lm_betaselect()}} or
\code{\link[=glm_betaselect]{glm_betaselect()}}
(See \code{\link[=anova.glm_betaselect]{anova.glm_betaselect()}}
and \code{\link[=anova.glm_betaselect]{anova.glm_betaselect()}}
for details.)
\item A \code{predict}-method for computing
predicted values. It can be used to
compute the predicted values given
a set of new unstandardized data.
The data will be standardized before
computing the predicted values in
the models with standardization.
(See \code{\link[=predict.lm_betaselect]{predict.lm_betaselect()}} and
\code{\link[=predict.glm_betaselect]{predict.glm_betaselect()}}
for details.)
\item The default \code{update}-method for updating
a call also works for an
\code{lm_betaselect} object or
a \code{glm_betaselect()} object. It can
update the model in the same
way it updates a model fitted by
\code{\link[stats:lm]{stats::lm()}} or \code{\link[stats:glm]{stats::glm()}},
and also update
the arguments of \code{\link[=lm_betaselect]{lm_betaselect()}}
or \code{\link[=glm_betaselect]{glm_betaselect()}}
such as the variables to be
standardized.
(See \code{\link[stats:update]{stats::update()}} for details.)
}

Most other methods for the output
of \code{\link[stats:lm]{stats::lm()}} and \code{\link[stats:glm]{stats::glm()}}
should also work
on an \code{lm_betaselect}-class object
or a \code{glm_betaselect}-class object,
respectively.
Some of them will give the same
results regardless of the variables
standardized. Examples are
\code{\link[=rstandard]{rstandard()}} and \code{\link[=cooks.distance]{cooks.distance()}}.
For some others, they should be used
with cautions if they make use of
the variance-covariance matrix
of the estimates.

To use the methods for \code{lm} objects
or \code{glm} objects
on the results without standardization,
simply use \code{\link[=raw_output]{raw_output()}}. For example,
to get the fitted values without
standardization, call
\code{fitted(raw_output(x))}, where \code{x}
is the output of \code{\link[=lm_betaselect]{lm_betaselect()}}
or \code{\link[=glm_betaselect]{glm_betaselect()}}.
}

The function \code{\link[=raw_output]{raw_output()}} simply extracts
the regression output by \code{\link[stats:lm]{stats::lm()}}
or \code{\link[stats:glm]{stats::glm()}}
on the variables without standardization.
}
\examples{

data(data_test_mod_cat)

# Standardize only iv

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
                           data = data_test_mod_cat,
                           to_standardize = "iv")
lm_beta_x
summary(lm_beta_x)

# Manually standardize iv and call lm()

data_test_mod_cat$iv_z <- scale(data_test_mod_cat[, "iv"])[, 1]

lm_beta_x_manual <- lm(dv ~ iv_z*mod + cov1 + cat1,
                       data = data_test_mod_cat)

coef(lm_beta_x)
coef(lm_beta_x_manual)

# Standardize all numeric variables

lm_beta_all <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
                             data = data_test_mod_cat)
# Note that cat1 is not standardized
summary(lm_beta_all)


data(data_test_mod_cat)

data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
                              yes = 1,
                              no = 0)
# Standardize only iv
logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
                                  family = binomial,
                                  data = data_test_mod_cat,
                                  to_standardize = "iv")
summary(logistic_beta_x)

logistic_beta_x
summary(logistic_beta_x)

# Manually standardize iv and call glm()

data_test_mod_cat$iv_z <- scale(data_test_mod_cat[, "iv"])[, 1]

logistic_beta_x_manual <- glm(p ~ iv_z*mod + cov1 + cat1,
                              family = binomial,
                              data = data_test_mod_cat)

coef(logistic_beta_x)
coef(logistic_beta_x_manual)

# Standardize all numeric predictors

logistic_beta_allx <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
                                     family = binomial,
                                     data = data_test_mod_cat,
                                     to_standardize = c("iv", "mod", "cov1"))
# Note that cat1 is not standardized
summary(logistic_beta_allx)


summary(raw_output(lm_beta_x))

}
\references{
Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
(2022) Improving an old way to measure moderation effect in standardized
units. \emph{Health Psychology}, \emph{41}(7), 502-505.
\doi{10.1037/hea0001188}

Craig, C. C. (1936). On the frequency function of xy.
\emph{The Annals of Mathematical Statistics, 7}(1),
1--15. \doi{10.1214/aoms/1177732541}

Gelman, A., Hill, J., & Vehtari, A. (2021).
\emph{Regression and other stories}.
Cambridge University Press.
\doi{10.1017/9781139161879}

Jones, J. A., & Waller, N. G. (2013). Computing confidence
intervals for standardized regression coefficients.
\emph{Psychological Methods, 18}(4), 435--453.
\doi{10.1037/a0033269}
}
\seealso{
\code{\link[=print.lm_betaselect]{print.lm_betaselect()}} and
\code{\link[=print.glm_betaselect]{print.glm_betaselect()}} for the
\code{print}-methods.
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
