% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lorimerCZR.R
\name{lorimerCZR}
\alias{lorimerCZR}
\title{Lorimer's Competition Zone Radius - Lorimer 1983}
\usage{
lorimerCZR(k, n)
}
\arguments{
\item{k}{constant, usually 0.4}

\item{n}{number of trees per hectare}
}
\value{
value of competition zone radius
}
\description{
Lorimer's Competition Zone Radius - Lorimer 1983
}
\details{
Estimates the competition zone radius, based on the number of
trees per hectare in the plot multiplied by a constant (\eqn{k}).
}
\examples{
data(bicuar)
lorimerCZR(1, nrow(bicuar))

}
\references{
Lorimer, C. G. (1983). Tests of age-independent competition
indices for individual trees in natural hardwood stands. Forest Ecology and
Management. Volume 6. Pages 343-360.
}
