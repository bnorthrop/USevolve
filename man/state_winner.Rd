% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state_winner.R
\name{state_winner}
\alias{state_winner}
\title{State Winner}
\usage{
state_winner(Year = 2016, states = c(), label = "candidate")
}
\arguments{
\item{Year}{United States Presidential Election year starting in 1976.}

\item{states}{region to plot.}

\item{label}{legend labels.}
}
\value{
a ggplot map object.
}
\description{
state_winner displays a chloropleth map of United States presidential election
results by party on the state level for a given year.
}
\examples{
state_winner(Year = 1988)
state_winner(states=c("california", "nevada", "arizona", "utah"))
}
\seealso{
\code{\link[=county_winner]{county_winner()}}
}
