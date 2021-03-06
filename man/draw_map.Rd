% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.r
\name{draw_map}
\alias{draw_map}
\title{Map running data using leaflet}
\usage{
draw_map(runs, runId)
}
\arguments{
\item{runs}{tibble}

\item{runId}{Vector of runId, runs is filtered by. Returned runs are mapped afterwards}
}
\value{
leaflet object
}
\description{
Filter running tibble and map runs.
}
\examples{
run_map(runs_tibble, 2)
}
