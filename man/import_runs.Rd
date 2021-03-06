% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importing.r
\name{import_runs}
\alias{import_runs}
\title{Import Running Data}
\usage{
import_runs(zipfile)
}
\arguments{
\item{zipfile}{Relative path to zipfile.}
}
\value{
Tibble with gps-position data (lat, lon, elevation), time_span, (cumulative)
    distance, time_span, duration, distace (cumulative).
}
\description{
Creates one tidy tibble from zipfile exported via
https://www.runtastic.com/de/benutzer/[...]/einstellungen/account-and-data.
All runs from ./Sport-sessions/GPS-data are imported via the parse_gps()
function.
}
\examples{
import_runs(zipfile = 'export-20200424-000.zip')
}
