% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_import_sensor.R
\name{import_sensor}
\alias{import_sensor}
\title{Imports data associated with a list of sensors}
\usage{
import_sensor(
  list_sensor,
  sensor_names = c("Burel-01", "Leclerc-02", "ParisMarche-03", "rueVignes-04",
    "ParisArcEnCiel-05", "RteVitre-06", "RueGdDomaine-07", "StDidierNord-08",
    "rueVallee-09", "StDidierSud-10", "RuePrieure-11", "RueCottage-12",
    "RueVeronniere-13", "RueDesEcoles-14", "RueManoirs-15", "RueToursCarree-16",
    "PlaceHotelDeVille-17", "BoulevardLiberte-18"),
  sensor_ids = c(9000002156, 9000001906, 9000001618, 9000003090, 9000002453, 9000001844,
    9000001877, 9000002666, 9000002181, 9000002707, 9000003703, 9000003746, 9000003775,
    9000003736, 9000004971, 9000004130, 9000004042, 9000004697)
)
}
\arguments{
\item{list_sensor}{A character vector specifying the names of sensors to import data for.}

\item{sensor_names}{A character vector containing the name of each sensor that is displayed to the user}

\item{sensor_ids}{A numeric vector containing the identifier name for each vector}
}
\value{
A data.frame containing the imported data.
}
\description{
Imports data associated with a given list of sensor names from .RData files contained in a data directory.
The main purpose of this function is to load the data saved with write update data.
}
