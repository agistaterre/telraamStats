% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_api_state.R
\name{api_state}
\alias{api_state}
\title{Check API State}
\usage{
api_state(key = get_telraam_token())
}
\arguments{
\item{key}{the api key (set by the set_telraam_token function)}
}
\value{
TRUE if the API responds well, FALSE otherwise
}
\description{
Return the state of he Telraam API. Determine if updates can be made.
}
