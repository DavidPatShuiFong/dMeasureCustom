% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{appointments_patientList}
\alias{appointments_patientList}
\title{patient appointment list combined with custom patient list labels}
\usage{
appointments_patientList(
  dMeasureCustom_obj,
  chosen_patientList = NA,
  screentag = FALSE,
  screentag_print = TRUE
)
}
\arguments{
\item{dMeasureCustom_obj}{R6 object}

\item{chosen_patientList}{names of chosen custom patient lists}

\item{screentag}{add HTML fomantic/semantic tags}

\item{screentag_print}{add 'printable' plaintext tags}
}
\value{
dataframe of apppointments
$Patient, $AppointmentDate, $AppointmentTime,
$Provider, $Status, $Label
}
\description{
derived from dM$appointments_filtered_time
}
