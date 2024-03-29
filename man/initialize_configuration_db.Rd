% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{initialize_configuration_db}
\alias{initialize_configuration_db}
\title{initialize the configuration database}
\usage{
initialize_configuration_db(config_db)
}
\arguments{
\item{config_db}{pointer to configuration database}
}
\description{
create, or expand, configuration database
}
\details{
Creates, if necessary, the configuration files

  If necessary, also adds additional columns to existing
  configuration tables
}
