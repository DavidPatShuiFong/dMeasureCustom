# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

##### functions to fill in R6 classes ######################
# first argument is the object generator of the class

.public <- function(obj, ...) obj$set("public", ...)
.private <- function(obj, ...) obj$set("private", ...)
.active <- function(obj, ...) obj$set("active", ...)

public_init_fields <- list(obj = NULL, name = NULL, value = NULL)
.public_init <- function(obj, name, value) {
  obj$set("public", name, NULL)
  public_init_fields$obj <<- c(public_init_fields$obj, deparse(substitute(obj)))
  public_init_fields$name <<- c(public_init_fields$name, name)
  public_init_fields$value <<- c(public_init_fields$value, value)
  # to be initialized during self$initialize
  # e.g. references another field's value
  # $value will be 'eval()' during initialization, so can be quote()'d
}

private_init_fields <- list(obj = NULL, name = NULL, value = NULL)
.private_init <- function(obj, name, value) {
  obj$set("private", name, NULL)
  private_init_fields$obj <<- c(private_init_fields$obj, deparse(substitute(obj)))
  private_init_fields$name <<- c(private_init_fields$name, name)
  private_init_fields$value <<- c(private_init_fields$value, value)
  # to be initialized during self$initialize
  # e.g. references another field's value
  # $value will be 'eval()' during initialization, so can be quote()'d
}

reactive_fields <- list(obj = NULL, name = NULL, value = NULL)
# this will progressively hold definitions of reactive fields
.reactive <- function(obj, name, value) {
  if (requireNamespace("shiny", quietly = TRUE)) {
    # only if reactive environment is possible (using shiny)
    obj$set("public", name, NULL)
    reactive_fields$obj <<- c(reactive_fields$obj, deparse(substitute(obj)))
    reactive_fields$name <<- c(reactive_fields$name, name)
    reactive_fields$value <<- c(reactive_fields$value, list(value))
    # $value is listed so it can hold NULL!
    # to be initialized as reactiveVal during self$initialize
    # $value will be 'eval()' during initialization, so can be quote()'d
  }
}

reactive_event <- list(obj = NULL, name = NULL, value = NULL)
# this will progressively hold definitions of reactive fields
.reactive_event <- function(obj, name, value) {
  if (requireNamespace("shiny", quietly = TRUE)) {
    # only if reactive environment is possible (using shiny)
    obj$set("public", name, NULL)
    reactive_event$obj <<- c(reactive_event$obj, deparse(substitute(obj)))
    reactive_event$name <<- c(reactive_event$name, name)
    reactive_event$value <<- c(reactive_event$value, list(value))
    # $value is listed so it can hold NULL!
    # to be initialized as reactiveVal during self$initialize
    # $value will be 'eval()' during initialization, so can be quote()'d
  }
}
