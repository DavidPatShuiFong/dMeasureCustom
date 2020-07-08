# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' dMeasureCustom - custom module for dMeasure
#'
#'
#' @name custom
#' @title dMeasureCustom
#'
#' @include r6_helpers.R
#' functions to help create R6 classes
NULL

#' dMeasureCustom class
#' @title dMeasureCustom class
#' @description list appointments (custom) by clinician providers
#' @export
dMeasureCustom <- R6::R6Class(
  "dMeasureCustom",
  public = list(
    # dM is a dMeasure object
    dM = NULL, # pointer to dMeasure R6 object
    patientLists = NULL,
    # pointer to patientLists table in configuration database
    patientList = NULL,
    # the actual patient lists
    initialize = function(dMeasure_obj) {
      # dMeasure_obj is a R6 dMeasure object
      self$dM <- dMeasure_obj

      if (length(public_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(public_init_fields$name)) {
          if (public_init_fields$obj[[i]] == "dMeasureCustom") {
            self[[public_init_fields$name[[i]]]] <-
              eval(public_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }
      if (length(private_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(private_init_fields$name)) {
          if (private_init_fields$obj[[i]] == "dMeasureCustom") {
            private[[private_init_fields$name[[i]]]] <-
              eval(private_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }

      if (requireNamespace("shiny", quietly = TRUE)) {
        # set reactive version only if shiny is available
        # note that this is for reading (from programs calling this object) only!
        if (length(reactive_fields$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_fields$name)) {
            if (reactive_fields$obj[[i]] == "dMeasureCustom") {
              self[[reactive_fields$name[[i]]]] <- shiny::reactiveVal(
                eval(reactive_fields$value[[i]]) # could 'quote' the value
              )
            }
          }
        }
        if (length(reactive_event$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_event$name)) {
            if (reactive_event$obj[[i]] == "dMeasureCustom") {
              self[[reactive_event$name[[i]]]] <-
                eval(reactive_event$value[[i]]) # could 'quote' the value
            }
          }
        }
      }
    }
  )
  # this is a 'skeleton' class
  # it is filled in the with the '.public' function
)

##### special reactive functions ##########################

.private(dMeasureCustom, "set_reactive", function(myreactive, value) {
  # reactive (if shiny/reactive environment is available) is set to 'value'
  # myreactive is passed by reference
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(value)
  }
})
.private(dMeasureCustom, "trigger", function(myreactive) {
  # toggles a reactive between (usually) 0 and 1
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(1 - shiny::isolate(myreactive()))
  }
})
