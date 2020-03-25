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
dMeasureCustom <- R6::R6Class("dMeasureCustom",
                              public = list(
                                # dM is a dMeasure object
                                dM = NULL,
                                patient_list = NULL,
                                initialize = function (dMeasure_obj) {
                                  # dMeasure_obj is a R6 dMeasure object
                                  self$dM <- dMeasure_obj
                                  self$patient_list <- read.csv(system.file("202003AsthmaCOPD_Phase1.csv",
                                                                            package = "dMeasureCustom"),
                                                                stringsAsFactors = FALSE)
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

###########################################################

#' item description for left sidebar menu
#'
#' @name shinydashboardmenuItem
#'
#' @return shinydashboard menuItem object
#'
#' @export
shinydashboardmenuItem <- function() {
  return(shinydashboard::menuItem("Custom",
                                  tabName = "custom", icon = shiny::icon("cocktail")))
}

#' center panel description
#'
#' @name dMeasureShinytabItems
#'
#' @return shinytabItems
#'
#' @export
dMeasureShinytabItems <- function() {

  x <- list(shinydashboard::tabItem(
    tabName = "custom",
    shiny::fluidRow(column(width = 12, align = "center",
                           h2("Custom"))),
    shiny::fluidRow(column(width = 12,
                           dMeasureCustom::datatableUI("custom_dt")))
  ))
  return(x)
}

#' Custom module - UI function
#'
#' Display appointments within selected range of dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
#'
#' @export
datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
                    shinyWidgets::switchInput(
                      inputId = ns("printcopy_view"),
                      label = paste("<i class=\"fas fa-print\"></i>",
                                    "<i class=\"far fa-copy\"></i>",
                                    "  Print and Copy View"),
                      labelWidth = "12em",
                      width = "20em")
      )
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("custom_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL)
  )
}

#' Custom module - server
#'
#' @name custom_dataatable
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMCustom dMeasureCustom R6 object
#'
#' @return none
#'
#' @export
datatableServer <- function(input, output, session, dMCustom) {

  ns <- session$ns

  styled_custom_list <- shiny::reactive({
    shiny::validate(
      shiny::need(dMCustom$dM$appointments_filtered_timeR(),
                  "No appointments in selected range")
    )
    datatable_styled(dMCustom$dM$appointments_filtered_timeR() %>>%
                       dplyr::filter(InternalID %in% dMCustom$patient_list$ID) %>>%
                       dplyr::left_join(dMCustom$patient_list,
                                        by = c("InternalID" = "ID")) %>>%
                       dplyr::select(Patient, AppointmentDate, AppointmentTime,
                                     Provider, Status, Disease))
  })

  output$custom_table <- DT::renderDT({
    styled_custom_list()
  })

}
