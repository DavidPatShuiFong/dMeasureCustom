# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Interface elements of dMeasureCustom
#'
#' requires R6 methods from Custom.R
#'
#' @include Custom.R
NULL

###########################################################

#' item description for left sidebar menu
#'
#' @name shinydashboardmenuItem
#'
#' @return shinydashboard menuItem object
#'
#' @export
shinydashboardmenuItem <- function() {
  x <- list(
    shinydashboard::menuItem(
      "Custom",
      tabName = "custom",
      icon = shiny::icon("cocktail")
    )
  )
  return(x)
}

#' center panel description
#'
#' @name dMeasureShinytabItems
#'
#' @return shinytabItems
#'
#' @export
dMeasureShinytabItems <- function() {
  x <- list(
    shinydashboard::tabItem(
      tabName = "custom",
      shiny::fluidRow(shiny::column(
        width = 12, align = "center",
        shiny::h2("Custom")
      )),
      shiny::fluidRow(shiny::column(
        width = 12,
        dMeasureCustom::datatableUI("Custom_dt")
      ))
    )
  )
  return(x)
}

#' Custom module - configuration tabpanel item
#'
#' @details
#'   This function is called by `DailyMeasure`
#'
#' @return tabPanel
#'
#' @export
dMeasureConfigurationTabPanelItem <- function() {
  x <- list(
    shiny::tabPanel(
      title = "Custom patient lists",
      value = "CustomPatientLists",
      shiny::column(
        width = 12,
        dMeasureCustom::dMeasureConfigurationTabPanelUI(
          "dMeasureCustom_config_dt"
        )
      )
    )
  )
  return(x)
}

#' Custom module - configuration panel UI
#'
#' @name dMeasureConfigurationTabPanelUI
#'
#' @details
#'   This function is called by `dMeasureConfigurationTabPanelItem`,
#'   with the same module `id` as `DailyMeasure` will
#'   call `dMeasureConfigurationTabPanel`.
#'
#'   The module `id` is also returned by the integration function
#'   `dMeasureIntegration` when called with parameter `configID`
#'
#' @param id module ID
#'
#' @return shiny user interface element
#'
#' @export
dMeasureConfigurationTabPanelUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      DTedit::dteditmodUI(ns("customPatientLists"))
    ),
    shiny::br(),
    shiny::uiOutput(ns("viewedListName")),
    shiny::fluidRow(
      DT::dataTableOutput(ns("showSpreadsheet"))
    )
  )
}

#' Custom module - configuration panel server
#'
#' @name dMeasureConfigurationTabPanel
#'
#' @details
#'   This (module) server is called by `DailyMeasure` with the
#'   same module `id` as `dMeasureConfigurationTabPanelItem` will
#'   call `dMeasureConfigurationTabPanelUI`
#'
#'   The module `id` is also returned by the integration function
#'   `dMeasureIntegration` when called with the parameter `configID`
#'
#' @param id as required by Shiny modules
#' @param dMCustom dMeasureCustom R6 object
#'
#' @return none
#'
#' @export
dMeasureConfigurationTabPanel <- function(id, dMCustom) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    viewedList <- shiny::reactiveVal(NULL)
    viewedListName <- shiny::reactiveVal("")
    output$viewedListName <- shiny::renderUI({
      shiny::h4(paste("Custom Patient List :", viewedListName()))
    })
    # the currently viewed list. columns are ID and Label
    patientList.callback.actionButton <- function(data, row, buttonID) {
      # data - the current copy of 'thedata'
      # row - the row number of the clicked button
      # buttonID - the buttonID of the clicked button

      if (substr(buttonID, 1, nchar("view")) == "view") {
        viewedList(unserialize(data[row, "patientList"][[1]]))
        # this will set viewedList to a dataframe of ID and Label
        viewedListName(data[row, "Name"])
      }
    }

    spreadsheet <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      viewedList(),
      ignoreNULL = TRUE, {
        intID <- c(-1, viewedList()$ID)
        spreadsheet(
          viewedList() %>>%
            dplyr::left_join(
              dMCustom$dM$db$patients %>>%
                dplyr::filter(InternalID %in% intID) %>>%
                dplyr::select(InternalID, Firstname, Surname, DOB),
              by = c("ID" = "InternalID"), copy = TRUE
            ) %>>%
            dplyr::mutate(DOB = as.Date(DOB))
        )
      }
    )
    output$showSpreadsheet <- DT::renderDT({
      DailyMeasure::datatable_styled(spreadsheet())
    })

    patientList.callback.insert <- function(data, row) {
      outfile <- tempfile(fileext = ".csv")
      # create temporary file name
      if (data[row, "Name"][[1]] %in% data[-row, ]$Name) {
        stop(paste("Can't use the same name as other lists!"))
      }
      if (data[row, "Name"][[1]] == "") {
        stop(paste("Name cannot be empty!"))
      }

      zz <- file(outfile, "wb") # create temporary file
      writeBin(object = unlist(data[row, "patientList"]), con = zz)
      # currently "patientList" column contains a binary blob of a file
      close(zz) # outputs the inserted CSV into a temporary file

      newID <- dMCustom$write_patientList(data[row, "Name"][[1]], outfile)
      # write_patientList also sets dMCustom$patientList
      data <- dMCustom$patientList # read the database back in

      # cleanup (remove the temporary file)
      file.remove(outfile)

      return(data)
    }

    patientList.callback.update <- function(data, olddata, row) {
      outfile <- tempfile(fileext = ".csv")
      # create temporary file name
      if (data[row, "Name"][[1]] %in% data[-row, ]$Name) {
        stop(paste("Can't use the same name as other lists!"))
      }
      if (data[row, "Name"][[1]] == "") {
        stop(paste("Name cannot be empty!"))
      }

      zz <- file(outfile, "wb") # create temporary file
      tryCatch(
        {
          x <- unserialize(unlist(data[row, "patientList"]))
          # can only be unserialized if this is a serialized object
          # however, if this is a *new* CSV file, then the object
          #  is actually a CSV file, and 'unserialize' will throw an error
          # "patientList" is a serialized object if, for example, only
          #  the 'Name' is changed, but no new CSV file was imported
          write.csv(x, file = zz)
        },
        error = function(e) {
          # the data is not a serialized object,
          # so this is a CSV file
          writeBin(object = unlist(data[row, "patientList"]), con = zz)
          # "patientList" column contains a binary blob of a CSV file
        }
      )
      close(zz) # outputs the inserted CSV into a temporary file

      tryCatch(
        result <- dMCustom$update_patientList(
          name = data[row, "Name"][[1]],
          # name might have been changed!
          filename = outfile,
          ID = data[row, "id"]
        ),
        error = function(e) stop(e)
      )

      data <- dMCustom$patientList # read the database back in

      # cleanup (remove the temporary file)
      file.remove(outfile)

      return(data)
    }

    patientList.callback.delete <- function(data, row) {
      dMCustom$remove_patientList(data[row, "Name"][[1]])
      data <- dMCustom$patientList # read the database back in

      return(data)
    }

    shiny::observeEvent(
      dMCustom$patientListNamesR(),
      ignoreNULL = TRUE, once = TRUE, {
        shiny::callModule(
          DTedit::dteditmod,
          "customPatientLists",
          thedata = dMCustom$patientList,
          view.cols = c("id", "Name"),
          edit.cols = c("Name", "patientList"),
          edit.label.cols = c("Name of List", "Patient List (.csv), must have 'ID' and 'Label' columns"),
          input.choices = list(patientList = ".csv"),
          show.copy = FALSE,
          action.buttons = list(
            viewlist = list(
              columnLabel = "View Patient List",
              buttonLabel = "View",
              buttonPrefix = "view"
            )
          ),
          callback.actionButton = patientList.callback.actionButton,
          callback.insert = patientList.callback.insert,
          callback.delete = patientList.callback.delete,
          callback.update = patientList.callback.update
        )
      }
    )
  })
}


#' Custom module - UI function
#'
#' Display appointments within selected range of dates and providers
#'
#' @name datatableUI
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
      shiny::column(
        4,
        shinyWidgets::switchInput(
          inputId = ns("printcopy_view"),
          label = paste(
            "<i class=\"fas fa-print\"></i>",
            "<i class=\"far fa-copy\"></i>",
            "  Print and Copy View"
          ),
          labelWidth = "12em",
          width = "20em"
        )
      ),
      shiny::column(
        2,
        offset = 3,
        shiny::uiOutput(ns("patientListNames"))
      )
    ),
    DT::DTOutput(ns("custom_table"))
  )
}

.reactive(dMeasureCustom, "printcopy_view", TRUE)

#' Custom module - server
#'
#' @name datatableServer
#'
#' @param id id
#' @param dMCustom dMeasureCustom R6 object
#'
#' @return none
#'
#' @export
datatableServer <- function(id, dMCustom) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$patientListNames <- shiny::renderUI({
      if (is.null(dMCustom$patientListNamesR())) {
        shinyWidgets::dropdown(
          inputId = ns("choice_dropdown"),
          "No patient lists defined",
          icon = shiny::icon("gear"),
          label = "Patient lists"
        )
      } else {
        shinyWidgets::dropdown(
          inputId = ns("choice_dropdown"),
          icon = shiny::icon("gear"),
          label = "Patient lists",
          shiny::actionButton(
            inputId = ns("view_patientLists"),
            label = "Change patient lists"
          )
        )
      }
    })
    shiny::observeEvent(
      input$view_patientLists,
      ignoreInit = TRUE, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Patient lists",
            shinyWidgets::checkboxGroupButtons(
              inputId = ns("patientList_chosen"),
              label = "Patient lists shown",
              choices = dMCustom$patientListNamesR(),
              selected = dMCustom$chosen_patientList,
              status = "primary",
              checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
            ),
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("patientLists_ok"), "OK")
            )
          )
        )

      }
    )
    shiny::observeEvent(
      input$patientLists_ok, {
        dMCustom$chosen_patientList <- input$patientList_chosen
        shiny::removeModal()
      }
    )

    shiny::observeEvent(input$printcopy_view, ignoreNULL = TRUE, {
      dMCustom$printcopy_view(input$printcopy_view)
    })

    styled_custom_list <- shiny::reactive({
      shiny::validate(
        shiny::need(
          dMCustom$dM$appointments_filtered_timeR(),
          "No appointments in selected range"
        )
      )
      if (input$printcopy_view == TRUE) {
        DailyMeasure::datatable_styled(
          dMCustom$appointments_patientListR()
        )
      } else {
        escape_column <- which(
          names(dMCustom$appointments_patientListR()) == "List"
        )
        DailyMeasure::datatable_styled(
          dMCustom$appointments_patientListR(),
          escape = c(escape_column),
          copyHtml5 = NULL, printButton = NULL,
          downloadButton = NULL # no copy/print buttons
        )
      }
    })

    output$custom_table <- DT::renderDT({
      styled_custom_list()
    })
  })
}
