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
    initialize = function (dMeasure_obj) {
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

###########################################################

#' item description for left sidebar menu
#'
#' @name shinydashboardmenuItem
#'
#' @return shinydashboard menuItem object
#'
#' @export
shinydashboardmenuItem <- function() {
  return(shinydashboard::menuItem(
    "Custom",
    tabName = "custom", icon = shiny::icon("cocktail"))
  )
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

#' Custom module - configuration tabpanel item
#'
#' @return tabPanel
#'
#' @export
dMeasureConfigurationTabPanelItem <- function() {
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
}

#' Custom module - configuration panel
#'
#' @name dMeasureConfigurationTabPanelUI
#'
#' @param id module ID
#'
#' @return shiny user interface element
#'
#' @export
dMeasureConfigurationTabPanelUI <- function(id) {
  ns <- shiny::NS(id)

}

#' Custom module - server
#'
#' @name dMeasureConfigurationTabPanel
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMCustom dMeasureCustom R6 object
#'
#' @return none
#'
#' @export
dMeasureConfigurationTabPanel <- function(input, output, session, dMCustom) {

  ns <- session$ns

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
      shiny::column(4,
                    shinyWidgets::switchInput(
                      inputId = ns("printcopy_view"),
                      label = paste("<i class=\"fas fa-print\"></i>",
                                    "<i class=\"far fa-copy\"></i>",
                                    "  Print and Copy View"),
                      labelWidth = "12em",
                      width = "20em")
      ),
      shiny::column(2,
        offset = 3,
        shiny::uiOutput(ns("patientListNames")))
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
#' @name datatableServer
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

  output$patientListNames <- shiny::renderUI({
    if (is.null(dMCustom$patientListNamesR())) {
      shinyWidgets::dropdown(
        inputId = ns("choice_dropdown"),
        "No patient lists defined",
        icon = icon("gear"),
        label = "Patient lists"
      )
    } else {
      shinyWidgets::dropdown(
        inputId = ns("choice_dropdown"),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("patientList_chosen"),
          label = "Patient lists shown",
          choices = dMCustom$patientListNames,
          selected = NULL,
          status = "primary",
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        icon = icon("gear"),
        label = "Patient lists"
      )
    }
  })

  styled_custom_list <- shiny::reactive({
    shiny::validate(
      shiny::need(
        dMCustom$dM$appointments_filtered_timeR(),
        "No appointments in selected range"
      )
    )
    datatable_styled(
      dMCustom$appointments_patientListR()
    )
  })

  output$custom_table <- DT::renderDT({
    styled_custom_list()
  })
}

.private(dMeasureCustom, ".chosen_patientList", NULL)
.active(dMeasureCustom, "chosen_patientList", function(value) {
  if (missing(value)) {
    return(private$.chosen_patientList)
  }
  if (is.null(setdiff(value, self$patientList$Name))) {
    # setdiff is assymmetrical
    # will be NULL if all 'value' is in self$patientList$Name
    private$.chosen_patientList <- value
    private$set_reactive(self$chose_patientListR, value)
  } else {
    warning(
      setdiff(value, self$patientList$Name),
      " is not a patientList."
    )
  }
})
.reactive(dMeasureCustom, "chosen_patientListR", NULL)

#' Custom module - initialize database table
#'
#' @name initialize_data_table
#'
#' @return list
#'
#' @export
initialize_data_table <- function() {
  return(
    list(
      tablename = "CustomPatientLists",
      variable_list = list(
        c("id", "integer"),
        c("Name", "character"),
        c("patientList", "list")
        # list will create a 'blob' column
        # 'raw' doesn't create a 'blob' column!
      )
    )
  )
}

#' read the configuration database
#'
#' @param dMeasureCustom_obj R6 object
#'
#' @export
read_configuration_db <- function(
  dMeasureCustom_obj) {
  dMeasureCustom_obj$read_configuration_db()
}
.public(dMeasureCustom, "read_configuration_db", function() {
  # read configuration database for patient lists

  self$patientLists <- self$dM$config_db$conn() %>>%
    dplyr::tbl("CustomPatientLists")
  # a link to the table

  self$patientList <-
    DBI::dbReadTable(self$dM$config_db$conn(), "CustomPatientLists")

  private$set_reactive(self$patientListNamesR, self$patientList$Name)
  # set to names of patient lists

  return(self$patientList)
})

.reactive(dMeasureCustom, "patientListNamesR", NULL)

#' write patient list to configuration database
#'
#' @param dMeasureCustom_obj R6 object
#' @param name name of patient list
#' @param filename CSV (comma-separated-value) filename
#'   expects at least two columns,
#'   'ID' and 'Label'
#' @param column_ID name of ID column
#' @param column_Label name of label column
#' @param keepAllColumns if FALSE, trim to just 'ID' and 'Label' columns
#'
#' @return ID of created patient list
#'
#' @export
write_patientList <- function(
  dMeasureCustom_obj,
  name,
  filename,
  column_ID = "ID",
  column_Label = "Label",
  keepAllColumns = FALSE) {
  dMeasureCustom_obj$write_patientList(
    name, filename,
    column_ID, column_Label,
    keepAllColumns
  )
}
.public(dMeasureCustom, "write_patientList",
  function(
    name, filename,
    column_ID = "ID", column_Label = "Label",
    keepAllColumns = FALSE
  ) {
    # write patient list to configuration database

    if (name %in% self$patientLists$Name) {
      # this name already chosen
      warning("'", name, "' already exists as a named patient list.")
      return(NULL)
    }

    if (!file.exists(filename)) {
      warning("'", filename, "' does not exist.")
      return(NULL)
    }

    read_csv_success <- TRUE # by default
    tryCatch(
      d <- read.csv(filename, stringsAsFactors = FALSE),
      error = function(e) {
        warning(e)
        read_csv_success <<- FALSE
      }
    )
    if (!read_csv_success) {
      warning("Unable to read file '", filename, "' as CSV.")
      return(NULL)
    }
    if (!exists(column_ID, d) || !exists(column_Label, d)) {
      warning(
        "File does not contain 'ID' and 'Label' column",
        " names '", column_ID, "' and '", column_Label, "'."
      )
      return(NULL)
    }

    d <- d %>>%
      dplyr::mutate(
        ID = !!as.symbol(column_ID),
        Label = !!as.symbol(column_Label)
      ) # converts column names to 'ID' and 'Label'
    if (!keepAllColumns) {
      # if keepAllColumns is TRUE, then extra original
      # columns are kept, which would take more storage space
      # to store
      d <- d %>>%
        dplyr::select(ID, Label)
    }

    newID <- max(self$patientLists$id, 0) + 1
    # initially might be an empty set, so need to append a '0'
    # note that 'id' is the identifier in the configuration database
    # not the 'ID' column of the patientList!

    query <- paste0(
      "INSERT INTO CustomPatientLists",
      "(id, Name, patientList)",
      "VALUES ($id, $name, $patientList)"
    )
    data_for_sql <- list(
      id = newID,
      name = name,
      patientList = data.frame(
        data = I(serialize(d, NULL, xdr = FALSE, version = 3))
      )
      # data.frame(data = I(serialize(d))) creates the 'single' object that
      # dbSendQuery wants
      # strangely, the data can be read as follow (for the first row)
      # x <- DBI::dbReadTable(dM$config_db$conn(), "CustomPatientLists")
      # x$patientList[1] - returns 'blob'
      # x$patientList[[1]] - returns raw
      # unserialize(x$patientList[[1]]) - returns dataframe
      # referring to 'data' as a column seems unnecessary!
    )

    self$dM$config_db$dbSendQuery(query, data_for_sql)

    return(newID)
  }
)

#' remove patient list from configuration database
#'
#' @param dMeasureCustom_obj R6 object
#' @param name name of patient list
#'
#' @return TRUE if removed successfully
#'
#' @export
remove_patientList <- function(dMeasureCustom_obj, name) {
  dMeasureCustom_obj$remove_patientList(
    name
  )
}
.public(dMeasureCustom, "remove_patientList",
  function(name) {
    # remove patient list from configuration database

    if (!name %in% self$patientList$Name) {
      # this name already chosen
      warning("'", name, "' not in patient list.")
      return(FALSE)
    }

    query <- paste0(
      "DELETE FROM CustomPatientLists WHERE Name = ?"
    )
    data_for_sql <- list(
      name = name
    )
    self$dM$config_db$dbSendQuery(query, data_for_sql)

    return(TRUE)
  }
)

#' patient appointment list combined with custom patient list labels
#'
#'  derived from dM$appointments_filtered_time
#'
#' @param dMeasureCustom_obj R6 object
#' @param chosen_patientList names of chosen custom patient lists
#'
#' @return dataframe of apppointments
#'  $Patient, $AppointmentDate, $AppointmentTime,
#'  $Provider, $Status, $Label
#'
#' @export
appointments_patientList <- function(dMeasureCustom_obj, chosen_patientList = NA) {
  dMeasureCustom_obj$appointments_patientList(chosen_patientList)
}
.public(dMeasureCustom, "appointments_patientList",
  function(chosen_patientList = NA) {

    if (is.na(chosen_patientList)) {
      chosen_patientList <- self$chosen_patientList
    }

    intID <- c(-1)

    for (i in chosen_patientList) {
      # go through patientLists, finding relevant internal IDs
      j <- match(i, self$patientList$Name)
      intID <- c(
        intID,
        unserialize(self$patientList$patientList[[j]])$ID
        )
    }

    l <- self$dM$appointments_filtered_time %>>%
      dplyr::filter(InternalID %in% intID) %>>%
      dplyr::mutate(LabelCombined = NA)

    for (i in chosen_patientList) {
      # go through patientLists, add column labels one-by-one
      j <- match(i, self$patientList$Name)
      l <- l %>>%
        dplyr::left_join(
          unserialize(self$patientList$patientList[[j]]),
          by = c("InternalID" = "ID")) %>>%
        dplyr::mutate(
          LabelCombined = dMeasure::paste2(
            LabelCombined,
            Label,
            na.rm = TRUE, sep = ", "
          )
          # sequentially added, separated by commas
        ) %>>%
        dplyr::select(-c("Label"))
    }
    l <- l %>>%
      dplyr::rename(Label = LabelCombined) %>>%
      dplyr::select(Patient, AppointmentDate, AppointmentTime,
        Provider, Status, Label)

    return(l)
  }
)
.reactive_event(
  dMeasureCustom, "appointments_patientListR",
  quote(
    shiny::eventReactive(
      c(
        self$chosen_patientListR(),
        self$dM$appointments_filtered_timeR()
      ), {
        self$appointments_patientList()
      }
    )
  )
)
