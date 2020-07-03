#' methods of dMeasureCustom
#'
#' requires R6 methods from Custom.R
#'
#' @include Custom.R
NULL

.private(dMeasureCustom, ".chosen_patientList", NULL)
.active(dMeasureCustom, "chosen_patientList", function(value) {
  if (missing(value)) {
    return(private$.chosen_patientList)
  }
  if (length(setdiff(value, self$patientList$Name)) == 0) {
    # setdiff is assymmetrical
    # will be length 0 if all 'value' is in self$patientList$Name
    private$.chosen_patientList <- value
    private$set_reactive(self$chosen_patientListR, value)
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
    dplyr::tbl("CustomPatientLists") %>>%
    dplyr::select(-patientList)
  # a link to the table
  # can't refer to the patientList column,
  #  because tibble can't handle blob columns (!)

  self$patientList <-
    DBI::dbReadTable(self$dM$config_db$conn(), "CustomPatientLists")
  # by contrast, dbReadTable *can* handle blob columns
  # but needs to be called every time the table changes

  self$patientListNames <- self$patientList$Name
  private$set_reactive(self$patientListNamesR, self$patientList$Name)
  # set to names of patient lists
  # this will also need to be called every time
  #  self$patientList changes

  return(self$patientList)
})

.public(dMeasureCustom, "patientListNames", NULL)
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
.public(
  dMeasureCustom, "write_patientList",
  function(
    name, filename,
    column_ID = "ID", column_Label = "Label",
    keepAllColumns = FALSE) {
    # write patient list to configuration database

    if (name %in% (self$patientList$Name)) {
      # this name already chosen
      warning("'", name, "' already exists as a named patient list.")
      return(NULL)
    }

    if (name == "") {
      # empty name!
      warning("'Name' cannot be empty!")
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

    newID <- max(self$patientList$id, 0) + 1
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

    self$patientList <-
      DBI::dbReadTable(self$dM$config_db$conn(), "CustomPatientLists")
    # re-read patient list
    private$set_reactive(self$patientListNamesR, self$patientList$Name)
    # set names of patient lists

    return(newID)
  }
)

#' update patient list in configuration database
#'
#' @param dMeasureCustom_obj R6 object
#' @param name name of patient list
#' @param filename CSV (comma-separated-value) filename
#'   expects at least two columns,
#'   'ID' and 'Label'
#' @param column_ID name of ID column
#' @param column_Label name of label column
#' @param keepAllColumns if FALSE, trim to just 'ID' and 'Label' columns
#' @param ID ID of patient list. If not provided, will try to infer from \code{name}
#'
#' @return ID of patient list.
#'  stop errors generated for several different reasons of failure
#'
#' @export
update_patientList <- function(
  dMeasureCustom_obj,
  name,
  filename,
  column_ID = "ID",
  column_Label = "Label",
  keepAllColumns = FALSE,
  ID = NULL) {
  dMeasureCustom_obj$write_patientList(
    name, filename,
    column_ID, column_Label,
    keepAllColumns, ID
  )
}
.public(
  dMeasureCustom, "update_patientList",
  function(
    name, filename,
    column_ID = "ID", column_Label = "Label",
    keepAllColumns = FALSE, ID = NULL) {
    # write patient list to configuration database

    if (!is.null(ID)) {
      # ID has been defined
      if (name %in% (self$patientList[self$patientList$id != ID, ]$Name)) {
        # this name already chosen with a different ID
        stop("'", name, "' already exists as a named patient list.")
      }
      # otherwise, we are going to change the name
    } else {
      # ID has *not* been defined, we need to find the ID
      # hopefully, the 'name' will help find the ID
      ID <- which(name == self$patientList$Name)
      if (length(ID) == 0) {
        # length will == 1 if an ID was found
        # ID == numeric(0) if not found
        stop("'", name, "' does not define a patient list, and ID not provided.")
      }
    }

    if (!file.exists(filename)) {
      stop("'", filename, "' does not exist.")
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
      stop("Unable to read file '", filename, "' as CSV.")
    }
    if (!exists(column_ID, d) || !exists(column_Label, d)) {
      stop(
        "File does not contain 'ID' and 'Label' column",
        " names '", column_ID, "' and '", column_Label, "'."
      )
    }

    d <- d %>>%
      dplyr::mutate(
        ID = !!as.symbol(column_ID),
        # patient identification IDs (internal ID)
        #
        # not that this ID is not the same as the
        # ID of the patient list!
        Label = !!as.symbol(column_Label)
      ) # converts column names to 'ID' and 'Label'
    if (!keepAllColumns) {
      # if keepAllColumns is TRUE, then extra original
      # columns are kept, which would take more storage space
      # to store
      d <- d %>>%
        dplyr::select(ID, Label)
    }

    query <- paste(
      "UPDATE CustomPatientLists SET",
      "Name = $name, patientList = $patientList",
      "WHERE id = $id"
    )
    data_for_sql <- list(
      name = name,
      patientList = data.frame(
        data = I(serialize(d, NULL, xdr = FALSE, version = 3))
      ),
      id = ID
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

    self$patientList <-
      DBI::dbReadTable(self$dM$config_db$conn(), "CustomPatientLists")
    # re-read patient list
    private$set_reactive(self$patientListNamesR, self$patientList$Name)
    # set names of patient lists

    return(ID)
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
.public(
  dMeasureCustom, "remove_patientList",
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
    data_for_sql <- list(name)
    self$dM$config_db$dbSendQuery(query, data_for_sql)

    self$patientList <-
      DBI::dbReadTable(self$dM$config_db$conn(), "CustomPatientLists")
    # re-read patient list
    private$set_reactive(self$patientListNamesR, self$patientList$Name)
    # set to names of patient lists

    return(TRUE)
  }
)

#' add tags to a patient list
#'
#' @param custom_patientLists the custom patient lists
#' @param chosen_patientList the names of the chosen patient lists
#' @param patient_list dataframe, must contain column internalID
#' @param screentag add fomantic/semantic tags
#' @param screentag_print  add printable/copyable text-only tags
#'
#' @return dataframe, with added column screentag or screentag_print
add_customTags <- function(
  custom_patientLists,
  chosen_patientList,
  patient_list,
  screentag = FALSE,
  screentag_print = TRUE) {

  if (screentag_print) {
    patient_list <- patient_list %>>%
      dplyr::mutate(screentag_print = NA) # prepare to be filled!
  } else if (screentag) {
    patient_list <- patient_list %>>%
      dplyr::mutate(screentag = NA)
  }

  for (i in chosen_patientList) {
    # go through patientLists, add column labels one-by-one
    j <- match(i, custom_patientLists$Name)
    patient_list <- patient_list %>>%
      dplyr::left_join(
        unserialize(custom_patientLists$patientList[[j]]),
        by = c("InternalID" = "ID")
      )
    if (screentag_print) {
      patient_list <- patient_list %>>%
        dplyr::mutate(
          screentag_print = dMeasure::paste2(
            screentag_print,
            Label,
            na.rm = TRUE, sep = ", "
          )
          # sequentially added, separated by commas
        )
    } else if (screentag) {
      patient_list <- patient_list %>>%
        dplyr::mutate(
          screentag = dMeasure::paste2(
            screentag,
            dMeasure::semantic_tag(
              trimws(Label),
              colour = "green"
            ),
            na.rm = TRUE, sep = ""
          )
        )
    }
    patient_list <- patient_list %>>%
      dplyr::select(-c("Label"))
    # need to remove Label column before next iteration
  }

  return(patient_list)
}

#' patient appointment list combined with custom patient list labels
#'
#'  derived from dM$appointments_filtered_time
#'
#' @md
#'
#' @param dMeasureCustom_obj R6 object
#' @param chosen_patientList names of chosen custom patient lists
#' @param screentag add HTML fomantic/semantic tags
#' @param screentag_print add 'printable' plaintext tags
#'
#' @return dataframe of apppointments
#'  $Patient, $AppointmentDate, $AppointmentTime,
#'  $Provider, $Status, $Label
#'
#' @export
appointments_patientList <- function(
  dMeasureCustom_obj,
  chosen_patientList = NA,
  screentag = FALSE,
  screentag_print = TRUE) {
  dMeasureCustom_obj$appointments_patientList(
    chosen_patientList,
    screentag,
    screentag_print
  )
}
.public(
  dMeasureCustom, "appointments_patientList",
  function(
    chosen_patientList = NA,
    screentag = FALSE,
    screentag_print = TRUE
  ) {
    if (is.na(chosen_patientList)) {
      chosen_patientList <- self$chosen_patientList
    }
    if (!screentag && !screentag_print) {
      stop("One of 'screentag' or 'screentag_print' must be set to TRUE")
    } else if (screentag && screentag_print) {
      stop("Only one of 'screentag' or 'screentag_print' can be set to TRUE")
    }

    intID <- c(-1) # create vector of intID in the chosen custom lists
    for (i in chosen_patientList) {
      # go through patientLists, finding relevant internal IDs
      j <- match(i, self$patientList$Name)
      intID <- c(
        intID,
        unserialize(self$patientList$patientList[[j]])$ID
      )
    }

    l <- self$dM$appointments_filtered_time %>>%
      dplyr::filter(InternalID %in% intID)

    l <- add_customTags(
      self$patientList,
      chosen_patientList,
      l,
      screentag = screentag,
      screentag_print = screentag_print
    )

    if (screentag) {
      l <- l %>>%
        dplyr::rename(List = screentag)
    } else if (screentag_print) {
      l <- l %>>%
        dplyr::rename(List = screentag_print)
    }

    l <- l %>>%
      dplyr::select(
        Patient, AppointmentDate, AppointmentTime,
        Provider, Status, List
      )

    return(l)
  }
)
.reactive_event(
  dMeasureCustom, "appointments_patientListR",
  quote(
    shiny::eventReactive(
      c(
        self$chosen_patientListR(),
        self$dM$appointments_filtered_timeR(),
        self$printcopy_view()
      ), {
        self$appointments_patientList(
          screentag = !self$printcopy_view(),
          screentag_print = self$printcopy_view()
        )
      }
    )
  )
)
