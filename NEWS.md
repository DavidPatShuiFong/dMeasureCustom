---
title: "NEWS"
author: "David Fong"
date: "19th June 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## 1.1.0
19th July

### New

* `dMeasureIntegration` for auto-loading

### Bugfix

* patient list chooser now shows current list choices

## 1.0.1
8th July

### Changes

* patient lists are chosen via a modal (so not updated with each 'click/unclick')
* formally change license to Mozilla Public License 2.0

### Improvements

* custom list names cannot be empty string ""

### Bugfix

* `patientList.callback.update` handles changes of `Name` only
  + previously did not handle properly if `patientList` was unchanged

### Changes

* source from `Custom.R` partly moved t o `methods.R` and `userInterface.R`


## 1.0.0
19th June 2020

* custom patient lists can be imported as CSV file
* update_patientList, remove_patientList, write_patientList
  + write_patientList - adds patient list stored in CSV
  + remove_patientList - removes by name
  + update_patientList - update patient lists

* private/active/reactive versions of chosen_patientList

* read_configuration_db - reads current custom patient list
  settings from dMeasure configuration sqlite
  + stores the list/dataframe in self$patientList
  columns are id (numeric), name (character) and patientList (blob)
  + note that $patientList cannot be manipulated by dplyr due
    to the presence of the blob.

* configuration from DailyMeasure configuration panel
  dMeasureConfigurationTabPanel, dMeasureConfigurationTabPanelItem,
  dMeasureConfigurationTabPanelUI
* appointments_patientList and reactive version
* add_customTags to add screen or print tags

* initialize_data_table to create table CustomPatientLists in
   dMeasure's SQLite configuration database

## 0.1.0
25th March 2020
