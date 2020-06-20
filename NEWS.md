---
title: "NEWS"
author: "David Fong"
date: "19th June 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

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
