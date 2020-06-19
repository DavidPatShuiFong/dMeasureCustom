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

* custom patient lists can be imported as CSV file
* update_patientList, remove_patientList, write_patientList
* private/active/reactive versions of chosen_patientList

* read_configuration_db - reads current custom patient list
  settings from dMeasure configuration sqlite

* configuration from DailyMeasure configuration panel
  dMeasureConfigurationTabPanel, dMeasureConfigurationTabPanelItem,
  dMeasureConfigurationTabPanelUI
* appointments_patientList and reactive version
* add_customTags to add screen or print tags
