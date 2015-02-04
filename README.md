## Analysis of the DPOE 2014 Survey Results

### Introduction

### Contents

* dpoe14_survey.Rproj - default setup for R project

#### data - datasets to be analyzed
* data_clean.csv - a cleaned version of the survey responses. See change_log.txt (not currently included)
* data_original.csv - the orginal survey responses as exported from SurveyMonkey (not currently included)
* data_popbystate - state by state population data from the US census (not currently included)
* change-log.txt - a description of changes made while cleaning the data
* codebook.txt - description of the survey variables

#### analysis - R scripts for analyzing the data
* 0_functions.r - shared R functions for analysis
* 1_setup.r - load and classify of data
* 2_time.r - subset and graph time data
* 3_org.r - subset and graph organizational data
* 4_training - subset and graph training preferences
* scratch.r - experiment with data views

#### reports - markdown files for discussing analysis
* analysis-tufte.Rnw - source for report formatted according to the Tufte class
