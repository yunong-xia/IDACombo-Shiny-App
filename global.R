#Library we will use
  ##Library for rendering
library(shiny)
library(shinydashboard)
library(shinyhelper)
library(DT)
library(tidyverse)
library(IDACombo)
library(shinycssloaders)
library(shinyWidgets)
library(rgl)
library(data.table)
library(future)
library(promises)
library(ipc)
library(car)
library(shinyjs)

#IDACombo Functionalities are implemented in each corresponding R file.
source("dataset_import.R")
source("two_drug.R")
source("two_drug_batch.R")
source("control_plus_one.R")
source("helper_func.R")
source("control_plus_one_batch.R")
source("test_vs_control.R")
source("test_vs_control_batch.R")



#options
options(rgl.useNULL = TRUE)
options(shiny.maxRequestSize = 300*1024^2)
par3d(cex = 1)
options(stringsAsFactors = FALSE)
plan(multisession)

