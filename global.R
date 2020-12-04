
# options
options(rgl.useNULL = TRUE)


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
library(car)
library(shinyjs)
library(shinybusy)
library(openxlsx)
library(ggplot2)
library(gridExtra)

options(shiny.maxRequestSize = 500 * 1024^2)
options(stringsAsFactors = FALSE)

par3d(cex = 1)


# IDACombo Functionalities are implemented in each corresponding R file.
source("dataset_import.R")
source("two_drug.R")
source("two_drug_batch.R")
source("control_plus_one.R")
source("control_plus_one_batch.R")
source("test_vs_control.R")
source("test_vs_control_batch.R")
