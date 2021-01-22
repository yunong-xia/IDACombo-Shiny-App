
# options
options(rgl.useNULL = TRUE)
options(future.rng.onMisuse = "ignore")

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
library(promises)
library(future)
library(ipc)
library(memuse)

options(shiny.maxRequestSize = 500 * 1024^2)
options(stringsAsFactors = FALSE)

par3d(cex = 1)
plan(multiprocess)

#Defining control constants
check_RAM_frequency <- 10 #RAM usage is checked every this many seconds
min_RAM_free_ratio_to_start_future <- 0.0 #The minimum ratio of free ram / total ram that must be available to start a new future (i.e. RAM and CPU intensive) calculation
min_RAM_free_ratio_within_future <- 1 #The minimum ratio of free ram / total ram that must be available for a future calculation to continue through it's loop

# IDACombo Functionalities are implemented in each corresponding R file.
source("dataset_import.R")
source("two_drug.R")
source("two_drug_batch.R")
source("control_plus_one.R")
source("control_plus_one_batch.R")
source("test_vs_control.R")
source("test_vs_control_batch.R")

preprovided_dataset <- list(
  readRDS(paste0(getwd(), "/www/provided_dataset/GDSC1_Calculated_Viabilities_for_IDACombo_shiny.rds")),
  readRDS(paste0(getwd(), "/www/provided_dataset/GDSC2_Calculated_Viabilities_for_IDACombo_shiny.rds")),
  readRDS(paste0(getwd(), "/www/provided_dataset/CTRPv2_Calculated_Viabilities_for_IDACombo_shiny.rds")),
  readRDS(paste0(getwd(), "/www/provided_dataset/PRISM_Repurposing_Calculated_Viabilities_for_IDACombo_shiny.rds"))
)

names(preprovided_dataset) <- c("GDSC1","GDSC2","CTRPv2","PRISM Repurposing")
