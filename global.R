
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
plan(multicore)


check_RAM_frequency <- 10 #RAM usage is checked every this many seconds
min_RAM_free_ratio_to_start_future <- 0.4 #The minimum ratio of free ram / total ram that must be available to start a new future (i.e. RAM and CPU intensive) calculation
min_RAM_free_ratio_within_future <- 0.2 #The minimum ratio of free ram / total ram that must be available for a future calculation to continue through it's loop

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

#add two columns about csustained concentration information to the dataset
for(i in seq_along(preprovided_dataset)){
  #add a logical col to indicate whether the monotherapy of Csustained concentration
  preprovided_dataset[[i]]$with_Csus_conc <- grepl("(Csustained)", preprovided_dataset[[i]]$Drug_Dose)
  drugs_Csus_pairs <- distinct(preprovided_dataset[[i]][preprovided_dataset[[i]]$with_Csus_conc,c("Drug","Drug_Dose")])
  drugs_Csus_pairs$Drug_Dose <- gsub("\\(Csustained\\) ","",drugs_Csus_pairs$Drug_Dose)
  all_drugs <- unique(preprovided_dataset[[i]]$Drug)
  Csus_for_all <- character(length(all_drugs))
  for(j in seq_along(all_drugs)){
    if(all_drugs[j] %in% drugs_Csus_pairs$Drug){
      Csus_for_all[j] <- drugs_Csus_pairs$Drug_Dose[drugs_Csus_pairs$Drug == all_drugs[j]]
    }
    else{
      Csus_for_all[j] <- Inf
    }
  }
  Csus_for_each_row <- character(nrow(preprovided_dataset[[i]]))
  for(j in seq_along(all_drugs)){
    Csus_for_each_row[which(preprovided_dataset[[i]]$Drug == all_drugs[j])] <- Csus_for_all[j]
  }
  preprovided_dataset[[i]]$in_range <- preprovided_dataset[[i]]$Drug_Dose <= Csus_for_each_row
  rm(all_drugs,Csus_for_all,drugs_Csus_pairs,Csus_for_each_row)
}



# define a globally used cell line selection module

#select cell lines
global.cellLineInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("cell_lines_selection"),"Select Cell Lines",
                choices = NULL,
                options = list(`actions-box` = TRUE,`live-search-style` = "startsWith" , `live-search` = TRUE,
                               `selected-text-format`= "count",
                               `count-selected-text` = "{0} cell lines chosen (on a total of {1})"),
                multiple = T),
    checkboxInput(ns("use_cell_line_filter"),label = "show cell line filter?"),
    uiOutput(ns("cell_line_filter"))
  )
}

global.cellLineServer <- function(id, cellLinesAndSubgroups) {
  moduleServer(id, function(input,output,session) {
    subgroups_choices <- reactive(
      unique(cellLinesAndSubgroups()$Cell_Line_Subgroup)
    )
    
    output$cell_line_filter <- renderUI({
      #if user doesnt check the "show cell line filter" check box
      if(input$use_cell_line_filter == FALSE){
        return(list(p("")))
      }else{
        # if user check
        
        # if there is no subgroups information in the dataset
        if(is.null(subgroups_choices())){
          return(list(
            h4("Filter cell line by:") %>% 
              helper(type = "inline",
                     title = "Cell Line filtering",
                     icon = "question-circle", colour = NULL,
                     content = c("This option can be used to filter the cell line options displayed in the \"Select Cell Lines\" menu. Note that selecting any options from a filter menu will omit all cell lines which lack annotated information for that menu's filtering criteria."),
                     size = "m",
                     buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
              ),
            wellPanel(
              p(HTML("<b>No subgroups information available in the loaded dataset</b>"), style = "color:red")
            )
          ))
        }else{
          # if there is subgroups information in the dataset
          
          # first, get namespace for UI's ID
          ns <- session$ns
          return(list(
            h4("Filter cell line by:") %>% 
              helper(type = "inline",
                     title = "Cell Line filtering",
                     icon = "question-circle", colour = NULL,
                     content = c("This option can be used to filter the cell line options displayed in the \"Select Cell Lines\" menu. Note that selecting any options from a filter menu will omit all cell lines which lack annotated information for that menu's filtering criteria."),
                     size = "m",
                     buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
              ),
            wellPanel(
              pickerInput(ns("subgroups_selection"),"Select Cell Lines By Subgroups",
                          choices = subgroups_choices(),
                          options = list(`actions-box` = TRUE,`live-search-style` = "startsWith" , `live-search` = TRUE,
                                         `selected-text-format`= "count",
                                         `count-selected-text` = "{0} models choosed (on a total of {1})"),
                          multiple = T) %>%
                helper(type = "inline",
                       title = "Mechanism of Action (MOA) Filtering",
                       icon = "question-circle", colour = NULL,
                       content = c("Limits cell lines displayed in the \"Select Cell Lines\" menu to compounds with at least one of the selected MOAs."),
                       size = "m",
                       buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                )
            )
          ))
        }
      }
    })
    
    cell_line_choices <- reactive({
      #Get available cell lines based on subgroups.
      if(! is.null(input$subgroups_selection)){
        filtered_cell_lines <- unique(cellLinesAndSubgroups()$Cell_Line[cellLinesAndSubgroups()$Cell_Line_Subgroup %in% input$subgroups_selection])
        return(filtered_cell_lines)
      }else{
        cell_lines_in_dataset <- unique(cellLinesAndSubgroups()$Cell_Line)
        return(cell_lines_in_dataset)
      }
    })
    
    observeEvent(cell_line_choices(),{
      updatePickerInput(session, inputId = "cell_lines_selection", label = "Select Cell Lines",
                        choices = cell_line_choices(),
                        selected = cell_line_choices())
    })
    
    list(
      cellLines = reactive(input$cell_lines_selection),
      subgroups = reactive(input$subgroups_selection)
    )
    
  })
}


