testVsControl.controlDrugInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("controlDrugs"),"Select Drugs in Control Treatment (Multiple)",
              choices = NULL,
              options = list(`actions-box` = TRUE,`live-search-style` = "startsWith" , `live-search` = TRUE),
              multiple = T)
}

testVsControl.controlDrugServer <- function(id, dataset) {
  moduleServer(id, function(input,output,session) {
    observeEvent(dataset(), {
      drug_choices <- unique(dataset()$Drug)
      updatePickerInput(session, inputId = "controlDrugs", label = "Select drugs in Control Treatment",
                        choices = drug_choices)
    })
    
    reactive(input$controlDrugs)
  })
}

testVsControl.controlDoseInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("controlDoseSelect"))
}

testVsControl.controlDoseServer <- function(id,dataset,fileType,selectedControlDrugs) {
  moduleServer(id, function(input,output,session) {
    output$controlDoseSelect <- renderUI({
      if(length(selectedControlDrugs()) > 0) {
        ns <- session$ns
        output = tagList()
        for(i in 1:length(selectedControlDrugs()) ){

          dose_choices <- (unique(dataset()$Drug_Dose[ dataset()$Drug == selectedControlDrugs()[i] ]))
          if(length(fileType()) != 0 && fileType() == "provided"){
            Csustained_conc <- as.numeric(gsub("\\(Csustained\\) ","",dose_choices[grep("\\(Csustained\\) ",dose_choices)]))
            clean_conc <- sort(as.numeric(gsub("\\(Csustained\\) ", "", dose_choices)))
            if(length(Csustained_conc) != 0){
              clean_conc[match(Csustained_conc,clean_conc)] <- paste0("(Csustained) ", Csustained_conc)
            }
            else{
              clean_conc <- as.character(clean_conc)
            }
            dose_choices <- clean_conc
          }
          output[[i]] = selectInput(ns(paste0("controlDose",i)), paste0("Select dose for Drug(Control Treatment): ",selectedControlDrugs()[i]),
                                    choices = dose_choices)
        } ## for loop
        
        output
      }
    })
    
    #return a vector of selected drug doses
    reactive({
      doses <- c()
      for(i in 1:length(selectedControlDrugs())) {
        doses <- c(doses, input[[paste0("controlDose",i)]])
      }
      doses
    })
  })
}


testVsControl.testDrugInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("testDrugs"),"Select Drugs in Test Treatment",
              choices = NULL,
              options = list(`actions-box` = TRUE,`live-search-style` = "startsWith" , `live-search` = TRUE),
              multiple = T)
}

testVsControl.testDrugServer <- function(id, dataset) {
  moduleServer(id, function(input,output,session) {
    observeEvent(dataset(), {
      drug_choices <- unique(dataset()$Drug)
      updatePickerInput(session, inputId = "testDrugs", label = "Select drugs in Test Treatment",
                        choices = drug_choices)
    })
    
    reactive(input$testDrugs)
  })
}

testVsControl.testDoseInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("testDoseSelect"))
}

testVsControl.testDoseServer <- function(id,dataset,fileType,selectedTestDrugs) {
  moduleServer(id, function(input,output,session) {
    output$testDoseSelect <- renderUI({
      if(length(selectedTestDrugs()) > 0) {
        ns <- session$ns
        lapply(1:length(selectedTestDrugs()), function(i) {
          dose_choices <- sort(unique(dataset()$Drug_Dose[ dataset()$Drug == selectedTestDrugs()[i] ]))
          if(length(fileType()) != 0 && fileType() == "provided"){
            Csustained_conc <- as.numeric(gsub("\\(Csustained\\) ","",dose_choices[grep("\\(Csustained\\) ",dose_choices)]))
            clean_conc <- sort(as.numeric(gsub("\\(Csustained\\) ", "", dose_choices)))
            if(length(Csustained_conc) != 0){
              clean_conc[match(Csustained_conc,clean_conc)] <- paste0("(Csustained) ", Csustained_conc)
            }
            else{
              clean_conc <- as.character(clean_conc)
            }
            dose_choices <- clean_conc
          }
          selectInput(ns(paste0("testDose",i)), paste0("Select dose for Drug(Test Treatment): ",selectedTestDrugs()[i]),
                      choices = dose_choices)
        })
      }
    })
    
    #return a vector of selected drug doses
    reactive({
      lapply(1:length(selectedTestDrugs()), function(i) {
        (input[[paste0("testDose",i)]])
      }) %>%
        unlist()
    })
  })
}

testVsControl.cellLineInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("subgroups"),"Select Cell Lines By Subgroups",
                choices = NULL,
                options = list(`live-search-style` = "startsWith" , `live-search` = TRUE),
                multiple = T),
    div(style="display:inline-block;width:40%;text-align: center;",actionButton(ns("selectAllSubgroups"),"All Subgroups")),
    div(style="display:inline-block;width:40%;text-align: center;",actionButton(ns("deselectAllSubgroups"),"Clean Subgroups")),
    pickerInput(ns("cell_lines"),"Select Cell Lines",
                choices = NULL,
                options = list(`actions-box` = TRUE,`live-search-style` = "startsWith" , `live-search` = TRUE,
                               `selected-text-format`= "count",
                               `count-selected-text` = "{0} models choosed (on a total of {1})"),
                multiple = T)
  )
}

testVsControl.CellLineServer <- function(id, dataset) {
  moduleServer(id,function(input,output,session){
    
    cl_sg_set <- reactive(
      if(!is.null(dataset()))
        distinct(dataset()[, c("Cell_Line","Cell_Line_Subgroup")])
      else
        NULL
    )
    
    cell_line_choices <- reactive(
      unique(cl_sg_set()$Cell_Line)
    )
    
    subgroups_choices <- reactive(
      unique(cl_sg_set()$Cell_Line_Subgroup)
    )
    
    observeEvent(c(dataset()), {
      updatePickerInput(session, inputId = "cell_lines", label = "Select Cell Lines",
                        choices = cell_line_choices())
      updatePickerInput(session, inputId = "subgroups", label = "Select Cell Lines By Subgroups",
                        selected = NULL,
                        choices = c("Custom",subgroups_choices()))
    })
    
    prev_selected_cell_lines <- reactiveVal(value = NULL)
    prev_selected_subgroups <- reactiveVal(value = NULL)
    
    observeEvent(input$subgroups, {
      
      if("Custom" %in% prev_selected_subgroups()) { ## Previously selected "Custom"
        if("Custom" %in% input$subgroups && length(input$subgroups)>1) { ## now select other subgroups. 
          new_subgroups <- setdiff(input$subgroups , "Custom")
          new_cell_lines <- cl_sg_set()$Cell_Line[cl_sg_set()$Cell_Line_Subgroup %in% new_subgroups]
          prev_selected_cell_lines(new_cell_lines)
          prev_selected_subgroups(new_subgroups)
          updatePickerInput(session, inputId = "cell_lines", label = "Select Cell Lines",
                            selected = new_cell_lines,
                            choices = cell_line_choices())
          updatePickerInput(session, inputId = "subgroups", label = "Select Cell Lines By Subgroups",
                            selected = new_subgroups,
                            choices = c("Custom",subgroups_choices()))
        }
        else if(is.null(input$subgroups)){  # when users deselect "Custom"
          prev_selected_cell_lines(NULL)
          updatePickerInput(session, inputId = "cell_lines", label = "Select Cell Lines",
                            selected = NULL,
                            choices = cell_line_choices())
        }
        else {
          #do nothing
        }
      }
      else { ## Previously didn't select "Custom"
        if("Custom" %in% input$subgroups) {  ## newly select "Custom"
          prev_selected_subgroups("Custom")
          updatePickerInput(session, inputId = "subgroups", label = "Select Cell Lines By Subgroups",
                            selected = "Custom",
                            choices = c("Custom",subgroups_choices()))
        }
        else { # just select other subgroups
          new_subgroups <- input$subgroups
          new_cell_lines <- cl_sg_set()$Cell_Line[cl_sg_set()$Cell_Line_Subgroup %in% new_subgroups]
          prev_selected_cell_lines(new_cell_lines)
          prev_selected_subgroups(new_subgroups)
          updatePickerInput(session, inputId = "cell_lines", label = "Select Cell Lines",
                            selected = new_cell_lines,
                            choices = cell_line_choices())
          updatePickerInput(session, inputId = "subgroups", label = "Select Cell Lines By Subgroups",
                            selected = new_subgroups,
                            choices = c("Custom",subgroups_choices()))
        }
      }
    }, ignoreNULL = F)
    
    observeEvent(input$selectAllSubgroups, {
      prev_selected_subgroups(subgroups_choices())
      updatePickerInput(session, inputId = "subgroups", label = "Select Cell Lines By Subgroups",
                        selected = subgroups_choices(),
                        choices = c("Custom",subgroups_choices()))
    })
    
    observeEvent(input$deselectAllSubgroups, {
      prev_selected_subgroups(NULL)
      updatePickerInput(session, inputId = "subgroups", label = "Select Cell Lines By Subgroups",
                        selected = NULL,
                        choices = c("Custom",subgroups_choices()))
    })
    
    
    observeEvent(input$cell_lines, {
      
      if("Custom" %in% input$subgroups){
        prev_selected_cell_lines(input$cell_lines)
      }
      else { 
        # check whether the selected cell line match those subgroups.
        if(!is.null(cl_sg_set()) && length(unique(cl_sg_set()$Cell_Line[cl_sg_set()$Cell_Line_Subgroup %in% input$subgroups ])) != length(input$cell_lines)){
          prev_selected_cell_lines(input$cell_lines)
          prev_selected_subgroups(input$subgroups)
          updatePickerInput(session, inputId = "subgroups", 
                            label = "Select Cell Lines By Subgroups",
                            selected = "Custom",
                            choices = c("Custom",subgroups_choices()))
        }
      }
    }, ignoreNULL = F)
    
    list(
      cellLines = reactive(input$cell_lines),
      subgroups = reactive(input$subgroups)
    )
  })
}

# 2Drug parameters and there helpers
testVsControl.parametersInput <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("uncertainty"), "Calculate Uncertainty") %>%
      helper(type = "inline",
             title = "Calculate Uncertainty",
             icon = "question-circle", colour = NULL,
             content = "Should a Monte Carlo simulation be performed to estimate uncertainties in the efficacy predictions based on uncertainties in the monotherapy efficacy measurements? Note that selecting this option will significantly extend the time it takes to complete the prediction.",
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    conditionalPanel(condition = "input.uncertainty", ns = ns,
                     numericInput(inputId = ns("nSimulation"), label = "Number of random samples to be drawn when calculating output efficacy prediction uncertainties", value = 1000, min = 40, max = 5000)),
    checkboxInput(ns("hazardRatio"), "Calculate Hazard Ratios") %>%
      helper(type = "inline",
             title = "Calculate IDAComboscore And HazardRatios",
             icon = "question-circle", colour = NULL,
             content = "Should Hazard Ratios (HRs) be calculated between the test and control therapies? Note that these values are only meaningful when efficacy values are scaled between 0 and 1 (i.e. viability, normalized AUC, etc.).",
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    checkboxInput(ns("averageDuplicate"),"Average Duplicate Records", value = T) %>%
      helper(type = "inline",
             title = "Average Duplicate Records",
             icon = "question-circle", colour = NULL,
             content =  "Should duplicated records (i.e. where a cell line has multiple records for being tested with a given drug at a given concentration) should be averaged? If this option is not selected and duplicates are found, IDACombo will skip all except the first occurence of each duplicate.",
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      )
  )
}

testVsControl.parametersServer <- function(id, fileType, isLowerEfficacy) {
  moduleServer(id, function(input,output,session) {
    
    list(isLowerEfficacy = isLowerEfficacy,
         uncertainty = reactive(input$uncertainty),
         hazardRatio = reactive(input$hazardRatio),
         averageDuplicate = reactive(input$averageDuplicate),
         nSim = reactive(input$nSimulation))
  })
}



testVsControl.ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 3, status = "primary", solidHeader = TRUE, title="Test Vs Control Input",
        testVsControl.controlDrugInput(ns("controlDrugSelection")),
        testVsControl.controlDoseInput(ns("controlDoseSelection")),
        tags$hr(),
        testVsControl.testDrugInput(ns("testDrugSelection")),
        testVsControl.testDoseInput(ns("testDoseSelection")),
        tags$hr(),
        testVsControl.cellLineInput(ns("cellLineSelection")),
        tags$hr(),
        testVsControl.parametersInput(ns("parametersCheck")),
        tags$hr(),
        actionButton(ns("button"), "RUN")
    ),
    box(width = 9, status = "primary", solidHeader = TRUE, title="Test VS Control Result",
        downloadButton(ns('downloadData'), 'Download DataTable'),
        downloadButton(ns('downloadLog'), 'Download Log File'),
        conditionalPanel(condition = "input.button",ns = ns, tabsetPanel(type = "tabs",
                                                                         tabPanel("Table", withSpinner(DT::dataTableOutput(ns("result")))),
                                                                         tabPanel('Log', withSpinner(verbatimTextOutput(ns('log')))))  
        )
    )
  )
}


testVsControl.server <- function(id, fileInfo) {
  moduleServer(id, function(input,output,session) {
    dataset <- fileInfo$dataset
    
    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    efficacyMetric <- fileInfo$efficacyMetric
    
    isLowerEfficacy <- fileInfo$isLowerEfficacy
    
    selectedControlDrugs <- testVsControl.controlDrugServer("controlDrugSelection", dataset)
    
    selectedControlDoses <- testVsControl.controlDoseServer("controlDoseSelection", dataset, fileType,selectedControlDrugs)
    
    selectedTestDrugs <- testVsControl.testDrugServer("testDrugSelection", dataset)
    
    selectedTestDoses <- testVsControl.testDoseServer("testDoseSelection", dataset, fileType, selectedTestDrugs)
    
    selectedCellLinesAndSubgroups <- testVsControl.CellLineServer("cellLineSelection",dataset)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups
    
    checkedParameters <- testVsControl.parametersServer("parametersCheck", fileType, isLowerEfficacy)
    
    nSim <- checkedParameters$nSim
    
    warningMessage <- reactiveVal(NULL)
    
    output$log <- renderText({
      warningMessage()
    })
    
    tableResult <- eventReactive(input$button, {
      validate(
        need(!is.null(dataset()), "Please upload your data"),
        need(!is.null(selectedControlDrugs()), "Please select control treatment drugs"),
        need(!is.null(selectedCellLines()), "Please select Cell lines"),
        need(length(selectedControlDoses()) == length(selectedControlDrugs()), "Please select control treatment doses"),
        need(!is.null(selectedTestDrugs()), "Please select test treatment drugs"),
        need(length(selectedTestDoses()) == length(selectedTestDrugs()), "Please select test treatment doses")
      )
      
      
      
      monotherapy_data <- dataset()
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL

      
      warning_msg <- ""
      res <- withCallingHandlers(
        tryCatch(
          expr = {
            res_list <- IDAPredict.TestvsControl(
              Monotherapy_Data = monotherapy_data,
              Cell_Line_Name_Column = "Cell_Line",
              Drug_Name_Column = "Drug",
              Drug_Concentration_Column = "Drug_Dose",
              Efficacy_Column = "Efficacy",
              LowerEfficacyIsBetterDrugEffect = checkedParameters$isLowerEfficacy(),
              Efficacy_Metric_Name = efficacyMetric(),
              Control_Treatment_Drugs = selectedControlDrugs(),
              Control_Treatment_Drug_Concentrations = selectedControlDoses(),
              Test_Treatment_Drugs = selectedTestDrugs(),
              Test_Treatment_Drug_Concentrations = selectedTestDoses(),
              Calculate_Uncertainty = checkedParameters$uncertainty(),
              Efficacy_SE_Column = eff_se_col,
              n_Simulations = nSim(),
              Calculate_Hazard_Ratio = checkedParameters$hazardRatio(),
              Average_Duplicate_Records = checkedParameters$averageDuplicate()
            )
            if(!is.data.frame(res_list[[1]])){
              NULL
            } else{
              res <- cbind(Control_Treatment_Drugs = paste(res_list$Control_Treatment$Control_Treatment_Drugs, collapse = ", "), 
                           Control_Treatment_Drug_Concentration = paste(res_list$Control_Treatment$Control_Treatment_Drug_Concentrations, collapse = ", "),
                           Test_Treatment_Drugs = paste(res_list$Test_Treatment$Test_Treatment_Drugs, collapse = ", "),
                           Test_Treatment_Drugs_Concentrations = paste(res_list$Test_Treatment$Test_Treatment_Drug_Concentrations, collapse = ", "),
                           Number_of_Cell_Lines_Used = length(res_list$Cell_Lines_Used),
                           Cell_Lines_Used = paste(res_list$Cell_Lines_Used, collapse = ", "),
                           res_list[[1]])
              res
            }
          }),
        warning = function(w) {
          warning_msg <<- paste0(warning_msg, paste0(Sys.Date(),": ",conditionMessage(w),"\n"))
          invokeRestart("muffleWarning")
        }
      )
      if(nchar(warning_msg) == 0)
        warning_msg <- "No warning messages"
      warningMessage(warning_msg)
      return(res)
      
      
    })
    
    output$result <- renderDataTable({
      if(!is.null(tableResult()))
        tableResult()[, names(tableResult()) != "Cell_Lines_Used"]
      },options = list(scrollX = TRUE))
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(con) {
        write_delim(result(), con, delim = "\t")
      }
    )
    
  })
}
