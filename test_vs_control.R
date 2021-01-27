testVsControl.controlDrugInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("controlDrugs"),"Select Drug(s) in Control Treatment",
              choices = NULL,
              options = list(`actions-box` = FALSE, `live-search-style` = "contains", `live-search` = TRUE, "max-options" = 10, "max-options-text" = "Maximum Number of Drugs Selected"),
              multiple = T)
}

testVsControl.controlDrugServer <- function(id, dataset) {
  moduleServer(id, function(input,output,session) {
    observeEvent(dataset(), {
      drug_choices <- unique(dataset()$Drug)
      updatePickerInput(session, inputId = "controlDrugs", label = "Select Drug(s) in Control Treatment",
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
  pickerInput(ns("testDrugs"),"Select Drug(s) in Test Treatment",
              choices = NULL,
              options = list(`actions-box` = FALSE, `live-search-style` = "contains", `live-search` = TRUE, "max-options" = 10, "max-options-text" = "Maximum Number of Drugs Selected"),
              multiple = T)
}

testVsControl.testDrugServer <- function(id, dataset) {
  moduleServer(id, function(input,output,session) {
    observeEvent(dataset(), {
      drug_choices <- unique(dataset()$Drug)
      updatePickerInput(session, inputId = "testDrugs", label = "Select Drug(s) in Test Treatment",
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
        global.cellLineInput(ns("cellLineSelection")),
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
    
    cellLinesAndSubgroups <- fileInfo$cellLinesAndSubgroups
    
    efficacyMetric <- fileInfo$efficacyMetric
    
    isLowerEfficacy <- fileInfo$isLowerEfficacy
    
    selectedControlDrugs <- testVsControl.controlDrugServer("controlDrugSelection", dataset)
    
    selectedControlDoses <- testVsControl.controlDoseServer("controlDoseSelection", dataset, fileType,selectedControlDrugs)
    
    selectedTestDrugs <- testVsControl.testDrugServer("testDrugSelection", dataset)
    
    selectedTestDoses <- testVsControl.testDoseServer("testDoseSelection", dataset, fileType, selectedTestDrugs)
    
    selectedCellLinesAndSubgroups <- global.cellLineServer("cellLineSelection",cellLinesAndSubgroups)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups
    
    checkedParameters <- testVsControl.parametersServer("parametersCheck", fileType, isLowerEfficacy)
    
    nSim <- checkedParameters$nSim
    
    
    computationResult <- eventReactive(input$button, {
      validate(
        need(!is.null(dataset()), "Please upload your data"),
        need(!is.null(selectedControlDrugs()), "Please select control treatment drugs"),
        need(!is.null(selectedCellLines()), "Please select Cell lines"),
        need(length(selectedControlDoses()) == length(selectedControlDrugs()), "Please select control treatment doses"),
        need(!is.null(selectedTestDrugs()), "Please select test treatment drugs"),
        need(length(selectedTestDoses()) == length(selectedTestDrugs()), "Please select test treatment doses")
      )
      
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL

      #show spinner and lock the whole ui
      show_modal_spinner(
        spin = "semipolar",
        color = "#112446",
        text = "Calculating Efficacy..."
      )
      isLowerEfficacyBetter <- checkedParameters$isLowerEfficacy()
      metricName <- efficacyMetric()
      controlDrugs <- selectedControlDrugs()
      controlDoses <- selectedControlDoses()
      testDrugs <- selectedTestDrugs()
      testDoses <- selectedTestDoses()
      calculateUncertainty <- checkedParameters$uncertainty()
      nSimulation <- nSim()
      calculateHazardRatio <- checkedParameters$hazardRatio()
      averageDuplicateRecords <- checkedParameters$averageDuplicate()
      data <- dataset()
      future_result <- future(
        global = c("isLowerEfficacyBetter","metricName","controlDrugs","controlDoses","testDrugs","testDoses","calculateUncertainty","nSimulation","calculateHazardRatio","averageDuplicateRecords","data", "eff_se_col"),
        packages = c("IDACombo", "ggplot2","data.table","gridExtra"),
        expr = {
          warning_msg <- ""
          res <- withCallingHandlers(
            tryCatch(
              expr = {
                res_list <- IDAPredict.TestvsControl(
                  Monotherapy_Data = data,
                  Cell_Line_Name_Column = "Cell_Line",
                  Drug_Name_Column = "Drug",
                  Drug_Concentration_Column = "Drug_Dose",
                  Efficacy_Column = "Efficacy",
                  LowerEfficacyIsBetterDrugEffect = isLowerEfficacyBetter,
                  Efficacy_Metric_Name = metricName,
                  Control_Treatment_Drugs = controlDrugs,
                  Control_Treatment_Drug_Concentrations = controlDoses,
                  Test_Treatment_Drugs = testDrugs,
                  Test_Treatment_Drug_Concentrations = testDoses,
                  Calculate_Uncertainty = calculateUncertainty,
                  Efficacy_SE_Column = eff_se_col,
                  n_Simulations = nSimulation,
                  Calculate_Hazard_Ratio = calculateHazardRatio,
                  Average_Duplicate_Records = averageDuplicateRecords
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
          return_value = list(res,warning_msg)
          names(return_value) <- c("table","warningMessage")
          return_value
        }
      )
      promise_race(future_result) %...>% {remove_modal_spinner()}#remove the spinnder
      
      return(future_result)
      
      
    })

    output$result <- renderDataTable({
      promise_all(data = computationResult()) %...>% with({
        if(!is.null(data$table))
          data$table[, names(data$table) != "Cell_Lines_Used"]
      })
    },options = list(scrollX = TRUE))
    
    output$log <- renderText({
      promise_all(data = computationResult()) %...>% with({
        data$warningMessage
      })
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(con) {
        promise_all(data = computationResult()) %...>% with({
          write_delim(data$table, con, delim = "\t")
        })
      }
    )
    
    output$downloadLog <- downloadHandler(
      filename = function() {
        paste('log-', Sys.Date(), '.txt', sep='')
      },
      content = function(file) {
        promise_all(data = computationResult()) %...>% with({
          write(data$warningMessage, file)
        })
      }
    )
    
    
    
  })
}
