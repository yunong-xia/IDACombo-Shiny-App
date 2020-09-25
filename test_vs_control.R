testVsControl.controlDrugInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("controlDrugs"),"Select Drugs in Control Treatment (Multiple)",
              choices = NULL,
              options = list(`actions-box` = TRUE,`liveSearchStyle` = "startsWith" , `liveSearch` = TRUE),
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

testVsControl.controlDoseServer <- function(id,dataset,selectedControlDrugs) {
  moduleServer(id, function(input,output,session) {
    output$controlDoseSelect <- renderUI({
      if(length(selectedControlDrugs()) > 0) {
        ns <- session$ns
        output = tagList()
        for(i in 1:length(selectedControlDrugs()) ){

          dose_choices <- (unique(dataset()$Drug_Dose[ dataset()$Drug == selectedControlDrugs()[i] ]))
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
              options = list(`actions-box` = TRUE,`liveSearchStyle` = "startsWith" , `liveSearch` = TRUE),
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

testVsControl.testDoseServer <- function(id,dataset,selectedTestDrugs) {
  moduleServer(id, function(input,output,session) {
    output$testDoseSelect <- renderUI({
      if(length(selectedTestDrugs()) > 0) {
        ns <- session$ns
        lapply(1:length(selectedTestDrugs()), function(i) {
          dose_choices <- sort(unique(dataset()$Drug_Dose[ dataset()$Drug == selectedTestDrugs()[i] ]))
          
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
                options = list(`liveSearchStyle` = "startsWith" , `liveSearch` = TRUE),
                multiple = T),
    actionButton(ns("selectAllSubgroups"),"Select All Subgroups"),
    actionButton(ns("deselectAllSubgroups"),"Deselect All Subgroups"),
    pickerInput(ns("cell_lines"),"Select Cell Lines",
                choices = NULL,
                options = list(`actions-box` = TRUE,`liveSearchStyle` = "startsWith" , `liveSearch` = TRUE,
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
    checkboxInput(ns("isLowerEfficacy"), "Lower Efficacy Is Better Drug Effect") %>%
      helper(type = "inline",
             title = "Lower Efficacy Is Better Drug Effect",
             icon = "question-circle", colour = NULL,
             content = c(
               "<p style='text-indent: 40px'>whether or not lower values efficacy indicate a more effective drug effect</p>"
             ),
             size = "s",
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    checkboxInput(ns("uncertainty"), "Calculate Uncertainty") %>%
      helper(type = "inline",
             title = "Calculate Uncertainty",
             icon = "question-circle", colour = NULL,
             content = c(
               "<p style= 'text-indent:40px'>whether or not a Monte Carlo simulation should be performed to estimate uncertainties in the efficacy predictions based on uncertainties in the monotherapy efficacy measurements.</p>"
             ),
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    conditionalPanel(condition = "input.uncertainty", ns = ns,
                     numericInput(inputId = ns("nSimulation"), label = "Number of random samples to be drawn when calculating output efficacy prediction uncertainties", value = 1000, min = 40, max = 5000)),
    checkboxInput(ns("comboscore"), "Calculate IDAComboscore And HazardRatios") %>%
      helper(type = "inline",
             title = "Calculate IDAComboscore And HazardRatios",
             icon = "question-circle", colour = NULL,
             content = c(
               "<p style = 'text-indent:40px'>whether or not IDA-Comboscores and Hazard Ratios (HRs) should be calculated between monotherapies and the drug combination.</p>"
             ),
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    checkboxInput(ns("averageDuplicate"),"Average Duplicate Records") %>%
      helper(type = "inline",
             title = "Average Duplicate Records",
             icon = "question-circle", colour = NULL,
             content = c(
               "<p style = 'text-indent:40px'>whether or not duplicated records (where a cell line has multiple records for being tested with a given drug at a given concentration) should be averaged</p>"
             ),
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      )
  )
}

testVsControl.parametersServer <- function(id, fileType) {
  moduleServer(id, function(input,output,session) {
    observeEvent(fileType(),{
      if(fileType() == "GDSC" || fileType() == "CTRPv2"){
        updateCheckboxInput(session, "isLowerEfficacy", "Lower Efficacy Is Better Drug Effect", value = TRUE)
        disable("isLowerEfficacy")
      }
      else{
        enable("isLowerEfficacy")
      }
    })
    
    list(isLowerEfficacy = reactive(input$isLowerEfficacy),
         uncertainty = reactive(input$uncertainty),
         comboscore = reactive(input$comboscore),
         averageDuplicate = reactive(input$averageDuplicate),
         nSim = reactive(input$nSimulation))
  })
}

testVsControl.nSimulationInput <- function(id) {
  ns <- NS(id)
  numericInput(inputId = ns("nSim"), label = "Number of random samples to be drawn when calculating output efficacy prediction uncertainties", value = 1000, min = 40, max = 5000)
}

testVsControl.nSimulationServer <- function(id) {
  moduleServer(id, function(input,output,session){
    reactive(input$nSim)
  })
}



#efficacy metric input
testVsControl.efficacyMetricInput <- function(id) {
  ns <- NS(id)
  textInput(ns("efficacyMetric"), "Your Efficacy Metric Name (can be empty)", "Viability", width = '70%')
}

testVsControl.efficacyMetricServer <- function(id, fileType) {
  moduleServer(id, function(input,output,session) {
    observeEvent(fileType(), {
      if(fileType() == "GDSC" || fileType() == "CTRPv2"){
        updateTextInput(session, "efficacyMetric", label = "Your Efficacy Metric Name (can be empty)", value = "Viability")
        disable("efficacyMetric")
      }
      else{
        enable("efficacyMetric")
      }
    })
    
    reactive(input$efficacyMetric)
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
        testVsControl.nSimulationInput(ns("n_simulation")),
        testVsControl.efficacyMetricInput(ns("efficacyMetric")),
        tags$hr(),
        actionButton(ns("button"), "RUN")
    ),
    box(width = 9, status = "primary", solidHeader = TRUE, title="Test VS Control Result",
        downloadButton(ns('downloadData'), 'Download DataTable'),
        
        conditionalPanel(condition = "input.button",ns = ns, tabsetPanel(type = "tabs",
                                                                         tabPanel("Table", withSpinner(DT::dataTableOutput(ns("result")))))  
        )
    )
  )
}


testVsControl.server <- function(id, fileInfo) {
  moduleServer(id, function(input,output,session) {
    dataset <- fileInfo$dataset
    
    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    selectedControlDrugs <- testVsControl.controlDrugServer("controlDrugSelection", dataset)
    
    selectedControlDoses <- testVsControl.controlDoseServer("controlDoseSelection", dataset, selectedControlDrugs)
    
    selectedTestDrugs <- testVsControl.testDrugServer("testDrugSelection", dataset)
    
    selectedTestDoses <- testVsControl.testDoseServer("testDoseSelection", dataset, selectedTestDrugs)
    
    selectedCellLinesAndSubgroups <- testVsControl.CellLineServer("cellLineSelection",dataset)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups
    
    checkedParameters <- testVsControl.parametersServer("parametersCheck", fileType)
    
    nSim <- checkedParameters$nSim
    
    efficacyMetric <- testVsControl.efficacyMetricServer("efficacyMetric", fileType)
    
    result <- eventReactive(input$button, {
      validate(
        need(!is.null(dataset()), "Please upload your data"),
        need(!is.null(selectedControlDrugs()), "Please select control treatment drugs"),
        need(!is.null(selectedCellLines()), "Please select Cell lines"),
        need(length(selectedControlDoses()) == length(selectedControlDrugs()), "Please select control treatment doses"),
        need(!is.null(selectedTestDrugs()), "Please select test treatment drugs"),
        need(length(selectedTestDoses()) == length(selectedTestDrugs()), "Please select test treatment doses")
      )
      
      usable_shared_cl <- selectedCellLines()
      for(i in 1:length(selectedControlDrugs())) {
        usable_shared_cl <- intersect(usable_shared_cl, 
                               unique(dataset()$Cell_Line[ dataset()$Drug == selectedControlDrugs()[i] & dataset()$Drug_Dose == selectedControlDoses()[i] ]))
      }
      for(i in 1:length(selectedTestDrugs())) {
        usable_shared_cl <- intersect(usable_shared_cl, 
                               unique(dataset()$Cell_Line[ dataset()$Drug == selectedTestDrugs()[i] & dataset()$Drug_Dose == selectedTestDoses()[i] ]))
      }
      
      validate(
        need(usable_shared_cl >= 2, "Usable shared cell lines among control and test treatments are less than 2")
      )
      
      controlData <- dataset() %>% 
        filter(Drug %in% selectedControlDrugs(),
               Cell_Line %in% usable_shared_cl)
      for(i in 1:length(selectedControlDrugs())){
        controlData <- controlData[!(controlData$Drug == selectedControlDrugs()[i] & controlData$Drug_Dose != selectedControlDoses()[i]),]
      }
      
      testData <- dataset() %>% 
        filter(Drug %in% selectedTestDrugs(),
               Cell_Line %in% usable_shared_cl)
      for(i in 1:length(selectedTestDrugs())){
        testData <- testData[!(testData$Drug == selectedTestDrugs()[i] & testData$Drug_Dose != selectedTestDoses()[i]),]
      }
      
      
      monoData <- rbindlist(list(controlData,testData))
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL
      
      res_list <- IDAPredict.TestvsControl(
        Monotherapy_Data = monoData,
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
        Calculate_Hazard_Ratio = checkedParameters$comboscore(),
        Average_Duplicate_Records = checkedParameters$averageDuplicate()
      )
      
      res <- cbind(Control_Treatment_Drugs = paste(res_list$Control_Treatment$Control_Treatment_Drugs, collapse = ", "), 
                   Control_Treatment_Drug_Concentration = paste(res_list$Control_Treatment$Control_Treatment_Drug_Concentrations, collapse = ", "),
                   Test_Treatment_Drugs = paste(res_list$Test_Treatment$Test_Treatment_Drugs, collapse = ", "),
                   Test_Treatment_Drugs = paste(res_list$Test_Treatment$Test_Treatment_Drug_Concentrations, collapse = ", "),
                   Number_of_Cell_Lines_Used = length(res_list$Cell_Lines_Used),
                   Cell_Lines_Used = paste(res_list$Cell_Lines_Used, collapse = ", "),
                   res_list[[1]])
    })
    
    output$result <- renderDataTable({
      if(!is.null(result()))
        result()[, names(result()) != "Cell_Lines_Used"]
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
