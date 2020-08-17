testVsControl.batch.fileInput <- function(id) {
  ns <- NS(id)
  fileInput(ns("controlTreatmentFile"), "Upload your input control treatment files",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
  )
}

testVsControl.batch.fileServer <- function(id) {
  moduleServer(id, function(input,output,session) {
    
    fileContent <- reactive({
      inFile <- input$controlTreatmentFile
      if(is.null(inFile))
        return(NULL)
      content <- read.delim(input$controlTreatmentFile$datapath)
      headers <- names(content)
      fixedHeaderNames <- c("Control_Treatment_Drugs", "Control_Treatment_Doses", "Test_Treatment_Drugs", "Test_Treatment_Doses")
      missedCol <- setdiff(fixedHeaderNames, headers)
      validate(
        need(length(missedCol) == 0, paste0("Warning: Please modify the column names to : ", missedCol))
      )
      content
    })
    
    fileContent
  })
}

testVsControl.batch.cellLineInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("subgroups"),"Select Cell Lines By Subgroups",
                choices = NULL,
                options = list(`actions-box` = TRUE,`liveSearchStyle` = "startsWith" , `liveSearch` = TRUE),
                multiple = T),
    pickerInput(ns("cell_lines"),"Cell-Line available for both drugs (Multiple)",
                choices = list(
                  `Cancer Cell Lines` = NULL),
                options = list(`actions-box` = TRUE,`liveSearchStyle` = "startsWith" , `liveSearch` = TRUE,
                               `selected-text-format`= "count",
                               `count-selected-text` = "{0} models choosed (on a total of {1})"),
                multiple = T)
  )
}

testVsControl.batch.cellLineServer <- function(id, dataset) {
  moduleServer(id, function(input,output,session) {
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
      updatePickerInput(session, inputId = "cell_lines", label = "Cell-Line available for both drugs (Multiple)",
                        choices = cell_line_choices())
      updatePickerInput(session, inputId = "subgroups", label = "Select All Cell Lines By Subgroups",
                        selected = NULL,
                        choices = c("Custom",subgroups_choices()))
    })
    
    prev_selected_cell_lines <- reactiveVal(value = NULL)
    prev_selected_subgroups <- reactiveVal(value = NULL)
    
    observeEvent(input$subgroups, {
      if("Custom" %in% prev_selected_subgroups()) { ## Previously selected "Custom"
        if("Custom" %in% input$subgroups && length(input$subgroups)>1) { ## check other subgroups. 
          new_subgroups <- setdiff(input$subgroups , "Custom")
          new_cell_lines <- cl_sg_set()$Cell_Line[cl_sg_set()$Cell_Line_Subgroup %in% new_subgroups]
          prev_selected_cell_lines(new_cell_lines)
          prev_selected_subgroups(new_subgroups)
          updatePickerInput(session, inputId = "cell_lines", label = "Cell-Line available for both drugs (Multiple)",
                            selected = new_cell_lines,
                            choices = cell_line_choices())
          updatePickerInput(session, inputId = "subgroups", label = "Select Cell Lines By Subgroups",
                            selected = new_subgroups,
                            choices = c("Custom",subgroups_choices()))
          
          
        }
        else if(is.null(input$subgroups)){  # when users deselect "Custom"
          prev_selected_cell_lines(NULL)
          updatePickerInput(session, inputId = "cell_lines", label = "Cell-Line available for both drugs (Multiple)",
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
        else {
          new_subgroups <- input$subgroups
          new_cell_lines <- cl_sg_set()$Cell_Line[cl_sg_set()$Cell_Line_Subgroup %in% new_subgroups]
          prev_selected_cell_lines(new_cell_lines)
          prev_selected_subgroups(new_subgroups)
          updatePickerInput(session, inputId = "cell_lines", label = "Cell-Line available for both drugs (Multiple)",
                            selected = new_cell_lines,
                            choices = cell_line_choices())
          updatePickerInput(session, inputId = "subgroups", label = "Select Cell Lines By Subgroups",
                            selected = new_subgroups,
                            choices = c("Custom",subgroups_choices()))
        }
      }
    }, ignoreNULL = F)
    
    observeEvent(input$cell_lines, {
      # ignoreNULL = T by default
      
      if("Custom" %in% input$subgroups){
        prev_selected_cell_lines(input$cell_lines)
      }
      else { 
        # check whether the selected cell line match those subgroups.
        if(nrow(cl_sg_set()[cl_sg_set()$Cell_Line_Subgroup %in% input$subgroups, ]) != length(input$cell_lines)){
          prev_selected_cell_lines(input$cell_lines)
          prev_selected_subgroups(input$subgroups)
          updatePickerInput(session, inputId = "subgroups", 
                            label = "Select Cell Lines By Subgroups",
                            selected = "Custom",
                            choices = c("Custom",subgroups_choices()))
        }
      }
    })
    
    list(
      cellLines = reactive(input$cell_lines),
      subgroups = reactive(input$subgroups)
    )
  })
  
}



testVsControl.batch.parametersInput <- function(id) {
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
    checkboxInput(ns("hazardRatio"), "Calculate Hazard Ratios") %>%
      helper(type = "inline",
             title = "Calculate Hazard Ratios",
             icon = "question-circle", colour = NULL,
             content = c(
               "<p style = 'text-indent:40px'>whether or not a Hazard Ratios (HR) should be calculated between the control and test treatments. Check if so. Should only be checked for efficacy metrics that range between 0 and 1.</p>"
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

testVsControl.batch.parametersServer <- function(id, fileType) {
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
         hazardRatio = reactive(input$hazardRatio),
         averageDuplicate = reactive(input$averageDuplicate))
  })
}

testVsControl.batch.nSimulationInput <- function(id) {
  ns <- NS(id)
  numericInput(inputId = ns("nSim"), label = "Number of random samples to be drawn when calculating output efficacy prediction uncertainties", value = 1000, min = 40, max = 5000)
}

testVsControl.batch.nSimulationServer <- function(id) {
  moduleServer(id, function(input,output,session){
    reactive(input$nSim)
  })
}

#efficacy metric input
testVsControl.batch.efficacyMetricInput <- function(id) {
  ns <- NS(id)
  textInput(ns("efficacyMetric"), "Your Efficacy Metric Name (can be empty)", "Viability", width = '70%')
}

testVsControl.batch.efficacyMetricServer <- function(id, fileType) {
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


testVsControl.batch.cellLinesThresholdInput <- function(id) {
  ns <- NS(id)
  numericInput(inputId = ns("clThreshold"), label = "Cell Lines Number Threshold", value = 10, min = 2, max = 100)
}

testVsControl.batch.cellLinesThresholdServer <- function(id) {
  moduleServer(id, function(input,output,server){
    reactive(input$clThreshold)
  })
}




testVsControl.batch.ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = NULL, status = "primary", solidHeader = TRUE, title="Control Treatment Input",
        h5("Please upload a text file containig control treatments information."),
        h6("Before uploading your file, please modify the header name to:"),
        h6("Control_Treatment_Drugs;"),
        h6("Control_Treatment_Doses;"),
        h6("Test_Treatment_Drugs;"),
        h6("Test_Treatment_Doses;"),
        h6("for each control treatment and its corresponding doses, splite each element by ','"),
        testVsControl.batch.fileInput(ns("batchInputFile")),
        testVsControl.batch.cellLinesThresholdInput(ns("cellLineThreshold")),
        testVsControl.batch.cellLineInput(ns("cellLineSelection")),
        tags$hr(),
        testVsControl.batch.parametersInput(ns("parametersCheck_batch")),
        testVsControl.batch.nSimulationInput(ns("n_sim")),
        testVsControl.batch.efficacyMetricInput(ns("efficacyMetric_batch")),
        tags$hr(),
        actionButton(ns("button_batch"), "RUN")
    ),
    box(width = NULL, status = "primary", solidHeader = TRUE, title = "Result",
        downloadButton(ns('downloadData_batch'), 'Download DataTable'),
        wellPanel(verbatimTextOutput(ns("log"))),
        conditionalPanel(condition = "input.button_batch",ns = ns, withSpinner(DT::dataTableOutput(ns("table_batch")))  
        ))
  )
}

testVsControl.batch.server <- function(id,fileInfo) {
  moduleServer(id, function(input,output,session) {
    dataset <- fileInfo$dataset
    
    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    batchInput <- testVsControl.batch.fileServer("batchInputFile")
    
    selectedCellLineAndSubgroups <- testVsControl.batch.cellLineServer("cellLineSelection", dataset)
    
    selectedCellLine <- selectedCellLineAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLineAndSubgroups$subgroups
    
    checkedParameters <- testVsControl.batch.parametersServer("parametersCheck_batch", fileType)
    
    efficacyMetric <- testVsControl.batch.efficacyMetricServer("efficacyMetric_batch", fileType)
    
    nSim <- testVsControl.batch.nSimulationServer("n_sim")
    
    logText <- reactiveVal(NULL)
    
    tableResult <- reactiveVal(NULL)
    
    output$log <- renderText(logText())
    
    output$table_batch <- DT::renderDataTable(tableResult(),
                                              options = list(scrollX = TRUE))
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(con) {
        write_delim(tableResult(), con, delim = "\t")
      }
    )
    
    clThreshold <- testVsControl.batch.cellLinesThresholdServer("cellLineThreshold")
    
    observeEvent(input$button_batch, {
      #check dataset
      if(is.null(dataset())){
        logText("Please Upload Your Data First!\n")
        return()
      }
      #check control treatments input files
      if(is.null(batchInput())){
        logText("Please Upload Your Control Treatment Text File!")
        return()
      }
      
      validate(
        need(!is.null(dataset()), "Please upload your dataset"),
        need(!is.null(batchInput()), "Please upload your input file"),
        need(!is.null(selectedCellLine()), "Please select cell lines")
      )
      
      # retrieve info in ctInput
      ## null file error handling
      if(nrow(batchInput()) == 0){
        logText("No data in your input file\n")
        return()
      }
      
      controlTreatmentList <- list()
      testTreatmentList <- list()
      
      for(i in 1:nrow(batchInput())){
        ctDrugs <- strsplit(batchInput()[i,]$Control_Treatment_Drugs, ",")[[1]]
        ctDoses <- strsplit(batchInput()[i,]$Control_Treatment_Doses, ",")[[1]]
        testDrugs <- strsplit(batchInput()[i,]$Test_Treatment_Drugs, ",")[[1]]
        testDoses <- strsplit(batchInput()[i,]$Test_Treatment_Doses, ",")[[1]]
        validate(
          need(length(ctDrugs) == length(ctDoses), "Length of control treatment drugs and doses does not match"),
          need(length(testDrugs) == length(testDoses), "Length of test treatment drugs and doses does not match")
        )
        controlTreatmentList[[i]] <- data.frame(Drug = ctDrugs, 
                                           Dose = ctDoses)
        testTreatmentList[[i]] <- data.frame(Drug = testDrugs, 
                                        Dose = testDoses)
      }
      
      sharedCellLineList <- list()
      cl_num <- c()
      msg <- ""
      #iterative way to find shared cell lines among each test-vs-control pair.
      for(i in 1:nrow(batchInput())){
        sharedCellLineList[[i]] <- selectedCellLine()
        for(j in 1:nrow(controlTreatmentList[[i]])) {
          sharedCellLineList[[i]] <- intersect(sharedCellLineList[[i]], 
                                      unique(dataset()$Cell_Line[ dataset()$Drug == controlTreatmentList[[i]]$Drug[j] & dataset()$Drug_Dose == controlTreatmentList[[i]]$Dose[j] ]))
        }
        for(j in 1:nrow(testTreatmentList[[i]])) {
          sharedCellLineList[[i]] <- intersect(sharedCellLineList[[i]], 
                                               unique(dataset()$Cell_Line[ dataset()$Drug == testTreatmentList[[i]]$Drug[j] & dataset()$Drug_Dose == testTreatmentList[[i]]$Dose[j] ]))
        }
        cl_num[i] <- length(sharedCellLineList[[i]])
        if(cl_num[i] < 2)
          msg <- paste0(msg, "Row ", i, " doesn't have enough shared cell lines (>=2)\n")
        else{
          msg <- paste0(msg, "Row ", i, " used ", cl_num[i], " cell lines\n")
        }
      }
      
      logText(paste0("Warning:\n", msg))
      
      usable_index <- which(cl_num >= 2)
      
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL

      isLowerEfficacy <- checkedParameters$isLowerEfficacy()
      uncertainty <- checkedParameters$uncertainty()
      hazardRatio <- checkedParameters$hazardRatio()
      averageDuplicate <- checkedParameters$averageDuplicate()
      efficacy_metric <- efficacyMetric()
      n_sim <- nSim()
      data <- dataset()
      getRes <- function(i) {
        controlData <- apply(controlTreatmentList[[i]], 1,function(row){
          data[ data$Drug == row[1] & data$Drug_Dose == row[2],]
        }) %>%
          rbindlist()
        
        testData <- apply(testTreatmentList[[i]], 1,function(row){
          data[ data$Drug == row[1] & data$Drug_Dose == row[2],]
        }) %>%
          rbindlist()
        
        monoData <- rbindlist(list(controlData, testData))
        
        ida_res <- IDAPredict.TestvsControl(
          Monotherapy_Data = monoData,
          Cell_Line_Name_Column = "Cell_Line",
          Drug_Name_Column = "Drug",
          Drug_Concentration_Column = "Drug_Dose",
          Efficacy_Column = "Efficacy",
          LowerEfficacyIsBetterDrugEffect = isLowerEfficacy,
          Efficacy_Metric_Name = efficacy_metric,
          Control_Treatment_Drugs = controlTreatmentList[[i]]$Drug,
          Control_Treatment_Drug_Concentrations = controlTreatmentList[[i]]$Dose,
          Test_Treatment_Drugs = testTreatmentList[[i]]$Drug,
          Test_Treatment_Drug_Concentrations = testTreatmentList[[i]]$Dose,
          Calculate_Uncertainty = uncertainty,
          Efficacy_SE_Column = eff_se_col,
          n_Simulations = n_sim,
          Calculate_Hazard_Ratio = hazardRatio,
          Average_Duplicate_Records = averageDuplicate
        )
      }
      
      res <- NULL
      if(length(usable_index) < 1){
        res <- NULL
      }
      else if(length(usable_index) <= 30) {
        withProgress(message = 'Computing...', value = 0, {
          res_list <- list()
          for(i in 1:length(usable_index)) {
            index <- usable_index[i]
            res_list <- c(res_list, list(getRes(index)))
            incProgress(1/length(usable_index))
          }
          res <- res_list %>%
            lapply(function(x) {
              cbind(Control_Treatment_Drugs = paste(x$Control_Treatment$Control_Treatment_Drugs, collapse = ", "), 
                    Control_Treatment_Drug_Concentration = paste(x$Control_Treatment$Control_Treatment_Drug_Concentrations, collapse = ", "),
                    Test_Treatment_Drugs = paste(x$Test_Treatment$Test_Treatment_Drugs, collapse = ", "),
                    Test_Treatment_Drugs = paste(x$Test_Treatment$Test_Treatment_Drug_Concentrations, collapse = ", "),
                    Cell_Lines_Used = paste(x$Cell_Lines_Used, collapse = ", "),
                    x[[1]])
            }) %>%  rbindlist()
        })
      }
      else{#parallel computing
        progress = AsyncProgress$new(message="Computing...")
        res_list <- list()
        Runs = 4
        for(i in 1:Runs) {
          range <- floor(length(usable_index)/Runs)
          test_seq <- ((i-1)*range+1):(i*range)
          res_list[[i]] <- future({
            subRes_list <- list()
            for(j in test_seq){
              index <- usable_index[j]
              subRes_list <- c(subRes_list,list(getRes(index)))
              progress$inc(1/length(usable_index))
            }
            subRes <- subRes_list %>% lapply(function(x) {
              cbind(Control_Treatment_Drugs = paste(x$Control_Treatment$Control_Treatment_Drugs, collapse = ", "), 
                    Control_Treatment_Drug_Concentration = paste(x$Control_Treatment$Control_Treatment_Drug_Concentrations, collapse = ", "),
                    Test_Treatment_Drugs = paste(x$Test_Treatment$Test_Treatment_Drugs, collapse = ", "),
                    Test_Treatment_Drugs = paste(x$Test_Treatment$Test_Treatment_Drug_Concentrations, collapse = ", "),
                    Cell_Lines_Used = paste(x$Cell_Lines_Used, collapse = ", "),
                    x[[1]])
            }) %>%  rbindlist()
            return(subRes)
          })
        }
        
        remainder <- future({
          if(length(usable_index) %% Runs != 0){
            range <- floor(length(usable_index)/Runs)
            test_seq <- (Runs*range+1):length(usable_index)
            subRes_list <- list()
            for(j in test_seq){
              index <- usable_index[j]
              subRes_list <- c(subRes_list,list(getRes(index)))
              progress$inc(1/length(usable_index))
            }
            subRes <- subRes_list %>% lapply(function(x) {
              cbind(Control_Treatment_Drugs = paste(x$Control_Treatment$Control_Treatment_Drugs, collapse = ", "), 
                    Control_Treatment_Drug_Concentration = paste(x$Control_Treatment$Control_Treatment_Drug_Concentrations, collapse = ", "),
                    Test_Treatment_Drugs = paste(x$Test_Treatment$Test_Treatment_Drugs, collapse = ", "),
                    Test_Treatment_Drugs = paste(x$Test_Treatment$Test_Treatment_Drug_Concentrations, collapse = ", "),
                    Cell_Lines_Used = paste(x$Cell_Lines_Used, collapse = ", "),
                    x[[1]])
            }) %>%  rbindlist()
            return(subRes)
          }else{
            return(data.frame())
          }
        })
        res <- promise_all(a = res_list[[1]], b = res_list[[2]], c = res_list[[3]], d = res_list[[4]], e = remainder) %...>%
          with({
            rbindlist(list(a,b,c,d,e))
          })
      }
      
      
      tableResult(res)
    })
  })
}




