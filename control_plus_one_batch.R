controlPlusOne.batch.fileInput <- function(id) {
  ns <- NS(id)
  fileInput(ns("controlTreatmentFile"), "Upload your input control treatment files",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
  )
}

controlPlusOne.batch.fileServer <- function(id) {
  moduleServer(id, function(input,output,session) {
    
    fileContent <- reactive({
      inFile <- input$controlTreatmentFile
      if(is.null(inFile))
        return(NULL)
      content <- read.delim(input$controlTreatmentFile$datapath)
      headers <- names(content)
      fixedHeaderNames <- c("Control_Treatment", "Doses", "Drug_To_Add")
      missedCol <- setdiff(fixedHeaderNames, headers)
      validate(
        need(length(missedCol) == 0, paste0("Warning: Please modify the column names to : ", missedCol))
      )
      content
    })
    
    fileContent
  })
}

controlPlusOne.batch.cellLineInput <- function(id) {
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

controlPlusOne.batch.cellLineServer <- function(id, dataset) {
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



controlPlusOne.batch.parametersInput <- function(id) {
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

controlPlusOne.batch.parametersServer <- function(id, fileType) {
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
         averageDuplicate = reactive(input$averageDuplicate))
  })
}


controlPlusOne.batch.nSimulationInput <- function(id) {
  ns <- NS(id)
  numericInput(inputId = ns("nSim"), label = "Number of random samples to be drawn when calculating output efficacy prediction uncertainties", value = 1000, min = 40, max = 5000)
}

controlPlusOne.batch.nSimulationServer <- function(id) {
  moduleServer(id, function(input,output,session){
    reactive(input$nSim)
  })
}


#efficacy metric input
controlPlusOne.batch.efficacyMetricInput <- function(id) {
  ns <- NS(id)
  textInput(ns("efficacyMetric"), "Your Efficacy Metric Name (can be empty)", "Viability", width = '70%')
}

controlPlusOne.batch.efficacyMetricServer <- function(id, fileType) {
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


controlPlusOne.batch.cellLinesThresholdInput <- function(id) {
  ns <- NS(id)
  numericInput(inputId = ns("clThreshold"), label = "Cell Lines Number Threshold", value = 10, min = 2, max = 100)
}

controlPlusOne.batch.cellLinesThresholdServer <- function(id) {
  moduleServer(id, function(input,output,server){
    reactive(input$clThreshold)
  })
}




controlPlusOne.batch.ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = NULL, status = "primary", solidHeader = TRUE, title="Control Treatment Input",
        h5("Please upload a text file containig control treatments information."),
        h6("Before uploading your file, please modify the header name to:"),
        h6("Control_Treatment;"),
        h6("Doses"),
        h6("Drug_To_Add"),
        h6("for each control treatment and its corresponding doses, splite each element by ','"),
        controlPlusOne.batch.fileInput(ns("ctFile")),
        controlPlusOne.batch.cellLinesThresholdInput(ns("cellLineThreshold")),
        controlPlusOne.batch.cellLineInput(ns("cellLineSelection")),
        tags$hr(),
        controlPlusOne.batch.parametersInput(ns("parametersCheck_batch")),
        controlPlusOne.batch.nSimulationInput(ns("n_simulation")),
        controlPlusOne.batch.efficacyMetricInput(ns("efficacyMetric_batch")),
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

controlPlusOne.batch.server <- function(id,fileInfo) {
  moduleServer(id, function(input,output,session) {
    dataset <- fileInfo$dataset
    
    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    ctInput <- controlPlusOne.batch.fileServer("ctFile")
    
    selectedCellLinesAndSubgroups <- controlPlusOne.batch.cellLineServer("cellLineSelection", dataset)
    
    selectedCellLine <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups
    
    checkedParameters <- controlPlusOne.batch.parametersServer("parametersCheck_batch", fileType)
    
    nSim <- controlPlusOne.batch.nSimulationServer("n_simulation")
    
    efficacyMetric <- controlPlusOne.batch.efficacyMetricServer("efficacyMetric_batch", fileType)
    
    logText <- reactiveVal(NULL)
    
    tableResult <- reactiveVal(NULL)
    
    output$log <- renderText(logText())
    
    output$table_batch <- DT::renderDataTable({
      if(!is.null(tableResult()))
        tableResult()[, names(result()) != "Cell_Lines_Used"]
      },options = list(scrollX = TRUE))
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(con) {
        write_delim(tableResult(), con, delim = "\t")
      }
    )
    
    clThreshold <- controlPlusOne.batch.cellLinesThresholdServer("cellLineThreshold")
    
    observeEvent(input$button_batch, {
      #check dataset
      if(is.null(dataset())){
        logText("Please Upload Your Data First!")
        return()
      }
      #check control treatments input files
      if(is.null(ctInput())){
        logText("Please Upload Your Control Treatment Text File!")
        return()
      }
      # retrieve data and write error message when filter the cell lines for each control treatment
          ## Initialize some variables
      all_cl <- unique(dataset()$Cell_Line)     #will be used to initialize "shared_cl" in each control treatment
      logText("") # initialize log. This will empty all the log shown with previous table result
      msg <- ""   # initialize error message. This is the main message of the log
      drugList <- list()   # control treatment drugs list
      doseList <- list()   # control treatment doses list
      drug_to_add_arr <- rep("",nrow(ctInput()))    # drug to add vector
      cl_num <- rep(0, nrow(ctInput()))    # shared cell line numbers vector
      shared_cl <- list()    #   shared cell lines list
      
          ##  begin data retrieve
      for(i in 1:nrow(ctInput())) {
        ### retrieve data from ctInput row
        ###    1. control treatment drugs
        drugList[[i]] <- strsplit(ctInput()$Control_Treatment[i], ",")[[1]]
        ###    2. control treatment doses
        doseList[[i]] <- strsplit(ctInput()$Doses[i], ",")[[1]]
        ###    3. drug to add
        drug_to_add_arr[i] <- ctInput()$Drug_To_Add[i]
        ### error handling of data retrieve
        validate(
          need(length(drugList[[i]])==length(doseList[[i]]), paste0("The number of drugs in control treatment doesn't match that of doses: Input Row ", i))
        )
        
        ### update the shared cell lines iteratively and store the final result into the list
        shared_cl[[i]] <- all_cl
        for(j in 1:length(drugList[[i]])){
          shared_cl[[i]] <- intersect(shared_cl[[i]], 
                                  unique(dataset()$Cell_Line[ dataset()$Drug == drugList[[i]][j]
                                                              & dataset()$Drug_Dose == doseList[[i]][j] ])
                                      )
        }
        shared_cl[[i]] <- intersect(shared_cl[[i]],
                               unique(dataset()$Cell_Line[ dataset()$Drug == ctInput()$Drug_To_Add[i] ]))
        shared_cl[[i]] <- shared_cl[[i]][ shared_cl[[i]] %in% selectedCellLine() ]
        ###   count the number of shared cell lines
        cl_num[i] <- length(shared_cl[[i]])
        ###   generate extra error message
        if(cl_num[i] < 2) {
          msg <- paste0(msg, "Row ", i, " doesn't have enough cell lines (>=2)\n")
        }
        else {
          msg <- paste0(msg, "Row ", i, " used ", cl_num[i], " cell lines\n")
        }
      }
          ## update log Text
      logText(paste0("Warning:\n", msg))
      
      ###  get usable rows index in original ctInput
      usable_index <- which(cl_num >= 2)
      
      # Now prepare prediction process
      ###   whether orignal dataset contain some extra cols
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL
      
          ## Localize reactive values in order to be used in definition of getRes
      ###   parameters
      isLowerEfficacy <- checkedParameters$isLowerEfficacy()
      uncertainty <- checkedParameters$uncertainty()
      comboscore <- checkedParameters$comboscore()
      averageDuplicate <- checkedParameters$averageDuplicate()
      ###   metric name
      efficacy_metric <- efficacyMetric()
      ###   dataset
      data <- dataset()
      ##   n_sim
      n_sim <- nSim()
      ###  now define getRes
      getRes <- function(i) {
        # prepare mono data of control treatment and of drug to add
        control_data <- data[ data$Drug %in% drugList[[i]] & data$Cell_Line %in% shared_cl[[i]] ,]
        for( j in 1:length(drugList[[i]]) ) {
          control_data <- control_data[!(control_data$Drug == drugList[[i]][j] & control_data$Drug_Dose != doseList[[i]][j]),]
        }

        drug_to_add_data <- data[data$Drug == drug_to_add_arr[i] & data$Cell_Line %in% shared_cl[[i]],]
        
        monoData <- rbindlist(list(control_data, drug_to_add_data))
        
        IDAPredict.ControlPlusOne(
          Monotherapy_Data = monoData,
          Cell_Line_Name_Column = "Cell_Line",
          Drug_Name_Column = "Drug",
          Drug_Concentration_Column = "Drug_Dose",
          Efficacy_Column = "Efficacy",
          LowerEfficacyIsBetterDrugEffect = isLowerEfficacy,
          Efficacy_Metric_Name = efficacy_metric,
          Control_Treatment_Drugs = drugList[[i]],
          Control_Treatment_Drug_Concentrations = doseList[[i]],
          Drug_to_Add = drug_to_add_arr[i],
          Calculate_Uncertainty = uncertainty,
          Efficacy_SE_Column = eff_se_col,
          n_Simulations = n_sim,
          Calculate_IDAcomboscore_And_Hazard_Ratio = comboscore,
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
          for(i in length(usable_index)) {
            index <- usable_index[i]
            res_list <- c(res_list, list(getRes(index)))
            incProgress(1/length(usable_index))
          }
          res <- res_list %>%
            lapply(function(x) {
              cbind(Control_Treatment = x$Control_Treatment, 
                    Drug_to_Add = x$Drug_to_Add,
                    Number_of_Cell_Lines_Used = length(x$Cell_Lines_Used),
                    Cell_Lines_Used = paste(x$Cell_Lines_Used, collapse = ", "),
                    x[[1]])
            }) %>%  rbindlist()
        })
      }
      else{# launch parallel computing
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
              cbind(Control_Treatment = x$Control_Treatment, 
                    Drug_to_Add = x$Drug_to_Add,
                    Number_of_Cell_Lines_Used = length(x$Cell_Lines_Used),
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
              cbind(Control_Treatment = x$Control_Treatment, 
                    Drug_to_Add = x$Drug_to_Add,
                    Number_of_Cell_Lines_Used = length(x$Cell_Lines_Used),
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




