


#batch input
twoDrugs.batch.drugInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("fixed_drug"),"Select the fixed drug",
                choices = NULL,
                options = list(`actions-box` = TRUE,`live-search-style` = "startsWith" , `live-search` = TRUE),
                multiple = F),
    pickerInput(ns("drugs_to_add"),"Select drugs to add",
                choices = NULL,
                options = list(`actions-box` = TRUE,`live-search-style` = "startsWith" , `live-search` = TRUE),
                multiple = T),
    checkboxInput(ns("use_no_Csus_drugs"),label = "Include drugs which do not have Csustained Concentration", value = TRUE)
  )
}

twoDrugs.batch.drugServer <- function(id, dataset) {
  moduleServer(id, function(input,output,session) {
    
    observeEvent(c(dataset(), input$use_no_Csus_drugs), {
      #If checkbox value is FALSE, then provide drugs that have Csustained monotherapy data
      if(input$use_no_Csus_drugs == FALSE && "with_Csus_conc" %in% names(dataset())){
        drug_choices <- unique(dataset()$Drug[dataset()$with_Csus_conc])
      }
      #If TRUE, then provide all drugs
      else{
        drug_choices <- unique(dataset()$Drug)
      }
      updatePickerInput(session, inputId = "fixed_drug", label = "Select the fixed drug",
                        choices = drug_choices)
      updatePickerInput(session, inputId = "drugs_to_add", label = "Select drugs to add",
                        choices = drug_choices)
    })
    
    list(
      fixed_drug = reactive(input$fixed_drug),
      drugs_to_add = reactive(input$drugs_to_add)
    )
  })
}

# 2Drug parameters and there helpers
twoDrugs.batch.parametersInput <- function(id) {
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
    checkboxInput(ns("comboscore"), "Calculate IDAComboscore And HazardRatios") %>%
      helper(type = "inline",
             title = "Calculate IDAComboscore And HazardRatios",
             icon = "question-circle", colour = NULL,
             content = "Should IDA-Comboscores and Hazard Ratios (HRs) be calculated between monotherapies and the drug combination? Note that these values are only meaningful when efficacy values are scaled between 0 and 1 (i.e. viability, normalized AUC, etc.).",
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    checkboxInput(ns("averageDuplicate"),"Average Duplicate Records", value = T) %>%
      helper(type = "inline",
             title = "Average Duplicate Records",
             icon = "question-circle", colour = NULL,
             content =  "Should duplicated records (i.e. where a cell line has multiple records for being tested with a given drug at a given concentration) should be averaged? If this option is not selected and duplicates are found, IDACombo will skip all except the first occurence of each duplicate.",
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    checkboxInput(ns("CsusRestrict"), "Only use concentration between 0 and Csustained", value = F) %>%
      helper(type = "inline",
             title = "Only use concentration between 0 and Csustained",
             icon = "question-circle", colour = NULL,
             content =  "Should restrict the analysis to only use concentrations from 0 up to Csustained for selected drugs that have Csustained available (would still use full concentration range for selected drugs that do not have Csustained available.",
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      )
  )
}

twoDrugs.batch.parametersServer <- function(id, fileType, isLowerEfficacy) {
  moduleServer(id, function(input,output,session) {
    
    list(isLowerEfficacy = isLowerEfficacy,
         uncertainty = reactive(input$uncertainty),
         comboscore = reactive(input$comboscore),
         averageDuplicate = reactive(input$averageDuplicate),
         CsusRestrict = reactive(input$CsusRestrict),
         nSim = reactive(input$nSimulation))
  })
}


twoDrugs.batch.ui <- function(id) {
  ns <- NS(id) 
  tagList(
    box(width = 3, status = "primary", solidHeader = TRUE, title="Batch Process Input",
        twoDrugs.batch.drugInput(ns("drugSelection_batch")),
        global.cellLineInput(ns("cellLineSelection_batch")),
        tags$hr(),
        twoDrugs.batch.parametersInput(ns("parametersCheck_batch")),
        tags$hr(),
        uiOutput(ns("RAM_warning_placeholder")),
        actionButton(ns("button_batch"), "RUN")
    ),
    box(width = 9, status = "primary", solidHeader = TRUE, title="2Drug Batch Process Result",
        downloadButton(ns('downloadData'), 'Download DataTable'),
        downloadButton(ns('downloadLog'), 'Download Log File'),
        conditionalPanel(condition = "input.button_batch",ns = ns, tabsetPanel(type = "tabs",
                                                                               tabPanel("Table", withSpinner(dataTableOutput(ns("table")))),
                                                                               tabPanel('Log', withSpinner(verbatimTextOutput(ns('log')))))
        )
    )
  )
}




twoDrugs.batch.server <- function(id, fileInfo) {
  moduleServer(id, function(input, output, session) {
    # retrieve parts of fileInfo
    dataset <- fileInfo$dataset
    
    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    cellLinesAndSubgroups <- fileInfo$cellLinesAndSubgroups
    
    efficacyMetric <- fileInfo$efficacyMetric
    
    isLowerEfficacy <- fileInfo$isLowerEfficacy
    
    # get user Input
    selectedFixedDrugAndDrugsToAdd <- twoDrugs.batch.drugServer("drugSelection_batch",dataset)
    
    selectedFixedDrug <- selectedFixedDrugAndDrugsToAdd$fixed_drug
    
    selectedDrugsToAdd <- selectedFixedDrugAndDrugsToAdd$drugs_to_add

    selectedCellLinesAndSubgroups <- global.cellLineServer("cellLineSelection_batch",cellLinesAndSubgroups)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups

    checkedParameters <- twoDrugs.batch.parametersServer("parametersCheck_batch", fileType, isLowerEfficacy)
    
    nSim <- checkedParameters$nSim
    
    #Rendering Action button to do viability calculations
    #Creating reactiveTimer to check whether or not this UI should exist once per 10 seconds
    RAM_timer <- reactiveTimer(10000)
    
    #Checking RAM usage
    RAM_Free_Ratio <- eventReactive(RAM_timer(), {
      warning("Checking Ram")
      gc()
      ram <- memuse::Sys.meminfo()
      ram$freeram/ram$totalram
    })
    
    #Rendering UI
    output$RAM_warning_placeholder <- renderUI({
      warning("Rendering UI")
      if(RAM_Free_Ratio() < min_RAM_free_ratio_to_start_future){
        wellPanel(
          p(HTML("<b>Due to high server usage, no further batch processing calculations can be initiated at this time. Please try again later. We apologize for the inconvenience.</b>"), style = "color:red") 
        )
      }else({
        return(NULL)
      })
    })
    
    observeEvent(RAM_Free_Ratio(),{
      if(RAM_Free_Ratio() < min_RAM_free_ratio_to_start_future){
        disable("button_batch")
      }else{
        enable("button_batch")
      }
    })
    
    #Calculate IDACombo result.
    computationResult <- eventReactive(input$button_batch,{
      validate(
        need(!is.null(dataset()), "Please upload your data"),
        need(!is.null(selectedFixedDrug()), "Please select the first drug"),
        need(!is.null(selectedDrugsToAdd()), "Please select drugs to add"),
        need(!is.null(selectedCellLines()), "Please select Cell lines")
      )
      
      
      show_modal_spinner(
        spin = "semipolar",
        color = "#112446",
        text = "Calculating Efficacy"
      )
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL
      
      isLowerEfficacy <- checkedParameters$isLowerEfficacy()
      efficacyMetric <- efficacyMetric()
      uncertainty <- checkedParameters$uncertainty()
      nSim <- nSim()
      comboscore <- checkedParameters$comboscore()
      averageDuplicate <- checkedParameters$averageDuplicate()
      monotherapy_data <- dataset()[dataset()$Cell_Line %in% selectedCellLines() & dataset()$Drug %in% c(selectedFixedDrug(),selectedDrugsToAdd()),]
      fixedDrug <- selectedFixedDrug()
      drugsToAdd <- selectedDrugsToAdd()
      
      #if restricted to analysis of conc in [0, Csustained]
      if(checkedParameters$CsusRestrict() == TRUE){
        monotherapy_data <- monotherapy_data %>%
          filter(in_range == T)
      }
      
      progress <- AsyncProgress$new(session, min = 0, max = length(drugsToAdd), message = "Initializing Calculation......")
      future_result <- future(
        expr = {
        warning_msg <- ""
        collected_result <- NULL
        progress$set(value = 0, message = paste0(0, " of ", length(drugsToAdd), " Combinations Complete..."))
        for(i in 1:length(drugsToAdd)) {
          #Checking if sufficient RAM exists to continue calculation
          temp_ram <- memuse::Sys.meminfo()
          temp_free_ram_ratio <- temp_ram$freeram/temp_ram$totalram
          if(temp_free_ram_ratio <= min_RAM_free_ratio_within_future){
            attempt <- 1
            while(temp_free_ram_ratio <= min_RAM_free_ratio_within_future & attempt <= 10){
              progress$set(value = i-1, message = paste0("WARNING: Server RAM low. Trying to continue...(Attempt ", attempt, " of 10)"))
              Sys.sleep(10)
              temp_ram <- memuse::Sys.meminfo()
              temp_free_ram_ratio <- temp_ram$freeram/temp_ram$totalram
              attempt <- attempt + 1
            }
            if(temp_free_ram_ratio > min_RAM_free_ratio_within_future){
              progress$set(value = i-1, message = paste0(i-1, " of ", length(drugsToAdd), " Compounds Complete..."))
            } else {
              Aborted <- i-1
              progress$set(value = i-1, message = "Aborting calculation...")
              warning_msg <- paste0("WARNING: Calculation aborted after ", Aborted, " of ", length(drugsToAdd)," compounds complete. WARNING: Calculation failed. Please reload the web page and try again. Contact us if this problem persists.")
              break
            }
          }
          
          #get mono data
          ith_result <- withCallingHandlers(
            tryCatch(
              expr = {
                res <- IDAPredict.2drug(
                  Monotherapy_Data = monotherapy_data,
                  Cell_Line_Name_Column = "Cell_Line",
                  Drug_Name_Column = "Drug",
                  Drug_Concentration_Column = "Drug_Dose",
                  Efficacy_Column = "Efficacy",
                  LowerEfficacyIsBetterDrugEffect = isLowerEfficacy,
                  Efficacy_Metric_Name = efficacyMetric,
                  Drug1 = fixedDrug,
                  Drug2 = drugsToAdd[i],
                  Calculate_Uncertainty = uncertainty,
                  Efficacy_SE_Column = eff_se_col,
                  n_Simulations = nSim,
                  Calculate_IDAcomboscore_And_Hazard_Ratio = comboscore,
                  Average_Duplicate_Records = averageDuplicate
                )
                if(!is.data.frame(res[[1]])){
                  NULL
                } else{
                  orgnaized_res <- cbind(
                    Drug1 = res$Drug1,
                    Drug2 = res$Drug2,
                    res$Efficacy_Predictions,
                    Cell_Lines_Used = paste(res$Cell_Lines_Used, collapse = ", "),
                    Number_of_Cell_Line_Used = length(res$Cell_Lines_Used))
                  rm(res)
                  orgnaized_res
                }
              }),
            warning = function(w) { #warning handling. This won't halt the computation. It only collect warning message.
              warning_msg <<- paste0(warning_msg, paste0(Sys.Date(),": ",conditionMessage(w),"\n"))
              invokeRestart("muffleWarning")
            }
          )
          if(is.null(collected_result)){
            collected_result <- ith_result
          }
          else{
            collected_result <- rbindlist(list(collected_result,ith_result))
          }
          rm(ith_result)
          gc()
          progress$set(value = i, message = paste0(i, " of ", nrow(pairs), " Combinations Complete..."))
        }
        progress$close()
        #if computation fail because of traffic in server RAM
        
        if(nchar(warning_msg) == 0)
          warning_msg <- "No warning messages"
        return_value <- list(collected_result, warning_msg)
        names(return_value) <- c("table","warningMessage")
        return_value
      })
      promise_race(future_result) %...>% {remove_modal_spinner()}
      future_result
    })
    
    
    output$table <- renderDataTable({
      promise_all(data = computationResult()) %...>% with({
        data$table[, names(data$table) != "Cell_Lines_Used", with = FALSE]
      })
    },
    options = list(scrollX = TRUE))
    
    output$log <- renderText({
      promise_all(data = computationResult()) %...>% with({
        data$warningMessage
      })
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(file) {
        promise_all(data = computationResult()) %...>% with({
          write_delim(data$table, file, delim = "\t")
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










