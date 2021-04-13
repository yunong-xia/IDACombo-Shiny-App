# control treatment
controlPlusOne.batch.controlTreatmentInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("drugs"), "Select Drug(s) in Control Treatment",
                choices = NULL,
                options = list(`live-search-style` = "contains", `live-search` = TRUE, "max-options" = 5, "max-options-text" = "Maximum Number of Drugs Selected"),
                multiple = T
    ),
    checkboxInput(ns("use_no_Csus_drugs"),label = "Include drugs which do not have Csustained Concentration", value = TRUE)
  )
}

controlPlusOne.batch.controlTreatmentServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    observeEvent(c(dataset(), input$use_no_Csus_drugs), {
      #If checkbox value is FALSE, then provide drugs that have Csustained monotherapy data
      if(input$use_no_Csus_drugs == FALSE && "with_Csus_concc" %in% names(dataset())){
        drug_choices <- unique(dataset()$Drug[dataset()$with_Csus_conc])
      }
      #If TRUE, then provide all drugs
      else{
        drug_choices <- unique(dataset()$Drug)
      }
      updatePickerInput(session,
                        inputId = "drugs", label = "Select Drug(s) in Control Treatment",
                        choices = drug_choices
      )
    })
    
    reactive(input$drugs)
  })
}


controlPlusOne.batch.doseInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("doseSelect"))
}

controlPlusOne.batch.doseServer <- function(id, dataset, fileType, selectedControlTreatment) {
  moduleServer(id, function(input, output, session) {
    output$doseSelect <- renderUI({
      if (length(selectedControlTreatment()) > 0) {
        ns <- session$ns
        start <- Sys.time()
        l <- lapply(1:length(selectedControlTreatment()), function(i) {
          dose_choices <- unique(dataset()$Drug_Dose[dataset()$Drug == selectedControlTreatment()[i]])
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
          selectInput(ns(paste0("dose", i)), paste0("Select dose for Drug: ", selectedControlTreatment()[i]),
                      choices = dose_choices
          )
        })
        end <- Sys.time()
        l
      }
    })
    
    # return a vector of selected drug doses
    reactive({
      lapply(1:length(selectedControlTreatment()), function(i) {
        (input[[paste0("dose", i)]])
      }) %>%
        unlist()
    })
  })
}


# plusOne treatment
controlPlusOne.batch.drugToAddInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("drugToAdd"), label = "Drugs to Add (Multiple Drugs)", choices = NULL, multiple = T,
              options = list(
                `actions-box` = TRUE, `live-search-style` = "startsWith", `live-search` = TRUE,
                `selected-text-format` = "count",
                `count-selected-text` = "{0} of drugs chosen (on a total of {1})"
              ))
}


controlPlusOne.batch.drugToAddServer <- function(id, dataset, selectedControlTreatment) {
  moduleServer(id, function(input, output, session) {
    observeEvent(c(dataset(), selectedControlTreatment()), {
      to_add_choices <- setdiff(unique(dataset()$Drug), selectedControlTreatment())
      updatePickerInput(session,
                        inputId = "drugToAdd", label = "Drugs to Add (Multiple Drugs)",
                        choices = to_add_choices
      )
    })
    
    reactive(input$drugToAdd)
  })
}




# 2Drug parameters and there helpers
controlPlusOne.batch.parametersInput <- function(id) {
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
             content = "Should IDAComboscores and Hazard Ratios (HRs) be calculated between the control and control+1 therapies? Note that these values are only meaningful when efficacy values are scaled between 0 and 1 (i.e. viability, normalized AUC, etc.).",
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

controlPlusOne.batch.parametersServer <- function(id, fileType, isLowerEfficacy) {
  moduleServer(id, function(input, output, session) {
    
    list(
      isLowerEfficacy = isLowerEfficacy,
      uncertainty = reactive(input$uncertainty),
      comboscore = reactive(input$comboscore),
      averageDuplicate = reactive(input$averageDuplicate),
      CsusRestrict = reactive(input$CsusRestrict),
      nSim = reactive(input$nSimulation)
    )
  })
}




controlPlusOne.batch.ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = 3, status = "primary", solidHeader = TRUE, title = "Control Plus One Input",
      controlPlusOne.batch.controlTreatmentInput(ns("controlTreatmentSelection")),
      controlPlusOne.batch.doseInput(ns("doseSelection")),
      controlPlusOne.batch.drugToAddInput(ns("drugToAddSelection")),
      global.cellLineInput(ns("cellLineSelection")),
      tags$hr(),
      controlPlusOne.batch.parametersInput(ns("parametersCheck")),
      tags$hr(),
      uiOutput(ns("RAM_warning_placeholder")),
      actionButton(ns("button"), "RUN")
    ),
    box(
      width = 9, status = "primary", solidHeader = TRUE, title = "Control Plus One Result",
      downloadButton(ns("downloadData"), "Download DataTable"),
      downloadButton(ns("downloadLog"), "Download Log File"),
      conditionalPanel(
        condition = "input.button", ns = ns,
        tabsetPanel(
          type = "tabs",
          tabPanel("Table", withSpinner(DT::dataTableOutput(ns("table")))),
          tabPanel("Log", withSpinner(wellPanel(verbatimTextOutput(ns("log")))))
        )
      )
    )
  )
}

controlPlusOne.batch.server <- function(id, fileInfo) {
  moduleServer(id, function(input, output, session) {
    dataset <- fileInfo$dataset
    
    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    cellLinesAndSubgroups <- fileInfo$cellLinesAndSubgroups
    
    efficacyMetric <- fileInfo$efficacyMetric
    
    isLowerEfficacy <- fileInfo$isLowerEfficacy
    
    selectedControlTreatment <- controlPlusOne.batch.controlTreatmentServer("controlTreatmentSelection", dataset)
    
    selectedDose <- controlPlusOne.batch.doseServer("doseSelection", dataset, fileType, selectedControlTreatment)
    
    selectedDrugToAdd <- controlPlusOne.batch.drugToAddServer("drugToAddSelection", dataset, selectedControlTreatment)
    
    selectedCellLinesAndSubgroups <- global.cellLineServer("cellLineSelection", cellLinesAndSubgroups)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups
    
    checkedParameters <- controlPlusOne.batch.parametersServer("parametersCheck", fileType, isLowerEfficacy)
    
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
        disable("button")
      }else{
        enable("button")
      }
    })
    
    computationResult <- eventReactive(input$button, {
      validate(
        need(!is.null(dataset()), "Please upload your data"),
        need(!is.null(selectedControlTreatment()), "Please select control treatment"),
        need(!is.null(selectedDrugToAdd()), "Please select Drug to Add"),
        need(!is.null(selectedCellLines()), "Please select Cell lines"),
        need(length(selectedDose()) == length(selectedControlTreatment()), "Please select doses")
      )
      if ("seCol" %in% extraCol()) {
        eff_se_col <- "Efficacy_SE"
      } else {
        eff_se_col <- NULL
      }
      show_modal_spinner(
        spin = "semipolar",
        color = "#112446",
        text = "Calculating Efficacy"
      )
      monotherapy_data <- dataset()[dataset()$Cell_Line %in% selectedCellLines() & dataset()$Drug %in% c(selectedDrugToAdd(), selectedControlTreatment()), ]
      
      #if restricted to analysis of conc in [0, Csustained]
      #we only need to restrict drugs to add.
      if(checkedParameters$CsusRestrict() == TRUE){
        monotherapy_data <- monotherapy_data[monotherapy_data$in_range | monotherapy_data$Drug %in% selectedControlTreatment(),]
      }
      
      isLowerEfficacyBetter <- checkedParameters$isLowerEfficacy()
      metricName <- efficacyMetric()
      controlTreatment <- selectedControlTreatment()
      dose <- selectedDose()
      drugsToAdd <- selectedDrugToAdd()
      calculateUncertainty <- checkedParameters$uncertainty()
      nSimulation <- nSim()
      calculateComboscoreAndHazardRatio <- checkedParameters$comboscore()
      averageDuplicateRecords <- checkedParameters$averageDuplicate()
      progress <- AsyncProgress$new(session, min = 0, max = length(drugsToAdd), message = "Initializing Calculation......")
      future_resulst <- future(
        seed = T,
        expr = {
        warning_msg <- ""
        collected_result <- NULL
        progress$set(value = 0, message = paste0(0, " of ", length(drugsToAdd), " Combinations Complete..."))
        for (i in seq_along(drugsToAdd)) {
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
              progress$set(value = i-1, message = paste0(i-1, " of ", length(temp_conc_list), " Compounds Complete..."))
            } else {
              Aborted <- i-1
              progress$set(value = i-1, message = "Aborting calculation...")
              warning_msg <- paste0("WARNING: Calculation aborted after ", Aborted, " of ", length(temp_conc_list)," compounds complete. WARNING: Calculation failed. Please reload the web page and try again. Contact us if this problem persists.")
              break
            }
          }
          # get mono data
          ith_result <- withCallingHandlers(
            tryCatch(
              expr = {
                res <- IDAPredict.ControlPlusOne(
                  Monotherapy_Data = monotherapy_data,
                  Cell_Line_Name_Column = "Cell_Line",
                  Drug_Name_Column = "Drug",
                  Drug_Concentration_Column = "Drug_Dose",
                  Efficacy_Column = "Efficacy",
                  LowerEfficacyIsBetterDrugEffect = isLowerEfficacyBetter,
                  Efficacy_Metric_Name = metricName,
                  Control_Treatment_Drugs = controlTreatment,
                  Control_Treatment_Drug_Concentrations = dose,
                  Drug_to_Add =drugsToAdd[i],
                  Calculate_Uncertainty = calculateUncertainty,
                  Efficacy_SE_Column = eff_se_col,
                  n_Simulations = nSimulation,
                  Calculate_IDAcomboscore_And_Hazard_Ratio = calculateComboscoreAndHazardRatio,
                  Average_Duplicate_Records = averageDuplicateRecords
                )
                if (!is.data.frame(res[[1]])) {
                  NULL
                } else {
                  res <- cbind(
                    Control_Treatment = res$Control_Treatment,
                    Drug_to_Add = res$Drug_to_Add,
                    Number_of_Cell_Lines_Used = length(res$Cell_Lines_Used),
                    Cell_Lines_Used = paste(res$Cell_Lines_Used, collapse = ", "),
                    res[[1]]
                  )
                  res
                }
              }
            ),
            warning = function(w) {
              warning_msg <<- paste0(warning_msg, paste0(Sys.Date(), ": ", conditionMessage(w), "\n"))
              invokeRestart("muffleWarning")
            }
          )
          if(is.null(collected_result)){
            collected_result <- ith_result
          }
          else{
            collected_result <- rbindlist(list(collected_result, ith_result))
          }
          rm(ith_result)
          gc()
          progress$set(value = i, message = paste0(i, " of ", length(drugsToAdd), " Combinations Complete..."))
        }
        progress$close()
        if (nchar(warning_msg) == 0) {
          warning_msg <- "No warning messages"
        }
        return_value = list(collected_result,warning_msg)
        names(return_value) = c("table","warningMessage")
        return_value
      })
      promise_all(future_resulst) %...>% {remove_modal_spinner()}
      future_resulst
    })
    
    output$table <- DT::renderDataTable(
      {
        promise_all(data = computationResult()) %...>% with({
          data$table[, names(data$table) != "Cell_Lines_Used", with = FALSE]
          
        })
      },
      options = list(scrollX = TRUE)
    )
    
    output$log <- renderText({
      promise_all(data = computationResult()) %...>% with({
        data$warningMessage        
      })
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        promise_all(data = computationResult()) %...>% with({
          write_delim(data$table, file, delim = "\t")
        })
      }
    )
    
    
    output$downloadLog <- downloadHandler(
      filename = function() {
        paste("log-", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        promise_all(data = computationResult()) %...>% with({
          write(data$warningMessage, file)
        })
      }
    )
    
    
    
  })
}