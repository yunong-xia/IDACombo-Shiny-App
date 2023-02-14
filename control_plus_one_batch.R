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
             content = "Should a Monte Carlo simulation be performed to estimate uncertainties in the efficacy predictions based on uncertainties in the monotherapy efficacy measurements? Note that selecting this option will significantly extend the time it takes to complete the prediction. For custom datasets, this option only works if an Efficacy_SE column was provided with the file.",
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    conditionalPanel(condition = "input.uncertainty", ns = ns,
                     numericInput(inputId = ns("nSimulation"), label = "Number of random samples to be drawn when calculating output efficacy prediction uncertainties", value = 1000, min = 40, max = 5000)),
    checkboxInput(ns("comboscore"), "Calculate IDAComboscore And HazardRatios", value = T) %>%
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
      downloadButton(ns('downloadPlot'), 'Download Plot(s)'),
      downloadButton(ns("downloadLog"), "Download Log File"),
      conditionalPanel(
        condition = "input.button", ns = ns,
        tabsetPanel(
          type = "tabs",
          tabPanel("Table", withSpinner(DT::dataTableOutput(ns("table")))),
          tabPanel("Plot", plotOutput(ns("plot"),  width = "100%", height = "800px")),
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
      # warning("Checking Ram")
      gc()
      ram <- memuse::Sys.meminfo()
      ram$freeram/ram$totalram
    })
    
    #Rendering UI
    output$RAM_warning_placeholder <- renderUI({
      # warning("Rendering UI")
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
        if(nrow(monotherapy_data) > 0){
          monotherapy_data <- monotherapy_data[monotherapy_data$in_range | monotherapy_data$Drug %in% selectedControlTreatment(),]
        }
      }
      
      isLowerEfficacyBetter <- checkedParameters$isLowerEfficacy()
      metricName <- efficacyMetric()
      controlTreatment <- selectedControlTreatment()
      dose <- selectedDose()
      drugsToAdd <- selectedDrugToAdd()
      calculateUncertainty <- checkedParameters$uncertainty()
      if(is.null(eff_se_col)){
        calculateUncertainty <- FALSE #preventing user from trying to calculate uncertainties without SE col, would like to put warning about this somewhere, but not sure best way to do that.
        warning("No SE column provided. Cannot calculate uncertainties.") #No idea if user will see this. (Alex)
      }
      nSimulation <- nSim()
      calculateComboscoreAndHazardRatio <- checkedParameters$comboscore()
      averageDuplicateRecords <- checkedParameters$averageDuplicate()
      currentFileType <- fileType()
      
      #Creating progress bar
        progress <- AsyncProgress$new(session, min = 0, max = length(drugsToAdd), message = "Initializing Calculation......", millis = 1000)
      
      #Running batch analysis using cluster
        future_results <- future(
          global = c("monotherapy_data", "progress", "min_RAM_free_ratio_within_future", "isLowerEfficacyBetter",
                     "metricName", "controlTreatment", "dose", "drugsToAdd", "calculateUncertainty", "nSimulation",
                     "calculateComboscoreAndHazardRatio", "averageDuplicateRecords", "currentFileType", "eff_se_col"),
          packages = c("IDACombo", "memuse", "ggplot2"),
          lazy = FALSE,
          seed = TRUE,
          gc = TRUE,
          expr = {
          warning_msg <- ""
          collected_result <- NULL
          
          #Starting progress bar count
            progress$set(value = 0, message = paste0(0, " of ", length(drugsToAdd), " Combinations Complete..."))
          
          #Doing calculations for each drug combo in parallel
            collected_result <- foreach(i = seq_along(drugsToAdd)) %do% {
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
              progress$set(value = i, message = paste0(i, " of ", length(drugsToAdd), " Combinations Complete..."))
              return(ith_result)
            } #END: collected_result <- foreach(i = seq_along(drugsToAdd)) %do% {
            
          #Updating progress to show calculations are complete and data is being collected.
            progress$set(value = length(drugsToAdd), message = "Organizing Results...")
            
          #Collapsing list to a data frame
            collected_result <- as.data.frame(do.call(rbind, collected_result))
            
          if (nchar(warning_msg) == 0) {
            warning_msg <- "No warning messages"
          }
          
          #Making plots
            if(is.null(collected_result)){
              pnull <- ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 1) +
                       annotate(geom="text", x=5, y=0.5, label="No plottable results produced.\nPlease see error log or contact\napp developers for help.", size= 9)
              plots <- list(pnull)
            } else {
              if(calculateComboscoreAndHazardRatio){
                #Determining which drugs could successfully be added to control treatment
                  Added_Drugs <- unique(collected_result$Drug_to_Add)
                #Adding MaxHR to results
                  collected_result$MaxHR <- pmax(collected_result$HR_vs_Control_Treatment, collected_result$HR_vs_Drug_to_Add)
                  if(calculateUncertainty){
                    collected_result$`MaxHR_95%_Confidence_Interval` <- collected_result$`HR_vs_Control_Treatment_95%_Confidence_Interval`
                    collected_result$`MaxHR_95%_Confidence_Interval`[collected_result$MaxHR == collected_result$HR_vs_Drug_to_Add] <- collected_result$`HR_vs_Drug_to_Add_95%_Confidence_Interval`[collected_result$MaxHR == collected_result$HR_vs_Drug_to_Add]
                  }
                  
                #Subsetting to data at max doses of all drugs to add
                  max_dose_Added_Drugs <- rep(NA, length(Added_Drugs))
                  for(i in 1:length(Added_Drugs)){
                    temp_doses <- collected_result$Drug_to_Add_Dose[collected_result$Drug_to_Add == Added_Drugs[i]]
                    clean_temp_doses <- temp_doses
                    if(length(currentFileType) != 0 && currentFileType == "provided"){
                      clean_temp_doses <- as.numeric(gsub("\\(Csustained\\) ", "", temp_doses))
                    }
                    max_dose_Added_Drugs[i] <- temp_doses[which.max(clean_temp_doses)]
                  }
      
                  max_dose_data <- NULL
                  for(i in 1:length(Added_Drugs)){
                    max_dose_data <- rbind(max_dose_data, collected_result[collected_result$Drug_to_Add == Added_Drugs[i] & collected_result$Drug_to_Add_Dose == max_dose_Added_Drugs[i],])
                  }
      
                #Finding top 10 IDAComboscores
                  top_10_comboscore <- max_dose_data[order(max_dose_data$IDA_Comboscore, decreasing = TRUE),][1:min(10,length(Added_Drugs)),]
                  top_10_comboscore$Drug_to_Add <- factor(top_10_comboscore$Drug_to_Add, levels = top_10_comboscore$Drug_to_Add)
                #Finding top 10 MaxHR
                  top_10_MaxHR <- max_dose_data[order(max_dose_data$MaxHR, decreasing = FALSE),][1:min(10,length(Added_Drugs)),]
                  top_10_MaxHR$Drug_to_Add <- factor(top_10_MaxHR$Drug_to_Add, levels = top_10_MaxHR$Drug_to_Add)
      
                p1 <- ggplot(data = top_10_comboscore, aes(x = Drug_to_Add, y = IDA_Comboscore)) +
                      geom_bar(stat="identity") +
                      xlab("") +
                      ylab("IDAComboscore") +
                      ggtitle("Top 10 IDAComboscores at Max Concentration of Each Selected Compound to Add") +
                      theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1))
      
                p2 <- ggplot(data = top_10_MaxHR, aes(x = Drug_to_Add, y = MaxHR)) +
                      geom_bar(stat = "identity") +
                      xlab("") +
                      ylab("Maximum Hazard Ratio") +
                      ggtitle("Lowest 10 MaxHRs at Max Concentration of Each Selected Compound to Add") +
                      theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1))
                
                if(calculateUncertainty){
                  temp_conf_1 <- as.data.frame(do.call(rbind, lapply(strsplit(top_10_comboscore$`IDA_Comboscore_95%_Confidence_Interval`, "_"), as.numeric)))
                  temp_ylims_1 <- layer_scales(p1)$y$range$range
                  temp_ylims_1[2] <- temp_ylims_1[2]*1.2
                  temp_ylims_1[1] <- 0
                  p1 <- p1 + geom_errorbar(aes(ymin=temp_conf_1$V1, ymax=temp_conf_1$V2), width=.05, position=position_dodge(.9)) + coord_cartesian(ylim = temp_ylims_1)
    
                  temp_conf_2 <- as.data.frame(do.call(rbind, lapply(strsplit(top_10_MaxHR$`MaxHR_95%_Confidence_Interval`, "_"), as.numeric)))
                  temp_ylims_2 <- layer_scales(p2)$y$range$range
                  temp_ylims_2[2] <- max(temp_ylims_2, 1)
                  temp_ylims_2[1] <- 0
                  p2 <- p2 + geom_errorbar(aes(ymin=temp_conf_2$V1, ymax=temp_conf_2$V2), width=.05, position=position_dodge(.9)) + coord_cartesian(ylim = temp_ylims_2)
                }
      
                plots <- list(p1, p2)
              } else {
                pnull <- ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 1) +
                       annotate(geom="text", x=5, y=0.5, label="IDAComboscores and HRs\nmust be calculated to\nproduce plottable results.", size= 9)
                plots <- list(pnull)
              }
            }
            
          #Collecting results in a list
            plots <- grid.arrange(grobs=plots,ncol = 1, nrow = 2)
            return_value = list(collected_result,plots,warning_msg)
            names(return_value) = c("table","plots","warningMessage")
            
          #Cleaning up data
            rm("monotherapy_data", "min_RAM_free_ratio_within_future", "isLowerEfficacyBetter",
               "metricName", "controlTreatment", "dose", "drugsToAdd", "calculateUncertainty", "nSimulation",
               "calculateComboscoreAndHazardRatio", "averageDuplicateRecords", "currentFileType", "eff_se_col",
               "collected_result", "plots", "warning_msg", "pnull")
            gc()
          
          return(return_value)
        })
        
      #Ending calculation and returning results
        
        promise_all(future_results) %...>% {
          progress$close()
          remove_modal_spinner()
        }
        future_results
    })
    
    #Rendering outputs for user
      observeEvent(computationResult(), {
        
        req(computationResult())
        
        promise_all(data = computationResult()) %...>% with({
          isolate({
            
            #Interactive Table
              output$table <- DT::renderDataTable(
                {
                  if (!is.null(data$table)) {
                    return(as.data.frame(data$table)[, ! colnames(data$table) %in% "Cell_Lines_Used"])
                  } else {
                    return(data.frame(
                      Warning = "No results produced; data$table is NULL. Please see error logs or contact app developers for help."
                    ))
                  }
                },
                options = list(scrollX = TRUE)
              )
              
            #App Plot
              output$plot <- renderPlot({
                plot(data$plot)
              })
              
            #App Log
              output$log <- renderText({
                data$warningMessage
              })
              
            #Downloadable Table
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste("data-", Sys.Date(), ".txt", sep = "")
                },
                content = function(file) {
                  write.table(as.data.frame(data$table), file, sep = "\t", row.names = FALSE, col.names = TRUE)
                }
              )
              
            #Downloadable Plot
              output$downloadPlot <- downloadHandler(
                filename = function() {
                  paste("plot(s)-", Sys.Date(), ".tiff", sep = "")
                },
                content = function(file) {
                  ggsave(file, plot = data$plots, width = 8, height = 8, units = "in")
                }
              )
              
            #Downloadable Log
              output$downloadLog <- downloadHandler(
                filename = function() {
                  paste("log-", Sys.Date(), ".txt", sep = "")
                },
                content = function(file) {
                  write(data$warningMessage, file)
                }
              )
            
          }) #END: isolate({
        }) #END: promise_all(data = computationResult()) %...>% with({
      }) #END: observeEvent(computationResult(), {
    
  }) #END: moduleServer(id, function(input, output, session) {
} #END: controlPlusOne.batch.server <- function(id, fileInfo) {