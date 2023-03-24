


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
             content = "Should a Monte Carlo simulation be performed to estimate uncertainties in the efficacy predictions based on uncertainties in the monotherapy efficacy measurements? Note that selecting this option will significantly extend the time it takes to complete the prediction. For custom datasets, this option only works if an Efficacy_SE column was provided with the file.",
             buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    conditionalPanel(condition = "input.uncertainty", ns = ns,
                     numericInput(inputId = ns("nSimulation"), label = "Number of random samples to be drawn when calculating output efficacy prediction uncertainties", value = 1000, min = 40, max = 5000)),
    checkboxInput(ns("comboscore"), "Calculate IDAComboscore And HazardRatios", value = T) %>%
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
        downloadButton(ns('downloadPlot'), 'Download Plot(s)'),
        downloadButton(ns('downloadLog'), 'Download Log File'),
        conditionalPanel(condition = "input.button_batch",ns = ns, tabsetPanel(type = "tabs",
                                                                               tabPanel("Table", withSpinner(dataTableOutput(ns("table")))),
                                                                               tabPanel("Plot", plotOutput(ns("plot"),  width = "100%", height = "800px")),
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
      if(is.null(eff_se_col)){
        uncertainty <- FALSE #preventing user from trying to calculate uncertainties without SE col, would like to put warning about this somewhere, but not sure best way to do that.
      }
      nSim <- nSim()
      comboscore <- checkedParameters$comboscore()
      averageDuplicate <- checkedParameters$averageDuplicate()
      monotherapy_data <- dataset()[dataset()$Cell_Line %in% selectedCellLines() & dataset()$Drug %in% c(selectedFixedDrug(),selectedDrugsToAdd()),]
      fixedDrug <- selectedFixedDrug()
      drugsToAdd <- selectedDrugsToAdd()
      currentFileType <- fileType()
      
      #if restricted to analysis of conc in [0, Csustained]
      if(checkedParameters$CsusRestrict() == TRUE){
        monotherapy_data <- monotherapy_data %>%
          filter(in_range == T)
      }
      
      #Creating progress bar
        progress <- AsyncProgress$new(session, min = 0, max = length(drugsToAdd), message = "Initializing Calculation......", millis = 1000)
      
      #Running batch analysis using cluster
        future_result <- future(
          global = c("monotherapy_data", "progress", "min_RAM_free_ratio_within_future", "isLowerEfficacy",
                     "efficacyMetric", "fixedDrug", "drugsToAdd", "uncertainty", "nSim",
                     "comboscore", "averageDuplicate", "currentFileType", "eff_se_col"),
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
            Abort_Filename <- tempfile()
            collected_result <- foreach(i = seq_along(drugsToAdd)) %do% {
              #Checking if this run is aborted
                Aborted <- file.exists(Abort_Filename)
                if(Aborted == TRUE){
                  stop("Insufficient RAM to complete calculation. Please try again later. We appologize for the inconvenience.")
                }
              #Checking if sufficient RAM exists to continue calculation
              temp_ram <- memuse::Sys.meminfo()
              temp_free_ram_ratio <- temp_ram$freeram/temp_ram$totalram
              if(temp_free_ram_ratio <= min_RAM_free_ratio_within_future){
                attempt <- 1
                while(temp_free_ram_ratio <= min_RAM_free_ratio_within_future & attempt <= 5){
                  progress$set(value = i-1, message = paste0("WARNING: Server RAM low. Trying to continue...(Attempt ", attempt, " of 5)"))
                  Sys.sleep(10)
                  temp_ram <- memuse::Sys.meminfo()
                  temp_free_ram_ratio <- temp_ram$freeram/temp_ram$totalram
                  attempt <- attempt + 1
                }
                if(temp_free_ram_ratio > min_RAM_free_ratio_within_future){
                  progress$set(value = i-1, message = paste0(i-1, " of ", length(drugsToAdd), " Compounds Complete..."))
                } else {
                  write("", Abort_Filename)
                  Aborted_at <- i-1
                  progress$set(value = i-1, message = "Aborting calculation...")
                  warning_msg <- paste0("WARNING: Calculation aborted after ", Aborted_at, " of ", length(drugsToAdd)," compounds complete. WARNING: Calculation failed. Please reload the web page and try again. Contact us if this problem persists.")
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
                      res <- cbind(
                        Drug1 = res$Drug1,
                        Drug2 = res$Drug2,
                        res$Efficacy_Predictions,
                        Cell_Lines_Used = paste(res$Cell_Lines_Used, collapse = ", "),
                        Number_of_Cell_Line_Used = length(res$Cell_Lines_Used))
                      res
                    }
                  }),
                warning = function(w) { #warning handling. This won't halt the computation. It only collect warning message.
                  warning_msg <<- paste0(warning_msg, paste0(Sys.Date(),": ",conditionMessage(w),"\n"))
                  invokeRestart("muffleWarning")
                }
              )
              progress$set(value = i, message = paste0(i, " of ", length(drugsToAdd), " Combinations Complete..."))
              return(ith_result)
            } #END: collected_result <- foreach(i = seq_along(drugsToAdd)) %do% {
            
          #Cleaning up abort file if it exists
            if(file.exists(Abort_Filename)){
              file.remove(Abort_Filename)
            }
            
          #Updating progress to show calculations are complete and data is being collected.
            progress$set(value = length(drugsToAdd), message = "Organizing Results...")
            
          #Collapsing list to a data frame
            if(length(collected_result) > 0){
              collected_result <- as.data.frame(do.call(rbind, collected_result))
            } else {
              collected_result <- NULL
            }

          #Making plots
          if(is.null(collected_result)){
              pnull <- ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 1) +
                       annotate(geom="text", x=5, y=0.5, label="No plottable results produced.\nPlease see error log or contact\napp developers for help.", size= 9)
              plots <- list(pnull)
            } else {
              
              if(comboscore){
                #Determining which drugs could successfully be added to control treatment
                  Added_Drugs <- unique(collected_result$Drug2)
                  warning(paste0(Added_Drugs, collapse = ", "))
                #Adding MaxHR to results
                  collected_result$MaxHR <- pmax(collected_result$HR_vs_Drug1, collected_result$HR_vs_Drug2)
                  if(uncertainty){
                    collected_result$`MaxHR_95%_Confidence_Interval` <- collected_result$`HR_vs_Drug1_95%_Confidence_Interval`
                    collected_result$`MaxHR_95%_Confidence_Interval`[collected_result$MaxHR == collected_result$HR_vs_Drug2] <- collected_result$`HR_vs_Drug2_95%_Confidence_Interval`[collected_result$MaxHR == collected_result$HR_vs_Drug2]
                  }
                  
                #Subsetting to data at max doses of all drugs
                  fixedDrug_doses <- unique(collected_result$Drug1Dose)
                  clean_fixedDrug_doses <- fixedDrug_doses
                  if(length(currentFileType) != 0 && currentFileType == "provided"){
                    clean_fixedDrug_doses <- as.numeric(gsub("\\(Csustained\\) ", "", fixedDrug_doses))
                  }
                  max_dose_selected_drug <- fixedDrug_doses[which.max(clean_fixedDrug_doses)]
                  
                  max_dose_Added_Drugs <- rep(NA, length(Added_Drugs))
                  for(i in 1:length(Added_Drugs)){
                    temp_doses <- collected_result$Drug2Dose[collected_result$Drug2 == Added_Drugs[i]]
                    clean_temp_doses <- temp_doses
                    if(length(currentFileType) != 0 && currentFileType == "provided"){
                      clean_temp_doses <- as.numeric(gsub("\\(Csustained\\) ", "", temp_doses))
                    }
                    max_dose_Added_Drugs[i] <- temp_doses[which.max(clean_temp_doses)]
                  }
                  print("C")
                  max_dose_data <- NULL
                  for(i in 1:length(Added_Drugs)){
                    max_dose_data <- rbind(max_dose_data, collected_result[collected_result$Drug1Dose == max_dose_selected_drug & collected_result$Drug2 == Added_Drugs[i] & collected_result$Drug2Dose == max_dose_Added_Drugs[i],])
                  }
      
                #Finding top 10 IDAComboscores
                  top_10_comboscore <- max_dose_data[order(max_dose_data$IDA_Comboscore, decreasing = TRUE),][1:min(10,length(Added_Drugs)),]
                  top_10_comboscore$Drug2 <- factor(top_10_comboscore$Drug2, levels = top_10_comboscore$Drug2)
                #Finding top 10 MaxHR
                  top_10_MaxHR <- max_dose_data[order(max_dose_data$MaxHR, decreasing = FALSE),][1:min(10,length(Added_Drugs)),]
                  top_10_MaxHR$Drug2 <- factor(top_10_MaxHR$Drug2, levels = top_10_MaxHR$Drug2)
      
                p1 <- ggplot(data = top_10_comboscore, aes(x = Drug2, y = IDA_Comboscore)) +
                      geom_bar(stat="identity") +
                      xlab("") +
                      ylab("IDAComboscore") +
                      ggtitle("Top 10 IDAComboscores at Max Concentration of Each Selected Compound") +
                      theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1))
    
                p2 <- ggplot(data = top_10_MaxHR, aes(x = Drug2, y = MaxHR)) +
                      geom_bar(stat = "identity") +
                      xlab("") +
                      ylab("Maximum Hazard Ratio") +
                      ggtitle("Lowest 10 MaxHRs at Max Concentration of Each Selected Compound") +
                      theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1))
    
                if(uncertainty){
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
            
          if(nchar(warning_msg) == 0)
            warning_msg <- "No warning messages"
          
          #Arranging data in list
            plots <- grid.arrange(grobs=plots,ncol = 1, nrow = 2)
            return_value <- list(collected_result, warning_msg, plots)
            names(return_value) <- c("table","warningMessage", "plots")
            
          #Cleaning up data
            rm("monotherapy_data", "progress", "min_RAM_free_ratio_within_future", "isLowerEfficacy",
               "efficacyMetric", "fixedDrug", "drugsToAdd", "uncertainty", "nSim",
               "comboscore", "averageDuplicate", "currentFileType", "eff_se_col",
               "collected_result", "plots", "warning_msg", "pnull")
            gc()
            
          #Returning data
            return(return_value)
        })

      #Ending calculation and returning results
        promise_race(future_result) %...>% {
          progress$close()
          remove_modal_spinner()
          }
        future_result
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
} #END: twoDrugs.batch.server <- function(id, fileInfo) {










