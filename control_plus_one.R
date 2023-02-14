
# control treatment
controlPlusOne.controlTreatmentInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("drugs"), "Select Drug(s) in Control Treatment",
    choices = NULL,
    options = list(`actions-box` = FALSE, `live-search-style` = "contains", `live-search` = TRUE, "max-options" = 10, "max-options-text" = "Maximum Number of Drugs Selected"),
    multiple = T
  )
}

controlPlusOne.controlTreatmentServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    observeEvent(dataset(), {
      drug_choices <- unique(dataset()$Drug)
      updatePickerInput(session,
        inputId = "drugs", label = "Select Drug(s) in Control Treatment",
        choices = drug_choices
      )
    })

    reactive(input$drugs)
  })
}


controlPlusOne.doseInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("doseSelect"))
}

controlPlusOne.doseServer <- function(id, dataset, fileType, selectedControlTreatment) {
  moduleServer(id, function(input, output, session) {
    output$doseSelect <- renderUI({
      if (length(selectedControlTreatment()) > 0) {
        ns <- session$ns
        l <- lapply(1:length(selectedControlTreatment()), function(i) {
          dose_choices <- sort(unique(dataset()$Drug_Dose[dataset()$Drug == selectedControlTreatment()[i]]))
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
        l
      }
    })

    # return a vector of selected drug doses
    reactive({
      lapply(1:length(selectedControlTreatment()), function(i) {
        input[[paste0("dose", i)]]
      }) %>%
        unlist()
    })
  })
}


# plusOne treatment
controlPlusOne.drugToAddInput <- function(id) {
  ns <- NS(id)
  selectInput(ns("drugToAdd"), label = "Drug to Add", choices = NULL)
}


controlPlusOne.drugToAddServer <- function(id, dataset, selectedControlTreatment) {
  moduleServer(id, function(input, output, session) {
    observeEvent(c(dataset(), selectedControlTreatment()), {
      to_add_choices <- setdiff(unique(dataset()$Drug), selectedControlTreatment())
      updateSelectInput(session,
        inputId = "drugToAdd", label = "Drug to Add",
        choices = to_add_choices
      )
    })

    reactive(input$drugToAdd)
  })
}


# 2Drug parameters and there helpers
controlPlusOne.parametersInput <- function(id) {
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
      )
  )
}

controlPlusOne.parametersServer <- function(id, fileType, isLowerEfficacy) {
  moduleServer(id, function(input, output, session) {

    list(
      isLowerEfficacy = isLowerEfficacy,
      uncertainty = reactive(input$uncertainty),
      comboscore = reactive(input$comboscore),
      averageDuplicate = reactive(input$averageDuplicate),
      nSim = reactive(input$nSimulation)
    )
  })
}




controlPlusOne.ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = 3, status = "primary", solidHeader = TRUE, title = "Control Plus One Input",
      controlPlusOne.controlTreatmentInput(ns("controlTreatmentSelection")),
      controlPlusOne.doseInput(ns("doseSelection")),
      controlPlusOne.drugToAddInput(ns("drugToAddSelection")),
      global.cellLineInput(ns("cellLineSelection")),
      tags$hr(),
      controlPlusOne.parametersInput(ns("parametersCheck")),
      tags$hr(),
      actionButton(ns("button"), "RUN")
    ),
    box(
      width = 9, status = "primary", solidHeader = TRUE, title = "Control Plus One Result",
      downloadButton(ns('downloadPlot'), 'Download Plot(s)'),
      downloadButton(ns("downloadData"), "Download DataTable"),
      downloadButton(ns('downloadLog'), 'Download Log File'),
      conditionalPanel(condition = "input.button", ns = ns, tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotOutput(ns("plot"),  width = "100%", height = "400px")),
        tabPanel("Table", withSpinner(dataTableOutput(ns("table")))),
        tabPanel('Log', withSpinner(verbatimTextOutput(ns('log'))))
      ))
    )
  )
}

controlPlusOne.server <- function(id, fileInfo) {
  moduleServer(id, function(input, output, session) {
    dataset <- fileInfo$dataset

    extraCol <- fileInfo$extraCol

    fileType <- fileInfo$type
    
    cellLinesAndSubgroups <- fileInfo$cellLinesAndSubgroups
    
    efficacyMetric <- fileInfo$efficacyMetric
    
    isLowerEfficacy <- fileInfo$isLowerEfficacy

    selectedControlTreatment <- controlPlusOne.controlTreatmentServer("controlTreatmentSelection", dataset)

    selectedDose <- controlPlusOne.doseServer("doseSelection", dataset, fileType, selectedControlTreatment)

    selectedDrugToAdd <- controlPlusOne.drugToAddServer("drugToAddSelection", dataset, selectedControlTreatment)

    selectedCellLinesAndSubgroups <- global.cellLineServer("cellLineSelection", cellLinesAndSubgroups)

    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines

    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups

    checkedParameters <- controlPlusOne.parametersServer("parametersCheck", fileType, isLowerEfficacy)

    nSim <- checkedParameters$nSim

    computationResult <- eventReactive(input$button, {
      validate(
        need(!is.null(dataset()), "Please upload your data"),
        need(!is.null(selectedControlTreatment()), "Please select control treatment"),
        need(!is.null(selectedDrugToAdd()), "Please select plusOne drug"),
        need(!is.null(selectedCellLines()), "Please select Cell lines"),
        need(length(selectedDose()) == length(selectedControlTreatment()), "Please select doses")
      )

      if ("seCol" %in% extraCol()) {
        eff_se_col <- "Efficacy_SE"
      } else {
        eff_se_col <- NULL
      }
      
      #show spinner and lock the whole ui
      show_modal_spinner(
        spin = "semipolar",
        color = "#112446",
        text = "Calculating Efficacy..."
      )
      
      isLowerEfficacyBetter <- checkedParameters$isLowerEfficacy()
      metricName <- efficacyMetric()
      controlTreatment <- selectedControlTreatment()
      dose <- selectedDose()
      drugToAdd <- selectedDrugToAdd()
      calculateUncertainty <- checkedParameters$uncertainty()
      if(is.null(eff_se_col)){
        calculateUncertainty <- FALSE #preventing user from trying to calculate uncertainties without SE col, would like to put warning about this somewhere, but not sure best way to do that.
      }
      nSimulation <- nSim()
      calculateComboscoreAndHazardRatio <- checkedParameters$comboscore()
      averageDuplicateRecords <- checkedParameters$averageDuplicate()
      data <- dataset() %>%
        filter(Cell_Line %in% selectedCellLines())
      file_type <- fileType()
      
      future_result <- future({
          warning_msg <- ""
          res <- withCallingHandlers(
            tryCatch(
              expr = {
                res_list <- IDAPredict.ControlPlusOne(
                  Monotherapy_Data = data,
                  Cell_Line_Name_Column = "Cell_Line",
                  Drug_Name_Column = "Drug",
                  Drug_Concentration_Column = "Drug_Dose",
                  Efficacy_Column = "Efficacy",
                  LowerEfficacyIsBetterDrugEffect = isLowerEfficacyBetter,
                  Efficacy_Metric_Name = metricName,
                  Control_Treatment_Drugs = controlTreatment,
                  Control_Treatment_Drug_Concentrations = dose,
                  Drug_to_Add = drugToAdd,
                  Calculate_Uncertainty = calculateUncertainty,
                  Efficacy_SE_Column = eff_se_col,
                  n_Simulations = nSimulation,
                  Calculate_IDAcomboscore_And_Hazard_Ratio = calculateComboscoreAndHazardRatio,
                  Average_Duplicate_Records = averageDuplicateRecords
                )
                if(!is.data.frame(res_list[[1]])){
                  NULL
                } else{
                  res <- cbind(
                    Control_Treatment = res_list$Control_Treatment,
                    Drug_to_Add = res_list$Drug_to_Add,
                    Number_of_Cell_Lines_Used = length(res_list$Cell_Lines_Used),
                    Cell_Lines_Used = paste(res_list$Cell_Lines_Used, collapse = ", "),
                    res_list[[1]]
                  )
                  res
                }
              }),
            warning = function(w) {
              warning_msg <<- paste0(warning_msg, paste0(Sys.Date(),": ",conditionMessage(w),"\n"))
              invokeRestart("muffleWarning")
            }
          )
          
          #get errorbar plots obeject
          name_of_combo_efficacy <- paste0("Mean_Combo_",metricName)
          if(file_type == "provided"){
            res$Drug_to_Add_Dose <- as.numeric(gsub("\\(Csustained\\) ", "", res$Drug_to_Add_Dose))
          }
          p1 <- qplot(as.numeric(res$Drug_to_Add_Dose),as.numeric(res[[name_of_combo_efficacy]])) +
            xlab("Drug to Add Dose") + ylab(paste0("Mean Combo ",metricName))
          if(calculateUncertainty){
            name_of_combo_efficacy_CI <- paste0("Mean_Combo_",metricName,"_95%_Confidence_Interval")
            viability_CI <- rbindlist(lapply(res[[name_of_combo_efficacy_CI]], function(s){as.data.frame(matrix(as.double(strsplit(s,"_")[[1]]),nrow = 1))}))
            p1 <- p1 + geom_errorbar(aes(ymin=viability_CI[[1]], ymax=viability_CI[[2]]), width=.05, position=position_dodge(.9))
          }
          if(calculateComboscoreAndHazardRatio){
            p2 <- qplot(as.numeric(res$Drug_to_Add_Dose),as.numeric(res$HR_vs_Control_Treatment)) +
              xlab("Drug to Add Dose") + ylab("HR vs Control Treatment")
            p3 <- qplot(as.numeric(res$Drug_to_Add_Dose),as.numeric(res$HR_vs_Drug_to_Add)) +
              xlab("Drug to Add Dose") + ylab("HR vs Drug to Add")
            p4 <- qplot(as.numeric(res$Drug_to_Add_Dose),as.numeric(res$IDA_Comboscore)) +
              xlab("Drug to Add Dose") + ylab("IDA Comboscore")
            if(calculateUncertainty){
              HR_vs_Control_Treatment_CI <- rbindlist(lapply(res[["HR_vs_Control_Treatment_95%_Confidence_Interval"]], function(s){as.data.frame(matrix(as.double(strsplit(s,"_")[[1]]),nrow = 1))}))
              HR_vs_Drug_To_Add_CI <- rbindlist(lapply(res[["HR_vs_Drug_to_Add_95%_Confidence_Interval"]], function(s){as.data.frame(matrix(as.double(strsplit(s,"_")[[1]]),nrow = 1))}))
              IDA_Comboscore_CI <- rbindlist(lapply(res[["IDA_Comboscore_95%_Confidence_Interval"]], function(s){as.data.frame(matrix(as.double(strsplit(s,"_")[[1]]),nrow = 1))}))
              p2 <- p2 + geom_errorbar(aes(ymin=HR_vs_Control_Treatment_CI[[1]], ymax=HR_vs_Control_Treatment_CI[[2]]), width=.05, position=position_dodge(.9))
              p3 <- p3 + geom_errorbar(aes(ymin=HR_vs_Drug_To_Add_CI[[1]], ymax=HR_vs_Drug_To_Add_CI[[2]]), width=.05, position=position_dodge(.9))
              p4 <- p4 + geom_errorbar(aes(ymin=IDA_Comboscore_CI[[1]], ymax=IDA_Comboscore_CI[[2]]), width=.05, position=position_dodge(.9))
            }
          }
          
          if(calculateComboscoreAndHazardRatio)
            #errorbar.plots <- grid.arrange(grobs=list(p1,p2,p3,p4),ncol = 2, nrow = 2)
            plots <- list(p1,p2,p3,p4)
          else
            #errorbar.plots <- p1
            plots <- list(p1)
          
          
          
          if(nchar(warning_msg) == 0)
            warning_msg <- "No warning messages"
          
          plots <- grid.arrange(grobs=plots,ncol = 2, nrow = 2)
          return_value <- list(res, warning_msg, plots)
          names(return_value) <- c("table","warningMessage","plots")
          return_value
      })
      
      promise_race(future_result) %...>% {remove_modal_spinner()}#remove the spinner
      
      future_result
    })
    
    
    
    
    output$table <- DT::renderDataTable(
      {
        promise_all(data = computationResult()) %...>% with({
          if (!is.null(data$table)) {
            data$table[, names(data$table) != "Cell_Lines_Used"]
          }
        })
      },
      options = list(scrollX = TRUE)
    )
    
    output$plot <- renderPlot({
      promise_all(data = computationResult()) %...>% with({
        plot(data$plots)
      })
    })
    
    output$log <- renderText({
      promise_all(data = computationResult()) %...>% with({
        data$warningMessage
      })
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".txt", sep = "")
      },
      content = function(con) {
        promise_all(data = computationResult()) %...>% with({
          write_delim(data$table, con, delim = "\t")
        })
      }
    )
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("plot(s)-", Sys.Date(), ".tiff", sep = "")
      },
      content = function(file) {
        promise_all(data = computationResult()) %...>% with({
          ggsave(file, plot = data$plots, width = 10, height = 8, units = "in")
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
