# select two drugs
twoDrugs.drugInput <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("drug1"), "Drug 1", NULL),
    selectInput(ns("drug2"), "Drug 2", NULL)
  )
}

twoDrugs.drugServer <- function(id, dataset) { #dataset is a reactive value
  moduleServer(id, function(input,output,session) {
    observeEvent(dataset(),{
      drug_choices <- unique(dataset()$Drug)
      updateSelectInput(session,"drug1",label = "Drug 1", choices = drug_choices)
      updateSelectInput(session,"drug2",label = "Drug 2", choices = drug_choices)
    })
    
    
    list(d1 = reactive(input$drug1),
         d2 = reactive(input$drug2))
    
  })
}


#select dosages
twoDrugs.doseInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("dose1"),"Drug dose available for drug 1 (Multiple)", 
                choices = NULL, 
                options = list(`actions-box` = TRUE,`selected-text-format` = "count > 2",
                               `count-selected-text` = "{0}/{1} Concentrations",
                               `live-search-style` = "startsWith" , `live-search` = TRUE),
                multiple = T),
    pickerInput(ns("dose2"),"Drug dose available for drug 2 (Multiple)", 
                choices = NULL, 
                options = list(`actions-box` = TRUE,`selected-text-format` = "count > 2",
                               `count-selected-text` = "{0}/{1} Concentrations",
                               `live-search-style` = "startsWith" , `live-search` = TRUE),
                multiple = T)
  )
}

twoDrugs.doseServer <- function(id, dataset, fileType,selectedDrug1, selectedDrug2, selectedSharedCellLines) {
  moduleServer(id, function(input,output,session) {
    
    observeEvent(c(dataset(), fileType(),selectedDrug1(), selectedDrug2(), selectedSharedCellLines()),{
      data <- dataset()
      drug1 <- selectedDrug1()
      drug2 <- selectedDrug2()
      shared_cls <- selectedSharedCellLines()
      
      dose1_choices <- sort(unique(data$Drug_Dose[data$Drug == drug1 & data$Cell_Line %in% shared_cls]))
      dose2_choices <- sort(unique(data$Drug_Dose[data$Drug == drug2 & data$Cell_Line %in% shared_cls]))
      
      #make sure fileType() is not null. Since at the beginning, nothing is selected.
      if(length(fileType()) != 0 && fileType() == "provided"){
        Csustained_conc1 <- as.numeric(gsub("\\(Csustained\\) ","",dose1_choices[grep("\\(Csustained\\) ",dose1_choices)]))
        Csustained_conc2 <- as.numeric(gsub("\\(Csustained\\) ","",dose2_choices[grep("\\(Csustained\\) ",dose2_choices)]))
        clean_conc1 <- sort(as.numeric(gsub("\\(Csustained\\) ", "", dose1_choices)))
        clean_conc2 <- sort(as.numeric(gsub("\\(Csustained\\) ", "", dose2_choices)))
        if(length(Csustained_conc1) != 0){
          clean_conc1[match(Csustained_conc1,clean_conc1)] <- paste0("(Csustained) ", Csustained_conc1)
        }
        else{
          clean_conc1 <- as.character(clean_conc1)
        }
        if(length(Csustained_conc2) != 0){
          clean_conc2[match(Csustained_conc2,clean_conc2)] <- paste0("(Csustained) ", Csustained_conc2)
        }
        else{
          clean_conc2 <- as.character(clean_conc2)
        }
        dose1_choices <- clean_conc1
        dose2_choices <- clean_conc2
      }
      
      updatePickerInput(session, inputId = "dose1", label = "Drug dose available for drug 1 (Multiple)",
                        choices = dose1_choices)
      updatePickerInput(session, inputId = "dose2", label = "Drug dose available for drug 2 (Multiple)",
                        choices = dose2_choices)
    })
    
    
    list(dose1 = reactive(input$dose1),
         dose2 = reactive(input$dose2))
    
  })
}



# 2Drug parameters and there helpers
twoDrugs.parametersInput <- function(id) {
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
             )
  )
}

twoDrugs.parametersServer <- function(id, fileType, isLowerEfficacy) {
  moduleServer(id, function(input,output,session) {
    
    list(isLowerEfficacy = isLowerEfficacy,
         uncertainty = reactive(input$uncertainty),
         comboscore = reactive(input$comboscore),
         averageDuplicate = reactive(input$averageDuplicate),
         nSim = reactive(input$nSimulation))
  })
}

#########   put all the elements together   ##########

twoDrugs.ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 3, status = "primary", solidHeader = TRUE, title="2 Drugs Input",
        twoDrugs.drugInput(ns("drugSelection")),
        global.cellLineInput(ns("cellLineSelection")),
        twoDrugs.doseInput(ns("doseSelection")),
        tags$hr(),
        twoDrugs.parametersInput(ns("parametersCheck")),
        tags$hr(),
        actionButton(ns("button"), "RUN")
    ),
    box(width = 9, status = "primary", solidHeader = TRUE, title="2Drug Result",
        downloadButton(ns('downloadData'), 'Download DataTable'),
        downloadButton(ns('downloadPlot'), 'Download 3D Plot'),
        downloadButton(ns('downloadLog'), 'Download Log File'),
        conditionalPanel(condition = "input.button",ns = ns, tabsetPanel(type = "tabs",
                                                                         tabPanel("Table", withSpinner(dataTableOutput(ns("table")))),
                                                                         tabPanel("3dPlot", withSpinner(rglwidgetOutput(ns("plot"),  width = 400, height = 400))),
                                                                         tabPanel('Log', withSpinner(verbatimTextOutput(ns('log')))))  
        )
    )
    
  )
}



twoDrugs.server <- function(id, fileInfo) {
  moduleServer(id, function(input,output,session) {
    dataset <- fileInfo$dataset

    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    cellLinesAndSubgroups <- fileInfo$cellLinesAndSubgroups
  
    efficacyMetric <- fileInfo$efficacyMetric
    
    isLowerEfficacy <- fileInfo$isLowerEfficacy
    
    selectedDrugs <- twoDrugs.drugServer("drugSelection", dataset)
    
    selectedCellLinesAndSubgroups <- global.cellLineServer("cellLineSelection", cellLinesAndSubgroups)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups
    
    selectedDose <- twoDrugs.doseServer("doseSelection", dataset, fileType, selectedDrugs$d1, selectedDrugs$d2, selectedCellLines)
    
    checkedParameters <- twoDrugs.parametersServer("parametersCheck",fileType, isLowerEfficacy)
    
    nSim <- checkedParameters$nSim

    
    computationResult <- eventReactive(input$button, {
      validate(
        need(!is.null(dataset()), "Please upload your data"),
        need(!is.null(selectedDrugs$d1()), "Please select drug 1"),
        need(!is.null(selectedDrugs$d2()), "Please select drug 2"),
        need(!is.null(selectedCellLines()), "Please select Cell lines"),
        need(!is.null(selectedDose$dose1()), "Please select doses of drug 1"),
        need(!is.null(selectedDose$dose2()), "Please select doses of drug 2")
      )
      
      #show spinner and lock the whole ui
      show_modal_spinner(
        spin = "semipolar",
        color = "#112446",
        text = "Calculating Efficacy..."
      )
      select1<-filter(dataset(),
                      Drug == selectedDrugs$d1(),
                      Drug_Dose %in% selectedDose$dose1(),
                      Cell_Line %in% selectedCellLines())
      select2<-filter(dataset(),
                      Drug == selectedDrugs$d2(),
                      Drug_Dose %in% selectedDose$dose2(),
                      Cell_Line %in% selectedCellLines())

      
      monotherapy_data <- rbindlist(list(select1,select2))
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL
      
      isLowerEfficacy <- checkedParameters$isLowerEfficacy()
      efficacyMetric <- efficacyMetric()
      d1 <- selectedDrugs$d1()
      d2 <- selectedDrugs$d2()
      uncertainty <- checkedParameters$uncertainty()
      nSim <- nSim()
      comboscore <- checkedParameters$comboscore()
      averageDuplicate <- checkedParameters$averageDuplicate()
      file_type <- fileType()
      
      future_result <- future({
        warning_msg <- ""
        res <- withCallingHandlers(
          tryCatch(
            expr = {
              res_list <- IDAPredict.2drug(
                Monotherapy_Data = monotherapy_data,
                Cell_Line_Name_Column = "Cell_Line",
                Drug_Name_Column = "Drug",
                Drug_Concentration_Column = "Drug_Dose",
                Efficacy_Column = "Efficacy",
                LowerEfficacyIsBetterDrugEffect = isLowerEfficacy,
                Efficacy_Metric_Name = efficacyMetric,
                Drug1 = d1,
                Drug2 = d2,
                Calculate_Uncertainty = uncertainty,
                Efficacy_SE_Column = eff_se_col,
                n_Simulations = nSim,
                Calculate_IDAcomboscore_And_Hazard_Ratio = comboscore,
                Average_Duplicate_Records = averageDuplicate
              )
              if(!is.data.frame(res_list[[1]])){
                NULL
              } else{
                res_dataframe <- res_list[[1]]
                res <- cbind(Drug_1 = res_list[[2]],
                             Drug_2 = res_list[[3]],
                             Numbers_of_Used_Cell_Lines = length(res_list[[4]]),
                             res_dataframe)
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
        
        return_value <- list(res, warning_msg)
        names(return_value) <- c("table","warningMessage")
        return_value
      }) 
      promise_race(future_result) %...>% {remove_modal_spinner()}#remove the spinner
      
      future_result
    })
    
    
    plot.object <- reactive({
      promise_all(data = computationResult())%...>% with({
        name_of_combo_efficacy <- paste0("Mean_Combo_",efficacyMetric())
        plot.data <- data$table[,c("Drug_1", "Drug_2", "Drug1Dose", "Drug2Dose", name_of_combo_efficacy)]
        if(plot.data$Drug_1[1] == plot.data$Drug_2[1]){
          plot.data$Drug_2 = paste0(plot.data$Drug_2,"_2")
        }
        plot.data$Group <- "Predicted Combination"
        if(fileType() == "provided"){
          #remove "(Csustained) " from the dose columns and convert these columns to numeric
          # in order to label the x and y axis.
          plot.data$Drug1Dose <- as.numeric(gsub("\\(Csustained\\) ", "", plot.data$Drug1Dose))
          plot.data$Drug2Dose <- as.numeric(gsub("\\(Csustained\\) ", "", plot.data$Drug2Dose))
        }
        plot.data$Group[plot.data$Drug1Dose == 0] <- paste0(plot.data$Drug_2[1], " Monotherapy")
        plot.data$Group[plot.data$Drug2Dose == 0] <- paste0(plot.data$Drug_1[1], " Monotherapy")
        to.add <- as.data.frame(matrix(c(rep(0, ncol(plot.data)-1), "Origin"), ncol = ncol(plot.data)))
        colnames(to.add) <- colnames(plot.data)
        plot.data <- rbind(plot.data, to.add)
        plot.data$Group <- factor(plot.data$Group, levels = c(paste0(plot.data$Drug_1[1], " Monotherapy"), paste0(plot.data$Drug_2[1], " Monotherapy"), "Origin", "Predicted Combination"))
        plot.data$Drug1Dose <- as.numeric(plot.data$Drug1Dose)
        plot.data$Drug2Dose <- as.numeric(plot.data$Drug2Dose)
        plot.data[[name_of_combo_efficacy]] <- as.numeric(plot.data[[name_of_combo_efficacy]])
        plot.data[[name_of_combo_efficacy]] <- plot.data[[name_of_combo_efficacy]] * 100
        
        groups <- levels(plot.data$Group)
        origin <- which(groups == "Origin")
        groups <- groups[-origin]
        colors <- c("blue", "green", "white", "magenta")
        rgl.open(useNULL = rgl.useNULL())
        name_of_combo_efficacy <- paste0("Mean_Combo_",efficacyMetric())
        scatter3d(x = plot.data$Drug2Dose, y = plot.data[[name_of_combo_efficacy]], z = plot.data$Drug1Dose, 
                  surface = F, grid = F, ellipsoid = F, xlab = "", zlab = "", ylab = "", 
                  groups = plot.data$Group, axis.ticks = F, axis.scales = T, axis.col = c("darkgreen", "black", "blue"), surface.col = colors)
        par3d(windowRect = c(-1873, -433, -590, 530))
        par3d(userMatrix = matrix(c(0.5643716, -0.006304999, -0.82549691, 0, -0.1164742, 0.989359140, -0.08718707, 0, 0.8172630, 0.145355001, 0.55763179, 0, 0, 0, 0, 1), 
                                  nrow = 4, ncol = 4, byrow = T))
        par3d(zoom = 1.1)
        at <- seq(0,1, length.out = 5)
        at <- at[2:4]
        z.labels <- formatC(seq(0,max(plot.data$Drug1Dose), length.out = 5), format = "g", digits = 2)[2:4]
        y.labels <- seq(0,100, length.out = 5)[2:4]
        x.labels <- formatC(seq(0,max(plot.data$Drug2Dose), length.out = 5), format = "g", digits = 2)[2:4]
        rgl.texts(x = at, y = -0.05, z = 0, text = x.labels, col = "darkgreen")
        rgl.texts(0, -0.1, at, z.labels, col = "blue")
        rgl.texts(-0.05, at, -0.05, y.labels, col = "black")
        
        best.monotherapy <- min(plot.data[[name_of_combo_efficacy]][grep("Monotherapy", plot.data$Group)])
        planes3d(a = 0, b = -1, c = 0, d = best.monotherapy/100, alpha = 0.5, col = "green")
        max.z <- signif(as.numeric(formatC(seq(0,max(plot.data$Drug1Dose), length.out = 5), format = "g", digits = 2)[5]), 1)
        max.x <- signif(as.numeric(formatC(seq(0,max(plot.data$Drug2Dose), length.out = 5), format = "g", digits = 2)[5]), 1)
        max.x.point <- max(plot.data$Drug2Dose)
        max.z.point <- max(plot.data$Drug1Dose)
        x.coord <- min(max.x.point/max.x, 1)
        z.coord <- min(max.z.point/max.z, 1)
        
        legend3d("topleft", legend = groups, col = colors[1:(length(groups)+1)][-origin], pch = 16, cex=3, inset=c(0.08, 0.07))
        
        rglwidget()
      })
    })
    
    
    output$table <- renderDataTable({
        promise_all(data = computationResult()) %...>% with({
          data$table
        })
      },
      options = list(scrollX = TRUE)
    )
    
    output$log <- renderText({
      promise_all(data = computationResult()) %...>% with({
        data$warningMessage
      })
    })
    
    output$plot <- renderRglwidget({
      promise_all(plot = plot.object()) %...>% with({
        plot
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
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste('plot-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        promise_all(data = computationResult()) %...>% with({
          htmlwidgets::saveWidget(data$plot3d, con,selfcontained = TRUE)
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

