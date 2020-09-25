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

#select cell lines
twoDrugs.cellLineInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("subgroups"),"Select Cell Lines By Subgroups",
                choices = NULL,
                options = list(`liveSearchStyle` = "startsWith" , `liveSearch` = TRUE),
                multiple = T),
    actionButton(ns("selectAllSubgroups"),"Select All Subgroups"),
    actionButton(ns("deselectAllSubgroups"),"Deselect All Subgroups"),
    pickerInput(ns("cell_lines"),"Select Cell Lines",
                choices = list(
                  `Cancer Cell Lines` = NULL),
                options = list(`actions-box` = TRUE,`liveSearchStyle` = "startsWith" , `liveSearch` = TRUE,
                               `selected-text-format`= "count",
                               `count-selected-text` = "{0} subgroups chosen (on a total of {1})"),
                multiple = T)
  )
}

twoDrugs.cellLineServer <- function(id, dataset) { #dataset, selectedDrugs are reactive values
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


#select dosages
twoDrugs.doseInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("dose1"),"Drug dose available for drug 1 (Multiple)", 
                choices = NULL, 
                options = list(`actions-box` = TRUE,`selected-text-format` = "count > 2",
                               `count-selected-text` = "{0}/{1} Concentrations",
                               `liveSearchStyle` = "startsWith" , `liveSearch` = TRUE),
                multiple = T),
    pickerInput(ns("dose2"),"Drug dose available for drug 2 (Multiple)", 
                choices = NULL, 
                options = list(`actions-box` = TRUE,`selected-text-format` = "count > 2",
                               `count-selected-text` = "{0}/{1} Concentrations",
                               `liveSearchStyle` = "startsWith" , `liveSearch` = TRUE),
                multiple = T)
  )
}

twoDrugs.doseServer <- function(id, dataset, selectedDrug1, selectedDrug2, selectedSharedCellLines) {
  stopifnot(is.reactive(dataset))
  stopifnot(is.reactive(selectedDrug1))
  stopifnot(is.reactive(selectedDrug2))
  stopifnot(is.reactive(selectedSharedCellLines))
  moduleServer(id, function(input,output,session) {
    
    observeEvent(c(dataset(), selectedDrug1(), selectedDrug2(), selectedSharedCellLines()),{
      data <- dataset()
      drug1 <- selectedDrug1()
      drug2 <- selectedDrug2()
      shared_cls <- selectedSharedCellLines()
      
      dose1_choices <- sort(unique(data$Drug_Dose[data$Drug == drug1 & data$Cell_Line %in% shared_cls]))
      dose2_choices <- sort(unique(data$Drug_Dose[data$Drug == drug2 & data$Cell_Line %in% shared_cls]))
      
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

twoDrugs.parametersServer <- function(id, fileType) {
  moduleServer(id, function(input,output,session) {
    observeEvent(fileType(),{
      if(fileType() == "GDSC" || fileType() == "CTRPv2"){
        updateCheckboxInput(session, "isLowerEfficacy", "Lower Efficacy Is Better Drug Effect", value = TRUE)
        disable("isLowerEfficacy")
      }
      else{
        enable("isLowerEfficacy")
        updateCheckboxInput(session,"isLowerEfficacy", "Lower Efficacy Is Better Drug Effect", value = FALSE)
      }
    })
    
    list(isLowerEfficacy = reactive(input$isLowerEfficacy),
         uncertainty = reactive(input$uncertainty),
         comboscore = reactive(input$comboscore),
         averageDuplicate = reactive(input$averageDuplicate),
         nSim = reactive(input$nSimulation))
  })
}

#efficacy metric input
twoDrugs.efficacyMetricInput <- function(id) {
  ns <- NS(id)
  textInput(ns("efficacyMetric"), "Your Efficacy Metric Name (can be empty)", "Viability", width = '70%')
}

twoDrugs.efficacyMetricServer <- function(id, fileType) {
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








#########   put all the elements together   ##########

twoDrugs.ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 3, status = "primary", solidHeader = TRUE, title="2 Drugs Input",
        twoDrugs.drugInput(ns("drugSelection")),
        twoDrugs.cellLineInput(ns("cellLineSelection")),
        twoDrugs.doseInput(ns("doseSelection")),
        tags$hr(),
        twoDrugs.parametersInput(ns("parametersCheck")),
        twoDrugs.nSimulationInput(ns("n_simulation")),
        twoDrugs.efficacyMetricInput(ns("efficacyMetric")),
        tags$hr(),
        actionButton(ns("button"), "RUN")
    ),
    box(width = 9, status = "primary", solidHeader = TRUE, title="2Drug Result",
        downloadButton(ns('downloadData'), 'Download DataTable'),
        downloadButton(ns('downloadPlot'), 'Download 3D Plot'),
        
        conditionalPanel(condition = "input.button",ns = ns, tabsetPanel(type = "tabs",
                                                                         tabPanel("Table", withSpinner(dataTableOutput(ns("table")))),
                                                                         tabPanel("3dPlot", withSpinner(rglwidgetOutput(ns("plot"),  width = 400, height = 400))))  
        )
    )
    
  )
}



twoDrugs.server <- function(id, fileInfo) {
  moduleServer(id, function(input,output,session) {
    dataset <- fileInfo$dataset

    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    selectedDrugs <- twoDrugs.drugServer("drugSelection", dataset)
    
    selectedCellLinesAndSubgroups <- twoDrugs.cellLineServer("cellLineSelection", dataset)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups
    
    selectedDose <- twoDrugs.doseServer("doseSelection", dataset, selectedDrugs$d1, selectedDrugs$d2, selectedCellLines)
    
    checkedParameters <- twoDrugs.parametersServer("parametersCheck",fileType)
    
    nSim <- checkedParameters$nSim
    
    efficacyMetric <- twoDrugs.efficacyMetricServer("efficacyMetric", fileType)
    
    tableResult <- eventReactive(input$button, {

      validate(
        need(!is.null(dataset()), "Please upload your data"),
        need(!is.null(selectedDrugs$d1()), "Please select drug 1"),
        need(!is.null(selectedDrugs$d2()), "Please select drug 2"),
        need(!is.null(selectedCellLines()), "Please select Cell lines"),
        need(!is.null(selectedDose$dose1()), "Please select doses of drug 1"),
        need(!is.null(selectedDose$dose2()), "Please select doses of drug 2")
      )

      select1<-filter(dataset(),
                      Drug == selectedDrugs$d1(),
                      Cell_Line %in% selectedCellLines(),
                      Drug_Dose %in% selectedDose$dose1())
      select2<-filter(dataset(),
                      Drug == selectedDrugs$d2(),
                      Cell_Line %in% selectedCellLines(),
                      Drug_Dose %in% selectedDose$dose2())


      select <- rbindlist(list(select1,select2))
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL
      
      res_list <- IDAPredict.2drug(
        Monotherapy_Data = select,
        Cell_Line_Name_Column = "Cell_Line",
        Drug_Name_Column = "Drug",
        Drug_Concentration_Column = "Drug_Dose",
        Efficacy_Column = "Efficacy",
        LowerEfficacyIsBetterDrugEffect = checkedParameters$isLowerEff(),
        Efficacy_Metric_Name = efficacyMetric(),
        Drug1 = selectedDrugs$d1(),
        Drug2 = selectedDrugs$d2(),
        Calculate_Uncertainty = checkedParameters$uncertainty(),
        Efficacy_SE_Column = eff_se_col,
        n_Simulations = nSim(),
        Calculate_IDAcomboscore_And_Hazard_Ratio = checkedParameters$comboscore(),
        Average_Duplicate_Records = checkedParameters$averageDuplicate()
      )
      res_dataframe <- res_list[[1]]
      cbind(Drug_1 = res_list[[2]],
            Drug_2 = res_list[[3]],
            res_dataframe,
            Numbers_of_Used_Cell_Lines = length(res_list[[4]]))

    })
    
    plot.data <- reactive({
      plot.data <- tableResult()[,c("Drug_1", "Drug_2", "Drug1Dose", "Drug2Dose", "Mean_Combo_Viability")]
      plot.data$Group <- "Predicted Combination"
      plot.data$Group[plot.data$Drug1Dose == 0] <- paste0(plot.data$Drug_2[1], " Monotherapy")
      plot.data$Group[plot.data$Drug2Dose == 0] <- paste0(plot.data$Drug_1[1], " Monotherapy")
      to.add <- as.data.frame(matrix(c(rep(0, ncol(plot.data)-1), "Origin"), ncol = ncol(plot.data)))
      colnames(to.add) <- colnames(plot.data)
      plot.data <- rbind(plot.data, to.add)

      plot.data$Group <- factor(plot.data$Group, levels = c(paste0(plot.data$Drug_1[1], " Monotherapy"), paste0(plot.data$Drug_2[1], " Monotherapy"), "Origin", "Predicted Combination"))
      plot.data$Drug1Dose <- as.numeric(plot.data$Drug1Dose)
      plot.data$Drug2Dose <- as.numeric(plot.data$Drug2Dose)
      plot.data$Mean_Combo_Viability <- as.numeric(plot.data$Mean_Combo_Viability)
      plot.data$Mean_Combo_Viability <- plot.data$Mean_Combo_Viability * 100
      plot.data
    })
    
    rglScene <- eventReactive(input$button,{
      groups <- levels(plot.data()$Group)
      origin <- which(groups == "Origin")
      groups <- groups[-origin]
      colors <- c("blue", "green", "white", "magenta")
      rgl.open(useNULL = rgl.useNULL())
      scatter3d(x = plot.data()$Drug2Dose, y = plot.data()$Mean_Combo_Viability, z = plot.data()$Drug1Dose, 
                surface = F, grid = F, ellipsoid = F, xlab = "", zlab = "", ylab = "", 
                groups = plot.data()$Group, axis.ticks = F, axis.scales = T, axis.col = c("blue", "black", "darkgreen"), surface.col = colors)
      par3d(windowRect = c(-1873, -433, -590, 530))
      par3d(userMatrix = matrix(c(0.5643716, -0.006304999, -0.82549691, 0, -0.1164742, 0.989359140, -0.08718707, 0, 0.8172630, 0.145355001, 0.55763179, 0, 0, 0, 0, 1), 
                                nrow = 4, ncol = 4, byrow = T))
      par3d(zoom = 1.1)
      at <- seq(0,1, length.out = 5)
      at <- at[2:4]
      z.labels <- formatC(seq(0,max(plot.data()$Drug1Dose), length.out = 5), format = "g", digits = 2)[2:4]
      y.labels <- seq(0,100, length.out = 5)[2:4]
      x.labels <- formatC(seq(0,max(plot.data()$Drug2Dose), length.out = 5), format = "g", digits = 2)[2:4]
      rgl.texts(x = at, y = -0.05, z = 0, text = x.labels, col = "blue")
      rgl.texts(0, -0.1, at, z.labels, col = "darkgreen")
      rgl.texts(-0.05, at, -0.05, y.labels, col = "black")
      
      best.monotherapy <- min(plot.data()$Mean_Combo_Viability[grep("Monotherapy", plot.data()$Group)])
      planes3d(a = 0, b = -1, c = 0, d = best.monotherapy/100, alpha = 0.5, col = "green")
      max.z <- signif(as.numeric(formatC(seq(0,max(plot.data()$Drug1Dose), length.out = 5), format = "g", digits = 2)[5]), 1)
      max.x <- signif(as.numeric(formatC(seq(0,max(plot.data()$Drug2Dose), length.out = 5), format = "g", digits = 2)[5]), 1)
      max.x.point <- max(plot.data()$Drug2Dose)
      max.z.point <- max(plot.data()$Drug1Dose)
      x.coord <- min(max.x.point/max.x, 1)
      z.coord <- min(max.z.point/max.z, 1)
      ##
      # arrow3d(p0 = c(x.coord, best.monotherapy/100, z.coord), p1 = c(x.coord, min(plot.data()$Mean_Combo_Viability[plot.data()$Group == "Predicted Combination"])/100, z.coord),
      #         type = "flat", col = "red", theta = pi/6)
      
      legend3d("topleft", legend = groups, col = colors[1:(length(groups)+1)][-origin], pch = 16, cex=3, inset=c(0.08, 0.07))
      
      rglwidget()
    })
    
    output$table <- renderDataTable(tableResult(),
                                    options = list(scrollX = TRUE))
    
    output$plot <- renderRglwidget({
      rglScene()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(con) {
        write_delim(tableResult(), con, delim = "\t")
      }
    )
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste('plot-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        htmlwidgets::saveWidget(rglScene(), con,selfcontained = TRUE)
      }
    )
  })
}

