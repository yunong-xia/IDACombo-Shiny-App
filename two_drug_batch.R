


#batch input
twoDrugs.batch.drugInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("drugs_batch"),"Select Drugs for Combinations (Multiple)",
              choices = NULL,
              options = list(`actions-box` = TRUE,`liveSearchStyle` = "startsWith" , `liveSearch` = TRUE),
              multiple = T)
}

twoDrugs.batch.drugServer <- function(id, dataset) {
  stopifnot(is.reactive(dataset))
  moduleServer(id, function(input,output,session) {
    
    observeEvent(dataset(), {
      drug_choices <- unique(dataset()$Drug)
      updatePickerInput(session, inputId = "drugs_batch", label = "Select Drugs for Combinations (Multiple)",
                        choices = drug_choices)
    })
    
    reactive(input$drugs_batch)
  })
}

twoDrugs.batch.cellLinesThresholdInput <- function(id) {
  ns <- NS(id)
  numericInput(inputId = ns("clThreshold"), label = "Cell Lines Number Threshold", value = 10, min = 2, max = 1000)
}

twoDrugs.batch.cellLinesThresholdServer <- function(id) {
  moduleServer(id, function(input,output,server){
    reactive(input$clThreshold)
  })
}


twoDrugs.batch.cellLineInput <- function(id) {
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


twoDrugs.batch.cellLineServer <- function(id, dataset) {
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




twoDrugs.batch.nSimulationInput <- function(id) {
  ns <- NS(id)
  numericInput(inputId = ns("nSim"), label = "Number of random samples to be drawn when calculating output efficacy prediction uncertainties", value = 1000, min = 40, max = 5000)
}

twoDrugs.batch.nSimulationServer <- function(id) {
  moduleServer(id, function(input,output,session){
    reactive(input$nSim)
  })
}



















twoDrugs.batch.ui <- function(id) {
  ns <- NS(id) 
  tagList(
    box(width = 3, status = "primary", solidHeader = TRUE, title="Batch Process Input",
        twoDrugs.batch.drugInput(ns("drugSelection_batch")),
        twoDrugs.batch.cellLineInput(ns("cellLineSelection_batch")),
        twoDrugs.batch.cellLinesThresholdInput(ns("cellLinesThreshold")),
        tags$hr(),
        twoDrugs.parametersInput(ns("parametersCheck_batch")),
        twoDrugs.batch.nSimulationInput(ns("n_simulation")),
        twoDrugs.efficacyMetricInput(ns("efficacyMetric_batch")),
        tags$hr(),
        actionButton(ns("button_batch"), "RUN")
    ),
    box(width = 9, status = "primary", solidHeader = TRUE, title="2Drug Batch Process Result",
        downloadButton(ns('downloadData_batch'), 'Download DataTable'),
        wellPanel(verbatimTextOutput(ns("log"))),
        conditionalPanel(condition = "input.button_batch",ns = ns, tabsetPanel(type = "tabs",
                                                                               tabPanel("Table", withSpinner(dataTableOutput(ns("table_batch")))))
        )
    )
  )
}




twoDrugs.batch.server <- function(id, fileInfo) {
  moduleServer(id, function(input, output, session) {
    # split the fileInfo into 1.dataset and 2.extraCol
    dataset <- fileInfo$dataset
    
    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    # get user Input
    selectedDrugs <- twoDrugs.batch.drugServer("drugSelection_batch",dataset)

    selectedCellLinesAndSubgroups <- twoDrugs.batch.cellLineServer("cellLineSelection_batch",dataset)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups

    checkedParameters <- twoDrugs.parametersServer("parametersCheck_batch", fileType)
    
    nSim <- twoDrugs.batch.nSimulationServer("n_simulation")

    efficacyMetric <- twoDrugs.efficacyMetricServer("efficacyMetric_batch", fileType)
    
    clThreshold <- twoDrugs.batch.cellLinesThresholdServer("cellLinesThreshold")
    
    # compute result and generate some message
    logText <- reactiveVal(NULL)
    
    tableResult <- reactiveVal(NULL)
    
    output$log <- renderText(logText())
    
    output$table_batch <- renderDataTable(tableResult(),
                                          options = list(scrollX = TRUE))
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(con) {
        write_delim(tableResult(), con, delim = "\t")
      }
    )
    
    
    observeEvent(input$button_batch,{
      if(is.null(dataset())){
        logText("Please Upload Your Data First!")
        return()
      }
      logText("")
      selected_drug <- selectedDrugs()
      pairs <- lapply(1:(length(selected_drug)-1), function(i){
        expand.grid(selected_drug[i], selected_drug[(i+1):length(selected_drug)])
      }) %>%
        rbindlist()
      cl<-selectedCellLines()
      data <- dataset()
      shared_cl_numbers <- pairs %>%
        apply(1,function(p){
          p = as.vector(t(p))
          d1 = p[1]
          d2 = p[2]
          cl1 = unique(data$Cell_Line[data$Drug == d1])
          cl2 = unique(data$Cell_Line[data$Drug == d2])
          shared = cl1[cl1%in%cl2]
          length(which(shared %in% cl))
        })
      
      #create an error msg
      unusable_index = which(shared_cl_numbers < clThreshold())
      msg <- ""
      if(length(unusable_index) > 0) {
        for(i in 1:length(unusable_index)){
          index = unusable_index[i]
          p = as.vector(t(pairs[index,]))
          msg = paste0(msg,"Combo ", p[1], " + ", p[2], " have only ", shared_cl_numbers[index], " shared cell lines\n")
        }
        #assign error msg to the reactive
        logText(paste0("Error:\n",msg))
      } else {
        logText("No errors\n")
      }
      
      ##
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL
      #filter by threshold
      usable_index <- which(shared_cl_numbers >= clThreshold())
      usable_pairs <- pairs[usable_index,]
      n_usable_cl <- shared_cl_numbers[usable_index]
      monoDataList <- lapply(selectedDrugs(),function(x){data[data$Drug == x,]})
      # localize reactive value in order to be used in getRes function.
      names(monoDataList) <- selectedDrugs()
      isLowerEfficacy <- checkedParameters$isLowerEfficacy()
      uncertainty <- checkedParameters$uncertainty()
      comboscore <- checkedParameters$comboscore()
      averageDuplicate <- checkedParameters$averageDuplicate()
      efficacy_metric <- efficacyMetric()
      n_sim <- nSim()
      getRes <- function(p){
        p = as.vector(t(p))
        monoData = rbindlist(list(monoDataList[[p[1]]],monoDataList[[p[2]]]))
        ls <- IDAPredict.2drug(
          monoData,
          Cell_Line_Name_Column = "Cell_Line",
          Drug_Name_Column = "Drug",
          Drug_Concentration_Column = "Drug_Dose",
          Efficacy_Column = "Efficacy",
          LowerEfficacyIsBetterDrugEffect = isLowerEfficacy,
          Efficacy_Metric_Name = efficacy_metric,
          Drug1 = p[1],
          Drug2 = p[2],
          Calculate_Uncertainty = uncertainty,
          Efficacy_SE_Column = eff_se_col,
          n_Simulations = n_sim,
          Calculate_IDAcomboscore_And_Hazard_Ratio = comboscore,
          Average_Duplicate_Records = averageDuplicate
        )
      }
      
      #A variable store the result
      res <- NULL
      
      if(nrow(usable_pairs) == 0) {
        res <- NULL
      }
      else if(nrow(usable_pairs) <= 30) {
        withProgress(message = 'Computing...', value = 0, {
          res_list <- list()
          for(i in nrow(usable_pairs)) {
            res_list <- c(res_list, list(getRes(usable_pairs[i,])))
            incProgress(1/nrow(usable_pairs))
          }
          res <- res_list %>%
            lapply(function(x){cbind(Drug_1 = x[["Drug1"]],
                                                      Drug_2 = x[["Drug2"]],
                                                      x[["Efficacy_Predictions"]],
                                                      Numbers_of_Used_Cell_Lines = length(x[["Cell_Line_Used"]]))}
          ) %>%
            rbindlist()
        })
      }
      else{
        progress = AsyncProgress$new(message="Computing...")
        res_list <- list()
        Runs = 4
        for(i in 1:Runs){
          range <- floor(length(usable_index)/Runs)
          pair_seq <- ((i-1)*range+1):(i*range)
          #multiprocess
          res_list[[i]] <- future({
            subRes <- list()
            for(j in pair_seq){
              subRes<-c(subRes,list(getRes(usable_pairs[j,])))
              progress$inc(1/length(usable_index))
            }
            subRes <- subRes %>%
              lapply(function(x){cbind(Drug_1 = x[["Drug1"]],
                                       Drug_2 = x[["Drug2"]],
                                       x[["Efficacy_Predictions"]],
                                       Numbers_of_Used_Cell_Lines = length(x[["Cell_Line_Used"]]))}
                     ) %>%
              rbindlist()
            return(subRes)
          })
        }
        #then solve for remainder
        remainder <- future({
          if(length(usable_index) %% Runs != 0){
            range <- floor(length(usable_index)/Runs)
            pair_seq <- (Runs*range+1):length(usable_index)
            subRes <- list()
            for(j in pair_seq){
              subRes<-c(subRes,list(getRes(usable_pairs[j,])))
              progress$inc(1/length(usable_index))
            }
            subRes <- subRes %>%
              lapply(function(x){cbind(Drug_1 = x[["Drug1"]],
                                       Drug_2 = x[["Drug2"]],
                                       x[["Efficacy_Predictions"]],
                                       Numbers_of_Used_Cell_Lines = length(x[["Cell_Line_Used"]]))}
              ) %>%
              rbindlist()
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










