


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
                multiple = T)
  )
}

twoDrugs.batch.drugServer <- function(id, dataset) {
  moduleServer(id, function(input,output,session) {
    
    observeEvent(dataset(), {
      drug_choices <- unique(dataset()$Drug)
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


twoDrugs.batch.cellLineInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("subgroups"),"Select Cell Lines By Subgroups",
                choices = NULL,
                options = list(`live-search-style` = "startsWith" , `live-search` = TRUE, `none-Selected-Text` = "Optional"),
                multiple = T),
    div(style="display:inline-block;width:40%;text-align: center;",actionButton(ns("selectAllSubgroups"),"All Subgroups")),
    div(style="display:inline-block;width:40%;text-align: center;",actionButton(ns("deselectAllSubgroups"),"Clean Subgroups")),
    pickerInput(ns("cell_lines"),"Select Cell Lines",
                choices = NULL,
                options = list(`actions-box` = TRUE,`live-search-style` = "startsWith" , `live-search` = TRUE,
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




twoDrugs.batch.ui <- function(id) {
  ns <- NS(id) 
  tagList(
    box(width = 3, status = "primary", solidHeader = TRUE, title="Batch Process Input",
        twoDrugs.batch.drugInput(ns("drugSelection_batch")),
        twoDrugs.batch.cellLineInput(ns("cellLineSelection_batch")),
        tags$hr(),
        twoDrugs.parametersInput(ns("parametersCheck_batch")),
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
    # split the fileInfo into 1.dataset and 2.extraCol
    dataset <- fileInfo$dataset
    
    extraCol <- fileInfo$extraCol
    
    fileType <- fileInfo$type
    
    efficacyMetric <- fileInfo$efficacyMetric
    
    isLowerEfficacy <- fileInfo$isLowerEfficacy
    
    # get user Input
    selectedFixedDrugAndDrugsToAdd <- twoDrugs.batch.drugServer("drugSelection_batch",dataset)
    
    selectedFixedDrug <- selectedFixedDrugAndDrugsToAdd$fixed_drug
    
    selectedDrugsToAdd <- selectedFixedDrugAndDrugsToAdd$drugs_to_add

    selectedCellLinesAndSubgroups <- twoDrugs.batch.cellLineServer("cellLineSelection_batch",dataset)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups

    checkedParameters <- twoDrugs.parametersServer("parametersCheck_batch", fileType, isLowerEfficacy)
    
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
              progress$set(value = i-1, message = paste0(i-1, " of ", length(temp_conc_list), " Compounds Complete..."))
            } else {
              Aborted <- i-1
              progress$set(value = i-1, message = "Aborting calculation...")
              warning_msg <- paste0("WARNING: Calculation aborted after ", Aborted, " of ", length(temp_conc_list)," compounds complete. WARNING: Calculation failed. Please reload the web page and try again. Contact us if this problem persists.")
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










