


#batch input
twoDrugs.batch.drugInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("drugs_batch"),"Select Drugs for Combinations (Multiple)",
              choices = NULL,
              options = list(`actions-box` = TRUE,`live-search-style` = "startsWith" , `live-search` = TRUE),
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


twoDrugs.batch.cellLineInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("subgroups"),"Select Cell Lines By Subgroups",
                choices = NULL,
                options = list(`live-search-style` = "startsWith" , `live-search` = TRUE),
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
    selectedDrugs <- twoDrugs.batch.drugServer("drugSelection_batch",dataset)

    selectedCellLinesAndSubgroups <- twoDrugs.batch.cellLineServer("cellLineSelection_batch",dataset)
    
    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines
    
    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups

    checkedParameters <- twoDrugs.parametersServer("parametersCheck_batch", fileType, isLowerEfficacy)
    
    nSim <- checkedParameters$nSim


    # compute result and generate some message
    warningMessage <- reactiveVal(NULL)
    
    
    output$log <- renderText({
       warningMessage()
    })
    
    output$table <- renderDataTable({
        tableResult()[, names(tableResult()) != "Cell_Lines_Used", with = F] # it is a data.table, rather than data.frame
      },
      options = list(scrollX = TRUE))
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(file) {
        write_delim(tableResult(), file, delim = "\t")
      }
    )
    
    output$downloadLog <- downloadHandler(
      filename = function() {
        paste('log-', Sys.Date(), '.txt', sep='')
      },
      content = function(file) {
        write(warningMessage(), file)
      }
    )
    
    #Calculate IDACombo result.
    tableResult <- eventReactive(input$button_batch,{
      validate(
        need(!is.null(dataset()), "Please upload your data"),
        need(!is.null(selectedDrugs()), "Please select drugs"),
        need(!is.null(selectedCellLines()), "Please select Cell lines")
      )
      selected_drug <- selectedDrugs()
      pairs <- lapply(1:(length(selected_drug)-1), function(i){
        expand.grid(selected_drug[i], selected_drug[(i+1):length(selected_drug)],stringsAsFactors = F)
      }) %>%
        rbindlist()
      
      if("seCol" %in% extraCol())
        eff_se_col = "Efficacy_SE"
      else
        eff_se_col = NULL
      
      warning_msg <- ""
      res_list <- vector("list", length = length(pairs))
      monotherapy_data <- dataset()[dataset()$Cell_Line %in% selectedCellLines(),]
      withProgress(message = "Computing...", value = 0, {
        for(i in 1:nrow(pairs)) {
          #get mono data
          res_list[[i]] <- withCallingHandlers(
            tryCatch(
              expr = {
                res <- IDAPredict.2drug(
                  Monotherapy_Data = monotherapy_data,
                  Cell_Line_Name_Column = "Cell_Line",
                  Drug_Name_Column = "Drug",
                  Drug_Concentration_Column = "Drug_Dose",
                  Efficacy_Column = "Efficacy",
                  LowerEfficacyIsBetterDrugEffect = checkedParameters$isLowerEfficacy(),
                  Efficacy_Metric_Name = efficacyMetric(),
                  Drug1 = as.character(pairs[i,1]),
                  Drug2 = as.character(pairs[i,2]),
                  Calculate_Uncertainty = checkedParameters$uncertainty(),
                  Efficacy_SE_Column = eff_se_col,
                  n_Simulations = nSim(),
                  Calculate_IDAcomboscore_And_Hazard_Ratio = checkedParameters$comboscore(),
                  Average_Duplicate_Records = checkedParameters$averageDuplicate()
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
            warning = function(w) {
              warning_msg <<- paste0(warning_msg, paste0(Sys.Date(),": ",conditionMessage(w),"\n"))
              invokeRestart("muffleWarning")
            }
          )
          incProgress(1/nrow(pairs))
        }
      })
      if(nchar(warning_msg) == 0)
        warning_msg <- "No warning messages"
      warningMessage(warning_msg)
      return(rbindlist(res_list))
    })

  })
}










