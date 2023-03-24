testVsControl.batch.fileInput <- function(id) {
  ns <- NS(id)
  fileInput(ns("controlTreatmentFile"), "Upload your input control treatment files",
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  )
}

testVsControl.batch.fileServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    fileContent <- reactive({
      inFile <- input$controlTreatmentFile
      if (is.null(inFile)) {
        return(NULL)
      }
      content <- read.delim(input$controlTreatmentFile$datapath)
      headers <- names(content)
      fixedHeaderNames <- c("Control_Treatment_Drugs", "Control_Treatment_Doses", "Test_Treatment_Drugs", "Test_Treatment_Doses")
      missedCol <- setdiff(fixedHeaderNames, headers)
      validate(
        need(length(missedCol) == 0, paste0("Warning: Please modify the column names to : ", missedCol))
      )
      content
    })

    fileContent
  })
}

testVsControl.batch.cellLineInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("subgroups"), "Select Cell Lines By Subgroups",
      choices = NULL,
      options = list(`live-search-style` = "startsWith", `live-search` = TRUE),
      multiple = T
    ),
    div(style="display:inline-block;width:10%;text-align: center;",actionButton(ns("selectAllSubgroups"),"All Subgroups")),
    div(style="display:inline-block;width:10%;text-align: center;",actionButton(ns("deselectAllSubgroups"),"Clean Subgroups")),
    pickerInput(ns("cell_lines"), "Cell-Line available for both drugs (Multiple)",
      choices = list(
        `Cancer Cell Lines` = NULL
      ),
      options = list(
        `actions-box` = TRUE, `live-search-style` = "startsWith", `live-search` = TRUE,
        `selected-text-format` = "count",
        `count-selected-text` = "{0} models choosed (on a total of {1})"
      ),
      multiple = T
    )
  )
}

testVsControl.batch.cellLineServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    cl_sg_set <- reactive(
      if (!is.null(dataset())) {
        distinct(dataset()[, c("Cell_Line", "Cell_Line_Subgroup")])
      } else {
        NULL
      }
    )

    cell_line_choices <- reactive(
      unique(cl_sg_set()$Cell_Line)
    )

    subgroups_choices <- reactive(
      unique(cl_sg_set()$Cell_Line_Subgroup)
    )

    observeEvent(c(dataset()), {
      updatePickerInput(session,
        inputId = "cell_lines", label = "Select Cell Lines",
        choices = cell_line_choices()
      )
      updatePickerInput(session,
        inputId = "subgroups", label = "Select Cell Lines By Subgroups",
        selected = NULL,
        choices = c("Custom", subgroups_choices())
      )
    })

    prev_selected_cell_lines <- reactiveVal(value = NULL)
    prev_selected_subgroups <- reactiveVal(value = NULL)

    observeEvent(input$subgroups,
      {
        if ("Custom" %in% prev_selected_subgroups()) { ## Previously selected "Custom"
          if ("Custom" %in% input$subgroups && length(input$subgroups) > 1) { ## now select other subgroups.
            new_subgroups <- setdiff(input$subgroups, "Custom")
            new_cell_lines <- cl_sg_set()$Cell_Line[cl_sg_set()$Cell_Line_Subgroup %in% new_subgroups]
            prev_selected_cell_lines(new_cell_lines)
            prev_selected_subgroups(new_subgroups)
            updatePickerInput(session,
              inputId = "cell_lines", label = "Select Cell Lines",
              selected = new_cell_lines,
              choices = cell_line_choices()
            )
            updatePickerInput(session,
              inputId = "subgroups", label = "Select Cell Lines By Subgroups",
              selected = new_subgroups,
              choices = c("Custom", subgroups_choices())
            )
          }
          else if (is.null(input$subgroups)) { # when users deselect "Custom"
            prev_selected_cell_lines(NULL)
            updatePickerInput(session,
              inputId = "cell_lines", label = "Select Cell Lines",
              selected = NULL,
              choices = cell_line_choices()
            )
          }
          else {
            # do nothing
          }
        }
        else { ## Previously didn't select "Custom"
          if ("Custom" %in% input$subgroups) { ## newly select "Custom"
            prev_selected_subgroups("Custom")
            updatePickerInput(session,
              inputId = "subgroups", label = "Select Cell Lines By Subgroups",
              selected = "Custom",
              choices = c("Custom", subgroups_choices())
            )
          }
          else { # just select other subgroups
            new_subgroups <- input$subgroups
            new_cell_lines <- cl_sg_set()$Cell_Line[cl_sg_set()$Cell_Line_Subgroup %in% new_subgroups]
            prev_selected_cell_lines(new_cell_lines)
            prev_selected_subgroups(new_subgroups)
            updatePickerInput(session,
              inputId = "cell_lines", label = "Select Cell Lines",
              selected = new_cell_lines,
              choices = cell_line_choices()
            )
            updatePickerInput(session,
              inputId = "subgroups", label = "Select Cell Lines By Subgroups",
              selected = new_subgroups,
              choices = c("Custom", subgroups_choices())
            )
          }
        }
      },
      ignoreNULL = F
    )

    observeEvent(input$selectAllSubgroups, {
      prev_selected_subgroups(subgroups_choices())
      updatePickerInput(session,
        inputId = "subgroups", label = "Select Cell Lines By Subgroups",
        selected = subgroups_choices(),
        choices = c("Custom", subgroups_choices())
      )
    })

    observeEvent(input$deselectAllSubgroups, {
      prev_selected_subgroups(NULL)
      updatePickerInput(session,
        inputId = "subgroups", label = "Select Cell Lines By Subgroups",
        selected = NULL,
        choices = c("Custom", subgroups_choices())
      )
    })


    observeEvent(input$cell_lines,
      {
        if ("Custom" %in% input$subgroups) {
          prev_selected_cell_lines(input$cell_lines)
        }
        else {
          # check whether the selected cell line match those subgroups.
          if (!is.null(cl_sg_set()) && length(unique(cl_sg_set()$Cell_Line[cl_sg_set()$Cell_Line_Subgroup %in% input$subgroups])) != length(input$cell_lines)) {
            prev_selected_cell_lines(input$cell_lines)
            prev_selected_subgroups(input$subgroups)
            updatePickerInput(session,
              inputId = "subgroups",
              label = "Select Cell Lines By Subgroups",
              selected = "Custom",
              choices = c("Custom", subgroups_choices())
            )
          }
        }
      },
      ignoreNULL = F
    )

    list(
      cellLines = reactive(input$cell_lines),
      subgroups = reactive(input$subgroups)
    )
  })
}



testVsControl.batch.parametersInput <- function(id) {
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
    checkboxInput(ns("hazardRatio"), "Calculate Hazard Ratios") %>%
      helper(type = "inline",
             title = "Calculate IDAComboscore And HazardRatios",
             icon = "question-circle", colour = NULL,
             content = "Should Hazard Ratios (HRs) be calculated between the test and control therapies? Note that these values are only meaningful when efficacy values are scaled between 0 and 1 (i.e. viability, normalized AUC, etc.).",
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

testVsControl.batch.parametersServer <- function(id, fileType, isLowerEfficacy) {
  moduleServer(id, function(input, output, session) {

    list(
      isLowerEfficacy = isLowerEfficacy,
      uncertainty = reactive(input$uncertainty),
      hazardRatio = reactive(input$hazardRatio),
      averageDuplicate = reactive(input$averageDuplicate),
      nSim = reactive(input$nSimulation)
    )
  })
}




testVsControl.batch.ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = NULL, status = "primary", solidHeader = TRUE, title = "Control Treatment Input",
      h5("Please upload a text file containig control treatments information."),
      h6("Before uploading your file, please modify the header name to:"),
      h6("Control_Treatment_Drugs;"),
      h6("Control_Treatment_Doses;"),
      h6("Test_Treatment_Drugs;"),
      h6("Test_Treatment_Doses;"),
      h6("for each control treatment and its corresponding doses, splite each element by ','"),
      testVsControl.batch.fileInput(ns("batchInputFile")),
      testVsControl.batch.cellLineInput(ns("cellLineSelection")),
      tags$hr(),
      testVsControl.batch.parametersInput(ns("parametersCheck_batch")),
      tags$hr(),
      actionButton(ns("button"), "RUN")
    ),
    box(
      width = NULL, status = "primary", solidHeader = TRUE, title = "Result",
      downloadButton(ns("downloadData"), "Download DataTable"),
      downloadButton(ns("downloadLog"), "Download Log"),
      conditionalPanel(condition = "input.button", ns = ns, tabsetPanel(
        type = "tabs",
        tabPanel("Table", withSpinner(DT::dataTableOutput(ns("table")))),
        tabPanel("Log", withSpinner(verbatimTextOutput(ns("log"))))
      ))
    )
  )
}

testVsControl.batch.server <- function(id, fileInfo) {
  moduleServer(id, function(input, output, session) {
    dataset <- fileInfo$dataset

    extraCol <- fileInfo$extraCol

    fileType <- fileInfo$type
    
    efficacyMetric <- fileInfo$efficacyMetric
    
    isLowerEfficacy <- fileInfo$isLowerEfficacy

    batchInput <- testVsControl.batch.fileServer("batchInputFile")

    selectedCellLineAndSubgroups <- testVsControl.batch.cellLineServer("cellLineSelection", dataset)

    selectedCellLine <- selectedCellLineAndSubgroups$cellLines

    selectedSubgroups <- selectedCellLineAndSubgroups$subgroups

    checkedParameters <- testVsControl.batch.parametersServer("parametersCheck_batch", fileType, isLowerEfficacy)

    nSim <- checkedParameters$nSim

    warningMessage <- reactiveVal(NULL)

    tableResult <- reactiveVal(NULL)

    output$log <- renderText(warningMessage())

    output$table <- DT::renderDataTable(
      {
        tableResult()[, names(tableResult()) != "Cell_Lines_Used", with = F] # it is a data.table, rather than data.frame
      },
      options = list(scrollX = TRUE)
    )

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".txt", sep = "")
      },
      content = function(con) {
        write_delim(tableResult(), con, delim = "\t")
      }
    )

    output$downloadLog <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".txt", sep = "")
      },
      content = function(con) {
        write_delim(warningMessage(), con, delim = "\t")
      }
    )

    observeEvent(input$button, {
      validate(
        need(!is.null(dataset()), "Please upload your dataset"),
        need(!is.null(batchInput()), "Please upload your input file"),
        need(!is.null(selectedCellLine()), "Please select cell lines")
      )

      controlTreatmentList <- list()
      testTreatmentList <- list()

      for (i in 1:nrow(batchInput())) {
        ctDrugs <- strsplit(batchInput()[i, ]$Control_Treatment_Drugs, ",")[[1]]
        ctDoses <- strsplit(batchInput()[i, ]$Control_Treatment_Doses, ",")[[1]]
        testDrugs <- strsplit(batchInput()[i, ]$Test_Treatment_Drugs, ",")[[1]]
        testDoses <- strsplit(batchInput()[i, ]$Test_Treatment_Doses, ",")[[1]]
        validate(
          need(length(ctDrugs) == length(ctDoses), "Length of control treatment drugs and doses does not match"),
          need(length(testDrugs) == length(testDoses), "Length of test treatment drugs and doses does not match")
        )
        controlTreatmentList[[i]] <- data.frame(
          Drug = ctDrugs,
          Dose = as.numeric(ctDoses),
          stringsAsFactors = F
        )
        testTreatmentList[[i]] <- data.frame(
          Drug = testDrugs,
          Dose = as.numeric(testDoses),
          stringsAsFactors = F
        )
      }


      if ("seCol" %in% extraCol()) {
        eff_se_col <- "Efficacy_SE"
      } else {
        eff_se_col <- NULL
      }
      
      calculateUncertainty <- checkedParameters$uncertainty()
      if(is.null(eff_se_col)){
        calculateUncertainty <- FALSE #preventing user from trying to calculate uncertainties without SE col, would like to put warning about this somewhere, but not sure best way to do that.
      }

      warning_msg <- ""
      res_list <- vector("list", length = length(controlTreatmentList))
      monotherapy_data <- dataset()
      withProgress(message = "Computing...", value = 0, {
        for (i in 1:length(controlTreatmentList)) {
          # get mono data
          res_list[[i]] <- withCallingHandlers(
            tryCatch(
              expr = {
                res <- IDAPredict.TestvsControl(
                  Monotherapy_Data = monotherapy_data,
                  Cell_Line_Name_Column = "Cell_Line",
                  Drug_Name_Column = "Drug",
                  Drug_Concentration_Column = "Drug_Dose",
                  Efficacy_Column = "Efficacy",
                  LowerEfficacyIsBetterDrugEffect = checkedParameters$isLowerEfficacy(),
                  Efficacy_Metric_Name = efficacyMetric(),
                  Control_Treatment_Drugs = controlTreatmentList[[i]]$Drug,
                  Control_Treatment_Drug_Concentrations = controlTreatmentList[[i]]$Dose,
                  Test_Treatment_Drugs = testTreatmentList[[i]]$Drug,
                  Test_Treatment_Drug_Concentrations = testTreatmentList[[i]]$Dose,
                  Calculate_Uncertainty = calculateUncertainty,
                  Efficacy_SE_Column = eff_se_col,
                  n_Simulations = nSim(),
                  Calculate_Hazard_Ratio = checkedParameters$hazardRatio(),
                  Average_Duplicate_Records = checkedParameters$averageDuplicate()
                )
                cat(str(res))
                if (!is.data.frame(res[[1]])) {
                  NULL
                } else {
                  res <- cbind(
                    Control_Treatment_Drugs = paste(res$Control_Treatment$Control_Treatment_Drugs, collapse = ", "),
                    Control_Treatment_Drug_Concentration = paste(res$Control_Treatment$Control_Treatment_Drug_Concentrations, collapse = ", "),
                    Test_Treatment_Drugs = paste(res$Test_Treatment$Test_Treatment_Drugs, collapse = ", "),
                    Test_Treatment_Drugs_Concentrations = paste(res$Test_Treatment$Test_Treatment_Drug_Concentrations, collapse = ", "),
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
          incProgress(1 / nrow(pairs))
        }
      })
      if (nchar(warning_msg) == 0) {
        warning_msg <- "No warning messages"
      }
      warningMessage(warning_msg)

      tableResult(rbindlist(res_list))
    })
  })
}
