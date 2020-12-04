
# control treatment
controlPlusOne.batch.controlTreatmentInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("drugs"), "Select Drugs in Treatment (Multiple)",
    choices = NULL,
    options = list(`actions-box` = TRUE, `live-search-style` = "startsWith", `live-search` = TRUE),
    multiple = T
  )
}

controlPlusOne.batch.controlTreatmentServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    observeEvent(dataset(), {
      drug_choices <- unique(dataset()$Drug)
      updatePickerInput(session,
        inputId = "drugs", label = "Select drugs in treatment (Multiple)",
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
  pickerInput(ns("drugToAdd"), label = "Drug to Add (Multiple Drugs)", choices = NULL, multiple = T,
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
        inputId = "drugToAdd", label = "Drug to Add",
        choices = to_add_choices
      )
    })

    reactive(input$drugToAdd)
  })
}



controlPlusOne.batch.cellLineInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("subgroups"), "Select Cell Lines By Subgroups",
      choices = NULL,
      options = list(`live-search-style` = "startsWith", `live-search` = TRUE),
      multiple = T
    ),
    div(style="display:inline-block;width:40%;text-align: center;",actionButton(ns("selectAllSubgroups"),"All Subgroups")),
    div(style="display:inline-block;width:40%;text-align: center;",actionButton(ns("deselectAllSubgroups"),"Clean Subgroups")),
    pickerInput(ns("cell_lines"), "Cell-Line available for both drugs (Multiple)",
      choices = list(
        `Cancer Cell Lines` = NULL
      ),
      options = list(
        `actions-box` = TRUE, `live-search-style` = "startsWith", `live-search` = TRUE,
        `selected-text-format` = "count",
        `count-selected-text` = "{0} of cell lines chosen (on a total of {1})"
      ),
      multiple = T
    )
  )
}


# shared among all the selected Drugs
controlPlusOne.batch.cellLineServer <- function(id, dataset) {
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
      controlPlusOne.batch.cellLineInput(ns("cellLineSelection")),
      tags$hr(),
      controlPlusOne.batch.parametersInput(ns("parametersCheck")),
      tags$hr(),
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
    
    efficacyMetric <- fileInfo$efficacyMetric
    
    isLowerEfficacy <- fileInfo$isLowerEfficacy

    selectedControlTreatment <- controlPlusOne.batch.controlTreatmentServer("controlTreatmentSelection", dataset)

    selectedDose <- controlPlusOne.batch.doseServer("doseSelection", dataset, fileType, selectedControlTreatment)

    selectedDrugToAdd <- controlPlusOne.batch.drugToAddServer("drugToAddSelection", dataset, selectedControlTreatment)

    selectedCellLinesAndSubgroups <- controlPlusOne.batch.cellLineServer("cellLineSelection", dataset)

    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines

    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups

    checkedParameters <- controlPlusOne.batch.parametersServer("parametersCheck", fileType, isLowerEfficacy)

    nSim <- checkedParameters$nSim

    warningMessage <- reactiveVal()

    result <- eventReactive(input$button, {
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

      warning_msg <- ""
      res_list <- vector("list", length = length(selectedDrugToAdd()))
      monotherapy_data <- dataset()[dataset()$Cell_Line %in% selectedCellLines(), ]
      withProgress(message = "Computing...", value = 0, {
        for (i in seq_along(selectedDrugToAdd())) {
          # get mono data
          res_list[[i]] <- withCallingHandlers(
            tryCatch(
              expr = {
                res <- IDAPredict.ControlPlusOne(
                  Monotherapy_Data = monotherapy_data,
                  Cell_Line_Name_Column = "Cell_Line",
                  Drug_Name_Column = "Drug",
                  Drug_Concentration_Column = "Drug_Dose",
                  Efficacy_Column = "Efficacy",
                  LowerEfficacyIsBetterDrugEffect = checkedParameters$isLowerEfficacy(),
                  Efficacy_Metric_Name = efficacyMetric(),
                  Control_Treatment_Drugs = selectedControlTreatment(),
                  Control_Treatment_Drug_Concentrations = selectedDose(),
                  Drug_to_Add = selectedDrugToAdd()[i],
                  Calculate_Uncertainty = checkedParameters$uncertainty(),
                  Efficacy_SE_Column = eff_se_col,
                  n_Simulations = nSim(),
                  Calculate_IDAcomboscore_And_Hazard_Ratio = checkedParameters$comboscore(),
                  Average_Duplicate_Records = checkedParameters$averageDuplicate()
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
          incProgress(1 / length(selectedDrugToAdd()))
        }
      })
      if (nchar(warning_msg) == 0) {
        warning_msg <- "No warning messages"
      }
      warningMessage(warning_msg)
      return(rbindlist(res_list))
    })

    output$table <- DT::renderDataTable(
      {
        result()[, names(result()) != "Cell_Lines_Used", with = F] # it is a data.table, rather than data.frame
      },
      options = list(scrollX = TRUE)
    )

    output$log <- renderText({
      warningMessage()
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        write_delim(result(), file, delim = "\t")
      }
    )


    output$downloadLog <- downloadHandler(
      filename = function() {
        paste("log-", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        write(warningMessage(), file)
      }
    )
  })
}
