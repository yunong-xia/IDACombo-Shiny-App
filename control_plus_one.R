
# control treatment
controlPlusOne.controlTreatmentInput <- function(id) {
  ns <- NS(id)
  pickerInput(ns("drugs"), "Select Drugs in Treatment (Multiple)",
    choices = NULL,
    options = list(`actions-box` = TRUE, `liveSearchStyle` = "startsWith", `liveSearch` = TRUE),
    multiple = T
  )
}

controlPlusOne.controlTreatmentServer <- function(id, dataset) {
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


controlPlusOne.doseInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("doseSelect"))
}

controlPlusOne.doseServer <- function(id, dataset, selectedControlTreatment) {
  moduleServer(id, function(input, output, session) {
    output$doseSelect <- renderUI({
      if (length(selectedControlTreatment()) > 0) {
        ns <- session$ns
        l <- lapply(1:length(selectedControlTreatment()), function(i) {
          dose_choices <- unique(dataset()$Drug_Dose[dataset()$Drug == selectedControlTreatment()[i]])
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
        (input[[paste0("dose", i)]])
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



controlPlusOne.cellLineInput <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("subgroups"), "Select Cell Lines By Subgroups",
      choices = NULL,
      options = list(`liveSearchStyle` = "startsWith", `liveSearch` = TRUE),
      multiple = T
    ),
    actionButton(ns("selectAllSubgroups"), "Select All Subgroups"),
    actionButton(ns("deselectAllSubgroups"), "Deselect All Subgroups"),
    pickerInput(ns("cell_lines"), "Cell-Line available for both drugs (Multiple)",
      choices = list(
        `Cancer Cell Lines` = NULL
      ),
      options = list(
        `actions-box` = TRUE, `liveSearchStyle` = "startsWith", `liveSearch` = TRUE,
        `selected-text-format` = "count",
        `count-selected-text` = "{0} models choosed (on a total of {1})"
      ),
      multiple = T
    )
  )
}


# shared among all the selected Drugs
controlPlusOne.cellLineServer <- function(id, dataset) {
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
controlPlusOne.parametersInput <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("isLowerEfficacy"), "Lower Efficacy Is Better Drug Effect") %>%
      helper(
        type = "inline",
        title = "Lower Efficacy Is Better Drug Effect",
        icon = "question-circle", colour = NULL,
        content = c(
          "<p style='text-indent: 40px'>whether or not lower values efficacy indicate a more effective drug effect</p>"
        ),
        size = "s",
        buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    checkboxInput(ns("uncertainty"), "Calculate Uncertainty") %>%
      helper(
        type = "inline",
        title = "Calculate Uncertainty",
        icon = "question-circle", colour = NULL,
        content = c(
          "<p style= 'text-indent:40px'>whether or not a Monte Carlo simulation should be performed to estimate uncertainties in the efficacy predictions based on uncertainties in the monotherapy efficacy measurements.</p>"
        ),
        buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    conditionalPanel(
      condition = "input.uncertainty", ns = ns,
      numericInput(inputId = ns("nSimulation"), label = "Number of random samples to be drawn when calculating output efficacy prediction uncertainties", value = 1000, min = 40, max = 5000)
    ),
    checkboxInput(ns("comboscore"), "Calculate IDAComboscore And HazardRatios") %>%
      helper(
        type = "inline",
        title = "Calculate IDAComboscore And HazardRatios",
        icon = "question-circle", colour = NULL,
        content = c(
          "<p style = 'text-indent:40px'>whether or not IDA-Comboscores and Hazard Ratios (HRs) should be calculated between monotherapies and the drug combination.</p>"
        ),
        buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      ),
    checkboxInput(ns("averageDuplicate"), "Average Duplicate Records") %>%
      helper(
        type = "inline",
        title = "Average Duplicate Records",
        icon = "question-circle", colour = NULL,
        content = c(
          "<p style = 'text-indent:40px'>whether or not duplicated records (where a cell line has multiple records for being tested with a given drug at a given concentration) should be averaged</p>"
        ),
        buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
      )
  )
}

controlPlusOne.parametersServer <- function(id, fileType) {
  moduleServer(id, function(input, output, session) {
    observeEvent(fileType(), {
      if (fileType() == "GDSC" || fileType() == "CTRPv2") {
        updateCheckboxInput(session, "isLowerEfficacy", "Lower Efficacy Is Better Drug Effect", value = TRUE)
        disable("isLowerEfficacy")
      }
      else {
        enable("isLowerEfficacy")
      }
    })

    list(
      isLowerEfficacy = reactive(input$isLowerEfficacy),
      uncertainty = reactive(input$uncertainty),
      comboscore = reactive(input$comboscore),
      averageDuplicate = reactive(input$averageDuplicate),
      nSim = reactive(input$nSimulation)
    )
  })
}




# efficacy metric input
controlPlusOne.efficacyMetricInput <- function(id) {
  ns <- NS(id)
  textInput(ns("efficacyMetric"), "Your Efficacy Metric Name (can be empty)", "Viability", width = "70%")
}

controlPlusOne.efficacyMetricServer <- function(id, fileType) {
  moduleServer(id, function(input, output, session) {
    observeEvent(fileType(), {
      if (fileType() == "GDSC" || fileType() == "CTRPv2") {
        updateTextInput(session, "efficacyMetric", label = "Your Efficacy Metric Name (can be empty)", value = "Viability")
        disable("efficacyMetric")
      }
      else {
        enable("efficacyMetric")
      }
    })

    reactive(input$efficacyMetric)
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
      controlPlusOne.cellLineInput(ns("cellLineSelection")),
      tags$hr(),
      controlPlusOne.parametersInput(ns("parametersCheck")),
      controlPlusOne.efficacyMetricInput(ns("efficacyMetric")),
      tags$hr(),
      actionButton(ns("button"), "RUN")
    ),
    box(
      width = 9, status = "primary", solidHeader = TRUE, title = "Control Plus One Result",
      downloadButton(ns("downloadData"), "Download DataTable"),
      conditionalPanel(condition = "input.button", ns = ns, tabsetPanel(
        type = "tabs",
        tabPanel("Table", withSpinner(dataTableOutput(ns("table")))),
        tabPanel("Plot", withSpinner(plotOutput(ns("plot"),  width = "100%", height = "400px")))
      ))
    )
  )
}

controlPlusOne.server <- function(id, fileInfo) {
  moduleServer(id, function(input, output, session) {
    dataset <- fileInfo$dataset

    extraCol <- fileInfo$extraCol

    fileType <- fileInfo$type

    selectedControlTreatment <- controlPlusOne.controlTreatmentServer("controlTreatmentSelection", dataset)

    selectedDose <- controlPlusOne.doseServer("doseSelection", dataset, selectedControlTreatment)

    selectedDrugToAdd <- controlPlusOne.drugToAddServer("drugToAddSelection", dataset, selectedControlTreatment)

    selectedCellLinesAndSubgroups <- controlPlusOne.cellLineServer("cellLineSelection", dataset)

    selectedCellLines <- selectedCellLinesAndSubgroups$cellLines

    selectedSubgroups <- selectedCellLinesAndSubgroups$subgroups

    checkedParameters <- controlPlusOne.parametersServer("parametersCheck", fileType)

    nSim <- checkedParameters$nSim

    efficacyMetric <- controlPlusOne.efficacyMetricServer("efficacyMetric", fileType)




    result <- eventReactive(input$button, {
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

      res_list <- IDAPredict.ControlPlusOne(
        Monotherapy_Data = dataset(),
        Cell_Line_Name_Column = "Cell_Line",
        Drug_Name_Column = "Drug",
        Drug_Concentration_Column = "Drug_Dose",
        Efficacy_Column = "Efficacy",
        LowerEfficacyIsBetterDrugEffect = checkedParameters$isLowerEfficacy(),
        Efficacy_Metric_Name = efficacyMetric(),
        Control_Treatment_Drugs = selectedControlTreatment(),
        Control_Treatment_Drug_Concentrations = selectedDose(),
        Drug_to_Add = selectedDrugToAdd(),
        Calculate_Uncertainty = checkedParameters$uncertainty(),
        Efficacy_SE_Column = eff_se_col,
        n_Simulations = nSim(),
        Calculate_IDAcomboscore_And_Hazard_Ratio = checkedParameters$comboscore(),
        Average_Duplicate_Records = checkedParameters$averageDuplicate()
      )

      cbind(
        Control_Treatment = res_list$Control_Treatment,
        Drug_to_Add = res_list$Drug_to_Add,
        Number_of_Cell_Lines_Used = length(res_list$Cell_Lines_Used),
        Cell_Lines_Used = paste(res_list$Cell_Lines_Used, collapse = ", "),
        res_list[[1]]
      )
    })

    output$table <- DT::renderDataTable(
      {
        if (!is.null(result())) {
          result()[, names(result()) != "Cell_Lines_Used"]
        }
      },
      options = list(scrollX = TRUE)
    )
    
    plot.object <- eventReactive(input$button,{
      res <- result()
      
      p1 <- qplot(res$Drug_to_Add_Dose,res$Mean_Combo_Viability)
      if(checkedParameters$uncertainty()){
        viability_CI <- rbindlist(lapply(res[["Mean_Combo_Viability_95%_Confidence_Interval"]], function(s){as.data.frame(matrix(as.double(strsplit(s,"_")[[1]]),nrow = 1))}))
        p1 <- p1 + geom_errorbar(aes(ymin=viability_CI[[1]], ymax=viability_CI[[2]]))
      }
      if(checkedParameters$comboscore()){
        p2 <- qplot(res$Drug_to_Add_Dose,res$HR_vs_Control_Treatment) 
        p3 <- qplot(res$Drug_to_Add_Dose,res$HR_vs_Drug_to_Add) 
        p4 <- qplot(res$Drug_to_Add_Dose,res$IDA_Comboscore)
        if(checkedParameters$uncertainty()){
          HR_vs_Control_Treatment_CI <- rbindlist(lapply(res[["HR_vs_Control_Treatment_95%_Confidence_Interval"]], function(s){as.data.frame(matrix(as.double(strsplit(s,"_")[[1]]),nrow = 1))}))
          HR_vs_Drug_To_Add_CI <- rbindlist(lapply(res[["HR_vs_Drug_to_Add_95%_Confidence_Interval"]], function(s){as.data.frame(matrix(as.double(strsplit(s,"_")[[1]]),nrow = 1))}))
          IDA_Comboscore_CI <- rbindlist(lapply(res[["IDA_Comboscore_95%_Confidence_Interval"]], function(s){as.data.frame(matrix(as.double(strsplit(s,"_")[[1]]),nrow = 1))}))
          p2 <- p2 + geom_errorbar(aes(ymin=HR_vs_Control_Treatment_CI[[1]], ymax=HR_vs_Control_Treatment_CI[[2]]))
          p3 <- p3 + geom_errorbar(aes(ymin=HR_vs_Drug_To_Add_CI[[1]], ymax=HR_vs_Drug_To_Add_CI[[2]]))
          p4 <- p4 + geom_errorbar(aes(ymin=IDA_Comboscore_CI[[1]], ymax=IDA_Comboscore_CI[[2]]))
        }
      }
      
      if(checkedParameters$comboscore())
        grid.arrange(grobs=list(p1,p2,p3,p4),ncol = 2, nrow = 2)
      else
        p1
    })
    
    output$plot <- renderPlot({
      plot.object()
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".txt", sep = "")
      },
      content = function(con) {
        write_delim(result(), con, delim = "\t")
      }
    )
  })
}
