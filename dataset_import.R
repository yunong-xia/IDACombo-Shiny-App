# Module UI function
datasetInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  box(
    width = NULL, status = "primary", solidHeader = TRUE, title = "Input",
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Preprovided Dataset",
        selectInput(
          ns("providedDataSet"), "Choose a preprovided dataset:",
          c("GDSC1", "GDSC2","CTRPv2","PRISM Repurposing")
        ),
        actionButton(ns("button"), "Load"),
        hr(),
        h5("You can download the preprovided dataset chosen in the above menu."),
        downloadButton(ns("download"), "Download Dataset")
      ),
      tabPanel(
        "Custom Dataset",
        h4("A custom dataset can be used to generate IDACombo predictions by uploading it here. The dataset must be saved in a tab separated text format (.tsv) with each row representing the measured effect of a single drug in a single cell line at a single concentration. The columns must be as follows:"),
        wellPanel(
        h5(strong("\"Cell_Line\""), ": The names of the cell lines being screened."),
        h5(strong("\"Drug\""), ": The names of the drugs being screened."),
        h5(strong("\"Drug_Dose\""), ": The drug concentrations being screened. "),
        h5(strong("\"Efficacy\""), ": Measured efficacy values for each set of cell lines, drugs, and concentrations."),
        h5(strong("\"Efficacy_SE\""), ": (optional) Standared errors of the measured efficacy values."),
        h5(strong("\"Cell_Line_Subgroup\""), ": (optional) Terms by which cell lines can be grouped together.")
        ),
        br(),
        h4(HTML("Please also specify the following <u>prior to uploading your file</u>:")),
        wellPanel(textInput(ns("Master_efficacyMetric"), "Your Efficacy Metric Name (can be empty)", NULL, width = '70%', placeholder = "(i.e. Viability, % Growth, AUC, etc.)"),
                  checkboxInput(ns("Master_isLowerEfficacy"), "Lower Efficacy Is Better Drug Effect") %>%
                    helper(type = "inline",
                           title = "Lower Efficacy Is Better Drug Effect",
                           icon = "question-circle", colour = NULL,
                           content = c(
                             "Check this box if a lower efficacy value indicates a better cell line response to the drug treatment. Leave this unchecked if a higher efficacy value indicates a better cell line response to the drug treatment."
                           ),
                           size = "m",
                           buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                    )),
        downloadButton(ns("sampleFile"), "Download Sample Input File"),
        
        fileInput(ns("dataset"), "Upload your data set",
          multiple = TRUE,
          accept = c(
            "text",
            "text/comma-separated-values,text/plain",
            ".tsv"
          )
        )
      )
    )
  )
}


# Module Server function
datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    fileInfo <- reactiveValues(dataset = NULL, extraCol = NULL, type = NULL, efficacyMetric = NULL, isLowerEfficacy = F)

    observeEvent(input$button, {
      fileInfo$dataset <- preprovided_dataset[[input$providedDataSet]]
      fileInfo$extraCol <- c("seCol", "subCol")
      fileInfo$type <- "provided"
      fileInfo$efficacyMetric <- "Viability"
      fileInfo$isLowerEfficacy <- T
    })

    observeEvent(input$dataset, {
      inFile <- input$dataset
      if (is.null(inFile)) {
        return(NULL)
      }
      show_modal_spinner(
        spin = "atom",
        color = "firebrick",
        text = "Please wait..."
      )
      content <- read.delim(input$dataset$datapath)
      remove_modal_spinner()
      headers <- names(content)
      extraCol <- NULL
      fixedHeaderNames <- c("Cell_Line", "Drug", "Drug_Dose", "Efficacy")
      if ("Efficacy_SE" %in% headers) {
        extraCol <- c(extraCol, "seCol")
      }
      if ("Cell_Line_Subgroup" %in% headers) {
        extraCol <- c(extraCol, "subCol")
      }
      missedCol <- setdiff(fixedHeaderNames, headers)
      validate(
        need(length(missedCol) == 0, paste0("Warning: Missing required column(s) : ", paste(missedCol, collapse = " ")))
      )
      
      fileInfo$dataset <- content
      fileInfo$extraCol <- extraCol
      fileInfo$type <- "custom"
      fileInfo$efficacyMetric <- input$Master_efficacyMetric
      fileInfo$isLowerEfficacy <- input$Master_isLowerEfficacy
    })

    output$download <- downloadHandler(
      filename = function() {
        paste(input$providedDataSet, "-", Sys.Date(), ".txt", sep = "")
      },
      content = function(con) {
        write_delim(preprovided_dataset[[input$providedDataSet]], con, delim = "\t")
      }
    )

    output$sampleFile <- downloadHandler(
      filename = function() {
        "Sample-Input.xlsx"
      },
      content = function(file) {
        df <- data.frame(
          Drug = paste0("drug", c(1, 2, 3)),
          Drug_Dose = runif(3, 0, 2),
          Cell_Line = paste0("cell_line", c(1, 2, 3)),
          Efficacy = runif(3, 0, 1),
          Efficacy_SE = runif(3, 0, 0.1),
          Cell_Line_Subgroup = paste0("group", c("A", "A", "B"))
        )
        write.xlsx(df, file)
      }
    )

    list(dataset = reactive(fileInfo$dataset), extraCol = reactive(fileInfo$extraCol), type = reactive(fileInfo$type),
         efficacyMetric = reactive(fileInfo$efficacyMetric),
         isLowerEfficacy = reactive(fileInfo$isLowerEfficacy))
  })
}
