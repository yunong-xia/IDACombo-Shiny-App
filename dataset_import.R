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
        tags$hr(),
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
        tags$br(),
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
    
    #loading preprovided dataset
    observeEvent(input$button, {
      show_modal_spinner(
        spin = "atom",
        color = "firebrick",
        text = "Loading dataset..."
      )
      Sys.sleep(1.2)
      fileInfo$dataset <- preprovided_dataset[[input$providedDataSet]]
      fileInfo$extraCol <- c("seCol", "subCol")
      fileInfo$type <- "provided"
      fileInfo$efficacyMetric <- "Viability"
      fileInfo$isLowerEfficacy <- T
      fileInfo$cellLinesAndSubgroups <- distinct(preprovided_dataset[[input$providedDataSet]][, c("Cell_Line","Cell_Line_Subgroup")])
      remove_modal_spinner()
    })
    
    #loading custom dataset
    observeEvent(input$dataset, {
      inFile <- input$dataset
      if (is.null(inFile)) {
        return(NULL)
      }
      show_modal_spinner(
        spin = "atom",
        color = "firebrick",
        text = "Loading dataset..."
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
      if ("Cell_Line_Subgroup" %in% headers) {
        fileInfo$cellLinesAndSubgroups <- distinct(preprovided_dataset[[input$providedDataSet]][, c("Cell_Line","Cell_Line_Subgroup")])
      }
      else{
        fileInfo$cellLinesAndSubgroups <- NULL
      }
    })

    output$download <- downloadHandler(
      filename = function() {
        paste(input$providedDataSet, "-", Sys.Date(), ".txt", sep = "")
      },
      content = function(con) {
        show_modal_spinner(
          spin = "atom",
          color = "firebrick",
          text = "Preparing dataset for download..."
        )
        write_delim(preprovided_dataset[[input$providedDataSet]], con, delim = "\t")
        remove_modal_spinner()
      }
    )

    output$sampleFile <- downloadHandler(
      filename = function() {
        "Sample-Input.tsv"
      },
      content = function(file) {
        df <- data.frame(
          Drug = paste0("drug", c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2)),
          Drug_Dose = c(0,1,10,0,1,10,0,1,10,0,10,100,0,10,100,0,10,90),
          Cell_Line = rep(paste0("cell_line", c(1,1,1,2,2,2,3,3,3)), 2),
          Efficacy = c(1,0.9,0.2,1,0.2,0.15,1.01,0.98,0.99,1.2,0.8,0.9,1,0.9,0.8,1,1,1),
          Efficacy_SE = runif(18, 0, 0.1),
          Cell_Line_Subgroup = rep(paste0("group", c("A", "A", "A", "A", "A", "A", "B", "B", "B")), 2)
        )
        write.table(df, file, sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
      }
    )

    list(dataset = reactive(fileInfo$dataset), extraCol = reactive(fileInfo$extraCol), type = reactive(fileInfo$type),
         cellLinesAndSubgroups = reactive(fileInfo$cellLinesAndSubgroups),
         efficacyMetric = reactive(fileInfo$efficacyMetric),
         isLowerEfficacy = reactive(fileInfo$isLowerEfficacy))
  })
}
