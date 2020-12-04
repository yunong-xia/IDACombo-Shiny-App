# Module UI function
datasetInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  box(
    width = NULL, status = "primary", solidHeader = TRUE, title = "Input",
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Custom Dataset",
        h5("Before uploading your dataset, please modify your header names to (No need to be in order shown below)"),
        h6("Cell_Line : Cancer cell lines"),
        h6("Drug : Drug names"),
        h6("Drug_Dose : drug doses"),
        h6("Efficacy"),
        h6("Efficacy_SE : Efficacy standard errors (if contain)"),
        h6("Cell_Line_Subgroup : subgroups of cancer cell lines (if contain)"),

        downloadButton(ns("sampleFile"), "Download Sample Input File"),



        fileInput(ns("dataset"), "Upload your data set",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".tsv"
          )
        )
      ),
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
      )
    )
  )
}


# Module Server function
datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    fileInfo <- reactiveValues(dataset = NULL, extraCol = NULL, type = NULL)

    observeEvent(input$button, {
      show_modal_spinner(
        spin = "self-building-square",
        color = "firebrick",
        text = "Loading Dataset..."
      )
      #first, clean the memory
      fileInfo$dataset <- NULL
      if (input$providedDataSet == "GDSC1") {
        # The path of the data should be modified if the location of the file changes.
        data_path <- paste0(getwd(), "/www/provided_dataset/GDSC1_Calculated_Viabilities_for_IDACombo_shiny.rds")
        fileInfo$dataset <- readRDS(data_path)
        fileInfo$extraCol <- c("seCol", "subCol")
        fileInfo$type <- "provided"
      }
      if (input$providedDataSet == "GDSC2") {
        # The path of the data should be modified if the location of the file changes.
        data_path <- paste0(getwd(), "/www/provided_dataset/GDSC1_Calculated_Viabilities_for_IDACombo_shiny.rds")
        fileInfo$dataset <- readRDS(data_path)
        fileInfo$extraCol <- c("seCol", "subCol")
        fileInfo$type <- "provided"
      }
      if (input$providedDataSet == "CTRPv2") {
        # The path of the data should be modified if the location of the file changes.
        data_path <- paste0(getwd(), "/www/provided_dataset/CTRPv2_Calculated_Viabilities_for_IDACombo_shiny.rds")
        fileInfo$dataset <- readRDS(data_path)
        fileInfo$extraCol <- c("seCol", "subCol")
        fileInfo$type <- "provided"
      }
      if (input$providedDataSet == "PRISM Repurposing") {
        # The path of the data should be modified if the location of the file changes.
        data_path <- paste0(getwd(), "/www/provided_dataset/PRISM_Repurposing_Calculated_Viabilities_for_IDACombo_shiny.rds")
        fileInfo$dataset <- readRDS(data_path)
        fileInfo$extraCol <- c("seCol", "subCol")
        fileInfo$type <- "provided"
      }
      remove_modal_spinner()
    })

    observeEvent(input$dataset, {
      inFile <- input$dataset
      if (is.null(inFile)) {
        return(NULL)
      }
      show_modal_spinner(
        spin = "self-building-square",
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
    })

    output$download <- downloadHandler(
      filename = function() {
        paste(input$providedDataSet, "-", Sys.Date(), ".txt", sep = "")
      },
      content = function(con) {
        data <- readRDS(paste0(getwd(), "/provided_dataset/", input$providedDataSet, "_Data.rds"))
        write_delim(data, con, delim = "\t")
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

    list(dataset = reactive(fileInfo$dataset), extraCol = reactive(fileInfo$extraCol), type = reactive(fileInfo$type))
  })
}
