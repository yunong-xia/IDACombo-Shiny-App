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
            ".csv"
          )
        )
      ),
      tabPanel(
        "Preprovided Dataset",
        selectInput(
          ns("providedDataSet"), "Choose a preprovided dataset:",
          c("GDSC", "CTRPv2")
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
        text = "Please wait..."
      )
      if (input$providedDataSet == "GDSC") {
        # should be modified if the location of the file changes.
        GDSC_Data_path <- paste0(getwd(), "/provided_dataset/GDSC_Data.rds")
        fileInfo$dataset <- readRDS(GDSC_Data_path)
        fileInfo$extraCol <- c("seCol", "subCol")
        fileInfo$type <- "GDSC"
      }
      if (input$providedDataSet == "CTRPv2") {
        # should be modified if the location of the file changes.
        CTRPv2_Data_path <- paste0(getwd(), "/provided_dataset/CTRPv2_Data.rds")
        fileInfo$dataset <- readRDS(CTRPv2_Data_path)
        fileInfo$extraCol <- c("seCol", "subCol")
        fileInfo$type <- "CTRPv2"
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
