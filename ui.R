sideBar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", selected = TRUE),
    menuItem("Dataset Loader", tabName = "table", icon = icon("table")),
    hr(),
    menuItem("2-Drug",
      tabName = "twoDrug",
      menuSubItem("Prediction of Two", tabName = "onlyTwo"),
      menuSubItem("Batch Processing", tabName = "twoDrugBatch")
    ),
    menuItem("Control Plus One",
      tabName = "controlPlusOne",
      menuSubItem("One Control Treatment", tabName = "onlyOneControlTreatment"),
      menuSubItem("Batch Processing", tabName = "controlPlusOneBatch")
    ),
    menuItem("Test Vs Control",
      tabName = "testVsControl",
      menuSubItem("Test vs Control", tabName = "onlyOnePair"),
      menuSubItem("Batch Processing", tabName = "testVsControlBatch")
    )
  )
)



introductionPage <- fluidPage(
  titlePanel("IDACombo Introduction"),
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      h3("IDACombo Package"),
      h4("IDACombo pacakage is a drug combination prediction tool implemented in R. 
               The core idea behind the package is Independent Drug Action. Before you use
               this web application, we recommend you to get familiar with what IDACombo do."),
      br(),
      br(),
      tags$a(
        href = "http://huang-lab.umn.edu",
        tags$img(
          src = "huang_lab_icon.png",
          align = "center",
          width = "100%",
          height = "auto"
        )
      )
    ),

    mainPanel(
      wellPanel(
        h2("How to use the app"),
        h4("The first step is to load a dataset. We have already prepare some datasets for you to load. You can go to page Dataset Loader
                   to check it out. You can also provide your own dataset.(Warning: Not allow to combine datasets)"),
        h4("Your dataset will be shown on the bottom of that page."),
        br(),
        h4("The second step is to make use of the functionalities we provide for you."),
      ),
      wellPanel(
        br(),
        h3("Functionality Illustration:"),
        h4("2-Drug:"),
        h4("2-Durg creates efficacy predictions for 2-drug combinations using monotherapy efficacy data and the assumptions of independent drug action. When data is available for multiple concentrations of each drug, efficacy predictions are made for all possible concentration combinations."),
        br(),
        h4("Control Plus One:"),
        h4("Control Plus One creates efficacy predictions for combinations of a control therapy + 1 additional drug using monotherapy efficacy data and the assumptions of independent drug action. When data is available for multiple concentrations of the drug to add, efficacy predictions are made for all possible concentration combinations."),
        br(),
        h4("Test Vs Control"),
        h4("Test Vs Control creates efficacy predictions for a pair of control and test treatments, each treatment consisting of a combination of one or more drugs, using monotherapy efficacy data and the assumptions of independent drug action. Concentrations must be specified for each drug in each treatment.")
      )
    )
  )
)


inputPage <- fluidPage(
  fluidRow(
    datasetInput("datafile")
  )
)


twoDrugsPage <- fluidPage(
  fluidRow(
    twoDrugs.ui("twoDrugs")
  )
)

batchTwoDrugsPage <- fluidPage(
  fluidRow(
    twoDrugs.batch.ui("twoDrugsBatch")
  )
)

controlPlusOnePage <- fluidPage(
  controlPlusOne.ui("controlPlusOne")
)

batchControlPlusOnePage <- fluidPage(
  fluidRow(
    controlPlusOne.batch.ui("controlPlusOneBatch")
  )
)

testVsControlPage <- fluidPage(
  fluidRow(
    testVsControl.ui("testVsControl")
  )
)

batchTestVsControlPage <- fluidPage(
  fluidRow(
    testVsControl.batch.ui("testVsControlBatch")
  )
)


body <- dashboardBody(
  useShinyjs(),
  # page for each tab
  tabItems(
    tabItem(
      tabName = "introduction",
      introductionPage
    ),
    # Input Tab
    tabItem(
      tabName = "table",
      inputPage
    ),

    # 2 Drug Tab
    tabItem(
      tabName = "onlyTwo",
      twoDrugsPage
    ),
    tabItem(
      tabName = "twoDrugBatch",
      batchTwoDrugsPage
    ),

    # Control Plus One
    tabItem(
      tabName = "onlyOneControlTreatment",
      controlPlusOnePage
    ),
    tabItem(
      tabName = "controlPlusOneBatch",
      batchControlPlusOnePage
    ),

    # Test Vs Control
    tabItem(
      tabName = "onlyOnePair",
      testVsControlPage
    ),
    tabItem(
      tabName = "testVsControlBatch",
      batchTestVsControlPage
    )
  )
)

ui <- dashboardPage(
  skin = "green",
  # Dashboard Header
  dashboardHeader(title = "IDACombo", titleWidth = 230),
  sideBar,
  body
)
