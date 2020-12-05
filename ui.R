sideBar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", selected = TRUE),
    menuItem("Dataset Loader", tabName = "table", icon=icon("table")),
    hr(),
    menuItem("2-Drug", tabName = "twoDrug",
             menuSubItem("Focused", tabName = "onlyTwo"),
             menuSubItem("Batch Processing", tabName = "twoDrugBatch")),
    menuItem("Control Plus One", tabName = "controlPlusOne",
             menuSubItem("Focused", tabName = "onlyOneControlTreatment"),
             menuSubItem("Batch Processing", tabName = "controlPlusOneBatch")),
    menuItem("Test Vs Control", tabName = "testVsControl"),
    menuItem("About This App", tabName = "about",
             menuSubItem("Source Citation", tabName = "citation"))
  )
)



introductionPage <- fluidPage(
  titlePanel("IDACombo Introduction"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      wellPanel(
        h3("IDACombo Package"),
        h4("This app provides a graphical user interface for the IDACombo package, which is a drug combination prediction tool implemented in R. The core idea behind the package is that drug combination efficacy can be predicted using monotherapy cell line data and Independent Drug Action."),
        h3("Source Code"),
        tags$a(
          href="https://github.com/Alexander-Ling/IDACombo/", 
          h4("IDACombo R Package")
        ),
        tags$a(
          href="https://github.com/yunong-xia/IDACombo-Shiny-App", 
          h4("IDACombo Shiny App")
        ),
        br(),
        br(),
        tags$a(
          href="http://huang-lab.umn.edu", 
          tags$img(
            src = "image/huang_lab_icon.png",
            align = "center",
            width = "100%",
            height = "auto"
          )
        )
      ),
      wellPanel(
        h3("To cite use of this app"),
        h4(HTML("If you use this resource for your research, please cite us and the researchers who generated the data used by the app! Citation information can be found in the <b>Source Citation</b> sub tab under <b>About This App</b>."))
      )
    ),
    
    mainPanel(
      wellPanel(
        h2("How to use the app"),
        h4("The first step is to load a monotherapy drug screening dataset to use with IDACombo. You can do this by navigating to the \"Dataset Loader\" tab on the left and selecting one of our pre-provided datasets or by uploading your own data in the specified format."),
        br(),
        h4("After loading a dataset, you can generate drug combination predictions from it using one of several functions which are described below."),
      ),
      wellPanel(
        h2("Function Descriptions:"),
        br(),
        h4(HTML("<b>2-Drug:</b>")),
        h4("The \"2-Drug\" tab is used to generate efficacy predictions for 2-drug combinations. When multiple concentrations are selected for each drug, efficacy predictions are made for all possible concentration combinations. The \"Focused\" subtab allows you to make predictions for a single 2-drug combination at user-specified drug concentrations, including 3-d visualizations of the predictions. The \"Batch Processing\" subtab allows you to make predictions for many 2-drug combinations using all available drug concentrations."),
        br(),
        h4(HTML("<b>Control Plus One:</b>")),
        h4("The \"Control Plus One\" tab is used to generate and compare efficay predictions for combinations of a control therapy vs the control therapy + 1 additional drug. When data is available for multiple concentrations of the drug to add, efficacy predictions are made for all possible concentration combinations. The \"Focused\" subtab allows you to make and visualize predictions for adding a single drug to a control treatment of interest. The \"Batch Processing\" subtab allows you to make predictions for adding multiple drugs (one at a time) to the control therapy."),
        br(),
        h4(HTML("<b>Test Vs Control:</b>")),
        h4("The \"Test Vs Control\" tab is used to generate and compare efficacy predictions for a pair of user-specified control and test treatments. Concentrations must be specified for each drug in each treatment. It allows you to easily make predictions for a single control/experimental treatment pair.")
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

# batchTestVsControlPage <- fluidPage(
#   fluidRow(
#     testVsControl.batch.ui("testVsControlBatch")
#   )
# )

citationPage <- fluidPage(
  h3("If you use this resource for your research, please cite us, along with the original creators of any datasets you use from Simplicity."),
  wellPanel(
    p(HTML("<b>Cite Us:</b>")),
    p(HTML("1.  Xia, Y., Ling, A., Huang, R.S. An IDACombo based web application for predicting cancer drug combination efficacy. <i>In Preparation.</i> (2020).")),
    p(HTML("2.  Ling, A. & Huang, R. S. Computationally predicting clinical drug combination efficacy with cancer cell line screens and independent drug action. <i>Nat. Commun.</i> <b>11</b>, 1–13 (2020).")),
    p(HTML("3.  Ling, A., Huang, R.S. Simplicity: a simple web interface to manipulate high-throughput cancer drug screening data. <i>In Preparation.</i> (2020)."))
  ),
  wellPanel(
    p(HTML("<b>CTRPv2:</b>")),
    p(HTML("1.	Basu, A. et al. An Interactive Resource to Identify Cancer Genetic and Lineage Dependencies Targeted by Small Molecules. <i>Cell</i> <b>154</b>, 1151-1161 (2013).")),
    p(HTML("2.	Seashore-Ludlow, B. et al. Harnessing Connectivity in a Large-Scale Small-Molecule Sensitivity Dataset. <i>Cancer Discov.</i> <b>5</b>, 1210-1223 (2015).")),
    p(HTML("3.	Rees, M. G. et al. Correlating chemical sensitivity and basal gene expression reveals mechanism of action. <i>Nat. Chem. Biol.</i> <b>12</b>, 109-116 (2016).")),
    p(HTML("Visit their website at <a href=\"https://portals.broadinstitute.org/ctrp/\">https://portals.broadinstitute.org/ctrp/</a>."))
  ),
  wellPanel(
    p(HTML("<b>GDSC1 and GDSC2:</b>")),
    p(HTML("1.	Iorio, F. et al. A Landscape of Pharmacogenomic Interactions in Cancer. <i>Cell</i> <b>166</b>, 740-754 (2016).")),
    p(HTML("2.	Yang, W. et al. Genomics of Drug Sensitivity in Cancer (GDSC): a resource for therapeutic biomarker discovery in cancer cells. <i>Nucleic Acids Res.</i> <b>41</b>, D955-D961 (2013).")),
    p(HTML("3.	Garnett, M. J. et al. Systematic identification of genomic markers of drug sensitivity in cancer cells. <i>Nature</i> <b>483</b>, 570-575 (2012).")),
    p(HTML("Visit their website at <a href=\"https://www.cancerrxgene.org/\">https://www.cancerrxgene.org/</a>."))
  ),
  wellPanel(
    p(HTML("<b>PRISM-Repurposing:</b>")),
    p(HTML("1.	Corsello, S. M. et al. Discovering the anti-cancer potential of non-oncology drugs by systematic viability profiling. <i>Nat. Cancer</i> <b>1</b>, 235-248 (2020).")),
    p(HTML("Visit their website at <a href=\"https://depmap.org/repurposing/\">https://depmap.org/repurposing/</a>."))
  )
)

body <- dashboardBody(
  useShinyjs(),
  #page for each tab
  tabItems(
    tabItem(tabName = "introduction",
            introductionPage),
    #Input Tab
    tabItem(tabName = "table",
            inputPage),
    
    #2 Drug Tab
    tabItem(tabName = "onlyTwo",
            twoDrugsPage),
    tabItem(tabName = "twoDrugBatch",
            batchTwoDrugsPage),
    
    #Control Plus One
    tabItem(tabName = "onlyOneControlTreatment",
            controlPlusOnePage),
    tabItem(tabName = "controlPlusOneBatch",
            batchControlPlusOnePage),
    
    #Test Vs Control
    tabItem(tabName = "testVsControl",
            testVsControlPage),
    # tabItem(tabName = "testVsControlBatch",
    #         batchTestVsControlPage),
    # 
    #About This App
    tabItem(tabName = "citation",
            citationPage)
    
  )
  
)

ui <- dashboardPage(
  skin = "green",
  #Dashboard Header
  dashboardHeader(title = "IDACombo" , titleWidth = 230),
  sideBar,
  body
)
