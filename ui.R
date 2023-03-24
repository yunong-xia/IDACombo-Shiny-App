sideBar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", selected = TRUE),
    menuItem("Dataset Loader", tabName = "table", icon=icon("table")),
    tags$hr(),
    menuItem("2-Drug", tabName = "twoDrug",
             menuSubItem("Focused", tabName = "onlyTwo"),
             menuSubItem("Batch Processing", tabName = "twoDrugBatch")),
    menuItem("Control Plus One", tabName = "controlPlusOne",
             menuSubItem("Focused", tabName = "onlyOneControlTreatment"),
             menuSubItem("Batch Processing", tabName = "controlPlusOneBatch")),
    menuItem("Test Vs Control", tabName = "testVsControl"),
    menuItem("About This App", tabName = "about",
             menuSubItem("FAQ", tabName = "faq"),
             menuSubItem("App Info", tabName = "appVersion"),
             menuSubItem("Citing This Resource", tabName = "citation"),
             menuSubItem("Contact Us", tabName = "contact"))
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
        h3("Video Tutorials"),
        tags$a(
          href="https://youtu.be/pnOEgZJmx9k", 
          h4("How the IDACombo algorithm works")
        ),
        tags$a(
          href="https://youtu.be/R4cupNBxz-E", 
          h4("How to use this app")
        ),
        tags$a(
          href="https://youtu.be/U9JYkD-hK5E", 
          h4("How to generate custom datasets for this app using the Simplicity app")
        ),
        h3("IDACombo Paper"),
        tags$a(
          href="https://www.nature.com/articles/s41467-020-19563-6#:~:text=Computationally%20predicting%20clinical%20drug%20combination%20efficacy%20with%20cancer,...%202%20Results%20Design%20principle%20and%20workflow%20", 
          h4("Ling and Huang, 2020")
        ),
        br(),
        br(),
        tags$a(
          href="http://huang-lab.umn.edu", 
          tags$img(
            src = "image/huang_lab_icon.png",
            align = "center",
            width = "70%",
            height = "auto"
          )
        )
      ),
      wellPanel(
        h3("To cite use of this app"),
        h4(HTML("If you use this resource for your research, please cite us and the researchers who generated the data used by the app! Citation information can be found in the <b>Citing This Resource</b> sub tab under <b>About This App</b>."))
      )
    ),
    
    mainPanel(
      wellPanel(
        h2("How to use the app"),
        h4("The first step is to load a monotherapy drug screening dataset to use with IDACombo. You can do this by navigating to the \"Dataset Loader\" tab on the left and selecting one of our pre-provided datasets or by uploading your own data in the specified format."),
        br(),
        h4("After loading a dataset, you can generate drug combination predictions from it using one of several functions which are described below.")
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
    p(HTML("1.  Xia, Y., Ling, A., Huang, R.S. An IDACombo based web application for predicting cancer drug combination efficacy. <i>In Preparation.</i> (2021).")),
    p(HTML("2.  Ling, A. & Huang, R. S. Computationally predicting clinical drug combination efficacy with cancer cell line screens and independent drug action. <i>Nat. Commun.</i> <b>11</b>, 1-13 (2020).")),
    p(HTML("3.  Ling, A., Huang, R.S. Simplicity: a simple web interface to manipulate high-throughput cancer drug screening data. <i>In Preparation.</i> (2021)."))
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

appVersion <- fluidPage(
  wellPanel(
    h4("Current Version: 0.1.0"),
    h4("Last Updated: 3/21/2023"),
    h3("Source Code"),
    tags$a(
      href="https://github.com/Alexander-Ling/IDACombo/", 
      h4("IDACombo R Package")
    ),
    tags$a(
      href="https://github.com/yunong-xia/IDACombo-Shiny-App", 
      h4("IDACombo Shiny App")
    )
  )
)

contact <- fluidPage(
  wellPanel(
    h4(strong("App Author:"), " Yunong Xia (xia00045@umn.edu)"),
    h4(strong("App Co-Author:"), " Alexander Ling (Alexander.L.Ling@gmail.com"),
    h4(strong("Principal Investigator:"), " R. Stephanie Huang (rshuang@umn.edu)")
  )
)

faq <- fluidPage(
  wellPanel(
    h2("We have several publications regarding the development and use of the data, algorithms, and interface for this app. We strongly suggest you read those papers before using this app."),
    h3(HTML("<b>For information on the development and validation of the IDACombo algorithm and a thorough discussion on how to interpret IDACombo's predictions as well as the algorithm's strengths and limitations:</b>")),
    h4(HTML("Ling, A. & Huang, R. S. Computationally predicting clinical drug combination efficacy with cancer cell line screens and independent drug action. <i>Nat. Commun.</i> <b>11</b>, 1-13 (2020)")),
    h3(HTML("<b>For information on this app:</b>")),
    h4(HTML("Xia, Y., Ling, A., Huang, R.S. An IDACombo based web application for predicting cancer drug combination efficacy. <i>In Preparation.</i> (2021)."))
  ),
  wellPanel(
    h2("How were the pre-provided datasets generated?"),
    h4(HTML("The viability values for the pre-provided datasets were generated using <a href=\"https://oncotherapyinformatics.org/simplicity/\">the Simplicity web app</a>. Viabilities were estimated for all cell lines and compounds in each dataset from concentrations of 0 up to the most commonly tested maximum concentration for each compound. For compounds which had annotated Csustained values, an additional 11 concentrations from 0 to Csustained were also estimated so long as the Csustained value was not more than 2x the most commonly tested maximum concentration for that compound in that dataset. These datasets were last generated using Simplicity v1.0 on 3/21/2023."))
  ),
  wellPanel(
    h2("How does this app predict drug combination efficacy?"),
    h4(HTML("This app uses monotherapy in vitro drug screening data to predict drug combination efficacy using the IDACombo R package. IDACombo relies on the principle of Independent Drug Action, which predicts that an organism's response to a combination of drugs will be dictated by the single drug in that combination to which the organism is most sensitive. While this is an obvious simplification, IDACombo's predictions have been shown to be highly concordant with measured drug combination efficacies both in vitro and in the results of phase 3 clinical trials."))
  ),
  wellPanel(
    h2("How should I interpret the predictions produced by IDACombo?"),
    h3("IDACombo produces several metrics that can help predict the efficacy of a drug combination. They are as follows:"),
    h4(HTML("<b>Viability:</b> Viabilities reported by IDACombo are the average viability across all selected cell lines when treated with a given drug at a drug combination at the specified drug concentration(s). The original, per cell line viabilities used to calculate these averages were measured differently depending on the dataset being used to predict drug combination efficacies.")),
    h4(HTML("<b>IDAComboscore:</b> The full derivation of the IDAComboscore is included in the methods section of Ling & Huang, 2020 (equation 4). A higher IDAComboscore indicates a combination that both minimizes the hazard ratio for the combination vs. the more efficacious of the therapies being combined and which minimizes the predicted mean viability upon treatment with the combination. This score is most useful for ranking many combinations composed of non-overlapping compounds, such that there is no default control therapy against which a comparable hazard ratio could be calculated.")),
    h4(HTML("<b>Hazard Ratio (HR):</b> Hazard ratios are calculated by dividing the viability of a predicted combination therapy by the viability of the therapy to which it is being compared. For example, if therapy with drugs A produces 80% mean viability, therapy with drugs B produces 60% viability, and a predicted combination of drugs A + B produces 55% mean viability, the resulting hazard ratio vs. drug A would be 0.69 while the resulting hazard ratio vs. drug B would be 0.92. <b>MaxHR</b> is simply the greater (less efficacious) of the two hazard ratios-in this case, 0.92. A lower hazard ratio indicates a combination which is more effective at killing cells. This is the primary metric that should be used when comparing between combinations which all share the same control therapy (i.e. comparing combinations of drug A + x, where many x's are possible and A is being considered at a single concentration), as it is easier to interpret than an IDAComboscore. However, an IDAComboscore will provide the same ranking in this situation.")),
  ),
  wellPanel(
    h2("Why does the app report predictions at discrete concentrations of each drug being combined? Can't you just give me a single summary metric of how good a drug combination is without complicating things with doses?"),
    h4(HTML("The same combination of drugs can behave very differently depending on what doses the drugs are combined at. The predictions of IDACombo have been shown to be dose dependent, and agreement of IDACombo's predictions with clinical trial results are best when made using clinically relevant concentrations for each of the drugs in a given combination."))
  ),
  wellPanel(
    h2("How am I supposed to know what concentration to use for each drug when making predictions?"),
    h4(HTML("Unfortunately, it is very difficult to know what drug concentration will be physiologically relevant for a given drug, especially if that drug hasn't made it into the clinic yet. For our initial validation of IDACombo, we defined the clinically relevant concentration for a drug to be the highest plasma concentration observed in patients at least 6 hours following treatment with that drug, something we called Csustained. Csustained concentrations have been indicated as such throughout the app for all drugs that we have this information for. However, it should be noted that Csustained can vary based on the dose and administration route of a compound, and plasma concentrations may not be as relevant for some tumor types as they are for others. For example, we would not expect Csustained to be especially informative for treating brain tumors with systemic administration of a compound which does not easily cross the blood-brain barrier. If you don't know what drug concentration tumor cells are likely to experience in vivo, we suggest making predictions across a range of concentrations you think most likely to be relevant, and then seeing whether or not your prediction is likely to be useful across most or all of that dose range. If the combination is only predicted to be good at a very narrow set of concentrations, then you may want to explore other possible combinations which have a wider window of combination benefit."))
  ),
  wellPanel(
    h2("I see that IDACombo requires me to select a set of cell lines to make predictions with. Which ones should I choose?"),
    h4(HTML("IDACombo makes efficacy predictions at a population level rather than for individual cell lines or patients. As such, you should select the set of cell lines which best represents the drug response heterogeneity you expect to see in the patient population you hope to treat with your candidate drug combination(s). Our preliminary research in this area suggests that prediction accuracy drops rapidly when using <50 cell lines to make predictions with. As such, using a large, pan-cancer set of cell lines is likely to be preferable to making predictions with a small set of cell lines specific to your cancer type or interest. However, especially when considering combinations with highly targeted therapies for drug targets that are highly enriched in the patient population you wish to treat, it may be necessary to carefully select your cell line panel to mimic the distribution of relevant targetable mutations/dependencies observed in your patient population of interest."))
  ),
  wellPanel(
    h2("I found an interesting combination with IDACombo and I tried to validate it experimentally in my favorite cell line, but I didn't see any benefit with the combination vs. the monotherapies making up the combination. Is IDACombo's prediction just wrong?"),
    h4(HTML("IDACombo predicts drug combination benefit across a population of patients/cell lines, not for individual patients/cell lines. In fact, <b>IDACombo is based on the assumption that there will be no benefit to an individual patient or cell line with the combination vs whichever of the monotherapies in that combination are most effective for that patient/cell line</b>. While it is certainly possible that you will see additive or synergistic benefit from a combination in a single cell line, IDACombo has no ability to predict if this will be the case. Rather, IDACombo predicts that a population of patients will, on average, have a better response to the combination than if the population was treated with one of the monotherapies comprising that combination. As such, to validate a candidate combination from IDACombo, it is necessary to test the combination in a representative population of cell lines, animal models, or patients rather than in a single model or patient. While this may seem counter-intuitive to how you are used to thinking about drug combinations (and is certainly less convenient to validate experimentally than a synergistic combination), please note that there is little evidence that pre-clinical observations of synergy are associated with clinical success-whereas there is growing evidence that Independent Drug Action can explain a large proportion of drug combination success in the clinic."))
  ),
  wellPanel(
    h2("The 95% confidence intervals for some of my predicted IDAComboscores and/or Hazard Ratios don't include the predicted values. Is this a bug?"),
    h4(HTML("a.	As mentioned in Ling and Huang (2020, see page 11), there are many limitations to the statistical approach applied in the IDACombo package. The key one here being that confidence intervals are being determined via a semi-parametric bootstrap rather than a fully parametric bootstrap. This compromise was made to enable the IDACombo package to work with input datasets which only have known viabilities and their associated standard errors rather than only to work with datasets which have reported uncertainties with each of the parameters in the logistic regressions used to fit dose-response curves. In certain situations, this approach results in confidence intervals that narrowly do not include the estimated value of an IDAComboscore or Hazard Ratio. It should be noted, however, that there are more fundamental limitations to the uncertainties and p-values being calculated by IDACombo. As noted in Ling and Huang (2020):")),
    wellPanel(
      h4(HTML("<i>Importantly, the standard errors estimated in this analysis only account for random errors in the measured viability values provided by the cell line drug screening datasets and in random sampling from the cell line population being used to make predictions. Systematic errors, such as might be caused by imprecise drug dilution or cell line counting, variation in phenotype between different aliquots of a cell line, or different protocols for performing cell line screens, are not modeled here. Likewise, no effort is made here to estimate uncertainties arising from predictions being made with cell line populations that do not adequately represent the patient population the predictions are to be applied to, as how to quantify how representative a collection of cell-line models is for a given set of cancer patients is an open question in the field. Additionally, no efforts are made to estimate the uncertainties introduced by uncertainty in the in vitro drug doses which most closely mimic the in vivo effect of each drug on patient tumors. As such, the uncertainties estimated for values calculated by IDACombo should be considered as the lower limits of uncertainty, and should not be considered to be particularly robust for hypothesis testing.</i>"))
    )
  )
)

body <- dashboardBody(
  #This is a initialization of shinyjs. shinyjs is a package. This app makes use of its ui lock feature.
  useShinyjs(),
  
  #Enable Google analytics and adding header text for info about loaded dataset
  tags$head(includeHTML(("./www/google-analytics.html")),
            tags$style(HTML(
            '.myClass { 
            font-size: 18px;
            line-height: 50px;
            text-align: left;
            font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
            padding: 0 15px;
            overflow: hidden;
            color: white;
            }
            '))
    ),
  tags$script(HTML('
                   $(document).ready(function() {
                   $("header").find("nav").append(\'<div id="pageHeader" class="myClass"></div>\');
                   })
                   ')
    ),
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
            citationPage),
    tabItem(tabName = "appVersion",
            appVersion),
    tabItem(tabName = "contact",
            contact),
    tabItem(tabName = "faq",
            faq)
    
  )
  
)

ui <- dashboardPage(
        skin = "green",
        #Dashboard Header
        dashboardHeader(title = "IDACombo" , titleWidth = 230),
        sideBar,
        body
      )