#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  fileInfo <- datasetServer("datafile")

  output$tablelook <- renderDataTable(fileInfo$dataset())

  observe_helpers() #This is an observer to observe the helper. see documentation of library(shinyhelper)

  twoDrugs.server("twoDrugs", fileInfo)

  twoDrugs.batch.server("twoDrugsBatch", fileInfo)

  controlPlusOne.server("controlPlusOne", fileInfo)

  controlPlusOne.batch.server("controlPlusOneBatch", fileInfo)

  testVsControl.server("testVsControl", fileInfo)

  testVsControl.batch.server("testVsControlBatch", fileInfo)
})
