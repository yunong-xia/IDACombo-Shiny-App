#Rendering Action button to do viability calculations
#Creating reactiveTimer to check whether or not this UI should exist once per 10 seconds
RAM_timer <- reactiveTimer(10000)

#Checking RAM usage
RAM_Free_Ratio <- eventReactive(RAM_timer(), {
  warning("Checking Ram")
  gc()
  ram <- memuse::Sys.meminfo()
  ram$freeram/ram$totalram
})

#Rendering UI
output$Viability_Calc_Action_Button_UI <- renderUI({
  warning("Rendering UI")
  if(RAM_Free_Ratio() < 0.3){
    wellPanel(
      p(HTML("<b>Due to high server usage, no further viability calculations can be initiated at this time. Please try again later. We apologize for the inconvenience.</b>"), style = "color:red") 
    )
  } else {
    actionButton(inputId = "Viability_Calc", label = "Calculate_Viability_Values")
  }
})