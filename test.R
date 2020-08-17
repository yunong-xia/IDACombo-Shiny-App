all_cl <- unique(dataSet$Cell_Line)
drugList <- list()   # control treatment drugs list
doseList <- list()   # control treatment doses list
drug_to_add_arr <- rep("",nrow(control_plus_one_input))    # drug to add vector
cl_num <- rep(0, nrow(control_plus_one_input))    # shared cell line numbers vector
shared_cl <- list()    #   shared cell lines list

for(i in 1:nrow(control_plus_one_input)) {
  ### retrieve data from ctInput row
  ###    1. control treatment drugs
  drugList[[i]] <- strsplit(control_plus_one_input$Control_Treatment[i], ",")[[1]]
  ###    2. control treatment doses
  doseList[[i]] <- as.numeric(strsplit(control_plus_one_input$Doses[i], ",")[[1]])
  ###    3. drug to add
  drug_to_add_arr[i] <- control_plus_one_input$Drug_To_Add[i]
  ### error handling of data retrieve
  validate(
    need(length(drugList[[i]])==length(doseList[[i]]), paste0("The number of drugs in control treatment doesn't match that of doses: Input Row ", i))
  )
  
  ### update the shared cell lines iteratively and store the final result into the list
  shared_cl[[i]] <- all_cl
  for(j in 1:length(drugList[[i]])){
    shared_cl[[i]] <- intersect(shared_cl[[i]], 
                                unique(dataSet$Cell_Line[ dataSet$Drug == drugList[[i]][j] & dataSet$Drug_Dose == doseList[[i]][j] ])
    )
  }
  shared_cl[[i]] <- intersect(shared_cl[[i]],
                              unique(dataSet$Cell_Line[ dataSet$Drug == control_plus_one_input$Drug_To_Add[i] ]))
  ###   count the number of shared cell lines
  cl_num[i] <- length(shared_cl[[i]])
  ###   generate extra error message
  if(cl_num[i] < 2) {
    paste0("Row ", i, " doesn't have enough cell lines (>=2)\n") %>% cat()
  }
  else {
    paste0("Row ", i, " used ", cl_num[i], " cell lines\n") %>% cat()
  }
}


usable_index <- which(cl_num >= 2)
Runs = 4
floor(length(usable_index)/Runs)


rm(all_cl,drugList,doseList,shared_cl,drug_to_add_arr,cl_num)











getRes <- function(i) {
  # prepare mono data of control treatment and of drug to add
  control_data <- dataSet %>% filter(Drug %in% drugList[[i]],
                                     Cell_Line %in% shared_cl[[i]])
  for(j in 1:length(doseList[[i]])){
    control_data <- control_data[!(control_data$Drug == drugList[[i]][j] & control_data$Drug_Dose != doseList[[i]][j] ),]
    
  }
  drug_to_add_data <- dataSet %>% filter(Drug == drug_to_add_arr[i],
                                         Cell_Line %in% shared_cl[[i]])
  monoData <- rbindlist(list(control_data, drug_to_add_data))
  
  IDAPredict.ControlPlusOne(
    Monotherapy_Data = monoData,
    Cell_Line_Name_Column = "Cell_Line",
    Drug_Name_Column = "Drug",
    Drug_Concentration_Column = "Drug_Dose",
    Efficacy_Column = "Efficacy",
    LowerEfficacyIsBetterDrugEffect = T,
    EfficacyMetricName = "Viability",
    Control_Treatment_Drugs = drugList[[i]],
    Control_Treatment_Drug_Concentrations = doseList[[i]],
    Drug_to_Add = drug_to_add_arr[i],
    Calculate_Uncertainty = F,
    Efficacy_SE_Column = "Efficacy_SE",
    n_Simulations = 1000,
    CalculateIDAComboscoreAndHazardRatios = F,
    Average_Duplicate_Records = F
  )
}