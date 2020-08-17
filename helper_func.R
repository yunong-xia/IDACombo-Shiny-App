getMonoDataList <- function(data) {
  drugList <- unique(data$Drug)
  monoDataList <- lapply(drugList,function(x){data[data$Drug == x,]})
  names(monoDataList) <- drugList
  return(monoDataList)
}


