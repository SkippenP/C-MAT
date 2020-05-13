RTSum <- function(list, sublist, sublist_levels,subj){
  RT   <- list()
  
  for (i in seq_along(subj)) {
    ## Create a dummy variable to store participant level data
    subjdummy <- list[[sublist]][list[[sublist]]$PartID == subj[i], ]
    ## Loop through participants and apply a percentage to each level of the list
    OmmRates[[i]] <-
      lapply(sublist_levels, function(x)
        (subjdummy$n[subjdummy[[sublist]] == x &
                       subjdummy$Accuracy == "Miss"] / sum(subjdummy$n[subjdummy[[sublist]] == x])) * 100)
    names(OmmRates[[i]]) <- sublist_levels
    
  names(OmmRates) <- subj
  }
  ## Transform into data frames
  
  ErrorRates.data <- do.call(rbind,ErrorRates)
  
  return(list(Errors = ErrorRates.data,
              Ommissions = OmmRates.data))
}