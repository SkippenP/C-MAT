ErrRates <- function(list, sublist, sublist_levels,subj) {
  
  ErrorRates <- list()
  OmmRates   <- list()
  
  for (i in seq_along(subj)) {
    ## Create a dummy variable to store participant level data
    subjdummy <- list[[sublist]][list[[sublist]]$PartID == subj[i], ]
    ## Loop through participants and apply a percentage to each level of the list
    OmmRates[[i]] <-
      lapply(sublist_levels, function(x)
        (subjdummy$n[subjdummy[[sublist]] == x &
                       subjdummy$Accuracy == "Miss"] / sum(subjdummy$n[subjdummy[[sublist]] == x])) * 100)
    names(OmmRates[[i]]) <- sublist_levels
    OmmRates[[i]] <- ifelse(length(unlist(OmmRates[[i]]))==0,NA,OmmRates[[i]])
  
    ## Do the same for errors
    ErrorRates[[i]] <-
      lapply(sublist_levels, function(x)
        (subjdummy$n[subjdummy[[sublist]] == x &
                       subjdummy$Accuracy == "Error"] / sum(subjdummy$n[subjdummy[[sublist]] == x])) * 100)
    names(ErrorRates[[i]]) <- sublist_levels
    ErrorRates[[i]] <- ifelse(length(unlist(ErrorRates[[i]]))==0,NA,ErrorRates[[i]])
    
    
  }
  names(OmmRates) <- subj
  names(ErrorRates) <- subj
  ## Transform into data frames
  
  OmmRates.data <- data.frame(do.call(rbind,OmmRates))
  ErrorRates.data <- data.frame(do.call(rbind,ErrorRates))
  
  return(list(Errors = ErrorRates.data,
              Ommissions = OmmRates.data))
}