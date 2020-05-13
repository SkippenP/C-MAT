TableFormat <- function(data,VOI,columns){
  Table <- data.frame(matrix(nrow = length(VOI), ncol = length(columns)))
  names(Table) <- columns
  Sum <- as.data.frame(summ[[measure]]) 
  Sum$name <- rownames(Sum)
  Table$Site <- paste(comp,gsub("StopSuccess_","",Sum %>% dplyr::filter(stringr::str_detect(name,pattern="StopSuccess")) %>% dplyr::filter(stringr::str_detect(name,pattern = sites)) %>% .$name),sep="_")
  Table$Measure <- measure
  # Table$t <- as.data.frame(t.tests[[measure]]) %>% filter(Variable=="statistic_t",site %in% rownames(Table)) %>% .$value
  # Table$pFDR <- as.data.frame(t.tests[[measure]]) %>% filter(Variable=="FDR_p_value",site %in% rownames(Table)) %>% .$value
  # # Table$df <- as.data.frame(t.tests[[measure]]) %>% filter(Variable=="parameter_df",site %in% rownames(Table)) %>% .$value
  for (i in 1:nrow(Table)){
    Table$BF[[i]] <- BayesFactor::extractBF(BF.tests[[measure]][[gsub(paste(comp,"_",sep=""),"",Table$Site[[i]])]],logbf = F,onlybf = T)
    
  }
  
  for ( i in 1:nrow(Table)){
    Table$Delta[[i]] <- delta.list[[measure]][[gsub(paste(comp,"_",sep=""),"",Table$Site[[i]])]]
    
    
  }
  
  
  Table$SS <- round(Sum %>% filter(stringr::str_detect(name,pattern="StopSuccess")) %>% filter(stringr::str_detect(name,pattern = sites)) %>% .$mean,2)
  Table$SF <- round(Sum %>% filter(stringr::str_detect(name,pattern="StopFailure")) %>% filter(stringr::str_detect(name,pattern = sites)) %>% .$mean,2)
  
  Table$SS_sd <- round(Sum %>% filter(stringr::str_detect(name,pattern="StopSuccess")) %>% filter(stringr::str_detect(name,pattern = sites)) %>% .$std.dev,2)
  Table$SF_sd <- round(Sum %>% filter(stringr::str_detect(name,pattern="StopFailure")) %>% filter(stringr::str_detect(name,pattern = sites)) %>% .$std.dev,2)
  
  Table$SS_Formatted <- paste(Table$SS," (",Table$SS_sd,")",sep="")
  Table$SF_Formatted <- paste(Table$SF," (",Table$SF_sd,")",sep="")
  
  
  
  
  
}