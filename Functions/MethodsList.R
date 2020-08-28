MethodsList <- function(data) {
  ## Calculate necessary variables and store in list:
  
  # # Summary of CTI
  # meanCTI = mean(data$CTI, na.rm = T)
  # sdCTI = sd(data$CTI, na.rm = T)
  # # Summary of ITI
  # meanITI = mean(data$ITI, na.rm = T)
  # sdITI = sd(data$ITI, na.rm = T)
  #
  # # Percentage of Errors, Split by condition
  # Errors_p = unlist(lapply(Subjidx_p, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Error" &
  #                    data$Condition == "p",]) / nrow(data[data$PartID == x &
  #                                                                     data$Condition == "p",])))
  # Errors_np = unlist(lapply(Subjidx, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Error" &
  #                    data$Condition == "np",]) / nrow(data[data$PartID == x &
  #                                                                      data$Condition == "np",])))
  # # Percentage of Misses, Split by condition
  # Miss_p = unlist(lapply(Subjidx_p, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Miss" &
  #                    data$Condition == "p",]) / nrow(data[data$PartID == x &
  #                                                                     data$Condition == "p",])))
  # Miss_np = unlist(lapply(Subjidx, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Miss" &
  #                    data$Condition == "np",]) / nrow(data[data$PartID == x &
  #                                                                      data$Condition == "np",])))
  # # Errors by Visual Cue Type, Split by Condition
  # Errors_p_Vis <- unlist(lapply(Subjidx_p, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Error" &
  #                    data$CueType == "Visual" &
  #                    data$Condition == "p",]) / nrow(data[data$PartID == x &
  #                                                                     data$CueType == "Visual" &
  #                                                                     data$Condition == "p",])))
  # Errors_np_Vis <- unlist(lapply(Subjidx, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Error" &
  #                    data$CueType == "Visual" &
  #                    data$Condition == "np",]) / nrow(data[data$PartID == x &
  #                                                                      data$CueType == "Visual" &
  #                                                                      data$Condition == "np",])))
  #
  # # Errors by Auditory Cue Type, Split by Condition
  # Errors_p_Aud <- unlist(lapply(Subjidx_p, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Error" &
  #                    data$CueType == "Auditory" &
  #                    data$Condition == "p",]) / nrow(data[data$PartID == x &
  #                                                                     data$CueType == "Auditory" &
  #                                                                     data$Condition == "p",])))
  # Errors_np_Aud <- unlist(lapply(Subjidx, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Error" &
  #                    data$CueType == "Auditory" &
  #                    data$Condition == "np",]) / nrow(data[data$PartID == x &
  #                                                                      data$CueType == "Auditory" &
  #                                                                      data$Condition == "np",])))
  # # Errors by Ambiguous Cue Type, Split by Condition
  # Errors_p_Amb <- unlist(lapply(Subjidx_p, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Error" &
  #                    data$CueType == "Ambiguous" &
  #                    data$Condition == "p",]) / nrow(data[data$PartID == x &
  #                                                                     data$CueType == "Ambiguous" &
  #                                                                     data$Condition == "p",])))
  # Errors_np_Amb <- unlist(lapply(Subjidx, function(x)
  #   nrow(data[data$PartID == x &
  #                    data$Accuracy == "Error" &
  #                    data$CueType == "Ambiguous" &
  #                    data$Condition == "np",]) / nrow(data[data$PartID == x &
  #                                                                      data$CueType == "Ambiguous" &
  #                                                                      data$Condition == "np",])))
  Summdata <- list()
  # # No. Participants
  # Summdata[["Demo"]][["N"]] <- length(unique(data$PartID))
  # # No. Participants, split by condition
  # Summdata[["Demo"]][["N_p"]] <-
  #   length(unique(data$PartID[data$Condition ==
  #                               "p"]))
  # Summdata[["Demo"]][["N_np"]] <-
  #   length(unique(data$PartID[data$Condition ==
  #                               "np"]))
  # # Number of Trials
  # Summdata[["Demo"]][["Trials"]] <- data %>%
  #   group_by(PartID) %>%
  #   tally() %>%
  #   .$n
  # # Number of Trials, split by condition
  # Summdata[["Demo"]][["Trials_p"]] <- data %>%
  #   dplyr::filter(Condition == "p")
  # group_by(PartID) %>%
  #   tally() %>%
  #   .$n
  # Summdata[["Demo"]][["Trials_np"]] <- data %>%
  #   dplyr::filter(Condition == "np")
  # group_by(PartID) %>%
  #   tally() %>%
  #   .$n
  # Means and variances of continuous variables
  Summdata <- data %>%
    group_by(PartID, Condition, CueType, Target, Accuracy, Distractor) %>%
    dplyr::summarise(
      meanRT = mean(RT, na.rm = T),
      sdRT   = sd(RT, na.rm = T),
      SERT   = stderr(RT, na.rm = T),
      N_trials = n(),
      # More in-depth count of trials.
      meanCTI = mean(CTI, na.rm = T),
      sdCTI = sd(CTI, na.rm = T),
      minCTI = min(CTI,na.rm = T),
      maxCTI = max(CTI,na.rm =T),
      meanITI = mean(ITI, na.rm = T),
      sdITI = sd(ITI, na.rm = T),
      minITI = min(ITI,na.rm = T),
      maxITI = max(ITI,na.rm =T)
    )
  
  # Rates of each trial type
  # Create dummy variable of total N_trials per condtion, per participant
  Summdata$Total_Trials <- NA # Create variable
  for (i in unique(Summdata$PartID)) {
    # Condition p
    Summdata$Total_Trials[Summdata$PartID == i &
                                       Summdata$Condition == "p"] <-
      sum(Summdata$N_trials[Summdata$PartID == i &
                                    Summdata$Condition ==
                                    "p"])
    # Condition np
    Summdata$Total_Trials[Summdata$PartID == i &
                                       Summdata$Condition == "np"] <-
      sum(Summdata$N_trials[Summdata$PartID == i &
                                    Summdata$Condition ==
                                    "np"])
  }
  # Create the percentage of each trial type.
  Summdata$PC_Trials <-
    (Summdata$N_trials / Summdata$Total_Trials) * 100
  
  
  return(Summdata)
}
