# Create dataframes of within subject error
wse <- list()
#### Modality ####
## Create summary using within subject data for error-bars
wse[["TargetAccuracy"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, Target, Accuracy) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Target","Accuracy"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["TargetCondition"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>% 
    dplyr::group_by(PartID, Target, Condition) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Target","Condition"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["TargetPreparation"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>% 
    dplyr::group_by(PartID, Target, CueType) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Target","CueType"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["TargetDistractor"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>% 
    dplyr::group_by(PartID, Target, Distractor) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Target","Distractor"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

#### Preparation ####
wse[["CueTypeAccuracy"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, CueType, Accuracy) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("CueType","Accuracy"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["CueTypeCondition"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>% 
    dplyr::group_by(PartID, CueType, Condition) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("CueType","Condition"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["CueTypeDistractor"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>% 
    dplyr::group_by(PartID, CueType, Distractor) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("CueType","Distractor"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["CueTypeModality"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>% 
    dplyr::group_by(PartID, CueType, Target) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("CueType","Target"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

#### Distraction ####
wse[["DistractorAccuracy"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, Distractor, Accuracy, DistractorPos) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Distractor","Accuracy"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["DistractorCondition"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, Distractor, Condition, DistractorPos) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Distractor","Condition"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["DistractorModality"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, Distractor, Target, DistractorPos) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Distractor","Target"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["DistractorPreparation"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, Distractor, CueType, DistractorPos) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Distractor","CueType"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

#### Condition ####
wse[["ConditionAccuracy"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, Condition, Accuracy) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Condition","Accuracy"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["ConditionTarget"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, Condition, Target) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Condition","Target"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["ConditionCueType"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, Condition, CueType) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Condition","CueType"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

wse[["ConditionDistraction"]] <- Rmisc::summarySEwithin(
  Trialdata %>%
    dplyr::filter(Accuracy!="Miss") %>%
    dplyr::group_by(PartID, Condition, Distractor) %>% 
    dplyr::summarise(RT = mean(RT,na.rm=T)),
  measurevar = "RT",
  withinvars = c("Condition","Distractor"),
  idvar = "PartID",
  na.rm = FALSE,
  conf.interval = .95
)

#### Plots ####

# Make plots
Plots <- list()
#### Modality ####
# Accuracy by Target
Plots[["Modality"]][["Accuracy"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = Target,
             fill = Accuracy)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Accuracy",
                    labels = c("Correct", "Error"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["TargetAccuracy"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

# Target by Condition
Plots[["Modality"]][["Condition"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = Target,
             fill = Condition)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Condition",
                    labels = c("No Pain", "Pain"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["TargetCondition"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

# Target by Distraction
Plots[["Modality"]][["Distraction"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = Target,
             fill = Distractor)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Distractor",
                    labels = c("Absent", "Present"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["TargetDistractor"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

# Target by Preparation
Plots[["Modality"]][["Preparation"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = Target,
             fill = CueType)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "CueType",
                    labels = c("Ambiguous", "Auditory","Visual"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["TargetPreparation"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))


#### Preparation ####
# Accuracy by CueType
Plots[["Preparation"]][["Accuracy"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = CueType,
             fill = Accuracy)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Accuracy",
                    labels = c("Correct", "Error"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["CueTypeAccuracy"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

# CueType by Condition
Plots[["Preparation"]][["Condition"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = CueType,
             fill = Condition)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Condition",
                    labels = c("No Pain", "Pain"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["CueTypeCondition"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

# CueType by Modaility
Plots[["Preparation"]][["Modality"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = CueType,
             fill = Target)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Target",
                    labels = c("Auditory", "Visual"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["CueTypeModality"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

# CueType by Distraction
Plots[["Preparation"]][["Distraction"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = CueType,
             fill = Distractor)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Distractor",
                    labels = c("Absent", "Present"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["CueTypeDistractor"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

#### Distraction ####
# Accuracy by Distractor
Plots[["Distraction"]][["Accuracy"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = Distractor,
             fill = Accuracy)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Accuracy",
                    labels = c("Correct", "Error"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["DistractorAccuracy"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

# Distractor by Condition
Plots[["Distraction"]][["Condition"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = Distractor,
             fill = Condition)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Condition",
                    labels = c("No Pain", "Pain"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["DistractorCondition"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

# Distractor by Preparation
Plots[["Distraction"]][["Preparation"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = Distractor,
             fill = CueType)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "CueType",
                    labels = c("Ambiguous", "Auditory","Visual"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["DistractorPreparation"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))

# Distractor by Modality
Plots[["Distraction"]][["Modality"]] <- 
  Trialdata %>% 
  ggplot(aes(y = RT,
             x = Distractor,
             fill = Target)) +
  geom_violin(alpha = .5,
              scale = "width",
              adjust = .5,
              colour = "black",
              draw_quantiles = c(.5)) +
  scale_fill_manual(name = "Target",
                    labels = c("Auditory", "Visual"),
                    values = cbbPalette) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(data = wse[["DistractorModality"]],
                aes(ymin = RT - se,
                    ymax = RT + se),
                lwd = .3,
                width = .5,
                position = position_dodge(.9))
#### Condition ####
# Condition by Accuracy
Plots[["Condition"]][["Accuracy"]] <-
  Trialdata %>%
  ggplot(aes(y = RT,
             x = Condition,
             fill = Accuracy)) +
  geom_violin(
    alpha = .5,
    scale = "width",
    adjust = .5,
    colour = "black",
    draw_quantiles = c(.5)
  ) +
  scale_colour_manual(
    name = "Accuracy",
    labels = c("Correct", "Error"),
    values = cbbPalette
  ) +
  scale_x_discrete(labels = c("No Pain", "Pain")) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(
    data = wse[["ConditionAccuracy"]],
    aes(ymin = RT - se,
        ymax = RT + se),
    lwd = .3,
    width = .5,
    position = position_dodge(.9)
  )

# Condition by Target
Plots[["Condition"]][["Modality"]] <-
  Trialdata %>%
  ggplot(aes(y = RT,
             x = Condition,
             fill = Target)) +
  geom_violin(
    alpha = .5,
    scale = "width",
    adjust = .5,
    colour = "black",
    draw_quantiles = c(.5)
  ) +
  scale_colour_manual(
    name = "Target",
    labels = c("Auditory", "Visual"),
    values = cbbPalette
  ) +
  scale_x_discrete(labels = c("No Pain", "Pain")) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(
    data = wse[["ConditionTarget"]],
    aes(ymin = RT - se,
        ymax = RT + se),
    lwd = .3,
    width = .5,
    position = position_dodge(.9)
  )

# Condition by CueType
Plots[["Condition"]][["Preparation"]] <-
  Trialdata %>%
  ggplot(aes(y = RT,
             x = Condition,
             fill = CueType)) +
  geom_violin(
    alpha = .5,
    scale = "width",
    adjust = .5,
    colour = "black",
    draw_quantiles = c(.5)
  ) +
  scale_colour_manual(
    name = "CueType",
    labels = c("Ambiguous", "Auditory", "Visual"),
    values = cbbPalette
  ) +
  scale_x_discrete(labels = c("No Pain", "Pain")) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(
    data = wse[["ConditionCueType"]],
    aes(ymin = RT - se,
        ymax = RT + se),
    lwd = .3,
    width = .5,
    position = position_dodge(.9)
  )

# Condition by Distraction
Plots[["Condition"]][["Distraction"]] <-
  Trialdata %>%
  ggplot(aes(y = RT,
             x = Condition,
             fill = Distractor)) +
  geom_violin(
    alpha = .5,
    scale = "width",
    adjust = .5,
    colour = "black",
    draw_quantiles = c(.5)
  ) +
  scale_colour_manual(
    name = "Distractor",
    labels = c("Absent", "Present"),
    values = cbbPalette
  ) +
  scale_x_discrete(labels = c("No Pain", "Pain")) +
  ylab("RT (ms)") +
  theme_classic() +
  geom_errorbar(
    data = wse[["ConditionDistraction"]],
    aes(ymin = RT - se,
        ymax = RT + se),
    lwd = .3,
    width = .5,
    position = position_dodge(.9)
  )

#### Save plots to pdf ####
save(Plots, file = "Output/RT_Plots.RData")
rmarkdown::render("Functions/RT_Plots.Rmd",
                  output_format = "pdf_document",
                  output_file = "../Output/RT_Boxplots.pdf") # since the rmd file is in Functions we need to go back one folder to find Output.
