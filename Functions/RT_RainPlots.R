RT_RainPlot <- function(data,
                        xval,
                        groupval,
                        w = 16,
                        h = 9) {
  # width and height variables for saved plots
  w = w
  h = h
  
  ## Take full data frame and reduce down into a list of smaller data frames with the same names.
  
  #e.g.,
  Plotdata <- list()
  
  Plotdata[[paste(xval, groupval, sep = "")]] <- data %>%
    dplyr::filter(Accuracy != "Miss") %>% # This is necessary across all RT data - Hard coded!
    dplyr::group_by(PartID, get(xval), get(groupval)) %>%
    dplyr::summarise(RT = mean(RT, na.rm = T))
  
  names(Plotdata[[paste(xval, groupval, sep = "")]]) <-
    c("PartID", "xval", "groupval", "RT")
  
  wse <- list()
  wse[[paste(xval, groupval, sep = "")]] <- Rmisc::summarySEwithin(
    Plotdata[[paste(xval, groupval, sep = "")]],
    measurevar = "RT",
    withinvars = c("xval", "groupval"),
    idvar = "PartID",
    na.rm = T,
    conf.interval = .95
  )
  
  ## Make and save plot
  ggplot(Plotdata[[paste(xval, groupval, sep = "")]], aes(x = xval, y = RT, fill = groupval)) +
    geom_flat_violin(
      aes(fill = groupval),
      position = position_nudge(x = .1, y = 0),
      adjust = 1.5,
      trim = FALSE,
      alpha = .5,
      colour = NA
    ) +
    geom_point(
      aes(
        x = as.numeric(xval) - .15,
        y = RT,
        colour = groupval
      ),
      position = position_jitter(width = .05),
      size = 1.5,
      shape = 20
    ) +
    geom_boxplot(
      aes(x = xval, y = RT, fill = groupval),
      outlier.shape = NA,
      alpha = .5,
      width = .1,
      colour = "black"
    ) +
    geom_point(
      data = wse[[paste(xval, groupval, sep = "")]],
      aes(
        x = as.numeric(xval) +-.15,
        y = RT,
        group = groupval,
        colour = groupval
      ),
      shape = 18,
      size = 3
    ) +
    geom_errorbar(
      data = wse[[paste(xval, groupval, sep = "")]],
      aes(
        x = as.numeric(xval) +-.15,
        y = RT,
        group = groupval,
        colour = groupval,
        ymin = RT - se,
        ymax = RT + se
      ),
      width = .05
    ) +
    labs(x = xval,
         colour = groupval,
         fill = groupval) +
    scale_colour_manual(values = wes_palette("GrandBudapest1")) + # Change to colour blind friendly for publication
    scale_fill_manual(values = wes_palette("GrandBudapest1")) +
    coord_flip() +
    theme_classic() +
    theme(legend.position = c(.9, .1)) +
    ggsave(
      paste("Output/Plots/", xval, groupval, ".tiff", sep = ""),
      width = w,
      height = h
    )
  
}