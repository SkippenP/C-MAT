ExtractTrialData <- function(PartID,COND){

## Code to extract RT values from MATLAB event structures

require(rmatio)
require(dplyr)

## Import Data
mat <-
  rmatio::read.mat(paste("../Data/",PartID,"/Behavior/",
                         paste(PartID,COND,sep = "_"),
                         "_EventCodes.mat",sep = ""))

EventData <- data.frame(Lat = unlist(mat$EventCodes$latency),
                        Trig = mat$EventCodes$type)

## Triggers of Interest; Stimulus codes, Target Codes, Response Codes
TOI <- list()
TOI[['Stim']] <- c("S  1","S  2","S  3")
TOI[['Target']] <- c("S101","S102","S103",
                     "S110","S112","S113",
                     "S121","S120","S123",
                     "S131","S132","S130")
TOI[["Resp"]] <- c("S201","S202","S203") # The first participants have these codes
                   # "S 73","S 74","S 75") # The px CLP130 had these codes
TOI[["TrialEnd"]] <- "S 50"

## Trigger codes are organised as follows:
# Cue: 1 = Visual, 2 = Auditory, 3 = Ambiguous
# Target: 1VA where V = Visual target, A = Auditory target
# E.g., 123 = Visual Target indicating middle resp; Auditory Target indicating Right response
# e.g., 101 = No Visual target; Auditory Target indicating Left response
# Repsonse: 1 = Left, 2 = Middle, 3 = Right.


## RT on trial one:
CueCode  <- which(EventData$Trig %in% TOI[["Stim"]]) # pos. of Cue Trig Codes
TargCode <- which(EventData$Trig %in% TOI[["Target"]]) # pos. of Target Trig Codes
RespCode <- which(EventData$Trig %in% TOI[["Resp"]]) # pos. of Response Trig Codes
ErrCheck <- which((TargCode+2) %in% RespCode)

# Misses (omissions) are not recorded.
# Instead the response code is positioned after the end of the trial
# Likewise, some double presses are coded.

## Create list with each trial as seperate dataframe
TrialList <- list()
for (i in seq_along(CueCode)){
TrialList[[i]] <- EventData[(CueCode[[i]]:(CueCode[[i]]+6)),]
}

## Find errors
# There are a few rules that a true trial follows. These inlcude;
# Starting with a Cue Trigger
# Finishing with an 'S 50' trigger
# Having an 'S 20' trigger in the second location
# Having an 'S 10' trigger following the response
# Having an 'S 40' trigger seperating the Target and Response

# Main 'programming' errors include double presses and omissions

## Remove double presses
# Double presses with have two triggers from the Resp codes
# We'll also give these an NA value.
checkdubs <- lapply(seq_along(TrialList),
                    function(x) sum(TrialList[[x]]$Trig %in% TOI$Resp)
)
# Since trues are coded as 1; a sum > 1 would indicate two instances
idxdubs   <- which(unlist(checkdubs) > 1)

# Remove the double RT values from ever being calculated
TrialList[idxdubs] <- NULL

# Calculate RT.
RTlist <- list()
for(i in seq_along(TrialList)){
 RTlist[[i]] <- (TrialList[[i]] %>% dplyr::filter(Trig %in% TOI$Resp) %>% .$Lat)-(TrialList[[i]] %>% dplyr::filter(Trig %in% TOI$Target) %>% .$Lat)
}

# Find omissions - [5,2] should be the response position. 
# In misses this is moved. We'll give these 'NA' RT
checkomms <- lapply(seq_along(TrialList),function(x) TrialList[[x]][[5,2]])
# These are the positions of the misses within the data
idxomms   <- which(!unlist(checkomms) %in% TOI$Resp)
# Mark Omissions with NA
RTlist[idxomms] <- NA

## Create Data Frame to hold trial information and RT values
Trialdata <- data.frame(TrialNum = seq_along(TrialList),
                     RT = unlist(RTlist))

## Define Trial types:
## Cues
# Visual Cues
VCcheck <- lapply(seq_along(TrialList),
             function(x) TrialList[[x]][[1,2]])
idxVC   <- which(unlist(VCcheck) %in% 'S  1')
# Auditory Cues
ACcheck <- lapply(seq_along(TrialList),
                  function(x) TrialList[[x]][[1,2]])
idxAC   <- which(unlist(ACcheck) %in% 'S  2')
# Ambiguous Cues
AmbCcheck <- lapply(seq_along(TrialList),
                  function(x) TrialList[[x]][[1,2]])
idxAmbC   <- which(unlist(AmbCcheck) %in% 'S  3')

Cues <- seq(1,length(TrialList))
Cues[idxVC] <- "Visual"
Cues[idxAC] <- "Auditory"
Cues[idxAmbC] <- "Ambiguous"

# Add to data frame
Trialdata$CueType <- Cues

## Target position
# Visual Targets
VTLcheck <- lapply(seq_along(TrialList),
                  function(x) TrialList[[x]][[3,2]])
idxVTL   <- which(unlist(VTLcheck) %in% c("S110","S112","S113"))

VTMcheck <- lapply(seq_along(TrialList),
                   function(x) TrialList[[x]][[3,2]])
idxVTM   <- which(unlist(VTMcheck) %in% c("S120","S121","S123"))

VTRcheck <- lapply(seq_along(TrialList),
                   function(x) TrialList[[x]][[3,2]])
idxVTR   <- which(unlist(VTRcheck) %in% c("S130","S132","S131"))

VisPositions <- rep(NA,length(TrialList))
VisPositions[idxVTL] <- "Left"
VisPositions[idxVTM] <- "Middle"
VisPositions[idxVTR] <- "Right"

# Add to data frame
Trialdata$VisualPos <- VisPositions

# Auditory Targets
ATLcheck <- lapply(seq_along(TrialList),
                   function(x) TrialList[[x]][[3,2]])
idxATL   <- which(unlist(ATLcheck) %in% c("S101","S121","S131"))

ATMcheck <- lapply(seq_along(TrialList),
                   function(x) TrialList[[x]][[3,2]])
idxATM   <- which(unlist(ATMcheck) %in% c("S132","S112","S102"))

ATRcheck <- lapply(seq_along(TrialList),
                   function(x) TrialList[[x]][[3,2]])
idxATR   <- which(unlist(ATRcheck) %in% c("S103","S113","S123"))

AudPositions <- rep(NA,length(TrialList))
AudPositions[idxATL] <- "Left"
AudPositions[idxATM] <- "Middle"
AudPositions[idxATR] <- "Right"

# Add to data frame
Trialdata$AuditoryPos <- AudPositions

## Target
Trialdata$Target <- Trialdata$CueType
Trialdata$Target[Trialdata$CueType=="Ambiguous"&
                   is.na(Trialdata$VisualPos)] <- "Auditory"
Trialdata$Target[Trialdata$CueType=="Ambiguous"&
                   is.na(Trialdata$AuditoryPos)] <- "Visual"

## Distractor Pos
Trialdata$DistractorPos <- "None"
Trialdata$DistractorPos[Trialdata$Target=="Visual"] <- Trialdata$AuditoryPos[Trialdata$Target=="Visual"]
Trialdata$DistractorPos[Trialdata$Target=="Auditory"] <- Trialdata$VisualPos[Trialdata$Target=="Auditory"]
Trialdata$DistractorPos[is.na(Trialdata$DistractorPos)] <- "None"
## Distractor
Trialdata$Distractor <- "Present"
Trialdata$Distractor[Trialdata$DistractorPos=="None"] <- "Absent"

## Responses
# Left Resp
LRcheck <- lapply(seq_along(TrialList),
                   function(x) TrialList[[x]][[5,2]])
idxLR   <- which(unlist(LRcheck) %in% c("S201","S 73"))

# Middle Resp
MRcheck <- lapply(seq_along(TrialList),
                  function(x) TrialList[[x]][[5,2]])
idxMR   <- which(unlist(MRcheck) %in% c("S202","S 74"))

# Left Resp
RRcheck <- lapply(seq_along(TrialList),
                  function(x) TrialList[[x]][[5,2]])
idxRR   <- which(unlist(RRcheck) %in% c("S203","S 75"))

Responses <- rep(NA,length(TrialList))
Responses[idxLR] <- "Left"
Responses[idxMR] <- "Middle"
Responses[idxRR] <- "Right"

# Add to data frame
Trialdata$Response <- Responses

#### Accuracy ####
Trialdata$Accuracy <- "Error"

## Visual Corrects
Trialdata$Accuracy[Trialdata$CueType=="Visual" &
                     Trialdata$VisualPos=="Left"&
                     Trialdata$Response=="Left"] <- "Correct"

Trialdata$Accuracy[Trialdata$CueType=="Visual" &
                     Trialdata$VisualPos=="Middle"&
                     Trialdata$Response=="Middle"] <- "Correct"

Trialdata$Accuracy[Trialdata$CueType=="Visual" &
                     Trialdata$VisualPos=="Right"&
                     Trialdata$Response=="Right"] <- "Correct"

## Auditory Corrects
Trialdata$Accuracy[Trialdata$CueType=="Auditory" &
                     Trialdata$AuditoryPos=="Left"&
                   Trialdata$Response=="Left"] <- "Correct"

Trialdata$Accuracy[Trialdata$CueType=="Auditory" &
                     Trialdata$AuditoryPos=="Middle"&
                     Trialdata$Response=="Middle"] <- "Correct"

Trialdata$Accuracy[Trialdata$CueType=="Auditory" &
                     Trialdata$AuditoryPos=="Right"&
                     Trialdata$Response=="Right"] <- "Correct"

## Ambiguous Cues - Auditory Targets
Trialdata$Accuracy[Trialdata$CueType=="Ambiguous" &
                     Trialdata$AuditoryPos=="Left"&
                     Trialdata$Response=="Left"] <- "Correct"

Trialdata$Accuracy[Trialdata$CueType=="Ambiguous" &
                     Trialdata$AuditoryPos=="Middle"&
                     Trialdata$Response=="Middle"] <- "Correct"

Trialdata$Accuracy[Trialdata$CueType=="Ambiguous" &
                     Trialdata$AuditoryPos=="Right"&
                     Trialdata$Response=="Right"] <- "Correct"

## Ambiguous Cues - Visual Targets
Trialdata$Accuracy[Trialdata$CueType=="Ambiguous" &
                     Trialdata$VisualPos=="Left"&
                     Trialdata$Response=="Left"] <- "Correct"

Trialdata$Accuracy[Trialdata$CueType=="Ambiguous" &
                     Trialdata$VisualPos=="Middle"&
                     Trialdata$Response=="Middle"] <- "Correct"

Trialdata$Accuracy[Trialdata$CueType=="Ambiguous" &
                     Trialdata$VisualPos=="Right"&
                     Trialdata$Response=="Right"] <- "Correct"


## Omissions
Trialdata$Accuracy[is.na(Trialdata$Response)] <- "Miss"

# ## Check errors
# Err <- Trialdata[Trialdata$Accuracy=="Error",]

## Add PartID
Trialdata$PartID <- PartID
Trialdata$Condition <- COND

## Add Cue-to-Target Interval
# Calculate CTI
CTIlist <- list()
for (i in seq_along(TrialList)) {
  CTIlist[[i]] <-
    (TrialList[[i]] %>% dplyr::filter(Trig %in% TOI$Target) %>% .$Lat) - (TrialList[[i]] %>% dplyr::filter(Trig %in% TOI$Stim) %>% .$Lat)
  if (length(CTIlist[[i]]) > 1) {
    CTIlist[[i]] <- NA
  }
}
Trialdata$CTI <- unlist(CTIlist)

## Add Inter-Trial Interval
# Calculate ITI
ITIlist <- list()
for (i in 1:(length(TrialList)-1)) {
  ITIlist[[i]] <-
    (TrialList[[i+1]] %>% dplyr::filter(Trig %in% TOI$Stim) %>% .$Lat) - (TrialList[[i]] %>% dplyr::filter(Trig %in% TOI$Stim) %>% .$Lat)
  if (length(ITIlist[[i]]) > 1) {
    ITIlist[[i]] <- NA
  }
}
ITIlist[[length(TrialList)]] <- NA
Trialdata$ITI <- unlist(ITIlist)
# Top 5 ITI's will be the ITI's between blocks. Give these NA
Trialdata$ITI[Trialdata$ITI %in% tail(sort(Trialdata$ITI),5)] <- NA

# Order Trialdata columns
Trialdata <- Trialdata %>% dplyr::select(PartID,TrialNum,CueType,
                                         VisualPos,AuditoryPos,
                                         Target,Distractor,DistractorPos,
                                         Response,RT,Accuracy,Condition,
                                         CTI,ITI)

#### Write Trialdata to file ####
write.csv(Trialdata,row.names = F,
          file = paste("../Data/",PartID,"/Behavior/",
                       paste(PartID,COND,sep = "_"),
                       "_TrialData.csv",sep = ""))

#### Create Summary Plots ####
return(Trialdata)
}










