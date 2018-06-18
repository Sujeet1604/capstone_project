library(dplyr)
library(tidyverse)
library(plyr)
library(stringr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

HCAHPS_Hospital <- read.csv("HCAHPS - Hospital.csv",stringsAsFactors = FALSE)

# CHECKING FOR "Not Available" #
nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Patient.Survey.Star.Rating == "Not Available"), ]) # 15720

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Answer.Percent == "Not Available"), ]) # 18648

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value == "Not Available"), ]) # 0

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Number.of.Completed.Surveys == "Not Available"), ]) # 14410

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Survey.Response.Rate.Percent == "Not Available"), ]) # 31845

# CHANGING "Not Available" TO NA #

HCAHPS_Hospital$Patient.Survey.Star.Rating[which(HCAHPS_Hospital$Patient.Survey.Star.Rating == "Not Available")] <- NA

HCAHPS_Hospital$HCAHPS.Answer.Percent[which(HCAHPS_Hospital$HCAHPS.Answer.Percent == "Not Available")] <- NA

HCAHPS_Hospital$Number.of.Completed.Surveys[which(HCAHPS_Hospital$Number.of.Completed.Surveys == "Not Available")] <- NA

HCAHPS_Hospital$Survey.Response.Rate.Percent[which(HCAHPS_Hospital$Survey.Response.Rate.Percent == "Not Available")] <- NA


# CHECKING FOR Blanks #

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Patient.Survey.Star.Rating.Footnote == ""), ]) # 249270

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Answer.Percent.Footnote == ""), ]) # 220958

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Number.of.Completed.Surveys.Footnote == ""), ]) # 233255

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Survey.Response.Rate.Percent.Footnote == ""), ]) # 189255

# Change all blanks to NA #

HCAHPS_Hospital$Patient.Survey.Star.Rating.Footnote[which(HCAHPS_Hospital$Patient.Survey.Star.Rating.Footnote == "")]<- NA

HCAHPS_Hospital$HCAHPS.Answer.Percent.Footnote[which(HCAHPS_Hospital$HCAHPS.Answer.Percent.Footnote == "")]<- NA

HCAHPS_Hospital$Number.of.Completed.Surveys.Footnote[which(HCAHPS_Hospital$Number.of.Completed.Surveys.Footnote == "")]<- NA

HCAHPS_Hospital$Survey.Response.Rate.Percent.Footnote[which(HCAHPS_Hospital$Survey.Response.Rate.Percent.Footnote == "")]<- NA

# Remove columns from dataframe where ALL values are NA
HCAHPS_Hospital <- HCAHPS_Hospital[,colSums(is.na(HCAHPS_Hospital))<nrow(HCAHPS_Hospital)] 

## Measure the percentage of NA's in each column

sapply(HCAHPS_Hospital, function(x) (sum(is.na(x))/nrow(HCAHPS_Hospital)*100))


colsToRemove = c("Patient.Survey.Star.Rating.Footnote",
                 "Number.of.Completed.Surveys.Footnote",
                 "Survey.Response.Rate.Percent.Footnote",
                 "HCAHPS.Answer.Percent.Footnote",
                 "Footnote.y")

HCAHPS_Hospital<-HCAHPS_Hospital[,-which(names(HCAHPS_Hospital) %in% colsToRemove)]

## converting date columns to POSIXlt

HCAHPS_Hospital$Measure.Start.Date<-as.POSIXlt(HCAHPS_Hospital$Measure.Start.Date, format = "%m/%d/%Y")
HCAHPS_Hospital$Measure.End.Date<-as.POSIXlt(HCAHPS_Hospital$Measure.End.Date, format = "%m/%d/%Y")

# CHECKING FOR "Not Applicable" #
nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Patient.Survey.Star.Rating == "Not Applicable"), ]) # 207174

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Answer.Percent == "Not Applicable"), ]) # 110814

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value == "Not Applicable"), ]) # 211992



# CHANGING "Not Applicable" TO 0 #

HCAHPS_Hospital$Patient.Survey.Star.Rating[which(HCAHPS_Hospital$Patient.Survey.Star.Rating == "Not Applicable")] <- 0

HCAHPS_Hospital$HCAHPS.Answer.Percent[which(HCAHPS_Hospital$HCAHPS.Answer.Percent == "Not Applicable")] <- 0

HCAHPS_Hospital$HCAHPS.Linear.Mean.Value[which(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value == "Not Applicable")] <- 0

HCAHPS_Hospital[is.na(HCAHPS_Hospital)] <- 0

# converting to numeric columns 

HCAHPS_Hospital$Patient.Survey.Star.Rating <-as.numeric(HCAHPS_Hospital$Patient.Survey.Star.Rating)
HCAHPS_Hospital$HCAHPS.Answer.Percent <- as.numeric(HCAHPS_Hospital$HCAHPS.Answer.Percent)
HCAHPS_Hospital$HCAHPS.Linear.Mean.Value <- as.numeric(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value)
HCAHPS_Hospital$Number.of.Completed.Surveys <- as.numeric(HCAHPS_Hospital$Number.of.Completed.Surveys)
HCAHPS_Hospital$Survey.Response.Rate.Percent <- as.numeric(HCAHPS_Hospital$Survey.Response.Rate.Percent)

# checking for duplicates post cleanup
nrow(HCAHPS_Hospital)
nrow(unique(HCAHPS_Hospital))

summary(HCAHPS_Hospital)
###########################################################################################################

HCAHPS_State <- read.csv("HCAHPS - State.csv",stringsAsFactors = FALSE)

#Checking for columns with NA along with percentage
sapply(HCAHPS_State, function(x) sum(is.na(x)))
sapply(HCAHPS_State, function(x) (sum(is.na(x))/nrow(HCAHPS_State)*100))

HCAHPS_State <- HCAHPS_State[,colSums(is.na(HCAHPS_State))<nrow(HCAHPS_State)] 

HCAHPS_State[HCAHPS_State == "" ] <- NA
HCAHPS_State[is.na(HCAHPS_State)] <- 0

#removing columns with all NA values - Footnote
HCAHPS_State<-HCAHPS_State[,-6]

## converting date columns to POSIXlt
HCAHPS_State$Measure.Start.Date<-as.POSIXlt(HCAHPS_State$Measure.Start.Date, format = "%m/%d/%Y")
HCAHPS_State$Measure.End.Date<-as.POSIXlt(HCAHPS_State$Measure.End.Date, format = "%m/%d/%Y")

# converting to numeric columns 
HCAHPS_State$HCAHPS.Answer.Percent <- as.numeric(HCAHPS_State$HCAHPS.Answer.Percent)

summary(HCAHPS_State)

# Prefixing columns specific to state with "S_"" except the columns used for merging

colnames(HCAHPS_State)[4:7] <- paste("S", colnames(HCAHPS_State[,c(4:7)]), sep = "_")
colnames(HCAHPS_State)[2] <- "S_HCAHPS_State$HCAHPS.Question"

HCAHPS<- merge(x = HCAHPS_Hospital, y = HCAHPS_State, by = c("State","HCAHPS.Measure.ID"), all.x = TRUE)
#############################################################################

HCAHPS_National <- read.csv("HCAHPS - National.csv",stringsAsFactors = FALSE)

#Checking for columns with NA along with percentage
sapply(HCAHPS_National, function(x) sum(is.na(x)))
sapply(HCAHPS_National, function(x) (sum(is.na(x))/nrow(HCAHPS_National)*100))

#removing columns with all NA values - Footnote
HCAHPS_National <- HCAHPS_National[,colSums(is.na(HCAHPS_National))<nrow(HCAHPS_National)] 

## converting date columns to POSIXlt
HCAHPS_National$Measure.Start.Date<-as.POSIXlt(HCAHPS_National$Measure.Start.Date, format = "%m/%d/%Y")
HCAHPS_National$Measure.End.Date<-as.POSIXlt(HCAHPS_National$Measure.End.Date, format = "%m/%d/%Y")

# converting to numeric columns 
HCAHPS_National$HCAHPS.Answer.Percent <- as.numeric(HCAHPS_National$HCAHPS.Answer.Percent)

# Prefixing columns specific to National with "N_"" except the columns used for merging
colnames(HCAHPS_National)[2:6] <- paste("N", colnames(HCAHPS_National[,c(2:6)]), sep = "_")

HCAHPS<- merge(x = HCAHPS, y = HCAHPS_National, by = "HCAHPS.Measure.ID", all.x = TRUE)

## once merged(left join) there would be lot of NA's due to the data in different files.
## Do we need to go for INNER JOIN even if the data volumes come down???
sapply(HCAHPS, function(x) (sum(is.na(x))/nrow(HCAHPS)*100))


################################# HAI Data Cleanup and Merge #####################################################################


HAI_Hospital <- read.csv("Healthcare Associated Infections - Hospital.csv",stringsAsFactors = FALSE)

#Checking for columns with NA along with percentage
sapply(HAI_Hospital, function(x) (sum(is.na(x))/nrow(HAI_Hospital)*100))

# Replacing all BLANKS and NOT APPLICABLE to NA
HAI_Hospital[HAI_Hospital == "" ] <- NA
HAI_Hospital[HAI_Hospital == "Not Applicable" ] <- NA

# Remove columns from dataframe where ALL values are NA
HCAHPS_Hospital <- HCAHPS_Hospital[,colSums(is.na(HCAHPS_Hospital))<nrow(HCAHPS_Hospital)] 

#Checking for columns with NA along with percentage
sapply(HAI_Hospital, function(x) (sum(is.na(x))/nrow(HAI_Hospital)*100))

# Removing columns with more than 60% NA
HAI_Hospital<- HAI_Hospital[ , -which(names(HAI_Hospital) %in% c("Compared.to.National","Footnote"))]

## converting date columns to POSIXlt
HAI_Hospital$Measure.Start.Date<-as.POSIXlt(HAI_Hospital$Measure.Start.Date, format = "%m/%d/%Y")
HAI_Hospital$Measure.End.Date<-as.POSIXlt(HAI_Hospital$Measure.End.Date, format = "%m/%d/%Y")

## converting to numeric columns after replacing NA with 0
HAI_Hospital[is.na(HAI_Hospital)] <- 0
HAI_Hospital$Score <- as.numeric(HAI_Hospital$Score)

########################################################################

HAI_State <- read.csv("Healthcare Associated Infections - State.csv",stringsAsFactors = FALSE)

sapply(HAI_State, function(x) (sum(is.na(x))/nrow(HAI_State)*100))

# Replacing all BLANKS and NOT APPLICABLE to NA
HAI_State[HAI_State == "" ] <- NA
HAI_State[HAI_State == "Not Applicable" ] <- NA

# Removing columns with more than 60% NA
HAI_State<- HAI_State[ , -which(names(HAI_State) %in% "Footnote")]

## converting date columns to POSIXlt
HAI_State$Measure.Start.Date<-as.POSIXlt(HAI_State$Measure.Start.Date, format = "%m/%d/%Y")
HAI_State$Measure.End.Date<-as.POSIXlt(HAI_State$Measure.End.Date, format = "%m/%d/%Y")

## converting to numeric columns after replacing NA with 0
HAI_State[is.na(HAI_State)] <- 0
HAI_State$Score <- as.numeric(HAI_State$Score)

# Re-ordering the columns for prefixing
refcols <- c("State", "Measure.ID")
  HAI_State <- HAI_State[, c(refcols, setdiff(names(HAI_State), refcols))]
names(HAI_State)

# Prefixing columns specific to state with "S_"" except the columns used for merging
colnames(HAI_State)[3:6] <- paste("S", colnames(HAI_State[,c(3:6)]), sep = "_")

# Merging the files with common columns
HAI<- merge(x = HAI_Hospital, y = HAI_State, by = c("State","Measure.ID"), all.x = TRUE)

# checking for NA's due to the merger
sapply(HAI, function(x) (sum(is.na(x))/nrow(HAI)*100))

#################################################################################################

HAI_National <- read.csv("Healthcare Associated Infections - National.csv",stringsAsFactors = FALSE)

#Checking for columns with NA along with percentage
sapply(HAI_National, function(x) (sum(is.na(x))/nrow(HAI_National)*100))

# Replacing all BLANKS and NOT APPLICABLE to NA
HAI_National[HAI_National == "" ] <- NA
HAI_National[HAI_National == "Not Applicable" ] <- NA

# Removing columns with more than 60% NA
HAI_National<- HAI_National[ , -which(names(HAI_National) %in% "Footnote")]

## converting date columns to POSIXlt
HAI_National$Measure.Start.Date<-as.POSIXlt(HAI_National$Measure.Start.Date, format = "%m/%d/%Y")
HAI_National$Measure.End.Date<-as.POSIXlt(HAI_National$Measure.End.Date, format = "%m/%d/%Y")

## converting to numeric columns after replacing NA with 0 
HAI_National[is.na(HAI_National)] <- 0
HAI_National$Score <- as.numeric(HAI_National$Score)

# Re-ordering the columns for prefixing
refcols <- c("Measure.ID")
HAI_National <- HAI_National[, c(refcols, setdiff(names(HAI_National), refcols))]
names(HAI_National)

# Prefixing columns specific to state with "S_"" except the columns used for merging

colnames(HAI_National)[2:5] <- paste("N", colnames(HAI_National[,c(2:5)]), sep = "_")

HAI<- merge(x = HAI, y = HAI_National, by = "Measure.ID", all.x = TRUE)


sapply(HAI, function(x) (sum(is.na(x))/nrow(HAI)*100)) 
## once merged(left join) there would be lot of NA's due to the data in different files.
## Do we need to go for INNER JOIN even if the data volumes come down???

######################################
