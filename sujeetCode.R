
library(dplyr)

library(tidyr)

library(stringr)

library(ggplot2)

library(lubridate)


## Loading the dataset


Hospital_Gen_info <- read.csv("Hospital General Information.csv",stringsAsFactors = FALSE,na.strings=c("","NA"))
## 4818 obs. of 28 variables
Hospital_Quaterly_MSPB<- read.csv("HOSPITAL_QUARTERLY_MSPB_6_DECIMALS.csv",stringsAsFactors = FALSE,na.strings=c("","NA"))
## 3232 obs. of 6 variables
Hospital_Quaterly_Quality<- read.csv("HOSPITAL_QUARTERLY_QUALITYMEASURE_PCH_HOSPITAL.csv",stringsAsFactors = FALSE,na.strings=c("","NA"))
## 33 obs. of 15 variables
Value_Of_Care_National<- read.csv("Value of Care - National.csv",stringsAsFactors = FALSE,na.strings=c("","NA"))
## 30 obs. of 5 variables

##Data understanding

summary(Hospital_Gen_info) 
summary(Hospital_Quaterly_MSPB)
summary(Hospital_Quaterly_Quality)
summary(Value_Of_Care_National)

########################################### Data Cleaning #########################################

#Hospital_Gen_info

#CHECK FOR DUPLICATE Provider IDs
sum(duplicated(Hospital_Gen_info$Provider.ID))
# 0 duplicate provider ID

#CHECK FOR DUPLICATE Hospital Names
sum(duplicated(Hospital_Gen_info$Hospital.Name))
# 201 duplicate Hospital Names with Different provider IDs


# Checking for NA values

sapply(Hospital_Gen_info, function(x) sum(is.na(x)))

## Footnotes are mostly having NA values, lets measure the same in percentage

sapply(Hospital_Gen_info, function(x) (sum(is.na(x))/nrow(Hospital_Gen_info)*100))


## So, Footnotes are haviing 57-79% of data with NA values so we can staightaway remove these
colnames(Hospital_Gen_info)

colsToRemove_GenInfo = c("Hospital.overall.rating.footnote","Mortality.national.comparison.footnote",
                         "Safety.of.care.national.comparison.footnote","Readmission.national.comparison.footnote",
                         "Patient.experience.national.comparison.footnote","Effectiveness.of.care.national.comparison.footnote",
                         "Timeliness.of.care.national.comparison.footnote","Efficient.use.of.medical.imaging.national.comparison.footnote")

Hospital_Gen_info<-Hospital_Gen_info[,-which(names(Hospital_Gen_info) %in% colsToRemove_GenInfo)]

##Now few columns are value as 'Not Available', which is similar to NA. So convert them to NA
Hospital_Gen_info[ Hospital_Gen_info == "Not Available" ] <- NA

##Now, again check the NA value count
sapply(Hospital_Gen_info, function(x) sum(is.na(x)))

##################Imputation############################

#1. County Name, with 15 NA values

Hospital_Gen_info[which(is.na(Hospital_Gen_info$County.Name)),]

##getting the city names for county with NA values
City_Names_woCounty<-Hospital_Gen_info[which(is.na(Hospital_Gen_info$County.Name)),]$City

## checking if county names are available for those cities
Hospital_Gen_info[which(Hospital_Gen_info$City %in% City_Names_woCounty),][,c(4,7)]

## 3 county names can be imputed with existing city names
Hospital_Gen_info[which(Hospital_Gen_info$City=="HOMER"),]$County.Name<- "CLAIBORNE"
Hospital_Gen_info[which(Hospital_Gen_info$City=="SEWARD"),]$County.Name<- "SEWARD"
Hospital_Gen_info[which(Hospital_Gen_info$City=="PETERSBURG"),]$County.Name<- "PETERSBURG CITY"

sapply(Hospital_Gen_info, function(x) (sum(is.na(x))/nrow(Hospital_Gen_info)*100))

##Now, lets convert columns to factors whichever required


colsToFactor_GenInfo<- c("City","State","ZIP.Code","County.Name","Hospital.Type","Hospital.Ownership","Emergency.Services",
                         "Meets.criteria.for.meaningful.use.of.EHRs","Hospital.overall.rating","Mortality.national.comparison",
                         "Safety.of.care.national.comparison","Readmission.national.comparison","Patient.experience.national.comparison",
                         "Effectiveness.of.care.national.comparison","Timeliness.of.care.national.comparison",
                         "Efficient.use.of.medical.imaging.national.comparison")

Hospital_Gen_info[colsToFactor_GenInfo] <- lapply(Hospital_Gen_info[colsToFactor_GenInfo], factor)

summary(Hospital_Gen_info)




###############################################Hospital_Quaterly_Quality###########################################################

##Few columns are value as 'Not Available', which is similar to NA. So convert them to NA
Hospital_Quaterly_Quality[ Hospital_Quaterly_Quality == "Not Available" ] <- NA


##Checking for columns with NA count

sapply(Hospital_Quaterly_Quality, function(x) sum(is.na(x)))

##Again Footnote column is non relevent
Hospital_Quaterly_Quality<-Hospital_Quaterly_Quality[,-13]

## converting date columns to POSIXlt

Hospital_Quaterly_Quality$RPTG_PRD_START_DT<-as.POSIXlt(Hospital_Quaterly_Quality$RPTG_PRD_START_DT, format = "%m/%d/%Y")
Hospital_Quaterly_Quality$RPTG_PRD_END_DT<-as.POSIXlt(Hospital_Quaterly_Quality$RPTG_PRD_END_DT, format = "%m/%d/%Y")

#Calculating Reporting duration in months

Hospital_Quaterly_Quality$RPTG_Duration_Months<- interval(Hospital_Quaterly_Quality$RPTG_PRD_START_DT, Hospital_Quaterly_Quality$RPTG_PRD_END_DT) %/% months(1)
## Reporting duration for all are 11 months so these columns are irrelevent 

Hospital_Quaterly_Quality<-Hospital_Quaterly_Quality[,-c(13:15)]

colnames(Hospital_Quaterly_Quality)




###############################################Hospital_Quaterly_MSPB###########################################################

summary(Hospital_Quaterly_MSPB)

## Start Date and End Date is irrelevent for analysis as duration is 1 year for all
## Footnotes are again irrelevent 


Hospital_Quaterly_MSPB <- Hospital_Quaterly_MSPB[,1:3]

#Converting Measure_Id to factor 

Hospital_Quaterly_MSPB$Measure_ID<- as.factor(Hospital_Quaterly_MSPB$Measure_ID)

summary(Hospital_Quaterly_MSPB$Measure_ID)

## As we have ony one value throughout the dataset, we can omit variable "Measure_ID" out of the context

Hospital_Quaterly_MSPB<- Hospital_Quaterly_MSPB[,c(1,3)]

## Checking the existence of NA values

sapply(Hospital_Quaterly_MSPB, function(x) sum(is.na(x)))

## No null values so Hospital_Quaterly_MSPB is now good to merge



###############################################Value_Of_Care_National###########################################################


summary(Value_Of_Care_National)

## Start Date and End Date is irrelevent for analysis as duration is 6 months for all

Value_Of_Care_National<- Value_Of_Care_National[,1:3]

## Let's make Value.of.care.measure.name & Value.of.care.measure.ID as facrors

Value_Of_Care_National$Value.of.care.measure.name<- as.factor(Value_Of_Care_National$Value.of.care.measure.name)

Value_Of_Care_National$Value.of.care.measure.ID<- as.factor(Value_Of_Care_National$Value.of.care.measure.ID)

summary(Value_Of_Care_National)

## It seems, This dataset is a consolidated reference