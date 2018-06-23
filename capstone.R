library(tidyverse)
library(plyr)



library(dplyr)

library(tidyr)

library(stringr)

library(ggplot2)

library(lubridate)


##################Loading CSVs into dataframe ########################################
## Reading Files from Directory
folder <- getwd()  
folder <- paste(folder,"/", sep="")

# path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder
list.filenames<-  gsub(".csv","",file_list)
list.filenames<-  gsub("-","_",list.filenames)
list.filenames<-  gsub(" ","_",list.filenames)
list.filenames<-  gsub("___","_",list.filenames)
list.filenames<-  gsub("__","_",list.filenames)


list.filenames

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(list.filenames[i], 
         read.csv(paste(folder, file_list[i], sep=''),stringsAsFactors = FALSE,na.strings=c("","NA"))
  )}


dfs<- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))


#####################Basic cleaning ############################################

## Removing variable with mostly having NA values
############################################
## 1 Hospital_Gen_info
############################################

Hospital_Gen_info<- Hospital_General_Information

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


############################################
## 2.Complications
############################################

df_Complications<-Complications
df_Complications[ df_Complications == "Not Available" ] <- NA

sapply(df_Complications, function(x) (sum(is.na(x))/nrow(df_Complications)*100))

Complications_Cols_To_Remove<- c("X","Hospital.Name","Address","City","ZIP.Code","County.Name",
                                 "Phone.Number","PSI_90_SAFETY_TOF_NAT","PSI_90_SAFETY_TOF_STA")


df_Complications<-df_Complications[,-which(names(df_Complications) %in% Complications_Cols_To_Remove)]

df_Complications[ df_Complications == "Better than the National Rate" ] <- "Better_Rate"
df_Complications[ df_Complications == "No Different than the National Rate" ] <- "Same_Rate"
df_Complications[ df_Complications == "Number of Cases Too Small" ] <- "Insufficient_Cases"
df_Complications[ df_Complications == "Worse than the National Rate" ] <- "Worse_Rate"

colnames(df_Complications)

Complications_Cols_To_Factor<- c("State","COMP_HIP_KNEE_CTN","PSI_12_POSTOP_PULMEMB_DVT_CTN","PSI_13_POST_SEPSIS_CTN",
                                 "PSI_14_POSTOP_DEHIS_CTN","PSI_15_ACC_LAC_CTN","PSI_3_ULCER_CTN","PSI_4_SURG_COMP_CTN",
                                 "PSI_6_IAT_PTX_CTN","PSI_7_CVCBI_CTN","PSI_8_POST_HIP_CTN","PSI_90_SAFETY_CTN")

df_Complications[Complications_Cols_To_Factor] <- lapply(df_Complications[Complications_Cols_To_Factor], factor)

summary(df_Complications[Complications_Cols_To_Factor])

sapply(df_Complications, function(x) (sum(is.na(x))/nrow(df_Complications)*100))


############################################
## 3.Complications_National
############################################

df_Complications_National<-Complications_National
df_Complications_National[ df_Complications_National == "Not Available" ] <- NA

sapply(df_Complications_National, function(x) (sum(is.na(x))/nrow(df_Complications_National)*100))

colnames(df_Complications_National)

df_Complications_National<-df_Complications_National[,-c(8:10)]



############################################
## 3.Complications_State
############################################

df_Complications_State<-Complications_State
df_Complications_State[ df_Complications_State == "Not Available" ] <- NA

sapply(df_Complications_State, function(x) (sum(is.na(x))/nrow(df_Complications_State)*100))

colnames(df_Complications_State)

df_Complications_State<-df_Complications_State[,-c(8:10)]


############################################
## 3.Complications_State
############################################

df_Complications_State<-Complications_State
df_Complications_State[ df_Complications_State == "Not Available" ] <- NA

sapply(df_Complications_State, function(x) (sum(is.na(x))/nrow(df_Complications_State)*100))

colnames(df_Complications_State)

df_Complications_State<-df_Complications_State[,-c(8:10)]


############################################
## 3.Complications_Hospital
############################################

df_Complications_Hospital<-Complications_Hospital
df_Complications_Hospital[ df_Complications_Hospital == "Not Available" ] <- NA

sapply(df_Complications_Hospital, function(x) (sum(is.na(x))/nrow(df_Complications_Hospital)*100))

colnames(df_Complications_Hospital)

df_Complications_Hospital<-df_Complications_Hospital[,-c(2:4,6:9,16:18)]




############################################
## xx Ambulatory_Surgical_Measures_Facility
############################################



df_ASMF <- Ambulatory_Surgical_Measures_Facility

df_ASMF[ df_ASMF == "Not Available" ] <- NA
sapply(df_ASMF, function(x) (sum(is.na(x))/nrow(df_ASMF)*100))


## There are footnotes columns with maximum NAs needs to be removed, also Date columns are irrelevent 

ASMF_Cols_To_Remove<- c("ASC10_Footnote","ASC9_Footnote","ASC8_Footnote","ASC7_Footnote","ASC6_Footnote","ASC5_Footnote",
                        "ASC4_Footnote","ASC3_Footnote","ASC2_Footnote","ASC1_Footnote",
                        "ASC_1_5_Encounter_Start_Date","ASC_1_5_Encounter_End_Date","ASC_6_7_Encounter_Start_Date",
                        "ASC_6_7_Encounter_End_Date","ASC_8_Encounter_Date","ASC_9_10_Encounter_Start_Date","ASC_9_10_Encounter_End_Date")


df_ASMF<-df_ASMF[,-which(names(df_ASMF) %in% ASMF_Cols_To_Remove)]

sapply(df_ASMF, function(x) (sum(is.na(x))/nrow(df_ASMF)*100))


#########IMPUTATION

###1.1 ProviderID

## checking the provider ids with NAs 

nrow(df_ASMF[which(is.na(df_ASMF$Provider_ID)),])

summary(df_ASMF[which(is.na(df_ASMF$Provider_ID)),])

## If we see the other variables with these provider Ids , most are NAs so w




##Now we have to impute NA values with average and median values given as state and National Level

## But if we look into the data It is clear that 2013 having the more NAs, so let's seperate these two before impute

df_ASMF_2014<- df_ASMF[ which(df_ASMF$Year == "2014"), ]
sapply(df_ASMF_2014, function(x) (sum(is.na(x))/nrow(df_ASMF_2014)*100))














