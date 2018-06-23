library(tidyverse)
library(plyr)



library(dplyr)

library(tidyr)

library(stringr)

library(ggplot2)

library(lubridate)


###########################################################################################################################################
############################################### Varad ########################################################################
###########################################################################################################################################

#------------------------------------COMPLICATIONS START------------------------------------#

# COMPLICATIONS HOSPITAL LEVEL CLEANING AND GROUPING #
# READ Complications_Hospital #
Complications_Hospital <- read.csv("Complications - Hospital.csv",stringsAsFactors = FALSE)
View(Complications_Hospital)

# CHECKING FOR NA'S #
nrow(Complications_Hospital[which(is.na(Complications_Hospital$Compared.to.National)),]) #0
nrow(Complications_Hospital[which(is.na(Complications_Hospital$Denominator)),]) #0
nrow(Complications_Hospital[which(is.na(Complications_Hospital$Score)),]) #0

# CHECKING FOR "Not Available" #
nrow(Complications_Hospital[which(Complications_Hospital$Compared.to.National ==
                                    "Not Available"), ]) #18410
nrow(Complications_Hospital[which(Complications_Hospital$Denominator ==
                                    "Not Available"), ]) #25098
nrow(Complications_Hospital[which(Complications_Hospital$Score ==
                                    "Not Available"), ]) #21874

# CHANGING "Not Available" TO NA #
Complications_Hospital[which(Complications_Hospital$Compared.to.National ==
                               "Not Available"), "Compared.to.National"]<- NA
Complications_Hospital[which(Complications_Hospital$Denominator ==
                               "Not Available"), "Denominator"]<- NA
Complications_Hospital[which(Complications_Hospital$Score ==
                               "Not Available"), "Score"]<- NA

# CHECKING FOR DUPLICATES #
nrow(Complications_Hospital)
nrow(unique(Complications_Hospital))

# TRANSPOSING THE MAIN VARIABLES Compared.to.National, Denominator AND Score #
Compared.to.National <-
  spread(Complications_Hospital[, c("Provider.ID", "Measure.ID", "Compared.to.National")],
         Measure.ID,
         Compared.to.National)

colnames(Compared.to.National)[-1]<-paste(colnames(Compared.to.National)[-1],"_CTN",sep = "")

Denominator <-
  spread(Complications_Hospital[, c("Provider.ID", "Measure.ID", "Denominator")],
         Measure.ID,
         Denominator)

colnames(Denominator)[-1]<-paste(colnames(Denominator)[-1],"_DEN",sep = "")

Score <-
  spread(Complications_Hospital[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score)[-1]<-paste(colnames(Score)[-1],"_SCO",sep = "")

# GETTING THE UNIQUE DATA RELATED TO HOSPITALS #
hospital_data<-unique(Complications_Hospital[,c(1,2,3,4,5,6,7,8)])

# JOINING hospital_data WITH Compared.to.National, Denominator AND Score #
final_complications <-
  join_all(list(hospital_data, Compared.to.National, Denominator, Score), by = "Provider.ID")
# COMPLICATIONS HOSPITAL LEVEL CLEANING AND GROUPING #

# COMPLICATIONS STATE LEVEL CLEANING AND GROUPING #
# READ Complications_State #
Complications_State <- read.csv("Complications - State.csv",stringsAsFactors = FALSE)
View(Complications_State)

# CHECKING FOR "Not Available" #
nrow(Complications_State[which(Complications_State$Number.of.Hospitals.Worse ==
                                 "Not Available"), ]) #61
nrow(Complications_State[which(Complications_State$Number.of.Hospitals.Same ==
                                 "Not Available"), ]) #61
nrow(Complications_State[which(Complications_State$Number.of.Hospitals.Better ==
                                 "Not Available"), ]) #61
nrow(Complications_State[which(Complications_State$Number.of.Hospitals.Too.Few ==
                                 "Not Available"), ]) #111

# CHANGING "Not Available" TO NA #
Complications_State[which(Complications_State$Number.of.Hospitals.Worse ==
                            "Not Available"), "Number.of.Hospitals.Worse"]<- NA
Complications_State[which(Complications_State$Number.of.Hospitals.Same ==
                            "Not Available"), "Number.of.Hospitals.Same"]<- NA
Complications_State[which(Complications_State$Number.of.Hospitals.Better ==
                            "Not Available"), "Number.of.Hospitals.Better"]<- NA
Complications_State[which(Complications_State$Number.of.Hospitals.Too.Few ==
                            "Not Available"), "Number.of.Hospitals.Worse"]<- NA


# TRANSPOSING THE MAIN VARIABLES Worse, Same, Better AND Too.Few #
Worse_STA <-
  spread(Complications_State[, c("State", "Measure.ID", "Number.of.Hospitals.Worse")],
         Measure.ID,
         Number.of.Hospitals.Worse)

colnames(Worse_STA)[-1]<-paste(colnames(Worse_STA)[-1],"_WOR_STA",sep = "")

Same_STA <-
  spread(Complications_State[, c("State", "Measure.ID", "Number.of.Hospitals.Same")],
         Measure.ID,
         Number.of.Hospitals.Same)

colnames(Same_STA)[-1]<-paste(colnames(Same_STA)[-1],"_SAM_STA",sep = "")

Better_STA <-
  spread(Complications_State[, c("State", "Measure.ID", "Number.of.Hospitals.Better")],
         Measure.ID,
         Number.of.Hospitals.Better)

colnames(Better_STA)[-1]<-paste(colnames(Better_STA)[-1],"_BET_STA",sep = "")

Too.Few_STA <-
  spread(Complications_State[, c("State", "Measure.ID", "Number.of.Hospitals.Too.Few")],
         Measure.ID,
         Number.of.Hospitals.Too.Few)

colnames(Too.Few_STA)[-1]<-paste(colnames(Too.Few_STA)[-1],"_TOF_STA",sep = "")

# JOINING STATE DATA WITH HOSPITAL DATA #
final_complications<-join_all(list(final_complications, Worse_STA, Same_STA, Better_STA, Too.Few_STA), by = "State")
# COMPLICATIONS STATE LEVEL CLEANING AND GROUPING #

# COMPLICATIONS NATIONAL LEVEL CLEANING AND GROUPING #
# READ Complications_National #
Complications_National <- read.csv("Complications - National.csv",stringsAsFactors = FALSE)
View(Complications_National)

# CHECKING FOR "Not Available" #
nrow(Complications_National[which(Complications_National$National.Rate ==
                                    "Not Available"), ]) #0
nrow(Complications_National[which(Complications_National$Number.of.Hospitals.Worse ==
                                    "Not Available"), ]) #0
nrow(Complications_National[which(Complications_National$Number.of.Hospitals.Same ==
                                    "Not Available"), ]) #0
nrow(Complications_National[which(Complications_National$Number.of.Hospitals.Better ==
                                    "Not Available"), ]) #0
nrow(Complications_National[which(Complications_National$Number.of.Hospitals.Too.Few ==
                                    "Not Available"), ]) #0

# TRANSPOSING THE MAIN VARIABLES Rate, Worse, Same, Better AND Too.Few #
Rate_NAT <-
  spread(Complications_National[, c("Measure.ID", "National.Rate")],
         Measure.ID,
         National.Rate)

colnames(Rate_NAT)<-paste(colnames(Rate_NAT),"_RAT_NAT",sep = "")

Worse_NAT <-
  spread(Complications_National[, c("Measure.ID", "Number.of.Hospitals.Worse")],
         Measure.ID,
         Number.of.Hospitals.Worse)

colnames(Worse_NAT)<-paste(colnames(Worse_NAT),"_WOR_NAT",sep = "")

Same_NAT <-
  spread(Complications_National[, c("Measure.ID", "Number.of.Hospitals.Same")],
         Measure.ID,
         Number.of.Hospitals.Same)

colnames(Same_NAT)<-paste(colnames(Same_NAT),"_SAM_NAT",sep = "")

Better_NAT <-
  spread(Complications_National[, c("Measure.ID", "Number.of.Hospitals.Better")],
         Measure.ID,
         Number.of.Hospitals.Better)

colnames(Better_NAT)<-paste(colnames(Better_NAT),"_BET_NAT",sep = "")

Too.Few_NAT <-
  spread(Complications_National[, c("Measure.ID", "Number.of.Hospitals.Too.Few")],
         Measure.ID,
         Number.of.Hospitals.Too.Few)

colnames(Too.Few_NAT)<-paste(colnames(Too.Few_NAT),"_TOF_NAT",sep = "")

# JOINING NATIONAL DATA WITH FINAL DATA #
final_complications <-
  cbind(final_complications,
        Rate_NAT,
        Worse_NAT,
        Same_NAT,
        Better_NAT,
        Too.Few_NAT)
# COMPLICATIONS NATIONAL LEVEL CLEANING AND GROUPING #

# REMOVING THE COLUMNS WITH ENTIRE NA OR BLANKS VALUES #
final_complications[final_complications==""]<-NA

grep(nrow(final_complications),
     sapply(final_complications, function(x)
       sum(is.na(x))))# COLUMNS 30 AND 52 ARE COMPLETELY BLANK

colnames(final_complications)[grep(
  nrow(final_complications),
  sapply(final_complications, function(x)
    sum(is.na(x)))
)]

final_complications <-
  final_complications[,-c(grep(
    nrow(final_complications),
    sapply(final_complications, function(x)
      sum(is.na(x)))
  ))]

# CHANGING final_complications INTO Complications #
Complications<-final_complications

# WRITE FILE COMPLICATIONS #
write.csv(Complications,file = "Complications.csv")

#------------------------------------COMPLICATIONS END------------------------------------#

#-----------------------MORTALITY AND READMISSION START------------------------------------#

# READMISSION AND DEATHS HOSPITAL LEVEL CLEANING AND GROUPING #
# READ Readmissions_and_Deaths_Hospital #
Readmissions_and_Deaths_Hospital <- read.csv("Readmissions and Deaths - Hospital.csv",stringsAsFactors = FALSE)
View(Readmissions_and_Deaths_Hospital)

# CHECKING FOR NA'S #
nrow(Readmissions_and_Deaths_Hospital[which(is.na(Readmissions_and_Deaths_Hospital$Compared.to.National)),]) #0
nrow(Readmissions_and_Deaths_Hospital[which(is.na(Readmissions_and_Deaths_Hospital$Denominator)),]) #0
nrow(Readmissions_and_Deaths_Hospital[which(is.na(Readmissions_and_Deaths_Hospital$Score)),]) #0

# CHECKING FOR "Not Available" #
nrow(Readmissions_and_Deaths_Hospital[which(Readmissions_and_Deaths_Hospital$Compared.to.National ==
                                              "Not Available"), ]) #14550
nrow(Readmissions_and_Deaths_Hospital[which(Readmissions_and_Deaths_Hospital$Denominator ==
                                              "Not Available"), ]) #25742
nrow(Readmissions_and_Deaths_Hospital[which(Readmissions_and_Deaths_Hospital$Score ==
                                              "Not Available"), ]) #25742

# CHANGING "Not Available" TO NA #
Readmissions_and_Deaths_Hospital[which(Readmissions_and_Deaths_Hospital$Compared.to.National ==
                                         "Not Available"), "Compared.to.National"]<- NA
Readmissions_and_Deaths_Hospital[which(Readmissions_and_Deaths_Hospital$Denominator ==
                                         "Not Available"), "Denominator"]<- NA
Readmissions_and_Deaths_Hospital[which(Readmissions_and_Deaths_Hospital$Score ==
                                         "Not Available"), "Score"]<- NA

# CHECKING FOR DUPLICATES #
nrow(Readmissions_and_Deaths_Hospital)
nrow(unique(Readmissions_and_Deaths_Hospital))

# TRANSPOSING THE MAIN VARIABLES Compared.to.National, Denominator AND Score #
Compared.to.National <-
  spread(Readmissions_and_Deaths_Hospital[, c("Provider.ID", "Measure.ID", "Compared.to.National")],
         Measure.ID,
         Compared.to.National)

colnames(Compared.to.National)[-1]<-paste(colnames(Compared.to.National)[-1],"_CTN",sep = "")

Denominator <-
  spread(Readmissions_and_Deaths_Hospital[, c("Provider.ID", "Measure.ID", "Denominator")],
         Measure.ID,
         Denominator)

colnames(Denominator)[-1]<-paste(colnames(Denominator)[-1],"_DEN",sep = "")

Score <-
  spread(Readmissions_and_Deaths_Hospital[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score)[-1]<-paste(colnames(Score)[-1],"_SCO",sep = "")

# GETTING THE UNIQUE DATA RELATED TO HOSPITALS #
hospital_data<-unique(Readmissions_and_Deaths_Hospital[,c(1,2,3,4,5,6,7,8)])

# JOINING hospital_data WITH Compared.to.National, Denominator AND Score #
final_readmi_deaths <-
  join_all(list(hospital_data, Compared.to.National, Denominator, Score), by = "Provider.ID")
# READMISSION AND DEATHS HOSPITAL LEVEL CLEANING AND GROUPING #

# READMISSION AND DEATHS STATE LEVEL CLEANING AND GROUPING #
# READ Readmissions_and_Deaths_State #
Readmissions_and_Deaths_State <- read.csv("Readmissions and Deaths - State.csv",stringsAsFactors = FALSE)
View(Readmissions_and_Deaths_State)

# CHECKING FOR "Not Available" #
nrow(Readmissions_and_Deaths_State[which(Readmissions_and_Deaths_State$Number.of.Hospitals.Worse ==
                                           "Not Available"), ]) #14
nrow(Readmissions_and_Deaths_State[which(Readmissions_and_Deaths_State$Number.of.Hospitals.Same ==
                                           "Not Available"), ]) #14
nrow(Readmissions_and_Deaths_State[which(Readmissions_and_Deaths_State$Number.of.Hospitals.Better ==
                                           "Not Available"), ]) #14
nrow(Readmissions_and_Deaths_State[which(Readmissions_and_Deaths_State$Number.of.Hospitals.Too.Few ==
                                           "Not Available"), ]) #14

# CHANGING "Not Available" TO NA #
Readmissions_and_Deaths_State[which(Readmissions_and_Deaths_State$Number.of.Hospitals.Worse ==
                                      "Not Available"), "Number.of.Hospitals.Worse"]<- NA
Readmissions_and_Deaths_State[which(Readmissions_and_Deaths_State$Number.of.Hospitals.Same ==
                                      "Not Available"), "Number.of.Hospitals.Same"]<- NA
Readmissions_and_Deaths_State[which(Readmissions_and_Deaths_State$Number.of.Hospitals.Better ==
                                      "Not Available"), "Number.of.Hospitals.Better"]<- NA
Readmissions_and_Deaths_State[which(Readmissions_and_Deaths_State$Number.of.Hospitals.Too.Few ==
                                      "Not Available"), "Number.of.Hospitals.Worse"]<- NA


# TRANSPOSING THE MAIN VARIABLES Worse, Same, Better AND Too.Few #
Worse_STA <-
  spread(Readmissions_and_Deaths_State[, c("State", "Measure.ID", "Number.of.Hospitals.Worse")],
         Measure.ID,
         Number.of.Hospitals.Worse)

colnames(Worse_STA)[-1]<-paste(colnames(Worse_STA)[-1],"_WOR_STA",sep = "")

Same_STA <-
  spread(Readmissions_and_Deaths_State[, c("State", "Measure.ID", "Number.of.Hospitals.Same")],
         Measure.ID,
         Number.of.Hospitals.Same)

colnames(Same_STA)[-1]<-paste(colnames(Same_STA)[-1],"_SAM_STA",sep = "")

Better_STA <-
  spread(Readmissions_and_Deaths_State[, c("State", "Measure.ID", "Number.of.Hospitals.Better")],
         Measure.ID,
         Number.of.Hospitals.Better)

colnames(Better_STA)[-1]<-paste(colnames(Better_STA)[-1],"_BET_STA",sep = "")

Too.Few_STA <-
  spread(Readmissions_and_Deaths_State[, c("State", "Measure.ID", "Number.of.Hospitals.Too.Few")],
         Measure.ID,
         Number.of.Hospitals.Too.Few)

colnames(Too.Few_STA)[-1]<-paste(colnames(Too.Few_STA)[-1],"_TOF_STA",sep = "")

# JOINING STATE DATA WITH HOSPITAL DATA #
final_readmi_deaths<-join_all(list(final_readmi_deaths, Worse_STA, Same_STA, Better_STA, Too.Few_STA), by = "State")
# READMISSION AND DEATHS STATE LEVEL CLEANING AND GROUPING #

# READMISSION AND DEATHS NATIONAL LEVEL CLEANING AND GROUPING #
# READ Readmissions_and_Deaths_National #
Readmissions_and_Deaths_National <- read.csv("Readmissions and Deaths - National.csv",stringsAsFactors = FALSE)
View(Readmissions_and_Deaths_National)

# CHECKING FOR "Not Available" #
nrow(Readmissions_and_Deaths_National[which(Readmissions_and_Deaths_National$National.Rate ==
                                              "Not Available"), ]) #0
nrow(Readmissions_and_Deaths_National[which(Readmissions_and_Deaths_National$Number.of.Hospitals.Worse ==
                                              "Not Available"), ]) #0
nrow(Readmissions_and_Deaths_National[which(Readmissions_and_Deaths_National$Number.of.Hospitals.Same ==
                                              "Not Available"), ]) #0
nrow(Readmissions_and_Deaths_National[which(Readmissions_and_Deaths_National$Number.of.Hospitals.Better ==
                                              "Not Available"), ]) #0
nrow(Readmissions_and_Deaths_National[which(Readmissions_and_Deaths_National$Number.of.Hospitals.Too.Few ==
                                              "Not Available"), ]) #0

# TRANSPOSING THE MAIN VARIABLES Rate, Worse, Same, Better AND Too.Few #
Rate_NAT <-
  spread(Readmissions_and_Deaths_National[, c("Measure.ID", "National.Rate")],
         Measure.ID,
         National.Rate)

colnames(Rate_NAT)<-paste(colnames(Rate_NAT),"_RAT_NAT",sep = "")

Worse_NAT <-
  spread(Readmissions_and_Deaths_National[, c("Measure.ID", "Number.of.Hospitals.Worse")],
         Measure.ID,
         Number.of.Hospitals.Worse)

colnames(Worse_NAT)<-paste(colnames(Worse_NAT),"_WOR_NAT",sep = "")

Same_NAT <-
  spread(Readmissions_and_Deaths_National[, c("Measure.ID", "Number.of.Hospitals.Same")],
         Measure.ID,
         Number.of.Hospitals.Same)

colnames(Same_NAT)<-paste(colnames(Same_NAT),"_SAM_NAT",sep = "")

Better_NAT <-
  spread(Readmissions_and_Deaths_National[, c("Measure.ID", "Number.of.Hospitals.Better")],
         Measure.ID,
         Number.of.Hospitals.Better)

colnames(Better_NAT)<-paste(colnames(Better_NAT),"_BET_NAT",sep = "")

Too.Few_NAT <-
  spread(Readmissions_and_Deaths_National[, c("Measure.ID", "Number.of.Hospitals.Too.Few")],
         Measure.ID,
         Number.of.Hospitals.Too.Few)

colnames(Too.Few_NAT)<-paste(colnames(Too.Few_NAT),"_TOF_NAT",sep = "")

# JOINING NATIONAL DATA WITH FINAL DATA #
final_readmi_deaths <-
  cbind(final_readmi_deaths,
        Rate_NAT,
        Worse_NAT,
        Same_NAT,
        Better_NAT,
        Too.Few_NAT)
# READMISSION AND DEATHS NATIONAL LEVEL CLEANING AND GROUPING #

# REMOVING THE COLUMNS WITH ENTIRE NA OR BLANKS VALUES #
final_readmi_deaths[final_readmi_deaths==""]<-NA

grep(nrow(final_readmi_deaths),
     sapply(final_readmi_deaths, function(x)
       sum(is.na(x))))# NONE OF THE COLUMNS ARE COMPLETELY BLANK

# SEPERATING final_readmi_deaths INTO Mortality AND Readmission #
Mortality <-
  final_readmi_deaths[, c(1:8, grep("MORT", colnames(final_readmi_deaths)))]

Readmission <-
  final_readmi_deaths[, c(1:8, grep("READM", colnames(final_readmi_deaths)))]

# FINDING FOR DEATHS OR READMISSIONS IN COMPLICATIONS DATA #
Complications_National[grep("Death",Complications_National$Measure.Name),"Measure.ID"] #PSI_4_SURG_COMP
Complications_National[grep("Read",Complications_National$Measure.Name),"Measure.ID"]

final_complications_death <-
  final_complications[, c(1:8, grep("PSI_4_SURG_COMP", colnames(final_complications)))]

final_complications_without_death<-final_complications[, -c(2:8, grep("PSI_4_SURG_COMP", colnames(final_complications)))]

# JOINING PSI_4_SURG_COMP TO Mortality
Mortality<-join_all(list(Mortality, final_complications_death), by = "Provider.ID")

# WRITE FILE MORTALITY AND READMISSION #
write.csv(Mortality,file = "Mortality.csv")
write.csv(Readmission,file = "Readmission.csv")

#-----------------------MORTALITY AND READMISSION END------------------------------------#

#-----------------------SAFETY OF CARE START------------------------------------#

# HAI HOSPITAL LEVEL CLEANING AND GROUPING #
# READ HAI_Hospital #
HAI_Hospital <- read.csv("Healthcare Associated Infections - Hospital.csv",stringsAsFactors = FALSE)
View(HAI_Hospital)

# CHECKING FOR NA'S #
nrow(HAI_Hospital[which(is.na(HAI_Hospital$Compared.to.National)),]) #0
nrow(HAI_Hospital[which(is.na(HAI_Hospital$Score)),]) #0

# CHECKING FOR "Not Available" #
nrow(HAI_Hospital[which(HAI_Hospital$Compared.to.National ==
                          "Not Available"), ]) #21010
nrow(HAI_Hospital[which(HAI_Hospital$Score ==
                          "Not Available"), ]) #101886

# CHANGING "Not Available" TO NA #
HAI_Hospital[which(HAI_Hospital$Compared.to.National ==
                     "Not Available"), "Compared.to.National"]<- NA
HAI_Hospital[which(HAI_Hospital$Score ==
                     "Not Available"), "Score"]<- NA

# CHECKING FOR DUPLICATES #
nrow(HAI_Hospital)
nrow(unique(HAI_Hospital))

# TRANSPOSING THE MAIN VARIABLES Compared.to.National, Denominator AND Score #
Compared.to.National <-
  spread(HAI_Hospital[, c("Provider.ID", "Measure.ID", "Compared.to.National")],
         Measure.ID,
         Compared.to.National)

colnames(Compared.to.National)[-1]<-paste(colnames(Compared.to.National)[-1],"_CTN",sep = "")

Score <-
  spread(HAI_Hospital[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score)[-1]<-paste(colnames(Score)[-1],"_SCO",sep = "")

# GETTING THE UNIQUE DATA RELATED TO HOSPITALS #
hospital_data<-unique(HAI_Hospital[,c(1,2,3,4,5,6,7,8)])

# JOINING hospital_data WITH Compared.to.National, Denominator AND Score #
final_hai <-
  join_all(list(hospital_data, Compared.to.National, Score), by = "Provider.ID")
# HAI HOSPITAL LEVEL CLEANING AND GROUPING #

# HAI STATE LEVEL CLEANING AND GROUPING #
# READ HAI_State #
HAI_State <- read.csv("Healthcare Associated Infections - State.csv",stringsAsFactors = FALSE)
View(HAI_State)

# CHECKING FOR "Not Available" #
nrow(HAI_State[which(HAI_State$Score ==
                       "Not Available"), ]) #73

# CHANGING "Not Available" TO NA #
HAI_State[which(HAI_State$Score ==
                  "Not Available"), "Score"]<- NA

# TRANSPOSING THE MAIN VARIABLES Worse, Same, Better AND Too.Few #
Score_STA <-
  spread(HAI_State[, c("State", "Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score_STA)[-1]<-paste(colnames(Score_STA)[-1],"_SCO_STA",sep = "")

# JOINING STATE DATA WITH HOSPITAL DATA #
final_hai<-join_all(list(final_hai, Score_STA), by = "State")
# HAI STATE LEVEL CLEANING AND GROUPING #

# HAI NATIONAL LEVEL CLEANING AND GROUPING #
# READ HAI_National #
HAI_National <- read.csv("Healthcare Associated Infections - National.csv",stringsAsFactors = FALSE)
View(HAI_National)

# CHECKING FOR "Not Available" #
nrow(HAI_National[which(HAI_National$Score ==
                          "Not Available"), ]) #0

# TRANSPOSING THE MAIN VARIABLES Rate, Worse, Same, Better AND Too.Few #
Score_NAT <-
  spread(HAI_National[, c("Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score_NAT)<-paste(colnames(Score_NAT),"_SCO_NAT",sep = "")

# JOINING NATIONAL DATA WITH FINAL DATA #
final_hai <-
  cbind(final_hai,
        Score_NAT)
# HAI NATIONAL LEVEL CLEANING AND GROUPING #

# REMOVING THE COLUMNS WITH ENTIRE NA VALUES/BLANKS
final_hai[final_hai==""]<-NA

grep(nrow(final_hai), sapply(final_hai, function(x)
  sum(is.na(x)))) # 40 COLUMNS ARE COMPLETELY BLANK/NA

final_hai<-final_hai[, -c(grep(nrow(final_hai),sapply(final_hai, function(x) sum(is.na(x)))))]

# JOINING final_hai AND final_complications_without_death INTO SOC i.e Safety Of Care #
SOC<-join_all(list(final_hai,final_complications_without_death),by = "Provider.ID")

# WRITE FILE Safety Of Care #
write.csv(SOC,file = "Safety Of Care.csv")


###########################################################################################################################################
############################################### Sujeet ########################################################################
###########################################################################################################################################

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



###########################################################################################################################################
############################################### Naresh ########################################################################
###########################################################################################################################################



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

