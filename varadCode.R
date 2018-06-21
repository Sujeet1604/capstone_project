library(tidyverse)
library(plyr)
library(stringr)

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

# TRANSPOSING THE MAIN VARIABLES Compared.to.National AND Score #
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

# TRANSPOSING THE MAIN VARIABLES Score #
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

# TRANSPOSING THE MAIN VARIABLES Score #
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

#-----------------------SAFETY OF CARE END------------------------------------#

#-----------------------PATIENT EXPERIENCE START------------------------------------#
# HCAHPS HOSPITAL LEVEL CLEANING AND GROUPING #
# READ HCAHPS_Hospital #
HCAHPS_Hospital <- read.csv("HCAHPS - Hospital.csv",stringsAsFactors = FALSE)
View(HCAHPS_Hospital)

# CHECKING FOR NA'S #
nrow(HCAHPS_Hospital[which(is.na(HCAHPS_Hospital$Patient.Survey.Star.Rating)),]) #0
nrow(HCAHPS_Hospital[which(is.na(HCAHPS_Hospital$HCAHPS.Answer.Percent)),]) #0
nrow(HCAHPS_Hospital[which(is.na(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value)),]) #0
nrow(HCAHPS_Hospital[which(is.na(HCAHPS_Hospital$Number.of.Completed.Surveys)),]) #0

# CHECKING FOR "Not Available" AND "Not Applicable" #
nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Patient.Survey.Star.Rating ==
                             "Not Available"), ]) #15720
nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Patient.Survey.Star.Rating ==
                             "Not Applicable"), ]) #207174

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Answer.Percent ==
                             "Not Available"), ]) #18648
nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Answer.Percent ==
                             "Not Applicable"), ]) #110814

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value ==
                             "Not Available"), ]) #14410
nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value ==
                             "Not Applicable"), ]) #211992

nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Number.of.Completed.Surveys ==
                             "Not Available"), ]) #31735
nrow(HCAHPS_Hospital[which(HCAHPS_Hospital$Number.of.Completed.Surveys ==
                             "Not Applicable"), ]) #0

# CHANGING "Not Available" AND "Not Applicable" TO NA #
HCAHPS_Hospital[which(HCAHPS_Hospital$Patient.Survey.Star.Rating ==
                        "Not Available"), "Patient.Survey.Star.Rating"]<- NA
HCAHPS_Hospital[which(HCAHPS_Hospital$Patient.Survey.Star.Rating ==
                        "Not Applicable"), "Patient.Survey.Star.Rating"]<- NA

HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Answer.Percent ==
                        "Not Available"), "HCAHPS.Answer.Percent"]<- NA
HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Answer.Percent ==
                        "Not Applicable"), "HCAHPS.Answer.Percent"]<- NA

HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value ==
                        "Not Available"), "HCAHPS.Linear.Mean.Value"]<- NA
HCAHPS_Hospital[which(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value ==
                        "Not Applicable"), "HCAHPS.Linear.Mean.Value"]<- NA

HCAHPS_Hospital[which(HCAHPS_Hospital$Number.of.Completed.Surveys ==
                        "Not Available"), "Number.of.Completed.Surveys"]<- NA

# CHECKING FOR DUPLICATES #
nrow(HCAHPS_Hospital)
nrow(unique(HCAHPS_Hospital))

# TRANSPOSING THE MAIN VARIABLES Patient.Survey.Star.Rating, HCAHPS.Answer.Percent, HCAHPS.Linear.Mean.Value AND Number.of.Completed.Surveys #
Patient.Survey.Star.Rating <-
  spread(HCAHPS_Hospital[, c("Provider.ID", "HCAHPS.Measure.ID", "Patient.Survey.Star.Rating")],
         HCAHPS.Measure.ID,
         Patient.Survey.Star.Rating)

colnames(Patient.Survey.Star.Rating)[-1]<-paste(colnames(Patient.Survey.Star.Rating)[-1],"_SSR",sep = "")

HCAHPS.Answer.Percent <-
  spread(HCAHPS_Hospital[, c("Provider.ID", "HCAHPS.Measure.ID", "HCAHPS.Answer.Percent")],
         HCAHPS.Measure.ID,
         HCAHPS.Answer.Percent)

colnames(HCAHPS.Answer.Percent)[-1]<-paste(colnames(HCAHPS.Answer.Percent)[-1],"_APE",sep = "")

HCAHPS.Linear.Mean.Value <-
  spread(HCAHPS_Hospital[, c("Provider.ID", "HCAHPS.Measure.ID", "HCAHPS.Linear.Mean.Value")],
         HCAHPS.Measure.ID,
         HCAHPS.Linear.Mean.Value)

colnames(HCAHPS.Linear.Mean.Value)[-1]<-paste(colnames(HCAHPS.Linear.Mean.Value)[-1],"_LMV",sep = "")

Number.of.Completed.Surveys <-
  spread(HCAHPS_Hospital[, c("Provider.ID", "HCAHPS.Measure.ID", "Number.of.Completed.Surveys")],
         HCAHPS.Measure.ID,
         Number.of.Completed.Surveys)

colnames(Number.of.Completed.Surveys)[-1]<-paste(colnames(Number.of.Completed.Surveys)[-1],"_NCS",sep = "")

# REMOVING COLUMNS WHICH ARE COMPLETELY BLANKS/NA FROM VARIABLES #
Patient.Survey.Star.Rating[Patient.Survey.Star.Rating==""]<-NA
HCAHPS.Answer.Percent[HCAHPS.Answer.Percent==""]<-NA
HCAHPS.Linear.Mean.Value[HCAHPS.Linear.Mean.Value==""]<-NA
Number.of.Completed.Surveys[Number.of.Completed.Surveys==""]<-NA

length(grep(nrow(Patient.Survey.Star.Rating),
            sapply(Patient.Survey.Star.Rating, function(x)
              sum(is.na(x)))))# 43 OF THE COLUMNS ARE COMPLETELY BLANK

length(grep(nrow(HCAHPS.Answer.Percent),
            sapply(HCAHPS.Answer.Percent, function(x)
              sum(is.na(x)))))# 23 OF THE COLUMNS ARE COMPLETELY BLANK

length(grep(nrow(HCAHPS.Linear.Mean.Value),
            sapply(HCAHPS.Linear.Mean.Value, function(x)
              sum(is.na(x)))))# 44 OF THE COLUMNS ARE COMPLETELY BLANK

length(grep(nrow(Number.of.Completed.Surveys),
            sapply(Number.of.Completed.Surveys, function(x)
              sum(is.na(x)))))# NONE OF THE COLUMNS ARE COMPLETELY BLANK

# MAJORITY OF THE COLUMNS IN Patient.Survey.Star.Rating AND HCAHPS.Linear.Mean.Value #
# ARE BLANK. HENCE WE WILL USE ONLY HCAHPS.Answer.Percent AND Number.of.Completed.Surveys #

blanks_columns <- grep(nrow(HCAHPS.Answer.Percent),
                       sapply(HCAHPS.Answer.Percent, function(x)
                         sum(is.na(x))))# 23 OF THE COLUMNS ARE COMPLETELY BLANK

Number.of.Completed.Surveys <-
  Number.of.Completed.Surveys[,-c(blanks_columns)]
HCAHPS.Answer.Percent <-
  HCAHPS.Answer.Percent[,-c(blanks_columns)]

# GETTING THE UNIQUE DATA RELATED TO HOSPITALS #
hospital_data<-unique(HCAHPS_Hospital[,c(1,2,3,4,5,6,7,8)])

# JOINING hospital_data WITH HCAHPS.Linear.Mean.Value AND Number.of.Completed.Surveys #
final_hcahps<-join_all(list(hospital_data, HCAHPS.Answer.Percent, Number.of.Completed.Surveys), by = "Provider.ID")
# HCAHPS HOSPITAL LEVEL CLEANING AND GROUPING #

# HCAHPS STATE LEVEL CLEANING AND GROUPING #
# READ HCAHPS_State #
HCAHPS_State <- read.csv("HCAHPS - State.csv",stringsAsFactors = FALSE)
View(HCAHPS_State)

# CHECKING FOR NA'S #
nrow(HCAHPS_State[which(is.na(HCAHPS_State$HCAHPS.Answer.Percent)),]) #0

# CHECKING FOR "Not Available" #
nrow(HCAHPS_State[which(HCAHPS_State$HCAHPS.Answer.Percent ==
                          "Not Available"), ]) #96

# CHANGING "Not Available" TO NA #
HCAHPS_State[which(HCAHPS_State$HCAHPS.Answer.Percent ==
                     "Not Available"), "HCAHPS.Answer.Percent"]<- NA

# CHECKING FOR DUPLICATES #
nrow(HCAHPS_State)
nrow(unique(HCAHPS_State))

# TRANSPOSING THE MAIN VARIABLES HCAHPS.Answer.Percent #
HCAHPS.Answer.Percent_STA <-
  spread(HCAHPS_State[, c("State", "HCAHPS.Measure.ID", "HCAHPS.Answer.Percent")],
         HCAHPS.Measure.ID,
         HCAHPS.Answer.Percent)

colnames(HCAHPS.Answer.Percent_STA)[-1]<-paste(colnames(HCAHPS.Answer.Percent_STA)[-1],"_APE_STA",sep = "")

# JOINING final_hcahps WITH HCAHPS.Answer.Percent #
final_hcahps<-join_all(list(final_hcahps,HCAHPS.Answer.Percent_STA), by = "State")
# HCAHPS STATE LEVEL CLEANING AND GROUPING #

# HCAHPS NATIONAL LEVEL CLEANING AND GROUPING #
HCAHPS_National <- read.csv("HCAHPS - National.csv",stringsAsFactors = FALSE)
View(HCAHPS_National)

# CHECKING FOR NA'S #
nrow(HCAHPS_National[which(is.na(HCAHPS_National$HCAHPS.Answer.Percent)),]) #0

# CHECKING FOR "Not Available" #
nrow(HCAHPS_National[which(HCAHPS_National$HCAHPS.Answer.Percent ==
                             "Not Available"), ]) #0

# CHECKING FOR DUPLICATES #
nrow(HCAHPS_National)
nrow(unique(HCAHPS_National))

# TRANSPOSING THE MAIN VARIABLES HCAHPS.Answer.Percent #
HCAHPS.Answer.Percent_NAT <-
  spread(HCAHPS_National[, c("HCAHPS.Measure.ID", "HCAHPS.Answer.Percent")],
         HCAHPS.Measure.ID,
         HCAHPS.Answer.Percent)

colnames(HCAHPS.Answer.Percent_NAT)[-1]<-paste(colnames(HCAHPS.Answer.Percent_NAT)[-1],"_APE_NAT",sep = "")

# JOINING final_hcahps WITH HCAHPS.Answer.Percent #
final_hcahps<-cbind(final_hcahps,HCAHPS.Answer.Percent_NAT)
# HCAHPS NATIONAL LEVEL CLEANING AND GROUPING #

# CHECKING FOR BLANK COLUMNS
final_hcahps[final_hcahps==""]<-NA

grep(nrow(final_hcahps),
     sapply(final_hcahps, function(x)
       sum(is.na(x))))# NONE OF THE COLUMNS ARE COMPLETELY BLANK

# CREATE VARIABLE PE i.e Patient Expetience #
PE<-final_hcahps

# WRITE FILE Patient Experience #
write.csv(PE,file = "Patient Experience.csv")

#-----------------------PATIENT EXPERIENCE END------------------------------------#

#-----------------------Timely and Effective Care START------------------------------------#

# TEC HOSPITAL LEVEL CLEANING AND GROUPING #
# READ TEC_Hospital #
TEC_Hospital <- read.csv("Timely and Effective Care - Hospital.csv",stringsAsFactors = FALSE)
View(TEC_Hospital)

TEC_MEI_MES <- TEC_Hospital[, c(10, 11)]

time_measure_id_1 <- unique(TEC_MEI_MES[which(
  TEC_MEI_MES$Measure.Name %in% grep(
    "time",
    TEC_MEI_MES[, 2],
    ignore.case = TRUE,
    value = TRUE
  )
), 1])

time_measure_id_2 <- unique(TEC_MEI_MES[which(
  TEC_MEI_MES$Measure.Name %in% grep(
    "hour",
    TEC_MEI_MES[, 2],
    ignore.case = TRUE,
    value = TRUE
  )
), 1])

time_measure_id <- union(time_measure_id_1,time_measure_id_2)
remove(time_measure_id_1)
remove(time_measure_id_2)
View(time_measure_id)

# CHECKING FOR NA'S #
nrow(TEC_Hospital[which(is.na(TEC_Hospital$Score)),]) #0

# CHECKING FOR "Not Available" #
nrow(TEC_Hospital[which(TEC_Hospital$Score ==
                          "Not Available"), ]) #129024

# CHANGING "Not Available" TO NA #
TEC_Hospital[which(TEC_Hospital$Score ==
                     "Not Available"), "Score"]<- NA

# CHECKING FOR DUPLICATES #
nrow(TEC_Hospital)
nrow(unique(TEC_Hospital))

# TRANSPOSING THE MAIN VARIABLES Score #
Score <-
  spread(TEC_Hospital[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score)[-1]<-paste(colnames(Score)[-1],"_SCO",sep = "")

# GETTING THE UNIQUE DATA RELATED TO HOSPITALS #
hospital_data<-unique(TEC_Hospital[,c(1,2,3,4,5,6,7,8)])

# JOINING hospital_data WITH Compared.to.National, Denominator AND Score #
final_tec <-
  join_all(list(hospital_data, Score), by = "Provider.ID")

blanks_columns <- grep(nrow(final_tec),
                       sapply(final_tec, function(x)
                         sum(is.na(x))))# NONE OF THE COLUMNS ARE COMPLETELY BLANK

# SEPERATING TIMELINESS AND EFFECTIVENESS #
timeliness <- final_tec[, c(paste(time_measure_id, "_SCO", sep = ""))]
timeliness <- cbind(final_tec[,c(1:8)],timeliness)

effectiveness <- final_tec[, !(colnames(final_tec) %in% c(paste(time_measure_id, "_SCO", sep = "")))]
# TEC HOSPITAL LEVEL CLEANING AND GROUPING #

# TEC STATE LEVEL CLEANING AND GROUPING #
TEC_State <- read.csv("Timely and Effective Care - State.csv",stringsAsFactors = FALSE)

# CHECKING FOR NA'S #
nrow(TEC_State[which(is.na(TEC_State$Score)),]) #0

# CHECKING FOR "Not Available" #
nrow(TEC_State[which(TEC_State$Score ==
                          "Not Available"), ]) #289

# CHANGING "Not Available" TO NA #
TEC_State[which(TEC_State$Score ==
                     "Not Available"), "Score"]<- NA

# CHECKING FOR DUPLICATES #
nrow(TEC_State)
nrow(unique(TEC_State))

# TRANSPOSING THE MAIN VARIABLES Score #
Score_STA <-
  spread(TEC_State[, c("State", "Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score_STA)[-1]<-paste(colnames(Score_STA)[-1],"_SCO_STA",sep = "")

# JOINING hospital_data WITH Compared.to.National, Denominator AND Score #
final_tec <-
  join_all(list(final_tec, Score_STA), by = "State")

blanks_columns <- grep(nrow(final_tec),
                       sapply(final_tec, function(x)
                         sum(is.na(x))))# NONE OF THE COLUMNS ARE COMPLETELY BLANK

# SEPERATING TIMELINESS AND EFFECTIVENESS #
timeliness_sta <- final_tec[, (colnames(final_tec) %in% c(paste(time_measure_id, "_SCO_STA", sep = "")))]
timeliness <- cbind(final_tec[,c(1:15)],timeliness_sta)

effectiveness <- final_tec[, !(colnames(final_tec) %in% c(paste(time_measure_id, "_SCO_STA", sep = "")))]
# TEC STATE LEVEL CLEANING AND GROUPING #

# TEC NATIONAL LEVEL CLEANING AND GROUPING #
TEC_National <- read.csv("Timely and Effective Care - National.csv",stringsAsFactors = FALSE)

# CHECKING FOR NA'S #
nrow(TEC_National[which(is.na(TEC_National$Score)),]) #0

# CHECKING FOR "Not Available" #
nrow(TEC_National[which(TEC_National$Score ==
                          "Not Available"), ]) #0

# CHECKING FOR DUPLICATES #
nrow(TEC_National)
nrow(unique(TEC_National))

# TRANSPOSING THE MAIN VARIABLES Score #
Score_NAT <-
  spread(TEC_National[, c("Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score_NAT)[-1]<-paste(colnames(Score_NAT)[-1],"_SCO_NAT",sep = "")

# JOINING hospital_data WITH Score #
final_tec <-
  cbind(final_tec, Score_NAT)

blanks_columns <- grep(nrow(final_tec),
                       sapply(final_tec, function(x)
                         sum(is.na(x))))# NONE OF THE COLUMNS ARE COMPLETELY BLANK

# SEPERATING TIMELINESS AND EFFECTIVENESS #
timeliness_nat <- final_tec[, (colnames(final_tec) %in% c(paste(time_measure_id, "_SCO_NAT", sep = "")))]
timeliness <- cbind(final_tec[,c(1:22)],timeliness_nat)

effectiveness <- final_tec[, !(colnames(final_tec) %in% c(paste(time_measure_id, "_SCO_NAT", sep = "")))]
# TEC NATIONAL LEVEL CLEANING AND GROUPING #

# WRITE FILE Timeliness Of Care #
write.csv(timeliness,file = "Timeliness Of Care.csv")
write.csv(effectiveness,file = "Effectiveness Of Care.csv")

#-----------------------Timely and Effective Care END------------------------------------#

#-----------------------Medical Imaging Efficiency START------------------------------------#

# MIE HOSPITAL LEVEL CLEANING AND GROUPING #
# READ MIE_Hospital #
MIE_Hospital <- read.csv("Outpatient Imaging Efficiency - Hospital.csv",stringsAsFactors = FALSE)
View(MIE_Hospital)

# CHECKING FOR NA'S #
nrow(MIE_Hospital[which(is.na(MIE_Hospital$Score)),]) #0

# CHECKING FOR "Not Available" #
nrow(MIE_Hospital[which(MIE_Hospital$Score ==
                          "Not Available"), ]) #12595

# CHANGING "Not Available" TO NA #
MIE_Hospital[which(MIE_Hospital$Score ==
                     "Not Available"), "Score"]<- NA

# CHECKING FOR DUPLICATES #
nrow(MIE_Hospital)
nrow(unique(MIE_Hospital))

# TRANSPOSING THE MAIN VARIABLES Score #
Score <-
  spread(MIE_Hospital[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score)[-1]<-paste(colnames(Score)[-1],"_SCO",sep = "")

# GETTING THE UNIQUE DATA RELATED TO HOSPITALS #
hospital_data<-unique(MIE_Hospital[,c(1,2,3,4,5,6,7,8)])

# JOINING hospital_data WITH Compared.to.National, Denominator AND Score #
final_mie <-
  join_all(list(hospital_data, Score), by = "Provider.ID")

blanks_columns <- grep(nrow(final_mie),
                       sapply(final_mie, function(x)
                         sum(is.na(x))))# NONE OF THE COLUMNS ARE COMPLETELY BLANK
# MIE HOSPITAL LEVEL CLEANING AND GROUPING #

# MIE STATE LEVEL CLEANING AND GROUPING #
MIE_State <- read.csv("Outpatient Imaging Efficiency - State.csv",stringsAsFactors = FALSE)

# CHECKING FOR NA'S #
nrow(MIE_State[which(is.na(MIE_State$Score)),]) #0

# CHECKING FOR "Not Available" #
nrow(MIE_State[which(MIE_State$Score ==
                       "Not Available"), ]) #15

# CHANGING "Not Available" TO NA #
MIE_State[which(MIE_State$Score ==
                  "Not Available"), "Score"]<- NA

# CHECKING FOR DUPLICATES #
nrow(MIE_State)
nrow(unique(MIE_State))

# TRANSPOSING THE MAIN VARIABLES Score #
Score_STA <-
  spread(MIE_State[, c("State", "Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score_STA)[-1]<-paste(colnames(Score_STA)[-1],"_SCO_STA",sep = "")

# JOINING hospital_data WITH Compared.to.National, Denominator AND Score #
final_mie <-
  join_all(list(final_mie, Score_STA), by = "State")

blanks_columns <- grep(nrow(final_mie),
                       sapply(final_mie, function(x)
                         sum(is.na(x))))# NONE OF THE COLUMNS ARE COMPLETELY BLANK
# MIE STATE LEVEL CLEANING AND GROUPING #

# MIE NATIONAL LEVEL CLEANING AND GROUPING #
MIE_National <- read.csv("Outpatient Imaging Efficiency - National.csv",stringsAsFactors = FALSE)

# CHECKING FOR NA'S #
nrow(MIE_National[which(is.na(MIE_National$Score)),]) #0

# CHECKING FOR "Not Available" #
nrow(MIE_National[which(MIE_National$Score ==
                          "Not Available"), ]) #0

# CHECKING FOR DUPLICATES #
nrow(MIE_National)
nrow(unique(MIE_National))

# TRANSPOSING THE MAIN VARIABLES Score #
Score_NAT <-
  spread(MIE_National[, c("Measure.ID", "Score")],
         Measure.ID,
         Score)

colnames(Score_NAT)[-1]<-paste(colnames(Score_NAT)[-1],"_SCO_NAT",sep = "")

# JOINING hospital_data WITH Score #
final_mie <-
  cbind(final_mie, Score_NAT)

blanks_columns <- grep(nrow(final_mie),
                       sapply(final_mie, function(x)
                         sum(is.na(x))))# NONE OF THE COLUMNS ARE COMPLETELY BLANK
# MIE NATIONAL LEVEL CLEANING AND GROUPING #

# WRITE FILE Medical Imaging Efficiency #
write.csv(final_mie,file = "Efficient Use Of Medical Imaging.csv")

#-----------------------Medical Imaging Efficiency END------------------------------------#