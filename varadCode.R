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

#-----------------------SAFETY OF CARE END------------------------------------#