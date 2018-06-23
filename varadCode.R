library(tidyverse)
library(plyr)
library(stringr)
library(DescTools)
library(rpart)
library(randomForest)
library(rpart.plot)
library(caret)
library(kernlab)
library(doParallel)

RDH <-
  read.csv("Readmissions and Deaths - Hospital.csv", stringsAsFactors = FALSE)

Mortality_Hosp <-
  RDH[which(
    RDH$Measure.ID %in% c(
      "MORT_30_AMI",
      "MORT_30_CABG",
      "MORT_30_COPD",
      "MORT_30_HF",
      "MORT_30_PN",
      "MORT_30_STK"
    )
  ),]

Readmission_Hosp <- RDH[which(
  RDH$Measure.ID %in% c(
    "READM_30_AMI",
    "READM_30_CABG",
    "READM_30_COPD",
    "READM_30_HF",
    "READM_30_HIP_KNEE",
    "READM_30_HOSP_WIDE",
    "READM_30_PN",
    "READM_30_STK"
  )
), ]

Complications <-
  read.csv("Complications - Hospital.csv", stringsAsFactors = FALSE)

Mortality_Hosp <-
  rbind(Mortality_Hosp, Complications[which(Complications$Measure.ID == "PSI_4_SURG_COMP"),])

HAI <-
  read.csv("Healthcare Associated Infections - Hospital.csv",
           stringsAsFactors = FALSE)

SOC_Hosp <- HAI[grep("_NUMERATOR", HAI$Measure.ID,),]

SOC_Hosp <- SOC_Hosp[-(grep("a_", SOC_Hosp$Measure.ID, )), ]

SOC_Hosp <-
  rbind(SOC_Hosp, Complications[which(Complications$Measure.ID %in% c("COMP_HIP_KNEE", "PSI_90_SAFETY")), -c(12, 14, 15)])

HCAHPS_Hosp <-
  read.csv("HCAHPS - Hospital.csv", stringsAsFactors = FALSE)

PE_Hosp <-
  HCAHPS_Hosp[which(
    HCAHPS_Hosp$HCAHPS.Measure.ID %in% c(
      "H_CLEAN_STAR_RATING",
      "H_COMP_1_STAR_RATING",
      "H_COMP_2_STAR_RATING",
      "H_COMP_3_STAR_RATING",
      "H_COMP_4_STAR_RATING",
      "H_COMP_5_STAR_RATING",
      "H_COMP_6_STAR_RATING",
      "H_COMP_7_STAR_RATING",
      "H_HSP_RATING_STAR_RATING",
      "H_QUIET_STAR_RATING",
      "H_RECMND_STAR_RATING"
    )
  ),]

TEC_Hosp <-
  read.csv("Timely and Effective Care - Hospital.csv", stringsAsFactors = FALSE)

EOC_Hosp <- TEC_Hosp[which(
  TEC_Hosp$Measure.ID %in%
    c(
      "CAC_3",
      "IMM_2",
      "IMM_3_OP_27_FAC_ADHPCT",
      "OP_4",
      "OP_22",
      "OP_23",
      "PC_01",
      "STK_1",
      "STK_4",
      "STK_6",
      "STK_8",
      "VTE_1",
      "VTE_2",
      "VTE_3",
      "VTE_5",
      "VTE_6",
      "OP_29",
      "OP_30"
    )
), ]

TOC_Hosp <- TEC_Hosp[which(
  TEC_Hosp$Measure.ID %in%
    c("ED_1b", "ED_2b", "OP_3b", "OP_5", "OP_18b", "OP_20", "OP_21")
),]

OIE_Hosp <-
  read.csv("Outpatient Imaging Efficiency - Hospital.csv",
           stringsAsFactors = FALSE)

EUMI_Hosp <-
  OIE_Hosp[(which(
    OIE_Hosp$Measure.ID %in% c("OP_8", "OP_10", "OP_11", "OP_13", "OP_14")
  )), ]

# Mortality_Hosp 7 Measures Seperated, 
# Readmission_Hosp 8 Measures Seperated,
# SOC_Hosp 8 Measures Seperated,
# PE_Hosp 11 Measures Seperated,
# EOC_Hosp 18 Measures Seperated,
# TOC_Hosp 7 Measures Seperated,
# EUMI_Hosp 5 Measures Seperated.
# So basically we have segregated our 64 measures that will be needed.

hospital_info <- unique(Mortality_Hosp[, c(1:8)])

a.mortality <-
  spread(Mortality_Hosp[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

b.readmission <-
  spread(Readmission_Hosp[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

c.safety <- spread(SOC_Hosp[, c("Provider.ID", "Measure.ID", "Score")],
                   Measure.ID,
                   Score)

d.patientexp <-
  spread(PE_Hosp[, c("Provider.ID",
                     "HCAHPS.Measure.ID",
                     "Patient.Survey.Star.Rating")],
         HCAHPS.Measure.ID,
         Patient.Survey.Star.Rating)

e.effectiveness <-
  spread(EOC_Hosp[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

f.timeliness <-
  spread(TOC_Hosp[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

g.effimaging <-
  spread(EUMI_Hosp[, c("Provider.ID", "Measure.ID", "Score")],
         Measure.ID,
         Score)

final_chs_data <- join_all(
  list(
    a.mortality,
    b.readmission,
    c.safety,
    d.patientexp,
    e.effectiveness,
    f.timeliness,
    g.effimaging
  ),
  by = "Provider.ID"
)

## CREATING SEPERATE FILE FOR EACH GROUP ##
write.csv(a.mortality,"Mortality.csv")
write.csv(b.readmission,"Readmission.csv")
write.csv(c.safety,"Safety.csv")
write.csv(d.patientexp,"PatientExp.csv")
write.csv(e.effectiveness,"Effectiveness.csv")
write.csv(f.timeliness,"Timeliness.csv")
write.csv(g.effimaging,"EffImaging.csv")
## CREATING SEPERATE FILE FOR EACH GROUP ##

##______GROUPING OF DATA IS COMPLETED______________##

##______NOW WE START WITH DATA CLEANING, SCALING AND OUTLIER TREATMENT______##

final_chs_data[final_chs_data == "Not Available"] <- NA
final_chs_data <-
  as.data.frame(sapply(final_chs_data, function(x)
    as.numeric(as.character(x))))

# a.mortality LOWER THE SCORE - BETTER THE RESULT,
# b.readmission LOWER THE SCORE - BETTER THE RESULT,
# c.safety LOWER THE SCORE - BETTER THE RESULT,
# d.patientexp HIGHER THE SCORE - BETTER THE RESULT,
# e.effectiveness MIX VARIABLES ARE PRESENT,
# f.timeliness LOWER THE SCORE - BETTER THE RESULT,
# g.effimaging LOWER THE SCORE - BETTER THE RESULT

mortality_var<-colnames(a.mortality[,-1])
readmission_var<-colnames(b.readmission[,-1])
safety_var<-colnames(c.safety[,-1])
patientexp_var<-colnames(d.patientexp[,-1])
effectiveness_var<-colnames(e.effectiveness[,-1])
timeliness_var<-colnames(f.timeliness[,-1])
effimaging_var<-colnames(g.effimaging[,-1])

## ACCORDING TO CHS METHODOLOGY ##
inverse_scale<-function(a){
  inv_scale<-(mean(a,na.rm = TRUE)-a)/sd(a,na.rm = TRUE)
}  

inv_scale_var<-union_all(mortality_var,readmission_var,safety_var,timeliness_var,effimaging_var)
scale_var<-patientexp_var

TEC_National <-
  read.csv("Timely and Effective Care - National.csv", stringsAsFactors = FALSE)

temp_1 <-
  unique(TEC_National[which(TEC_National$Measure.ID %in% effectiveness_var), c(1, 2)])

effectiveness_higher <- temp_1[which(
  temp_1$Measure.Name %in%
    grep(
      "higher",
      temp_1$Measure.Name,
      ignore.case = TRUE,
      value = TRUE
    )
), 2]
effectiveness_lower <- temp_1[which(
  temp_1$Measure.Name %in%
    grep(
      "lower",
      temp_1$Measure.Name,
      ignore.case = TRUE,
      value = TRUE
    )
), 2]

effectiveness_higher <-
  union_all(effectiveness_higher, c("OP_29", "OP_30"))
effectiveness_lower <-
  effectiveness_lower[!(effectiveness_lower == "STK_6")]

scale_var <- union_all(scale_var, effectiveness_higher)
inv_scale_var <- union_all(inv_scale_var, effectiveness_lower)

final_chs_data[,scale_var]<-scale(final_chs_data[,scale_var])
final_chs_data[,inv_scale_var]<-sapply(final_chs_data[,inv_scale_var],inverse_scale)

## FURTHER AS MENTIONED WE NEED TO LIMIT THE OUTLIER TO +3 TO -3 ##
trial<-final_chs_data[,-1]
trial[trial > 3] <- 3
trial[trial < -3] <- -3

final_chs_data <- cbind(final_chs_data[, 1], trial)
colnames(final_chs_data)[1]<-"Provider.ID"

## CLEANING FINISHES ##

## READING THE HOSPITAL GENERAL INFORMATION FILE ##
HGI_Hosp <-
  read.csv("Hospital General Information.csv",
           stringsAsFactors = FALSE)

## ADDING THE RATING VARIABLE TO THE final_chs_data ##
final_chs_data <-
  merge(final_chs_data, HGI_Hosp[, c(1, 13)], by = "Provider.ID")
final_chs_data$Hospital.overall.rating<-as.numeric(as.character(final_chs_data$Hospital.overall.rating))





## CONVERTING RATING TO FACTOR ##
final_chs_data$Hospital.overall.rating <-
  as.factor(final_chs_data$Hospital.overall.rating)
##______DATA CLEANING, SCALING AND OUTLIER TREATMENT COMPLETED______##

## STARTING RANDOM FOREST MODELLING ##
rf_data<-final_chs_data[,-1]

## REMOVING COLUMNS WHERE THERE IS EXCESS NA WE ARE TAKING 60:40 RATIO ##
## WE ARE ONLY TAKING COLUMNS WHERE VALUE OF NA IS LESS THAN 40 PERCENT ##
rf_data<-rf_data[,sapply(rf_data, function(x) (sum(is.na(x))/length(x))*100)<40]

## NOW WE WILL REPLACE NA WITH MEDIAN VALUES ##
na_treatment<-function(a){
  a[is.na(a)]<-median(a,na.rm = TRUE)
  return(a)
}

rf_data[,-ncol(rf_data)]<-as.data.frame(sapply(rf_data[,-ncol(rf_data)], function(x) na_treatment(x)))

## CREATING TRAIN AND TEST DATA SET
indices <- sample(1:nrow(rf_data), size = 0.7 * nrow(rf_data))
train<-rf_data[indices,]
test<-rf_data[-indices,]

## RANDOM FOREST WITH 1000 TREES ##
rf <-
  randomForest(
    Hospital.overall.rating ~ .,
    data = train,
    mtry = 24,
    na.action = na.omit,
    ntree = 1000
  )

rf_pred <- predict(rf, test[, -ncol(rf_data)])
table(rf_pred, test[, ncol(rf_data)])
confusionMatrix(rf_pred, test[, ncol(rf_data)])

# Overall Statistics
# 
# Accuracy : 0.8083          
# 95% CI : (0.7839, 0.8311)
# No Information Rate : 0.4919          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6984          
# Mcnemar's Test P-Value : NA    

## TRAINING RANDOM FOREST MODEL ##
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(100)
mtry <- sqrt(ncol(train))
grid <- expand.grid(.mtry=mtry)
fit.rf <- train(Hospital.overall.rating~., data=train, method="rf", metric=metric, 
                tuneGrid=grid, trControl=trainControl,na.action=na.omit)
rf_pred_cv<- predict(fit.rf, test)
confusionMatrix(rf_pred_cv, test[, ncol(rf_data)])

# Overall Statistics
# 
# Accuracy : 0.7857          
# 95% CI : (0.7603, 0.8096)
# No Information Rate : 0.4919          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6569          
# Mcnemar's Test P-Value : NA

##  TRAINING ON RPART ##
tree <- rpart(Hospital.overall.rating ~., data=train, na.action=na.omit, 
              control = rpart.control(minsplit=10, cp=0.01))
tree_pred <-  predict(tree, test[, -ncol(rf_data)], type = "class")
table(tree_pred, test[, ncol(rf_data)])
confusionMatrix(tree_pred, test[, ncol(rf_data)])

# Overall Statistics
# 
# Accuracy : 0.6483          
# 95% CI : (0.6193, 0.6764)
# No Information Rate : 0.4919          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4414          
# Mcnemar's Test P-Value : NA  

## TRAINING ON SVM ##
trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"
set.seed(100)
grid <- expand.grid(C=seq(1, 5, by=1))
fit.svm <- train(Hospital.overall.rating~., data=train, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl,na.action=na.omit)
svm_pred_cv<- predict(fit.svm, test)
confusionMatrix(svm_pred_cv, test[, ncol(rf_data)])

# Overall Statistics
# 
# Accuracy : 0.877           
# 95% CI : (0.8562, 0.8958)
# No Information Rate : 0.4919          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.812           
# Mcnemar's Test P-Value : NA  

## TRAINING ON KNN ##
knnmodel <-  train(Hospital.overall.rating~., 
                   data = train,
                   method = "knn", 
                   preProcess = c("center", "scale"),
                   tuneLength = 10,
                   trControl = trainControl(method = "cv"),
                   na.action=na.omit)

knn_pred_cv<- predict(knnmodel, test)
confusionMatrix(knn_pred_cv, test[, ncol(rf_data)])

# Overall Statistics
# 
# Accuracy : 0.6998          
# 95% CI : (0.6718, 0.7267)
# No Information Rate : 0.4919          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5132          
# Mcnemar's Test P-Value : NA 

## THUS WE CAN SEE THAT SVM AND RANDOM FOREST PROVIDE THE BEST ACCURACY POSSIBLE ##

## EDA ANALYSIS ##






################### Analysis #################################


### Multiplot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

############### 1. overall rating analysis###########################
final_chs_data_analyze<- final_chs_data

final_chs_data_analyze$Hospital.overall.rating<- as.character(final_chs_data_analyze$Hospital.overall.rating)


final_chs_data_ratings <- final_chs_data_analyze[which(!is.na(final_chs_data_analyze$Hospital.overall.rating)),] %>%
  group_by(Hospital.overall.rating) %>%
  tally

colnames(final_chs_data_ratings) <- c("Hospital.overall.rating","total") 

head(final_chs_data_ratings)

ggplot(final_chs_data_ratings, aes(x = Hospital.overall.rating,y=total)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#3caed2")+
  geom_text(data=final_chs_data_ratings,aes(x=Hospital.overall.rating,y=total,label=total),angle = 90,hjust=1, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Ratings", y="Total Count")+
  ggtitle("Overall Ratings")

############### 2. analysis on mortality measures###########################

Mortality_Hosp_analyze<- a.mortality

Mortality_Hosp_analyze[ Mortality_Hosp_analyze == "Not Available" ] <- NA

Mortality_Hosp_count<- sapply(Mortality_Hosp_analyze, function(y) sum(length(which(!is.na(y)))))
Mortality_Hosp_count <- data.frame(Mortality_Hosp_count)
Mortality_Hosp_count$Measure_Id<-rownames(Mortality_Hosp_count)
Mortality_Hosp_count<-Mortality_Hosp_count[-1,]

Mortality_Hosp_Per<- sapply(Mortality_Hosp_analyze, function(y) paste(round((sum(length(which(!is.na(y))))/nrow(Mortality_Hosp_analyze)*100),digit=2),"%",sep=""))
Mortality_Hosp_Per<-data.frame(Mortality_Hosp_Per)
Mortality_Hosp_Per$Measure_Id<-rownames(Mortality_Hosp_Per)
Mortality_Hosp_Per<-Mortality_Hosp_Per[-1,]

summ_Mortality_Hosp<- merge(Mortality_Hosp_count,Mortality_Hosp_Per,by="Measure_Id")

ggplot(summ_Mortality_Hosp, aes(x = Measure_Id,y=Mortality_Hosp_count)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#e896e6")+
  geom_text(data=summ_Mortality_Hosp,aes(x=Measure_Id,y=Mortality_Hosp_count,label=Mortality_Hosp_count),angle = 90,hjust=2, vjust=0)+
  geom_text(data=summ_Mortality_Hosp,aes(x=Measure_Id,y=Mortality_Hosp_count,label=Mortality_Hosp_Per),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "No of Mortality Measures Repoted", y="Hospital Counts")+
  ggtitle("Count of Hospitals by Number of Mortality Measures")


############### 3. analysis on Safety of Care###########################

Safety_analyze<- c.safety

Safety_analyze[ Safety_analyze == "Not Available" ] <- NA

Safety_count<- sapply(Safety_analyze, function(y) sum(length(which(!is.na(y)))))
Safety_count <- data.frame(Safety_count)
Safety_count$Measure_Id<-rownames(Safety_count)
Safety_count<-Safety_count[-1,]

Safety_Per<- sapply(Safety_analyze, function(y) paste(round((sum(length(which(!is.na(y))))/nrow(Safety_analyze)*100),digit=2),"%",sep=""))
Safety_Per<-data.frame(Safety_Per)
Safety_Per$Measure_Id<-rownames(Safety_Per)
Safety_Per<-Safety_Per[-1,]

summ_Safety<- merge(Safety_count,Safety_Per,by="Measure_Id")


ggplot(summ_Safety, aes(x = Measure_Id,y=Safety_count)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed4e70")+
  geom_text(data=summ_Safety,aes(x=Measure_Id,y=Safety_count,label=Safety_count),angle = 90,hjust=2, vjust=0)+
  geom_text(data=summ_Safety,aes(x=Measure_Id,y=Safety_count,label=Safety_Per),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "No of Safety of Care Repoted", y="Hospital Counts")+
  ggtitle("Count of Hospitals by Number of Safety of Care Measures")



############### 4. analysis on Readmissions###########################

Readmissions_analyze<- b.readmission

Readmissions_analyze[ Readmissions_analyze == "Not Available" ] <- NA

Readmissions_count<- sapply(Readmissions_analyze, function(y) sum(length(which(!is.na(y)))))
Readmissions_count <- data.frame(Readmissions_count)
Readmissions_count$Measure_Id<-rownames(Readmissions_count)
Readmissions_count<-Readmissions_count[-1,]

Readmissions_Per<- sapply(Readmissions_analyze, function(y) paste(round((sum(length(which(!is.na(y))))/nrow(Safety_analyze)*100),digit=2),"%",sep=""))
Readmissions_Per<-data.frame(Readmissions_Per)
Readmissions_Per$Measure_Id<-rownames(Readmissions_Per)
Readmissions_Per<-Readmissions_Per[-1,]

summ_Readmissions<- merge(Readmissions_count,Readmissions_Per,by="Measure_Id")


ggplot(summ_Readmissions, aes(x = Measure_Id,y=Readmissions_count)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=summ_Readmissions,aes(x=Measure_Id,y=Readmissions_count,label=Readmissions_count),angle = 90,hjust=2, vjust=0)+
  geom_text(data=summ_Readmissions,aes(x=Measure_Id,y=Readmissions_count,label=Readmissions_Per),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "No of Readmissions Measures Repoted", y="Hospital Counts")+
  ggtitle("Count of Hospitals by Number of Readmission Measures")


############### 5. analysis on Patient Experience###########################

Patientexp_analyze<- d.patientexp

Patientexp_analyze[ Patientexp_analyze == "Not Available" ] <- NA

final_Patientexp_CLEAN_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_CLEAN_STAR_RATING)),] %>%
  group_by(H_CLEAN_STAR_RATING) %>%
  tally

final_Patientexp_H_COMP_1_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_COMP_1_STAR_RATING)),] %>%
  group_by(H_COMP_1_STAR_RATING) %>%
  tally

final_Patientexp_H_COMP_2_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_COMP_2_STAR_RATING)),] %>%
  group_by(H_COMP_2_STAR_RATING) %>%
  tally

final_Patientexp_H_COMP_3_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_COMP_3_STAR_RATING)),] %>%
  group_by(H_COMP_3_STAR_RATING) %>%
  tally

final_Patientexp_H_COMP_4_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_COMP_4_STAR_RATING)),] %>%
  group_by(H_COMP_4_STAR_RATING) %>%
  tally


final_Patientexp_H_COMP_5_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_COMP_5_STAR_RATING)),] %>%
  group_by(H_COMP_5_STAR_RATING) %>%
  tally

final_Patientexp_H_COMP_6_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_COMP_6_STAR_RATING)),] %>%
  group_by(H_COMP_6_STAR_RATING) %>%
  tally

final_Patientexp_H_COMP_7_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_COMP_7_STAR_RATING)),] %>%
  group_by(H_COMP_7_STAR_RATING) %>%
  tally

final_Patientexp_H_HSP_RATING_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_HSP_RATING_STAR_RATING)),] %>%
  group_by(H_HSP_RATING_STAR_RATING) %>%
  tally

final_Patientexp_H_QUIET_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_QUIET_STAR_RATING)),] %>%
  group_by(H_QUIET_STAR_RATING) %>%
  tally

final_Patientexp_H_RECMND_STAR <- Patientexp_analyze[which(!is.na(final_chs_data_analyze$H_RECMND_STAR_RATING)),] %>%
  group_by(H_RECMND_STAR_RATING) %>%
  tally



plot_final_Patientexp_CLEAN_STAR<- ggplot(final_Patientexp_CLEAN_STAR, aes(x = as.character(H_CLEAN_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_CLEAN_STAR,aes(x=as.character(H_CLEAN_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "CLEAN_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by CLEAN_STAR_RATING")

plot_final_Patientexp_H_COMP_1_STAR<- ggplot(final_Patientexp_H_COMP_1_STAR, aes(x = as.character(H_COMP_1_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_COMP_1_STAR,aes(x=as.character(H_COMP_1_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "H_COMP_1_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by H_COMP_1_STAR_RATING")

plot_final_Patientexp_H_COMP_2_STAR<- ggplot(final_Patientexp_H_COMP_2_STAR, aes(x = as.character(H_COMP_2_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_COMP_2_STAR,aes(x=as.character(H_COMP_2_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "H_COMP_2_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by H_COMP_2_STAR_RATING")

plot_final_Patientexp_H_COMP_3_STAR<- ggplot(final_Patientexp_H_COMP_3_STAR, aes(x = as.character(H_COMP_3_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_COMP_3_STAR,aes(x=as.character(H_COMP_3_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "H_COMP_3_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by H_COMP_3_STAR_RATING")

plot_final_Patientexp_H_COMP_4_STAR<- ggplot(final_Patientexp_H_COMP_4_STAR, aes(x = as.character(H_COMP_4_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_COMP_4_STAR,aes(x=as.character(H_COMP_4_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "H_COMP_4_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by H_COMP_4_STAR_RATING")

plot_final_Patientexp_H_COMP_5_STAR<- ggplot(final_Patientexp_H_COMP_5_STAR, aes(x = as.character(H_COMP_5_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_COMP_5_STAR,aes(x=as.character(H_COMP_5_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "H_COMP_5_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by H_COMP_5_STAR_RATING")

plot_final_Patientexp_H_COMP_6_STAR<- ggplot(final_Patientexp_H_COMP_6_STAR, aes(x = as.character(H_COMP_6_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_COMP_6_STAR,aes(x=as.character(H_COMP_6_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "H_COMP_6_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by H_COMP_6_STAR_RATING")

plot_final_Patientexp_H_COMP_7_STAR<- ggplot(final_Patientexp_H_COMP_7_STAR, aes(x = as.character(H_COMP_7_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_COMP_7_STAR,aes(x=as.character(H_COMP_7_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "H_COMP_7_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by H_COMP_7_STAR_RATING")

plot_final_Patientexp_H_HSP_RATING_STAR<- ggplot(final_Patientexp_H_HSP_RATING_STAR, aes(x = as.character(H_HSP_RATING_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_HSP_RATING_STAR,aes(x=as.character(H_HSP_RATING_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "H_HSP_RATING_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by H_HSP_RATING_STAR_RATING")

plot_final_Patientexp_H_QUIET_STAR<- ggplot(final_Patientexp_H_QUIET_STAR, aes(x = as.character(H_QUIET_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_QUIET_STAR,aes(x=as.character(H_QUIET_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "QUIET_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by QUIET_STAR_RATING")

plot_final_Patientexp_H_RECMND_STAR<- ggplot(final_Patientexp_H_RECMND_STAR, aes(x = as.character(H_RECMND_STAR_RATING),y=n)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ed854d")+
  geom_text(data=final_Patientexp_H_RECMND_STAR,aes(x=as.character(H_RECMND_STAR_RATING),y=n,label=n),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "RECMND_STAR_RATING", y="Hospital Counts")+
  ggtitle("Count of Hospitals by RECMND_STAR_RATING")


multiplot(plot_final_Patientexp_CLEAN_STAR,plot_final_Patientexp_H_COMP_1_STAR,
          plot_final_Patientexp_H_COMP_2_STAR,plot_final_Patientexp_H_COMP_3_STAR,
          plot_final_Patientexp_H_COMP_4_STAR,plot_final_Patientexp_H_COMP_5_STAR,
          plot_final_Patientexp_H_COMP_6_STAR,plot_final_Patientexp_H_COMP_7_STAR,
          plot_final_Patientexp_H_HSP_RATING_STAR,plot_final_Patientexp_H_QUIET_STAR,
          plot_final_Patientexp_H_RECMND_STAR, cols=3)



############### 6. Effectiveness of Care###########################

Effectiveness_analyze<- e.effectiveness

Effectiveness_analyze[ Effectiveness_analyze == "Not Available" ] <- NA

Effectiveness_count<- sapply(Effectiveness_analyze, function(y) sum(length(which(!is.na(y)))))
Effectiveness_count <- data.frame(Effectiveness_count)
Effectiveness_count$Measure_Id<-rownames(Effectiveness_count)
Effectiveness_count<-Effectiveness_count[-1,]

Effectiveness_Per<- sapply(Effectiveness_analyze, function(y) paste(round((sum(length(which(!is.na(y))))/nrow(Safety_analyze)*100),digit=2),"%",sep=""))
Effectiveness_Per<-data.frame(Effectiveness_Per)
Effectiveness_Per$Measure_Id<-rownames(Effectiveness_Per)
Effectiveness_Per<-Effectiveness_Per[-1,]

summ_Effectiveness<- merge(Effectiveness_count,Effectiveness_Per,by="Measure_Id")


ggplot(summ_Effectiveness, aes(x = Measure_Id,y=Effectiveness_count)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#92ed4d")+
  geom_text(data=summ_Effectiveness,aes(x=Measure_Id,y=Effectiveness_count,label=Effectiveness_count),angle = 90,hjust=1, vjust=0)+
  geom_text(data=summ_Effectiveness,aes(x=Measure_Id,y=Effectiveness_count,label=Effectiveness_Per),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "No of Effectiveness of Care Measures Repoted", y="Hospital Counts")+
  ggtitle("Count of Hospitals By Number of Effectiveness of Care Measures")



############### 7. Timeliness of Care###########################

Timeliness_analyze<- f.timeliness

Timeliness_analyze[ Timeliness_analyze == "Not Available" ] <- NA

Timeliness_count<- sapply(Timeliness_analyze, function(y) sum(length(which(!is.na(y)))))
Timeliness_count <- data.frame(Timeliness_count)
Timeliness_count$Measure_Id<-rownames(Timeliness_count)
Timeliness_count<-Timeliness_count[-1,]

Timeliness_Per<- sapply(Timeliness_analyze, function(y) paste(round((sum(length(which(!is.na(y))))/nrow(Safety_analyze)*100),digit=2),"%",sep=""))
Timeliness_Per<-data.frame(Timeliness_Per)
Timeliness_Per$Measure_Id<-rownames(Timeliness_Per)
Timeliness_Per<-Timeliness_Per[-1,]

summ_Timeliness<- merge(Timeliness_count,Timeliness_Per,by="Measure_Id")


ggplot(summ_Timeliness, aes(x = Measure_Id,y=Timeliness_count)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#719ce2")+
  geom_text(data=summ_Timeliness,aes(x=Measure_Id,y=Timeliness_count,label=Timeliness_count),angle = 90,hjust=1, vjust=0)+
  geom_text(data=summ_Timeliness,aes(x=Measure_Id,y=Timeliness_count,label=Timeliness_Per),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "No of Timeliness of Care Measures Repoted", y="Hospital Counts")+
  ggtitle("Count of Hospitals By Number of Timeliness of Care Measures")

############### 8. Efficient Use of Medical Imaging###########################

Effimaging_analyze<- g.effimaging

Effimaging_analyze[ Effimaging_analyze == "Not Available" ] <- NA

Effimaging_count<- sapply(Effimaging_analyze, function(y) sum(length(which(!is.na(y)))))
Effimaging_count <- data.frame(Effimaging_count)
Effimaging_count$Measure_Id<-rownames(Effimaging_count)
Effimaging_count<-Effimaging_count[-1,]

Effimaging_Per<- sapply(Effimaging_analyze, function(y) paste(round((sum(length(which(!is.na(y))))/nrow(Safety_analyze)*100),digit=2),"%",sep=""))
Effimaging_Per<-data.frame(Effimaging_Per)
Effimaging_Per$Measure_Id<-rownames(Effimaging_Per)
Effimaging_Per<-Effimaging_Per[-1,]

summ_Effimaging<- merge(Effimaging_count,Effimaging_Per,by="Measure_Id")


ggplot(summ_Effimaging, aes(x = Measure_Id,y=Effimaging_count)) + 
  geom_bar(alpha = 0.8, width=0.4,stat = "identity",fill="#ab71e2")+
  geom_text(data=summ_Effimaging,aes(x=Measure_Id,y=Effimaging_count,label=Effimaging_count),angle = 90,hjust=1, vjust=0)+
  geom_text(data=summ_Effimaging,aes(x=Measure_Id,y=Effimaging_count,label=Effimaging_Per),hjust=0, vjust=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "No of Efficient use of Medical imaging Measures Repoted", y="Hospital Counts")+
  ggtitle("Count of Hospitals By Number of Efficient Use of Medical Imaging Measures")




#############################################End of analysis###########################################################################



####################################################

