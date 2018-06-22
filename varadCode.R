library(tidyverse)
library(plyr)
library(stringr)
library(DescTools)

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

e.effictivness <-
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
    e.effictivness,
    f.timeliness,
    g.effimaging
  ),
  by = "Provider.ID"
)
##______GROUPING OF DATA IS COMPLETED______________##

##______NOW WE START WITH DATA SCALING AND OUTLIER TREATMENT______##

final_chs_data[final_chs_data == "Not Available"] <- NA
final_chs_data <-
  as.data.frame(sapply(final_chs_data, function(x)
    as.numeric(as.character(x))))

# a.mortality LOWER THE SCORE - BETTER THE RESULT,
# b.readmission LOWER THE SCORE - BETTER THE RESULT,
# c.safety LOWER THE SCORE - BETTER THE RESULT,
# d.patientexp HIGHER THE SCORE - BETTER THE RESULT,
# e.effictivness MIX VARIABLES ARE PRESENT,
# f.timeliness LOWER THE SCORE - BETTER THE RESULT,
# g.effimaging LOWER THE SCORE - BETTER THE RESULT

mortality_var<-colnames(a.mortality[,-1])
readmission_var<-colnames(b.readmission[,-1])
safety_var<-colnames(c.safety[,-1])
patientexp_var<-colnames(d.patientexp[,-1])
effictivness_var<-colnames(e.effictivness[,-1])
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
  unique(TEC_National[which(TEC_National$Measure.ID %in% effictivness_var), c(1, 2)])

effictiveness_higher <- temp_1[which(
  temp_1$Measure.Name %in%
    grep(
      "higher",
      temp_1$Measure.Name,
      ignore.case = TRUE,
      value = TRUE
    )
), 2]
effictiveness_lower <- temp_1[which(
  temp_1$Measure.Name %in%
    grep(
      "lower",
      temp_1$Measure.Name,
      ignore.case = TRUE,
      value = TRUE
    )
), 2]

effictiveness_higher <-
  union_all(effictiveness_higher, c("OP_29", "OP_30"))
effictiveness_lower <-
  effictiveness_lower[!(effictiveness_lower == "STK_6")]

scale_var <- union_all(scale_var, effictiveness_higher)
inv_scale_var <- union_all(inv_scale_var, effictiveness_lower)

final_chs_data[,scale_var]<-scale(final_chs_data[,scale_var])
final_chs_data[,inv_scale_var]<-sapply(final_chs_data[,inv_scale_var],inverse_scale)

## FURTHER AS MENTIONED WE NEED TO LIMIT THE OUTLIER TO +3 TO -3 ##
trial<-final_chs_data[,-1]
trial[trial > 3] <- 3
trial[trial < -3] <- -3

final_chs_data <- cbind(final_chs_data[, 1], trial)
colnames(final_chs_data)[1]<-"Provider.ID"
