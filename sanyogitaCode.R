
library(dplyr)
folder <- "D:\\Sanyogita\\Upgrad\\Healthcare\\capstone\\Hospital_Revised_FlatFiles_20161110/"     
# path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder
list.filenames<-  gsub(".csv","",file_list)
list.filenames<-  gsub(" ","-",list.filenames)
list.filenames<-  gsub("---","-",list.filenames)

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(list.filenames[i], 
         read.csv(paste(folder, file_list[i], sep=''))
  )}

#Combine files of hospital(facility),National,State

#str(`Ambulatory-Surgical-Measures-Facility`)
Ambul_surg_measure <- left_join(left_join(`Ambulatory-Surgical-Measures-Facility`,`Ambulatory-Surgical-Measures-National`,c("Year")),`Ambulatory-Surgical-Measures-State`,c("State","Year"))
Complications<-left_join(left_join(`Complications-Hospital`,`Complications-National`,c("Measure.Name","Measure.ID")),`Complications-State`,c("State","Measure.Name","Measure.ID"))
HCAHPS <- left_join(left_join(`HCAHPS-Hospital`,`HCAHPS-National`,c("HCAHPS.Measure.ID")),`HCAHPS-State`,c("State","HCAHPS.Measure.ID"))

###Looking at files of hvbp They have identical rows Even first 6 cols are same so good to combine them via merge
##Two df hvbp_Efficiency_08_26_2016 and hvbp_imm2_08_26_2016 have col name as County_name and rest has county.Name
##so changed the col name 
colnames(hvbp_Efficiency_08_26_2016)[7] <- "County.Name"
colnames(hvbp_imm2_08_26_2016)[7] <- "County.Name"

hvbp<-merge(hvbp_ami_08_26_2016,hvbp_Efficiency_08_26_2016)
hvbp<-merge(hvbp,hvbp_hai_08_26_2016)
hvbp<-merge(hvbp,hvbp_hcahps_08_26_2016)
hvbp<-merge(hvbp,hvbp_imm2_08_26_2016)
hvbp<-merge(hvbp,hvbp_outcome_08_26_2016)
hvbp<-merge(hvbp,hvbp_pn_08_26_2016)
hvbp<-merge(hvbp,hvbp_scip_08_26_2016)
hvbp<-merge(hvbp,hvbp_tps_08_26_2016)

HAI <- left_join(`HAI_FluVac_2016Q1_state_03_17_2017-Corrected`,`HAI_FluVac_2016Q1_natl_03_17_2017-Corrected`,c("RPTG_PRD_START_DT","RPTG_PRD_END_DT"))
`Healthcare-Associated-Infections` <-left_join(left_join(`Healthcare-Associated-Infections-Hospital`,`Healthcare-Associated-Infections-State`,
                     c("Measure.Name","Measure.ID","Measure.Start.Date","Measure.End.Date")),`Healthcare-Associated-Infections-National`,
                     c("Measure.Name","Measure.ID","Measure.Start.Date","Measure.End.Date"))

#,`Healthcare-Associated-Infections-National` ,c("Measure.Name","Measure.ID"))


'HOSPITAL_QUARTERLY_IPFQR'<-left_join(HOSPITAL_QUARTERLY_IPFQR_MEASURES_HOSPITAL,HOSPITAL_QUARTERLY_IPFQR_MEASURES_STATE,by.x="HBIPS.2_Measure_Description",by.y="S_HBIPS.2_Measure_Description")

Medicare_Spending <- left_join(left_join(`Medicare-Hospital-Spending-per-Patient-Hospital`,`Medicare-Hospital-Spending-per-Patient-State`,c("Measure.ID","Measure.Name")),`Medicare-Hospital-Spending-per-Patient-National`,c("Measure.ID","Measure.Name"))

`Outpatient-Imaging_eff`<-left_join(left_join(`Outpatient-Imaging-Efficiency-Hospital`,`Outpatient-Imaging-Efficiency-State`,c("Measure.ID","Measure.Name")),`Outpatient-Imaging-Efficiency-National`,c("Measure.ID","Measure.Name"))
payment <- left_join(left_join(`Payment-and-Value-of-Care-Hospital`,`Payment-State`,by.x=c("Payment.Measure.ID","Payment.Measure.Name"), by.y=c("Measure.ID","Measure.Name")),`Payment-National`,c("Measure.ID","Measure.Name"))
redadd_death<-left_join(left_join(`Readmissions-and-Deaths-Hospital`,`Readmissions-and-Deaths-State`,c("Measure.ID","Measure.Name")),`Readmissions-and-Deaths-National`,c("Measure.ID","Measure.Name"))