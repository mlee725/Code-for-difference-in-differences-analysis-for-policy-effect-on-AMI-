#########################
#
# DiD script 
#########################
# load in packages
library(did)
library(dplyr)
library(reshape2)
library(stringr)
library(tidyverse)
library(data.table)
library(readxl)
library(ggpubr)
library(moments)

#########################
# load in data

AMISexOlder <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/McGIll/snowglobe/Coal Ban/AMI Metadata/Mock 2007_2019SexOlder.xlsx", sheet = "Sheet1")
AMI <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/McGIll/snowglobe/Coal Ban/AMI Metadata/Mock dataset2007_2019.xlsx", sheet = "dataset")
DiDCovariates <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/McGIll/snowglobe/Coal Ban/Data/DiDCovariates20220404.csv")
agricultural_data <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/McGIll/snowglobe/Coal Ban/Data/2010 census data/agricultural_data.xlsx")
Treated <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/McGIll/snowglobe/Coal Ban/Data/Townships Exposure/Two year townships treatment March292020_Jie.xlsx", sheet = "Sheet1")
HealthCareAccessibilty <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/McGIll/snowglobe/Coal Ban/Data/Accessibility_E2SFCA_mockdataset.xlsx")
CVDriskfactors <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/McGIll/snowglobe/Coal Ban/Data/CVD risk factors.xlsx", sheet = "Sheet1")
RetiredPowerPlants <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/McGIll/snowglobe/Coal Ban/Data/BeijignTownships_Powerplants.xlsx", sheet = "Sheet1")
Temperature_fluct <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/McGIll/snowglobe/Coal Ban/Data/Temperature_fluctuation.xlsx")

AMI_1year <- read_excel()
AMISexOlder <- read_excel()
AMI <- read_excel()
DiDCovariates <- read.csv()
agricultural_data <- read_excel()
Treated <- read_excel()
HealthCareAccessibilty <- read_excel()
CVDriskfactors <- read_excel()
RetiredPowerPlants <- read_excel()
Temperature_fluct <- read_excel()

#####################
# AMI data

AMI <- rbind(AMI[,c("ID","Sex","Age","Period","Rate")], AMISexOlder[,c("ID","Sex","Age","Period","Rate")])
WideAMI <- AMI %>%
  reshape2::dcast(ID + Period ~ Sex + Age, value.var = "Rate") %>% # long to wide
  filter(Period == "13_14"|Period == "16_17"|Period == "18_19") %>%
  `colnames<-`(c("ID", "Period", "AMI_All_65", "AMI_All_Standardized", "AMI_men_65", "AMI_men_all", "AMI_women_65","AMI_women_all")) %>%
  mutate(Key = paste0(ID, "_", stringr::str_replace_all(Period, "_", "")))


# townships data
DiDCovariates$HSchool<- (DiDCovariates$Total_totover5-DiDCovariates$Subtotal_never-DiDCovariates$Subtotal_primary-DiDCovariates$Subtotal_juniorhigh)/DiDCovariates$Total_totover5
DiDCovariates$Unempl <- DiDCovariates$Unemployed/DiDCovariates$TotalEconomicallyActivePopulation_over15
DiDCovariates$HSchool[is.na(DiDCovariates$HSchool)] <- DiDCovariates[DiDCovariates$ID == 261,]$HSchool

DiDCovariates <- aggregate(cbind(HSchool, Unempl) ~ ID, data = DiDCovariates, mean)

# agi data
agricultural_data$propAgri <- agricultural_data$agri_total/agricultural_data$total

# Treated status
Treated <- Treated %>%
  group_by(ID) %>%
  mutate(First.year = min(
    if_else(TreatPropCuml > 0.5, as.integer(Year), NA_integer_),
    na.rm = TRUE)) 
Treated$First.year[Treated$First.year == Inf] <- 0
Treated <- Treated[Treated$Year == 2019|Treated$Year == 2017,
                   c("ID", "TimePeriod", "TreatPropCuml", "TreatedPropMax", "First.year")]
Treated$Period <- ifelse(Treated$TimePeriod == 1, "1617", "1819")
Treated$Key <- paste0(Treated$ID, "_", Treated$Period)

# load in health care access
HealthCareAccess <- HealthCareAccessibilty %>%
  filter(Year == 2013|Year == 2014|Year == 2016|Year == 2017| Year == 2018|Year == 2019) %>% # remove some years
  mutate(TimePeriod = case_when(Year == 2019|Year == 2018 ~ "1819",
                                Year == 2016|Year == 2017 ~ "1617",
                                Year == 2013|Year == 2014 ~ "1314")) %>%
  group_by(ID, TimePeriod) %>%
  mutate(Bed = mean(Access_bed_thou)) 
Health <- HealthCareAccess[!duplicated(HealthCareAccess[, c("ID", "TimePeriod", "Bed")]), c("ID", "TimePeriod","Bed")] 
Health$Key <- paste0(Health$ID, "_", Health$TimePeriod)

# CVD risk factors
CVDriskfactors <- CVDriskfactors %>%
  dplyr::select(c("ID", "rate_obesity2014", "rate_hyperTC2014", "rate_smoke2014",
                  "rate_obesity2017", "rate_hyperTC2017", "rate_smoke2017"))

# retired power plants
RetiredPowerPlants$Key <- paste0(RetiredPowerPlants$ID, "_", RetiredPowerPlants$TimePeriod)
RetiredPowerPlants$BPPlant <- ifelse(RetiredPowerPlants$PPlant > 0, 1, 0)

# merge dataset
Tm <- merge(Treated[, c("TreatPropCuml", "TreatedPropMax", "Key", "First.year", "ID")], CVDriskfactors, by = "ID", all = T)
Tm <- merge(Tm, agricultural_data[,c("ID", "propAgri")], by = "ID", all = T)
Tm <- merge(Tm, DiDCovariates[,c("ID", "HSchool", "Unempl")], by = "ID", all = T)
Tm <- merge(Tm, Health[,c("TimePeriod", "Bed", "Key")], by = "Key", all = T)
Tm <- merge(Tm, RetiredPowerPlants[,c("BPPlant", "Key")], by = "Key", all = T)
Tm <- merge(Tm, Temperature_fluct[,c("dailymean_sd_heat", "dailymean_sd_annual", "Key")], by = "Key", all = T)
Merge <- merge(Tm, WideAMI[,c("AMI_All_Standardized", "AMI_men_65", "AMI_women_65", 
                                   "AMI_All_65", "AMI_men_all", "AMI_women_all", "Key")], 
               by = "Key", all = T)

# add in the ineligible townships
Pull <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,
          23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,
          39,  40,  41,  42,  43,  44, 45,  46,  47,  48,  49,  50,  51,  52,  53,  54,
          57,  58,  59,  60,  61,  62,  65,  69,  70,  71,  72,  73,74,  75,  76,  77,  
          78,  79,  80,  81,  82,  83,  84,  86,  87,  88,  89,  90,  91,  92,  93,  94,
          95,  96, 97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 
          111, 112, 117, 118, 119, 126, 130, 148, 149, 159, 164, 165, 166, 179, 182, 183,
          184, 190, 195, 196, 197, 220, 221, 238, 239, 240, 258, 259, 260, 277, 279, 281,
          282, 284, 285, 287, 290, 291, 292, 293, 294, 295, 298, 299, 300, 301, 303, 304,
          305, 307, 289, 288, 121, 67)
Merge$Ineligible <- ifelse(Merge$ID %in% Pull, 1,0)

Merge$TreatedPropMax[is.na(Merge$TreatedPropMax)] <- 0 # replace NAs with 0 for the 1314 time points
Merge <- Merge %>%
  fill(everything(), .direction = 'up') %>%
  mutate(TPoint = case_when(str_detect(Key, "1314") ~ 2014,
                            str_detect(Key, "1617") ~ 2017,
                            str_detect(Key, "1819") ~ 2019)) %>%
  group_by(ID) %>%
  mutate(TreatPropCuml = ifelse(is.na(TreatPropCuml),0, TreatPropCuml)) %>%
  mutate(Treated = case_when(TreatedPropMax > 0.5 ~ 1,TreatedPropMax <= 0.5 ~ 0)) %>%
  mutate(First.T = case_when(First.year == 2016 | First.year == 2017 ~ 2017,
                             First.year == 2018 | First.year == 2019 ~ 2019,
                             First.year == 0 ~ 0)) %>%
  mutate(Smoking = case_when(TimePeriod == "1314" ~ rate_smoke2014,
                             TimePeriod == "1617" ~ rate_smoke2017,
                             TimePeriod == "1819" ~ rate_smoke2017)) %>%
  mutate(Obesity = case_when(TimePeriod == "1314" ~ rate_obesity2014,
                             TimePeriod == "1617" ~ rate_obesity2017,
                             TimePeriod == "1819" ~ rate_obesity2017)) %>%
  mutate(hyperTC = case_when(TimePeriod == "1314" ~ rate_hyperTC2014,
                             TimePeriod == "1617" ~ rate_hyperTC2017,
                             TimePeriod == "1819" ~ rate_hyperTC2017)) %>%
  mutate(MaxTreat = (max(TreatPropCuml))*100) 


####################
Merge<- as.data.frame(Merge)

##############################################
# Staggered Difference in Difference 
##############################################

List_Strag <- c("AMI_All_Standardized", "AMI_men_all", "AMI_women_all",
                "AMI_All_65","AMI_men_65", "AMI_women_65")

#-----------------------------
# staggered DiD with covariates - 

StaggeredDiDData_covarJan_Multi <- data.frame()
for(i in 1:length(List_Strag)){
  Merge$Outcome <- log(Merge[,List_Strag[i]])
  #1
  did_att_gt <- att_gt(yname="Outcome",
                       tname="TPoint",
                       idname="ID",
                       gname="First.T",
                       est_method = "dr",
                       xformla = ~ Bed + propAgri + HSchool + Unempl + Smoking + Obesity  + BPPlant + dailymean_sd_annual,
                       data = Merge[Merge$Ineligible == 0, ]
  )
  #summary(did_att_gt)
  #ggdid(did_att_gt)
  # aggregate them into event study plot
  did.es <- aggte(did_att_gt, type="dynamic")
  # plot the event study
  #ggdid(did.es)
  StaggeredDiDData_covarJan_Multi <- rbind(StaggeredDiDData_covarJan_Multi, cbind(List_Strag[i], paste("overall"), did.es$overall.att,
                                                                                  did.es$overall.att-(did.es$overall.se*1.96),
                                                                                  did.es$overall.att+(did.es$overall.se*1.96)))  
  
  
}

names(StaggeredDiDData_covarJan_Multi) <- c("Model", "Vari", "ATT", "LowerCI", "UpperCI")
StaggeredDiDData_covarJan_Multi$expATT <- (exp(as.numeric(StaggeredDiDData_covarJan_Multi$ATT))-1)*100
StaggeredDiDData_covarJan_Multi$expLowerCI <- (exp(as.numeric(StaggeredDiDData_covarJan_Multi$LowerCI))-1)*100
StaggeredDiDData_covarJan_Multi$expUpperCI <- (exp(as.numeric(StaggeredDiDData_covarJan_Multi$UpperCI))-1)*100

#-----------------------------
# staggered DiD without covariates
StaggeredDiDData_Uni <- data.frame()
for(i in 1:length(List_Strag)){
  Outcome <- log(Merge[,List_Strag[i]])
  Merge$Outcome <- Outcome
  did_att_gt <- att_gt(yname="Outcome",
                       tname="TPoint",
                       idname="ID",
                       gname="First.T",
                       xformla = ~ NULL,
                       est_method = "dr",
                       data = Merge[Merge$Ineligible == 0, ]
  )
  #summary(did_att_gt)
  ggdid(did_att_gt)
  # aggregate them into event study plot
  did.es <- aggte(did_att_gt, type="dynamic")
  # plot the event study
  #ggdid(did.es)
  StaggeredDiDData_Uni <- rbind(StaggeredDiDData_Uni, cbind(List_Strag[i], paste("overall"), did.es$overall.att,
                                                            did.es$overall.att-(did.es$overall.se*1.96),
                                                            did.es$overall.att+(did.es$overall.se*1.96)))
  
  
}
names(StaggeredDiDData_Uni) <- c("Model", "Vari", "ATT", "LowerCI", "UpperCI")
StaggeredDiDData_Uni$expATT <- (exp(as.numeric(StaggeredDiDData_Uni$ATT))-1)*100
StaggeredDiDData_Uni$expLowerCI <- (exp(as.numeric(StaggeredDiDData_Uni$LowerCI))-1)*100
StaggeredDiDData_Uni$expUpperCI <- (exp(as.numeric(StaggeredDiDData_Uni$UpperCI))-1)*100


