################################################################################
############################### Candice Yu #####################################
################################################################################


################################################################################
################# Dataset 1: Contact, plan, year, enrollment ###################
################################################################################

#Dataset 1: Contact, plan, year, enrollment
library(tidyverse)

## Set working direction
setwd("/Users/candiceyu/Documents/PSTAT296A/Mycode/Data")

Enrol_sub <- read.csv("CPSC_Enrollment_Info_2019_09.csv")

Enrol_201909 <- subset(Enrol_sub, State=="CT")
Enrol_201909$Year <- 2019

Enrol_201909  <- Enrol_201909[!is.na(as.numeric(Enrol_201909$Enrollment)),]
write.csv(Enrol_201909,
          "/Users/candiceyu/Documents/PSTAT296A/Mycode/Data/Enrollment_Contract_Plan_201909.csv",
          row.names = FALSE)



################################################################################
################# Dataset 2: Contract #, plan #, year ########################## 
######## 8 columns for each county (if plan has service in that county) ########
################################################################################

#1. Change county to dummy variables 
#install.packages("fastDummies") # Install the required package
library(fastDummies) # Load the library
Enrol_Drop <- Enrol_201909[, -c(3,4,7)]
Enrol_dummy <- dummy_cols(Enrol_Drop, select_columns = "County") # Create dummy variable
Enrol_dummy <- Enrol_dummy[, -4]
Enrol_aggregated <- aggregate(. ~ Contract.Number + Plan.ID + State + Year, data = Enrol_dummy, sum)
write.csv(Enrol_aggregated,
          "/Users/candiceyu/Documents/PSTAT296A/Mycode/Data/Enrollment_County_Contract_Plan_201909.csv",
          row.names = FALSE)


########################## PlanArea importer with Segment ID ###################

#Contract #, plan #, segment #, year, 
# 8 columns for each county (if plan has service in that county like Yes or No), benefits

#1. Extract data from PlanArea
library("tidyverse")
library("readxl")

PlanArea <- read_excel("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/PBP Benefits - 2019 - Quarter 4/PlanArea.xltx")
PlanArea_201909 <- subset(PlanArea, stcd=="CT")
#str(PlanArea_201909)

PlanArea_201909_Subset <-PlanArea_201909[, c("pbp_a_hnumber", "pbp_a_plan_identifier", "segment_id...3", 
                                             "eghp_flag", "county", "stcd", "contract_year")]


#2. Change county to dummy variables 
#install.packages("fastDummies") # Install the required package
library(fastDummies)# Load the library
PlanArea_201909_dummy <- dummy_cols(PlanArea_201909_Subset, select_columns = "county") # Create dummy variable
PlanArea_201909_dummy <- PlanArea_201909_dummy[, -5]
str(PlanArea_201909_dummy)
#str(PlanArea_201909_dummy)
PlanArea_aggregated <- aggregate(. ~ pbp_a_hnumber + pbp_a_plan_identifier + segment_id...3 + eghp_flag 
                                 +contract_year + stcd, data = PlanArea_201909_dummy, sum)
str(PlanArea_aggregated) #[1] 345  14
View(PlanArea_aggregated)
colnames(PlanArea_aggregated)[c(1,2,3,6)] <- c("Contract_ID", "Plan_ID", "Segment_ID", "State")
write.csv(PlanArea_aggregated,
          "/Users/candiceyu/Documents/PSTAT296A/Mycode/Data/PlanArea_aggregated_201909.csv",
          row.names = FALSE)


################################################################################
############################### Benefit data ###################################
################################################################################

########################## 1. Download the Benefit data ########################
#install.packages("readxl")
library("readxl")
#2.1. pbp_Section_D
pbp_Section_D<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                  "PBP Benefits - 2019 - Quarter 4/pbp_Section_D_Excel.xltx"))

#2.1.1. Premium	(pbp_d_mplusc_premium):	Indicate Plan Premium Amount (Part A/B)
Premium<- pbp_Section_D[, c(1,2,3, which(names(pbp_Section_D)=="pbp_d_mplusc_premium"))]

#2.1.2. Medical Deductible (y/n) (pbp_d_inn_deduct_yn): Is there an In-Network Plan Deductible? (Yes=1, No=2)
Medical_Deductible_yn<- pbp_Section_D[, c(1,2,3, which(names(pbp_Section_D)=="pbp_d_inn_deduct_yn"))]

#2.1.3. Medical Deductible (pbp_d_inn_deduct_amt): Indicate In-Network Plan Deductible Amount
Medical_Deductible<- pbp_Section_D[, c(1,2,3, which(names(pbp_Section_D)=="pbp_d_inn_deduct_amt"))]

#2.1.4. OOP Max	(pbp_d_out_pocket_amt): Indicate In-Network Maximum Enrollee Out-of-Pocket Cost Amount
OOP_Max<- pbp_Section_D[, c(1,2,3, which(names(pbp_Section_D)=="pbp_d_out_pocket_amt"))]

#2.2. pbp_b1a_inpat_hosp
#2.2.1. Inpatient copay (pbp_b1a_copay_mcs_amt_t1): Indicate Copayment amount for the Medicare-covered stay
pbp_b1a_inpat_hosp<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                  "PBP Benefits - 2019 - Quarter 4/pbp_b1a_inpat_hosp.xltx"))
Inpatient_copay <- pbp_b1a_inpat_hosp[, c(1,2,3, which(names(pbp_b1a_inpat_hosp)=="pbp_b1a_copay_mcs_amt_t1"))]

#2.3. pbp_b8_clin_diag_ther
pbp_b8_clin_diag_ther<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                       "PBP Benefits - 2019 - Quarter 4/pbp_b8_clin_diag_ther.xltx"))
#2.3.1. Rx deductible (pbp_b8b_ded_amt): Outpnt X-Rays Deductible Amt
Rx_deductible <- pbp_b8_clin_diag_ther[, c(1,2,3, which(names(pbp_b8_clin_diag_ther)=="pbp_b8b_ded_amt"))]

#2.3.2. Rx copay minimum (pbp_b8b_copay_mc_amt): Outpnt X-Rays MC Min Copay Amt
Rx_copay_minimum <- pbp_b8_clin_diag_ther[, c(1,2,3, which(names(pbp_b8_clin_diag_ther)=="pbp_b8b_copay_mc_amt"))]

#2.3.3. Rx copay maximum (pbp_b8b_copay_mc_amt_max): Outpnt X-Rays MC Max Copay Amt
Rx_copay_maximum <- pbp_b8_clin_diag_ther[, c(1,2,3, which(names(pbp_b8_clin_diag_ther)=="pbp_b8b_copay_mc_amt_max"))]

#2.3.4. Rx coinsurance minimum (pbp_b8b_coins_pct_cmc): Outpnt X-Rays MC Min Coins Pct
Rx_coinsurance_minimum <- pbp_b8_clin_diag_ther[, c(1,2,3,which(names(pbp_b8_clin_diag_ther)=="pbp_b8b_coins_pct_cmc"))]

#2.3.5. Rx coinsurance maximum (pbp_b8b_coins_pct_cmc_max): Outpnt X-Rays MC Max Coins Pct
Rx_coinsurance_maximum <- pbp_b8_clin_diag_ther[, c(1,2,3,which(names(pbp_b8_clin_diag_ther)=="pbp_b8b_coins_pct_cmc_max"))]

#2.4. pbp_b7_health_prof
pbp_b7_health_prof<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                          "PBP Benefits - 2019 - Quarter 4/pbp_b7_health_prof.xltx"))
#2.4.1. PCP copay minimum (pbp_b7a_copay_amt_mc_min): Primary Care MC Min Copay Amt
PCP_copay_minimum <- pbp_b7_health_prof[, c(1,2,3,which(names(pbp_b7_health_prof)=="pbp_b7a_copay_amt_mc_min"))]

#2.4.2. PCP copay maximum (pbp_b7a_copay_amt_mc_max): Primary Care MC Max Copay Amt
PCP_copay_maximum <- pbp_b7_health_prof[, c(1,2,3,which(names(pbp_b7_health_prof)=="pbp_b7a_copay_amt_mc_max"))]

#2.4.3. Specialist copay minimum (pbp_b7d_copay_amt_mc_min): Phys Spclist Min MC Copay Amt
Specialist_copay_minimum <- pbp_b7_health_prof[, c(1,2,3,which(names(pbp_b7_health_prof)=="pbp_b7d_copay_amt_mc_min"))]

#2.4.4. Specialist copay maximum (pbp_b7d_copay_amt_mc_max): Phys Spclist Max MC Copay Amt
Specialist_copay_maximum <- pbp_b7_health_prof[, c(1,2,3,which(names(pbp_b7_health_prof)=="pbp_b7d_copay_amt_mc_max"))]


#2.5. pbp_b17_eye_exams_wear
pbp_b17_eye_exams_wear<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                       "PBP Benefits - 2019 - Quarter 4/pbp_b17_eye_exams_wear.xltx"))

#2.5.1. Vision maximum (pbp_b17b_comb_maxplan_amt): Indicate Combined Maximum Plan Benefit Coverage amount
Vision_maximum <- pbp_b17_eye_exams_wear[, c(1,2,3,which(names(pbp_b17_eye_exams_wear)=="pbp_b17b_comb_maxplan_amt"))]

#2.5.2. Vision periodicity (pbp_b17b_comb_maxplan_per): Select the Combined Maximum Plan Benefit Coverage periodicity
Vision_periodicity <- pbp_b17_eye_exams_wear[, c(1,2,3,which(names(pbp_b17_eye_exams_wear)=="pbp_b17b_comb_maxplan_per"))]

#2.6. pbp_b13_other_services
pbp_b13_other_services<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                           "PBP Benefits - 2019 - Quarter 4/pbp_b13_other_services.xltx"))

#2.6.1. OTC maximum	(pbp_b13b_maxplan_amt): OTC Max Plan Amt
OTC_maximum <- pbp_b13_other_services[, c(1,2,3,which(names(pbp_b13_other_services)=="pbp_b13b_maxplan_amt"))]

#2.6.2. OTC periodicity	(pbp_b13b_otc_maxplan_per): OTC Max Period
OTC_periodicity <- pbp_b13_other_services[, c(1,2,3,which(names(pbp_b13_other_services)=="pbp_b13b_otc_maxplan_per"))]

#2.7. pbp_b10_amb_trans
pbp_b10_amb_trans<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                           "PBP Benefits - 2019 - Quarter 4/pbp_b10_amb_trans.xltx"))

#2.7.1. Ambulance copay minimum	(pbp_b10a_copay_gas_amt_min): Indicate the Minimum Copayment amount for Medicare-covered Ground Ambulance Services
Ambulance_copay_minimum <- pbp_b10_amb_trans[, c(1,2,3,which(names(pbp_b10_amb_trans)=="pbp_b10a_copay_gas_amt_min"))]

#2.7.2. Ambulance copay maximum	(pbp_b10a_copay_gas_amt_max): Indicate the Maximum Copayment amount for Medicare-covered Ground Ambulance Services
Ambulance_copay_maximum <- pbp_b10_amb_trans[, c(1,2,3,which(names(pbp_b10_amb_trans)=="pbp_b10a_copay_gas_amt_max"))]

#2.7.3. Transport (number of trips)	(pbp_b10b_bendesc_amt_pal): Indicate number of trips for Plan Approved Health-related Location
Transport_number_trips <- pbp_b10_amb_trans[, c(1,2,3,which(names(pbp_b10_amb_trans)=="pbp_b10b_bendesc_amt_pal"))]

#2.7.4. Transport periodicity (pbp_b10b_bendesc_per_pal): Select Plan Approved Health-related Location Trips periodicity
Transport_periodicity <- pbp_b10_amb_trans[, c(1,2,3,which(names(pbp_b10_amb_trans)=="pbp_b10b_bendesc_per_pal"))]

#2.8. pbp_b13_other_services
pbp_b13_other_services<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                      "PBP Benefits - 2019 - Quarter 4/pbp_b13_other_services.xltx"))

#2.8.1. Meals maximum	(pbp_b13c_maxplan_amt):	Meal Benefit Max Plan Amt
Meals_maximum <- pbp_b13_other_services[, c(1,2,3,which(names(pbp_b13_other_services)=="pbp_b13c_maxplan_amt"))]

#2.8.2. Meals periodicity	(pbp_b13c_maxplan_per):	Meal Benefit Max Plan Per
Meals_periodicity <- pbp_b13_other_services[, c(1,2,3,which(names(pbp_b13_other_services)=="pbp_b13c_maxplan_per"))]

#2.9. pbp_b18_hearing_exams_aids
pbp_b18_hearing_exams_aids<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                           "PBP Benefits - 2019 - Quarter 4/pbp_b18_hearing_exams_aids.xltx"))

#2.9.1. Hearing maximum amount	(pbp_b18b_maxplan_amt):	Hearing Aids Max Plan Amt
Hearing_maximum_amount <- pbp_b18_hearing_exams_aids[, c(1,2,3,which(names(pbp_b18_hearing_exams_aids)=="pbp_b18b_maxplan_amt"))]

#2.9.2. Hearing periodicity	(pbp_b18b_maxplan_per):	Hearing Aids Max Plan Per
Hearing_periodicity <- pbp_b18_hearing_exams_aids[, c(1,2,3,which(names(pbp_b18_hearing_exams_aids)=="pbp_b18b_maxplan_per"))]

#2.10. pbp_b14_preventive
pbp_b14_preventive<- read_excel(paste0("/Users/candiceyu/Documents/PSTAT296A/MyCode/Data/", 
                                               "PBP Benefits - 2019 - Quarter 4/pbp_b14_preventive.xltx"))

#2.10.1. Fitness (pbp_b14c_bendesc_ehc): Select enhanced benefit (Select all that apply)
Fitness <- pbp_b14_preventive[, c(1,2,3,which(names(pbp_b14_preventive)=="pbp_b14c_bendesc_ehc"))]

########################### 2. Benefit data merging ############################

library("dplyr")
library("plyr")
library("tidyr")

Benefit <- merge(Premium, Medical_Deductible_yn, by = c("pbp_a_hnumber", "pbp_a_plan_identifier", "segment_id"))

Listname<- list(Medical_Deductible, OOP_Max, Inpatient_copay, 
            Rx_deductible, Rx_copay_minimum, Rx_copay_maximum, Rx_coinsurance_minimum, 
            Rx_coinsurance_maximum, PCP_copay_minimum, PCP_copay_maximum, 
            Specialist_copay_minimum, Specialist_copay_maximum, Vision_maximum, 
            Vision_periodicity, OTC_maximum, OTC_periodicity, Ambulance_copay_minimum, 
            Ambulance_copay_maximum, Transport_number_trips, Transport_periodicity, 
            Meals_maximum, Meals_periodicity, Hearing_maximum_amount, Hearing_periodicity, Fitness)

for (i in Listname){
  Benefit <- merge(Benefit, i, by = c("pbp_a_hnumber", "pbp_a_plan_identifier", "segment_id"))
}


Benefit_fitness <- Benefit %>%
  mutate(ID = 1:n()) %>%
  mutate(pbp_b14c_bendesc_ehc = strsplit(pbp_b14c_bendesc_ehc, split = ";")) %>%
  unnest(cols = c(pbp_b14c_bendesc_ehc)) %>%
  mutate(Value = 1) %>%
  spread(pbp_b14c_bendesc_ehc, Value, fill = 0) %>%
  select(-ID)

Benefit_201909 <- Benefit_fitness[-48]
str(Benefit_201909)
colnames(Benefit_201909)[31:47] = c("14c1_Health_Education", "14c10_In-Home_Safety_Assessment", 
                                    "14c11_Personal_Emergency_Response_System", 
                                    "14c12_Medical_Nutrition_Therapy", "14c13_In-Home_Medication_Reconciliation", 
                                    "14c14_Re-admission_Prevention", "14c15_Wigs_for_Hair_Loss", 
                                    "14c16_Weight_Management_Programs", "14c17_Alternative_Therapies",
                                    "14c2_Nutritional/Dietary_Benefit", "14c3_Smoking_Tobacco_Counseling",
                                    "14c4_Fitness_Benefit", "14c5_Enhanced_Disease_Management", 
                                    "14c6_Telemonitoring_Services", "14c7_Remote_Access_Technologies", 
                                    "14c8_Bathroom_Safety_Devices", "14c9_Counseling_Services")

str(Benefit_201909)
colnames(Benefit_201909)[2:30] = c("Contract_ID", "Plan_ID", "Segment_ID", "Premium", 
                                  "Medical_Deductible_yn", "Medical_Deductible", "OOP_Max", 
                                  "Inpatient_copay", "Rx_deductible", "Rx_copay_minimum", 
                                  "Rx_copay_maximum", "Rx_coinsurance_minimum", "Rx_coinsurance_maximum", 
                                  "PCP_copay_minimum", "PCP_copay_maximum", "Specialist_copay_minimum", 
                                  "Specialist_copay_maximum", "Vision_maximum", "Vision_periodicity", 
                                  "OTC_maximum", "OTC_periodicity", "Ambulance_copay_minimum", 
                                  "Ambulance_copay_maximum", "Transport_number_trips", 
                                  "Transport_periodicity", "Meals_maximum", "Meals_periodicity", 
                                  "Hearing_maximum_amount", "Hearing_periodicity")
Benefit_201909 <- Benefit_201909[-1]
Benefit_201909$Year <- 2019
Benefit_201909 <- Benefit_201909 %>% relocate(Year, .after = Segment_ID)

write.csv(Benefit_201909,
          "/Users/candiceyu/Documents/PSTAT296A/Mycode/Data/Benefit_201909_fitness_20221103.csv",
          row.names = FALSE)

Benefit_201909 <- read.csv("/Users/candiceyu/Documents/PSTAT296A/Mycode/Data/Benefit_201909_fitness_20221103.csv")


