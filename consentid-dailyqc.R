# HOUSEHOLD CONSENT AND HOUSE BARCODING IN INDONESIA



library (tidyverse)
library (lubridate)
library (stringr)

rm(list = ls())
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/1. raw data/20180801")

# DEFINE TODAY FOR DAILY QC - this is the date of data collection
date <- "2018-08-01"
day.qc <- ymd (date)
rm(date)

# house consent information and barcode
consent <- read_csv (file="RISE_consent_final_ID.csv")

# household consent form
consent1 <- read_csv (file = "RISE_consent_final_ID-consent_form1.csv")

# child consent form
consent3 <- read_csv (file = "RISE_consent_final_ID-consent_form3.csv")

# children names and DOB
consent.childnames <- read_csv (file = "RISE_consent_final_ID-consent3_childname.csv")
#############################################

# FIX ALL DATES *****************************
fix_date <- function(x_date){
  x_date <- ifelse(!is.na(ymd_hms(x_date)), ymd_hms(x_date), mdy_hm(x_date))  # Check the format and return the correct integer-date
  x_date <- as.POSIXct(x_date, origin = "1970-01-01", tz = "UTC")  # Convert the integer-date to a consistent format
}

consent$endtime <- fix_date(consent$endtime) 
consent$starttime <- fix_date (consent$starttime)
consent$endtime <- fix_date (consent$endtime)
consent$time1 <- fix_date (consent$time1)
consent$time2 <- fix_date (consent$time2)
consent$time5 <- fix_date (consent$time5)
consent$time9 <- fix_date (consent$time9)
consent$time10 <- fix_date (consent$time10)
consent$time11 <- fix_date (consent$time11)
consent$today <- mdy (consent$today)

consent1$time3 <- fix_date (consent1$time3)
consent1$time4 <- fix_date (consent1$time4)

consent3$time6 <- fix_date (consent3$time6)
consent3$time8 <- fix_date (consent3$time8)

#############################################
##  Correct known errors in the data       ##
#############################################
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/2. code")
source("consentid-corrections.R")
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data")




#############################################
#############################################
##  Clean variables                        ##
#############################################
#############################################
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/2. code")
source("consentid-clean.R")
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data")


#############################################
#############################################
##  MERGE DATA FILES                       ##
#############################################
#############################################

# HOUSEHOLD CONSENT
# remember that there can be multiple surveys for each house because of return visits
# MERGE consent and consent1 to have full set of data
colnames (consent1) <- paste (colnames (consent1), "1", sep = "_")  # add suffix
names (consent1)[names (consent1) == "PARENT_KEY_1"] <- "KEY"  # change column names
hhd.consent.merge <- merge (consent, consent1, by = "KEY")  # MERGE using "KEY"

# CHILD CONSENT
colnames (consent3) <- paste (colnames (consent3), "3", sep = "_")
names (consent3)[names (consent3) == "PARENT_KEY_3"] <- "PARENT_KEY"  # change column names
names (consent)[names (consent) == "KEY"] <- "PARENT_KEY"  # change column names
child.consent.merge <- merge (consent, consent3, by = "PARENT_KEY")  # MERGE using "PARENT_KEY"

# CHILD NAMES
colnames (consent.childnames) <- paste (colnames (consent.childnames), "child", sep = "_")
names (consent.childnames)[names (consent.childnames) == "PARENT_KEY_child"] <- "KEY_3"  # change column names
childnames.merge <- merge (child.consent.merge, consent.childnames, by = "KEY_3")

#############################################


#############################################
#############################################
# SUBSET - ONLY DATA COLLECTED TODAY
#############################################
#############################################

subconsent <- subset (consent, today == day.qc, 
                      select = c (community_name, house_no, house_no_old, today, home_yn, barcode_yn, 
                                  duration, time1, time5, time9, time11)) 

subbarcodes <- subset (subconsent, barcode_yn == 1, 
                       select = c (community_name, house_no, house_no_old, today, home_yn, barcode_yn)) 

subhhd.consent.merge <- subset (hhd.consent.merge, today == day.qc & consented_any_form1_1 == 1, 
                                select = c (community_name, house_no, house_no_old, today, 
                                            consented_any_form1_1)) 

subchild.consent.merge <- subset (child.consent.merge, today == day.qc & consented_any_form3_3 == 1, 
                                  select = c (community_name, house_no, house_no_old, today, 
                                              consented_any_form3_3)) 

subchildnames.merge <- subset (childnames.merge, today == day.qc & consented_any_form3_3 == 1, 
                               select = c (community_name, house_no, house_no_old, today, 
                                           consented_any_form3_3, child_name_child, age_calc_child)) 

#############################################


#############################################
#############################################
# DAILY QC REPORT
#############################################
#############################################

#############################################
####### what communities were visited ************
communities <-  subconsent %>% 
  arrange (community_name) %>% 
  group_by (community_name) %>%
  summarize (count = n ())
communities.list <- pull (communities, var = community_name)

#############################################
####### house vacant, under construction or demolished ************
house.vacant <-  subconsent %>% 
  filter (home_yn == 4 | home_yn == 5 | home_yn == 6)


#############################################
####### time to complete survey sections - TODAY ####### 
# ***************************
# only if home
subhhd.merge.times <- subset (hhd.consent.merge, today == day.qc & home_yn == 1, 
                                      select = c (KEY, community_name, house_no, house_no_old, today, 
                                                  home_yn, barcode_yn, consent_form1_yn_1, 
                                                  duration, time1, time3_1, time5, time9, time11))

#only if form completed - Form 1
subhhd.consent.merge.times <- subset (hhd.consent.merge, today == day.qc & home_yn == 1 & consent_form1_yn_1 ==1, 
                                      select = c (KEY, community_name, house_no, house_no_old, today, 
                                                  home_yn, barcode_yn, consent_form1_yn_1, 
                                                  duration, time1, time3_1, time5, time9, time11)) 
#only if form completed - Form 3
subchild.consent.merge.times <- subset (child.consent.merge, today == day.qc & child_under5_yn ==1 & 
                                          consent_form3_yn_3 ==1,  
                                        select = c (consent_form3_yn_3, child_under5_yn, PARENT_KEY, 
                                                    guardian_form3_available_3, time6_3)) 
names (subchild.consent.merge.times)[names (subchild.consent.merge.times) == "PARENT_KEY"] <- "KEY"  # change column names
survey.times <- merge (subhhd.consent.merge.times, subchild.consent.merge.times, by = "KEY", ALL=TRUE)

# only if barcode installed
barcode.times <- subset (consent, today == day.qc & barcode_yn == 1, 
                         select = c (community_name, house_no, house_no_old, today, 
                                     barcode_yn, time9, time11)) 

# LENGTH OF TIME TO COMPLETE SECTIONS OF THE SURVEY = 
min(subhhd.merge.times$duration/60) 

# TO END OF HOUSEHOLD CONSENT
subhhd.consent.merge.times$duration1<-as.integer((subhhd.consent.merge.times$time5-subhhd.consent.merge.times$time1))
summary(subhhd.consent.merge.times$duration1) # minutes to complete form1

# TO COMPLETE FORM 3
survey.times$duration2<-as.integer((survey.times$time9-survey.times$time5))
summary(survey.times$duration2) # minutes to complete form3

# BARCODE INSTALLATION
barcode.times$duration3<-as.integer((barcode.times$time11-barcode.times$time9))
summary(barcode.times$duration3)

# START TO FINISH
summary(subhhd.merge.times$duration)/60 #
# **********

####### NUMBER OF HOUSEHOLD CONSENT REFUSALS ####### 
# refusal identified by:
# 1) home_yn = 1
# 2) consent_form1_yn = 2 = not willing to sign form
# 3) signed_form1
# 4) consented_none_form1_check1 = 1

subhhd.consent.merge2 <- subset (hhd.consent.merge, today == day.qc, 
                                select = c (community_name, house_no, house_no_old, today, home_yn, 
                                            consent_form1_yn_1, consented_none_form1_1, signed_form1_1)) 

hhd.refusals <- subhhd.consent.merge2 %>% 
  filter (home_yn == 1) %>% 
  filter (consent_form1_yn_1 == 2 | consented_none_form1_1 == 1 | signed_form1_1 == 0)

####### NUMBER OF REFUSALS FOR CHILD SAMPLING CONSENT 
# refusal identified by:
# 1) home_yn = 1
# 2) child_under5_yn=1
# 3) guardian_form3_available=1
# 4) consent_form3_yn=0 or, ${consented_none_form3} =1

subchild.consent.merge2 <- subset (child.consent.merge, today == day.qc, 
                                 select = c (community_name, house_no, house_no_old, today, home_yn, 
                                             child_under5_yn, guardian_form3_available_3, consent_form3_yn_3, 
                                             consented_none_form3_3, signed_form3_3)) 

child.refusals <- subchild.consent.merge2 %>% 
  filter (home_yn == 1) %>% 
  filter (child_under5_yn == 1) %>% 
  filter (guardian_form3_available_3 == 1) %>% 
  filter (consent_form3_yn_3 == 0 | consented_none_form3_3 == 1 | signed_form3_3 == 0)























# 
#############################################
####### checking for errors
#############################################

# mismatch between participate_yn and completing form 1 - which means they weren't asked about children consent*****
xxx <- subset (hhd.consent.merge, today == day.qc, 
                                select = c (community_name, house_no, house_no_old, today, 
                                            consented_any_form1_1, participate_yn)) 

check1 <- xxx %>% 
  group_by (community_name, house_no_old) %>% 
  summarise (consent = max (consented_any_form1_1), 
             x = max (participate_yn))  # collapse to one row per house
check1$check2 <- ifelse(check1$x == check1$consent, 0, 1)  # 1 means there is an error
check_participate <- subset (check1, check2 == 1, 
                             select = c (community_name, house_no_old)) 


# mismatch between scanned barcode number and entered house number
yyy <- subset (consent, today == day.qc, 
                      select = c (community_name, barcode_id, barcode_id_text, barcode_prev_yn, barcode_scan, 
                                  barcode_scan_text, house_no, house_no_old, house_no_old2, house_no_old_confirm)) 

barcodes <- yyy %>% 
  select (community_name, barcode_id, barcode_id_text, barcode_prev_yn, barcode_scan, 
          barcode_scan_text, house_no, house_no_old, house_no_old2, house_no_old_confirm) %>% 
  tidyr::separate (barcode_id, c ("a", "b", "house_no1"))  # split barcode_id = scan of installation
barcodes <- barcodes %>% 
  select (community_name, barcode_id_text, barcode_prev_yn, barcode_scan, barcode_scan_text, 
          house_no, house_no_old, house_no_old2, house_no_old_confirm, house_no1) %>% 
  tidyr::separate (barcode_id_text, c ("a", "b", "house_no2"))  # split barcode_id_text = text of installation
barcodes <- barcodes %>% 
  select (community_name, barcode_prev_yn, barcode_scan, barcode_scan_text, 
          house_no, house_no_old, house_no_old2, house_no_old_confirm, house_no1, house_no2) %>% 
  tidyr::separate (barcode_scan, c ("a", "b", "house_no3"))  # split barcode_scan = scan of previous installation
barcodes <- barcodes %>% 
  select (community_name, barcode_prev_yn, barcode_scan_text, 
          house_no, house_no_old, house_no_old2, house_no_old_confirm, house_no1, house_no2, house_no3) %>% 
  tidyr::separate (barcode_scan_text, c ("a", "b", "house_no4"))  # split barcode_scan_text = text of previous installation
barcodes <- barcodes %>% 
  select (community_name, barcode_prev_yn, house_no, house_no1, house_no2, house_no3, house_no4) %>% 
  mutate (house_check = ifelse (!is.na (house_no1), house_no1, 
                                ifelse (!is.na (house_no2), house_no2, 
                                        ifelse (!is.na(house_no3), house_no3, house_no4))))
barcodes$house_check <- as.numeric (barcodes$house_check)
barcodes <- barcodes %>% 
  select (community_name, house_no, house_check) %>% 
  mutate (house_compare = ifelse (house_no == house_check, 1, 0)) %>% 
  filter(house_compare==0) %>% 
  select (community_name, house_no)



# ****************************
# summary by community and house
# ****************************

no.houses.approached.comm <- subconsent %>% 
  group_by(community_name) %>% 
  summarize (no.houses.approached = n())
no.houses.approached.comm <- rbind (no.houses.approached.comm, data.frame(community_name='Total', 
                                                                          no.houses.approached = nrow(subconsent)))

no.house.vacant.comm <- house.vacant %>% 
  group_by(community_name) %>% 
  summarize (no.house.vacant = n())
no.house.vacant.comm <- rbind (no.house.vacant.comm, data.frame(community_name='Total', 
                                                                no.house.vacant = nrow(house.vacant)))


no.barcode.install.comm <- subbarcodes %>% 
  group_by(community_name) %>% 
  summarize (no.barcode.install = n())
no.barcode.install.comm <- rbind (no.barcode.install.comm, data.frame(community_name='Total', 
                                                                      no.barcode.install = nrow(subbarcodes)))


no.hhd.consent.comm <- subhhd.consent.merge %>% 
  group_by(community_name) %>% 
  summarize (no.hhd.consent = n())
no.hhd.consent.comm <- rbind (no.hhd.consent.comm, data.frame(community_name='Total', 
                                                              no.hhd.consent = nrow(subhhd.consent.merge)))


no.hhd.refusal.comm <- hhd.refusals %>% 
  group_by(community_name) %>% 
  summarize (no.hhd.refusal = n())
no.hhd.refusal.comm <- rbind (no.hhd.refusal.comm, data.frame(community_name='Total', 
                                                              no.hhd.refusal = nrow(hhd.refusals)))


no.child.consent.comm <- subchild.consent.merge %>% 
  group_by(community_name) %>% 
  summarize (no.child.consent = n())
no.child.consent.comm <- rbind (no.child.consent.comm, data.frame(community_name='Total', 
                                                                  no.child.consent = nrow(subchild.consent.merge)))


no.children.comm <- subchildnames.merge %>% 
  group_by(community_name) %>% 
  summarize (no.child.consento.children = n())
no.children.comm <- rbind (no.children.comm, data.frame(community_name='Total', 
                                                        no.child.consento.children = nrow(subchildnames.merge)))


no.child.refusal.comm <- child.refusals %>% 
  group_by(community_name) %>% 
  summarize (no.child.refusal = n())
no.child.refusal.comm <- rbind (no.child.refusal.comm, data.frame(community_name='Total', 
                                                                  no.child.refusal = nrow(child.refusals)))


by.comm <- left_join(no.houses.approached.comm, no.house.vacant.comm, by="community_name")
by.comm <- left_join(by.comm, no.barcode.install.comm, by="community_name")
by.comm <- left_join(by.comm, no.hhd.consent.comm, by="community_name")
by.comm <- left_join(by.comm, no.hhd.refusal.comm, by="community_name")
by.comm <- left_join(by.comm, no.child.consent.comm, by="community_name")
by.comm <- left_join(by.comm, no.children.comm, by="community_name")
by.comm <- left_join(by.comm, no.child.refusal.comm, by="community_name")