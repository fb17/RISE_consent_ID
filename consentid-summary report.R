# HOUSEHOLD CONSENT AND HOUSE BARCODING IN INDONESIA

library (tidyverse)
library (lubridate)
library (stringr)

rm(list = ls())
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/1. raw data/20180906")
# setwd("C:/Users/Fiona Barker/Dropbox/work/ID trip/Consent data analysis/raw data/20180810")

#############################################
#############################################
##  DOWNLOAD ALL FILES                     ##
#############################################
#############################################

# house consent information and barcode
# consent <- read_csv (file="RISE_consent_final_ID.csv")
consent <- read.delim (file="RISE_consent_final_ID.csv", header=TRUE, stringsAsFactors=FALSE)

# household consent form
consent1 <- read.delim (file = "RISE_consent_final_ID-consent_form1.csv", header=TRUE, stringsAsFactors=FALSE)

# child consent form
consent3 <- read.delim (file = "RISE_consent_final_ID-consent_form3.csv", header=TRUE, stringsAsFactors=FALSE)

# children names and DOB
# consent.childnames <- read_csv (file = "RISE_consent_final_ID-consent3_childname.csv")
consent.childnames <- read.csv(file="RISE_consent_final_ID-consent3_childname.csv", header=TRUE, sep="\t")
# this file includes a re-calculation of child age from DOB done by Zainal - calculated at 1 Oct 2018
# consent.childnames$age_new <- (dmy("01-10-2018") - consent.childnames$dob) / 365

#############################################

# FIX ALL DATES *****************************
fix_date <- function(x_date){
  x_date <- ifelse(!is.na(ymd_hms(x_date)), ymd_hms(x_date), mdy_hm(x_date))  # Check the format and return the correct integer-date
  x_date <- as.POSIXct(x_date, origin = "1970-01-01", tz = "UTC")  # Convert the integer-date to a consistent format
}

consent$endtime <- fix_date(consent$endtime) 
consent$starttime <- fix_date (consent$starttime)
# consent$endtime <- fix_date (consent$SubmissionDate)
consent$time1 <- fix_date (consent$time1)
consent$time2 <- fix_date (consent$time2)
consent$time5 <- fix_date (consent$time5)
consent$time9 <- fix_date (consent$time9)
consent$time10 <- fix_date (consent$time10)
consent$time11 <- fix_date (consent$time11)
consent$today <- ymd (consent$today)

consent1$time3 <- fix_date (consent1$time3)
consent1$time4 <- fix_date (consent1$time4)

consent3$time6 <- fix_date (consent3$time6)
consent3$time8 <- fix_date (consent3$time8)

consent.childnames$dob <- mdy (consent.childnames$dob)

#############################################
##  Correct known errors in the data       ##
#############################################
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/2. code")
# setwd("C:/Users/Fiona Barker/Dropbox/work/ID trip/Consent data analysis/code")
source("consentid-corrections.R")
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data")
# setwd("C:/Users/Fiona Barker/Dropbox/work/ID trip/Consent data analysis/raw data")



#############################################
#############################################
##  Clean variables                        ##
#############################################
#############################################
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/2. code")
# setwd("C:/Users/Fiona Barker/Dropbox/work/ID trip/Consent data analysis/code")
source("consentid-clean.R")
setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data")
# setwd("C:/Users/Fiona Barker/Dropbox/work/ID trip/Consent data analysis/raw data")

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


# CHECKING ERRORS IN HOUSE NUMBERS

# 1. DIFFERENT house_no_old for same house_no_final
error1 <- consent %>% 
  select (community_name, house_no_final, house_no_old_final) %>% 
  group_by (community_name, house_no_final) %>% 
  summarise (min = min(house_no_old_final, na.rm = TRUE), 
             max = max(house_no_old_final, na.rm = TRUE), count = n())
error1 <- subset (error1, !is.na(house_no_final))  # remove house_no_final=NA
error1$check <- if_else(error1$min == error1$max, 1, 0)
table (error1$check)  
rm(error1)

# 2. SAME house_no_old for multiple house_no_final
error2 <- consent %>% 
  filter (house_no_old_final != "NEW" & house_no_old_final != "New") %>% 
  select (community_name, house_no_final, house_no_old_final) %>% 
  group_by (community_name, house_no_old_final) %>% 
  summarise (min = min(house_no_final, na.rm = TRUE), 
             max = max(house_no_final, na.rm = TRUE), count = n())
error2 <- subset (error2, min != Inf)  # remove house_no_final=NA
error2 <- subset (error2, max != -Inf) 
error2$check <- if_else(error2$min == error2$max, 1, 0)
table (error2$check) #no mis-matches!
rm(error2)


# check <- consent %>% 
#   filter (community == "U") #Cedde = U
# rm(check)

####### DATE RANGE ********************************
summary (consent$today) 
start <- min (consent$today)
end <- max (consent$today)

#############################################
#############################################
#  SUMMARY REPORT TO MATCH WITH SUPERVISOR SUMMARY SHEET
#############################################
#############################################   
# summary by community and house

# 1. HOUSES WITH AND WITHOUT BARCODES
barcoded <- consent %>% 
  select (community_name, house_no_final) %>% 
  filter (!is.na(house_no_final))
barcoded <- unique (barcoded) #651 unique house_no_final = barcoded houses
barcoded$barcode_installed <- 1

#match to house_no_old
x <- consent %>% 
  select (community_name, house_no_final, house_no_old_final) %>% 
  group_by (community_name, house_no_old_final) %>% 
  mutate (house_no_new = max (house_no_final, na.rm = TRUE)) %>% 
  select (community_name, house_no_new)
x <- unique (x)  #672 unique houses, including NA for new house no (i.e. not barcoded)

#check
count <- x %>% filter (!is.na(house_no_new)) #672 - which matches above  good; 21 did not get barcoded
names(count)[names(count) == 'house_no_new'] <- 'house_no_final'
count <- unique (count) 
n_occur <- data.frame(table(count$community_name, count$house_no_final)) 
#found the 2 that are duplicated and have fixed
rm(count, n_occur)

x$houses.approached <- 1
names(x)[names(x) == 'house_no_new'] <- 'house_no_final'

houses <- full_join (barcoded, x, by = c("community_name", "house_no_final")) # 
sum(houses$barcode_installed, na.rm = TRUE) #651 - ok
rm(x)
houses$barcode_installed <- if_else (is.na(houses$house_no_final), 0, houses$barcode_installed)
sum(houses$barcode_installed, na.rm = TRUE) #651 - ok
houses$barcode.not.installed <- if_else(houses$barcode_installed == 0 | 
                                          is.na(houses$barcode_installed), 1, 0)
table(houses$barcode.not.installed) #21 not installed

# 2. BARCODE REFUSALS
barcode.refuse <- consent %>% 
  select (community_name, house_no_old_final, house_no_final, barcode_yn, home_yn, barcode_prev_yn) %>% 
  group_by(community_name, house_no_old_final) %>% 
  summarise (barcoded1 = max (barcode_yn, na.rm = TRUE), 
             barcoded2 = min (barcode_yn, na.rm = TRUE), 
             barcoded3 = max (barcode_prev_yn, na.rm = TRUE),
             home_yn_min = min (home_yn, na.rm = TRUE), 
             home_yn_max = max (home_yn, na.rm = TRUE), 
             house_no_final = max (house_no_final, na.rm = TRUE)) %>% 
  filter (house_no_final == -Inf) #21 with no barcode

# home_yn: 3	No one at home, 4	No, house is vacant
table (barcode.refuse$home_yn_min, barcode.refuse$home_yn_max) #these are all vacant

barcode.refuse$barcode.refuse <- if_else (barcode.refuse$home_yn_max == 1, 1, 0)
barcode.refuse <- barcode.refuse  %>% 
  select (community_name, house_no_old_final, barcode.refuse)

summary.byhouse <- full_join (houses, barcode.refuse, 
                              by = c("community_name" = "community_name",
                                     "house_no_old_final" = "house_no_old_final")) 

# VACANT HOUSES (MAY OR MAY NOT BE BARCODED)
table(consent$home_yn) 
#27 that indicated they were vacant (4), none under construction (5) or demolished (6) - although some identified in Icha's spreadsheet
#83 indicated no one at home but family ok with barcoding (7)

vacant2 <- consent 
vacant2$vacant_y <- if_else (vacant2$home_yn == 4, 1, 0) #27 identified as vacant
table (vacant2$vacant_y, useNA = "always")

vacant <- vacant2 %>% 
  select (community_name, house_no_old_final, house_no_final, home_yn, vacant_y, today) %>% 
  group_by(community_name, house_no_old_final) %>% 
  mutate (min = min(home_yn, na.rm = TRUE), max = max(home_yn, na.rm = TRUE), 
          house_no_final = max(house_no_final, na.rm = TRUE), 
          vacant = max(vacant_y, na.rm = TRUE), count = n()) %>% 
  slice (which.max (as.Date (today, '%m/%d/%Y'))) #keep most recent entry
vacant <- vacant %>% 
  select (community_name, house_no_old_final, vacant)
vacant$vacant <- as.numeric(vacant$vacant)
rm(vacant2)
table(vacant$vacant) #just 24 vacant houses

summary.byhouse <- full_join (summary.byhouse, vacant, 
                              by = c("community_name" = "community_name",
                                     "house_no_old_final" = "house_no_old_final")) 

# 3. HOUSEHOLD CONSENTS - Y/N
####### HOUSEHOLD CONSENT forms (form 1) #######
# (signed and at least one consent item=yes)
table(hhd.consent.merge$signed_form1_1, hhd.consent.merge$consented_any_form1_1) #632 signed and consented

summ.hhd.consent <- hhd.consent.merge %>% 
  filter (signed_form1_1 == 1 & consented_any_form1_1 == 1) %>% 
  select (community_name, house_no_old_final) %>% 
  group_by(community_name, house_no_old_final) %>% 
  summarize (no.hhd.consents = n()) #588 houses
sum (summ.hhd.consent$no.hhd.consents) #632 - ok
table (summ.hhd.consent$no.hhd.consents) #up to 5 consents in a house
#   1   2   3   4   5 
# 555  26   4   2   1
summary (summ.hhd.consent$no.hhd.consents)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.075   1.000   5.000 

summary.byhouse <- full_join (summary.byhouse, summ.hhd.consent, 
                              by = c("community_name" = "community_name", 
                                     "house_no_old_final" = "house_no_old_final"))

no.hhd.consents <- summ.hhd.consent %>% 
  select (no.hhd.consents) %>% 
  group_by (no.hhd.consents) %>% 
  summarise (houses = n())

# 3. HOUSEHOLD REFUSALS
####### NUMBER OF HOUSEHOLD CONSENT REFUSALS ####### 
# refusal identified by:
# 1) home_yn = 1
# 2) consent_form1_yn = 2 = not willing to sign form
# 3) signed_form1
# 4) consented_none_form1_check1 = 1

hhd.refusals <- hhd.consent.merge %>% 
  # filter (home_yn == 1 ) %>%  # needed to remove this for the rogue entry
  filter (consent_form1_yn_1 == 2 | consented_none_form1_1 == 1 | signed_form1_1 == 0) %>% 
  select (community_name, house_no_final, house_no_old_final) %>% 
  group_by (community_name, house_no_old_final) %>% 
  summarize (hhd.refusal = n())

summary.byhouse <- full_join (summary.byhouse, hhd.refusals,  # number of hhd refusals 
                              by = c("community_name" = "community_name", 
                                     "house_no_old_final" = "house_no_old_final"))

# *********************************
# List of houses, consented_yn, vacant_yn, never home, 

# merge consent and hhd consent - but keep all entries
names (consent)[names (consent) == "PARENT_KEY"] <- "KEY"
hhd.consent.merge.all <- full_join (consent, consent1, by = "KEY") #819

hhd.reasons <- hhd.consent.merge.all %>% 
  mutate (home1 = if_else(home_yn == 1, 1, 0), vacant1 = if_else (home_yn == 4, 1, 0), 
          hhd.consent1 = if_else(signed_form1_1 == 1 & consented_any_form1_1 == 1, 1, 0), 
          refuse1 = if_else(consent_form1_yn_1 == 2 | consented_none_form1_1 == 1 | 
                              signed_form1_1 == 0, 1, 0), 
          return1 = if_else(return_visit_yn == 1, 1, 0)) %>% 
  group_by (community_name, house_no_old_final) %>% 
  summarize (home = max (home1, na.rm = TRUE), vacant = max (vacant1, na.rm = TRUE), 
             hhd.consent = max (hhd.consent1, na.rm = TRUE), hhd.consent.no = sum (hhd.consent1, na.rm = TRUE), 
             refuse = max (refuse1, na.rm = TRUE), refuse.no = sum (refuse1, na.rm = TRUE),
             return = max (return1, na.rm = TRUE)) #

table (hhd.reasons$home) #594 houses with someone at home at least once
table (hhd.reasons$vacant) #24 vacant
table (hhd.reasons$home, hhd.reasons$vacant) #
table (hhd.reasons$home, hhd.reasons$refuse) #7 at home but refused + 1 rogue return visit refusal = 8
table (hhd.reasons$hhd.consent)  #588 houses with consents
table (hhd.reasons$hhd.consent.no) #555+26+4+2+1 = 588 houses with consent, up to 5 hhd consents in one house
sum (hhd.reasons$hhd.consent.no) #632
table (hhd.reasons$refuse.no) #9 refusals (one house with 2 hhd refusals)
table (hhd.reasons$hhd.consent, hhd.reasons$home) 


# non-consenting houses
hhd.reasons.no <- hhd.reasons %>% 
  filter (hhd.consent != 1) #84 houses with no consents
#replace INF with NA
hhd.reasons.no$vacant[hhd.reasons.no$vacant == -Inf] <- NA
hhd.reasons.no$refuse[hhd.reasons.no$refuse == -Inf] <- NA
hhd.reasons.no$home[hhd.reasons.no$home == -Inf] <- NA

sum (hhd.reasons.no$vacant, na.rm = TRUE) #23
sum (hhd.reasons.no$refuse, na.rm = TRUE) #7 houses + 1 house that had a refusal and a consent
table (hhd.reasons.no$home, hhd.reasons.no$vacant) [1,1] #54 not home, 24 vacant

# checking
refuse <- hhd.reasons %>% 
  filter (refuse == 1) #8 refusals

# 
# check <- hhd.reasons %>%
#   filter (community == "U") %>%
#   filter (house_no_old_final == "37")
# 
# check <- summary.byhouse %>%
#   filter (community_name == "Kg Cedde") %>%
# 
# check <- hhd.consent.merge %>%
#   select (community, house_no_old_final, house_no_final, home_yn, signed_form1_1, consented_any_form1_1)
# 
# 
# check <- child.consent.merge %>%
#   filter (community == "P") %>%
#   filter (house_no_final == 83)
# 
# check <- consent %>% 
#     filter (home_yn == 4)  
#   
  
not.home <- hhd.reasons %>% 
  filter (hhd.consent != 1) %>%  #84 houses with no consents
  filter (vacant != 1) %>%  #23 vacant = 61 houses left
  filter (refuse != 1) #7 remaining houses with at least one refusal = 54


# 4. CHILD CONSENTS
table(child.consent.merge$signed_form3_3, child.consent.merge$consented_any_form3_3) #217 signed and consented; 2 signed but no consent items
table(consent3$signed_form3_3, consent3$consented_any_form3_3)

summ.child.consent <- child.consent.merge %>% 
  filter (signed_form3_3 == 1 &  consented_any_form3_3 == 1) %>% 
  select (community_name, house_no_old_final) %>% 
  group_by(community_name, house_no_old_final) %>% 
  summarize (no.child.consents = n())
sum(summ.child.consent$no.child.consents) #217 - ok

summary.byhouse <- full_join (summary.byhouse, summ.child.consent,
                              by = c("community_name" = "community_name", 
                                     "house_no_old_final" = "house_no_old_final")) 

# check
table(child.consent.merge$signed_form3_3, useNA = "always") #219 signed
table(child.consent.merge$consented_any_form3_3, useNA = "always") #217, 19=0
table(child.consent.merge$consented_none_form3_3, useNA = "always") #2
table(child.consent.merge$child_under5_no_3, useNA = "always") #17 NA
table(child.consent.merge$guardian_form3_available_3, useNA = "always") #7 not available
table(child.consent.merge$guardian_form3_available_3, 
      child.consent.merge$child_under5_no_3, useNA = "always")
table(child.consent.merge$guardian_form3_available_3, 
      child.consent.merge$consent_form3_yn_3, useNA = "always") 
#of the 229 with guardian available, 219 said yes to consent form
table(child.consent.merge$consent_form3_yn_3, 
      child.consent.merge$signed_form3_3, useNA = "always") #all 219 signed
table(child.consent.merge$signed_form3_3, 
      child.consent.merge$consented_any_form3_3, useNA = "always") 
#of the 219, 2 did not consent to anything
#so 217 valid consent forms

# 5. NUMBER OF CHILDREN CONSENTED
table(childnames.merge$signed_form3_3, childnames.merge$consented_any_form3_3) #246 children (2 not consented to any)

children.consented.house <- childnames.merge %>% 
  filter (signed_form3_3 == 1 & consented_any_form3_3 == 1) %>% 
  select (community_name, house_no_old_final) %>% 
  group_by(community_name, house_no_old_final) %>% 
  summarize (no.children.consented = n())
sum(children.consented.house$no.children.consented) #246

summary.byhouse <- full_join (summary.byhouse, children.consented.house,
                              by = c("community_name" = "community_name",
                                     "house_no_old_final" = "house_no_old_final"))

# checks
table(childnames.merge$signed_form3_3, childnames.merge$consented_any_form3_3, 
      useNA = "always") #248 signed, but 246 consented to at least one
table(childnames.merge$signed_form3_3, childnames.merge$consented_none_form3_3, 
      useNA = "always") #2 consented to none

# 6. number of child refusals
# refusal identified by:
# 1) home_yn = 1
# 2) child_under5_yn=1
# 3) guardian_form3_available=1
# 4) consent_form3_yn=0 or, ${consented_none_form3} =1

child.refusals <- child.consent.merge %>% 
  filter (home_yn == 1 | forms_child ==1) %>% 
  filter (child_under5_yn == 1) %>% 
  filter (guardian_form3_available_3 == 1) %>% 
  filter (consent_form3_yn_3 == 0 | consented_none_form3_3 == 1 | signed_form3_3 == 0) %>% 
  select (community_name, house_no_old_final) %>% 
  group_by (community_name, house_no_old_final) %>% 
  summarize (no.child.refusals = n())

summary.byhouse <- full_join (summary.byhouse, child.refusals,
                              by = c("community_name" = "community_name", 
                                     "house_no_old_final" = "house_no_old_final"))
summary.byhouse$test <-1

# TOTALS FOR ALL COMMUNITIES
table.by.comm <- summary.byhouse %>% 
  group_by(community_name) %>% 
  summarise (houses.approached = sum(houses.approached, na.rm = TRUE),
             barcode.installed = sum(barcode_installed, na.rm = TRUE), 
             barcode.not.installed = sum(barcode.not.installed, na.rm = TRUE),
             barcode.refuse = sum(barcode.refuse, na.rm = TRUE),
             house.vacant = sum (vacant, na.rm = TRUE), 
             #not.home = sum (not.home, na.rm = TRUE), 
             no.hhd.consents = sum(no.hhd.consents, na.rm = TRUE), 
             no.hhd.refusals = sum(hhd.refusal, na.rm = TRUE), 
             no.child.consents = sum(no.child.consents, na.rm = TRUE), 
             no.children.consented = sum(no.children.consented, na.rm = TRUE), 
             no.child.refusals = sum(no.child.refusals, na.rm = TRUE))

table.all.comm <- summary.byhouse %>% 
  group_by(test) %>% 
  summarise (houses.approached = sum(houses.approached, na.rm = TRUE), 
             barcode.installed = sum(barcode_installed, na.rm = TRUE), 
             barcode.not.installed = sum(barcode.not.installed, na.rm = TRUE),
             barcode.refuse = sum(barcode.refuse, na.rm = TRUE),
             house.vacant = sum(vacant, na.rm = TRUE),
             #not.home = sum(not.home, na.rm = TRUE),
             no.hhd.consents = sum(no.hhd.consents, na.rm = TRUE), 
             no.hhd.refusals = sum(hhd.refusal, na.rm = TRUE), 
             no.child.consents = sum(no.child.consents, na.rm = TRUE), 
             no.children.consented = sum(no.children.consented, na.rm = TRUE), 
             no.child.refusals = sum(no.child.refusals, na.rm = TRUE)) %>% 
  mutate (community_name = "Total") %>% 
  select (community_name, everything ())
table.all.comm$test <- NULL

table.all.comm <- rbind (table.by.comm, table.all.comm)

table.all.comm <- full_join (comm.id, table.all.comm)



summary.byhouse$test <- NULL
summary.byhouse <- summary.byhouse %>% 
  select (community_name, house_no_final, house_no_old_final, houses.approached, everything())


# ************************
# split by community

for (i in c("Kg Lempangang", "Kawasan Untia", "Kg Nelayan, Barombong", "Kg Bonelengga",
            "Kg Tunas Jaya", "Jl Barawaja 2, Pampang", "Kg Cedde","Kg Gampangcayya, Tallo",
            "Kg Bambu-Bambu, Jl Birta", "Kg Baru, Antang", "Jl Borong Raya Baru", "Kg Alla-Alla")){
  x <- str_replace_all(i, " ", "")
  x <- str_replace_all(x, "-", "")
  x <- str_replace_all(x, ",", "")
  assign (x, data.frame())

  table.comm <- summary.byhouse %>% 
    filter (community_name == i) %>% 
    arrange (house_no_final, house_no_old_final)
  table.comm$house_no_final <- as.character(table.comm$house_no_final)

  y <-  data.frame (houses.approached = sum(table.comm$houses.approached, na.rm = TRUE), 
                    barcode.installed = sum(table.comm$barcode_installed, na.rm = TRUE), 
                    barcode.not.installed = sum(table.comm$barcode.not.installed, na.rm = TRUE),
                    barcode.refuse = sum(table.comm$barcode.refuse, na.rm = TRUE), 
                    house.vacant = sum(table.comm$vacant, na.rm = TRUE), 
                    #not.home = sum(table.comm$not.home, na.rm = TRUE), 
                    no.hhd.consents = sum(table.comm$no.hhd.consents, na.rm = TRUE),
                    hhd.refusal = sum(table.comm$hhd.refusal, na.rm = TRUE),
                    no.child.consents = sum(table.comm$no.child.consents, na.rm = TRUE),
                    no.children.consented = sum(table.comm$no.children.consented, na.rm = TRUE),
                    no.child.refusals = sum(table.comm$no.child.refusals, na.rm = TRUE))
  
  z <- data.frame (community_name ="" , house_no_final="Total", house_no_old_final= "")
  zz <- bind_cols (z, y)
  zz$house_no_old_final <- as.character(zz$house_no_old_final)
  
  table <- bind_rows(table.comm, zz)
  table$community_name <- NULL
  
  assign (x, table) #rename dataframe to community name
}






#############################################
####### time to complete survey sections ####### 
# ***************************
# only if home
home.times <- subset (consent, home_yn == 1, 
                      select = c (community_name, house_no_final, house_no_old, home_yn, 
                                  duration))
home.times.comm <- home.times %>% 
  group_by (community_name) %>% 
  summarize(mean = round(mean (duration/60), digits = 1), min = round(min (duration/60), digits = 1), 
            max = round(max(duration/60), digits = 1))

#only if form completed - Form 1
hhd.consent.merge.times <- subset (hhd.consent.merge, consent_form1_yn_1 ==1, 
                                   select = c (KEY, community_name, house_no_final, house_no_old, today, 
                                               home_yn, barcode_yn, consent_form1_yn_1, 
                                               duration, time1, time3_1, time5, time9, time11)) 
#only if form completed - Form 3
child.consent.merge.times <- subset (child.consent.merge, child_under5_yn == 1 & consent_form3_yn_3 == 1, 
                                     select = c (PARENT_KEY, guardian_form3_available_3, time6_3)) 
names (child.consent.merge.times)[names (child.consent.merge.times) == "PARENT_KEY"] <- "KEY"  # change column names
all.survey.times <- merge (hhd.consent.merge.times, child.consent.merge.times, by = "KEY", ALL=TRUE)

# only if barcode installed
barcode.times <- subset (consent, barcode_yn == 1, 
                         select = c (community_name, house_no_final, house_no_old, today, 
                                     barcode_yn, time9, time11)) 

# LENGTH OF TIME TO COMPLETE SECTIONS OF THE SURVEY = 
min(home.times$duration/60) 

# TO END OF HOUSEHOLD CONSENT
hhd.consent.merge.times$duration1<-as.integer((hhd.consent.merge.times$time5-hhd.consent.merge.times$time1)/60)
summary(hhd.consent.merge.times$duration1) # minutes to complete form1

hhd.duration.comm <- hhd.consent.merge.times %>% 
  group_by (community_name) %>% 
  summarize(mean = round(mean (duration1), digits = 1), min = round(min (duration1), digits = 1), 
            max = round(max(duration1), digits = 1))

# TO COMPLETE FORM 3
all.survey.times$duration2 <- as.integer((all.survey.times$time9 - all.survey.times$time5)/60)
summary(all.survey.times$duration2) # minutes to complete form3

child.duration.comm <- all.survey.times %>% 
  group_by (community_name) %>% 
  summarize(mean = round(mean (duration2), digits = 1), min = round(min (duration2), digits = 1), 
            max = round(max(duration2), digits = 1))

# BARCODE INSTALLATION
barcode.times$duration3<-as.integer((barcode.times$time11 - barcode.times$time9)/60)
summary(barcode.times$duration3)

barcode.duration.comm <- barcode.times %>% 
  group_by (community_name) %>% 
  summarize(mean = round(mean (duration3), digits = 1), min = round(min (duration3), digits = 1), 
            max = round(max(duration3), digits = 1))

# START TO FINISH
hhd.consent.merge.times$duration4<-as.integer((hhd.consent.merge.times$time11-hhd.consent.merge.times$time1)/60)
summary(hhd.consent.merge.times$duration4)
summary(home.times$duration)/60 #so these are pretty close
# **********

#############################################
####### time to complete survey sections - collapsed by day ####### 
# ***************************

# only if home
home.times2 <- subset (consent, home_yn == 1, 
                       select = c (today, duration))
survey.duration <- home.times2 %>%
  group_by(today) %>%
  summarize(mean = round(mean (duration/60), digits = 1), min = round(min (duration/60), digits = 1), max = round(max(duration/60), digits = 1))

# TO END OF HOUSEHOLD CONSENT
summary(hhd.consent.merge.times$duration1) # minutes to complete form1
hhd.duration <- hhd.consent.merge.times %>% 
  group_by(today) %>%
  summarize(mean = round(mean (duration1), digits = 1), min = round(min (duration1), digits = 1), max = round(max(duration1), digits = 1))

# TO COMPLETE FORM 3
summary(all.survey.times$duration2) # minutes to complete form3
child.duration <- all.survey.times %>% 
  group_by(today) %>%
  summarize(mean = round(mean (duration2/60), digits = 1), min = round(min (duration2/60), digits = 1), max = round(max(duration2/60), digits = 1))

# BARCODE INSTALLATION
summary(barcode.times$duration3)
barcode.duration <- barcode.times %>% 
  group_by(today) %>%
  summarize(mean = round(mean (duration3), digits = 1), min = round(min (duration3), digits = 1), max = round(max(duration3), digits = 1))

# START TO FINISH
summary(hhd.consent.merge.times$duration4)
all.surveys.duration <- hhd.consent.merge.times %>% 
  group_by(today) %>%
  summarize(mean = round(mean (duration4), digits = 1), min = round(min (duration4), digits = 1), max = round(max(duration4), digits = 1))




########################################################
# ADDITIONAL ITEMS FOR FINAL REPORT
# CONSENT DATA - INDONESIA
# 
# 
# 
########################################################


####### NUMBER OF HOUSES WHERE ADULT HOME AND AVAILABLE TO TALK TO FIELD WORKERS#######
at.home.comm <- consent %>% 
  filter (home_yn == 1) %>% 
  group_by (community_name, house_no_old_final) %>% 
  summarise (count = n())  # to get rid of multiple visits to individual houses
# at.home.comm <- at.home.comm %>%
#   group_by (community_name) %>% 
#   summarise (count = n())
# at.home <- sum (at.home.comm$count)  # total number of houses with adult home and willing to talk

####### NUMBER OF HOUSES THAT HAVE BEEN APPROACHED**
house.visits <- consent %>% 
  arrange (community_name, house_no_old_final) %>% 
  group_by (community_name, house_no_old_final) %>% 
  summarise (count = n())  # this gets us number of visits to each house

house.visits.sum <- house.visits %>% 
  group_by (community_name) %>% 
  summarise (sum(count))  # this is number of house visits per community

house.visits.avg <- house.visits %>% 
  group_by (community_name) %>% 
  summarise (avg = mean (count)) %>%  # this is number of house visits per community
  mutate (avg = round (avg, 1) )  #round to 1 decimal

####### NUMBER OF HOUSEHOLD CONSENTS BY CONSENT ITEM
# a)?Participation in the 5-year research program, which will include an engineering intervention either during the research program (intervention group - tranche 1) or 2 years later at the end of the study ('control' group - tranche 2).
# b) We will attach an identifying number to the outside of your house to help researchers identify your household when they conduct their visits.
# c) My household taking part in surveys to be administered 3-monthly at my home over the period 2018 - 2022.
# d) Collection of samples of water, wastewater, soil and other objects or surfaces in or around my home.
# e) Photography of household water storage vessels / household items / house exterior during surveying or when environmental samples are collected.
# f) If requested, allow attachment of temperature monitoring devices ("i-buttons") inside or around my home.
# g) If requested, allow mosquito traps to be installed inside or around my home.
# h) If requested, allow trapping of rodents or rodent activity monitoring around my home.
# i) Allowing researchers to contact me if I leave the settlement and move to live elsewhere.
# j) Allowing researchers to contact me after extreme events (such as extreme climatic events) that might impact on my home.
# k) Allowing researchers to contact me to invite me to participate in SMS surveys that may arise during the study period.

hhd.consents.indiv <- hhd.consent.merge %>% 
  group_by (community_name) %>% 
  summarise (c1 = sum (consent1_1_1, na.rm = TRUE), 
             c2 = sum (consent1_2_1, na.rm = TRUE), 
             c3 = sum (consent1_3_1, na.rm = TRUE), 
             c4 = sum (consent1_4_1, na.rm = TRUE), 
             c5 = sum (consent1_5_1, na.rm = TRUE), 
             c6 = sum (consent1_6_1, na.rm = TRUE),
             c7 = sum (consent1_7_1, na.rm = TRUE), 
             c8 = sum (consent1_8_1, na.rm = TRUE), 
             c9 = sum (consent1_9_1, na.rm = TRUE), 
             c10 = sum (consent1_10_1, na.rm = TRUE), 
             c11 = sum (consent1_11_1, na.rm = TRUE))

total <- table.by.comm %>% 
  select (community_name, no.hhd.consents)
names (total)[names (total) == "no.hhd.consents"] <- "total (consented to any)" #rename

hhd.consents.indiv <- left_join(hhd.consents.indiv, total, by="community_name")

####### NUMBER OF HOUSES WITH CHILDREN ####### 
table(consent$child_under5_yn, useNA = "always") #231

houses.children <- consent %>% 
  filter (child_under5_yn == 1) %>% 
  group_by (community_name, house_no_old_final) %>% 
  summarise (sum (child_under5_yn)) #219 houses

houses.children <- houses.children %>% 
  group_by (community_name) %>% 
  summarise (count = n())  # number of houses with children in each community

####### PARENT/GUARDIAN NOT AVAILABLE 
parent.not.available1 <- child.consent.merge %>% 
  select (community_name, house_no_old_final, child_under5_yn, guardian_form3_available_3) %>% 
  group_by (community_name, house_no_old_final) %>% 
  summarise (child_max = max (child_under5_yn, na.rm = TRUE), 
             guardian_max = max (guardian_form3_available_3, na.rm = TRUE), count = n())

parent.not.available <- parent.not.available1 %>% 
  filter (guardian_max == 0) 

####### NUMBER OF children consented BY CONSENT ITEM
# a) Up to quarterly collection of faecal samples from my child and subsequent analysis of samples.
# b) Collection of venous blood samples once per year and subsequent analysis of samples.
# c) Yearly measurement of height and weight.
# d) The faecal and blood samples provided during this research, and associated data, may be used by named researchers in this program in future research programs.
# e) Researchers accessing local health centre records for vaccination, medical presentations and growth measurements since the birth of my child.

child.consents.indiv <- childnames.merge %>% 
  group_by (community_name) %>% 
  summarise (fecal = sum (consent3_1_3, na.rm = TRUE), 
             blood = sum (consent3_2_3, na.rm = TRUE), 
             height.weight = sum (consent3_3_3, na.rm = TRUE), 
             data.use = sum (consent3_4_3, na.rm = TRUE), 
             health.records = sum (consent3_5_3, na.rm = TRUE), 
             total = sum (consented_any_form3_3, na.rm = TRUE))

#children details

# span of child ages by community
child.details <- childnames.merge %>% 
  filter (signed_form3_3 == 1 &  consented_any_form3_3 ==1) %>% 
  select (community_name, house_no_old_final, house_no_final, gender_child, dob_child, age_final_child, age_calc_child) %>% 
  mutate (female = ifelse (gender_child == 0, 1, 0), male = ifelse (gender_child == 1, 1, 0), 
          genderNA = ifelse (is.na(gender_child), 1, 0), age0_6mo = ifelse (age_final_child < 0.5, 1, 0), 
          age6mo_1yr = ifelse (age_final_child >= 0.5 & age_final_child <1, 1, 0), 
          age1yr_2yr = ifelse (age_final_child >= 1 & age_final_child <2, 1, 0), 
          age2yr_3yr = ifelse (age_final_child >= 2 & age_final_child <3, 1, 0), 
          age3yr_4yr = ifelse (age_final_child >= 3 & age_final_child <4, 1, 0),
          age4yr_5yr = ifelse (age_final_child >= 4 & age_final_child <5, 1, 0), 
          ageNA = ifelse (is.na(age_final_child), 1, 0))
  
child_demog <- child.details %>% 
  select (community_name, female, male, genderNA, age0_6mo, age6mo_1yr, age1yr_2yr, age2yr_3yr, age3yr_4yr, age4yr_5yr, ageNA) %>% 
  group_by (community_name) %>% 
  summarise (female = sum(female, na.rm = TRUE), male = sum(male, na.rm = TRUE), genderNA = sum(genderNA, na.rm = TRUE), 
             age0_6mo = sum(age0_6mo, na.rm = TRUE), age6mo_1yr  = sum(age6mo_1yr, na.rm = TRUE), 
             age1yr_2yr  = sum(age1yr_2yr, na.rm = TRUE), age2yr_3yr  = sum(age2yr_3yr, na.rm = TRUE), 
             age3yr_4yr  = sum(age3yr_4yr, na.rm = TRUE), age4yr_5yr  = sum(age4yr_5yr, na.rm = TRUE), 
             ageNA  = sum(ageNA, na.rm = TRUE))
#add totals
child_demog$test <- 1

table.child.demog <- child_demog %>% 
  group_by(test) %>% 
  summarise (female = sum(female, na.rm = TRUE), male = sum(male, na.rm = TRUE), genderNA = sum(genderNA, na.rm = TRUE), 
             age0_6mo = sum(age0_6mo, na.rm = TRUE), age6mo_1yr  = sum(age6mo_1yr, na.rm = TRUE), 
             age1yr_2yr  = sum(age1yr_2yr, na.rm = TRUE), age2yr_3yr  = sum(age2yr_3yr, na.rm = TRUE), 
             age3yr_4yr  = sum(age3yr_4yr, na.rm = TRUE), age4yr_5yr  = sum(age4yr_5yr, na.rm = TRUE), 
             ageNA  = sum(ageNA, na.rm = TRUE)) %>% 
  mutate (community_name = "Total") %>% 
  select (community_name, everything ())
table.child.demog$test <- NULL
child_demog$test <- NULL

Table6 <- rbind (child_demog, table.child.demog)
Table6 <- Table6[, 1:4]

Table7 <- child_demog  %>% 
  select(community_name, age0_6mo, age6mo_1yr, age1yr_2yr, age2yr_3yr, age3yr_4yr, age4yr_5yr, ageNA)
#make wide to long
Table7_long <- gather (Table7, key = age, value = number.of.children, age0_6mo:ageNA)
#reorder
Table7_long$age <- factor(Table7_long$age, levels = c("ageNA", "age4yr_5yr", "age3yr_4yr", 
                                                      "age2yr_3yr", "age1yr_2yr", "age6mo_1yr", 
                                                      "age0_6mo")) 

ggplot(data = Table7_long, aes(x = community_name, y = number.of.children, fill = age)) + 
  geom_bar(stat = "identity") + 
  xlab ("Settlement") + ylab ("# children") +
  scale_fill_brewer(palette = "Greys") + 
  theme_minimal () + #remove grey background
  theme (panel.grid = element_blank (),              # No underlying grid lines
         axis.text.x = element_text (angle = 90, vjust = 0.5, hjust = 0)) +  # rotated x-axis text 
  ggtitle("Figure 2: Consented children by age and settlement")  
  

# ***do with fewer age categories
child.details2 <- childnames.merge %>% 
  filter (signed_form3_3 == 1 &  consented_any_form3_3 ==1) %>% 
  select (community_name, house_no_old_final, house_no_final, gender_child, dob_child, age_final_child, age_calc_child) %>% 
  mutate (female = ifelse (gender_child == 0, 1, 0), male = ifelse (gender_child == 1, 1, 0), 
          genderNA = ifelse (is.na(gender_child), 1, 0), age0_6mo = ifelse (age_final_child < 0.5, 1, 0), 
          age6mo_2yr = ifelse (age_final_child >= 0.5 & age_final_child <2, 1, 0), 
          age2yr_5yr = ifelse (age_final_child >= 2 & age_final_child <5, 1, 0), 
          ageNA = ifelse (is.na(age_final_child), 1, 0))

child_demog2 <- child.details2 %>% 
  select (community_name, female, male, genderNA, age0_6mo, age6mo_2yr, age2yr_5yr, ageNA) %>% 
  group_by (community_name) %>% 
  summarise (female = sum(female, na.rm = TRUE), male = sum(male, na.rm = TRUE), genderNA = sum(genderNA, na.rm = TRUE), 
             age0_6mo = sum(age0_6mo, na.rm = TRUE), age6mo_2yr  = sum(age6mo_2yr, na.rm = TRUE), 
             age2yr_5yr  = sum(age2yr_5yr, na.rm = TRUE), ageNA  = sum(ageNA, na.rm = TRUE))
#add totals
child_demog2$test <- 1

table.child.demog2 <- child_demog2 %>% 
  group_by(test) %>% 
  summarise (female = sum(female, na.rm = TRUE), male = sum(male, na.rm = TRUE), genderNA = sum(genderNA, na.rm = TRUE), 
             age0_6mo = sum(age0_6mo, na.rm = TRUE), age6mo_2yr  = sum(age6mo_2yr, na.rm = TRUE), 
             age2yr_5yr  = sum(age2yr_5yr, na.rm = TRUE), ageNA  = sum(ageNA, na.rm = TRUE)) %>% 
  mutate (community_name = "Total") %>% 
  select (community_name, everything ())
table.child.demog2$test <- NULL
child_demog2$test <- NULL

Table8 <- rbind (child_demog2, table.child.demog2)
Table8 <- Table8[, 1:4]

Table9 <- child_demog2  %>% 
  select(community_name, age0_6mo, age6mo_2yr, age2yr_5yr)
#make wide to long
Table9_long <- gather (Table9, key = age, value = number.of.children, age0_6mo:age2yr_5yr)
#reorder
Table9_long$age <- factor(Table9_long$age, levels = c("age2yr_5yr", "age6mo_2yr", "age0_6mo")) 

ggplot(data = Table9_long, aes(x = community_name, y = number.of.children, fill = age)) + 
  geom_bar(stat = "identity") + 
  xlab ("Settlement") + ylab ("# children") +
  scale_fill_brewer(palette = "Greys") + 
  theme_minimal () + #remove grey background
  theme (panel.grid = element_blank (),              # No underlying grid lines
         axis.text.x = element_text (angle = 90, vjust = 0.5, hjust = 0)) +  # rotated x-axis text 
  ggtitle("Figure 2: Consented children by age and settlement") 


  

  










# other family member agreed to barcode installion
barcode.check <- consent %>% 
  select (community_name, house_no_old_final, home_yn, house_no_final) %>% 
  mutate (other_approve = ifelse(home_yn == 7, 1, 0)) %>% 
  group_by (community_name, house_no_old_final) %>% 
  summarize (max = max (other_approve, na.rm = TRUE), count = n()) %>% 
  filter (max >= 1)


# CREATE TABLES FOR FINAL REPORT

Table1 <- table.all.comm %>% 
  select(community_name, houses.approached, no.hhd.consents, no.hhd.refusals, house.vacant)

Table3 <- table.all.comm %>% 
  select(community_name, no.child.consents, no.children.consented, no.child.refusals)

Table5 <- table.all.comm %>% 
  select(community_name, barcode.installed, barcode.not.installed, barcode.refuse, house.vacant)











# APPENDICES FOR FINAL REPORT

# APPENDIX 1 - List of all houses that have children less than 5 years old ***not checked YET

####### TABLE 5. LIST HOUSES WITH CHILDREN BY COMMUNITY #######
appendix1 <- consent %>%
  filter (child_under5_yn == 1) %>%
  arrange (community_name, house_no_old_final, house_no_final) %>%
  group_by (community_name, house_no_old_final, house_no_final) %>%
  summarize (count = n()) %>% 
  arrange (community_name, house_no_final, house_no_old_final)
appendix1$count <- NULL

# 
# 
# #   EXPORT DATA FOR ICHA TO CHECK HOUSE NUMBERS BY COMMUNITY
# house_no_maps <- summary.byhouse %>%
#   select (community_name, house_no_final, house_no_old_final, barcode_installed,
#           no.hhd.consents, no.children.consented) %>%
#   arrange (community_name, house_no_final, house_no_old_final)
# 
# setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/3. analysis")
# write_csv(house_no_maps, path = "S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/3. analysis/RISE_consent_2018_maps.csv")



house_no_maps <- summary.byhouse %>%
  select (community_name, house_no_final, house_no_old_final, barcode_installed, vacant, hhd.refusal, 
          no.hhd.consents, no.children.consented) %>%
  arrange (community_name, house_no_final, house_no_old_final)
house_no_maps_20180913 <- house_no_maps
# setwd("S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/3. analysis")
# write_csv(house_no_maps_20180913, path = "S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/3. analysis/RISE_consent_2018_maps_20180913.csv")


# LIST OF CONSENTS FOR FIELD TEAM
# generating a full list of data from consent for the CFW to help them with baseline. 
# My plan so far was to include:
# community
# house number
# head of household name
# household consents that are relevant at baseline - survey, mosquito traps... etc.
# list of all children: name, age or date of birth, gender, and their consents (for each item)


# household consents
hhd.consent.items.study <- hhd.consent.merge %>%
  select (community_name, house_no_final, house_no_old_final, hhd_head_name_1, 
          signed_form1_1, consent1_1_1, consent1_3_1) %>%
  rename (settlement = community_name, house.no = house_no_final, house.no.old = house_no_old_final, 
          hhd.head = hhd_head_name_1, signed.yn = signed_form1_1, 
          study = consent1_1_1, surveys = consent1_3_1) %>% 
  arrange (settlement, house.no) %>% 
  mutate (signed.yn = recode (signed.yn, '0' = "no", '1' = "yes"), 
          study = recode (study, '0' = "no", '1' = "yes"), 
          surveys = recode (surveys, '0' = "no", '1' = "yes"))

# check
table (hhd.consent.items.study$signed.yn) #635 signed
table (hhd.consent.items.study$signed.yn, hhd.consent.items.study$study)  #631 signed and yes to study participation
# no yes
# no    1   0
# yes   4 631
table (hhd.consent.items.study$signed.yn, hhd.consent.items.study$surveys) #629 signed and yes to surveys
# no yes
# no    1   0
# yes   6 629
# write_csv(hhd.consent.items.study, path = "S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/4. reports/hhd_consent_items.csv")


#child consents
child.consent.items <- childnames.merge %>%
  select (community_name, house_no_final, house_no_old_final, guardian_form3_3, 
          child_name_child, gender_child, dob_child, 
          signed_form3_3, consent3_1_3, consent3_2_3, consent3_3_3) %>%
  rename (settlement = community_name, house.no = house_no_final, house.no.old = house_no_old_final, 
          guardian.name = guardian_form3_3, child.name = child_name_child, gender = gender_child, 
          dob = dob_child, signed.yn = signed_form3_3,  
          feces = consent3_1_3, blood = consent3_2_3, height.weight = consent3_3_3) %>% 
  mutate (age.20190201 = (dmy("01-02-2019") - dob)/365, 
          gender = recode (gender, '0' = "female", '1' = "male"), 
          signed.yn = recode (signed.yn, '0' = "no", '1' = "yes"), 
          feces = recode (feces, '0' = "no", '1' = "yes"), 
          blood = recode (blood, '0' = "no", '1' = "yes"), 
          height.weight = recode (height.weight, '0' = "no", '1' = "yes")) %>% 
  select (settlement, house.no, house.no.old, guardian.name, child.name, gender, age.20190201, 
          signed.yn, feces, blood, height.weight) 


# check
sum (child.consent.items$consented_any_form3_3) #246 children
table (child.consent.items$signed_form3_3, child.consent.items$consented_any_form3_3) 

# write_csv(child.consent.items, path = "S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/4. reports/child_consent_items.csv")
# 

# LIST OF CONSENTS FOR OBJ 2A (GRANT, GENIE, PETE)
# they also need copy of maps from Icha!

hhd.consent.items.enviro <- hhd.consent.merge %>%
  select (community_name, house_no_final, house_no_old_final, 
          signed_form1_1, consented_any_form1_1, consent1_4_1, consent1_5_1, 
          consent1_6_1, consent1_7_1, consent1_8_1) %>%
  rename (samples = consent1_4_1, photos = consent1_5_1, ibuttons = consent1_6_1, 
          mosquito = consent1_7_1, rodent = consent1_8_1) %>% 
  arrange (community_name, house_no_final)


# write_csv(hhd.consent.items.enviro, path = "S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/4. reports/consent_enviro.csv")
