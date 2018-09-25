####################################################
##            CORRECTIONS                         ##
## This file contains running corrections         ##
## to the data before any analysis was conducted. ##
####################################################

# **********************
# ZAINAL'S FIXES
# *********************

# zainal's fixes #1-4
# 20180703 - Fix barcodes (failed scanning) from Day 1 (3 July 2018)
consent$barcode_id <- as.character(consent$barcode_id)

consent$barcode_id[consent$today == "2018-07-03" & consent$name_surveyor == "14" &
                     consent$community == "W" & consent$house_no == "20" & 
                     consent$barcode_id == "10212228"] <- "NW-HH-020"
consent$barcode_id[consent$today == "2018-07-03" & consent$name_surveyor == "14" &
                     consent$community == "W" & consent$house_no == "16" & 
                     consent$barcode_id == "4410171"] <- "NW-HH-016"
consent$barcode_id[consent$today == "2018-07-03" & consent$name_surveyor == "4" &
                     consent$community == "W" & consent$house_no == "23" & 
                     consent$barcode_id == "11104292"] <- "NW-HH-023"
consent$barcode_id[consent$today == "2018-07-09" & consent$name_surveyor == "10 14" &
                     consent$community == "W" & consent$house_no == "7" & 
                     consent$barcode_id == "4110139"] <- "NW-HH-007"

# zainal's fix #5
#20180717 - wrong community selected
consent$community[consent$today == "2018-07-17" & consent$name_surveyor == "14 15" &
                    consent$community == "W" & consent$house_no == "36"] <- "R"
consent$community_name[consent$today == "2018-07-17" & consent$name_surveyor == "14 15" &
                         consent$community == "R" & consent$house_no == "36"] <- "Kampung Bonelengga, Bulurokeng"

# zainal's fixes 6-9 - are guardian names - 
# 6	01/08/2018	25/08/2018	10	X=Baru, Antang	old=2	new=2	guardian_form3	1	Wahyudi
consent3$guardian_form3[consent3$KEY == "uuid:faf24ba9-ae10-47b8-a09f-15bf07cf0dad/consent_form3[1]"] <- "Wahyudi"
# 7	01/08/2018	26/08/2018	10	X=Baru, Antang	old=30	new=32	guardian_form3	1	Rika
# can't fix as house numbers don't match????????????

# 8	01/08/2018	26/08/2018	10	X=Baru, Antang	old=47		guardian_form3	1	Abdul Jasman
consent3$guardian_form3[consent3$KEY == "uuid:bd455b6e-00e4-4a39-9241-6d3113400427/consent_form3[1]"] <- "Abdul Jasman"

# 9	01/08/2018	30/08/2018	10	S=Tunas Jaya	old=59	new=62	guardian_form3	1	Suriyanti dg. Sunggu
consent3$guardian_form3[consent3$KEY == "uuid:9a092552-8cf3-40b5-ba07-9045a480f27a/consent_form3[1]"] <- "Suriyanti dg. Sunggu"


# zainal's fix #10
#20180731 - wrong community selected
consent$community[consent$today == "2018-07-31" & consent$name_surveyor == "16" &
                    consent$community == "X" & consent$house_no == "50"] <- "U"
consent$community_name[consent$today == "2018-07-31" & consent$name_surveyor == "16" &
                         consent$community == "U" & consent$house_no == "50"] <- "Kampung Ce'de, Bakung"

# zainal's fix #11
# There is a mis-match between scanned barcode (NU-HH-009) and recorded house_no=8; which is correct? please check the map	
#refer to query 15 below
consent$barcode_id[consent$community == "U" & consent$house_no_old == 8 & 
                     consent$house_no == "8" & consent$today == "2018-07-30"] <- "NU-HH-008"

# zainal's fix #12- Kg Nelayan, Barombong	old=56, 61	new=53
#refer to query 19 below
# Multiple "old" house numbers recorded for one "new" house number - Are these 2 merged into 1 house? NO
#house_no entered as 53 but should be 58
consent$house_no[consent$community == "Q" & consent$house_no_old == "61" & 
                   consent$house_no == "53"] <- "58"

# zainal's fixes #13-15
#20180809 - wrong community selected
consent$community[consent$today == "2018-08-09" & consent$name_surveyor == "8" &
                    consent$community == "Q" & consent$house_no == "1"] <- "T"
consent$community_name[consent$today == "2018-08-09" & consent$name_surveyor == "8" &
                         consent$community == "T" & consent$house_no == "1"] <- "Jalan Barawaja 2, Pampang"

consent$community[consent$today == "2018-08-09" & consent$name_surveyor == "8" &
                    consent$community == "Q" & consent$house_no == "15"] <- "T"
consent$community_name[consent$today == "2018-08-09" & consent$name_surveyor == "8" &
                         consent$community == "T" & consent$house_no == "15"] <- "Jalan Barawaja 2, Pampang"

consent$community[consent$today == "2018-08-09" & consent$name_surveyor == "8" &
                    consent$community == "Q" & consent$barcode_scan == "NT-HH-015"] <- "T"
consent$community_name[consent$today == "2018-08-09" & consent$name_surveyor == "8" &
                         consent$community == "T" & consent$barcode_scan == "NT-HH-015"] <- "Jalan Barawaja 2, Pampang"

# zainal's fix #16
#20180809 - wrong house_no_old
consent$house_no_old[consent$today == "2018-08-09" & consent$name_surveyor == "4" &
                       consent$community == "Q" & consent$barcode_scan == "NQ-HH-043"] <- "46"

# zainal's fix #17
# ??
# zainal's fix #18
# consent$house_no_old[consent$today == "2018-08-09" & consent$name_surveyor == "4" &  #zainal to check this one
#                        consent$community == "Q" & consent$house_no == "91" 
#                      & consent$house_no_old == "91"] <- "93A"

# consent$house_no_old[consent$today == "2018-08-09" & consent$name_surveyor == "4" &  #zainal to check this one
#                        consent$community == "Q" & consent$house_no == "90" 
#                      & consent$house_no_old == "90"] <- "93"


#####################
#other fixes found by Fiona
#####################

#20180703 - Fix old house numbers from Day 1 and 2 (they were entered incorrectly)
consent <- mutate (consent, house_no_old = ifelse (house_no_old == "001", "1", 
                                                   ifelse(house_no_old == "004", "4", 
                                                          ifelse(house_no_old == "006", "6", 
                                                                 ifelse(house_no_old == "007", "7", 
                                                                        ifelse(house_no_old == "008", "8", 
                                                                               ifelse(house_no_old == "009", "9", 
                                                                                      ifelse(house_no_old == "010", "10", 
                                                                                             ifelse(house_no_old == "012", "12", 
                                                                                                    ifelse(house_no_old == "014", "14", 
                                                                                                           house_no_old))))))))))


#fix house number
consent$house_no_old[consent$community == "Q" & consent$house_no_old == "64a"] <- "64A"


#############################################
#ERRORS and INFILL HOUSE NUMBERS
consent <- consent %>%
  select (-house_no, everything ()) %>% 
  select (-house_no_old, everything ()) %>% 
  select (-community_name, everything ()) #move to end

consent$a <- as.integer(substring (consent$barcode_scan, nchar(consent$barcode_scan)-2, nchar(consent$barcode_scan)))  #from return visits
consent$b <- as.integer(substring (consent$barcode_scan_text, nchar(consent$barcode_scan_text)-2, nchar(consent$barcode_scan_text)))  #from return visits
consent$c <- as.integer(substring (consent$barcode_id, nchar(consent$barcode_id)-2, nchar(consent$barcode_id))) #from installation
consent$d <- as.integer(substring (consent$barcode_id_text, nchar(consent$barcode_id_text)-2, nchar(consent$barcode_id_text))) #from installation

consent$check <- if_else (consent$c == consent$house_no, 1, 99)

# ERROR CHECK ##############################################################
errors <- consent %>% 
  filter (check == 99) #looking for mis-match between house_no and barcode scan
print(nrow(errors))

# FIXED BELOW:
# 30 July Ce'de - barcode wrong house_no_old=8 (query 15)
# 2 Aug Untia - house_no_old=28 (query 33)
# 8 Aug Barombong - house_no_old = 61 (query 19 and fix 12)



# infill final house number
consent$house_no <- as.integer(consent$house_no)


# hierarchy = house_no, consent$c (from scan), consent$d (from text), consent$a (from scan), consent$b (from text)
consent$house_no_final <- if_else (!is.na(consent$house_no), consent$house_no, consent$c)
consent$house_no_final <- if_else (is.na(consent$house_no_final), consent$d, consent$house_no_final)
consent$house_no_final <- if_else (is.na(consent$house_no_final), consent$a, consent$house_no_final)
consent$house_no_final <- if_else (is.na(consent$house_no_final), consent$b, consent$house_no_final)

consent$a <- NULL                                   
consent$b <- NULL   
consent$c <- NULL   
consent$d <- NULL   
consent$check <- NULL   

rm(errors)

# ***********************************


# ***********************************
#corrections from google sheet
# https://docs.google.com/spreadsheets/d/17IkvfhBbxdij71YMdeJoVt6DmzVA2LhzIO3sQ5hFPDw/edit#gid=0
# ***********************************

#1. Community =Kg Baru, Antang; Survey Date =29 July 2018,  	Community =Kg Baru, Antang 	House_no_old=105; 
# This was recorded as a return visit, but there is no record of a first visit. Is that right?
# FIX: this was not a return visit
# ?????
# not sure if I need to make any changes?
consent$barcode_yn[consent$today == "2018-07-29" &
                     consent$community == "X" & consent$house_no_old == "105"] <- 1

# 2. Community = Kg Tunas Jaya, House_no_old=17, house_no_final=17
# FIX: House no (final) 17 is a new house. There was no old house number.
consent$house_no_old[consent$community == "S" & consent$house_no_final == 17 & 
                       consent$house_no_old == "17"] <- "NEW"

# 3. Community = Kg Baru, Antang, House_no_old=83 and 72, house_no_final=85
# House no old 83 and 72 were merged into House no (final) 85.
# FIX: this is correct, no change
#do merge below

# 4. Community = Jl Borong Raya Baru	, House_no_old= 6, 6B	house_no_final=7
# House no old 6 and 6B merged into House no (final) 7.
#  FIX: this is correct, no change
#do merge below

# 5.Kg Alla-Alla	House_no_old=8, 9	house_no_final=9 
# House no old 8 has become House no (final) 9. 
# House no old 9 has become House no (final) 10.
# FIX: refer to fix #14 below

# 6. Kg Alla-Alla	 House_no_old=NEW	17, house_no_final=17
# FIX: House no (final) 17 is a new house. There was no old house number.
consent$house_no_old[consent$community == "Z" & consent$house_no_final == 17 & 
                       consent$house_no_old == "17"] <- "NEW"

# 7. Kg Alla-Alla	House_no_old=26, 27	house_no_final=28
# FIX: House no old 26 and 28 has merged to become House no (final) 28. 
# House no old 27 has become House no (final) 29.
consent$house_no_old[consent$community == "Z" & consent$house_no_final == 28 & 
                       consent$house_no_old == "27"] <- "28"
#do merge below

# 8. Kg Tunas Jaya	House_no_old=17	 house_no_final=17,20
# FIX: House no old 17 has become House no (final) 20.
# fixed by query 2 - house_no_final =17 is a NEW house

# 9. Kg Baru, Antang	House_no_old=103	house_no_final=109, 113
# House no old 103 has become House no (final) 109. 
# House no old 106 has become House no (final) 113, refused to be consented, but was barcoded.
# FIX: note, there is only ONE entry for House no (final) 113, so assume that house_no_old entered wrong
consent$house_no_old[consent$community == "X" & consent$house_no_final == 113 & 
                       consent$house_no_old == "103"] <- "106"

# 10. Kg Baru, Antang	House_no_old=110	 house_no_final=117,118
# House no old 110 has become House no (final) 117. 
# House no old 111 has become House no (final) 118.
# FIX: note, there is only ONE entry for House no (final) 118, so assume that house_no_old entered wrong
consent$house_no_old[consent$community == "X" & consent$house_no_final == 118 & 
                       consent$house_no_old == "110"] <- "111"

# 11. Kg Baru, Antang	House_no_old=30	house_no_final=31, 32
# House no old 30 has become House no (final) 31. 
# House no old 31 has become House no (final) 32.
# FIX: note, there is only ONE entry for House no (final) 32, so assume that house_no_old entered wrong
consent$house_no_old[consent$community == "X" & consent$house_no_final == 32 & 
                       consent$house_no_old == "30"] <- "31"

# 12. Kg Alla-Alla	18	 house_no_final=19,20
# House no old 18 has become House no (final) 20. 
# House no old 17 has become House no (final) 19.
consent$house_no_old[consent$community == "Z" & consent$house_no_final == 19 & 
                       consent$house_no_old == "18"] <- "17"

# 13.  Kg Alla-Alla	27	 house_no_final=28,29 - this was dealt with in #7
# House no old 26 and 28 has merged to become House no (final) 28. 
# House no old 27 has become House no (final) 29.
# consent$house_no_old[consent$community == "Z" & consent$house_no_final == 28 & 
#                        consent$house_no_old == "27"] <- "28"
# consent$house_no_old_final[consent$community == "Z" & consent$house_no_final == 28] <- "26 & 28" - done below

# 14. Kg Alla-Alla	9	 house_no_final=9,10
# House no old 9 has become House no (final) 10. 
# House no old 8 has become House no (final) 9.
# error in return visit - accidently entered new house number as old house number
consent$house_no_old[consent$community == "Z" & consent$house_no_final == 9 & 
                       consent$house_no_old == "9"] <- "8"

# 15.30 July 2018	Kampung Ce'de, Bakung	8	8 or 9?	
# There is a mis-match between scanned barcode (NU-HH-009) and recorded house_no=8; which is correct? please check the map	
# refer to fix 11 - barcode scan was incorrect 

# 16.	31 July 2018	Kampung Ce'de, Bakung	13	13, 14	There are multiple "new" house numbers for one "old" house number - is that correct?	
# House no old 13 has become House no (final) 14.
# House no old 12 has become House no (final) 13
consent$house_no_old[consent$community == "U" & consent$house_no_final == 13 & 
                       consent$house_no_old == "13"] <- "12"

# 17.		Kg Baru, Antang	old=13	new=13,14	
# There are multiple "new" house numbers for one "old" house number - is that correct?
# House no old 13 no change and still be House no (final) 13. 
# House no old 13A has become House no (final) 14.
consent$house_no_old[consent$community == "X" & consent$house_no_final == 14 & 
                       consent$house_no_old == "13"] <- "13A"
  
# 18.		Kampung Ce'de, Bakung	old=9	new=9,21	
# There are multiple "new" house numbers for one "old" house number - is that correct?
# House no old 9 no change and still be House no (final) 9. 
# House no old 19 has become House no (final) 21.
consent$house_no_old[consent$community == "U" & consent$house_no_final == 21 & 
                       consent$house_no_old == "9"] <- "19"

# 19.	Kg Nelayan, Barombong	old=56, 61	new=53	
# Multiple "old" house numbers recorded for one "new" house number - Are these 2 merged into 1 house? NO
#refer to Zainal's fix #12 above; house_no_final entered as 53 but should be 58

#20 Kg Nelayan, Barombong; old=36, 38, new=36	Multiple "old" house numbers recorded for one "new" house number - are they merged houses?
# no, there was a return visit where old house number entered incorrectly
consent$house_no_old[consent$community == "Q" & consent$return_visit_yn == 1 & 
                       consent$house_no_old == "36"] <- "38"

#21		Kg Nelayan, Barombong	old=39, 41, new=39	Multiple "old" house numbers recorded for one "new" house number - are they merged houses?
# no, there was a return visit where old house number entered incorrectly
consent$house_no_old[consent$community == "Q" & consent$return_visit_yn == 1 & 
                       consent$house_no_old == "39"] <- "41"

#another return visit error
consent$house_no_old[consent$community == "Q" & consent$return_visit_yn == 1 & 
                       consent$house_no_old == "37"] <- "39"

#22		Kg Nelayan, Barombong	14	14, 15	So, did house_no_old get split into 2 houses? house_no_final = 14 and 15?????
# House no old 14 no change still house no (final) 14. 
# House no (final)=15 was skipped, vacant (not at home), not barcoded, sticker kept for future possible consent.
# this problem stemmed from an incorrectly entered settlement name - fixed in Zainal's fixes (above)


# 23. Kg Nelayan, Barombong	old=28	scan - 27, return visit = 1; fix house_no_old
# RETURN VISIT: house 28 becomes 27; house 29 becomes 28
#error in return visit
consent$house_no_old[consent$community == "Q" & consent$house_no_final == 28 & consent$today == "2018-08-09" 
                     & consent$return_visit_yn == 1 & consent$house_no_old == "28"] <- "29"

# 24.	Kg Nelayan, Barombong	37	35, 37	house_no_old=37 became house_no_final=35; 
# But, there is a return visit for house_no_final=37, showing old_house_no = 37;
# What should this be?	
# House no old 37 become house no (final) 35. 
# House no (final) 37 is merged from house no old 39 and 39A (see above)
# ?fixed above

# 25.	Kg Nelayan, Barombong	43	41, 43	So, is it house_no_old=43 became house_no_final = 41? 
# and house_no_old=46 became house_no_final = 43?	
# RETURN VISIT: house 43 becomes 41; house 46 becomes 43
# looks ok

# 26.	Kg Nelayan, Barombong	90	86, 90	There are multiple "new" house numbers for one "old" house number 
# - is that correct? is it house_no_old = 93A becomes house_no_final=90?	
# RETURN VISIT: house 90 becomes 86; house 93a becomes 90
#error in return visit
consent$house_no_old[consent$community == "Q" & consent$house_no_final == 90 & consent$today == "2018-08-09" 
                     & consent$return_visit_yn == 1 & consent$house_no_old == "90"] <- "93A"

# 27.	Kg Nelayan, Barombong	91	87, 91	return visit; 	
# RETURN VISIT: house 91 becomes 87; house 93 becomes 91
# return visit error
consent$house_no_old[consent$community == "Q" & consent$house_no_final == 91 & consent$today == "2018-08-09" 
                     & consent$return_visit_yn == 1 & consent$house_no_old == "91"] <- "93"

# 28.	Jl Barawaja 2, Pampang	13	13, 14	Did house 13 get split into house 13 and 14?	
# House no old 13 become house no (final) 14. House no (final) 13 is from house no old 12.
# mis-entry of old house number
consent$house_no_old[consent$community == "T" & consent$house_no_final == 13 & consent$today == "2018-08-09" 
                     & consent$return_visit_yn == 0 & consent$house_no_old == "13"] <- "12"

#29. 2 Aug 2018	Kawasan Untia, Untia	28.	This house had a bad barcode scan on 2 Aug 2018; 
# just checking the house numbers are correct; CFW was Hatta - House numbers are correct, date and CFW verified.
consent$barcode_id[consent$community == "P" & consent$house_no_final == 28 & consent$today == "2018-08-02" 
                     & consent$return_visit_yn == 0 & consent$house_no_old == "28"] <- "NP-HH-028"



# ##########################################
# also need a final house_no_old in order to identify merged houses
consent$house_no_old_final <- consent$house_no_old




check <- consent %>% 
  filter (community == "Q") %>% 
  select (today, house_no_old, house_no_old_final, house_no_final, barcode_scan, return_visit_yn, 
          barcode_id, community_name)

# From Icha's spreadsheet 20180906 (house status) - MERGED HOUSES

# Tunas Jaya, S
# consent$house_no_old_final[consent$community == "S" & consent$house_no_final == 28] <- "22 & 23" #?????check*******

# Barombong	Q
# old=39 & 39A, new=	37
consent$house_no_old_final[consent$community == "Q" & consent$house_no_old_final == "39"] <- "39 & 39A"
# old=35 & 36, new=	34
consent$house_no_old_final[consent$community == "Q" & consent$house_no_final == 34] <- "35 & 36"
# old=76 & 77, new=	75
consent$house_no_old_final[consent$community == "Q" & consent$house_no_final == 75] <- "76 & 77"
# old=95 & 97, new= 	93
consent$house_no_old_final[consent$community == "Q" & consent$house_no_final == 93] <- "95 & 97"
# old=78 & 79, new=	77
consent$house_no_old_final[consent$community == "Q" & consent$house_no_old_final == "78"] <- "78 & 79"
consent$house_no_old_final[consent$community == "Q" & consent$house_no_old_final == "79"] <- "78 & 79"

# Lempangang, Bulurokeng	N
# old=16 & 17 	 new=	15
consent$house_no_old_final[consent$community == "N" & consent$house_no_old_final == "16"] <- "16 & 17"
consent$house_no_old_final[consent$community == "N" & consent$house_no_old_final == "17"] <- "16 & 17"

# Ce'de, 	U
# old=62 & 63	new=	57
consent$house_no_old_final[consent$community == "U" & consent$house_no_old_final == "62-63"] <- "62 & 63"
# old=31 & 32	new=	31
consent$house_no_old_final[consent$community == "U" & consent$house_no_final == 31] <- "31 & 32"

# Bonelengga, 	R
# old=17 & 18	new=	16
consent$house_no_old_final[consent$community == "R" & consent$house_no_final == 16] <- "17 & 18"

# Baru, Antang	X
# old=76 & 78	new=	79
consent$house_no_old_final[consent$community == "X" & consent$house_no_final == 79] <- "76 & 78"
# old=72 & 83	new=	85
consent$house_no_old_final[consent$community == "X" & consent$house_no_final == 85] <- "72 & 83"

# Alla-Alla, 	Z
consent$house_no_old_final[consent$community == "Z" & consent$house_no_final == 28] <- "26 & 28"

# Untia	P
# old=91 & 92	 - not barcoded
# old=25 & 26	new=26
consent$house_no_old_final[consent$community == "P" & consent$house_no_final == 26] <- "25 & 26"

# Raya Baru, 	Y
# old=2A, 2B, 3B	new=3
consent$house_no_old_final[consent$community == "Y" & consent$house_no_final == 3] <- "2A, 2B & 3B"
# old=3 & 3A	new=4
consent$house_no_old_final[consent$community == "Y" & consent$house_no_final == 4] <- "3 & 3A"
# old=4 & 4A	new=5
consent$house_no_old_final[consent$community == "Y" & consent$house_no_final == 5] <- "4 & 4A"
# old=6 & 6B	new=7
consent$house_no_old_final[consent$community == "Y" & consent$house_no_final == 7] <- "6 & 6B"

# Pampang	T
# old=14 & 14A	new=15
consent$house_no_old_final[consent$community == "T" & consent$house_no_final == 15] <- "14 & 14A"
# old=18 & 18 A	new=20
consent$house_no_old_final[consent$community == "T" & consent$house_no_final == 20] <- "18 & 18A"

# Tallo	V - none merged
# Bambu-Bambu, W - none merged





# then need to fix house_no_old for "new" houses
consent$house_no_old_final <- if_else (consent$house_no_old_final == "New" | consent$house_no_old_final == "NEW", 
                                       paste ("NEW-", as.character(consent$house_no_final)), 
                                       consent$house_no_old_final)

check <- consent %>% 
  filter (community == "T") %>% 
  select (today, house_no_old, house_no_old_final, house_no_final, barcode_scan, return_visit_yn, 
          barcode_id, community_name, home_yn)
# ##########################################
# Fixes from Icha in spreadsheet "20180903_RISE_consent_2018_house_status_Icha.xlsx"
# revised spreadsheet: "20180906_RISE_consent_2018_house_status_Icha.xlsx"
# identifying vacant vs. not home

# Tunas Jaya
consent$home_yn[consent$today == "2018-07-30" & consent$house_no_final == "51" 
                & consent$community == "S"] <- 3

# BAROMBONG
consent$home_yn[consent$house_no_old_final == "10" & consent$community == "Q"] <- 4
consent$home_yn[consent$house_no_old_final == "15" & consent$community == "Q"] <- 4
consent$home_yn[consent$today == "2018-08-09" & consent$house_no_old_final == "38" 
                & consent$community == "Q"] <- 3
consent$home_yn[consent$house_no_old_final == "39 & 39A" & consent$community == "Q"] <- 3 #not at home
consent$home_yn[consent$house_no_old_final == "86" & consent$community == "Q"] <- 4
consent$home_yn[consent$house_no_old_final == "102" & consent$community == "Q"] <- 4
consent$home_yn[consent$today == "2018-08-09" & consent$house_no_old_final == "64A" 
                & consent$community == "Q"] <- 3

#Lempangang
consent$home_yn[consent$house_no_old_final == "15" & consent$community == "N"] <- 4

# TALLO
consent$home_yn[consent$today == "2018-07-12" & consent$house_no_old_final == "18" 
                & consent$community == "V"] <- 3

# Ce'de
consent$home_yn[consent$today == "2018-08-06" & consent$house_no_old_final == "4" 
                & consent$community == "U"] <- 3
consent$home_yn[consent$today == "2018-08-06" & consent$house_no_old_final == "27" 
                & consent$community == "U"] <- 3

#no - this was incorrect - should be a refusal
# consent$home_yn[consent$house_no_old_final == "29" & consent$community == "U"] <- 4
# consent$participate_yn[consent$house_no_old_final == "29" & consent$community == "U"] <- NA



# consent$home_yn[consent$today == "2018-08-06" & consent$house_no_old_final == "39" 
#                 & consent$community == "U"] <- 3 #I don't think it is right to change this

# BONELENGGA
consent$home_yn[consent$house_no_old_final == "16" & consent$community == "R"] <- 4

# consent$home_yn[consent$today == "2018-07-17" & consent$house_no_old_final == "40" 
#                 & consent$community == "U"] <- 3 #Icha said not home, but I don't think I can change from home to not home

# ANTANG
consent$home_yn[consent$today == "2018-07-26" & consent$house_no_old_final == "9" 
                & consent$community == "X"] <- 3

# consent$home_yn[consent$house_no_old_final == "104" & consent$community == "X"] <- 3  #Icha said not home, but I don't think I can change from home to not home
consent$home_yn[consent$house_no_old_final == "109" & consent$community == "X"] <- 3

# BAMBU-BAMBU
consent$home_yn[consent$house_no_old_final == "7" & consent$community == "W"] <- 3
consent$home_yn[consent$house_no_old_final == "9" & consent$community == "W"] <- 3
consent$home_yn[consent$house_no_old_final == "12" & consent$community == "W"] <- 4

# ALLA-ALLA
consent$home_yn[consent$house_no_old_final == "33" & consent$community == "Z"] <- 4
consent$home_yn[consent$house_no_old_final == "35" & consent$community == "Z"] <- 4
consent$home_yn[consent$house_no_old_final == "36" & consent$community == "Z"] <- 4

# UNTIA
consent$home_yn[consent$today == "2018-08-06" & consent$house_no_old_final == "31" 
                & consent$community == "P" & consent$return_visit_yn == 1] <- 3
consent$home_yn[consent$today == "2018-08-06" & consent$house_no_old_final == "71" 
                & consent$community == "P"] <- 3
consent$home_yn[consent$today == "2018-08-06" & consent$house_no_old_final == "77" 
                & consent$community == "P"] <- 3
consent$home_yn[consent$today == "2018-08-06" & consent$house_no_old_final == "78" 
                & consent$community == "P"] <- 3

# RAYA BARU
# consent$home_yn[consent$house_no_old_final == "17" & consent$community == "Y"] <- 3  #Icha said not home, but I don't think I can change from home to not home
consent$home_yn[consent$house_no_old_final == "18A" & consent$community == "Y"] <- 4

# PAMPANG (T)
# no fixes needed


check <- consent %>% 
  filter (community == "U") %>% 
  select (today, house_no_old, house_no_old_final, house_no, barcode_scan, return_visit_yn, 
          barcode_id, community_name, home_yn, house_no_final)


# Fixes from Icha in email on 31 Aug 2018; revisions from Jane in email 5 Sept 2018

# PAMPANG (T): House no old 30a is actually house no (final) = 35; House no old 30b is actually house no (final) = 36
# no, they confirmed that what was entered in surveyCTO is correct
# no changes required

# BAROMBONG (Q)
# These house no new should be "not at home": 24, 36, 37, 41, 95, 63 (barcodes installed) - fine
# These house no old should be "vacant": 10, 15, 86, 102, 71 (barcode not installed) - fine

# TUNAS JAYA:
#   These houses 
# Please note: Vacant vs. not at home status of houses were not always correctly labeled. CFW would mark
# as "vacant" houses were there was no one at home. Most of the time, if there is no barcode, the house is
# vacant. If the house is barcoded and there is no consent, means no one was at home for up to 3 visits.
# There are but very few cases whereby a house is vacant but CFW obtained permission from a neighbor to
# install a barcode. There are also but very few cases whereby a house has occupants but no one was at
# home for up to 3 visits, and CFW could not barcode the house. The above notes in Barombong and Tunas
# Jaya were an attempt to rectify these, but we have not checked the other sites. We will go through house by
# house in the remaining settlements next week Monday 
# Thank you and have a good weekend!!!!
# 
# 
# 
# 







