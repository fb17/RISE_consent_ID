

# 1. RECODE VALUES ******************
consent1[, 5:15] <- sapply (consent1[, 5:15], 
                            function(x) as.numeric (ifelse (x=="yes", 1, ifelse (x=="no", 0, "NA"))))
consent3[, 9:13] <- sapply (consent3[, 9:13], 
                            function(x) as.numeric (ifelse (x=="yes", 1, ifelse (x=="no", 0, "NA"))))

# 2. split surveyor number
consent <- consent %>% 
  separate (name_surveyor, c("surveyor1", "surveyor2"), " ")


consent$surveyor1 <- factor (consent$surveyor1,
                             levels = c (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 77),
                             labels = c ("Asmira", "Fitri", "Hamdan", "Idha", "Ihsan", "Ina", "Jane", 
                                         "Liza", "Maghfira", "Muhammad", "Noor", "Nur", "Rufika", 
                                         "Syaidah", "Zainal", "Rizaldi", "other")) 
consent$surveyor2 <- factor (consent$surveyor2,
                             levels = c (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 77),
                             labels = c ("Asmira", "Fitri", "Hamdan", "Idha", "Ihsan", "Ina", "Jane", 
                                         "Liza", "Maghfira", "Muhammad", "Noor", "Nur", "Rufika", 
                                         "Syaidah", "Zainal", "Rizaldi","other")) 

# 3. add shortened community names for tables
consent$community_name <- factor (consent$community_name,
                                  levels = c ("Kampung Lempangang, Bulurokeng", "Kawasan Untia, Untia",
                                              "Kampung Nelayan, Jalan Mallombassi, Barombong",
                                              "Kampung Bonelengga, Bulurokeng", "Kampung Tunas Jaya, Sudiang",
                                              "Jalan Barawaja 2, Pampang", "Kampung Ce'de, Bakung",
                                              "Kampung Gampangcayya, Tallo",
                                              "Kampung Bambu-Bambu, Jalan Birta, Manggala",
                                              "Kampung Baru, Antang", "Jalan Borong Raya Baru, Batua",
                                              "Kampung Alla-Alla, Lorong 2 Masjid Babussaadah, Batua"),
                                  labels = c ("Kg Lempangang", "Kawasan Untia", "Kg Nelayan, Barombong", "Kg Bonelengga",
                                              "Kg Tunas Jaya", "Jl Barawaja 2, Pampang", "Kg Cedde","Kg Gampangcayya, Tallo",
                                              "Kg Bambu-Bambu, Jl Birta", "Kg Baru, Antang", "Jl Borong Raya Baru", "Kg Alla-Alla")) 


comm.id <- data.frame (c ("N", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"),
                       c("Kg Lempangang", "Kawasan Untia", "Kg Nelayan, Barombong", "Kg Bonelengga",
                         "Kg Tunas Jaya", "Jl Barawaja 2, Pampang", "Kg Cedde","Kg Gampangcayya, Tallo",
                         "Kg Bambu-Bambu, Jl Birta", "Kg Baru, Antang", "Jl Borong Raya Baru", "Kg Alla-Alla"))
colnames(comm.id)[1] <- "comm.id"  #rename
colnames(comm.id)[2] <- "community_name"






# comm.id <- data.frame (c ("S0", "N", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), 
#                        c("Site Zero", "Kg Lempangang", "Kawasan Untia", "Kg Nelayan, Barombong", "Kg Bonelengga", 
#                          "Kg Tunas Jaya", "Jl Barawaja 2, Pampang", "Kg Cedde","Kg Gampangcayya, Tallo", 
#                          "Kg Bambu-Bambu, Jl Birta", "Kg Baru, Antang", "Jl Borong Raya Baru", "Kg Alla-Alla"))
# colnames(comm.id)[1] <- "comm.id"  #rename
# colnames(comm.id)[2] <- "community_name"


# fix age of children
consent.childnames$time7 <- ymd_hms(consent.childnames$time7)
consent.childnames$today <- as.Date (consent.childnames$time7)
consent.childnames$age_final2 <- round((consent.childnames$today - consent.childnames$dob)/365, digits = 1)
consent.childnames$today <- NULL
consent.childnames$age_final <- NULL
consent.childnames$age_final <- consent.childnames$age_final2
consent.childnames$age_final2 <- NULL