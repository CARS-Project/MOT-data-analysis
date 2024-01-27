library(data.table)
library(dplyr)
library(furrr)

vis = readRDS("data/DVLA_VIS.Rds")
mot = readRDS("data/MOT_2023_main.Rds")

#summary(duplicated(vis$registrationNumber))
# Mode    FALSE 
# logical 52146673 
#summary(duplicated(mot$registration))
# Mode    FALSE     TRUE 
# logical 71195241   259088 

mot_sub = mot[mot$registration %in% vis$registrationNumber,]
names(mot_sub) = paste0("DVSA_",names(mot_sub))

# Pre-clean make
vis$make <- as.character(vis$make)

# Strip leading/trailing whitespace
vis$make_clean <- trimws(vis$make, which = "both")
mot_sub$DVSA_make_clean <- trimws(mot_sub$DVSA_make, which = "both")

#FOrce Upper Case
vis$make_clean <- toupper(vis$make_clean)
mot_sub$DVSA_make_clean <- toupper(mot_sub$DVSA_make_clean)

# Strip Punctuation except hyphen
vis$make_clean <- gsub("[^[:alnum:][:space:]-]", "", vis$make_clean)
mot_sub$DVSA_make_clean <- gsub("[^[:alnum:][:space:]-]", "", mot_sub$DVSA_make_clean)

# Repace hyphen with space
# vis$make <- gsub("-", " ", vis$make)
# mot_sub$DVSA_make <- gsub("-", " ", mot_sub$DVSA_make)

# Manual Approval of makes
approved_makes = read.csv("data/approved_makes.csv")
approved_makes = approved_makes[approved_makes$approved != "",]

rejected_makes = approved_makes[approved_makes$approved == "n",]
approved_makes = approved_makes[approved_makes$approved == "y",]

# Check approved_makes
for(i in 1:nrow(approved_makes)){
  if(any(stringi::stri_detect_regex(approved_makes$makes[i], paste0("^",approved_makes$makes[-i],"\\b")))){
    stop(approved_makes$makes[i]," matches another approved make ")
    approved_makes$makes[stringi::stri_detect_regex(approved_makes$makes[i], paste0("^",approved_makes$makes,"\\b"))]
  }
}

rejected_makes = rejected_makes[!duplicated(rejected_makes$makes),]

# Remove known typos e.g MERCEDES vs MERCEDES BENZ
clean1b = function(x){
  if(is.na(x)){
    return(x)
  }
  y = stringi::stri_detect_regex(x, paste0("^",rejected_makes$makes,"\\b"))
  if(any(y)){
    mtch = rejected_makes$makes[y]
    rep = rejected_makes$should_be[y]
    if(length(mtch) > 1){
      # Multiple Matches
      nch = nchar(mtch)
      nch = nch == max(nch)
      mtch = mtch[nch]
      rep = rep[nch]
    }
    x = stringi::stri_replace(str = x, 
                              replacement = rep,
                              regex = paste0("^",mtch,"\\b")
                              )
  }
  x
}

dvla_clean = data.frame(before = unique(vis$make_clean))
dvla_clean$after = purrr::map_chr(dvla_clean$before, clean1b, .progress = TRUE)
vis$make_clean = dvla_clean$after[match(vis$make_clean, dvla_clean$before)]


dvsa_clean = data.frame(before = unique(mot_sub$DVSA_make_clean))
dvsa_clean$after = purrr::map_chr(dvsa_clean$before, clean1b, .progress = TRUE)
mot_sub$DVSA_make_clean = dvsa_clean$after[match(mot_sub$DVSA_make_clean, dvsa_clean$before)]

# If starts with an approved make drop the rest
clean2 = function(x, appr){
  if(is.na(x)){
    return(x)
  }
  y = stringi::stri_detect_regex(x, paste0("^",appr,"\\b"))
  if(any(y)){
    x = appr[y]
  }
  x
}

dvla_clean = data.frame(before = unique(vis$make_clean))
dvla_clean$after = purrr::map_chr(dvla_clean$before, clean2, appr = approved_makes$makes, .progress = TRUE)
vis$make_clean = dvla_clean$after[match(vis$make_clean, dvla_clean$before)]

dvsa_clean = data.frame(before = unique(mot_sub$DVSA_make_clean))
dvsa_clean$after = purrr::map_chr(dvsa_clean$before, clean2, appr = approved_makes$makes, .progress = TRUE)
mot_sub$DVSA_make_clean = dvsa_clean$after[match(mot_sub$DVSA_make_clean, dvsa_clean$before)]

makes = c(vis$make_clean, mot_sub$DVSA_make_clean)
makes = as.data.frame(table(makes))
makes = makes[order(makes$Freq, decreasing = TRUE),]

makes = left_join(makes, approved_makes[,c("makes","approved")], by = "makes")
makes = makes[order(makes$approved),]

rejected_makes$Freq <- NA
makes$should_be = NA

makes = rbind(rejected_makes, makes)

write.csv(makes,"data/unique_makes_cleaned_DVSA_DVLA.csv", row.names = FALSE, na = "") #23,802 unique makes

summary(mot_sub$DVSA_make_clean %in% approved_makes$makes)
summary(vis$make_clean %in% approved_makes$makes)

stop("Do you want to run the slow joining process?")

summary(duplicated(vis$registrationNumber))
summary(duplicated(mot_sub$DVSA_registration))

mot_dups = mot_sub$DVSA_registration[duplicated(mot_sub$DVSA_registration)]
mot_dups = mot_sub[mot_sub$DVSA_registration %in% mot_dups, ]
# Looks like duplicates are due to cherished plates
#VIS only has most recent plate wile MOT history can have older plates.


#join = mot_sub[vis, on  = .(DVSA_registration = registrationNumber)]
join = vis[mot_sub, on  = .(registrationNumber = DVSA_registration)]



check = join[,c("DVSA_registration","DVSA_make","DVSA_make_clean","make","make_clean","DVSA_fuelType","fuelType",
                "DVSA_registrationDate","monthOfFirstRegistration","DVSA_primaryColour",
                "colour","DVSA_engineSize","engineCapacity")]
check$DVSA_primaryColour = toupper(check$DVSA_primaryColour)
check$DVSA_fuelType = toupper(check$DVSA_fuelType)

check$match_colour = check$DVSA_primaryColour == check$colour
check$match_fuelType = check$DVSA_fuelType == check$fuelType
check$match_engineSize = check$DVSA_engineSize == check$engineCapacity
check$match_date = paste0(lubridate::year(check$DVSA_registrationDate),"-",lubridate::month(check$DVSA_registrationDate)) == paste0(lubridate::year(check$monthOfFirstRegistration),"-",lubridate::month(check$monthOfFirstRegistration))
check$match_make = check$DVSA_make == check$make
check$match_make_clean = check$DVSA_make_clean == check$make_clean

check$match_colour[is.na(check$match_colour)] = FALSE
check$match_fuelType[is.na(check$match_fuelType)] = FALSE
check$match_engineSize[is.na(check$match_engineSize)] = FALSE
check$match_date[is.na(check$match_date)] = FALSE
check$match_make[is.na(check$match_make)] = FALSE
check$match_make_clean[is.na(check$match_make_clean)] = FALSE

check$match_count = rowSums(check[,c("match_colour","match_fuelType","match_engineSize","match_date","match_make","match_make_clean")])

check_no_match = check[check$match_count < 5,]

no_match_summary = check_no_match %>%
  group_by(DVSA_make_clean, make_clean, DVSA_fuelType, fuelType, DVSA_registrationDate, monthOfFirstRegistration, 
           DVSA_primaryColour, colour, DVSA_engineSize, engineCapacity,match_colour, match_fuelType, 
           match_engineSize, match_date, match_make_clean, match_count) %>%
  summarise(n_vehicles = n())

no_match_summary$make_dvla_in_dvsa = NULL

no_match_summary$make_dvsa_in_dvla = unlist(purrr::map2(no_match_summary$DVSA_make_clean, no_match_summary$make_clean, grepl, fixed = TRUE))
no_match_summary$make_dvla_in_dvsa = unlist(purrr::map2(no_match_summary$make_clean, no_match_summary$DVSA_make_clean, grepl, fixed = TRUE))
no_match_summary$make_partial_match = no_match_summary$make_dvsa_in_dvla | no_match_summary$make_dvla_in_dvsa

make_bad = no_match_summary[!no_match_summary$match_make_clean,]
make_good = no_match_summary[no_match_summary$match_make_clean,]

make_bad_summary = make_bad %>%
  group_by(DVSA_make_clean, make_clean,  make_dvsa_in_dvla, make_dvla_in_dvsa, make_partial_match, 
           match_count, match_colour, match_fuelType, match_engineSize, match_date) %>%
  summarise(n_vehicles = sum(n_vehicles))
make_bad_summary = make_bad_summary[order(make_bad_summary$n_vehicles, decreasing = TRUE),]




# Read in Ivo's matches

ivo = readxl::read_excel("D:/OneDrive - University of Leeds/Data/DVLA/Ivo/DVLA_DVSA tools 2023 11 02.xlsx",
                         col_types = "text")
summary(duplicated(ivo[,1:2]))

make_bad_summary = left_join(make_bad_summary, ivo, by = c("DVSA_make" = "DVSA", "make" = "DVLA"))

write.csv(make_bad_summary,"data/bad_match_make_DVSA_DVLA_summary.csv", row.names = FALSE, na = "")


