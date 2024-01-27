library(dplyr)
library(data.table)

# Load Anouymised MOT Data

car_data <- readRDS("D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/car_data_2005_2021.Rds")
test_data <- readRDS("D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/test_data_2005_2021.Rds")

if(FALSE){
  test_data_summary = test_data %>%
    group_by(vehicle_id) %>%
    arrange(desc(test_date), .by_group = TRUE) %>%
    summarise(t1_date = test_date[1],
              t2_date = test_date[2],
              t3_date = test_date[3],
              t4_date = test_date[4],
              t5_date = test_date[5],
              t1_mileage = test_mileage[1],
              t2_mileage = test_mileage[2],
              t3_mileage = test_mileage[3],
              t4_mileage = test_mileage[4],
              t5_mileage = test_mileage[5]
    )
  
  saveRDS(test_data_summary,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/most_recent_test_dates.Rds")
} else{
  test_data_summary = readRDS("D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/most_recent_test_dates.Rds")
}

# Join on the car and test summary
names(car_data) = paste0("mot_",names(car_data))
car_data = left_join(car_data, test_data_summary, by = c("mot_vehicle_id" = "vehicle_id"))


summary(duplicated(car_data[,c("mot_make","mot_model","mot_colour","mot_fuel_type",
                              "mot_cylinder_capacity","mot_first_use_date",
                              "t1_date","t2_date","t3_date","t4_date","t5_date",
                              "t1_mileage","t2_mileage","t3_mileage","t4_mileage","t5_mileage")]))
# Mode    FALSE       TRUE 
# logical 67,268,306    23,874 
# Most duplicates are All NAs

# Load the DVSA data
dvsa_test_data = readRDS("data/MOT_2023_tests.Rds")
dvsa_car_data = readRDS("data/MOT_2023_main.Rds")

if(FALSE){
  dvsa_test_data$test_date = as.Date(dvsa_test_data$completedDate)
  dvsa_test_data = dvsa_test_data[dvsa_test_data$test_date <= lubridate::ymd("2021-12-30"),]
  dvsa_test_data$test_mileage = round(dplyr::if_else(dvsa_test_data$odometerUnit == "km", 
                                                     dvsa_test_data$odometerValue * 0.621371, 
                                                     dvsa_test_data$odometerValue))
  
  
  dvsa_test_data_summary = dvsa_test_data %>%
    group_by(vehicleId) %>%
    arrange(desc(test_date), .by_group = TRUE) %>%
    summarise(t1_date = test_date[1],
              t2_date = test_date[2],
              t3_date = test_date[3],
              t4_date = test_date[4],
              t5_date = test_date[5],
              t1_mileage = test_mileage[1],
              t2_mileage = test_mileage[2],
              t3_mileage = test_mileage[3],
              t4_mileage = test_mileage[4],
              t5_mileage = test_mileage[5]
    )
  saveRDS(dvsa_test_data_summary,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/dvsa_most_recent_test_dates.Rds")
} else {
  dvsa_test_data_summary = readRDS("D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/dvsa_most_recent_test_dates.Rds")
}

names(dvsa_car_data) = paste0("dvsa_",names(dvsa_car_data))
dvsa_car_data = left_join(dvsa_car_data, dvsa_test_data_summary, by = c("dvsa_vehicleId" = "vehicleId"))

summary(duplicated(dvsa_car_data[,c("dvsa_make","dvsa_model","dvsa_firstUsedDate","dvsa_fuelType",
                                    "dvsa_primaryColour","dvsa_engineSize","dvsa_message",
                                    "t1_date","t2_date","t3_date","t4_date","t5_date","t1_mileage",
                                    "t2_mileage","t3_mileage","t4_mileage","t5_mileage")]))
#    Mode    FALSE     TRUE 
# logical 68,663,569  2,790,760


# Remove Dups
mot_names = c("mot_make","mot_model","mot_colour","mot_fuel_type",
              "mot_cylinder_capacity","mot_first_use_date",
              "t1_date","t2_date","t3_date","t4_date","t5_date",
              "t1_mileage","t2_mileage","t3_mileage","t4_mileage","t5_mileage")

mot_dup = car_data[, ..mot_names]
mot_dup = mot_dup[duplicated(mot_dup),]
mot_dup = unique(mot_dup)
mot_dup$mot_dup = TRUE
car_data = left_join(car_data, mot_dup, by = mot_names)
car_data = car_data[is.na(car_data$mot_dup),]
car_data$mot_dup = NULL

dvsa_names = c("dvsa_make","dvsa_model","dvsa_firstUsedDate","dvsa_fuelType",
                 "dvsa_primaryColour","dvsa_engineSize","dvsa_message",
                 "t1_date","t2_date","t3_date","t4_date","t5_date","t1_mileage",
                 "t2_mileage","t3_mileage","t4_mileage","t5_mileage")


dvsa_dup = dvsa_car_data[, ..dvsa_names]
dvsa_dup = dvsa_dup[duplicated(dvsa_dup),]
dvsa_dup = unique(dvsa_dup)
dvsa_dup$dvsa_dup = TRUE
dvsa_car_data = left_join(dvsa_car_data, dvsa_dup, by = dvsa_names)
dvsa_car_data = dvsa_car_data[is.na(dvsa_car_data$dvsa_dup),]
dvsa_car_data$dvsa_dup = NULL




# Do a strong join - very good match

test_join = left_join(dvsa_car_data, car_data, 
                      by = c("dvsa_make" = "mot_make",
                             "dvsa_firstUsedDate" = "mot_first_use_date",
                             "dvsa_engineSize" = "mot_cylinder_capacity",
                             "t1_date" = "t1_date",
                             "t2_date" = "t2_date",
                             "t3_date" = "t3_date",
                             "t4_date" = "t4_date",
                             "t5_date" = "t5_date",
                             "t1_mileage" = "t1_mileage",
                             "t2_mileage" = "t2_mileage",
                             "t3_mileage" = "t3_mileage",
                             "t4_mileage" = "t4_mileage",
                             "t5_mileage" = "t5_mileage"))


summary(is.na(test_join$mot_vehicle_id))
#Mode    FALSE     TRUE 
#logical 37,733,101 30,430,400 #55% success rate

test_join_good = test_join[!is.na(test_join$mot_vehicle_id),]
saveRDS(test_join_good,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/join_dvsa_anoymised_round1_strong_match.Rds")

dvsa_car_data_r2 = dvsa_car_data[!dvsa_car_data$dvsa_vehicleId %in% test_join_good$dvsa_vehicleId, ]
car_data_r2 = car_data[!car_data$mot_vehicle_id %in% test_join_good$mot_vehicle_id, ]

summary(duplicated(dvsa_car_data_r2[,c("t1_date","t2_date","t3_date","t4_date",
                                       "t5_date","t1_mileage","t2_mileage","t3_mileage","t4_mileage",
                                       "t5_mileage")]))

# foo = dvsa_car_data_r2[duplicated(dvsa_car_data_r2[,c("t1_date","t2_date","t3_date","t4_date",
#                                                       "t5_date","t1_mileage","t2_mileage","t3_mileage","t4_mileage",
#                                                       "t5_mileage")]), ]

# Try a join just on dates
# test_join2 = left_join(dvsa_car_data_r2, car_data_r2, 
#                       by = c("dvsa_firstUsedDate" = "mot_first_use_date",
#                              "t1_date" = "t1_date",
#                              "t2_date" = "t2_date",
#                              "t3_date" = "t3_date",
#                              "t4_date" = "t4_date",
#                              "t5_date" = "t5_date",
#                              "t1_mileage" = "t1_mileage",
#                              "t2_mileage" = "t2_mileage",
#                              "t3_mileage" = "t3_mileage",
#                              "t4_mileage" = "t4_mileage",
#                              "t5_mileage" = "t5_mileage"))
# 
# summary(is.na(test_join2$mot_vehicle_id))
#Mode    FALSE     TRUE 
#logical   876174 29562380 #55% success rate

# Take and Example
# ex_dvsa = dvsa_car_data_r2[3,]
# ex_mot = car_data_r2[car_data_r2$mot_make == ex_dvsa$dvsa_make,]
# ex_mot = ex_mot[ex_mot$mot_first_use_date == ex_dvsa$dvsa_firstUsedDate, ]
# ex_mot = ex_mot[ex_mot$mot_model == ex_dvsa$dvsa_model, ]

# Memory Issues
rm(car_data, dvsa_car_data, dvsa_dup, mot_dup, dvsa_test_data_summary, test_data_summary ,test_join, test_join_good)
gc()

# Make into groups
dvsa_r2_lst = group_by(dvsa_car_data_r2, dvsa_make, dvsa_firstUsedDate) %>%
  group_split()
names(dvsa_r2_lst) = sapply(dvsa_r2_lst, function(x){
  paste0(x$dvsa_make[1]," ",x$dvsa_firstUsedDate[1])
})

summary(sapply(dvsa_r2_lst, nrow))
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#     1.0      1.0      2.0     34.8      8.0 412281.0

saveRDS(dvsa_r2_lst,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/dvsa_round1_unjoined.Rds", compress = FALSE)
rm(dvsa_car_data_r2)

car_data_r2$mot_make <- as.character(car_data_r2$mot_make)
car_data_r2$mot_model <- as.character(car_data_r2$mot_model)

mot_r2_lst = group_by(car_data_r2, mot_make, mot_first_use_date) %>%
  group_split()
names(mot_r2_lst) = sapply(mot_r2_lst, function(x){
  paste0(x$mot_make[1]," ",x$mot_first_use_date[1])
})


sizes_mot = sapply(mot_r2_lst[1:10], object.size)
sizes_dvsa = sapply(dvsa_r2_lst[1:10], object.size)

saveRDS(mot_r2_lst,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/anoymised_round1_unjoined.Rds", compress = FALSE)
summary(sapply(mot_r2_lst, nrow))


nms_all = unique(c(names(dvsa_r2_lst), names(mot_r2_lst)))
nms_all = nms_all[order(nms_all)]

combined_r2_lst = list()

for(i in seq_len(length(nms_all))){
  message(nms_all[i])
  mot = try(mot_r2_lst[[nms_all[i]]], silent = T)
  dvsa = try(dvsa_r2_lst[[nms_all[i]]], silent = T)
  if(inherits(mot,"try-error")){
    mot = NULL
  }
  if(inherits(dvsa,"try-error")){
    dvsa = NULL
  }
  
  combined_r2_lst[[i]] = list(mot = mot,
                              dvsa = dvsa)
  rm(mot, dvsa)
  
}

message(Sys.time())
saveRDS(combined_r2_lst,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/anoymised_dvsa_unjoined_paired_list.Rds")
message(Sys.time())

combined_r2_lst_sizes = sapply(combined_r2_lst, object.size, USE.NAMES = FALSE)

zigzag_sort <- function(x, n = 34){
  sortvec <- rep(c(seq(1,n),seq(n, 1)), length = length(x))
  sortvec <- order(sortvec)
  sortvec
}

combined_r2_lst = combined_r2_lst[zigzag_sort(combined_r2_lst_sizes)]
rm(combined_r2_lst_sizes)

partial_match = function(x){
  
  dvsa = x$dvsa
  mot = x$mot
  
  if(is.null(dvsa) | is.null(mot)){
    return(NULL)
  }
  
  mot_dates = as.matrix(mot[,c("t1_date", "t2_date", "t3_date", "t4_date", "t5_date")])
  mot_mileage = as.matrix(mot[,c("t1_mileage", "t2_mileage", "t3_mileage", "t4_mileage", "t5_mileage")])
  
  res = list()
  
  for(i in seq(1, nrow(dvsa))){
    dvsa_sub = dvsa[i,]
    dvsa_sub_dates = unique(c(dvsa_sub$t1_date, dvsa_sub$t2_date, dvsa_sub$t3_date, dvsa_sub$t4_date, dvsa_sub$t5_date))
    dvsa_sub_dates = as.character(dvsa_sub_dates)
    dvsa_sub_mileage = unique(c(dvsa_sub$t1_mileage, dvsa_sub$t2_mileage, dvsa_sub$t3_mileage, dvsa_sub$t4_mileage, dvsa_sub$t5_mileage))
    
    counts_dates = lapply(dvsa_sub_dates, function(y){
      z <- +(mot_dates == y)
      z[is.na(z)] <- 0
      z
    })
    
    counts_dates = Reduce('+',counts_dates)
    counts_dates = rowSums(counts_dates)
    
    counts_mileage = lapply(dvsa_sub_mileage, function(y){
      z <- +(mot_mileage == y)
      z[is.na(z)] <- 0
      z
    })
    
    counts_mileage = Reduce('+',counts_mileage)
    counts_mileage = rowSums(counts_mileage)
    
    counts_df <- data.frame(dvsa_vehicleId = dvsa_sub$dvsa_vehicleId, 
                            mot_vehicle_id = mot$mot_vehicle_id,
                            match = counts_dates + counts_mileage)
    counts_df <- counts_df[counts_df$match > 6,]
    res[[i]] <- counts_df
    
  }
  
  res = dplyr::bind_rows(res)
  res = res[order(res$match, res$dvsa_vehicleId, decreasing = TRUE),]
  res$duplicated = duplicated(res$dvsa_vehicleId)
  
  return(res)
}


future::plan("future::multisession", workers = 34)
match_summary <- furrr::future_map(combined_r2_lst,
                                     partial_match,
                                     .progress = TRUE)
future::plan("sequential")

message(Sys.time())
saveRDS(match_summary,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/anoymised_dvsa_match_summary_list.Rds")
message(Sys.time())

match_summary_df <- data.table::rbindlist(match_summary)
message(Sys.time())
saveRDS(match_summary_df,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/anoymised_dvsa_match_summary.Rds")
message(Sys.time())

match_summary_df = match_summary_df[!match_summary_df$duplicated,]

summary(duplicated(dvsa_car_data$dvsa_registration))
dvsa_car_data_r2 = dvsa_car_data[dvsa_car_data$dvsa_registration %in% match_summary_df$dvsa_registration,]

