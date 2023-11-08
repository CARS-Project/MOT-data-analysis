library(dplyr)
library(data.table)

# Load Anoaymised MOT Data

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


# summary(duplicated(car_data[,c("mot_make","mot_model","mot_colour","mot_fuel_type",
#                               "mot_cylinder_capacity","mot_first_use_date",
#                               "t1_date","t2_date","t3_date","t4_date","t5_date",
#                               "t1_mileage","t2_mileage","t3_mileage","t4_mileage","t5_mileage")]))
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

# summary(duplicated(dvsa_car_data[,c("dvsa_make","dvsa_model","dvsa_firstUsedDate","dvsa_fuelType",
#                                     "dvsa_primaryColour","dvsa_engineSize","dvsa_message",
#                                     "t1_date","t2_date","t3_date","t4_date","t5_date","t1_mileage",
#                                     "t2_mileage","t3_mileage","t4_mileage","t5_mileage")]))
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


#summary(is.na(test_join$mot_vehicle_id))
#Mode    FALSE     TRUE 
#logical 37,733,101 30,430,400 #55% success rate

test_join_good = test_join[!is.na(test_join$mot_vehicle_id),]
saveRDS(test_join_good,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/join_dvsa_anoymised_round1_strong_match.Rds")

dvsa_car_data_r2 = dvsa_car_data[!dvsa_car_data$dvsa_vehicleId %in% test_join_good$dvsa_vehicleId, ]
car_data_r2 = car_data[!car_data$mot_vehicle_id %in% test_join_good$mot_vehicle_id, ]

# Make into groups
dvsa_r2_lst = group_by(dvsa_car_data_r2, dvsa_make, dvsa_firstUsedDate) %>%
  group_split()
names(dvsa_r2_lst) = sapply(dvsa_r2_lst, function(x){
  paste0(x$dvsa_make[1]," ",x$dvsa_firstUsedDate[1])
})

#summary(sapply(dvsa_r2_lst, nrow))
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#     1.0      1.0      2.0     34.8      8.0 412281.0

mot_r2_lst = group_by(car_data_r2, mot_make, mot_first_use_date) %>%
  group_split()
names(mot_r2_lst) = sapply(mot_r2_lst, function(x){
  paste0(x$mot_make[1]," ",x$mot_first_use_date[1])
})

summary(sapply(mot_r2_lst, nrow))

saveRDS(mot_r2_lst,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/anoymised_round1_unjoined.Rds")
saveRDS(dvsa_r2_lst,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/dvsa_round1_unjoined.Rds")

stop()

nms_all = unique(c(names(dvsa_r2_lst), names(mot_r2_lst)))
nms_all = nms_all[order(nms_all)]

combined_r2_lst = list()








saveRDS(test_join,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/test_join_dvsa_anoymised.Rds")
message(Sys.time())





stop()

failed_ids = test_join$vehicleId[is.na(test_join$vehicle_id)]

dvsa_car_data_fail = dvsa_car_data[dvsa_car_data$vehicleId %in% failed_ids,]

names(car_data) = paste0("mot_",names(car_data))
names(dvsa_car_data) = paste0("dvsa_",names(dvsa_car_data))
test_join = left_join(test_join, car_data, by = c("vehicle_id" = "mot_vehicle_id"))
test_join = left_join(test_join, dvsa_car_data, by = c("vehicleId" = "dvsa_vehicleId"))

saveRDS(test_join,"D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/test_join_dvsa_anoymised_vehicle_data.Rds")

summary(test_join$dvsa_make == test_join$mot_make)
summary(test_join$dvsa_model == test_join$mot_model)
summary(test_join$dvsa_primaryColour == test_join$mot_colour)

sub = unique(test_join[,c("dvsa_make","mot_make","dvsa_model","mot_model","dvsa_primaryColour","mot_colour")])
