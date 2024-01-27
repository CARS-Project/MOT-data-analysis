library(dplyr)
library(data.table)

#load Joining Summaries
join_r1 = readRDS("D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/join_dvsa_anoymised_round1_strong_match.Rds")
join_r2 = readRDS("D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/anoymised_dvsa_match_summary.Rds")

#Load Original Data
car_data <- readRDS("D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/car_data_2005_2021.Rds")
dvsa_car_data <- readRDS("data/MOT_2023_main.Rds")

join_r1 = join_r1[,c("dvsa_vehicleId","mot_vehicle_id")]
join_r2 = join_r2[!join_r2$duplicated,]
join_r2 = join_r2[,c("dvsa_vehicleId","mot_vehicle_id")]

join = rbind(join_r1, join_r2)

car_data_unjoined = car_data[!car_data$vehicle_id %in% join$mot_vehicle_id,]
dvsa_car_data_unjoined = dvsa_car_data[!dvsa_car_data$vehicleId %in% join$dvsa_vehicleId,]

# Make into groups
dvsa_r3_lst = group_by(dvsa_car_data_unjoined, make, firstUsedDate) %>%
  group_split()
names(dvsa_r3_lst) = sapply(dvsa_r3_lst, function(x){
  paste0(x$make[1]," ",x$firstUsedDate[1])
})

summary(sapply(dvsa_r3_lst, nrow))

car_data_unjoined$make <- as.character(car_data_unjoined$make)
car_data_unjoined$model <- as.character(car_data_unjoined$model)

mot_r3_lst = group_by(car_data_unjoined, make, first_use_date) %>%
  group_split()
names(mot_r3_lst) = sapply(mot_r3_lst, function(x){
  paste0(x$make[1]," ",x$first_use_date[1])
})




