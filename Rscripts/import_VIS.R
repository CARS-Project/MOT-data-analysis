# Import VIS data

path = "D:/OneDrive - University of Leeds/Data/DVLA/VIS/"
fls = list.files(path, recursive = TRUE)

vis = list()
for(i in 1:length(fls)){
  if(i %% 100 == 0| i == 1){
    message(Sys.time()," ",i)
  }
  vis[[i]] <- readRDS(file.path(path, fls[i]))
}
vis <- data.table::rbindlist(vis, fill = TRUE)

# summary(duplicated(vis$registrationNumber))
# dup <- vis$registrationNumber[duplicated(vis$registrationNumber)]
# dup <- vis[vis$registrationNumber %in% dup,]
# summary(duplicated(dup))
# uni = unique(dup)
# uni$dup = duplicated(uni$registrationNumber)
# uni = uni[order(uni$registrationNumber),]

vis = vis[!duplicated(vis$registrationNumber),]

vis$taxStatus <- as.factor(vis$taxStatus)
vis$taxDueDate <- lubridate::ymd(vis$taxDueDate)
vis$motStatus <- as.factor(vis$motStatus)
vis$make <- as.factor(vis$make)
vis$fuelType <- as.factor(vis$fuelType)
vis$colour <- as.factor(vis$colour)
vis$typeApproval <- as.factor(vis$typeApproval)
vis$dateOfLastV5CIssued <- lubridate::ymd(vis$dateOfLastV5CIssued)
vis$motExpiryDate <- lubridate::ymd(vis$motExpiryDate)
vis$wheelplan <- as.factor(vis$wheelplan)
vis$monthOfFirstRegistration <- lubridate::ym(vis$monthOfFirstRegistration)
vis$monthOfFirstDvlaRegistration <- lubridate::ym(vis$monthOfFirstDvlaRegistration)
vis$artEndDate <- lubridate::ymd(vis$artEndDate)
vis$euroStatus <- as.factor(vis$euroStatus)
vis$realDrivingEmissions <- as.factor(vis$realDrivingEmissions)

saveRDS(vis, "data/DVLA_VIS.Rds")

# Missing VRMS

vrms_2023 = readRDS("data/MOT_2023_main.Rds")
vrms_2023 = vrms_2023[,c("registration","firstUsedDate")]
vrms_2018 = readRDS("D:/OneDrive - University of Leeds/Data/CREDS Data/MOT API/VRMs.Rds")

vrms_2023 = vrms_2023[!vrms_2023$registration %in% vis$registrationNumber, ]
vrms_2018 = vrms_2018[!vrms_2018$registration %in% vis$registrationNumber, ]

vrms_missing = rbind(vrms_2023, vrms_2018)
vrms_missing = vrms_missing[!duplicated(vrms_missing$registration),]

saveRDS(vrms_missing, "data/VRMs_missing_from_DVLA_first_pass.Rds")

summary(duplicated(main$registration))

