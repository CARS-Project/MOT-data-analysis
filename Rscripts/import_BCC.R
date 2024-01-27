#
library(sf)
library(tmap)
tmap_mode("view")
dat1 = readxl::read_excel("D:/OneDrive - University of Leeds/Data/DVLA/BCC/NumberPlatesMITAI_20231111.xlsx")
dat1 = dat1[!is.na(dat1$plate),]


dat2 = readxl::read_excel("D:/OneDrive - University of Leeds/Data/DVLA/BCC/NumberPlatesPWA_20231111.xlsx")
dat3 = readxl::read_excel("D:/OneDrive - University of Leeds/Data/DVLA/BCC/NumberPlatesPWA_20231125.xlsx")
dat4 = readxl::read_excel("D:/OneDrive - University of Leeds/Data/DVLA/BCC/NumberPlatesPWA_20231215.xlsx")
dat1$Timestamp <- NULL

dat = rbind(dat1, dat2, dat3, dat4)
dat = dat[!is.na(dat$latitude),]
dat = dat[!grepl("^TEST",dat$plate),]

dat = st_as_sf(dat, coords = c("longitude","latitude"), crs = 4326)
qtm(dat, dots.col = "parking_type")

qtm(dat, dots.col = "id")

summary(duplicated(dat))
