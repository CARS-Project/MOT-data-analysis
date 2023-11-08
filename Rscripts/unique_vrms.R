# Get list of unique VRMS
main = readRDS("data/MOT_2023_main.Rds")
vrms_old = readRDS("D:/OneDrive - University of Leeds/Data/CREDS Data/MOT API/VRMs.Rds")
main = main[,c("registration","firstUsedDate")]

summary(duplicated(main$registration))
# Mode    FALSE     TRUE 
# logical 71195241   259088

vrms_new = main[!main$registration %in% vrms_old$registration,]
summary(duplicated(vrms_new$registration))
#Mode    FALSE     TRUE 
#logical 23943754    29115

vrms_new = vrms_new[!duplicated(vrms_new$registration),]
vrms_new = vrms_new[order(vrms_new$firstUsedDate, decreasing = TRUE),]

# Break into batches
batch_size = 10000

n_batches <- ceiling(nrow(vrms_new) / batch_size)
vrm_batch <- dplyr::group_split(vrms_new, rep(1:n_batches,
                                              each = batch_size,
                                              length.out = nrow(vrms_new)))

# vrm_batch <- split(vrms_new, rep(1:n_batches,
#                              each = batch_size,
#                              length.out = length(vrms_new)))

for(i in 1:length(vrm_batch)){
  sub = vrm_batch[[i]]
  sub = sub$registration
  saveRDS(sub, file.path("D:/OneDrive - University of Leeds/Data/DVLA/VRMS/2023",paste0("VRMS_extra_batch_",i,"_of_",n_batches,".Rds") ))
}
