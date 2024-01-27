# VRMS that failed firs time in the VES, try again

vrms_missing  = readRDS("data/VRMs_missing_from_DVLA_first_pass.Rds")

# Break into batches
batch_size = 10000

n_batches <- ceiling(nrow(vrms_missing) / batch_size)
vrm_batch <- dplyr::group_split(vrms_missing, rep(1:n_batches,
                                              each = batch_size,
                                              length.out = nrow(vrms_missing)))

for(i in 1:length(vrm_batch)){
  sub = vrm_batch[[i]]
  sub = sub$registration
  saveRDS(sub, file.path("D:/OneDrive - University of Leeds/Data/DVLA/VRMS/2023_secondgo",paste0("VRMS_extra_batch_",i,"_of_",n_batches,".Rds") ))
}



vrms_missing  = readRDS("data/VRMs_missing_from_DVLA_second_pass.Rds")

# Break into batches
batch_size = 10000

n_batches <- ceiling(nrow(vrms_missing) / batch_size)
vrm_batch <- dplyr::group_split(vrms_missing, rep(1:n_batches,
                                                  each = batch_size,
                                                  length.out = nrow(vrms_missing)))

for(i in 1:length(vrm_batch)){
  sub = vrm_batch[[i]]
  sub = sub$registration
  saveRDS(sub, file.path("D:/OneDrive - University of Leeds/Data/DVLA/VRMS/2023_thirdgo",paste0("VRMS_extra_batch_",i,"_of_",n_batches,".Rds") ))
}
