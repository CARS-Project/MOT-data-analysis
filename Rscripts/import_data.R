# Import batched data from API

path = "D:/OneDrive - University of Leeds/Data/DVLA/MOT/attempt_2"

fls_main = list.files(path, pattern = "_main_")
fls_tests = list.files(path, pattern = "_tests_")
fls_comments = list.files(path, pattern = "_comments_")


rem_dup_pages = function(x){
  # Check page numbers
  pgs = strsplit(x,"_pages_")
  pgs = sapply(pgs, `[[`, 2)
  pgs = gsub(".Rds","", pgs)
  pgs = strsplit(pgs,"-")
  pgs2 = lapply(pgs, function(x){seq(as.numeric(x[1]), as.numeric(x[2]))})
  pgs2 = unlist(pgs2, use.names = FALSE)
  pgs2 = pgs2[order(pgs2)]
  summary(pgs2)
  summary(pgs2 %in% 0:310000)
  summary(0:310000 %in% pgs2 )
  summary(duplicated(pgs2))
  
  dups = pgs2[duplicated(pgs2)]
  dups = dups[dups %% 100 == 0]
  # Some duplicated pages 27300 to 30900 in both batch 20230917  and 20230822 and 20230821
  
  pgs3 = sapply(pgs, paste, collapse = "-")
  
  x = x[!duplicated(pgs3)]
  x
}

fls_main = rem_dup_pages(fls_main)
fls_tests = rem_dup_pages(fls_tests)
fls_comments = rem_dup_pages(fls_comments)

main = list()
for(i in 1:length(fls_main)){
  if(i %% 100 == 0| i == 1){
    message(Sys.time()," ",i)
  }
  main[[i]] <- readRDS(file.path(path, fls_main[i]))
}
main <- data.table::rbindlist(main, fill = TRUE)
saveRDS(main, "data/MOT_2023_main.Rds")

tests = list()
for(i in 1:length(fls_tests)){
  if(i %% 100 == 0| i == 1){
    message(Sys.time()," ",i)
  }
  tests[[i]] <- readRDS(file.path(path, fls_tests[i]))
}
tests <- data.table::rbindlist(tests, fill = TRUE)
saveRDS(tests, "data/MOT_2023_tests.Rds")


comments = list()
for(i in 1:length(fls_comments)){
  if(i %% 100 == 0| i == 1){
    message(Sys.time()," ",i)
  }
  comments[[i]] <- readRDS(file.path(path, fls_comments[i]))
}
comments <- data.table::rbindlist(comments, fill = TRUE)
saveRDS(comments, "data/MOT_2023_comments.Rds")

