makes = readLines("data/makes.txt")
makes = trimws(makes, "both")
makes = strsplit(makes, " ")

makes_first = sapply(makes, function(x){
  lth = length(x)
  y = x[seq(1,lth/2)]
  y = paste(y, collapse = " ")
})

makes_second = sapply(makes, function(x){
  lth = length(x)
  y = x[seq(lth/2 + 1,lth)]
  y = paste(y, collapse = " ")
})

makes_first = toupper(makes_first)
makes_second = toupper(makes_second)
summary(makes_second == makes_first)

makes_first[makes_second != makes_first]

makes_first = makes_first[order(makes_first)]
makes_first = gsub(" ","-", makes_first)

approved_makes = read.csv("data/approved_makes.csv")
approved_makes = approved_makes[approved_makes$approved != "",]
approved_makes = approved_makes[approved_makes$approved == "y",]

new_makes = makes_first[!makes_first %in% approved_makes$makes]
new_makes = unique(new_makes)

write.csv(new_makes, "data/makes_autotrader.csv")
