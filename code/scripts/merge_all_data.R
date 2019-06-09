#Merging all files into one
print("Script para fusionar los datos")
print(getwd())
dataset_cities <- read.csv("../../datasets/cities.csv")
cities <- dataset_cities$NAME
columns <-c("city","station","month","year","hour","houred","totinc","totdecr","medbikes","meanbikes","lastbikes","propempty","propfull","count")
result <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(result) <- columns
for(city in cities){# Each city
  setwd("../../datasets/bikes_agg_v2")
  dir <- list.files(pattern = city)
  for(d in dir){ # Each station
    setwd(d)
    print(getwd())
    file_name <- list.files()
    filename_splited <- strsplit(d, ":")
    station <- filename_splited[[1]][2]
    aux <- read.csv(file = file_name, header = TRUE, sep=",")
    aux <- cbind(station = station, aux)
    aux <- cbind(city = city, aux)
    result <- rbind(result, aux)
    setwd("..")
    print(getwd())
  }
  print(paste0("City ", city, " done!"))
}
setwd("../data_merged")
write.csv(result, "all_bicycle_data.csv", row.names = FALSE)
