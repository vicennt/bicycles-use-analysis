#Merging all files into one
dataset_cities <- read.csv("../../datasets/cities.csv")
cities <- dataset_cities$NAME
columns <-c("station","month","year","hour","houred","totinc","totdecr","medbikes","meanbikes","lastbikes","propempty","propfull","count")
for(city in cities){# Each city
  setwd("../../datasets/bikes_agg_v2")
  result <- data.frame(matrix(ncol = 13, nrow = 0))
  colnames(result) <- columns
  dir <- list.files(pattern = city)
  for(d in dir){ # Each station
    setwd(d)
    file_name <- list.files()
    filename_splited <- strsplit(d, ":")
    station <- filename_splited[[1]][2]
    aux <- read.csv(file = file_name, header = TRUE, sep=",")
    aux <- cbind(station = station, aux)
    result <- rbind(result, aux)
    setwd("..")
  }
  write.csv(result, paste0("../data_merged/cities/",city,"/",city,".csv"), row.names = FALSE)
  print(paste0("City ", city, " done!"))
}
print("Files merged!")
