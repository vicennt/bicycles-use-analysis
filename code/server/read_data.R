#API
key <- readLines("api_key")

#Datasets
stations <- read.csv(file="../datasets/stations.csv", header=TRUE, sep=",")
cities <- read.csv(file="../datasets/cities.csv", header=TRUE, sep=",")
