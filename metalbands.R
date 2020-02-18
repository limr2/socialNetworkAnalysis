#install.packages('statnet',repos = 'http://statnet.org') to install statnet on lab-machines
library(igraph)
artists <- read.csv("artists_20190223.csv",
                     header = TRUE)
bands <- read.csv("lineups_20190223.csv")


#Sample Artists in United States
US_idx <- which(artists[5] == 'United States')
US_artists <- artists[US_idx,]
#Sample Artists with nonempty Location
non_empty_idx <- which(US_artists[6] != '')
US_artists <- US_artists[non_empty_idx,]

#remove NA age from US_artists
US_artists <- US_artists[which(US_artists[3] != 'N/A'),]
#US_artists <- US_artists[which(is.na(US_artists[3])),]
#remove 0000-00-00 age from US_artists
US_artists <- US_artists[which(US_artists[3] != "0000-00-00"),]
#remove XXXX-00-00 age from US_artists
library(stringr)
US_artists <- US_artists[which(!stringr::str_detect(US_artists[,3], "00-00")),]
#remove XXX-XX-00 age from US_artists
US_artists <- US_artists[which(!stringr::str_detect(US_artists[,3],"-00")),]

#Retrieve artist ids and band ids
artist_id <- US_artists[,1]
band_id <- bands[,1]
#Sample US Bands
US_bands <- bands[which(band_id %in% artist_id),]
US_bands[1] <- US_bands[,1]*-1

#Convert Dataframe to Graph
g1 <- graph.data.frame(US_bands, directed = FALSE)

#Alter ArtistId
US_artists[1] <- US_artists[,1]*-1
#Add $type attribute to each vertex, if artist or band
g2 <- igraph::set.vertex.attribute(g1, "type", index = which(strtoi(V(g1)$name) > 0), value = "band")
g2 <- igraph::set.vertex.attribute(g2, "type", index = which(is.na(strtoi(V(g1)$name))), value = "band")
g2 <- igraph::set.vertex.attribute(g2, "type", index = which(strtoi(V(g2)$name) < 0), value = "artist")

artist_names <- US_artists[,2]
unique_band_ids <- unique(US_bands[,2])
band_names <- US_bands[match(unique_band_ids,US_bands[,2]),3]
names <- c( as.character(artist_names),  as.character(band_names))
#Add $name attribute to each vertex
V(g2)$name <- names

#setting artists sex
artist_sex<- c(as.character(US_artists[,4]), NA[1:11931])
V(g2)$sex <- artist_sex

#setting artists birthday
#artist_birthday<- c(as.character(US_artists[,3]), NA[1:11931])
#V(g2)$birthday <- artist_birthday

#setting artists age
library(eeptools)
dates <- as.Date(US_artists[,3], "%Y-%m-%d")
age_vector <- age_calc(dates, units = 'years')
age_vector <- round(age_vector)

artist_age <- c(as.character(age_vector), NA[1:11931])
V(g2)$age <- artist_age
#setting the ids for all artists and bands
ids <- c(artist_id, unique_band_ids)
V(g2)$id <- ids
#setting artist city
city <- c(as.character(US_artists[,6]), NA[1:11931])
V(g2)$city <- city
#V(g2)$type <- V(g2)$type == "band"
g2 <- bipartite_projection(g2, which = c("FALSE"))
#Plot graph
#fr <- layout_with_fr(g2, minx = rep(-Inf, vcount(g2)), maxx = rep(Inf, vcount(g2)), miny = rep(-Inf, vcount(g2)), maxy = rep(Inf, vcount(g2)))
plot(g2,vertex.size = log(igraph::degree(g2)), vertex.frame.color = NA, vertex.label = "", main = "Metal Artists and Bands", 
     vertex.color = c("blue","pink")[ 1+(igraph::get.vertex.attribute(g2, "sex")=="Female")], alpha = 80, layout = layout_with_fr(g2))

library(intergraph)
network <- asNetwork(g2)
library(statnet)
ga.base<-ergm(network~edges + nodematch("sex")) 
summary(ga.base)
