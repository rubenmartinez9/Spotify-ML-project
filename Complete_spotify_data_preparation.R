####DOWNLOAD SPOTIFY DATA ----
#Packages we'll need.
#Install package spotifyr. No longer in CRAN, only in Github.
if (!require(spotifyr)) {
  devtools::install_github('JosiahParry/genius')
  library(spotifyr)
}
if (!require(httr)) {
  install.packages('httr')
  library(httr)
}
if (!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}

#Get your personal credentials. Go to devspoty in the following website
#https://developer.spotify.com/dashboard/applications
#Click on create app and log in. Give it a name (e.g. gater data) and
#Retrieve you ID and your secret ID and plug them in.
#These are mine
id <- 'ecad27467d6c48c09537ead3d99bb66e'
secret <- '83b99d41264f4bd9ad3a59014dc97aee'
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

#Technical stuff. Not even sure what it is about
response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(id, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)
mytoken = content(response)$access_token
HeaderValue = paste0('Bearer ', mytoken)
access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), 
                                         client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

#If you want to run it genre by genre, comment in the lines below
#and comment out lines 8, 23, 24 and 51.
#n <- 0 #start with n=0 and increase by 1 every genre
#genres <- "Pop" #go one by one changing genres 
#datalist <- list() # run this ONLY once at the begining

#Define genres be want to obtain data from
genres <- c("Pop", "Rock", "Rap", "Jazz", "EDM", "Metal", "Songwriter")
#Create databases per genre. Done in separate databases for several reasons, the main one 
#to not make too many queries at the same time
for (genre in genres){
  assign(paste(genre, "artists", sep = "_"), 
         (get_genre_artists(genre = genre, market = NULL, limit = 10,
                            offset = 0, authorization = get_spotify_access_token()))$name)
}

#Prepare empty datasets to store the features of the variables
for (genre in genres){
  assign(paste(genre, "database", sep = "_"), NULL) 
}

#Download features. Double loop (for each genre and for each artist)
n <- 0
datalist <- list()
for(genre in genres){ 
  for (artist in get(paste(genre, "artists", sep = "_"))){
    new_obs <- get_artist_audio_features(artist)[,c(1,2,7,9,10,11,12,13,14,15,16,17,18,19,20,22,26,30)]
    assign(paste(genre, "database", sep = "_"), rbind(get(paste(genre, "database", sep = "_")), new_obs))
  }
  #Add variable Genre
  #get(paste(genre, "database", sep = "_")) <- mutate(get(paste(genre, "database", sep = "_")),
  # genre = genre)
  assign(paste(genre, "database", sep = "_"), mutate(get(paste(genre, "database", sep = "_")),
                                                     genre = genre))
  #Remove text after " -" in the songs name (useful for downloading lyrics, since song's name is
  #one of the arguments)
  #Rock_database <- mutate(Rock_database,
  #track_name_fix = gsub(" -.*?$","", Rock_database$track_name))
  assign(paste(genre, "database", sep = "_"), mutate(get(paste(genre, "database", sep = "_")),
                                                     track_name = 
                                                       gsub(" -.*?$","", 
                                                            get(paste(genre, "database", sep = "_"))$track_name)))
  n <- n + 1
  datalist[[n]] <- get(paste(genre, "database", sep = "_"))
}  

#Merge it in a single database
spotify_data <- do.call("rbind", datalist)

#Write csv. Set your own directory (this path is with Mac. Widows starts with "C:")
write_csv(spotify_data, "/Users/rubenmartinezcuella/Desktop/Exam/spotify_data.csv")

####DOWNLOAD LYRICS ----
#import spotify database with 33k obs and load package genius (maybe you have to install it)
spotify_data <- read.csv("~/Desktop/Exam/spotify_data.csv")
library(assertr)
library(geniusr)
#subset you have to run
#we defined subsets to parallelize and make it faster. To do it all at once, change the index "8000"
#to "lenght(spotify_data)"
spotify_data_subset1 <- spotify_data[1:8000,]
#Lets do small subgoups to avoid it to get stuck
subset_list11 <- NULL
subset_list11 <- spotify_data_subset1 %>%
  group_by(artist_name, key, album_release_year, mode, time_signature, danceability) %>% nest() 

#lets make the 1st loop. The function get_lyrics_search is quite inconsistent. We will run it a few times
#to make sure we obtain as many lyrics as possible
# 1st go, 2nd , 3rd, etc
i <- 0 # runonly once per go (when it reaches iteration "nwrow(subset_list11)"). Press cntrl+enter many times (lines 37 to 50)
#when it says "Iteration X" run "i<-0" again, and then the for loop many times 
for (i in (i+1):nrow(subset_list11)){
  info <- (subset_list11$data[[i]]$artist_and_song)
  n <- 0
  print(paste("Iteration", i))
  for (j in info){
    n <- n+1 
    if((subset_list11$data[[i]]$lyrics[n])==""){
      subset_list11$data[[i]]$lyrics[n]<- get_lyrics_search(artist = gsub("wwwr.*?$", "", j),
                                                            song = gsub("^.*?wwwr", "", j))[,1] %>%
        assertr::col_concat()%>% 
        paste(collapse = " ") 
    }
  }
}

#When you get an error, you can run these two commands below to see the proportion of songs that have lyrics
#downloaded. It is increasing little by little. Don't think it will get above 50%.
spotify_data_subset1 <- subset_list11 %>% unnest(data)
#Count prportion of obs with lyrics
sum(spotify_data_subset1$lyrics !="", na.rm = T)/nrow(spotify_data_subset1)

#when you have iterated many times, write csv
#Write csv. Set your own directory (this path is with Mac. Widows starts with "C:"). Set your path
write_csv(spotify_data_subset1, "/Users/rubenmartinezcuella/Desktop/Exam/spotify_data_subset1.csv")


####IMPORT DATASET ----
spotify_total <- do.call("rbind", list(spotify_part1, spotify_part2, spotify_part3, spotify_part41))
sum(spotify_total$lyrics !="", na.rm = T)


####CLEAN DATASET----
#Delete erratic dates
cleandataset <- spotify_total[!((spotify_total$artist_name == "Nat King Cole" & spotify_total$album_release_year > 1965 ) | 
                                  (spotify_total$artist_name == "Vince Guaraldi Trio" & spotify_total$album_release_year > 1989 ) |
                                  (spotify_total$artist_name == "Ella Fitzgerald" & spotify_total$album_release_year > 1988 ) |
                                  (spotify_total$artist_name == "Louis Amstrong" & spotify_total$album_release_year > 1972 ) |
                                  (spotify_total$artist_name == "EWF" & spotify_total$album_release_year > 2016 )|
                                  (spotify_total$artist_name == "EWF" & spotify_total$album_release_year == 2008 )|
                                  (spotify_total$artist_name == "Perry Como" & spotify_total$album_release_year > 1998 )|
                                  (spotify_total$artist_name == "Diplo" & spotify_total$album_release_year == 2014 )|
                                  (spotify_total$artist_name == "The Beatles" )|
                                  (spotify_total$artist_name == "Slipknot" & spotify_total$album_release_year == 1999 )|
                                  (spotify_total$artist_name == "Tony Bennett" & spotify_total$album_release_year == 1994 )|
                                  (spotify_total$artist_name == "Tony Bennett" & spotify_total$album_release_year == 2006 )|
                                  (spotify_total$artist_name == "Tony Bennett" & spotify_total$album_release_year == 2016 )|
                                  (spotify_total$artist_name == "Tony Bennett" & spotify_total$album_release_year == 2019 )|
                                  (spotify_total$artist_name == "Willie Nelson" & spotify_total$album_release_year == 2004 )|
                                  (spotify_total$artist_name == "Willie Nelson" & spotify_total$album_release_year == 2005 )|
                                  (spotify_total$artist_name == "Queen" & spotify_total$album_release_year > 2004 )|
                                  (spotify_total$artist_name == "Drake" & spotify_total$album_release_year == 2019 )|
                                  (spotify_total$artist_name == "Taylor Swift" & spotify_total$album_release_year == 2018 )|
                                  (spotify_total$artist_name == "AC/DC" & spotify_total$album_release_year == 2012 )|
                                  (spotify_total$artist_name == "Elton John" & spotify_total$album_release_year == 2020 )|
                                  (spotify_total$artist_name == "Elton John" & spotify_total$album_release_year == 2008 )|
                                  (spotify_total$artist_name == "The Neighbourhood" & spotify_total$album_release_year == 2014 )|
                                  (spotify_total$artist_name == "Lil Uzi Vert" & spotify_total$album_release_year == 2015 )|
                                  (spotify_total$artist_name == "Lil Uzi Vert" & spotify_total$album_release_year == 2016 )|
                                  (spotify_total$artist_name == "Fleetwood Mac" & spotify_total$album_release_year > 2018 )|
                                  (spotify_total$artist_name == "Fleetwood Mac" & spotify_total$album_release_year == 1997 )),] 


#Dataset with lyrics wrong dates(7453)
raw3 <- (spotify_total[-which(duplicated(spotify_total$lyrics)),]) %>% na.omit()
raw3$track_id <- raw3$track_id %>% as.character()
raw3$track_name <- raw3$track_name %>% as.character()
raw3$artist_and_song <- NULL
raw3$lyrics <- raw3$lyrics %>% as.character()
str(raw3)

#Dataset without lyrics(11448)
raw2 <- (cleandataset[-which(duplicated(spotify_total$artist_and_song)),])[, -21] %>% na.omit()
raw2$track_id <- raw2$track_id %>% as.character()
raw2$track_name <- raw2$track_name %>% as.character()
raw2$artist_and_song <- NULL
str(raw2)

#Dataset with lyrics right dates(3929)
raw <- (cleandataset[-which(duplicated(spotify_total$lyrics)),]) %>% na.omit()
raw$track_id <- raw$track_id %>% as.character()
raw$track_name <- raw$track_name %>% as.character()
raw$artist_and_song <- NULL
raw$lyrics <- raw$lyrics %>% as.character()
str(raw)


####CLEAN LYRICS----
# Build corpus to clean
trial <- raw$lyrics
library(tm)
corpus <- iconv(trial, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))

# Clean text
corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

cleanset <- tm_map(corpus, stripWhitespace)

#Back to character vector
empty_database <- NULL
for(i in 1:length(cleanset)){
  empty_database[i] <- corpus[[i]]$content
}

####GET SENTIMENT----
# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Obtain sentiment scores, standardize and turn to 0 those that don't detect any emotion
lyric_sentiments <- get_nrc_sentiment(empty_database)
head(lyric_sentiments)
lyric_sentiments_std <- sweep(lyric_sentiments[,], 1, rowSums(lyric_sentiments[,]), FUN="/")
lyric_sentiments_std_na <- lyric_sentiments_std
lyric_sentiments_std_na[is.na(lyric_sentiments_std_na)] = 0

####MERGE WITH DATASET AND SAVE----
spotify_right_lyrics_right_dates <- cbind(raw, lyric_sentiments_std_na)
write_csv(spotify_right_lyrics_right_dates, "/Users/rubenmartinezcuella/Desktop/Exam/spotify_right_lyrics_right_dates.csv")
