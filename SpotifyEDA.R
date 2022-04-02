
library(ggplot2) 
library(readr) 
library(dplyr)



SpotifyTop50 <- read_csv("SpotifyTop50.csv")

head(SpotifyTop50,10)



#Dataset name: TOP 50 SPOTIFY SONGS - 2019  

#This dataset is describing the top 50 most listened songs around the world in 2019 from Spotify.   
#Columns:   
#•	ID   
#•	Track.Name - name of song  
#•	Artist.Name - name of artist  
#•	Genre - genre of song   
#•	Beats.Per.Minute - BPM of song  
#•	Energy  
#•	Danceability  
#•	Loudness..dB.  
#•	Liveness  
#•	Valence  
#•	Length - how long the song is  
#•	Acousticness  
#•	Popularity - how popular the song is 

#I did not have to tidy the dataset as the columns i used had tidy data.  

#Plot 1: Genre of songs in Top 50   
#This plot shows the distribution of different genres in the Top 50 songs. 
#We can see that most songs in the Top 50 are of the Genre “Dance pop” and “Pop”. 

genre = SpotifyTop50 %>% group_by(Genre) %>% summarise(Count = n())

ggplot(aes(x = Genre , y = Count , fill=Count) , data=genre) +
  geom_bar(colour='black',stat='identity') + theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) )+ 
  labs(title = "Genre Count", 
       subtitle = "Genre of songs in Top 50",
       x = "Genre", 
       y = "Count",
       fill = "Count") +
  scale_fill_distiller(palette = 'Greens')


#Plot 2: Top Artists  
#This plot shows which Artists are dominating the chart. 
#It gives us all the artists’ names who have more than 1 song of the chart. 
#Here we can see that in 2019, Ed Sheeran had the most number of his songs being listened to on Spotify. 


top_artists <- SpotifyTop50 %>%
  group_by(Artist)  %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance > 1) %>%
  arrange(desc(n_apperance))

ggplot(top_artists, aes(x = Artist, y = n_apperance, fill = n_apperance)) +
  geom_bar(color = 'black', stat = "identity", width = 0.6 ) + theme_bw()+
  labs(title = "Top Artists", 
       subtitle = "Artists with more than 1 song in Top 50",
       x = "Artists", 
       y = "Number of Apperance on the Top 50",
       fill = "Count") +
  coord_flip() + 
  
  scale_fill_distiller(palette = 'Greens') 


#Plot 3: Ed Sheeran Songs in Top 50  
#As we know from a previous plot that Ed Sheeran had the most number of songs on the chart, 
#this plot gives us a brief analysis of his songs which appeared on the chart. 
#We see the relation between Beats Per Minute and Energy for his songs. 
#Here we can see that his song “Antisocial” has a high BPM and also is high on energy. 
#Whereas, his song “Cross Me” even though is high on Energy, has a relatively smaller BPM. 



ed_sheeran <- SpotifyTop50 %>%
  filter(Artist == "Ed Sheeran")

Ed_Plot <- ed_sheeran %>% ggplot(aes(x = Song, y = BPM, color = Energy)) + 
  geom_point(alpha = 0.7, size = 4) + theme_bw() +
  labs(title = "Ed Sheeran Songs in Top 50", 
       subtitle = "analysis of their BPM and Energy", 
       y = "Beats per Minute(BPM)", x = "Ed Sheeran Song in Top 50", fill = "Energy" )+
  theme(axis.text.x = element_text(angle=30,hjust=1)) + 
  scale_colour_gradient(low = "light green", high = "black")

Ed_Plot


#Plot 4: How Danceability affects Popularity  
#In this plot, I choose the most popular Genres in the chart(which appear the highest number of times) 
#and plotted a graph of how in each genre, the danceability affects the popularity of the song. 
#In each Genre, except for Pop, we can see that more the danceability of the song, the more popular it is. 



SpotifyData <- SpotifyTop50 %>% filter(Genre %in% c("dance pop", "pop", "latin","edm"))


ggplot(data = SpotifyData, aes(x = Danceability, y = Popularity, color = Genre))+
  geom_point(alpha = 0.5, size = 3) + facet_grid(~Genre) +
  theme_bw() +
  labs(title = 'How Danceability affects Popularity', 
       subtitle = 'For top Genres',
       x= 'Danceability',
       y = 'Popularity' ) +
  theme(axis.text.x = element_text(angle=45,hjust=1)) + 
  scale_color_viridis_d(option = "viridis")



