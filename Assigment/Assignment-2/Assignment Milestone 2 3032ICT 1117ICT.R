#library-------
library(vosonSML)
library(magrittr)
library(tidyr)
library(tidytext)
library(stopwords)
library(textclean)
library(qdapRegex)
library(tm)
library(SnowballC)
library(Rspotify)
library(spotifyr)
library(igraph)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggridges)
library(httpuv)
library(tuber)
library(syuzhet)

library(C50)
library(caret)
library(e1071)
library(dplyr)
library(topicmodels)
library(slam)
library(Rmpfr)
library(reshape2)
library(topicmodels)
library(askpass)
library(rsample)


library(tidyverse)
library(jsonlite)
################################
#DATA SELECTION & EXPLORATION (CONTINUED)
#2.1) Spotify data retrieval -------------------------------------------------------------------------------
# Configure application to store Spotify authentication data in cache
options(httr_oauth_cache = TRUE)
# Set up authentication variables
app_id <- "67d3dd0bcc304eb0968de7be9857a1f5"
app_secret <- "e2f2696b6a5848048efed21ae9aef2d4"
token <- "1"

# Authentication for Rspotify package:
keys <- spotifyOAuth(token, app_id, app_secret) 

# Get Spotify data on 'Twice'
find_my_artist <- searchArtist("Twice", token = keys)
my_artist <- getArtist("7n2Ycct7Beij7Dj7meI4X0", token = keys)


# Authentication for spotifyr package:
Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()

#get all products own by 'Twice'
Twice <-get_artist_audio_features(artist = "7n2Ycct7Beij7Dj7meI4X0",
                                  include_groups = c("album", "single"),
                                  return_closest_artist = TRUE,
                                  dedupe_albums = TRUE,
                                  market = NULL,
                                  authorization = get_spotify_access_token())
Twice <- Twice[!duplicated(Twice$track_name),]

#calculate the number of active year
earliest_year <- min(Twice$album_release_year)
num_years_active <- (as.integer(format(Sys.Date(), "%Y")) - earliest_year)

# Retrieve album data of artist
albums <- getAlbums("7n2Ycct7Beij7Dj7meI4X0", token = keys)

# Get audio features for 'Twice'
audio_features <- get_artist_audio_features("Twice")
audio_feature <- audio_features[!duplicated(audio_features$track_name, audio_features$album_name), ]

# Plot happiness (valence) scores for each album
ggplot(audio_features, aes(x = valence, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Prevalent features in Twice Albums",
          subtitle = "Based on valence from Spotify's Web API")

# Plot liveness scores for each album
ggplot(audio_features, aes(x = liveness, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Liveness features in Twice Albums",
          subtitle = "Based on valence from Spotify's Web API")

# Retrieve information about related artists
related_artits <- getRelated("Twice", token = keys)

# Create a network of artists related to the Top 100 artists
topsongs <- getPlaylistSongs("spotify", "4hOKQuZbraPDIfaGbM3lKI", token = keys)

edges <- c()
for (artist in topsongs$artist){
  related <- getRelated(artist, token = keys)
  for (relatedartist in related$name){
    edges <- append(edges, artist)
    edges <- append(edges, relatedartist)
  }
}

# Convert network to graph and save as external file
related_artists_graph <- graph(edges)
write.graph(related_artists_graph, file = "RelatedArtists.graphml", format = "graphml")

#2.2) YouTube views/likes-------------------------------------------------------------------------------
api_key <- "AIzaSyC5ETz15tC79ufPNN17EpgQR9fhJYzDuZ8"
client_id <- "1054539266290-kuikj7tp57uln1242jnosv8ekrunm31v.apps.googleusercontent.com"
client_secret <- "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"

# Authenticate to YouTube using the tuber package
yt_oauth(app_id = client_id, app_secret = client_secret)

# Search YouTube
video_search <- yt_search("#twice OR twice kpop")

# FIND OUT the video having highest number of views and likes
# Choose some videos and store their video IDs
video_ids <- as.vector(video_search$video_id[1:5])
print(video_ids)

#
video_view_likes = data.frame()
for (i in video_ids){
  video_view_like <- c(i,get_stats(i)$viewCount,get_stats(i)$likeCount)
  video_view_likes<-rbind(video_view_likes, video_view_like)
}
columns <-c("video_id","viewCount","likeCount")
colnames(video_view_likes) = columns

#convert datatype from char to numeric
video_view_likes$viewCount = as.numeric(as.character(video_view_likes$viewCount))
video_view_likes$likeCount = as.numeric(as.character(video_view_likes$likeCount)) 

#videos have the highest number of views:
i <- max(video_view_likes$viewCount) 
print(video_view_likes[which(video_view_likes$viewCount == i), ])

#videos have the highest number of likes:
j <- max(video_view_likes$likeCount) 
print(video_view_likes[which(video_view_likes$likeCount == j), ])

#videos have the both highest number of views and likes:
print(video_view_likes[which(video_view_likes$viewCount == i,video_view_likes$likeCount == j), ])


################################
#TEXT PRE-PROCESSING
#2.3) Pre-processing & term-document matrix & top 10 terms-------------------------------------------------------------------------------
twitter_data <- readRDS("D:/GU/Big data/Assignment/Ass2/2023-03-31_001726-TwitterData.rds")

# Clean the tweet text
clean_text <- twitter_data$tweets$text %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_emoji() %>% 
  replace_emoticon()


# Convert clean_text vector into a document corpus (collection of documents)
text_corpus <- VCorpus(VectorSource(clean_text))


# Perform further pre-processing 
text_corpus <- text_corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords(kind = "SMART")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)


# Transform corpus into a Document Term Matrix
doc_term_matrix <- DocumentTermMatrix(text_corpus)


# Sort words by total frequency across all documents
dtm_df <- as.data.frame(as.matrix(doc_term_matrix))
freq <- sort(colSums(dtm_df), decreasing = TRUE)
head(freq, n = 10)
# Plot word frequency
word_frequ_df <- data.frame(word = names(freq), freq)

ggplot(subset(word_frequ_df, freq > 2), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")

ggplot(subset(word_frequ_df, freq > 67), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")

################################
#SOCIAL NETWORK ANALYSIS
#2.4) Centrality analysis
#Twice-------
# Create twomode (bimodal) network
twomode_network_Twice <- twitter_data %>% Create("twomode", 
                                                 removeTermsOrHashtags = c("#twice OR twice kpop"))
twomode_graph__Twice <- twomode_network_Twice %>% Graph()

# Write graph to file
write.graph(twomode_graph__Twice, file = "Twomode_Twice.graphml", format = "graphml")

# Inspect the graph object
length(V(twomode_graph__Twice))
V(twomode_graph__Twice)$name

# Find all maximum components that are weakly connected
twomode_comps_Twice <- components(twomode_graph__Twice, mode = c("weak"))
twomode_comps_Twice$no
twomode_comps_Twice$csize
head(twomode_comps_Twice$membership, n = 30)


# Get sub-graph with most members
largest_comp_Twice <- which.max(twomode_comps_Twice$csize)
twomode_subgraph_Twice <- twomode_graph__Twice %>% 
  induced_subgraph(vids = which(twomode_comps_Twice$membership == largest_comp_Twice))


# Display top 20 nodes from the sub-graph ordered by degree centrality
sort(degree(twomode_subgraph_Twice, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph_Twice, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph_Twice, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by closeness centrality
sort(closeness(twomode_subgraph_Twice, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph_Twice, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph_Twice, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality
sort(betweenness(twomode_subgraph_Twice, directed = FALSE), decreasing = TRUE)[1:20]

#Blackpink-------
# Create actor network
youtubeAuth <- Authenticate("youtube", apiKey = api_key)
videoID <- "https://www.youtube.com/watch?v=hR1gMWQS-ws"
Blackpink_data <- youtubeAuth %>%Collect(videoID, maxComments = 1000, writeToFile = TRUE)
twomode_network_Blackpink <- Blackpink_data |> Create("actor") |> AddText(Blackpink_data)
twomode_graph_Blackpink <- twomode_network_Blackpink %>% Graph()

# Write graph to file
write.graph(twomode_graph_Blackpink, file = "Twomode_Blackpink.graphml", format = "graphml")

# Inspect the graph object
length(V(twomode_graph_Blackpink))
V(twomode_graph_Blackpink)$name

# Find all maximum components that are weakly connected
twomode_comps_Blackpink <- components(twomode_graph_Blackpink, mode = c("weak"))
twomode_comps_Blackpink$no
twomode_comps_Blackpink$csize
head(twomode_comps_Blackpink$membership, n = 30)


# Get sub-graph with most members
largest_comp_Blackpink <- which.max(twomode_comps_Blackpink$csize)
twomode_subgraph_Blackpink <- twomode_graph_Blackpink %>% 
  induced_subgraph(vids = which(twomode_comps_Blackpink$membership == largest_comp_Blackpink))


# Display top 20 nodes from the sub-graph ordered by degree centrality
sort(degree(twomode_subgraph_Blackpink, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph_Blackpink, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph_Blackpink, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by closeness centrality
sort(closeness(twomode_subgraph_Blackpink, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph_Blackpink, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph_Blackpink, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality
sort(betweenness(twomode_subgraph_Blackpink, directed = FALSE), decreasing = TRUE)[1:20]

#Momoland-------
# Create actor network
youtubeAuth <- Authenticate("youtube", apiKey = api_key)
videoID <- "https://www.youtube.com/watch?v=crUnaCpci2U"
Momoland_data <- youtubeAuth %>%Collect(videoID, maxComments = 1000, writeToFile = TRUE)
twomode_network_Momoland <- Momoland_data |> Create("actor") |> AddText(Momoland_data)
twomode_graph_Momoland <- twomode_network_Momoland %>% Graph()

# Write graph to file
write.graph(twomode_graph_Momoland, file = "Twomode_Momoland.graphml", format = "graphml")

# Inspect the graph object
length(V(twomode_graph_Momoland))
V(twomode_graph_Momoland)$name

# Find all maximum components that are weakly connected
twomode_comps_Momoland <- components(twomode_graph_Momoland, mode = c("weak"))
twomode_comps_Momoland$no
twomode_comps_Momoland$csize
head(twomode_comps_Momoland$membership, n = 30)


# Get sub-graph with most members
largest_comp_Momoland <- which.max(twomode_comps_Momoland$csize)
twomode_subgraph_Momoland <- twomode_graph_Momoland %>% 
  induced_subgraph(vids = which(twomode_comps_Momoland$membership == largest_comp_Momoland))


# Display top 20 nodes from the sub-graph ordered by degree centrality
sort(degree(twomode_subgraph_Momoland, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph_Momoland, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph_Momoland, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by closeness centrality
sort(closeness(twomode_subgraph_Momoland, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph_Momoland, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph_Momoland, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality
sort(betweenness(twomode_subgraph_Momoland, directed = FALSE), decreasing = TRUE)[1:20]

#2.5) Community analysis-----
#Twice-----
#with some videos and store their video IDs,
#for which we want to collect comments
# and build an actor network
yt_data <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

yt_actor_network <- yt_data %>% Create("actor")
yt_actor_graph <- Graph(yt_actor_network)

# Transform into an undirected graph
undir_yt_actor_graph <- as.undirected(yt_actor_graph, mode = "collapse")

# Run Louvain algorithm
louvain_yt_actor <- cluster_louvain(undir_yt_actor_graph)

# See sizes of communities
sizes(louvain_yt_actor)

# Visualise the Louvain communities
plot(louvain_yt_actor, 
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)

write.graph(undir_yt_actor_graph, 
            file = "undir_yt_actor_graph.graphml",
            format = "graphml")

# Run Girvan-Newman (edge-betweenness) algorithm
eb_yt_actor <- cluster_edge_betweenness(undir_yt_actor_graph)


# See sizes of communities
sizes(eb_yt_actor)


# Visualise the edge-betweenness communities
plot(eb_yt_actor,
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy
yt_actor_graph2 <- yt_actor_graph
V(yt_actor_graph2)$name <- V(yt_actor_graph2)$screen_name
undir_yt_actor_graph2 <- as.undirected(yt_actor_graph2, mode = "collapse")
eb_yt_actor2 <- cluster_edge_betweenness(undir_yt_actor_graph2)

is_hierarchical(eb_yt_actor2)
as.dendrogram(eb_yt_actor2)
plot_dendrogram(eb_yt_actor2)

plot_dendrogram(eb_yt_actor2, mode = "dendrogram", xlim = c(1,20))
#Black Pink----
video_search_Blackpink <- yt_search("blackpink")
# Choose some videos and store their video IDs,
# for which we want to collect comments
# and build an actor network
video_ids_Blackpink <- as.vector(video_search_Blackpink$video_id[1:5])

#with some videos and store their video IDs,
#for which we want to collect comments
# and build an actor network
yt_data_Blackpink <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids_Blackpink,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

yt_actor_network_Blackpink <- yt_data_Blackpink %>% Create("actor")
yt_actor_graph_Blackpink <- Graph(yt_actor_network_Blackpink)

# Transform into an undirected graph
undir_yt_actor_graph_Blackpink <- as.undirected(yt_actor_graph_Blackpink, mode = "collapse")

# Run Louvain algorithm
louvain_yt_actor_Blackpink <- cluster_louvain(undir_yt_actor_graph_Blackpink)

# See sizes of communities
sizes(louvain_yt_actor_Blackpink)

# Visualise the Louvain communities
plot(louvain_yt_actor_Blackpink, 
     undir_yt_actor_graph_Blackpink, 
     vertex.label = V(undir_yt_actor_graph_Blackpink)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)

write.graph(undir_yt_actor_graph_Blackpink, 
            file = "undir_yt_actor_graph_Blackpink.graphml",
            format = "graphml")

# Run Girvan-Newman (edge-betweenness) algorithm
eb_yt_actor_Blackpink <- cluster_edge_betweenness(undir_yt_actor_graph_Blackpink)


# See sizes of communities
sizes(eb_yt_actor_Blackpink)


# Visualise the edge-betweenness communities
plot(eb_yt_actor_Blackpink,
     undir_yt_actor_graph_Blackpink, 
     vertex.label = V(undir_yt_actor_graph_Blackpink)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy
yt_actor_graph_Blackpink2 <- yt_actor_graph_Blackpink
V(yt_actor_graph_Blackpink2)$name <- V(yt_actor_graph_Blackpink2)$screen_name
undir_yt_actor_graph_Blackpink2 <- as.undirected(yt_actor_graph_Blackpink2, mode = "collapse")
eb_yt_actor_Blackpink2 <- cluster_edge_betweenness(undir_yt_actor_graph_Blackpink2)

is_hierarchical(eb_yt_actor_Blackpink2)
as.dendrogram(eb_yt_actor_Blackpink2)
plot_dendrogram(eb_yt_actor_Blackpink2)

plot_dendrogram(eb_yt_actor_Blackpink2, mode = "dendrogram", xlim = c(1,20))

#Momoland----
video_search_Momoland <- yt_search("momoland")
# Choose some videos and store their video IDs,
# for which we want to collect comments
# and build an actor network
video_ids_Momoland <- as.vector(video_search_Momoland$video_id[1:5])


#with some videos and store their video IDs,
#for which we want to collect comments
# and build an actor network
yt_data_Momoland <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids_Momoland,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

yt_actor_network_Momoland <- yt_data_Momoland %>% Create("actor")
yt_actor_graph_Momoland <- Graph(yt_actor_network_Momoland)

# Transform into an undirected graph
undir_yt_actor_graph_Momoland <- as.undirected(yt_actor_graph_Momoland, mode = "collapse")

# Run Louvain algorithm
louvain_yt_actor_Momoland <- cluster_louvain(undir_yt_actor_graph_Momoland)

# See sizes of communities
sizes(louvain_yt_actor_Momoland)

# Visualise the Louvain communities
plot(louvain_yt_actor_Momoland, 
     undir_yt_actor_graph_Momoland, 
     vertex.label = V(undir_yt_actor_graph_Momoland)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)

write.graph(undir_yt_actor_graph_Momoland, 
            file = "undir_yt_actor_graph_Momoland.graphml",
            format = "graphml")

# Run Girvan-Newman (edge-betweenness) algorithm
eb_yt_actor_Momoland <- cluster_edge_betweenness(undir_yt_actor_graph_Momoland)


# See sizes of communities
sizes(eb_yt_actor_Momoland)


# Visualise the edge-betweenness communities
plot(eb_yt_actor_Momoland,
     undir_yt_actor_graph_Momoland, 
     vertex.label = V(undir_yt_actor_graph_Momoland)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy
yt_actor_graph_Momoland2 <- yt_actor_graph_Momoland
V(yt_actor_graph_Momoland2)$name <- V(yt_actor_graph_Momoland2)$screen_name
undir_yt_actor_graph_Momoland2 <- as.undirected(yt_actor_graph_Momoland2, mode = "collapse")
eb_yt_actor_Momoland2 <- cluster_edge_betweenness(undir_yt_actor_graph_Momoland2)

is_hierarchical(eb_yt_actor_Momoland2)
as.dendrogram(eb_yt_actor_Momoland2)
plot_dendrogram(eb_yt_actor_Momoland2)

plot_dendrogram(eb_yt_actor_Momoland2, mode = "dendrogram", xlim = c(1,20))



################################
#MACHINE LEARNING MODELS
#2.6) Sentiment analysis
#Twice-------
clean_text <- twitter_data$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()
# Assign sentiment scores to tweets
sentiment_scores <- get_sentiment(clean_text, method = "afinn") %>% sign()
sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)

# Convert sentiment scores to labels: positive, neutral, negative
sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 

# Plot sentiment classification
ggplot(sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets")


# Assign emotion scores to tweets
emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]
emo_scores_df <- data.frame(clean_text, emo_scores)

# Calculate proportion of emotions across all tweets
emo_sums <- emo_scores_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_df) 
names(emo_sums)[1] <- "Proportion" 

# Plot emotion classification
ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Tweets") +
  ggtitle("Emotion Analysis of Tweets")

#Blackpink-------
clean_text <- Blackpink_data$Comment  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()

# Assign sentiment scores to tweets
sentiment_scores_Blackpink <- get_sentiment(clean_text, method = "afinn") %>% sign()
sentiment_df_Blackpink <- data.frame(text = clean_text, sentiment = sentiment_scores_Blackpink)

# Convert sentiment scores to labels: positive, neutral, negative
sentiment_df_Blackpink$sentiment <- factor(sentiment_df_Blackpink$sentiment, levels = c(1, 0, -1),
                                           labels = c("Positive", "Neutral", "Negative")) 

# Plot sentiment classification
ggplot(sentiment_df_Blackpink, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Comments") +
  ggtitle("Sentiment Analysis of Comments")


# Assign emotion scores to tweets
emo_scores_Blackpink <- get_nrc_sentiment(clean_text)[ , 1:8]
emo_scores_Blackpink_df <- data.frame(clean_text, emo_scores_Blackpink)

# Calculate proportion of emotions across all tweets
emo_sums_Blackpink <- emo_scores_Blackpink_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_Blackpink_df) 
names(emo_sums_Blackpink)[1] <- "Proportion" 

# Plot emotion classification
ggplot(emo_sums_Blackpink, aes(x = reorder(rownames(emo_sums_Blackpink), Proportion),
                               y = Proportion,
                               fill = rownames(emo_sums_Blackpink))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Comments") +
  ggtitle("Emotion Analysis of Comments")

#Momoland-------
clean_text <- Momoland_data$Comment  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()

# Assign sentiment scores to tweets
sentiment_scores_Momoland <- get_sentiment(clean_text, method = "afinn") %>% sign()
sentiment_df_Momoland <- data.frame(text = clean_text, sentiment = sentiment_scores_Momoland)

# Convert sentiment scores to labels: positive, neutral, negative
sentiment_df_Momoland$sentiment <- factor(sentiment_df_Momoland$sentiment, levels = c(1, 0, -1),
                                          labels = c("Positive", "Neutral", "Negative")) 

# Plot sentiment classification
ggplot(sentiment_df_Momoland, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Comments") +
  ggtitle("Sentiment Analysis of Comments")


# Assign emotion scores to tweets
emo_scores_Momoland <- get_nrc_sentiment(clean_text)[ , 1:8]
emo_scores_Momoland_df <- data.frame(clean_text, emo_scores_Momoland)

# Calculate proportion of emotions across all tweets
emo_sums_Momoland <- emo_scores_Momoland_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_Momoland_df) 
names(emo_sums_Momoland)[1] <- "Proportion" 

# Plot emotion classification
ggplot(emo_sums_Momoland, aes(x = reorder(rownames(emo_sums_Momoland), Proportion),
                              y = Proportion,
                              fill = rownames(emo_sums_Momoland))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Comments") +
  ggtitle("Emotion Analysis of Comments")

#2.7) Decision tree-----
#Twice-------
# Get songs from Twice and their audio features
Twice_features <- get_artist_audio_features("Twice")
data.frame(colnames(Twice_features))
Twice_features_subset <- Twice_features[ , 9:20]

# Get top 100 songs and their audio features
top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
data.frame(colnames(top100_features))
top100_features_subset <- top100_features[ , 6:17]
top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)

# Add the 'isTwice' column (class variable) to each data frame
# to indicate which songs are by Twice and which are not
top100_features_subset["isTwice"] <- 0
Twice_features_subset["isTwice"] <- 1


# Remove any songs by Twice that appear in the top 100
# and combine the two data frames into one dataset
top100_features_noTwice <- anti_join(top100_features_subset,
                                     Twice_features_subset,
                                     by = "track_id")
comb_data <- rbind(top100_features_noTwice, Twice_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isTwice' column into a factor
# and remove the 'track_id' column
comb_data$isTwice <- factor(comb_data$isTwice)
comb_data <- select(comb_data, -track_id)


# Randomise the dataset (shuffle the rows)
comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)
split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]


# Train the decision tree model
dt_model <- train(isTwice~ ., data = training_set, method = "C5.0")


# Sample a single prediction (can repeat)
prediction_row <- 8 
if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}

# Analyse the model accuracy with a confusion matrix
confusionMatrix(dt_model, reference = testing_set$isTwice)

#Blackpink-------
# Get songs from Blackpink and their audio features
Blackpink_features <- get_artist_audio_features("Blackpink")
data.frame(colnames(Blackpink_features))
Blackpink_features_subset <- Blackpink_features[ , 9:20]

# Get top 100 songs and their audio features
top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
data.frame(colnames(top100_features))
top100_features_subset <- top100_features[ , 6:17]
top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)

# Add the 'isBlackpink' column (class variable) to each data frame
# to indicate which songs are by Blackpink and which are not
top100_features_subset["isBlackpink"] <- 0
Blackpink_features_subset["isBlackpink"] <- 1


# Remove any songs by Blackpink that appear in the top 100
# and combine the two data frames into one dataset
top100_features_noBlackpink <- anti_join(top100_features_subset,
                                         Blackpink_features_subset,
                                         by = "track_id")
comb_data <- rbind(top100_features_noBlackpink, Blackpink_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isBlackpink' column into a factor
# and remove the 'track_id' column
comb_data$isBlackpink <- factor(comb_data$isBlackpink)
comb_data <- select(comb_data, -track_id)


# Randomise the dataset (shuffle the rows)
comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)
split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]


# Train the decision tree model
dt_model <- train(isBlackpink~ ., data = training_set, method = "C5.0")


# Sample a single prediction (can repeat)
prediction_row <- 1 # MUST be smaller than or equal to training set size
if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}

# Analyse the model accuracy with a confusion matrix
confusionMatrix(dt_model, reference = testing_set$isBlackpink)
#Momoland-------
# Get songs from Momoland and their audio features
Momoland_features <- get_artist_audio_features("Momoland")
data.frame(colnames(Momoland_features))
Momoland_features_subset <- Momoland_features[ , 9:20]

# Get top 100 songs and their audio features
top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
data.frame(colnames(top100_features))
top100_features_subset <- top100_features[ , 6:17]
top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)

# Add the 'isMomoland' column (class variable) to each data frame
# to indicate which songs are by Momoland and which are not
top100_features_subset["isMomoland"] <- 0
Momoland_features_subset["isMomoland"] <- 1


# Remove any songs by Momoland that appear in the top 100
# and combine the two data frames into one dataset
top100_features_noMomoland <- anti_join(top100_features_subset,
                                        Momoland_features_subset,
                                        by = "track_id")
comb_data <- rbind(top100_features_noMomoland, Momoland_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isMomoland' column into a factor
# and remove the 'track_id' column
comb_data$isMomoland <- factor(comb_data$isMomoland)
comb_data <- select(comb_data, -track_id)


# Randomise the dataset (shuffle the rows)
comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)
split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]


# Train the decision tree model
dt_model <- train(isMomoland~ ., data = training_set, method = "C5.0")


# Sample a single prediction (can repeat)
prediction_row <- 1 # MUST be smaller than or equal to training set size
if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}

# Analyse the model accuracy with a confusion matrix
confusionMatrix(dt_model, reference = testing_set$isMomoland)
#2.8) Topic Modelling----
#twice-------
# Clean the tweet text
clean_text <- twitter_data$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()


# Convert clean tweet vector into a document corpus (collection of documents)
text_corpus <- VCorpus(VectorSource(clean_text))

# Remove stop words
text_corpus <- text_corpus %>%
  tm_map(removeWords, stopwords(kind = "SMART"))


# Transform corpus into a Document Term Matrix and remove 0 entries
doc_term_matrix <- DocumentTermMatrix(text_corpus)
non_zero_entries = unique(doc_term_matrix$i)
dtm = doc_term_matrix[non_zero_entries,]

# Optional: Remove objects and run garbage collection for faster processing
save(dtm, file = "doc_term_matrix.RData")
rm(list = ls(all.names = TRUE))
load("doc_term_matrix.RData")

# Create LDA model with k topics
lda_model <- LDA(dtm, k = 6)
# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic
tweet_topics <- tidy(lda_model, matrix = "beta")


# Visualise the top 10 terms per topic
top_terms <- tweet_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#Blackpink----
clean_text <- Blackpink_data$Comment  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()


# Convert clean tweet vector into a document corpus (collection of documents)
text_corpus_Blackpink <- VCorpus(VectorSource(clean_text))

# Remove stop words
text_corpus_Blackpink <- text_corpus_Blackpink %>%
  tm_map(removeWords, stopwords(kind = "SMART"))


# Transform corpus into a Document Term Matrix and remove 0 entries
doc_term_matrix_Blackpink <- DocumentTermMatrix(text_corpus_Blackpink)
non_zero_entries_Blackpink = unique(doc_term_matrix_Blackpink$i)
dtm_Blackpink = doc_term_matrix_Blackpink[non_zero_entries_Blackpink,]

# Optional: Remove objects and run garbage collection for faster processing
save(dtm_Blackpink, file = "doc_term_matrix_Blackpink.RData")
rm(list = ls(all.names = TRUE))
gc() 
load("doc_term_matrix_Blackpink.RData")

# Create LDA model with k topics
lda_model_Blackpink <- LDA(dtm_Blackpink, k = 6)
# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic
comment_topics_Blackpink <- tidy(lda_model_Blackpink, matrix = "beta")


# Visualise the top 10 terms per topic
top_terms_Blackpink <- comment_topics_Blackpink %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms_Blackpink %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#Momoland----
clean_text <- Momoland_data$Comment  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()


# Convert clean tweet vector into a document corpus (collection of documents)
text_corpus_Momoland <- VCorpus(VectorSource(clean_text))

# Remove stop words
text_corpus_Momoland <- text_corpus_Momoland %>%
  tm_map(removeWords, stopwords(kind = "SMART"))


# Transform corpus into a Document Term Matrix and remove 0 entries
doc_term_matrix_Momoland <- DocumentTermMatrix(text_corpus_Momoland)
non_zero_entries_Momoland = unique(doc_term_matrix_Momoland$i)
dtm_Momoland = doc_term_matrix_Momoland[non_zero_entries_Momoland,]

# Optional: Remove objects and run garbage collection for faster processing
save(dtm_Momoland, file = "doc_term_matrix_Momoland.RData")
rm(list = ls(all.names = TRUE))
gc() 
load("doc_term_matrix_Momoland.RData")

# Create LDA model with k topics
lda_model_Momoland <- LDA(dtm_Momoland, k = 6)
# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic
comment_topics_Momoland <- tidy(lda_model_Momoland, matrix = "beta")


# Visualise the top 10 terms per topic
top_terms_Momoland <- comment_topics_Momoland %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms_Momoland %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

################################
#VISUAL----
#Gephi----
JustinBieber_data<- readRDS("D:/GU/Big data/Assignment/Ass2/JustinBieber.rds")

actorNetwork  <- JustinBieber_data %>% Create("actor",writeToFile = TRUE, verbose = TRUE)
actorGraph <- actorNetwork |> Graph(writeToFile = TRUE, verbose = TRUE)

write.graph(actorGraph, file = "actorGraph.graphml", format = "graphml")
#Tableau----
ListJSON <-toJSON(x = twitter_data)
write(ListJSON, "ListJSON.json")
