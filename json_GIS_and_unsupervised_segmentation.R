##################################################################################################
##################################################################################################
### Case exercise: How users interact with their online travel website
### Author: Henri Furstenau
### Inputs: 1 json file (I will upload some random generated data soon)
### Outputs: Answer to question 1: 1 table
### Outputs: Answer to question 2: 1 data frame with distances per session_id, 1 unsupervised clustering model,
###          2 plots, 1 table
### Body: Step 1. Import libraries
### Body: Step 2. Import data
### Body: Step 3. Cleaning data
### Body: Step 4. Data pre-processsing and Profiling
### Body: Question 1
### Body: Step 5. Finding cities geo coordinates
### Body: Step 6. Calculating distances between cities (GIS)
### Body: Question 2
### Body: Step 7. Deriving new variables
### Body: Step 8. Step 8: Clustering
### Body: Step 9. Validation
### Results
##################################################################################################
##################################################################################################

############################# Step 1: Import libraries ###########################################
library(rjson) #fromJSON
library(tidyr) # separate_rows
library(mapproj) # map, mapproject
library(geosphere) # distm
library(ggplot2) # ggplot2
library(factoextra) # eclust, fviz_cluster, fviz_silhouette
##################################################################################################

############################# Step 2: Import data ################################################
json_file <- fromJSON(file = "file.json")

'
(I will upload some random generated data soon)
Data example. List 996:

[[996]]
[[996]]$session_id
[1] "QCD0MWJT7XMP9"

[[996]]$unix_timestamp
[1] 1439793783

[[996]]$cities
[1] "OTTAWA ON"

[[996]]$user
[[996]]$user[[1]]
[[996]]$user[[1]][[1]]
[[996]]$user[[1]][[1]]$user_id
[1] 4227

[[996]]$user[[1]][[1]]$joining_date
[1] "2015-04-01"

[[996]]$user[[1]][[1]]$country
[1] ""
'
##################################################################################################

##################### Step 3: Cleaning data (Do not run) #########################################
# Opening the json data in a browser such a Firefox is a good way to visualise the data.
# A first inspection shows the file has 6 columns. However, transforming the data to a data frame return errors.
# Finding what rows are returning error: The error is a 7th column named _row that is present for some sessions
inspect1 <- data.frame()
options(warn=1)
for(i in 1:length(json_file)){
  temp <- data.frame(matrix(unlist(json_file[[i]]), ncol=6, byrow=T))
  inspect1 <- rbind(inspect1, temp)
  print(i) # relative position
}
##################################################################################################

##################### Step 4: Data pre-processsing and Profiling #################################
# Transforms the json file to a data frame, fixing the error
json_file <- lapply(json_file, function(x){
  unlist(x)[1:6] # removes column "_row"
})
df0 <- as.data.frame(do.call("rbind", json_file), stringsAsFactors = F)

# Profiling data
summary(do.call("rbind", json_file)) # users are from 7 countries. 1 is missing
table(is.na(do.call("rbind", json_file))) # no missing values
length(df0$session_id) # session_id is unique (20022)
length(unique(df0$user.user_id)) # 5777 unique user_id

# Transforms df to working df. Removes unecessary fields.
# Splits cities string. Assigns session_id, user_id and country for each city.
df <- separate_rows(df0[, c("session_id", "user.user_id", "user.country", "cities")], cities, sep =", ")
names(df) <- c("session_id", "user_id", "country", "cities")

# Profilig Data
unique(df$cities) # destination cities are within USA and Canada (confirmed below in the script when plotting map)
max(table(df$session_id, df$cities)) # No duplicated cities in each session 
##################################################################################################

##################### Question 1 #################################################################
# Q: One country didn't get logged. How to find it?
# A: Canada is the most likely country since it would be expected significant number of domestic clients.
#    Challenge 3 dataset shows a significant number of domestic USA users but no Canadian users. 
table(df$cities, df$country) # Frequency per country

#    The missing country could also be Mexico since it has the most nationals to visit North America with
#   about 30% more visitors than the second (USA visitors to Canada – non domestic) and about 400% more 
#    visitors than the UK (4th place to visit the USA and 2nd to visit Canada)# more visitors than Canada (2nd place) 
#    and about 400% more visitors than the UK (3r place to visit the USA and 2nd to visit Canada)
# source (https://www.dhs.gov/immigration-statistics/yearbook/2015/table28)
# source (http://www.statcan.gc.ca/tables-tableaux/sum-som/l01/cst01/arts38a-eng.htm)
##################################################################################################

##################### Step 5: Finding cities geo coordinates #####################################
# Assumption: Company wants to avoid Google maps API (paid after certain number of logs in a day)
# Creates URL for open street map API
unique_cities <- unique(df$cities)
url <- paste0("http://nominatim.openstreetmap.org/search.php?q=",
              unique_cities,
             "&limit=1&format=json")
url <- gsub(" ", "+", url)

# df with unique city names and lat-lon coordinates
df_latlon <- data.frame() 
for(i in 1: length(url)){
  city_data <- RJSONIO::fromJSON(url[i])
  temp <- data.frame(cities = unique_cities[i],
                     lon = city_data[[1]]$lon,
                     lat = city_data[[1]]$lat,
                     display_name = city_data[[1]]$display_name) #display name for QA
  df_latlon <- rbind(df_latlon, temp)
  print(i)
}
# Fixing data types
df_latlon$cities <- as.character(df_latlon$cities)
df_latlon$lat <- as.numeric(as.character(df_latlon$lat))
df_latlon$lon <- as.numeric(as.character(df_latlon$lon))
df_latlon$display_name <- as.character(df_latlon$display_name)

# QA: Plotting on map
map(database= "world", ylim=c(45,90), xlim=c(-155,-45), col="grey80", fill=TRUE, projection="gilbert", orientation= c(90,0,225))
coord <- mapproject(df_latlon$lon, df_latlon$lat, proj="gilbert", orientation=c(90, 0, 225))  #convert points to projected lat/long
points(coord, pch=20, cex=1, col="blue")  #plot converted points
identify(coord) # Click on points out of North America: row 74 is wrong

# QA: Replaces wrong row
city_data <- RJSONIO::fromJSON("http://nominatim.openstreetmap.org/search.php?q=Saint+Paul+MN&limit=2&format=json")
df_latlon$lon[74] <- as.numeric(city_data[[2]]$lon)
df_latlon$lat[74] <- as.numeric(city_data[[2]]$lat)
df_latlon$display_name[74] = as.character(city_data[[2]]$display_name)

# QA: Visual inspection to df_latlon shows Victoria BC coordinates as positive (therefore wrong)
city_data <- RJSONIO::fromJSON("http://nominatim.openstreetmap.org/search.php?q=Victoria+BC+Canada&limit=2&format=json")
df_latlon$lon[19] <- as.numeric(city_data[[1]]$lon)
df_latlon$lat[19] <- as.numeric(city_data[[1]]$lat)
df_latlon$display_name[19] = as.character(city_data[[1]]$display_name)
##################################################################################################

##################### Step 6: Calculating distances between cities (GIS) #########################
# The best performance for finding distances in this case was to identify all combinations of cities 
# per session_id and user_id (df A, larger df) and match it to all possible pair combinations of cities (df B, smaller df).  

# session df
df_session <- df[, c("session_id", "cities")]

# Need to divide the sessions that searched only for one city and therefore have distance = 0
df_session_unique <- df_session[!(duplicated(df_session$session_id)|duplicated(df_session$session_id, fromLast=TRUE)), ]
df_session_dup <- df_session[duplicated(df_session$session_id)|duplicated(df_session$session_id, fromLast=TRUE), ]

## Working df A (matching distances)
## All combinations of two cities grouped by session (sessions with two or more cities)
df_allcomb <- data.frame()
position <- 0
for (i in unique(df_session_dup$session_id)){
  temp <- subset(df_session_dup, df_session_dup$session_id == i)
  temp <- data.frame(session_id = i,
                     t(combn(c(temp$cities), 2)))
  df_allcomb <- rbind(df_allcomb, temp)
  position <- position + 1
  print(position)
} # < 1.5 mins

# Sorting the two cities to match with working df B (df_latloncomb)
comb_sort <- as.data.frame(t(apply(df_allcomb[, c("X1", "X2")], 1, sort)))
df_allcomb$paste <- paste(comb_sort$V1, comb_sort$V2)
##

## Working df B (matching distance)
## Unique combinations of two cities 
df_latloncomb <- as.data.frame(t(combn(df_latlon$cities, 2)))

# Matching to df_latlon to define coordinates
df_latloncomb[, c("lon1", "lat1")] <- df_latlon[match(df_latloncomb$V1, df_latlon$cities), c("lon", "lat")]
df_latloncomb[, c("lon2", "lat2")] <- df_latlon[match(df_latloncomb$V2, df_latlon$cities), c("lon", "lat")]

# Calculates distance (m) for df B (smaller df)
distances <- c()
for(i in 1:nrow(df_latloncomb)){
temp <- distm(c(df_latloncomb$lon1[i], df_latloncomb$lat1[i]), c(df_latloncomb$lon2[i], df_latloncomb$lat2[i]), fun = distHaversine)
distances <- c(distances, temp)
}
df_latloncomb$distances <- distances

# Sorting the two cities to match with working df A (df_allcomb)
df_latloncomb_sort <- as.data.frame(t(apply(df_latloncomb[, c("V1", "V2")], 1, sort)))
df_latloncomb$paste <- paste(df_latloncomb_sort$V1, df_latloncomb_sort$V2)
##

# Matching df A and df B: Obtain distances for each pair combination of cities per session_id and user_id 
df_dist_session <- data.frame(session_id = df_allcomb$session_id,
                    distances = df_latloncomb[match(df_allcomb$paste, df_latloncomb$paste), "distances"],
                    stringsAsFactors = F)

# Rejoining session_ids with only one city 
# Assuming only one city is distance 0 and high purchasing intent
df_dist_session <- rbind(df_dist_session,
               data.frame(session_id = df_session_unique$session_id,
                          distances = 0,
                          stringsAsFactors = F))
##################################################################################################

##################### Question 3 #################################################################
# Q: Create an algorithm that clusters user sessions into two groups: high purchase intent and low intent.
# A: Clustering sessions in two groups (high and low intent) can be obtained by calculating the distances between cities for 
#    new sessions (following Steps 1 to 6). 
#    We can assume that any distance above 300 Km ("straight" line) should be considered cities far away. That distance would take almost 5 
#    hours to drive, which is also time enough to make travellers consider opting for flights.

mean_distance <- aggregate(df_dist_session[, c("distances")], list(df_dist_session$session_id), mean)
names(mean_distance) <- c("session_id", "distances")

ggplot(data=mean_distance, aes(distances)) + 
  geom_histogram(bins = 20, col = "black", fill = "blue", alpha = .2) + 
  theme_bw() + labs(x = "Distances (m) per session", y = "Count")

#    However, investigating only sessions will not answer the primary question: how users interact with their online travel website?
#    I recommend to derive an algorithm focused in users having the explanatory variables: number of sessions, average distance between cities, 
#    max number of sessions visiting a same city and total number of cities. The independent variable should be purchase rate or purchasing intent.
#    If we have access to extra data that shows purchase rate of users, we can create a machine learning logist regression supervised algorithm. 
#    Since purchasing rate data was not provided, we can use an unsupervised clustering algorithm applying k-means for this Proof of Concept.
##################################################################################################

##################### Step 7: Deriving new variables #############################################
# Max number of sessions visiting a same city (cities don't repeat in one session therefore we can calculate the max city per user)
ncities <- table(df$user_id, df$cities)
nsamecity <- as.data.frame(apply(ncities, 1, max))
names(nsamecity) <- "max_n_same_city"

# Total number of cities
ncities[ncities > 0] <- 1 # avoid city repetition
ncities <- as.data.frame(apply(ncities, 1, sum))
names(ncities) <- "number_of_cities"

# Number of sessions (session_id is unique therefore we can simply count the user_id frquency from the raw data df0)
nsessions <- as.data.frame(table(df0$user.user_id))

# Average distance between cities max number of sessions visiting a same city
df0$distances <- mean_distance[match(df0$session_id, mean_distance$session_id), "distances"]
dist <- aggregate(df0$distances, list(df0$user.user_id), mean) # perhaps should investigate re-calculating the distance modifying Step 6

# Bind all
# session_is is in the same order for all variables
df_res <- data.frame(
  # user_id = as.character(nsessions$Var1), 
  avg_distances = as.numeric(dist$x), 
  number_of_sessions = as.numeric(nsessions$Freq), 
  max_n_same_city = as.numeric(unlist(nsamecity)), 
  number_of_cities = as.numeric(unlist(ncities))
  )
##################################################################################################

##################### Step 8: Clustering #########################################################
# Feature scaling
ml_set = as.data.frame(scale(df_res))

# Using the elbow method to find the optimal number of clusters
# Analysing the "elbow", the optimal number of clusters should be 3 or 4. In other words, four classes of purchasing intent.
# Since we want only high and low intent I will proceed with 2 clusters.
wcss <- vector()
for (i in 1:10) {wcss[i] <- sum(kmeans(ml_set, i)$withinss)}
plot(1:10,
     wcss,
     type = 'b',
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
kmeans <- eclust(ml_set, "kmeans", k = 2, nstart = 25, graph = FALSE) # avoids kmeans initialisation trap
# kmeans <- kmeans(x = ml_set, centers = 2) # for large datasets

# Visualize k-means clusters
fviz_cluster(kmeans, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

# Change to factor to colour ggplots
ml_set$cluster <- as.character(kmeans$cluster)
ml_set$cluster[ml_set$cluster == "1"] <- "High Intent"
ml_set$cluster[ml_set$cluster == "2"] <- "Low Intent"
ml_set$cluster <- as.factor(ml_set$cluster)
##################################################################################################

##################### Step 9: Validation #########################################################
# silhouette plot and coefficient (Si) 
# Si values range from 1 to -1. A value of Si close to 1 indicates that the object is well clustered.
fviz_silhouette(kmeans, palette = "jco", 
                ggtheme = theme_classic())
# table: Si width average 0.35. That means the clustering is around 65% adequate and could be improved by 
#        re-assigning a different number of clusters.
# Graph: It can be seen that several samples, in cluster 2 (Low Intent), have a negative silhouette coefficient. 
#        This means that they are not in the right cluster. We can find the name of these samples and determine 
#        the clusters they are closer (neighbor cluster), as follow:
sil <- kmeans$silinfo$widths[, 1:3]
neg_sil_index <- which(sil[, 'sil_width'] < 0) # Objects with negative silhouette
sil[neg_sil_index, , drop = FALSE]
##################################################################################################

##################### Results ####################################################################
# Kmeans centers suggest that the greater avg_distances, number_of_sessions, 
# max_n_same_city and/or number_of_cities, the lowest is the intent purchase.
kmeans$centers

## Plotting results
# 60% of users have high intent of purchase
ggplot(ml_set, aes(cluster) ) + 
  geom_bar(col = "black", fill = "blue", alpha = .2) + 
  theme_bw() 

# High intent is still driven by the high number of sessions with only city searched
table(df_res$avg_distances == 0)

# The inclusion of other 3 explanatory variables to "avg_distance", suggest that some users searched 
# for close cities but indeed can have low purchase intent 
ggplot(ml_set, aes(y = avg_distances, x = number_of_sessions, color = cluster)) + 
  geom_point() + theme_bw()

# Recommendations: Unsupervised clustering usually don't require dividing the data in training and testing because there is no label to be trained.
#                  However, if we want to test increasing the number of clusters, it may be a good practice to divide the data in training and testing
#                  This is a way to avoid overfitting since increasing number of clusters will always "increase the performance".
# Recommendations: Include purchase rate data to improve predictions with a supervised classification algorithm
#                  such as logistic regression (see Question 3 answers for more details). Scaling up of a classification model 
#                  could be applied using xgboost or ANN deep learning python libraries (xgboost-distributed computing or tensorflow-theano-keras-GPU).
# Recommendations: If purchase rate data is not available, the clustering of purchase intent should have 3 or 4 classes (eg. low, medium and high intent)
##################################################################################################

