# Set working directory
setwd("/Users/livia/R/soms/code")

library(kohonen)
library(ggplot2)
library(rgdal)
library(rgeos)
library(gridExtra)
library(grid)


#read in the boundary data for the edinburgh area, already matched up by row with the census data
edinburgh_map <- readOGR(dsn="../data/SG_SIMD_2016_EDINBURGH.shp", layer="SG_SIMD_2016_EDINBURGH")

#######CONVERT PROJECTION TO LAT LONG
#convert the object into latitude and longitude for easier use with ggmap
edinburgh_map <- spTransform(edinburgh_map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


#convert to data frame
edin_data <- as.data.frame(edinburgh_map) 
#rename
names(edin_data) <- c("Datazone", "Area Name","Total Population","Working Age Population","SIMD Domain Rank","Quantile SIMD Rank","Decile SIMD Rank","Vigintile SIMD Rank","Percentile SIMD Ranking","Income Rate","Income Count","Income Domain Rank","Employment Rate","Employment Count","Employment Domain Ranking","Comparative Illness Factor","Hospital-Alcohol","Hospital-Drugs","Standard Mortality Rate","Depression","Low Birth Weight","Emergancy stays in hospital","Health Domain Ranking","Education Attendance","Education Attainment", "Education- No Qualifications","Education-In full time employment or ed.","Education-Entering higher education", "Education Domain Rank","Access- Drive to petrol", "Access- Drive to gp","Access- Drive to post office","Access- Drive to primary school","Access- Drive to retail","Access-Drive to secondary school","Access-Public transport to GP","Access-Public transport to post office","Access-Public transport to retail","Geographical Access Domain Ranking","Crime Count","Crime Rate","Crime Domain Rank", "Housing- Overcrowded count","Housing-No central heating count","Housing - Overcrowding rate","Housing- No central heating rate","Housing Domain Ranking","Shape Length","Shape Area","Intermediary Name" )

##FORTIFY
#convert spatial polygon to dataframe including columns of spatial information
edinburgh_fort <- fortify(edinburgh_map, region= "DataZone")
#merge the new dataframe with the edinburgh simd data using their shared column
edinburgh_fort <- merge(edinburgh_fort, edin_data, by.x="id", by.y="DataZone")

##PREPARE HEALTH DATA
health <- edin_data[, c(16,17,18,19,20,21,22)]
## standardise (between 0 and 1)
health_st<-apply(health, MARGIN = 2, FUN=function(X) (X-min(X))/diff(range(X))) #Margin=2 rescales column wise
## sum standardised values
health_sum<-rowSums(health_st[,c(1,2,3,4,5,6,7)])
health_sum

##PREPARE ACCESS DATA
access <- edin_data[, c(30,31,32,33,34,35,36,37,38)]
access_st<-apply(access, MARGIN = 2, FUN=function(X) (X-min(X))/diff(range(X))) 
access_sum <- rowSums(access_st[,c(1,2,3,4,5,6,7,8,9)]) #sum the colums

##PREPARE EDUCATION DATA
education <- edin_data[, c(24,25,26,27,28)]
education_st1 <- apply(education[,c(1,2,5)], MARGIN = 2, FUN=function(X) (max(X)-X)/diff(range(X))) #invert
education_st2 <- apply(education[,c(3,4)], MARGIN = 2, FUN=function(X) (X-min(X))/diff(range(X)))
education_sum <- rowSums(cbind(education_st1, education_st2))
#check if standardisation was right
education_check<-cbind(education_st1, education_st2, education_sum,edin_data[,29])

##PREPARE HOUSING DATA
housing <- edin_data[, c(45,46)]
housing_st <-apply(housing, MARGIN = 2, FUN=function(X) (X-min(X))/diff(range(X)))
housing_sum <- rowSums(housing_st[,c(1,2)])

#INCOME
income <- edin_data[,10]

#EMPLOYMENT
employment <- edin_data[,13]

#CRIME
crime <- edin_data[,41]

##Bringing all the variables into a data frame
summed_vars<-as.data.frame(matrix(c(income,employment,health_sum,education_sum,access_sum,crime,housing_sum),nrow=length(access_sum)))
names(summed_vars) <- c("Income", "Employment", "Health","Education", "Access", "Crime", "Housing")


########## SOM TRAINING

#choose the variables with which to train the SOM by subsetting the dataframe called data
# uncomment this to use all the variables and use it below instead of summed_vars
#data_train <- edin_data[, c(10,13,16,17,18,19,20,21,22,24,25,26,27,28,30,31,32,33,34,35,36,37,38,41,45,46)]

#standardise the data creating z-scores and convert to a matrix
data_train_matrix <- as.matrix(scale(summed_vars))
#keep the column names of data_train as names in new matrix 
names(data_train_matrix) <- names(summed_vars)

########## SOM GRID
#define the size, shape and topology of the som grid
som_grid <- somgrid(xdim = 13, ydim=9, topo="hexagonal", neighbourhood.fct="gaussian")

########## TRAIN
# Train the SOM model, alpha is learning rate, rlen is number of iterations
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=500, 
                 alpha=c(0.1,0.01), 
                 keep.data = TRUE )

# Plot of the training progress - how the node distances have stabilised over time.
# mean distance to closes codebook vector during training
plot(som_model, type = "changes")

## load custom palette, created by Shane Lynn
source('coolBlueHotRed.R')

###PLOT COUNT
#counts within nodes
plot(som_model, type = "counts", main="Node Counts per Node", palette.name=coolBlueHotRed, shape="straight", border="transparent")

###CREATE PLOTS OF QUALITY AND NEIGHBOUR DISTANCES
par(mfrow = c(1,2)) #create both plots next to each other
#map quality
plot(som_model, type = "quality", main="Distances within Nodes (Quality)", palette.name=grey.colors, shape="straight", border="darkgrey")
#neighbour distances
plot(som_model, type="dist.neighbours", main = "Distances to Neighbouring Nodes", shape="straight", palette.name=grey.colors, border="darkgrey")

dev.off() #mfrow off

#code spread, plot codebook vectors
plot(som_model, main="Codebook Vectors", type = "codes", shape="straight", bgcol="lightgrey", palette.name=rainbow, border="darkgrey")

#plot codebook vectors of crime
plot(som_model, type ="property", property=getCodes(som_model)[,6], main="Crime", palette.name=coolBlueHotRed,shape="straight", border="transparent")

##plot all the component planes into the same image
par(mfrow = c(3,3)) # 3 x 3 grid
for (i in 1:7) { # loop through all of them and plot
  plot(som_model, type = "property", property = getCodes(som_model)[,i],
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed, shape="straight", border="transparent")
  #add.cluster.boundaries(som_model, som_cluster)
  }
dev.off()

#reset margins
par(mar=c(5,5,4,2))

# show the WCSS metric for kmeans for different clustering sizes.
# Can be used as a "rough" indicator of the ideal number of clusters
mydata <- getCodes(som_model) #extract codebook vectors
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
#calculate sums of squares for 2-15 clusters
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
#plot wcss
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")


############################### CLUSTERS
# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 7)

# Colour palette definition
class_colors <- c("coral3","orange3", "darkseagreen4", "khaki3", "lightpink3","mediumpurple2", "aquamarine3","peru")

# plot codes with cluster colours as background
plot(som_model, type="codes", bgcol = class_colors[som_cluster], main = "Clusters", shape="straight", palette.name=rainbow, border="transparent")
add.cluster.boundaries(som_model, som_cluster)


#######CLUSTER VARIABLE ANALYSIS
edin_data <- as.data.frame(edinburgh_map) 
cluster<-data.frame(id=edin_data$DataZone, cluster=som_cluster[som_model$unit.classif], data_train_matrix)
agg<-aggregate(cluster[,3:9], list(cluster$cluster), mean)

result<-c()#empty variables
#create same colours as in som maps
colour_scheme <- c("#FF0000FF", "#FFDB00FF", "#49FF00FF", "#00FF92FF", "#0092FFFF", "#4900FFFF", "#FF00DBFF")

for (i in 2:8) {
  aggtemp<-agg[,c(1,i)]
  aggtemp["Variable"]=names(agg)[i]
  names(aggtemp)<-c("Cluster","Value")
  result<-rbind(result,aggtemp)
}
names(result)<-c("Cluster","Value", "Variable")

##PLOT GRAPH
par(mar=c(5,5,4,2), bg="grey99")
plot(result[,1], result[,2], main = "Eruptions of Old Faithful", xlab = "Cluster", ylab = "Z-Scores", col=colour_scheme[factor(result$Variable)],type="p", pch = 16)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey90") #plot a grey rect
points(result[,1], result[,2], col=colour_scheme, type="p", pch = 16) #plot points over rect
# Add a legend
par(bg="white")
legend(5, 11, legend=names(cluster)[3:9],
       col=colour_scheme, pch = 16, horiz=FALSE, cex=0.8)


##### MAKE GEOGRAPHIC MAP

#create dataframe of the small area id and of the cluster unit
cluster_details <- data.frame(id=edin_data$DataZone, cluster=som_cluster[som_model$unit.classif])

#we can just merge our cluster details onto the fortified spatial polygon dataframe we created earlier
mappoints <- merge(edinburgh_fort, cluster_details, by.x="id", by.y="id")

# Finally map the areas and colour by cluster
ggplot(data=mappoints, aes(x=long, y=lat, group=group, fill=factor(cluster))) + 
  geom_polygon(colour="transparent")  + 
  coord_equal() + 
  scale_fill_manual(values = class_colors) 


# combine map with cluster details
ed_map <- merge(edinburgh_map, cluster_details, by.x="DataZone", by.y="id")

# save as an esri shapefile
writeOGR(obj=ed_map, dsn="../output", layer="edinburgh_map", driver="ESRI Shapefile")



