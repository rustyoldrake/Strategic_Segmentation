######################################################
### Experimental R Code.  
### Code to Generate a SIMULATED POPULATION CSV file so we can play
### "Imagine All the people"
###  Basic script to generate some semi-random, patchy data for us to cluster and tell stories with
######################################################

library(RCurl) # to talk to Watson - REST APIS # install.packages("RCurl") # if the package is not already installed
library(httr) # comms
library(XML)  # comms and data
library(data.table) # data shaping
library(reshape2) # data shaping
library(tidyr) # data cleaning
library(dplyr) # data cleaning
library(png) # for the presenting of images
library(diffusionMap) # http://www.stat.berkeley.edu/~jwrichar/software/diffusionMap-manual.pdf
# implements diffusion map method of data parametrization, including creation and visualization of diffusion map, clustering with diffusion K-means and regression using adaptive regression model
library(kohonen)
library(randomForest)

# Wardrobe
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/Project_Gravity_Well")
getwd()
#source("keys.R") # this files is where you put your Access Credentials from Bluemix (username and password)

#### this is ONLY to get test data - this is NOT Kosher Stats method. Worst practices :)
headcount <- 10000

#### OK - let's begin - data is our population of 100o 
x <- seq(0, 100, length=headcount)
hx <- dnorm(scale(x))

# rt(a,b) returns a random draws from a t distribution centered at 0 with b degrees of freedom.
d <- rt(headcount, df=10) #dems
d <- sort(d) # list of semirandom democrats with 'meh' middle
head(d)

r <- rt(headcount, df=10) #republicans
r <- -sort(r)  # list of semirandom democrats with 'meh' middle
head(r)

qqnorm(r) #look  
qqnorm(d) #look

# here we go!  if you are a 'real' stats person, close your eyes :)
data <- data.frame(index=1:headcount,vocal_democrat=d,vocal_republican=r,silent_majority=rt(headcount, df=10),dc_outsider=rt(headcount, df=15),economy=rt(headcount, df=10))
data <- data[order(-data$vocal_democrat), ]
plot(data[2:6])

# OK - if you are loud, and high ampliude political, you are NOT SILENT majority :)
data$silent_majority[data$vocal_democrat > 2] <- 0
data$silent_majority[data$vocal_republican > 2] <- 0
data$silent_majority[data$dc_outsider > 3] <- 0  # vocal about this issue? 
data$silent_majority[data$economy > 4] <- 0  # vocal about this issue? 

## this is a 'silence score' shhhh - are you not tweeting or talkign about politics?  you are interesting and valuable
data$silent_majority <- abs(data$silent_majority) #absolute  value
plot(data[2:6])

summary(data$silent_majority)
cutoff <- summary(data$silent_majority)[5] # 5 is 3rd quartile

## and if you are silent, you're not talking about DC or economy
data$dc_outsider[data$silent_majority > cutoff] <- 0
data$economy[data$silent_majority > cutoff] <- 0
data$dc_outsider <- -abs(data$dc_outsider) # only negative sentiment here :)
data$dc_outsider <- data$dc_outsider*0.1

data <- data[2:6] # dont need index anymore
plot(data)

###### OK - let's play with KOHONEN SELF ORGANIZING MAPS

data.sc <- scale(data)
set.seed(7)
som_grid <- somgrid(xdim = 3, ydim=3, topo="rectangular")
data.som <- som(data.sc,  grid = som_grid)
plot(data.som, palette.name = coolBlueHotRed, main = "Voter Segmentation")

dists <- unit.distances(data.som$grid, toroidal=TRUE)
plot(data.som, type="property", property=dists[1,],
     main="Distances to unit 1 (toroidal)", zlim=c(0,6),
     palette = rainbow, ncolors = 7, contin = TRUE)
dev.off()

# ref: http://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/
par(mfrow=c(3,3))

plot(data.som, type="codes") 

## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes)), 6)
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)

plot(data.som, type="count") 
#plot(data.som, type="changes") 
plot(data.som, type="dist.neighbours") 

features <- dim(data.som$codes)[2]

for(i in 1:features)
{
  print(i)
  plot(data.som, type = "property", property = data.som$codes[,i], main=colnames(data.som$codes)[i], palette.name=coolBlueHotRed)
  Sys.sleep(.01)
  }

graphics.off()

###########
som_model <- som(data.sc,  grid = somgrid(10, 10, "hexagonal"))
mydata <- som_model$codes 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) { wss[i] <- sum(kmeans(mydata, centers=i)$withinss) }
plot(wss)







### OK - Let's play with Diffusion Map

D = dist(scale(data)) # use Euclidean distance on data (no index)
## DIST: This function computes and returns the distance matrix computed by using the specified distance measure to compute the distances between the rows of a data matrix.


## diffuse: Uses the pair-wise distance matrix for a data set to compute the diffusion map coefficients. Computes the Markov transition probability matrix, and its eigenvalues and left & right eigenvectors. Returns a 'dmap' object.
#eps = 250 and t=1 and neigen = 2 is nice


par(mfrow=c(2,2))

dmap = diffuse(D,eps.val=10, t=100, neigen=2) 
plot(dmap$X[,1],dmap$X[,2],
     xlab="Diffusion Map Coordinate 1", ylab="Diffusion Map Coordinate 2",
     main="Diffusion Map of Voters")

dmap = diffuse(D,eps.val=250, t=1, neigen=2)
plot(dmap$X[,1],dmap$X[,2],
     xlab="Diffusion Map Coordinate 1", ylab="Diffusion Map Coordinate 2",
     main="Diffusion Map of Voters")

dmap = diffuse(D,eps.val=20) 
plot(dmap$X[,1],dmap$X[,2],col=data$silent_majority,pch=paste(data$silent_majority), 
     xlab="Diffusion Map Coordinate 1", ylab="Diffusion Map Coordinate 2",
     main="Diffusion Map of Voters")

dmap = diffuse(D,eps.val=4) 
plot(dmap$X[,1],dmap$X[,2],col=data$silent_majority,pch=paste(data$silent_majority), 
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2",
     main="Diffusion Map of Voters")

# edit DIFFUSE variable values - what changes? 


graphics.off()
  




#====
graphics.off()
par(mfrow=c(1,2))  
data.sc <- scale(data)
set.seed(7)
som_grid <- somgrid(xdim = 3, ydim=3, topo="rectangular")
data.som <- som(data.sc,  grid = som_grid)

#pies
plot(data.som, palette.name = coolBlueHotRed, main = paste("Voter Segmentation \n", dim(data)[1], "eligible voters"))
#and count heatmap
plot(data.som, type="count",palette.name = coolBlueHotRed, main = paste("Voter Segmentation \n eligible voters per group")) 

dists <- unit.distances(data.som$grid, toroidal=TRUE)
plot(data.som, type="property", property=dists[1,],
     main="Distances to unit 1 (toroidal)", zlim=c(0,6),
     palette = rainbow, ncolors = 7, contin = TRUE)
dev.off()

# ref: http://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/
par(mfrow=c(3,3))

plot(data.som, type="codes") 

## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes)), 6)
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)

plot(data.som, type="count") 
#plot(data.som, type="changes") 
plot(data.som, type="dist.neighbours") 

features <- dim(data.som$codes)[2]





