install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
library(ggplot2)
library(tidyr)
library(cluster)
library(factoextra)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

mk.df <- readxl::read_excel("C:/Study/Projects/R Group project/marketing_data.xlsx")
summary(mk.df)
mk.df

#Calculate Age of the customer
mk.df['Age'] <- 2021-mk.df$Year_Birth
mk.df['Age'] <- as.numeric(unlist(mk.df['Age']))
summary(mk.df)


#Total Amont Spend
mk.df["TotalAmount"] <- mk.df$MntWines+mk.df$MntFruits+mk.df$MntFishProducts+mk.df$MntGoldProds+mk.df$MntMeatProducts+mk.df$MntSweetProducts
mk.df


#Kidhome + Teenhome
mk.df["Dependents"] <- mk.df$Kidhome+mk.df$Teenhome
summary(mk.df)
View(mk.df)

#Total responses
mk.df["TotalResponses"] <- mk.df$Response+mk.df$AcceptedCmp1+mk.df$AcceptedCmp2+mk.df$AcceptedCmp3+
                            mk.df$AcceptedCmp4+mk.df$AcceptedCmp5

#Remove NA rows of Income from data 
mk.df<-na.omit(mk.df)
summary(mk.df)

#---------Age and Amount spend
ggplot(mk.df, aes(x = Age, y = TotalAmount)) +
  geom_col(position = "dodge")+

boxplot(mk.df$TotalAmount ~ mk.df$Age, xlab = "Age", ylab = "TotalAmount", col = "#03F8DA")  
#For Age we can not see any certain pattern on spending the money however it has a outliers 
#So we can remove three records where ages are 121,122,128

mk.df <- mk.df[!(mk.df$Age==121|mk.df$Age==122|mk.df$Age==128),]
#-------------Dependents VS Total Amount Spend
ggplot(mk.df, aes(x = Dependents, y = TotalAmount)) +
  geom_col(position = "dodge")+scale_fill_brewer(palette="Greens")+ theme_minimal()
#BarPlot
boxplot(mk.df$TotalAmount ~ mk.df$Dependents, xlab = "Dependents", ylab = "TotalAmount", col = "#03F8DA")  
#The person who will have no dependents is having more expenditure


#--------Education level vs Income
boxplot(mk.df$Income ~ mk.df$Education, xlab = "Education", ylab = "Income", col = "#03F8DA")  

#--Education level VS total amount spend
boxplot(mk.df$TotalAmount ~ mk.df$Education, xlab = "Education", ylab = "TotalAmount", col = "#03F8DA")  
#Graduates have a highest income and highest spending rate because it has a one outlier 

#---Marital Status VS Total Amount Spend 

mk.df <- mk.df[!(mk.df$Marital_Status=="Absurd"|mk.df$Marital_Status=="YOLO"),]

boxplot(mk.df$TotalAmount ~ mk.df$Marital_Status, xlab = "Marital_Status", ylab = "TotalAmount", col = "#03F8DA")  
#Single Spend more 
#--------That means we can conclude that 0 dependents single people spend more on shopping

#---Country wise accepted campaign 
 Tab<- mk.df %>%
  group_by(mk.df$Country) %>%
  summarise(across(c(AcceptedCmp1, AcceptedCmp2,AcceptedCmp3,AcceptedCmp4,AcceptedCmp5,Response), sum))

 #-----K-means-----
 #Income,Recency(Number of days since customers last purchase),Age,TotalAmount,Dependents,TotalResponses
View(mk.df)
 
 mk.df.neededcolumns <- mk.df[c(5,9,29,30,31,32)]
 
 
 mk.df.normalize1 <- scale(mk.df.neededcolumns)
 mk.df.normalize1
 

 
 #elbow chart 
 fviz_nbclust(mk.df.normalize1, kmeans, method = "wss") + theme_minimal() + ggtitle("Elbow Chart")
 #As seen from the elbow graph, the slope changes at k=2. 
 #However, since spltting the dataset into 2 groups would not be very beneficial for segmenting customers, 
 #we further evaluate clusters for higher values of k.
 
 #------K-means with 4 clusters 
 km55.clusters <- eclust(mk.df.normalize1, "kmeans", k = 5, nstart = 100, graph = TRUE)
 
 km44.clusters <- eclust(mk.df.normalize1, "kmeans", k = 4, nstart = 100, graph = TRUE)
 
 km33.clusters <- eclust(mk.df.normalize1, "kmeans", k = 3, nstart = 100, graph = TRUE)
 
 km22.clusters <- eclust(mk.df.normalize1, "kmeans", k = 2, nstart = 100, graph = TRUE)
 
 #We will go ahead with 3 clusters
 
 
 #Interpret the clusters that have been created and provide a meaningful name for each. 
 #Finding the mean of all the values
 df1 <- data.frame(mk.df.normalize1, km33.clusters$cluster) # append cluster membership
 Cluster_mean1 <- aggregate(df1, by=list(km33.clusters$cluster), FUN=mean)
 Cluster_mean1
 
 
 
 ####################################################################################################
 #To find the size of clusters 
 Cluster_sum <- aggregate(df1, by=list(km33.clusters$cluster), FUN=sum) 
 d1 <- transform(Cluster_sum, clusterSize = km33.clusters.cluster / Group.1)
 d1 <- transform(d1, km33.clusters.cluster= km33.clusters.cluster/ clusterSize)
 d1
 #Appending the size of clusters with the mean of all other values
 Cluster_mean1$clusterSize <- d1$clusterSize
 Cluster_mean1$clusterPCT <- (d1$clusterSize*100)/2213
 
 # transpose to change from horizontal to vertical
 temp1 <- t(Cluster_mean1)
 
 round_df <- function(x, digits) {
   # round all numeric variables
   # x: data frame 
   # digits: number of digits to round
   numeric_columns    <- sapply(x, class) == 'numeric'
   x[numeric_columns] <-  round(x[numeric_columns], digits)
   x
 }
 
 temp2 <- round_df (temp1,2)
 temp2
 
 #-- Cluster 1 [Low income group]
#This cluster has the highest number of customers(56%) their income is very low as compared to other 2 groups
#They are not recent purchasers
#They have dependents
#their total responses to campaigns are the lowest 
 
 #--Cluster 2[Highest Income Group]
 #This cluster has the lowest number of customers(10%) their income is highest 
 #They are not a recent purchasers(so we can work on the frequency)
 #Total Amount they are spending is the highest And they have less number of dependents as compare to other 2 groups
 #their total responses to campaigns is the highest
 
 
 #----Cluster 3[Mid level income] 
#This cluster has 34% of total customers and their income is in a middle range as compare to 2 clusters
#The age group of this cluster is highest as compare to other clusters
#Their exenditure is also mid level
 #Recent visitors
#Total responses are also in mid scale

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 km3.clusters <- kmeans(mk.df.normalize1, 3, nstart=100)
 fviz_cluster(km3.clusters, data = mk.df.normalize1,
              palette = c("#2E9FDF", "#000000", "#E7B800"), 
              geom = "point",
              show.clust.cent = TRUE,
              ellipse.type = "convex", 
              ggtheme =theme_minimal(),
 )
 
 
 
 