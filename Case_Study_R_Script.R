# INSTALLING THE REQUIRED PACKAGES
install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster")
install.packages("openxlsx")

# THE NECESSARY LIBRARIES
library(readxl)
library(tidyverse)
library(cluster)
library(openxlsx)

# IMPORTING THE DATA
SmartWatch_Data_File <- read_excel(file.choose())

# Viewing the dataset to get an idea
View(SmartWatch_Data_File)

# Checking the structure of the data
names(SmartWatch_Data_File)  
summary(SmartWatch_Data_File)
str(SmartWatch_Data_File)

# SELECTING THE RELEVANT FEATURES REQUIRED FOR CLUSTERING
features <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", 
              "Wellness", "Athlete", "Style")
df_cluster <- SmartWatch_Data_File[, features] 

# STANDARDIZING THE DATA
df_scaled <- scale(df_cluster)

# CALCULATING EUCLIDEAN DISTANCE
distance <- dist(df_scaled, method = "euclidean")

#PERFORMING HIERARCHICAL CLUSTERING using WARD'S METHOD
hc <- hclust(distance, method = "ward.D2")

# PLOTTING THE  DENDROGRAM
plot(hc, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")
abline(h = 35, col = "red", lty = 2)

# DETERMINING THE OPTIMAL NUMBER OF CLUSTERS USING ELBOW METHOD)
x <- c(1:10)
sort_height <- sort(hc$height, decreasing = TRUE)  
y <- sort_height[1:10]

plot(x, y, type = "b", main = "Elbow Plot", xlab = "Cluster", ylab = "WSS")
lines(x, y, col = "blue")

#CHOOSING NUMBER OF CLUSTERS
num_clusters <- 3

# COMBINING ORIGINAL DATA WITH THE CLUSTER ASSIGNMENTS
df_final <- cbind(SmartWatch_Data_File, Cluster = cutree(hc, k = num_clusters))

#UPDATED DATASET
View(df_final)

# DISPLAYING CLUSTER SIZES
print(table(df_final$Cluster))  

# CALCULATING SEGMENT PROPORTIONS
proportions <- table(df_final$Cluster) / nrow(df_final)
percentages <- proportions * 100
print(percentages)

# SEGMENT MEAN CHARACTERISTICS
segments <- df_final %>% 
  group_by(Cluster) %>% 
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))

# DISPLAYING SEGMENT SUMMARY
print(segments)

# EXPORTING THE SEGMENT SUMMARY TO EXCEL FILE
write.xlsx(segments, "Smartwatch_Segment_Summary.xlsx")

# END OF SCRIPT
