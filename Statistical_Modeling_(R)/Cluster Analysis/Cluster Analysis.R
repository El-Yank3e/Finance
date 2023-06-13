# Figure 15.1
# Read data file
utilities.df <- read.csv("Utilities.csv")
plot(utilities.df$Fuel_Cost ~ utilities.df$Sales,
     main = "", xlab = "Sales", ylab = "Fuel Cost",
     pch = 15, col = "blue", xlim = c(2500, 20000))
text(utilities.df$Sales, utilities.df$Fuel_Cost,
     labels = utilities.df$Company, pos = 4, cex = 0.6)

# Figure 15.2
# Remove the company names
row.names(utilities.df) <- utilities.df[, 1]
utilities.df <- utilities.df[, -1]
utilities.df
# Compute the non-normalized distances
d.euclidean <- dist(utilities.df, method = "euclidean")
d.maximum <- dist(utilities.df, method = "maximum")
d.manhattan <- dist(utilities.df, method = "manhattan")

# Table 15.4
# Normalize all attributes
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df)
utilities.df.norm
# Compute the normalized distances
d.euclidean.norm <- dist(utilities.df.norm, method = "euclidean")
d.maximum.norm <- dist(utilities.df.norm, method = "maximum")
d.manhattan.norm <- dist(utilities.df.norm, method = "manhattan")

# Figure 15.3
# Hierarchical clustering and dendrograms
hc.single <- hclust(d.euclidean.norm, method = "single")
plot(hc.single, hang = -1, ann = TRUE, main = "Single Linkage",
     sub = "", xlab = "", ylab = "")
hc.complete <- hclust(d.euclidean.norm, method = "complete")
plot(hc.complete, hang = -1, ann = TRUE, main = "Complete Linkage",
     sub = "", xlab = "", ylab = "")
hc.average <- hclust(d.euclidean.norm, method = "average")
plot(hc.average, hang = -1, ann = TRUE, main = "Average Linkage",
     sub = "", xlab = "", ylab = "")
hc.centroid <- hclust(d.euclidean.norm, method = "centroid")
plot(hc.centroid, hang = -1, ann = TRUE, main = "Centroid Linkage",
     sub = "", xlab = "", ylab = "")
hc.ward.D <- hclust(d.euclidean.norm, method = "ward.D2")
plot(hc.ward.D, hang = -1, ann = TRUE, main = "Ward's Method",
     sub = "", xlab = "", ylab = "")

# Table 15.6
# Displaying clusters in text
memb.single <- cutree(hc.single, k = 4)
memb.single
memb.complete <- cutree(hc.complete, k = 4)
memb.complete
memb.average <- cutree(hc.average, k = 4)
memb.average
memb.centroid <- cutree(hc.centroid, k = 4)
memb.centroid
memb.ward.D <- cutree(hc.ward.D, k = 4)
memb.ward.D

# Figure 15.4
# Make sure that the clustering method ("single", "complete", "average", etc.)
# matches the method in heatmap() function
# Methd = "single"
utilities.df.norm.heatmap <- utilities.df.norm
row.names(utilities.df.norm.heatmap) <- paste(
  memb.single, ": ", row.names(utilities.df.norm), sep = "")
heatmap(as.matrix(utilities.df.norm.heatmap), Colv = NA,
        hclustfun = function(x) hclust(x, method = "single"),
        col = rev(paste("gray", 1:99, sep = "")))
# Methd = "complete"
utilities.df.norm.heatmap <- utilities.df.norm
row.names(utilities.df.norm.heatmap) <- paste(
  memb.complete, ": ", row.names(utilities.df.norm), sep = "")
heatmap(as.matrix(utilities.df.norm.heatmap), Colv = NA,
        hclustfun = function(x) hclust(x, method = "complete"),
        col = rev(paste("gray", 1:99, sep = "")))
# Methd = "average"
utilities.df.norm.heatmap <- utilities.df.norm
row.names(utilities.df.norm.heatmap) <- paste(
  memb.average, ": ", row.names(utilities.df.norm), sep = "")
heatmap(as.matrix(utilities.df.norm.heatmap), Colv = NA,
        hclustfun = function(x) hclust(x, method = "average"),
        col = rev(paste("gray", 1:99, sep = "")))
# Methd = "centroid"
utilities.df.norm.heatmap <- utilities.df.norm
row.names(utilities.df.norm.heatmap) <- paste(
  memb.centroid, ": ", row.names(utilities.df.norm), sep = "")
heatmap(as.matrix(utilities.df.norm.heatmap), Colv = NA,
        hclustfun = function(x) hclust(x, method = "centroid"),
        col = rev(paste("gray", 1:99, sep = "")))
# Methd = "ward.D"
utilities.df.norm.heatmap <- utilities.df.norm
row.names(utilities.df.norm.heatmap) <- paste(
  memb.ward.D, ": ", row.names(utilities.df.norm), sep = "")
heatmap(as.matrix(utilities.df.norm.heatmap), Colv = NA,
        hclustfun = function(x) hclust(x, method = "ward.D2"),
        col = rev(paste("gray", 1:99, sep = "")))

# Figure 15.9
set.seed(2)
k <- 6
km <- kmeans(utilities.df.norm, k, trace = TRUE)
# Membership of clusters
km$cluster
# Centroids of clusters
km$centers
# Within cluster sum of squares (measure of whinin cluster distance)
km$withinss
# Clsuter size
km$size

# Figure 15.5
plot(c(0), xaxt = "n", ylab = "", type = "l",
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))
axis(1, at = c(1:8), labels = names(utilities.df), cex.axis = 0.65)
for(i in 1:k) {
  lines(km$centers[i, ], lty = i, lwd = 2,
        col = rep_len(c("red", "blue", "green", "black"), k)[i])
}
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:k)))

# Table 15.11
dist(km$centers)

# Figure 15.6
set.seed(2)
k <- c(1:8)
withinss <- c()
for(i in k){
  km <- kmeans(utilities.df.norm, i)
  withinss <- c(withinss, sum(km$withinss))
}
plot(withinss ~ k, type = "l", lwd = 2, xlab = expression(
  paste("Number of Clusters (", italic(k), ")")),
  ylab = "Total Within-Cluster Squared Distance")
points(x = k, y = withinss, pch = 19, col = "black", cex = 1.5)