library(stats)

#' Calculate total WSS within a cluster.
#'
#' Calculates sum of square distances from the mean.
#'
#' @param clustermat A data matrix representing cluster.
#'
#' @return Sum of squares within the cluster.
wss.cluster = function(clustermat) {
  # find the centroid of the cluster, the point with all coordinates = mean
  c0 = apply(clustermat, 2, FUN = mean)
  # calculate the total sum of distances from each point (row) to the centroid
  sum(apply(clustermat, 1, FUN = function(row) { sum((row-c0)^2) }))
}

#' Calculate total WSS for all the clusters – clustering set.
#'
#' @seealso \code{\link{wss.cluster}}
#'
#' @param dmatrix A data matrix.
#' @param labels Clustering of the data matrix.
#'
#' @return Sum of squares within all the clusters.
wss.total = function(dmatrix, labels) {
  sum(sapply(1:length(unique(labels)), function(i) wss.cluster(subset(dmatrix, labels==i))))
}

#' Calculate total WSS for all the clusters created with kmeans method.
#'
#' @seealso \code{\link{wss.total}}
#'
#' @import stats
#'
#' @param d A data matrix.
#' @param kmax Maximal number of clusters to make the clustering.
#'
#' @return A vector of sum of squares within all the clusters for each number of clusters from 1 to kmax.
kmeans_wss = function(d, kmax = 10) {
  sapply(1:kmax, function(i) wss.total(d, kmeans(d, i)$cluster))
}

#' Calculate total WSS for all the clusters created with hierarchical clustering method.
#'
#' @seealso \code{\link{wss.total}}
#'
#' @import stats
#'
#' @param d A data matrix.
#' @param kmax Maximal number of clusters to make the clustering.
#'
#' @return A vector of sum of squares within all the clusters for each number of clusters from 1 to kmax.
hclust_wss = function(d, kmax = 10) {
  hc = hclust(d = dist(d, method = 'euclidean'), method = 'ward.D')
  sapply(1:kmax, function(i) wss.total(d, cutree(hc, i)))
}

#' Plot a graph (number of clusters, total sum of squares within all the clusters created with the given method).
#'
#' Method must be either "kmeans" or "hclust". Default method, if none is given, is hclust.
#'
#' @seealso \code{\link{kmeans_wss}}
#' @seealso \code{\link{hclust_wss}}
#'
#' @param d A data matrix.
#' @param kmax Maximal number of clusters to plot the clustering.
#' @param method Clustering method, either "kmeans" or "hclust".
#'
#' @return No return. Plots a graph.
elbow_method = function(d, kmax = 10, method) {
  if (method == "kmeans") {
    wcss = kmeans_wss(d, kmax)
  } else {
    wcss = hclust_wss(d, kmax)
  }
  plot(1:kmax, wcss, type = 'b', main = paste('The Elbow Method:', method), xlab = 'Number of clusters', ylab = 'WCSS')
}
