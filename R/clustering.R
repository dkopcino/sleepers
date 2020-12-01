#' Transform cumulative probabilities (distribution) into single probabilities.
#'
#' Transform cumulative probabilities (distribution) into single probabilities.
#' Adds 1 at the end and calculates differences respectively.
#'
#' @seealso \code{\link{invlogit}}
#'
#' @param cumprobs A vector of cumulative probabilities (without the last 1).
#' @param nms Optional, names of the probabilities values.
#' @return Vector of single probabilities.
print_clusters <- function(labels, k, d) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(d[labels == i, ])
  }
}

sqr_edist = function(x, y) {
  sum((x-y)^2)
}

# function to calculate the total WSS within a cluster
wss.cluster = function(clustermat) {
  # find the centroid of the cluster, the point with all coordinates = mean
  c0 = apply(clustermat, 2, FUN=mean)
  # calculate the total sum of distances from each point (row) to the centroid
  sum(apply(clustermat, 1, FUN = function(row) { sqr_edist(row, c0) }))
}

# calculate the total WSS for all the clusters – clustering set
wss.total = function(dmatrix, labels) {
  wsstot = 0
  k = length(unique(labels))
  for (i in 1:k)
    wsstot = wsstot + wss.cluster(subset(dmatrix, labels==i))
  wsstot
}


### --> DETERMINING NUMBER OF CLUSTERS

#library(ggplot2)
#library(reshape2)

## FIND THE ELBOW
elbow_method = function(d, kmax = 10, method = "kmeans") {
  wcss = vector()
  if (method == "kmeans") {
    for (i in 1:kmax) {
      y_clust = kmeans(d, i)$cluster
      wcss[i] = wss.total(d, y_clust)
    }
  } else if (method == "hclust") {
    hc = hclust(d = dist(d, method = 'euclidean'), method = 'ward.D')
    for (i in 1:kmax) {
      y_clust = cutree(hc, i)
      wcss[i] = wss.total(d, y_clust)
    }
  }
  plot(1:kmax, wcss, type = 'b', main = paste('The Elbow Method:', method),
       xlab = 'Number of clusters', ylab = 'WCSS')
}

### --> Hierarchical Clustering
HIERARCHICAL_CLUSTERING = function(d, n_clusters) {
  library(fpc)
  cboot = clusterboot(d, clustermethod = hclustCBI, method = "ward.D", k = n_clusters)
  model_hclust = cboot$result$result
  # The results are in cboot$result. The output is in cboot$result$result.
#  summary(cboot$result)
  # Groups, partition of rows
  y_clust = cboot$result$partition
  # Vector of cluster stabilities, each should be > 0.7.
  print(cboot$bootmean)
  # How many times each cluster was dissolved (in total of 100 iterations).
  print(cboot$bootbrd)

  list(
    "dmodel" = cboot$result,
    "dclustering" = y_clust
  )
}

### --> KNN Clustering
KNN_CLUSTERING = function(d, n_clusters) {
  library(fpc)
  cboot = clusterboot(d, clustermethod = kmeansCBI, k = n_clusters)
  model_kmeans = cboot$result$result
  # The results are in cboot$result. The output is in cboot$result$result.
#  summary(cboot$result)
  # Groups, partition of rows
  y_clust = cboot$result$partition
  # Vector of cluster stabilities, each should be > 0.7.
  print(cboot$bootmean)
  # How many times each cluster was dissolved (in total of 100 iterations).
  print(cboot$bootbrd)

  list(
    "dmodel" = cboot$result,
    "dclustering" = y_clust
  )
}

### --> Mclust Clustering
MCLUST_CLUSTERING = function(d, n_clusters) {
  # Uses only numeric data!
  library(mclust)

  df = d
  for (v in colnames(d)) {
    if (is.numeric(d[[v]])) {
      df[[v]] = d[[v]]
    } else if (is.factor(d[[v]])) {
      df[[v]] = as.numeric(d[[v]])
    } else {
      # what to do with other types?
    }
  }

  model_mclust = Mclust(df, G = n_clusters)

  #summary(model_mclust)
  # Groups, partition of rows
  y_clust = model_mclust$classification


  list(
    "dmodel" = model_mclust,
    "dclustering" = y_clust
  )
}

### --> POLCA Clustering
POLCA_CLUSTERING = function(d, n_clusters) {
  # Works with only factor variables!
  df = d
  for (v in colnames(d)) {
    if (is.factor(d[[v]])) {
      df[[v]] = d[[v]]
    } else if (is.numeric(d[[v]])) {
      df[[v]] = factor(ifelse(d[[v]] < median(d[[v]]), 1, 2))
    } else {
      # what to do with other types?
    }
  }

  library(poLCA)
  fff = paste(paste("cbind(", paste(colnames(df), collapse = ","), sep=""), ") ~ 1", sep = "")
  model_polca = poLCA(as.formula(fff), data = df, nclass = n_clusters)

  print(model_polca$bic)
  print(model_polca$aic)

  # Groups, partition of rows
  y_clust = model_polca$predclass

  list(
    "dmodel" = model_polca,
    "dclustering" = y_clust
  )
}
