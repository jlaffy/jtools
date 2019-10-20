#' @title HC
#' @description Hierarchical clustering
#' @param m matrix. Default: NULL
#' @param cr correlation matrix. If provided, cr will not be computed with m. Default: FALSE
#' @param dist distance matrix of class 'dist'. If provided, dist will not be computed with cr. Default: FALSE
#' @param hc hclust object. If provided, hc will not be computed with cr. Default: FALSE
#' @param ord ordered character vector, retrieved from hc. Default: FALSE
#' @param clusters list of clusters (character vectors), retrieved from hc. Default: FALSE
#' @param returnSteps logical indicating whether to return a list containg all intermediate objects that were computed. Default: FALSE 
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: ''euclidean'
#' @param ord.labels if FALSE, will return ordered indices rather than character vector. Default: T
#' @param h desired height at which to cut tree. Only used if the call to HC includes generation of clusters (ie. if none of cr, dist, hc, ord are set to TRUE) or if clusters is set to TRUE. Default: NULL
#' @param k desired number of clusters. Only used if the call to HC includes generation of clusters (ie. if none of cr, dist, hc, ord are set to TRUE) or if clusters is set to TRUE. Default: NULL
#' @param min.cluster.size fraction between 0 and 1 or absolute value specifying the minimum allowed number of members in a cluster. Clusters smaller than this will be discarded. Only used if the call to HC includes generation of clusters (ie. if none of cr, dist, hc, ord are set to TRUE) or if clusters is set to TRUE. Default: 5
#' @param max.cluster.size fraction between 0 and 1 or absolute value specifying the maximum allowed number of members in a cluster. Clusters larger than this will be discarded. Only used if the call to HC includes generation of clusters (ie. if none of cr, dist, hc, ord are set to TRUE) or if clusters is set to TRUE. Default: 0.8
#' @return The function proceeds through the following steps: (1) compute correlation matrix, (2) compute hierarchical clustering object, (3) retrieve ordered character vector. It will start at the point for which an object is provided. i.e. if cr is provided, then step (1) is skipped. It will either return a list with all computed objects \code{list(cr = cr, hc = hc, ord = ord)} or stop at the point at which an argument is set to TRUE. i.e. if cr = T, step (3) is skipped and
#' only the correlation matrix is returned. Thus, users can provide an expression matrix and ask for the ordered columns' character vector, or provide a correlation matrix and ask for the corresponding hierarchical clustering object and so on.
#' @seealso 
#'  \code{\link[stats]{cor}},\code{\link[stats]{hclust}},\code{\link[stats]{dist}}
#' @rdname HC
#' @export 
#' @importFrom stats cor hclust dist as.dist
HC = function(m = NULL,
              cr = FALSE,
              dist = FALSE,
              hc = FALSE,
              ord = FALSE,
		      clusters = FALSE,
              returnSteps = FALSE,
              hc.method = 'average', 
              cor.method = 'pearson',
              compute.dist = T,
              dist.method = 'euclidean',
              ord.labels = T,
  		      h = NULL,
		      k = NULL,
		      min.cluster.size = 5,
		      max.cluster.size = 0.8) {

  # CORRELATION MATRIX
  # run?
    
    List = c()
    objects_to_compute = list(cr, dist, hc, ord, clusters)
    start_computation = 0
    end_computation = length(objects_to_compute)
    custom_start = sapply(objects_to_compute, function(obj) !is.logical(obj))
    custom_end = sapply(objects_to_compute, isTRUE)

    if (any(custom_start)) {
        start_computation = max(which(custom_start))
    }

    if (any(custom_end)) {
        end_computation = max(which(custom_end))
    }

    if (start_computation == 0) {
        cr = stats::cor(m, method = cor.method)
        start_computation = start_computation + 1
    }

    List = c(List, list(cr = cr))
    
    if (end_computation == 1) {
        if (returnSteps) return(List)
        return(cr)
    }

    if (start_computation == 1) {
        if (compute.dist) dist = stats::dist(1 - cr, method = dist.method)
        else dist = stats::as.dist(1 - cr)
        start_computation = start_computation + 1
    }

    List = c(List, list(dist = dist))
    
    if (end_computation == 2) {
        if (returnSteps) return(List)
        return(dist)
    }

    if (start_computation == 2) {
        hc = stats::hclust(dist, method = hc.method)
        start_computation = start_computation + 1
    }

    List = c(List, list(hc = hc))

    if (end_computation == 3) {
        if (returnSteps) return(List)
        else return(hc)
    }

    if (start_computation == 3) {
        if (!ord.labels) ord = hc$order
        else ord = hc$labels[hc$order]
        start_computation = start_computation + 1
    }

    List = c(List, list(ord = ord))

    if (end_computation == 4) {
        if (returnSteps) return(List)
        else return(ord)
    }

    if (start_computation == 4) {
	    clusters = .extractClusters(hc = hc,
				                    h = h,
				                    k = k,
				                    min.cluster.size = min.cluster.size,
				                    max.cluster.size = max.cluster.size)
        start_computation = start_computation + 1
    }

    List = c(List, list(clusters = clusters))

    if (end_computation == 5) {
        # returns everything
        if (returnSteps) return(List)
        return(clusters)
    }

    List
}
