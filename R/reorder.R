#!/usr/bin/env Rscript

#' @title Reorder matrices on the fly, FLexiblY. 
#' @description Calls the jtools function HC() to order the columns and rows by hierarchical clustering, again with parameters specified by the user (defaults are recommended for single-cell expression (RNAseq) data). 
#' @param m matrix to be reordered Default: NULL
#' @param cr correlation matrix to be reordered. This will allow (1) the correlation matrix to be used, rather than computed, in defining the hierarchical clustering order and (2) if row and col are both set to TRUE, the clustering will only be run once and the ordering applied to both rows and columns. Default: NULL
#' @param col logical indicating whether to reorder columns. Default: TRUE
#' @param row logical indicating whether to reorder rows. Default: TRUE
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: ''euclidean'
#' @return an ordered matrix with the same number of rows and columns as there were columns in the original matrix.
#' @rdname reorder
#' @export 
reorder = function(m = NULL,
                   cr = NULL,
                   col = T,
                   row = T,
                   hc.method = 'average', 
                   cor.method = 'pearson',
                   compute.dist = T,
                   dist.method = 'euclidean') {

    if (!is.null(m) & is.null(cr)) X = m
    else if (is.null(m) & !is.null(cr)) X = cr
    else stop('Please provide one of <m> or <cr> matrices.')

    if (col) {
        ord = HC(m = m,
                 cr = cr,
                 ord = T,
                 returnSteps = F,
                 hc.method = hc.method,
                 cor.method = cor.method,
                 compute.dist = compute.dist,
                 dist.method = dist.method)
        X = X[, ord]
    }

    if (row) {
        if (!is.null(cr)) {
            if (col) {
                tord = ord
            } else {
                tord = HC(m = m,
                          cr = cr,
                          ord = T,
                          returnSteps = F,
                          hc.method = hc.method,
                          cor.method = cor.method,
                          compute.dist = compute.dist,
                          dist.method = dist.method)
            }
        } else {
            tord = HC(m = t(m),
                      ord = T,
                      returnSteps = F,
                      hc.method = hc.method,
                      cor.method = cor.method,
                      compute.dist = compute.dist,
                      dist.method = dist.method)
        }
        X = X[tord, ]
    }

    X
}
