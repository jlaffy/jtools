#!/usr/bin/env Rscript

#' @title Generate ordered correlation matrices on the fly, FLexiblY. 
#' @description Calls the jtools function HC() to generate a correlation matrix with the parameters specified and subsequently order the columns and rows by hierarchical clustering, again with parameters specified by the user (defaults are recommended for single-cell expression (RNAseq) data). 
#' @param m matrix. Default: NULL
#' @param cr correlation matrix. If provided, cr will not be computed from <m>. Default: NULL
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: ''euclidean'
#' @return an ordered correlation matrix with the same number of rows and columns as there were columns in the original matrix.
#' @rdname creorder
#' @export 
creorder = function(m = NULL,
                    cr = NULL,
                    hc.method = 'average', 
                    cor.method = 'pearson',
                    compute.dist = T,
                    dist.method = 'euclidean') {

    obj = HC(m = m,
             cr = cr, 
             ord = T,
             returnSteps = T,
             hc.method = hc.method,
             cor.method = cor.method,
             compute.dist = compute.dist,
             dist.method = dist.method)

    obj$cr[obj$ord, obj$ord]
}
