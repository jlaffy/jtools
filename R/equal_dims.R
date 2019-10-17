

#' @title Intersection of N atomic vectors
#' @description Return the intersection of any number of atomic vectors provided on input
#' @param ... atomic vectors to intersect
#' @return intersect of atomic vectors
#' @rdname intersect_vctrs
intersect_vctrs = function(...) {
    dots = list(...)
    Reduce(intersect, dots)
}

#' @title Subset matrices to contain the same rows
#' @description subset matrices to contain the same rows
#' @param ... matrices to subset
#' @param rows character vector; additional rownames to subset matrices by. Default: NULL
#' @return matrices with equal rows 
#' @examples 
#' \dontrun{
#'  c(m1, m2) %<-% equal_rows(m1, m2, rows = NULL)
#' }
#' @rdname equal_rows
#' @export 
equal_rows = function(..., rows = NULL) {
    mats = list(...)
    matRows = do.call(intersect_vctrs, sapply(mats, rownames, simplify = F))
    if (!is.null(rows)) rows = intersect(rows, matRows)
    else rows = matRows
    sapply(1:length(mats), function(i) mats[[i]][rows, ], simplify = F)
}

#' @title Subset matrices to contain the same columns
#' @description subset matrices to contain the same columns
#' @param ... matrices to subset
#' @param cols character vector; additional colnames to subset matrices by. Default: NULL
#' @return matrices with equal columns 
#' @examples 
#' \dontrun{
#'  c(m1, m2) %<-% equal_cols(m1, m2, cols = NULL)
#' }
#' @rdname equal_cols
#' @export 
equal_cols = function(..., cols = NULL) {
    mats = list(...)
    matCols = do.call(intersect_vctrs, sapply(mats, colnames, simplify = F))
    if (!is.null(cols)) cols = intersect(cols, matCols)
    else cols = matCols
    sapply(1:length(mats), function(i) mats[[i]][, cols], simplify = F)
}
