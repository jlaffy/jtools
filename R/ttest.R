

#' @title t-tests
#' @description t-tests
#' @param x matrix or character vector with matrix column names
#' @param y matrix to compare against or matrix with x and y column names
#' @param ... adjust.method and cutoff
#' @return t-tests
#' @rdname ttest
#' @export 
ttest = function(x, y, ...) {
    stopifnot(has_dim(y))
    UseMethod('ttest', x)
}

#' @export 
ttest.NULL = function(...) {
    "NULL"
}

#' @export 
ttest.character = function(x, y, ...) {
    c(x, y) %<-% split_matrix(m = y, by = x)
    ttest.matrix(x = x, y = y, ...)
}

#' @export 
ttest.matrix = function(x, y, adjust.method = 'BH', cutoff = NULL, ...) {
    x = as.matrix(x)
    y = as.matrix(y)
    stopifnot(have_equal_rownames(x, y))
    res = sapply(1:nrow(x), function(i) stats::t.test(x[i, ], y[i, ])$p.value)
    res = stats::setNames(stats::p.adjust(res, method = adjust.method), rownames(x))
    if (!is.null(cutoff) && is_p_value(cutoff)) res = res[res <= cutoff]
    res
}

#' @export 
ttest.data.frame = ttest.matrix

#' @export 
ttest.default = function(x, y, ...) {
    message('Class of <x> not recognised.')
}
