
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param groups PARAM_DESCRIPTION
#' @param bins PARAM_DESCRIPTION, Default: NULL
#' @param mat2bin PARAM_DESCRIPTION, Default: NULL
#' @param nbin PARAM_DESCRIPTION, Default: 30
#' @param n PARAM_DESCRIPTION, Default: 100
#' @param replace PARAM_DESCRIPTION, Default: F
#' @param return.details PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname control_groups
#' @export 
control_groups = function(groups,
                          bins = NULL,
                          mat2bin = NULL,
                          nbin = 30,
                          n = 100,
                          replace = F,
                          return.details = F) {

    stopifnot(is.list(groups))
    if (is.null(bins) & is.null(mat2bin)) {
        stop('If bins not provided, they must be calculated from <mat2bin> matrix')
    }
    if (is.null(bins)) {
        bins = bin(mat = mat2bin, breaks = nbin)
    }

    cont.groups = sapply(groups, binmatch, bins = bins, n = n, replace = replace, simplify = F)
    if (return.details) {
        return(list(cont.groups = cont.groups, bins = bins))
    }
    cont.groups
}

