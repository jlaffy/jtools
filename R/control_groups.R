
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

