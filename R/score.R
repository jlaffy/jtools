.score.raw = function(m, groups) {
    stopifnot(has_dim(m))
    if (is.character(groups)) groups = list(groups)  # for 1 group, if char, as list
    stopifnot(is.list(groups))
    groups = remove_missing_genes(groups = groups, m = m)
    sapply(groups, function(gr) colMeans(m[gr, ]))
}

.score.cont.mean = function(m) {
    as.matrix(colMeans(m))
}

.score.cont.median = function(m) {
    as.matrix(matrixStats::colMedians(m))
}


score = function(m,
                 groups,
                 correction = c('groups2', 'mean', 'median', 'none'),
                 groups2 = NULL,
                 m2 = NULL,
                 ...) {

    # CHECKS
    ## control matrix
    if (is.null(m2)) m2 = m
    # check that columns are the same
    stopifnot(identical(colnames(m), colnames(m2)))
    # note that genes do not need to match (e.g. m2 can have more genes)
    correction = match.arg(correction)

    # RAW SCORES
    scores = .score.raw(m, groups)
    
    # CONTROL SCORES
    if (correction == 'none') return(scores)
    else if (correction == 'mean') cont.scores = .score.cont.mean(m2)
    else if (correction == 'median') cont.scores = .score.cont.median(m2)
    else if (correction == 'groups2') {
        if (is.null(groups2)) groups2 = control_groups(groups = groups, m = m2, ...)
        if (is.list(groups2)) cont.scores = .score.raw(m = m2, groups = groups2)
        else stop('groups2 not recognised (should be list length equal to groups')}
    else stop('<correction> does not exist')

    stopifnot(nrow(scores) == nrow(cont.scores))
    stopifnot(ncol(cont.scores) == 1 || ncol(cont.scores) == ncol(scores))
    scores - cont.scores
}
