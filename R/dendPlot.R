
#' @title  Plot Pretty Dendrograms
#' @description Plot pretty dendrograms. Change labels, label colours, branch colours, line-widths etc. Change the colours according to k desired number of clusters, or according to h desired height at which to cut the three, or vector <clusters> of the same length as features (leaves) in the tree and with the clusters pre-defined.
#' @param dend object of class 'dendrogram’ or ‘hclust’
#' @param k number of groups (passed to ‘cutree’).  Default: NULL
#' @param h height at which to cut tree (passed to ‘cutree’). Default: NULL
#' @param clusters a character or integer vector of cluster ids. If provided, the branches will be coloured according to these clusters. The length must match the number of leaves in the dendrogram (ie number of features). If the vector is named, the function will check the order of clusters and reorder it according to the leaves in the dendrogram if necessary. If the vector is such that the names are the cluster ids, instead of the feature ids, then the function will first flip the names in the vector and then proceed. If the vector is not named, an already correct order is assumed. Default: NULL
#' @param labels rename the labels on the dendrogram. The order of labels is checked as above for <clusters>. If <clusters> (the default), the labels will be replaced with the ID of the cluster in question. Default: clusters
#' @param groupLabels if TRUE add numeric group label. See dendextend::color_branches for details. Default: NULL
#' @param branches_lwd lwd of branches., Default: c(1, 1, 1)
#' @param branches_lty lty of branches. Default: c(1, 1, 1)
#' @param labels_cex cex of labels. Default: 0.5
#' @param do.par logical indicating whether to use default margin layout for base plot. Default: F
#' @param col vector of colours to match clusters, either defined by k, h, or clusters arguments. If the vector is shorter than the number of clusters, colours will be recycled; if it is longer, later colours will be ignored. Default: RColorBrewer::brewer.pal(9, "Set1")[-c(6:7)]
#' @param ... other arguments passed to base plot (e.g. main = 'My Dendrogram') 
#' @return base plot; if output is saved, then return an invisible list containing the dendrogram object and the clusters and labels vectors in case they were reordered/changed in function call. 
#' @seealso 
#'  \code{\link[RColorBrewer]{RColorBrewer}}
#'  \code{\link[graphics]{par}}
#'  \code{\link[stats]{setNames}}
#'  \code{\link[dendextend]{color_branches}},\code{\link[dendextend]{set}},\code{\link[dendextend]{color_labels}},\code{\link[dendextend]{labels_colors}},\code{\link[dendextend]{get_leaves_branches_col}}
#'  \code{\link[base]{labels}}
#' @rdname dendPlot
#' @export 
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics par
#' @importFrom stats setNames
#' @importFrom dendextend color_branches set color_labels labels_colors get_leaves_branches_col
dendPlot <- function(dend,
                     k = NULL,
                     h = NULL,
                     clusters = NULL,
                     labels = clusters,
                     groupLabels = NULL,
                     branches_lwd = c(1, 1, 1),
                     branches_lty = c(1, 1, 1),
                     labels_cex = 0.5,
                     do.par = F,
                     col = RColorBrewer::brewer.pal(9, 'Set1')[-c(6:7)],
                     ...) {

    # if dend is hc, make dendrogram
    if (class(dend) == 'hclust') dend = stats::as.dendrogram(dend)
    stopifnot(class(dend) == 'dendrogram') 

    if (do.par) graphics::par(mfrow = c(1,1), mar = c(5,5,1,0))

    labels.orig = labels(dend)

    if (!is.null(clusters)) {
        if (!is.null(names(clusters))) {
            if (all(labels.orig %in% clusters)) {
                clusters = stats::setNames(names(clusters), clusters)
            }
            clusters = clusters[match(labels.orig, names(clusters))]
        }
        if (!is.numeric(clusters)) clusters = as.numeric(as.factor(clusters))
    }

    if (!is.null(clusters)) {
        dend <- dendextend::color_branches(dend, clusters = clusters, col = col)
    }
    else if (!is.null(k) | !is.null(h)) {
        dend <- dendextend::color_branches(dend, k = k, h = h,groupLabels = groupLabels)
    }

    dend = dend %>%
        dendextend::set("branches_lwd", branches_lwd) %>%
        dendextend::set("branches_lty", branches_lty)

    if (!is.null(labels)) {
        if (isFALSE(labels)) {
            # remove labels
            suppressWarnings(labels(dend) <- NULL)
        }

        else if (is.character(labels)) {
            if (!is.null(names(labels))) {
                # reorder labels
                if (all(labels.orig %in% labels)) {
                    labels = stats::setNames(names(labels), labels)
                }
                labels = labels[match(labels.orig, names(labels))]
            }
            # assume correct order
            # if lengths do not match, labels will be recycled
            labels(dend) <- labels
        }

        else stop('Labels not recognised')
    }

    if (!is.null(k) | !is.null(h)) {
        dend = dendextend::color_labels(dend, k = k, h = h)
    }
    if (!is.null(clusters)) {
        dendextend::labels_colors(dend) <- dendextend::get_leaves_branches_col(dend)
    }

    dend = dend %>% dendextend::set("labels_cex", labels_cex)

    graphics::plot(dend, ...)
    invisible(list(dend = dend, clusters = clusters, labels = labels))
}

###
# AND THEN RUN:
# plot(dend)
# colored_bars(colors = the_bars, dend = dend, rowLabels = "Age", sort_by_labels_order=F)
###


# # dend <- color_labels(dend, k = 3)
# # # The same as:
# # # labels_colors(dend)  <- get_leaves_branches_col(dend)
# # plot(dend)
# labels_col", "blue") %>% plot(main = "Change label's color") # change color 
# dend15 %>% set("labels_cex", 2) %>% plot(main = "Change label's size") # change size")"
