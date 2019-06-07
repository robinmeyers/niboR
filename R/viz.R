
#' Format a vector of numeric values according
#' to the International System of Units.
#' http://en.wikipedia.org/wiki/SI_prefix
#'
#' Based on code by Ben Tupper
#' https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
#'
#' @param ... args passed to format()
#'
#' @return A function to format a vector of strings using SI prefix notation
#' @description Reverts gene names that have been converted to dates by Excel.
#' @export
format_si <- function(...) {
    function(x) {
        limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                    1e-9,  1e-6,  1e-3,  1e0,   1e3,
                    1e6,   1e9,   1e12,  1e15,  1e18,
                    1e21,  1e24)
        prefix <- c("y",   "z",   "a",   "f",   "p",
                    "n",   "u",   "m",   "",   "k",
                    "M",   "G",   "T",   "P",   "E",
                    "Z",   "Y")
        prefix <- ifelse(prefix != "", paste0(" ", prefix), "")

        # Vector with array indices according to position in intervals
        i <- findInterval(abs(x), limits, rightmost.closed = T)

        # Set prefix to " " for very small values < 1e-24
        i <- ifelse(i == 0, which(limits == 1e0), i)

        paste0(format(round(x/limits[i], 1),
                     trim = TRUE, scientific = FALSE, ...),
              prefix[i])
    }
}

#' Pretty breaks for log10 scale
#' @description function to set pretty log10 breaks
#' @export
pretty_log10_breaks <- function() {
    function(x) {
        top_10 <- ceiling(log10(max(x)))
        bottom_10 <- floor(log10(min(x)))

        breaks = 10^rep(bottom_10:top_10, each=3) * c(1,2,5)

    }
}

#' Pretty limits for log10 scale
#' @param x numeric vector to find pretty log10 limits outside of range
#' @return length two vector of limits
#' @export
pretty_log10_limits <- function(x) {
    max_x <- log10(max(x))
    options_upper_x <- c(10^floor(max_x), 2*10^(floor(max_x)),
                         5*10^floor(max_x), 10^ceiling(max_x))
    upper_x <- min(options_upper_x[options_upper_x >= 10^max_x])
    min_x <- log10(min(x))
    options_lower_x <- c(10^floor(min_x), 2*10^(floor(min_x)),
                         5*10^floor(min_x), 10^ceiling(min_x))
    lower_x <- max(options_lower_x[options_lower_x <= 10^min_x])
    return(c(lower_x, upper_x))
}

#' Pretty log10 scale
#' @description pretty log10 scale for ggplot
#' @export
scale_y_log10_pretty <- function (...)
{
    scale_y_continuous(..., trans = scales::log10_trans(),
                       labels = ifelse(hasArg(labels), labels,
                                       format_si()),
                       breaks = ifelse(hasArg(breaks), breaks,
                                       pretty_log10_breaks()))
}


#' Pretty log10 scale
#' @description pretty log10 scale for ggplot
#' @export
scale_x_log10_pretty <- function (...)
{
    scale_x_continuous(..., trans = scales::log10_trans(),
                       labels = ifelse(hasArg(labels), labels,
                                       format_si()),
                       breaks = ifelse(hasArg(breaks), breaks,
                                       pretty_log10_breaks()))
}

