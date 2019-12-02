#' read a *.gct file into your session
#' @param gct.file path to gct formatted file
#' @return matrix with a "description" attribute
#' @export
read.gct <- function(gct.file) {
    dat <- read.delim(gct.file, skip=2, header=TRUE, sep="\t",
                      check.names=F, as.is=T)
    # if (colnames(dat)[1] == "NAME") {
    #     dat <- rename(dat, Name = NAME)
    # }
    # if (colnames(dat)[2] == "DESCRIPTION") {
    #     dat <- rename(dat, Description = DESCRIPTION)
    # }

    colnames(dat)[1] <- "Name"
    colnames(dat)[2] <- "Description"

    mat <- as.matrix(select(dat, -Name, -Description)) %>%
        magrittr::set_rownames(dat$Name)
    attr(mat,"Description") <- dat$Description %>% magrittr::set_names(dat$Name)
    return(mat)
}

#' read a *.gct[.gz] file into your session
#' @param gct_file path to gct formatted file
#' @param chunk_size number of lines to read at once
#' @return matrix with a "Description" attribute
#' @export
read_gct <- function(gct_file, chunk_size = 1000) {

    if (! stringr::str_detect(gct_file, "\\.gct(.gz)?$")) {
        stop("Error: gct_file extension not recognized")
    }

    if (stringr::str_detect(gct_file, "\\.gct\\.gz$")) {
        f <- gzfile(gct_file)
    } else {
        f <- file(gct_file)
    }

    open(f)
    first_line <- readLines(f, n=1)
    second_line <- readLines(f, n=1)
    header_raw <- readLines(f, n=1)
    header <- header_raw %>%
        stringr::str_split("\t") %>%
        unlist %>%
        tail(-2)

    row_names <- c()
    row_descriptions <- c()

    dat <- matrix(nrow=0, ncol=length(header))

    repeat {
        l <- readLines(f, n = chunk_size)
        if (length(l) == 0) break
        chunk <- stringr::str_split(l, "\t") %>%
            plyr::laply(identity)
        row_names <- c(row_names, chunk[,1])
        row_descriptions <- c(row_descriptions, chunk[,2])
        dat <- rbind(dat, plyr::aaply(chunk[,-1:-2], 1, as.numeric))
    }
    close(f)

    attr(dat, "Description") <- row_descriptions

    dat %>%
        magrittr::set_rownames(row_names) %>%
        magrittr::set_colnames(header) %>%
        return

}

#' write a *.gct file to disk
#' @param mat data matrix, optionally with attribute "description"
#' @param gct.file path to file to write
#' @param description character vector; length must equal number of rows in mat
#' @export
write.gct <- function(mat, gct.file, description = attr(mat, "Description")) {

    stopifnot(!is.null(rownames(mat)))
    stopifnot(!is.null(colnames(mat)))

    if(is.null(description)) {
        description <- rownames(mat)
    }

    stopifnot(length(description) == nrow(mat))


    dat <- as.data.frame(mat) %>%
        bind_cols(Name=rownames(mat), Description=description, .)

    cat("#1.2\n", file=gct.file)
    cat(nrow(mat), ncol(mat), sep="\t", file=gct.file, append = TRUE)
    cat("\n", file=gct.file, append =  TRUE)
    cat(colnames(dat), sep="\t", file=gct.file, append = TRUE)
    cat("\n", file=gct.file, append =  TRUE)
    suppressWarnings(
        write.table(dat, file=gct.file, append = TRUE,
                quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE )
    )

}

#' read a *.gmt file into your session
#' @param gmt.file path to gmt formatted file
#' @param as.df whether to return as data frame or list
#' @return either a list of character vectors or a two column data.frame
#' @export
read.gmt <- function(gmt, as.df=F, sep="\t") {
    genesets <- readLines(gmt, warn=F) %>%
        str_split(sep) %>%
        magrittr::set_names(., laply(., function(l) l[1])) %>%
        plyr::llply(function(l) l[3:length(l)])

    if (as.df) {
        genesets.df <- plyr::ldply(genesets,
                                   function (gs) data_frame(Gene = gs),
                                   .id="GeneSet")
        return(genesets.df)
    } else {
        return(genesets)
    }
}
