
#' Attempts to reverse adverse effects of opening a table in Excel.
#'
#' @param df data frame
#' @param x gene column name
#'
#' @return data frame with fixed gene column
#' @description Reverts gene names that have been converted to dates by Excel.
#' @export
de_excelify_genes <- function(df, x=Gene) {

    x <- enquo(x)

    df %>%
        mutate(!! quo_name(x) := ifelse(is.na(as.integer(!! x)), !! x,
                                        as.Date(as.integer(!! x), origin="1900-01-01") %>%
                                            format("%d-%b") %>% str_replace("-0", "-"))) %>%
        mutate(!! quo_name(x) := str_replace(!! x, "(\\d+)-Sep", "SEPT\\1"),
               !! quo_name(x) := str_replace(!! x, "(\\d+)-Mar", "MARCH\\1"))

}
