#' Count Occurrences of Gene Products in Metadata
#'
#' Extracts and tallies gene product names (e.g., "cytochrome c oxidase subunit I")
#' from a comma-separated column (e.g., `Products` from `get_metadata()`).
#'
#' @param df A data frame (e.g., output from `get_metadata()`).
#' @param columna Unquoted column name (e.g., `Products`).
#'
#' @return A `tibble` with columns `value` (product name) and `cantidad` (count).
#'
#' @importFrom dplyr pull
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' meta <- get_metadata(seqs)
#' get_products_counts(meta, Products)
#' }
#'
#' @export
get_products_counts <- function(df, columna) {
  df %>%
    dplyr::pull({{columna}}) %>%
    strsplit(split = ",") %>%
    unlist(use.names = FALSE) %>%
    trimws() %>%
    tibble::as_tibble_col(column_name = "value") %>%
    dplyr::count(value, name = "cantidad") %>%
    dplyr::arrange(dplyr::desc(cantidad))
}
