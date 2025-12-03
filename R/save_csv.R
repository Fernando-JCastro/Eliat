#' Save Metadata Data Frame as CSV (Interactive Dialog)
#'
#' Opens a file-save dialog (via `svDialogs`) to save a data frame as CSV.
#' Automatically appends `.csv` extension if missing.
#'
#' @param data A data frame (e.g., output from `get_metadata()`).
#'
#' @return Invisible `NULL`. Side effect: writes CSV file.
#'
#' @importFrom svDialogs dlg_save
#'
#' @examples
#' \dontrun{
#' meta <- get_metadata(seqs)
#' save_csv(meta)
#' }
#'
#' @export
save_csv <- function(data) {
  if (!requireNamespace("svDialogs", install.packages("svDialogs")))


  dlg_res <- svDialogs::dlg_save(
    title = "Save metadata as CSV",
    filters = list("CSV Files" = c("csv"), "All files" = "*")
  )

  if (is.null(dlg_res) || length(dlg_res$res) == 0 || dlg_res$res == "") {
    stop("Save canceled by user", call. = FALSE)
  }

  file_path <- dlg_res$res
  if (!grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    file_path <- paste0(file_path, ".csv")
  }

  utils::write.csv(data, file = file_path, row.names = FALSE, fileEncoding = "UTF-8")
  message("✅ CSV saved to: ", file_path)
}
