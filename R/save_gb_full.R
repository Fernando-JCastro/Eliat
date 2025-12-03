#' Save GenBank Records to File (Interactive Dialog)
#'
#' Saves raw GenBank sequences (character vector or single string) to `.gb` file.
#' Ensures proper `//` separators and appends `.gb` extension.
#'
#' @param sequence_gb Character vector (from `search_gb_seq()`) or single string.
#'
#' @return Invisible `NULL`. Side effect: writes GenBank flatfile.
#'
#' @importFrom svDialogs dlg_save
#'
#' @examples
#' \dontrun{
#' seqs <- search_gb_seq("Mus musculus", retmax = 20)
#' save_gb_full(seqs)
#' }
#'
#' @export
save_gb_full <- function(sequence_gb) {
  if (base::is.vector(sequence_gb)) {
    texto_a_guardar <- base::paste(sequence_gb, collapse = "\n//\n") %>% stringr::str_trim()
    if (!base::grepl("\\n//$", texto_a_guardar)) texto_a_guardar <- base::paste0(texto_a_guardar, "\n//")
  } else {
    texto_a_guardar <- base::as.character(sequence_gb)
  }

  dlg_result <- svDialogs::dlg_save(
    default = "sequences.gb",
    title = "Save GenBank records",
    filters = list("GenBank Files" = c("gb", "gbk"))
  )

  file_path <- dlg_result$res
  if (file_path == "" || base::is.null(file_path)) {
    base::stop("Save canceled by user", call. = FALSE)
  }

  if (!base::grepl("\\.(gb|gbk)$", file_path, ignore.case = TRUE)) {
    file_path <- base::paste0(file_path, ".gb")
  }

  base::writeLines(texto_a_guardar, con = file_path)
  message("✅ GenBank file saved to: ", file_path)
}

library("svDialogs")
