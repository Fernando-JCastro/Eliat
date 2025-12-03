#' Search and Download GenBank Nucleotide Sequences
#'
#' This function queries the NCBI Nucleotide database (GenBank) using `rentrez`,
#' retrieves full GenBank flatfile records in batches, and returns them as a
#' character vector (one element per sequence).
#'
#' @param organismo Character. Scientific or common name of the target organism (e.g., `"Homo sapiens"`).
#'   Must match NCBI taxonomy naming.
#' @param marcador Character. Optional molecular marker (e.g., `"COI"`, `"16S"`, `"ITS"`).
#'   Added to search term as `[All Fields]`.
#' @param retmax Integer. Maximum number of records to retrieve *per batch* (NCBI limit = 10,000).
#'   Default: `NULL` → auto-adjusts (5,000 if >50k hits; else 10,000).
#' @param max_intentos Integer. Maximum number of retry attempts on failure (default: 5).
#'
#' @return Character vector. Each element is a raw GenBank record (text), ending with `"//\n"`.
#'
#' @importFrom rentrez entrez_search entrez_fetch
#'
#'
#' @examples
#' \dontrun{
#' seqs <- search_gb_seq("Drosophila melanogaster", "COI", retmax = 500)
#' }
#'
#' @export
search_gb_seq <- function(organismo, marcador = "", retmax = NULL, max_intentos = 5) {
  # Build search term
  term_busqueda <- if (nzchar(marcador)) {
    paste0(organismo, "[Organism] AND ", marcador, "[All Fields]")
  } else {
    paste0(organismo, "[Organism]")
  }

  # Search in NCBI
  search_results <- rentrez::entrez_search(db = "nucleotide",
                                           term = term_busqueda,
                                           use_history = TRUE)
  total_registros <- search_results$count

  if (total_registros == 0) {
    stop("No NCBI records found for term: \"", term_busqueda, "\"", call. = FALSE)
  }

  # Auto-set retmax if NULL
  if (is.null(retmax)) {
    if (total_registros > 50000) {
      retmax <- 5000
      message("⚠️  ", total_registros, " records found. ",
              "Using retmax = 5000 for stability.")
    } else {
      retmax <- 10000
    }
  }

  # Validate retmax
  if (retmax <= 0) {
    stop("`retmax` must be > 0", call. = FALSE)
  }
  if (retmax > 10000) {
    warning("`retmax` capped at 10,000 (NCBI limit). Using 10,000.", call. = FALSE)
    retmax <- 10000
  }

  secuencias <- character()
  intentos <- 0
  exito <- FALSE

  while(!exito && intentos < max_intentos) {
    intentos <- intentos + 1
    tryCatch({
      n_paginas <- ceiling(total_registros / retmax)
      paginas <- vector("list", n_paginas)

      for(i in seq_len(n_paginas)) {
        start <- (i - 1) * retmax
        pagina_texto <- rentrez::entrez_fetch(db = "nucleotide",
                                              web_history = search_results$web_history,
                                              rettype = "gb", retmode = "text",
                                              retmax = retmax, retstart = start)
        paginas[[i]] <- pagina_texto
      }

      # Combine & split cleanly at //
      secuencias <- unlist(strsplit(paste(paginas, collapse = "\n//\n"), "\n//\n"))
      # Remove empty elements and trailing whitespace
      secuencias <- trimws(secuencias[secuencias != ""])
      exito <- TRUE
    }, error = function(e) {
      message("Attempt ", intentos, " failed: ", e$message)
      Sys.sleep(2)
    })
  }

  if (!exito) stop("Failed to complete request after ", max_intentos, " attempts", call. = FALSE)

  return(secuencias)
}

