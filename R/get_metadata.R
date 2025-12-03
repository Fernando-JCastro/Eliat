#' Extract Structured Metadata from GenBank Records
#'
#' Parses raw GenBank flatfile text (output from `search_gb_seq()`) into a tidy
#' data frame with taxonomy, geography, specimen details, and more.
#'
#' @param secuencias Character vector. Output from `search_gb_seq()`.
#'
#' @return A `data.frame` with columns:
#'   - `Locus`, `Accession`, `Definition`, `Organism`, `Country`, `City`,
#'   - `latitude`, `longitude`, `Organelle`, `Specimen_voucher`, `Products`,
#'   - Taxonomic ranks: `Kingdom`, `Phylum`, `Subphylum`, `Class`, `Order`, `Family`, `Genus`
#'
#' @importFrom stringr str_extract str_extract_all
#' @importFrom dplyr bind_rows %>%
#' @importFrom tidyr separate
#'
#' @examples
#' \dontrun{
#' seqs <- search_gb_seq("Panthera leo", retmax = 10)
#' meta <- get_metadata(seqs)
#' }
#'
#' @export
get_metadata <- function(secuencias) {
  get_metadata1 <- function(sequence) {
    locus <- stringr::str_extract(sequence, "(?<=LOCUS\\s)([^\\n]+)")
    accession <- stringr::str_extract(sequence, "(?<=ACCESSION\\s)([^\\n]+)")
    organism <- stringr::str_extract(sequence, "(?<=ORGANISM\\s)([^\\n]+)")
    lat_lon <- stringr::str_extract(sequence, "(?<=lat_lon=\")[^\"]+")
    geo_loc_name <- stringr::str_extract(sequence, "(?<=geo_loc_name=\")[^\"]+")
    definition <- stringr::str_extract(sequence, "(?<=DEFINITION\\s)([^\\n]+)")
    organelle <- stringr::str_extract(sequence, "(?<=organelle=\")[^\"]+")
    specimen_voucher <- stringr::str_extract(sequence, "(?<=specimen_voucher=\")[^\"]+")
    products <- stringr::str_extract_all(sequence, "(?<=product=\")[^\"]+")[[1]]
    taxonomy <- stringr::str_extract(sequence, "(?<=ORGANISM\\s)[\\s\\S]+?\\n\\S")

    data.frame(
      Locus = locus,
      Accession = accession,
      Taxonomy = taxonomy,
      Organism = organism,
      Lat_Lon = lat_lon,
      Geo_Loc_Name = geo_loc_name,
      Definition = definition,
      Organelle = organelle,
      Specimen_voucher = specimen_voucher,
      Products = paste(unique(products), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }

  data <- dplyr::bind_rows(lapply(secuencias, get_metadata1))

  data <- data %>%
    tidyr::separate(Lat_Lon, into = c("latitude", "lat_dir", "longitude", "lon_dir"), sep = " ", remove = FALSE, fill = "right") %>%
    dplyr::mutate(
      latitude = as.numeric(latitude) * dplyr::if_else(lat_dir == "S", -1, 1),
      longitude = as.numeric(longitude) * dplyr::if_else(lon_dir == "W", -1, 1)
    ) %>%
    dplyr::select(-lat_dir, -lon_dir) %>%
    tidyr::separate(Geo_Loc_Name, into = c("Country", "City"), sep = ":", remove = TRUE, fill = "right") %>%
    tidyr::separate(Locus, into = c("Locus", "delete"), sep = "                 ", remove = FALSE, fill = "right") %>%
    tidyr::separate(delete, into = c("Source_pb"), sep = " ", remove = TRUE, fill = "right") %>%
    dplyr::mutate(
      Taxonomy = stringr::str_replace_all(Taxonomy, "\n", ";")
    ) %>%
    tidyr::separate(Taxonomy, into = c("x1", "x2", "Kingdom","Phylum","x5", "Subphylum",
                                       "x7", "x8", "Class", "x10", "Order", "x12","x13",
                                       "Family", "x15", "Genus"), sep = ";", fill = "right", extra = "drop") %>%
    dplyr::select(-c(4,5,8,10,11,13,15,16,18))

  return(data)
}

