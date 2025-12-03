#' Export Metadata as Point Shapefile (WGS84)
#'
#' Converts data frame with latitude/longitude to an ESRI Shapefile.
#' Uses interactive dialog for file selection.
#'
#' @param data Data frame with geographic coordinates.
#' @param lon_col Character. Column name for longitude (default: `"longitude"`).
#' @param lat_col Character. Column name for latitude (default: `"latitude"`).
#'
#' @return Invisible `NULL`. Side effect: writes shapefile (`.shp`, `.shx`, `.dbf`, etc.).
#'
#' @importFrom sf st_as_sf st_write
#' @importFrom dplyr filter
#' @importFrom svDialogs dlg_save
#'
#' @examples
#' \dontrun{
#' meta <- get_metadata(seqs)
#' save_shapefile(meta)
#' }
#'
#' @export
save_shapefile <- function(data, lon_col = "longitude", lat_col = "latitude") {

  if (!(lon_col %in% base::colnames(data)) || !(lat_col %in% base::colnames(data))) {
    base::stop("El dataframe debe contener las columnas especificadas para longitud y latitud")
  }

  # Diálogo para seleccionar archivo shapefile para guardar (extensión .shp)
  dlg_res <- svDialogs::dlg_save(default = "datos.shp",
                                 filters = list("Shapefile" = c("shp"), "All files" = "*"))

  file_path <- dlg_res$res
  if (file_path == "" || base::is.null(file_path)) {
    base::stop("Guardado cancelado por el usuario")
  }

  # Convertir a objeto sf de puntos
  sf_obj <- data %>%
    dplyr::filter(!base::is.na(.data[[lon_col]]), !base::is.na(.data[[lat_col]])) %>%
    sf::st_as_sf(coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)

  # Guardar shapefile; esto genera varios archivos con extensiónes .shp, .shx, .dbf etc,
  # pero solo se indica la ruta de uno (normalmente .shp)
  sf::st_write(sf_obj, dsn = file_path, driver = "ESRI Shapefile", delete_dsn = TRUE)

  message("✅ GeShapefile guardado en: ", file_path)
}


