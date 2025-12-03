#' Rasterize Point Shapefile to GeoTIFF
#'
#' Converts point data (sf or shapefile path) to a raster where each cell value
#' corresponds to a field (e.g., species ID). Uses interactive save dialog.
#'
#' @param input_shp `sf` object or path to point shapefile.
#' @param output_raster Optional path to save `.tif`. If `NULL`, opens save dialog.
#' @param resolution Numeric. Raster cell size in degrees (default ≈ 10 km at equator).
#' @param field_name Character. Column name to rasterize (if missing/invalid, creates `id`).
#'
#' @return Invisible `NULL`. Side effect: writes GeoTIFF.
#'
#' @importFrom sf st_read
#' @importFrom terra vect rast ext rasterize writeRaster
#' @importFrom dplyr mutate
#' @importFrom svDialogs dlg_save
#'
#' @examples
#' \dontrun{
#' shp <- st_read("data.shp")
#' points_to_raster(shp, resolution = 0.1)
#' }
#'
#' @export
# Paquetes requeridos: sf, terra, svDialogs

points_to_raster <- function(input_shp, output_raster = NULL,
                             resolution = 0.08983111749910, field_name = NULL) {

  # Detectar si input_shp es objeto sf o ruta a shapefile [web:1]
  if (inherits(input_shp, "sf")) {
    shp <- input_shp
  } else {
    shp <- sf::st_read(input_shp)  # sf::st_read [web:4]
  }

  # Si field_name no está definido o no existe en el shapefile, crear campo id
  if (is.null(field_name) || !(field_name %in% names(shp))) {
    shp$id <- 1:nrow(shp)
    field_name <- "id"
  }

  # Convertir a SpatVector (terra)
  vect_obj <- terra::vect(shp)  # terra::vect [web:3]

  # Obtener extensión del vector
  e <- terra::ext(vect_obj)  # terra::ext [web:3]

  # Crear raster con la extensión y resolución dadas
  r_template <- terra::rast(e, resolution = resolution)  # terra::rast [web:3]

  # Rasterizar usando el campo elegido
  rasterized <- terra::rasterize(vect_obj, r_template, field = field_name, background = NA)  # terra::rasterize [web:3]

  # Si no hay ruta para guardar, abrir diálogo para seleccionar ubicación
  if (is.null(output_raster)) {
    dlg_save <- svDialogs::dlg_save(default = "raster_output.tiff",
                                    filters = list("GeoTIFF files" = c("tif", "tiff"), "All files" = "*"))
    output_raster <- dlg_save$res
    if (is.null(output_raster) || output_raster == "") {
      stop("Operación cancelada: no se seleccionó destino para guardar")
    }
    # Añadir extensión .tif si no existe
    if (!grepl("\\.tif$", output_raster, ignore.case = TRUE)) {
      output_raster <- paste0(output_raster, ".tif")
    }
  }

  # Guardar raster en archivo
  terra::writeRaster(rasterized, filename = output_raster, overwrite = TRUE)  # terra::writeRaster [web:3]

  message("✅ Gaster guardado en: ", output_raster)
}

