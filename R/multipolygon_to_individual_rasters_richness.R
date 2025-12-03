#' Convert Multipolygon to Per-Species Rasters and Richness Map
#'
#' Splits a multipolygon (e.g., species ranges) into individual polygons,
#' rasterizes each as binary presence/absence, and sums to get richness.
#' Uses interactive file selectors.
#'
#' @param resolution Numeric. Raster resolution in degrees (default: 0.01 ≈ 1.1 km).
#' @param field_name Character. Column with species/feature names (default: `"SCI_NAME"`).
#'
#' @return Invisible `NULL`. Side effects:
#'   - Saves one `.tif` per feature in chosen folder.
#'   - Saves `"suma_total.tif"` (richness layer).
#'
#' @importFrom sf st_read st_bbox st_crs st_cast
#' @importFrom terra rast vect rasterize writeRaster app
#' @importFrom svDialogs dlg_open dlg_dir
#'
#' @importFrom dplyr if_else
#'
#' @examples
#' \dontrun{
#' multipolygon_to_individual_rasters_richness(resolution = 0.05)
#' }
#'
#' @export
multipolygon_to_individual_rasters_richness <- function(resolution = 0.01, field_name = "SCI_NAME") {
  # Seleccionar archivo multipolígono
  multipolygon_path <- svDialogs::dlg_open(title = "Selecciona el archivo multipolígono")$res
  if (multipolygon_path == "") stop("No se seleccionó ningún archivo")

  # Leer el multipolígono
  multipolygon_sf <- sf::st_read(multipolygon_path, quiet = TRUE)

  # Seleccionar carpeta de salida
  output_folder <- svDialogs::dlg_dir(title = "Selecciona carpeta para guardar rásters")$res
  if (output_folder == "") stop("No se seleccionó ninguna carpeta")

  # Validar resolución
  if (is.null(resolution)) {
    stop("Debe especificar un valor para 'resolution' en grados")
  }

  # Calcular extensión del multipolígono
  bbox <- sf::st_bbox(multipolygon_sf)

  # Obtener CRS en formato WKT
  crs_val <- sf::st_crs(multipolygon_sf)$wkt

  # Crear ráster plantilla
  template_raster <- terra::rast(
    xmin = bbox["xmin"], xmax = bbox["xmax"],
    ymin = bbox["ymin"], ymax = bbox["ymax"],
    resolution = resolution,
    crs = crs_val
  )

  # Convertir multipolígono a polígonos individuales
  individual_polygons <- sf::st_cast(multipolygon_sf, "POLYGON")

  # Crear carpeta si no existe
  if (!base::dir.exists(output_folder)) {
    base::dir.create(output_folder, recursive = TRUE)
  }

  # Procesar cada polígono y guardar ráster
  for (i in base::seq_along(individual_polygons$geometry)) {
    pol <- individual_polygons[i, ]
    pol_vect <- terra::vect(pol)

    r_temp <- terra::rast(template_raster)
    terra::values(r_temp) <- 0

    r_pol <- terra::rasterize(pol_vect, r_temp, field = 1, background = 0)

    name_val <- pol[[field_name]]
    name_val <- ifelse(base::is.na(name_val), base::paste0("polygon_", i), name_val)
    name_val <- gsub("[^a-zA-Z0-9]", "_", name_val)

    file_out <- base::file.path(output_folder, base::paste0(name_val, ".tif"))
    terra::writeRaster(r_pol, filename = file_out, overwrite = TRUE)
  }

  # Leer todos los rasters generados en la carpeta de salida
  archivos <- base::list.files(output_folder, pattern = "\\.tif$", full.names = TRUE)
  lista_rasters <- base::lapply(archivos, terra::rast)

  # Apilar todos en un solo SpatRaster con varias capas
  stack_rasters <- terra::rast(lista_rasters)

  # Sumar todas las capas
  suma_raster <- terra::app(stack_rasters, fun = base::sum, na.rm = TRUE)

  # Guardar la raster de la suma
  suma_path <- base::file.path(output_folder, "suma_total.tif")
  terra::writeRaster(suma_raster, filename = suma_path, overwrite = TRUE)

  base::cat("✅ Proceso completado. Rasters individuales y suma total guardados en:", output_folder, "\n")
}

