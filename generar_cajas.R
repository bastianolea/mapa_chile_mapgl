# genera cajas (bounding boxes) para regiones y comunas
# usando {chilemapas} para geometrías
# y {territorial} para nombres de comunas y regiones.
# guarda los resultados como archivos RDS para usar en los
# selectores de zoom del mapa interactivo.

library(chilemapas)
library(territorial)
library(dplyr)
library(sf)
library(purrr)

# cargar geometrías de {chilemapas}
regiones_geo <- chilemapas::generar_regiones()
comunas_geo <- chilemapas::mapa_comunas

# limpiar regiones
regiones_geo_2 <- regiones_geo |>
  mutate(codigo_region = as.numeric(codigo_region)) |>
  mutate(nombre_region = territorial::as_nombre_region(codigo_region)) |>
  territorial::ordenar_regiones()

# limpiar comunas
comunas_geo_2 <- comunas_geo |>
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |>
  territorial::contextualizar(codigo_comuna)

# revisar mapa de regiones
# library(ggplot2)
#
# regiones_geo_2 |>
#   st_crop(c(xmin = -75.7, ymin = -55.9, xmax = -66.4, ymax = -17.5)) |>
#   ggplot() +
#   geom_sf()

# recortar mapa de regiones al chile continental
regiones_geo_3 <- regiones_geo_2 |>
  st_crop(
    c(xmin = -75.7, ymin = -55.9, xmax = -66.4, ymax = -17.5)
  )

# país ----
# calcular caja del territorio completo
pais_bbox_valores <- st_bbox(regiones_geo_3)

pais_bbox <- tibble(
  nombre_region = "País",
  xmin = pais_bbox_valores[["xmin"]],
  ymin = pais_bbox_valores[["ymin"]],
  xmax = pais_bbox_valores[["xmax"]],
  ymax = pais_bbox_valores[["ymax"]]
)

# regiones ----
# calcular cajas por región
region_bbox <- regiones_geo_3 |>
  mutate(
    bbox = map(geometry, st_bbox),
    xmin = map_dbl(bbox, "xmin"),
    ymin = map_dbl(bbox, "ymin"),
    xmax = map_dbl(bbox, "xmax"),
    ymax = map_dbl(bbox, "ymax")
  ) |>
  select(nombre_region, xmin, ymin, xmax, ymax) |>
  st_drop_geometry() |>
  # agregar caja del país
  bind_rows(pais_bbox)

saveRDS(region_bbox, "datos/cajas_regiones.rds")
message("Guardado datos/cajas_regiones.rds con ", nrow(region_bbox), " filas.")

# comunas ----
# calcular cajas por comuna
comunas_bbox <- comunas_geo_2 |>
  mutate(
    bbox = map(geometry, st_bbox),
    xmin = map_dbl(bbox, "xmin"),
    ymin = map_dbl(bbox, "ymin"),
    xmax = map_dbl(bbox, "xmax"),
    ymax = map_dbl(bbox, "ymax")
  ) |>
  select(nombre_region, nombre_comuna, xmin, ymin, xmax, ymax) |>
  st_drop_geometry()

saveRDS(comunas_bbox, "datos/cajas_comunas.rds")
message("Guardado datos/cajas_comunas.rds con ", nrow(comunas_bbox), " filas.")
