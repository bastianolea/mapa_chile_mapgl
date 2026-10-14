library(shiny)
library(bslib)
library(shinyjs)
library(dplyr)
library(mapgl) # https://walker-data.com/mapgl/
library(territorial) # https://bastianolea.github.io/territorial/
library(chilemapas) # https://pacha.dev/chilemapas/
library(sf)

# convertir mapa chilemapas::mapa_comunas a sf
mapa_comunas <- sf::st_as_sf(chilemapas::mapa_comunas)

# cargar cajas (bounding boxes) precalculadas para zoom
caja_region <- readRDS("datos/cajas_regiones.rds")
caja_comuna <- readRDS("datos/cajas_comunas.rds")

# interfaz de la app
ui <- page_fillable(
  padding = 0, # app sin bordes
  useShinyjs(), # para ocultar/mostrar elementos

  # css para los selectores dentro de la tarjeta
  tags$style(
    ".card:has(#region) {
      overflow: visible !important;
    }
    .card:has(#region) .card-body {
      overflow: visible !important;
    }"
  ),

  # mapa
  mapgl::maplibreOutput("mapa"),

  # panel
  card(
    style = "position: absolute; top: 0; left: 0;
             width: 280px; height: auto;
             margin: 18px;",
    full_screen = FALSE,

    h2("Mapa"),

    markdown(
      "Plantilla de app Shiny hecha en R con un mapa `{mapgl}` y un selector de regiones y comunas de Chile"
    ),

    markdown(
      "Revisa más sobre el paquete `{mapgl}` [en este tutorial](https://bastianolea.rbind.io/blog/mapas_mapgl/) o en su [documentación oficial](https://walker-data.com/mapgl/)."
    ),

    # selector de regiones
    selectInput(
      inputId = "region",
      label = "Región:",
      choices = c(
        "País",
        territorial::regiones()
      )
    ),

    # selector de comunas
    selectInput(
      inputId = "comuna",
      label = "Comuna:",
      choices = NULL
    ) |>
      hidden(),
  ),
)

# lógica de la app
server <- function(input, output, session) {
  # mapa base ----
  output$mapa <- renderMaplibre({
    maplibre(
      center = c(-71.7, -35),
      zoom = 4,
      minZoom = 4,
      maxZoom = 14
      # attributionControl = FALSE
    ) |>
      # capa con polígonos
      add_fill_layer(
        id = "comunas",
        source = mapa_comunas, # mapa como objeto sf
        fill_color = "#965DDD",
        fill_opacity = 0.2
      )
  })

  # selectores ----

  # mostrar/ocultar selector de comunas según región
  observe({
    if (input$region == "País") {
      hide("comuna")
    } else {
      show("comuna")
    }
  })

  # actualizar opciones de comunas cuando cambia la región
  observeEvent(input$region, {
    updateSelectInput(
      session,
      "comuna",
      choices = c(
        "Todas",
        caja_comuna |>
          filter(nombre_region == input$region) |>
          distinct(nombre_comuna) |>
          arrange(nombre_comuna) |>
          pull(nombre_comuna)
      )
    )
  })

  # zoom ----

  # caja a usar según el territorio elegido: país, región o comuna
  caja_mapa <- reactive({
    req(input$region)

    # dependiendo de los selectores, se filtra la caja apropiada

    # ver país completo
    if (input$region == "País") {
      caja_region |>
        filter(nombre_region == "País")
    } else if (is.null(input$comuna) || input$comuna == "Todas") {
      # ver región (sin comuna seleccionada o con "Todas")
      caja_region |>
        filter(nombre_region == input$region)
    } else {
      # ver comuna específica
      caja_comuna |>
        filter(nombre_region == input$region, nombre_comuna == input$comuna)
    }
  })

  # hacer zoom cada vez que cambie la caja seleccionada
  observe({
    # caja dependiendo de selectores
    caja <- caja_mapa()

    maplibre_proxy("mapa") |>
      fit_bounds(
        c(caja$xmin, caja$ymin, caja$xmax, caja$ymax),
        animate = TRUE
      )
  })
}

shinyApp(ui = ui, server = server)
