#---------------------------------------------------------------
#libraries
library(shiny)
library(leaflet)
library(shinyjs)
library(shinybusy)
library(tools)
library(readxl)
library(shinyWidgets)
library(tidyverse)
library(shinyalert)
library(shinyscreenshot)
library(cartography)
library(bslib)
library(DT)
library(RColorBrewer)
library(sf)
library(leafpop)

#---------------------------------------------------------------
#USER INTERFACE
#################################### USER INTERFACE #############################
ui <- navbarPage(title = "Landfill Example",
                 windowTitle = "Landfill Example",
                 theme = bs_theme(version = 4, bg = "#040404", fg = "#FFF1F1", 
                                         font_scale = 0.8, bootswatch = "litera"),
                 tabPanel('Mapa principal',
                          add_loading_state(".leaflet", 
                                            text = "Cargando datos...",
                                            svgColor = "steelblue",timeout = 2500),
                          tags$style(
                            '#map { cursor: Auto;}
                                     #controls {background-color: #040404;padding: 0 20px 20px 20px;
                                     cursor: move; opacity: 0.75; zoom: 0.9;transition: opacity 500ms 2s;}
                                      #controls:hover {opacity: 0.95;  transition-delay: 0;}'),
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"),
                          div(class="outer",
                              tags$style(type = "text/css",".outer {position: fixed; top: 41px; left: 0;
                                                right: 0; bottom: 0; overflow: hidden;padding: 0}"),
                              leafletOutput(outputId = 'map',width="100%", height="100%"),
                              absolutePanel(id="controls",
                                            style="z-index:500; overflow-y: scroll",
                                            class = "panel panel-default",
                                            fixed = TRUE,
                                            draggable = F, top = 50, left = "auto", 
                                            right = 20, bottom = 'auto',
                                            width = 400, height = "auto",
                                            ## user's controls
                                            useShinyjs(),
                                          h4('Panel de opciones'),
                                          pickerInput(inputId = 'campo',
                                                      label = 'Campo de visualización',
                                                      choices = c('COSTO_HAB','COSTO_TON','REC_FRECUE'),
                                                      selected = 'COSTO_HAB'),
                                          pickerInput("breaks", #tipo de quiebres 
                                                      label = "Tipo de clasificación de histograma",
                                                      choices = c("Cuantiles", "Quiebres naturales",
                                                                  "Geométricos", "Desviación estándar",
                                                                  'Intervalos iguales'),
                                                      selected = "Quiebres naturales"),
                                          numericInput('nBreaks','N°de clases',value = 5),
                                          pickerInput("paleta", #tipo de quiebres 
                                                      label = "Paleta de colores",
                                                      choices = c("YlOrRd" = 'YlOrRd',
                                                                  "Spectral" = 'Spectral',
                                                                  "YlGnBu" = 'YlGnBu',
                                                                  "RdBu" = 'RdBu'),
                                                      selected = "YlOrRd"),
                                          checkboxInput('reverse',label = 'Invertir paleta',
                                                        FALSE)
                              ),
                              fixedPanel(top = 50,bottom = 'auto',left = 45,width = 375,
                                         actionButton('print','Exportar mapa')
                                         )),
                          useShinyalert()),
                 tabPanel('Información'#,
                        #includeHTML('infoTab.html')
                 )
                 
)

#------------------------------------------------------------------------------------
#server

server <- function(input, output, session) {
  #load vectors
  load('data/vectores.RData')
  
   ## render main map
  output$map <- renderLeaflet({
    caja <- shp %>% st_bbox() %>% as.numeric()
    leaflet() %>% addProviderTiles("Esri.WorldImagery",group = 'ESRI satellite') %>% 
      fitBounds(lng1 = caja[1],lat1 = caja[2],lng2 = caja[3],lat2 = caja[4]) %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
      addLayersControl(overlayGroups = c('Indicadores de gestión'),position = 'topleft') %>% 
      addLayersControl(
        baseGroups = c('ESRI',"OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = 'Indicadores de gestión', position = 'topleft',
        options = layersControlOptions(collapsed = F)
      )
  })
  
  #tabla popup
    tabla_pop <- shp %>% dplyr::select(c('REGION','COMUNA','SITIO_ACTI','REC_SISTEM','COSTO_HAB','COSTO_TON',
    'CATEGORIA1','CATEGORI_1')) %>% as.data.frame() %>% dplyr::select(-c('geometry'))
    names(tabla_pop) <- c('Región','Comuna','Nombre sitio','Sistema recolección',
                          'Costo/hab','Costo/ton','Nivel costos','Nivel eficiencia')
  
  #FILTER CAMPO
  shp_filter <- eventReactive(input$campo,{
    capa <- shp %>% dplyr::select(input$campo) %>% st_transform(4326)
    names(capa)<- c('campo','geometry')
    capa
  })
#------------------------------------------------------------------------------------------
  #reactive maps
 #first load map
  observeEvent(c(input$campo,input$breaks,input$paleta,input$reverse,input$nBreaks), {
    proxyMap <- leafletProxy('map') %>% clearControls() %>% 
      addLayersControl(
        baseGroups = c('ESRI',"OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = 'Indicadores de gestión', position = 'topleft',
        options = layersControlOptions(collapsed = F)
      )
    
    # input parameters for plot choroplet
    quiebres <- switch(input$breaks, 
                       "Cuantiles" = 'quantile',
                       "Quiebres naturales" = 'fisher-jenks',
                       "Geométricos" = 'geom',
                       "Desviación estándar" = 'sd',
                       "Intervalos iguales"='equal')
    paleta <- switch(input$paleta, 
                     "YlOrRd" = 'YlOrRd',
                     "Spectral" = 'Spectral',
                     "YlGnBu" = 'YlGnBu',
                     "RdBu" = 'RdBu')
    
    nClases <- input$nBreaks
    revertir <- input$reverse
    
    if(revertir ==TRUE){
      colores <- rev(brewer.pal(n = nClases, name = paleta))}
    if(revertir ==FALSE){
      colores <- brewer.pal(n = nClases, name = paleta)}
    
    tabla <- shp_filter() %>% as.data.frame() 
    dominio <- tabla[, 'campo'] %>% unique
    breaks.plot <- getBreaks(v = dominio, nclass = nClases,method = quiebres)
    
    #creando paleta de colores dinámica
    pal <- colorBin(palette = paleta,domain = dominio,bins = breaks.plot,reverse = revertir)
    
    shp <- shp_filter()
    
    caja <- shp %>% st_bbox() %>% as.numeric()
    proxyMap <- proxyMap %>% 
      addCircleMarkers(data = shp,group = 'Indicadores de gestión',stroke = FALSE,color = ~pal(campo),
                       fillOpacity = 1,clusterOptions = markerClusterOptions(),radius = 10,
                       popup = popupTable(tabla_pop,row.numbers =F)) %>% 
      leaflet::addLegend("bottomleft", pal = pal, values = dominio,
                                                             title = input$columna,
                                                             opacity = 1,group = 'Leyenda')
    proxyMap
    
  })

  #----------------------------------------------------------------------------------------------
  #screenshot map
    observeEvent(input$print,{
      screenshot(filename = 'myMap',id =  'map')
    })

}

#----------------------------------------------------------------------------------
#compile
shinyApp(ui, server)
