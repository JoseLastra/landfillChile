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

#---------------------------------------------------------------
#USER INTERFACE
#################################### USER INTERFACE #############################
ui <- navbarPage(title = "GEO1015",
                 windowTitle = "GEO1015",
                 theme = bs_theme(version = 4, bg = "#040404", fg = "#FFF1F1", 
                                         font_scale = 0.8, bootswatch = "litera"),
                 tabPanel('Mapa principal',
                          add_loading_state(".leaflet", 
                                            text = "Cargando datos...",
                                            svgColor = "steelblue",timeout = 5500),
                          add_loading_state(".plotly", text = "Refrescando el mapa...",
                                            svgColor = "steelblue",timeout = 3500),
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
                                            right = 20, bottom = 5,
                                            width = 400, height = "auto",
                                            ## user's controls
                                            useShinyjs(),
                                            h4('Seleccione su DPA'),
                                            uiOutput('reg_list'),
                                            uiOutput('comu_list'),
                                            #subir archivo para join con comunas,
                                        
                                            awesomeRadio(
                                              inputId = "DPA",
                                              label = "Seleccione UA", 
                                              choices = c("Comunas", "Manzanas censales"),
                                              selected = "Comunas"
                                            ),
                                            fileInput('target_upload', 
                                                      'Suba su tabla con información de manzanas',
                                                      accept = c('text/csv',
                                                                 'text/comma-separated-values',
                                                                 '.csv',
                                                                 '.xls',
                                                                 '.xlsx')),
                                            #selector de separador
                                            radioButtons("separator","Separador: ",
                                                         choices = c(";",",",":"),
                                                         selected=",",
                                                         inline=TRUE),
                                            uiOutput('datos'),
                                            pickerInput("breaks", #tipo de quiebres 
                                                        label = "Tipo de clasificación de histograma",
                                                        choices = c("Cuantiles", "Quiebres naturales",
                                                                    "Geométricos", "Desviación estándar",
                                                                    'Intervalos iguales'),
                                                        selected = "Quiebres naturales"),
                                            numericInput('nBreaks','N°de clases',value = 5),
                                            pickerInput("paleta", #tipo de quiebres 
                                                        label = "Acá deberían elegir paleta",
                                                        choices = c("YlOrRd" = 'YlOrRd',
                                                                    "Spectral" = 'Spectral',
                                                                    "YlGnBu" = 'YlGnBu',
                                                                    "RdBu" = 'RdBu'),
                                                        selected = "YlOrRd"),
                                            checkboxInput('reverse',label = 'Inventir paleta',
                                                          FALSE),
                                            actionButton('plot','Visualizar coropleta')
                                            
                              ),
                              fixedPanel(top = 50,bottom = 'auto',left = 45,width = 375,
                                         actionButton('print','Exportar mapa')
                                         )),
                          useShinyalert()),
                 tabPanel('Información',
                        includeHTML('infoTab.html')
                 )
                 
)

#------------------------------------------------------------------------------------
#server

server <- function(input, output, session) {
  
  vectores <- load('data/vectorCompress.RData')
  comunas$CUT_COM <- comunas$CUT_COM %>% as.numeric()
  
   ## render main map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldImagery",group = 'ESRI satellite') %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Stamen Terrain") %>% 
      addLayersControl(overlayGroups = c('DPA'),position = 'topleft') %>% 
      addLayersControl(
        baseGroups = c('ESRI',"OSM (default)", "CartoDB Dark", "Stamen Terrain"),
        overlayGroups = 'DPA', position = 'topleft',
        options = layersControlOptions(collapsed = F)
      )
  })
  
  #lista de regiones
  regiones_list <- comunas$REGION %>% unique()
  
  
  #render UI regiones
  output$reg_list <- renderUI({
    pickerInput(
      inputId = "regiones",
      label = "Seleccione Región", 
      choices = regiones_list, selected = "Valparaíso"
    )
  })
  
  #render comunas
  output$comu_list <- renderUI({
    req(input$regiones)
    #lista de comunas
    comunas_list <-  apple %>% filter(REGION==input$regiones) %>% as.data.frame() %>% 
      dplyr::select('COMUNA') %>% as_vector() %>% unique() 
    
    pickerInput(
      inputId = "com",
      label = "Seleccione comuna", 
      choices = comunas_list, selected = comunas_list
    )
  })
    

  ##target upload
  datos.df <- reactive({
    req(input$target_upload)
      inFile <- input$target_upload
      if (is.null(inFile)){
        return(NULL)}
      
      if(file_ext(x = inFile$datapath)=='csv'){
      df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)# %>% select(-c('X'))
      }
      if(file_ext(x = inFile$datapath)=='xls'){
        df <- read_xls(inFile$datapath,sheet = 1)
      }
      if(file_ext(x = inFile$datapath)=='xlsx'){
        df <- read_xlsx(inFile$datapath,sheet = 1)
      }
      
    return(df)
  })
  
  
  ##enable and disable
  observeEvent(input$DPA, {
    if(input$DPA == 'Comunas'){
      shinyjs::show("target_upload")
      shinyjs::show("separator")
      shinyjs::hide('comu_list')
    }
    if(input$DPA == 'Manzanas censales'){
      shinyjs::show("target_upload")
      shinyjs::show("separator")
      shinyjs::show('comu_list')
    }
    
  })

  ## filter basic shapefile
  #comunas
  shp <- eventReactive(c(input$regiones, input$com,input$DPA),{
    req(input$regiones)
    if(input$DPA == 'Comunas'){
    capa <- comunas %>% filter(REGION == input$regiones)
    }
    if(input$DPA == 'Manzanas censales'){
      capa <- apple %>% filter(REGION == input$regiones)# %>% filter(COMUNA == input$com)
    }
    capa %>% st_transform(4326)
  })
  
  #render datos
  output$datos <- renderUI({
    req(!is.null(datos.df()))
    
    if(input$DPA == 'Manzanas censales'){
    shp <- shp()
    df1 <- datos.df()
    tabla_pob <- merge(shp, df1, by="MANZENT")
    data.num <- select_if(tabla_pob, is.numeric) %>% as.data.frame() %>% dplyr::select(-c('geometry'))
    
    }
    if(input$DPA == 'Comunas'){

      df1 <- datos.df()
      shp1 <- shp()
      shp1$CUT_COM <- shp1$CUT_COM %>% as.numeric()
      tabla_pob <- merge(shp1, df1, by.x = "CUT_COM", by.y = "COD_COM")
      data.num <- select_if(tabla_pob, is.numeric) %>% as.data.frame() %>% 
        dplyr::select(-c('geometry'))
    
    }
    
    pickerInput(
      "columna",
      "Seleccione un campo",
      colnames(data.num)
    )
  })
#------------------------------------------------------------------------------------------
  #reactive maps
 #first load map
  observeEvent(input$regiones, {
    proxyMap <- leafletProxy('map') %>% clearControls() %>% 
      addLayersControl(
        baseGroups = c('ESRI',"OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = 'DPA', position = 'topleft',
        options = layersControlOptions(collapsed = F)
      )
    
    caja <- shp() %>% st_bbox() %>% as.numeric()
    proxyMap <- proxyMap %>% fitBounds(lng1 = caja[1],lat1 = caja[2],lng2 = caja[3],lat2 = caja[4])
    proxyMap
    
  })

  #----------------------------------------------------------------------------------------------
  #comunas reactive base
  observeEvent(c(input$regiones,input$DPA,input$com),{
    
  proxyMap <- leafletProxy('map') %>% clearControls() %>% clearShapes() %>% 
    addLayersControl(
      baseGroups = c('ESRI',"OSM (default)", "CartoDB Dark", "Stamen Terrain"),
      overlayGroups = 'DPA', position = 'topleft',
      options = layersControlOptions(collapsed = F)
    )
  
  if(input$DPA == 'Comunas'){
    caja <- shp() %>% st_bbox() %>% as.numeric()
    proxyMap <- proxyMap %>% clearShapes() %>% fitBounds(lng1 = caja[1],lat1 = caja[2],lng2 = caja[3],lat2 = caja[4]) %>% 
      addPolygons(data = shp(),group = 'DPA', fillColor = 'white', fillOpacity = 0.1,
                  stroke = 0.1,color = 'red',weight = 1, smoothFactor = 0.2) 
  }
  if(input$DPA == 'Manzanas censales'){
    req(input$com)
    shp <- shp()
    shp <- shp %>% filter(COMUNA == input$com)
    caja <- shp %>% st_bbox() %>% as.numeric()
    proxyMap <- proxyMap %>% clearShapes() %>% fitBounds(lng1 = caja[1],lat1 = caja[2],lng2 = caja[3],lat2 = caja[4]) %>% 
      addPolygons(data = shp,group = 'DPA', fillColor = 'white', fillOpacity = 0.1,
                  stroke = 0.1,color = 'red',weight = 1, smoothFactor = 0.2) 
  }
  proxyMap
  })
  
  # reactive table join
  shp_join <- eventReactive(c(input$plot,input$target_upload,input$DPA,input$columna),{
    req(!is.null(datos.df()))
    
    if(input$DPA == 'Manzanas censales'){
      shp <- shp()
      df1 <- datos.df()
      tabla_pob <- merge(shp, df1, by="MANZENT")
  
    }
    if(input$DPA == 'Comunas'){
      df1 <- datos.df()
      shp1 <- shp()
      shp1$CUT_COM <- shp1$CUT_COM %>% as.numeric()
      tabla_pob <- merge(shp1, df1, by.x = "CUT_COM", by.y = "COD_COM")
    }
    tabla_pob <- tabla_pob %>% dplyr::select(input$columna)
    names(tabla_pob) <-c('campo','geometry')
    tabla_pob
  })
  
  #reactive coropleth map object
  map_export <- eventReactive(c(input$plot,input$columna,input$breaks,input$paleta,input$reverse,input$nBreaks),{
    req(input$plot)
    proxyMap <- leafletProxy('map') %>% clearControls() %>% clearShapes() %>% 
      addLayersControl(
        baseGroups = c('ESRI',"OSM (default)", "CartoDB Dark", "Stamen Terrain"),
        overlayGroups = 'DPA', position = 'topleft',
        options = layersControlOptions(collapsed = F)
      ) %>% addScaleBar(position = 'bottomleft',options = scaleBarOptions(
        maxWidth = 200,
        metric = TRUE,
        imperial = F,
        updateWhenIdle = TRUE
      )) 
    
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
    
    tabla <- shp_join() %>% as.data.frame() 
    dominio <- tabla[, 'campo'] %>% unique
    breaks.plot <- getBreaks(v = dominio, nclass = nClases,method = quiebres)
    
    #creando paleta de colores dinámica
    pal <- colorBin(palette = paleta,domain = dominio,bins = breaks.plot,reverse = revertir)
    
    caja <- shp_join() %>% st_bbox() %>% as.numeric()
    proxyMap <- proxyMap %>% clearShapes() %>% clearControls() %>%
      fitBounds(lng1 = caja[1],lat1 = caja[2],lng2 = caja[3],lat2 = caja[4]) %>%   
      addPolygons(data = shp_join(),group = 'DPA', fillColor = ~pal(campo), fillOpacity = 0.7,
                  stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2) %>%
      leaflet::addLegend("bottomleft", pal = pal, values = dominio,
                title = input$columna,
                opacity = 0.7,group = 'Leyenda')
    
    proxyMap
    
  })
  
  #render reactive map
  observeEvent(c(input$plot),{
    map_export()
  })
  
  #screenshot map
    observeEvent(input$print,{
      screenshot(filename = 'myMap',id =  'map')
    })

}

#----------------------------------------------------------------------------------
#compile
shinyApp(ui, server)
