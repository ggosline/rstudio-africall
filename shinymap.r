
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyTree)
library(sf)
#library(sp)
library(pool)
library(RPostgres)
library(DBI)
library(dplyr)
library(DT)
library(data.table)

library(mapview)

# specimenlist <- readRDS("ginthreat.rds")
# specimenlist <- ginspecs

#browser()

GuineaTIPAs <- readRDS("guineatipas.rds")

pool <- dbConnect(drv=RPostgres::Postgres(), dbname="rainbio", port=1433, user="ggosline", password="Uvariops1s", host="kewwta.chh53kepvixq.eu-west-2.rds.amazonaws.com")

sql <- "Select * from africaall where africaall.\"countryCode\" = ?country;"
query <- sqlInterpolate(pool, sql, country='GIN')
ginspecs <- st_read_db(pool, query=query)

ginspecs$fIUCN <- factor(ginspecs$IUCN)
ginthreat <- ginspecs[ginspecs$IUCN %in% c("CR", "EN", "VU"),]

dbDisconnect(pool)

# speciesnames <- data.table(ginspecs[,c("family", "genus","species","IUCN"),drop=TRUE]) %>% group_by(family, genus, species) %>% count()
# specieslist <- split(speciesnames, speciesnames$genus)
# 
# spll <- mapply(function(z){mapply(function(x,y) { y }, z[[2]], z[[3]], SIMPLIFY = FALSE,USE.NAMES = TRUE)},specieslist,USE.NAMES = T)

speciesnames <- data.table(ginspecs,drop=TRUE)[,.(IUCN=max(IUCN),.N),by=list(family,genus,species)]
specieslist <- split(speciesnames,by=c("family","genus"),sorted=TRUE,flatten=FALSE)

spll <- mapply(function(z1){mapply(function(z){mapply(function(y) { y }, z$species,SIMPLIFY = FALSE)},z1,SIMPLIFY=FALSE)},specieslist,USE.NAMES = TRUE, SIMPLIFY=FALSE)

specimenpal <- colorFactor("Reds", levels(ginspecs$fIUCN))

ui <- navbarPage("Threatened Species", id="nav",

    tabPanel("Map",
     div(class="outer", 
         
      tags$head(tags$style(type = "text/css", 
            "div.outer {position: fixed; top: 56px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}
             #controls {background-color: white;
               padding: 0 20px 20px 20px;
               cursor: move;
               opacity: 0.65;
               zoom: 0.9;
               transition: opacity 500ms 1s;}
             #controls:hover {
               opacity: 0.95;
               transition-delay: 0;}" )),  
         
      leafletOutput("mymap", height="100%", width="100%"),
      
    # sidebarPanel(selectInput("selectedspecies", "Species:",specieslist), multiple=TRUE),
      absolutePanel(id="controls", class = "panel panel-default", 
                    fixed=TRUE, draggable=TRUE, top = 60, left = "auto", right = 20, bottom="auto",
                  width = 330, height="auto", 
                  
        checkboxInput("threatenedonly", label="Threatened Only", value=TRUE),
        checkboxInput("selectspecs", label="Select In Area", value=FALSE),
        checkboxInput("clusterpts", label="Cluster Points", value=FALSE),
        checkboxInput("showallspecs", label="Select taxa", value=FALSE),
        conditionalPanel(condition = "input.showallspecs", style = "height:600px; overflow-y:scroll;", height="500px", 
          shinyTree('speciesselect',checkbox = TRUE))
      ))
  ),
            
    tabPanel("Species List",
            downloadButton("downloadspecieslist", label="Download"),
            DT::dataTableOutput("specieslist"))
              
    )
  


server <- function(input, output, session) {
  
  specimenlist <- reactiveVal(ginthreat)
  
  speciesselection <- reactiveVal()
  
  output$speciesselect <- renderTree(spll)
  
  specimens <- reactive({
              if (!input$showallspecs | length(get_selected(input$speciesselect,format="names")) == 0) specimenlist()
              else specimenlist()[specimenlist()$species %in% get_selected(input$speciesselect,format="names"),]
            })
  
  clustering <- reactive({
            if (!input$clusterpts) NULL
            else markerClusterOptions()
  })                   
  
  observe( {

    leafletProxy("mymap") %>% clearMarkerClusters() %>% clearMarkers() %>%
                              addCircleMarkers(data = specimens(), label = ~species,
                                  radius = 3,
                                  color = ~specimenpal(fIUCN),
                                  clusterOptions = clustering(),
                                  popup = ~sprintf("<strong>%s</strong> %s<br>%s %s<br>%s<br>%s<br>%s",
                                                   species,IUCN,recordedby,recordnumber,eventdate,catalogNumber, rowid
                                  )
                                  )
  })
  
  observeEvent(input$threatenedonly,  { 
    if (input$threatenedonly)  specimenlist(ginthreat) 
    else   specimenlist(ginspecs)
               })
  
  observeEvent(input$selectspecs,  {
    if (input$selectspecs) {
      leafletProxy("mymap") %>% 
      addDrawToolbar(
        targetGroup='select',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        singleFeature = TRUE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()) )}
    else {leafletProxy("mymap") %>% removeDrawToolbar(clearFeatures = TRUE )}
  })  
    
  
  output$mymap <- renderLeaflet({
    
    leaflet(options=leafletOptions(CANVAS=TRUE)) %>% 
      # Base groups
      addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo") %>%
      addProviderTiles(providers$OpenTopoMap, group = "OpenTopo") %>%
      addScaleBar('bottomleft') %>%
      addLayersControl(
        baseGroups = c("OSM", "Imagery", "NatGeo", "OpenTopo"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      addMouseCoordinates(style="basic")  %>%
      addPolygons(data=GuineaTIPAs) # %>%
      
  })
  
  output$downloadspecieslist <- downloadHandler(
    filename = function() {
         paste('selectedspecies', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(speciesselection(), con)
       })
  
  output$specieslist <- DT::renderDataTable({

    #use the draw_stop event to detect when users finished drawing
    req(input$mymap_draw_stop)
    # print(input$mymap_draw_new_feature)
    feature_type <- input$mymap_draw_new_feature$properties$feature_type

    if(feature_type %in% c("rectangle","polygon")) {

      #get the coordinates of the polygon
      polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]

      #transform them to an sf Polygon
      drawn_polygon <- st_sf(1, st_sfc(st_polygon(list(matrix(unlist(polygon_coordinates),ncol=2,byrow=TRUE)))))
      st_crs(drawn_polygon) <- st_crs(ginspecs)

      #use over from the sp package to identify selected specimenlist
      selected_features <- st_intersects(specimens() , drawn_polygon, sparse=FALSE)

      #print the name of the specimenlist
      speciesselection(data.table(specimens())[selected_features[,1]][order(family, species),.(.N),by=.(family, species, IUCN)])
      datatable( speciesselection(),
                options=list(pageLength=25))
      
    # } else if(feature_type=="circle") {
    #   #get the coordinates of the center of the cirle
    #   center_coords <- matrix(c(input$mymap_draw_new_feature$geometry$coordinates[[1]],input$mymap_draw_new_feature$geometry$coordinates[[2]]),ncol=2)
    # 
    #   #calculate the distance of the specimenlist to the center
    #   dist_to_center <- spDistsN1(specimenlist,longlat=TRUE)
    # 
    #   #select the specimenlist that are closer to the center than the radius of the circle
    #   as.character(specimenlist[dist_to_center < input$mymap_draw_new_feature$properties$radius/1000,"AccentCity"])
    }
    
    
  })
  
}

shinyApp(ui, server)