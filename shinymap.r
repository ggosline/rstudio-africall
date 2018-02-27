
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyTree)
library(sf)
#library(sp)
library(RPostgres)
library(dplyr)
library(DT)
library(data.table)
#library(mapedit)

# specimenlist <- readRDS("ginthreat.rds")
# specimenlist <- ginspecs

GuineaTIPAs <- readRDS("guineatipas.rds")

specimenlist <- ginspecs
specimenlist$fIUCN <- factor(specimenlist$IUCN)

speciesnames <- specimenlist %>% group_by(genus, species) %>% count()
specieslist <- split(speciesnames, speciesnames$genus)

spll <- mapply(function(z){mapply(function(x,y) { y }, z[[2]], z[[3]], SIMPLIFY = FALSE,USE.NAMES = TRUE)},specieslist,USE.NAMES = T)
specimenpal <- colorFactor("Reds",specimenlist$fIUCN)

ui <- navbarPage("Threatened Species", id="nav",

    tabPanel("Map",
     div(class="outer", 
         
      tags$head(tags$style(type = "text/css", 
            "div.outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}
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
              dataTableOutput("specieslist"))
              
    )
  


server <- function(input, output, session) {
  
  output$speciesselect <- renderTree(spll)
  
  specimens <- reactive({
              if (!input$showallspecs | length(get_selected(input$speciesselect,format="names")) == 0) specimenlist
              else specimenlist[specimenlist$species %in% get_selected(input$speciesselect,format="names"),]
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
    if (input$threatenedonly) {specimenlist <- ginspecs[ginspecs$IUCN %in% c("CR","EN","VU"),]}
    else {specimenlist <- ginspecs}
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
    
    leaflet(specimenlist) %>% 
      # Base groups
      addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo") %>%
      addProviderTiles(providers$OpenTopoMap, group = "OpenTopo") %>%
      
      addLayersControl(
        baseGroups = c("OSM", "Imagery", "NatGeo", "OpenTopo"),
        options = layersControlOptions(collapsed = TRUE)) %>%
        
      addPolygons(data=GuineaTIPAs) # %>%
      
  })
  
  output$specieslist <- renderDataTable({

    #use the draw_stop event to detect when users finished drawing
    req(input$mymap_draw_stop)
    # print(input$mymap_draw_new_feature)
    feature_type <- input$mymap_draw_new_feature$properties$feature_type

    if(feature_type %in% c("rectangle","polygon")) {

      #get the coordinates of the polygon
      polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]

      #transform them to an sf Polygon
      drawn_polygon <- st_sf(1, st_sfc(st_polygon(list(matrix(unlist(polygon_coordinates),ncol=2,byrow=TRUE)))))
      st_crs(drawn_polygon) <- st_crs(specimenlist)

      #use over from the sp package to identify selected specimenlist
      selected_features <- st_intersects(specimens() , drawn_polygon, sparse=FALSE)

      #print the name of the specimenlist
      data.table(specimens())[selected_features[,1]][order(species),.(.N),by=.(species, IUCN)]
      
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