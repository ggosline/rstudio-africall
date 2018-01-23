
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyTree)
library(sf)
library(sp)
library(dplyr)
library(DT)
library(data.table)
#library(mapedit)


#AcanthAll <- SpatialPointsDataFrame(AcanthAll[,c("Longitude","Latitude")],AcanthAll)
speciesnames <- AcanthAll %>% group_by(genus, species) %>% count()
specieslist <- split(speciesnames, speciesnames$genus)

spll <- mapply(function(z){mapply(function(x,y) { y }, z[[2]], z[[3]], SIMPLIFY = FALSE,USE.NAMES = TRUE)},specieslist,USE.NAMES = T)

ui <- fillPage(
  # titlePanel("Acanthaceae of Africa"),
  
  sidebarLayout(
    # sidebarPanel(selectInput("selectedspecies", "Species:",specieslist), multiple=TRUE),
    sidebarPanel(width=2,
      checkboxInput("clusterpts", label="Cluster Points", value=TRUE),
      checkboxInput("showallspecs", label="Select taxa", value=FALSE),
      conditionalPanel(condition = "input.showallspecs",
        shinyTree('speciesselect',checkbox = TRUE))
      ),
    mainPanel(
      tabsetPanel(
          tabPanel("Map",
            leafletOutput("mymap", width="100%", height="700px")),
          tabPanel("Species List",
              dataTableOutput("specieslist"))
              )
    )
  )
)

server <- function(input, output, session) {
  
  output$speciesselect <- renderTree(spll)
  
  specimens <- reactive({
              if (!input$showallspecs | length(get_selected(input$speciesselect,format="names")) == 0) AcanthAll
              else AcanthAll[AcanthAll$species %in% get_selected(input$speciesselect,format="names"),]
            })
                     
  
  observe( {
    leafletProxy("mymap") %>% clearMarkerClusters() %>%
                              addCircleMarkers(data = specimens(), label = ~species,
                                  radius = 1,
                                  clusterOptions = markerClusterOptions(),
                                  popup = ~sprintf("<strong>%s</strong><br>%s %s<br>%s<br>%s<br>%s",
                                                   species,recordedby,recordnumber,eventdate,catalogNumber, rowid
                                  )
                                  )
  })
  
  observeEvent(input$clusterpts, {
    if (input$clusterpts)
    {leafletProxy("mymap") %>% clearMarkers() %>%
        addCircleMarkers(data = specimens(), label = ~species, radius=1,
                   clusterOptions = markerClusterOptions(),
                   popup = ~sprintf("<strong>%s</strong><br>%s %s<br>%s<br>%s<br>%s",
                                    species,recordedby,recordnumber,eventdate, catalogNumber ,rowid))}
    else {
      leafletProxy("mymap") %>% clearMarkerClusters() %>%
      addCircleMarkers(data = specimens(), label = ~species, radius=1,
                       popup = ~sprintf("<strong>%s</strong><br>%s %s<br>%s<br>%s<br>%s",
                                        species,recordedby,recordnumber,eventdate, catalogNumber ,rowid))}
                 
  })
  
  output$mymap <- renderLeaflet({
    
    leaflet(AcanthAll) %>% addTiles() %>% 
      addMarkers(label = ~species,
                 clusterOptions = markerClusterOptions(),
                 popup = ~sprintf("<strong>%s</strong><br>%s %s<br>%s<br>%s<br>%s",
                                  species,recordedby,recordnumber,eventdate, catalogNumber,rowid)) %>%
      addDrawToolbar(
        targetGroup='select',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        circleOptions = TRUE,
        singleFeature = TRUE) %>%
      
    addLayersControl(overlayGroups = c('select'), options =
                       layersControlOptions(collapsed=FALSE)) 

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
      st_crs(drawn_polygon) <- st_crs(AcanthAll)

      #use over from the sp package to identify selected AcanthAll
      selected_features <- st_intersects(specimens() , drawn_polygon, sparse=FALSE)

      #print the name of the AcanthAll
      data.table(specimens())[selected_features[,1]][order(species),.(.N),by=species]
      
    } else if(feature_type=="circle") {
      #get the coordinates of the center of the cirle
      center_coords <- matrix(c(input$mymap_draw_new_feature$geometry$coordinates[[1]],input$mymap_draw_new_feature$geometry$coordinates[[2]]),ncol=2)

      #calculate the distance of the AcanthAll to the center
      dist_to_center <- spDistsN1(AcanthAll,longlat=TRUE)

      #select the AcanthAll that are closer to the center than the radius of the circle
      as.character(AcanthAll[dist_to_center < input$mymap_draw_new_feature$properties$radius/1000,"AccentCity"])
    }
    
    
  })
  
}

shinyApp(ui, server)