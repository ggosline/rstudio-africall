
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyTree)
library(sf)
library(sp)
library(mapedit)

#AcanthAll <- SpatialPointsDataFrame(AcanthAll[,c("Longitude","Latitude")],AcanthAll)
speciesnames <- AcanthAll %>% group_by(genus, species) %>% count()
specieslist <- split(speciesnames, speciesnames$genus)

spll <- mapply(function(z){mapply(function(x,y) { y }, z[[2]], z[[3]], SIMPLIFY = FALSE,USE.NAMES = TRUE)},specieslist,USE.NAMES = T)

ui <- fluidPage(
  titlePanel("Acanthaceae of Africa"),
  
  sidebarLayout(
    # sidebarPanel(selectInput("selectedspecies", "Species:",specieslist), multiple=TRUE),
    sidebarPanel(shinyTree('speciesselect',checkbox = T)),
    mainPanel(leafletOutput("mymap"))
  
  )
)

server <- function(input, output, session) {
  
  output$speciesselect <- renderTree(spll)
  
  output$mymap <- renderLeaflet({
    
    leaflet(filter(AcanthAll,species %in% get_selected(input$speciesselect))) %>% addTiles() %>% 
              
    addMarkers(label = ~species,
               clusterOptions = markerClusterOptions()) %>%
      
    addDrawToolbar(
      targetGroup='select',
      polylineOptions=FALSE,
      markerOptions = FALSE,
      circleOptions = TRUE,
      singleFeature = TRUE) %>%
      
    addLayersControl(overlayGroups = c('select'), options =
                       layersControlOptions(collapsed=FALSE)) 

  })
  
  output$selected_points <- renderText({

    #use the draw_stop event to detect when users finished drawing
    req(input$mymap_draw_stop)
    print(input$mymap_draw_new_feature)
    feature_type <- input$mymap_draw_new_feature$properties$feature_type

    if(feature_type %in% c("rectangle","polygon")) {

      #get the coordinates of the polygon
      polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]

      #transform them to an sp Polygon
      drawn_polygon <- st_sf(1, st_sfc(st_polygon(list(matrix(unlist(polygon_coordinates),ncol=2,byrow=TRUE)))))
      st_crs(drawn_polygon) <- st_crs(AcanthAll)

      #use over from the sp package to identify selected AcanthAll
      selected_features <- st_intersects(AcanthAll , drawn_polygon, sparse=FALSE)

      #print the name of the AcanthAll
      as.character(as.data.frame(AcanthAll)[selected_features,"species"])
      
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