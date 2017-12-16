
library(shiny)
library(leaflet)
library(leaflet.extras)

#AcanthAll <- SpatialPointsDataFrame(AcanthAll[,c("Longitude","Latitude")],AcanthAll)

ui <- fluidPage(
  leafletOutput("mymap"),
  textOutput("selected_AcanthAll")
)


server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(0,0,2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data=AcanthAll,label=~species) %>%
      addDrawToolbar(
        targetGroup='select',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        circleOptions = TRUE)  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE)) 
  })
  
  output$selected_AcanthAll <- renderText({
    #use the draw_stop event to detect when users finished drawing
    req(input$mymap_draw_stop)
    print(input$mymap_draw_new_feature)
    feature_type <- input$mymap_draw_new_feature$properties$feature_type
    
    if(feature_type %in% c("rectangle","polygon")) {
      
      #get the coordinates of the polygon
      polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
      
      #transform them to an sp Polygon
      drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      
      #use over from the sp package to identify selected AcanthAll
      selected_AcanthAll <- AcanthAll %over% SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
      
      #print the name of the AcanthAll
      AcanthAll[which(!is.na(selected_points)),"species"]
    } else if(feature_type=="circle") {
      #get the coordinates of the center of the cirle
      center_coords <- matrix(c(input$mymap_draw_new_feature$geometry$coordinates[[1]],input$mymap_draw_new_feature$geometry$coordinates[[2]]),ncol=2)
      
      #calculate the distance of the AcanthAll to the center
      dist_to_center <- spDistsN1(AcanthAll,longlat=TRUE)
      
      #select the AcanthAll that are closer to the center than the radius of the circle
      AcanthAll[dist_to_center < input$mymap_draw_new_feature$properties$radius/1000,"AccentCity"]
    }
    
    
  })
  
}

shinyApp(ui, server)