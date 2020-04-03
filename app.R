library('shiny')
library('leaflet')
library('rsconnect')

#### gooogle sheets ##
##### the data #######
#shop_data_url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vT_uHb2LCaDla2FDfrcQziAsb8e38KXCkBv0CHxkvldGYQf0ngAb8Kxdwt17ke7oikLkOq_Y70dVHqc/pub?gid=0&single=true&output=csv'
#shop_data_url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vR7_BV83iKdjtI8rM4PRRyuSjCgbeeRPWEdcuEqiO7ODu3mJbYMsi_4wkTSm6vAePIFPTr6ofiSTZ_D/pub?output=csv'
#shop_data <- read_csv(url(shop_data_url))
#######################
#rsconnect::deployApp('/Users/izzyeverall/Documents/cr_map_k')
# user interface
ui <- fluidPage(
  mainPanel(
    #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"), # setting map to be whole window view
    #tags$style(type = "text/css", ".box-body {height:80vh}"),
    leafletOutput(outputId = "mymap", width='100%', height="1000")) #this will create a space for us to display our map #width="100%", height="1000px"
  
  )

# server code:
server <- function(input, output) {
  shop_data_url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vR7_BV83iKdjtI8rM4PRRyuSjCgbeeRPWEdcuEqiO7ODu3mJbYMsi_4wkTSm6vAePIFPTr6ofiSTZ_D/pub?output=csv'
  shop_data <- read.csv(url(shop_data_url))
  
  #API call for Croydon borough boundary?
  
  output$mymap <- renderLeaflet({
    leaflet(shop_data) %>% 
      addTiles(group='OSM (default)') %>%
      setView(lng=-0.098034,lat=51.380666, zoom=12) %>%
      addMarkers(lng = ~long_wgs, lat = ~lat_wgs,
                 popup = paste("<b>Shop Name: </b>", shop_data$store_name, "<br/>", "<b>Website: </b>", shop_data$website, "<br/>", "<b>Phone number: </b>", shop_data$telephone, "<br/>",
                               "<b>Address: </b>", shop_data$add_one, ', ', shop_data$suburb, "<br/>", "<b>Postcode: </b>", shop_data$postcode, "<br/>", sep=''))
    
  })  
}

shinyApp(ui, server)
