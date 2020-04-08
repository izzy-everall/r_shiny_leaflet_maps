#setwd('/Users/izzyeverall/Documents/croydon_map')
library('shiny')
library('leaflet')
library('remotes')
library('rsconnect')
library('fontawesome')
library('purrr')
library('tidyverse')
library('leaflet.extras')
library('PostcodesioR')
library('geosphere')

#remotes::install_github("rstudio/fontawesome")

#reads data from google sheet
#shop_data_url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vTZv53uxm___rwa56YeqRgMfd54DhAL_3LN6mrAPoZbrFpjI_VGkn8QwkTswfPKHYuJ78pF1zDRzcD4/pub?output=csv'
shop_data_url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vTZTWchHMMYb_S-edEE0VoRIYcUn0fprImUiCJwX1bNRkOt4o57n1dEvxFKsQafpIbBiX6qDg_-PVgf/pub?output=csv'
shop_data <- read.csv(url(shop_data_url))

#user interface
ui <- bootstrapPage(
  
  sidebarPanel(id = "controls", #class = "panel panel-default",
               fixed = TRUE,
               draggable = TRUE,
               #top = 100, left = "auto", right = 20, bottom = "auto",
               #width = 330, height = "auto",
               h4("Find out which shops are nearby?"),
               textInput(inputId = "userlocation",
                         label = "Enter postcode",
                         value = ""),
               helpText("Enter the postcode (e.g CR2 7HH) to search for nearby shops. If the APP SAYS RELOAD either there are no shops within that distance or there's an error with the postcode"),
               
               sliderInput(inputId = "searchdistance", label = "Search Distance (Km):",
                           min = 0, max = 10, value = 5),
               helpText("Select a distance and the shops within the distance from your postcode will appear on the map."),
               actionButton("go", "Search"),
               actionButton("reset","Reset map")),
  
  #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$style(type = "text/css", ".box-body {height:80vh}"),
  mainPanel(leafletOutput("mymap"))
)


# server code:
server <- function(input, output) {
  
  #hopefully nice icons - doesn't recognise them all even though the icon name is defo correct!
  shop_icons <- awesomeIconList(
    Pharmacy = makeAwesomeIcon(icon = "prescription-bottle-alt", library = "fa", markerColor = "green"),
    Local   = makeAwesomeIcon(icon = "store-alt", library = "fa", markerColor = "purple"),
    Supermarket  = makeAwesomeIcon(icon = "shopping-cart", library = "fa", markerColor = "orange"),
    Home = makeAwesomeIcon(icon = 'home', library = "fa", markerColor = "red")
  )
  
  
  #format websites
  shop_data <- shop_data %>%
    mutate(format_website = paste('https://',shop_data$Website, sep=""))
  
  #add popup to shop data 
  shop_data <- shop_data %>%
    mutate(popup = paste("<b>Shop Name: </b>", shop_data$Name, "<br/>", 
                         "<b>Address: </b>", shop_data$Address, "<br/>",
                         "<b>Website: </b>", "<b><a href='", shop_data$format_website,"'>", shop_data$Website,"</a></b>","<br/>",
                         "<b>Phone number: </b>", shop_data$Phone.number, "<br/>",
                         "<b>Email: </b>", shop_data$Email, "<br/>",
                         "<b>Opening Days: </b>", shop_data$Normal.opening.days, "<br/>", "<b>Opening hours: </b>", shop_data$Normal.opening.hours, "<br/>",
                         "<b>Vulnerable groups days and hrs: </b>", shop_data$Vulnerable.groups.Shopping.days, ", ", 
                         shop_data$Vulnerable.groups.Shopping.hours, "<br/>",
                         "<b>Key Workers days and hrs: </b>", shop_data$Key.Workers.Shopping.days, ", ",
                         shop_data$Key.Workers.Shopping.hours, "<br/>",
                         "<b>Online shopping, collect in store: </b>", shop_data$Online.shopping.collect.in.store, "<br/>", 
                         "<b>Instore shopping, then delivery: </b>", shop_data$Instore.shopping.then.delivery, "<br/>", 
                         "<b>Online shopping, then delivery: </b>", shop_data$Online.shopping.then.delivery, "<br/>",
                         "<b>Form of Payment accepted: </b>", shop_data$Form.of.Payment.Accepted, "<br/>",
                         "<b>Disabled access: </b>", shop_data$Disabled.Access, "<br/>",
                         "<b>Notes: </b>", shop_data$Notes, "<br/>", sep=''))
  
  
  
  # Determine shops in area of interest
  shops_in_area_reactive <- eventReactive(input$go, { 
    address_latlon <- postcode_lookup(input$userlocation)
    dist <- distm(x = matrix(data = c(shop_data$Longitude, shop_data$Latitude), ncol = 2), 
                  y = c(lon = address_latlon$longitude, lat = address_latlon$latitude), 
                  fun = distVincentySphere)
    dist <- dist/1000
    
    
  })
  
  #filter data frame based on user input:
  shop_reactive <- reactive({shop_data[shops_in_area_reactive() <= input$searchdistance,]})
  
  
  #Render leaflet 
  output$mymap <- renderLeaflet({
    m <- leaflet(shop_data) %>%
      addTiles(group='OSM (default)') %>%
      setView(lng=-0.098034,lat=51.380666, zoom=10) %>%
      addMarkers(data=shop_data, lng=~Longitude, lat=~Latitude, label=~Name, popup=~popup, group='shops') %>%
      addResetMapButton() %>%
      addSearchFeatures(
        targetGroups = c('shops','user'),
        options = searchFeaturesOptions(
          zoom=18, openPopup = TRUE, firstTipSubmit = TRUE,
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))
    
    m  
    
  })
  
  # Function to plot only shops in certain distance from house by 
  observeEvent(input$go, {
    
    #get user location 
    user_loc <- postcode_lookup(input$userlocation)
    
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      setView(lng=user_loc$longitude,lat=user_loc$latitude, zoom=13) %>%
      addAwesomeMarkers( lng=user_loc$longitude, lat=user_loc$latitude, popup=paste('You are here approximately'), label=paste('You are here approximately'), group='user',icon=shop_icons[["Home"]]) %>%
      addMarkers(data = shop_reactive(),
                 lng = ~Longitude, lat = ~Latitude,
                 # popup
                 popup = ~popup,
                 group = "shops", label=~Name)
  })
  
  
  observeEvent(input$reset, {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      setView(lng=-0.098034,lat=51.380666, zoom=10) %>%
      addMarkers(data=shop_data, lng=~Longitude, lat=~Latitude, label=~Name, popup=~popup, group='shops') %>%
      addResetMapButton() %>%
      addSearchFeatures(
        targetGroups = c('shops','user'),
        options = searchFeaturesOptions(
          zoom=18, openPopup = TRUE, firstTipSubmit = TRUE,
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))
    
  })
  
}

shinyApp(ui, server)

