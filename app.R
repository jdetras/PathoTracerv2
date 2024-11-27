library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(shinyjs)
library(slickR)

# Load dataset
rice_data <- read.csv("data/latest-data-loading-28May-2024/all_rice_bb_data.csv")


# Filter rows where latitude and longitude are NA
missing_coordinates <- rice_data %>%
  filter(is.na(latitude) | is.na(longitude))

# For Debugging: View the filtered data and output the missing coordinates
print(missing_coordinates)
write.csv(missing_coordinates, "missing_coordinates.csv", row.names = FALSE)

## Cleaning: remove all entries with missing Lat and Long
# Remove rows with missing latitude or longitude
rice_data <- rice_data %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Summarize cumulative isolates per country
cumulative_isolates <- rice_data %>%
  group_by(country = country) %>%  # Replace with the correct country column
  summarize(IsolateCount = n())

# Load world boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

# For debugging purposes
print(unique(rice_data$country))
print(unique(world$sovereignt))

# Merge isolate data with world boundaries
world_data <- left_join(world, cumulative_isolates, by = c("sovereignt" = "country"))


# Define custom color palette
#custom_palette <- c("#ece75f","#e8e337","#e5de00","#e6cc00","#e6b400","#e69b00","#e47200")
#custom_palette <- c("#E0AAFF","#9D4EDD","#7B2CBF","#5A189A","#3C096C","#240046","#10002B") # purple
custom_palette <-c("#ffcbd1", "#f69697", "#ee6b6e" ,"#f94449","#ff2c2c","#f01e2c", "#de0a26", "#d1001f", "#c30010") #red

# Create a color bin function
pal <- colorBin(palette = custom_palette, domain = world_data$IsolateCount, bins = 6, na.color = "gray")

# Define the color palette for AxooPopn
axoo_colors <- c(
  "Xoc" = "#ffff99",
  "Axoo_01" = "#111E6C",
  "Axoo_02" = "#0F52BA",
  "Axoo_03" = "#0000FF",
  "Axoo_04" = "#FA8072",
  "Axoo_05" = "#EA3C53",
  "Axoo_06" = "#CD5C5C",
  "Axoo_07" = "#B22222",
  "Axoo_08" = "#FF2400",
  "Axoo_09" = "#960018",
  "Axoo_10" = "#C7EA46",
  "Axoo_11" = "#4F7942",
  "Axoo_12" = "#0B6623",
  "L1_Unresolved" = "palegreen",
  "L2_Unresolved" = "yellow2",
  "L3_Unresolved" = "wheat3",
  "NA" = "gray"
)

# Create a color factor to map AxooPopn to colors
axoo_pal <- colorFactor(palette = axoo_colors, domain = names(axoo_colors))

# Custom function to create cluster icons based on AxooPopn
getClusterIcon <- function(AxooPopn) {
  color <- ~axoo_pal(AxooPopn)
  # Create a custom icon for the cluster based on AxooPopn
  icon <- makeIcon(
    iconUrl = paste0("https://dummyimage.com/24x24/", substr(color, 2, 7), "/ffffff&text=+"), # Cluster color
    iconWidth = 24,
    iconHeight = 24
  )
  return(icon)
}

custom_theme <- theme(
  plot.title = element_text(size = 20, face = "bold", hjust = 0),  # Align title to left
  plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
  axis.text.x = element_text(angle = 30, hjust = 1, size = 12, vjust = 0.5),
  axis.text.y = element_text(size = 14),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(angle = 90,size = 14),
  legend.title = element_text(size = 14, face="bold"),
  legend.text = element_text(size = 10),
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white", color = NA)
)

# Apply the theme globally
theme_set(custom_theme)

ui <- navbarPage(
  title = tags$div(
    tags$img(src = "pathotracer_logo.png", height = "50px")
    #"PathoTracer"
  ),
  
  theme = bslib::bs_theme(
    bg = "#F9F9F9",     # Light gray background
    fg = "#000000",     # Black text
    primary = "#00AF4F",# Main green
    secondary = "#FFFF00", # Yellow
    success = "#00B050",  # Accent green
    base_font = bslib::font_google("Merriweather")
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.fullscreen/1.6.0/leaflet.fullscreen.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.fullscreen/1.6.0/Leaflet.fullscreen.min.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # Include the CSS
  ),
  
  navbarMenu("Home",
             tabPanel("About the Tool",
                      fluidRow(style = "background-color: #4CAF50;",
                               column(12,
                                      tags$div(
                                        style = "position: relative; text-align: center; color: white;",
                                        tags$img(src = "bacterial_blight_crop.jpg"),
                                        tags$div(
                                          style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                                          tags$h1("PathoTracer", class = "banner-header"),
                                          tags$h3("An informed-decision platform to reduce the risk of rice diseases", style = "font-size: 1.5em; font-weight: bold;")
                                        )
                                      )
                               )
                      ),
                      fluidRow(
                        column(12, class = "about-section-header",
                               tags$h2("ABOUT THE TOOL")
                        ),
                        column(12, class = "banner-text",
                               tags$p(tags$b("PathoTracer"), "is a decision support system that integrates early-season pathogen diagnostics and disease resistance profiles intended for use by public and private enterprises in accurately defining breeding priorities and in implementing coordinated actions to manage crop diseases in real-time.")
                        )
                      ),
                      fluidRow(
                        column(12,
                               tags$h2("WHAT OUR PARTNERS ARE SAYING", class = "about-section-header"),
                               div(class = "slick-carousel", slickROutput("testimonials_carousel", width = "90%"))
                        )
                      )
             ),
             tabPanel(tags$a(href = "https://app.smartsheet.com/b/publish?EQBCT=7fd054ee4696420a91bd4a05e14f6fe0", "Sample Submission(SMS)"))
  ),
  
  # Disease Maps
  navbarMenu(
    title = "Disease Maps",
    tabPanel("Bacterial Blight", 
      fluidPage(
        #titlePanel("Bacterial Blight"),
        fluidRow(
          tags$div(
            class = "card card-custom", 
            div(
              tags$p(
                class = "card-text",
                tags$b(class = "card-title", "Bacterial Blight"), 
                "is caused primarily by Xanthomonas oryzae pv. oryzae (Xoo). Asian strains of Xoo were found to have 3 major lineages which are further divided into 12 subpopulations based on 22,115 genome-wide SNP data. Navigate the Map tab below to see the distribution of each Asian Xoo."
              )
            )
          )
        ),
        tags$br(),
        tabsetPanel(
          type="pills",
          tabPanel("Overview",
            fluidRow(
              sidebarLayout(
                sidebarPanel(
                  #selectInput("country_filter", "Select Country", choices = c("All", unique(rice_data$country))),
                  #selectInput("province_filter", "Select Province", choices = c("All")),
                  #actionButton("reset_map", "Reset All Filters", class = "btn-reset"),
                  div(
                    class = "info-box",
                    h4("Information")
                  ),
                  div(
                    class ="info-content",
                    textOutput("country_info"),
                    textOutput("total_isolates"),
                  ),
                  div(
                    class = "info-box",
                    h4("Distribution"),
                  ),
                  plotOutput("isolate_barchart", height = "250px"), # Placeholder for bar chart
                  actionButton("view_chart", "View Chart", class = "btn-view-chart") # Button for pop-out
                  #actionButton("reset_map", "Reset Map",class= "btn-reset ") # Reset button
                ),
                mainPanel(
                  leafletOutput("map", height = "500px")
                )
              )
            ),
            tags$br(),
            fluidRow(
              # Plot the AxooPopn per Country
              # Show as stacked chart
              tags$br(),
              plotOutput("stacked_barchart", height = "400px", width = "100%") # Display the dynamic stacked chart
            )
          ),
          
          tabPanel("Filter",
            "Under construction. We will be back soon."
          ) # end of Filter Panel
          
        )  # end of contents of tabsetPabel
      )#end of fluidPage
    ),
    tabPanel("Rice Blast", fluidPage(
      #titlePanel("Rice Blast"),
       tags$br(),
       tags$h4("Genotyping markers for blast are now available."),
       tags$p("To genotype your samples, please submit your samples using this ",tags$a(href="https://app.smartsheet.com/b/publish?EQBCT=7fd054ee4696420a91bd4a05e14f6fe0", "link"))
    ))
  ), #end of disease Maps
  
  tabPanel("News",
     fluidRow(
       column(12,style="text-align:center;",
              tags$h2("Our Impact", style = "background-color: #4CAF50; color: white; padding: 10px 0; text-align: center; font-weight: bold; margin-bottom: 10px;"),
              div(class="slick-carousel-news", slickROutput("news_carousel", width = "90%"))
       )
     )      
  ),
  
  #tabPanel("Contact Us", "Contact details here.")
  tabPanel("Contact Us",
     fluidRow(
       tags$h2(tags$b(tags$u("Contact Us"))),
       tags$br(),
       column(6,
              class = "contact-card",
              tags$h5(tags$b("Dr. Van Schepler-Luu")),
              tags$p(tags$b("Group leader/Scientist II: Plant Pathology and Host Plant Resistance")),
              tags$p("Rice Breeding Innovation Department (RBI)"),
              tags$p("International Rice Research Institute (IRRI)"),
              tags$p("Los Baños, Laguna 4031, Philippines"),
              tags$p("Phone: +63 (2) 8580 5600 ext. 2743"),
              tags$p("Email: v.scheplerluu@irri.org")
              
       ),
       column(6,
              class = "contact-card",
              tags$h5(tags$b("Dale Pinili")),
              tags$p(tags$b("Assistant Scientist - Plant Pathology and Host Plant Resistance")),
              tags$p("Rice Breeding Innovation Department (RBI)"),
              tags$p("International Rice Research Institute (IRRI)"),
              tags$p("Los Baños, Laguna 4031, Philippines"),
              tags$p("Email: d.pinili@irri.org")
              
       )
     )
  ) # end of Contact Us
)

server <- function(input, output, session) {
  
  ## testimonies for slickR
  output$testimonials_carousel <- renderSlickR({
    testimonials <- list(
      div(
        style = "padding: 10px 60px; border: 1px solid #ddd; margin-bottom: 20px;",
        tags$p(tags$i("\"BLB is a widespread problem for rice growers across Pakistan, especially in the basmati rice basket of Punjab. Through the Pathotracer collaboration, we have started studying the genetic diversity of Xoo bacterias thriving in the different rice agroecologies across the country and discovered different strains. This information will help us identify which BLB-resistant varieties to deploy now and in the future, so that farmers harvest more grain, with less chemical sprays.\"")),
        tags$p(tags$b("Dr. Muhammad Zakria, Principal Scientific Officer, Crop Diseases Research Institute (CDRI), National Agricultural Research Center (NARC), Islamabad, Pakistan."))
      ),
      div(
        style = "padding: 10px 60px; border: 1px solid #ddd; margin-bottom: 20px;",
        tags$p(tags$i("\"Rice is very important for Bangladeshi people. Bacterial blight caused by Xoo pathogen reduces rice yields by 10 to 15% every year, and the disease has become unpredictable because of changing climate and emergence of new Xoo strains. Pathotracer will help us map which types of BLB are virulent in each rice growing region, detect the possible changes across the seasons, and make better informed decisions to manage this disease.\"")),
        tags$p(tags$b("Dr. MA Latif, Chief scientific officer and Head of Plant Pathology Division, BRRI, Gazipur, Bangladesh."))
      )
    )
    slickR(testimonials)
  })
  
  ## news
  output$news_carousel <- renderSlickR({
    news <- list(
      div(
        style = "padding: 10px 60px; border: 1px solid #ddd; margin-bottom: 20px;",
        tags$p(tags$i("\"A disease-surveillance network in Africa will accelerate detection and actions to prevent the spread of major rice diseases\""), style="font-size:30px;"),
        tags$p(tags$a(href="https://www.irri.org/news-and-events/news/disease-surveillance-network-africa-will-accelerate-detection-and-actions", "Read More"), style="font-color: #880808;")
      ),
      div(
        style = "padding: 10px 60px; border: 1px solid #ddd; margin-bottom: 20px;",
        tags$p(tags$i("\"CGIAR Science Day in Vietnam showcases research and innovations for food security\""),style="font-size:30px;"),
        tags$p(tags$a(href="https://www.ilri.org/news/cgiar-science-day-vietnam-showcases-research-and-innovations-food-security", "Read More"), style="font-color: #880808;")
      )
    )
    slickR(news)
  })
  
  # Calculate default statistics
  # Get Total Distinct Countries
  total_countries <- rice_data %>%
    summarize(n_countries = n_distinct(country)) %>%
    pull(n_countries)
  
  # GEt Total Distinct Institute
  total_institutes <- rice_data %>%
    summarize(n_institutes = n_distinct(institute)) %>%
    pull(n_institutes)  # Replace 'institute' with the correct column name in your dataset
  
  # Calculate default data for all AxooPopn
  default_axoo_data <- rice_data %>%
    group_by(AxooPopn) %>%
    summarize(Count = n(), .groups = "drop")
  
  # Reactive value to track the selected country
  selected_country <- reactiveVal(NULL)
  
  
  # Update sidebar information
  observeEvent(input$map_shape_click, {
    clicked <- input$map_shape_click
    
    if (!is.null(clicked$id)) {
      selected_country(clicked$id)
    } else {
      selected_country(NULL)
    }
  })
  
  # Handle reset button
  observeEvent(input$reset_map, {
    selected_country(NULL) # Clear the selected country
  })
  
  # Render sidebar information
  output$country_info <- renderText({
    if (is.null(selected_country())) {
      paste("Total  distinct countries: ", total_countries)
    } else {
      #paste("Country Name: ", selected_country())
      selected_country()
    }
  })
  
  output$total_isolates <- renderText({
    if (is.null(selected_country())) {
      paste("Total  distinct institutes: ", total_institutes)
    } else {
      # Fetch data for the selected country
      country_data <- world_data %>%
        filter(sovereignt == selected_country()) %>%
        as.data.frame()
      
      paste("Total Isolates: ", ifelse(is.na(country_data$IsolateCount[1]), 0, country_data$IsolateCount[1]))
    }
  })
  
  # Render the pop-out modal dialog
  observeEvent(input$view_chart, {
    showModal(
      modalDialog(
        title = "Isolate Distribution Chart",
        plotOutput("isolate_barchart", height = "400px", width = "100%"), # Larger plot in modal
        #class= "",
        downloadButton("download_chart", "Download"), # Add download button
        easyClose = TRUE, # Allow closing the modal by clicking outside
        footer = modalButton("Close") # Add a close button
      )
    )
  })
  
  # Render the bar chart for the AxooPopn
  output$isolate_barchart <- renderPlot({
    if (is.null(selected_country())) {
      # No country selected: show total isolates per AxooPopn globally
      ggplot(default_axoo_data, aes(x = reorder(AxooPopn, -Count), y = Count, fill = AxooPopn)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = axoo_colors, na.translate = TRUE) +
        theme_minimal() +
        labs(
          title = "Global Distribution of Pathogen Population",
          x = "AxooPopn",
          y = "Sample Count"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # Country selected: show isolates per AxooPopn in the selected country
      country_name <- selected_country()
      
      filtered_data <- rice_data %>%
        filter(country == country_name) %>%
        group_by(AxooPopn) %>%
        summarize(Count = n(), .groups = "drop")
      
      if (nrow(filtered_data) > 0) {
        ggplot(filtered_data, aes(x = reorder(AxooPopn, -Count), y = Count, fill = AxooPopn)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = axoo_colors, na.translate = TRUE) +
          theme_minimal() +
          labs(
            title = paste("Pathogen Distribution in", country_name),
            x = "Pathogen Population",
            y = "Sample Count"
          ) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        # No data for the selected country
        ggplot() +
          annotate("text", x = 1, y = 1, label = "No data available for this country.") +
          theme_void()
        }
      }
    })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet(world_data, options = leafletOptions(worldCopyJump = TRUE)) %>%
      #addTiles() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom=3, maxZoom=15)) %>%
      addFullscreenControl()  %>%    # Add fullscreen control button
      setView(lng = mean(rice_data$longitude, na.rm = TRUE), lat = mean(rice_data$latitude, na.rm = TRUE), zoom = 4) %>%
      addPolygons(
        layerId = ~sovereignt,  # Add this line for reactVal to get the shape_id
        fillColor = ~pal(IsolateCount),
        weight = 1,
        color = "gray",
        fillOpacity = 0.8,
        smoothFactor = 0.2,
        popup = ~paste(sovereignt, "<br>", "Total Samples: ", IsolateCount)
      ) %>%
      addCircleMarkers(
        data = rice_data,
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste("Database Code: ", databasecode, "<br>",
                       "AxooPopn: ", AxooPopn, "<br>",
                       "District: ", district, "<br>",
                       "Province: ", province, "<br>",
                       "Country: ", country, "<br>",
                       "Year collected: ", year
                       ),
        radius = 5,
        color = ~axoo_pal(AxooPopn),
        fillColor = ~axoo_pal(AxooPopn),
        fillOpacity = 0.7,
        #clusterOptions = markerClusterOptions(  # show the clusters on each country
        #  showCoverageOnHover = TRUE, 
        #  zoomToBoundsOnClick=TRUE, 
        #  removeOutsideVisibleBounds = TRUE, 
        #  iconCreateFunction = JS(
        #    "function(cluster) { 
        #      var markers = cluster.getAllChildMarkers(); 
        #      var firstMarker = markers[0];
        #      var color = firstMarker.options.color;
        #      return L.divIcon({ 
        #        className: 'leaflet-cluster', 
        #        html: '<div style=\"background-color:' + color + '; width: 30px; height: 30px; line-height: 30px; color: white; border-radius: 50%; text-align: center;\">' + markers.length + '</div>', 
        #        iconSize: [30, 30] 
        #      }); 
        #    }"
        #  )
        #  ),
        stroke = FALSE
      ) %>%
      addLegend(
        pal = pal,
        values = ~IsolateCount,
        position = "topleft",
        title = "Sample Density",
        labFormat = labelFormat(suffix = ""),
        opacity = 0.5
      ) %>%
      addLegend(
        colors = axoo_colors,
        labels = names(axoo_colors),
        position = "topright",
        title = "Pathogen Population",
        opacity = "0.7"
      )
    
  })
  
  # When a country is clicked
  observeEvent(input$map_shape_click, {
    # If error encountered, click$id is NULL, make sure there's a layer ID in the map.AddPolygons()
    clicked <- input$map_shape_click
    
    # For Debugging Purposes: Show the clicked object
    print(clicked$id)  
    
    if (!is.null(clicked$id)) {
      selected_country <- world_data %>%
        filter(sovereignt == clicked$id) %>%  # Ensure `sovereignt` matches `clicked$id`
        as.data.frame()
      
      if (nrow(selected_country) > 0) {
        country_name <- selected_country$sovereignt[1]
        
        output$country_info <- renderText({ paste("Country Name:", country_name )})
        output$total_isolates <- renderText({
          paste("Total Isolates: ", ifelse(is.na(selected_country$IsolateCount[1]), 0, selected_country$IsolateCount[1]))
        })
        
        # Filter rice data for the selected country
        filtered_data <- rice_data %>%
          filter(country == country_name) %>%  # Ensure the 'country' column in your CSV is correct
          group_by(AxooPopn) %>%               # Group only by AxooPopn
          summarize(Count = n(), .groups = "drop")  # Summarize by AxooPopn counts
        
        # For Debugging Purposes: Show the filtered data
        print(filtered_data)  
        
        # Render the bar chart
        if (nrow(filtered_data) > 0) {
          output$isolate_barchart <- renderPlot({
            ggplot(filtered_data, aes(x = reorder(AxooPopn, -Count), y = Count, fill = AxooPopn)) +
              geom_bar(stat = "identity") +
              scale_fill_manual(values = axoo_colors, na.translate = TRUE) +
              #theme_minimal() +
              labs(
                title = paste("Pathogen Population Distribution in", country_name),
                x = "AxooPopn",
                y = "Sample Count"
              ) #+
              #theme(axis.text.x = element_text(angle = 45, hjust = 1))
          })
        } else {
          # Show a message if no data is available
          output$isolate_barchart <- renderPlot({
            ggplot() +
              annotate("text", x = 1, y = 1, label = "No data available for this country.") +
              theme_void()
          })
        }
      }
    }
  })
  
  ## For downloading the bar chart
  # Download handler for the chart
  output$download_chart <- downloadHandler(
    filename = function() {
      paste0("bar_chart_", Sys.Date(), ".png") # Filename with date
    },
    content = function(file) {
      # Save the current plot to a PNG file
      ggsave(
        file,
        plot = isolate(
          if (is.null(selected_country())) {
            ggplot(default_axoo_data, aes(x = reorder(AxooPopn, -Count), y = Count, fill = AxooPopn)) +
              geom_bar(stat = "identity") +
              scale_fill_manual(values = axoo_colors, na.translate = TRUE) +
              theme_minimal() +
              labs(
                title = "Global Distribution of Pathogen Population",
                x = "Pathogen Population (AxooPopn)",
                y = "Sample Count"
              ) +
              theme(  )
          } else {
            country_name <- selected_country()
            filtered_data <- rice_data %>%
              filter(country == country_name) %>%
              group_by(AxooPopn) %>%
              summarize(Count = n(), .groups = "drop")
            
            if (nrow(filtered_data) > 0) {
              ggplot(filtered_data, aes(x = reorder(AxooPopn, -Count), y = Count, fill = AxooPopn)) +
                geom_bar(stat = "identity") +
                scale_fill_manual(values = axoo_colors, na.translate = TRUE) +
                theme_minimal() +
                labs(
                  title = paste("Pathogen Distribution in", country_name),
                  x = "Pathogen Population",
                  y = "Sample Count"
                ) +
                theme(
                  axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                  plot.title = element_text(size = 14, face = "bold"),
                  panel.background = element_rect(fill = "white"),
                  plot.background = element_rect(fill = "white", color = NA)
                )
            } else {
              ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = "No data available for this country.") +
                theme_void()
            }
          }
        ),
        device = "png",
        width = 10,
        height = 7
      )
    })
  
    # Render the stacked bar chart dynamically
    output$stacked_barchart <- renderPlot({
      # Reactive check for selected country
      country_name <- selected_country()
      
      if (is.null(country_name)) {
        # No country selected: Show global distribution
        ggplot(rice_data, aes(x = country, fill = AxooPopn)) +
          geom_bar(position = "stack") +
          scale_fill_manual(values = axoo_colors, na.translate = TRUE) +
          #theme_minimal() +
          labs(
            title = "Global Distribution of Pathogen Populations",
            x = "Country",
            y = "Sample Count",
            fill = "AxooPopn"
          ) #+
          #theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        # Country selected: Filter data for the selected country
        filtered_data <- rice_data %>% filter(country == country_name)
        
        ggplot(filtered_data, aes(x = AxooPopn, fill = AxooPopn)) +
          geom_bar(position = "stack") +
          scale_fill_manual(values = axoo_colors, na.translate = TRUE) +
          #theme_minimal() +
          labs(
            title = paste("Pathogen Distribution in", country_name),
            x = "Pathogen Population",
            y = "Sample Count",
            fill = "AxooPopn"
          )# +
          #theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
}

shinyApp(ui, server)
