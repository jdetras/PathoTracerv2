#
# PathoTracer: 
#
#

library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(sf)
library(readr)
library(shinythemes)

## Metrics
total_isolates <- nrow(rice_data)
total_unique_institutes <- n_distinct(rice_data$institute)
total_unique_countries <- n_distinct(rice_data$country)

countries <- st_read("data/latest-data-loading-28May-2024/filtered_world_administrative_boundaries.geojson")

# Map IRBB lines to Xa-genes
irbb_to_xa <- c("IRBB4" = "Xa4", "IRBB5" = "Xa5", "IRBB7" = "Xa7", "IRBB10" = "Xa10", "IRBB13" = "Xa13", "IRBB14" = "Xa14", "IRBB21" = "Xa21")
genes_frequency_data$Xa_gene <- irbb_to_xa[genes_frequency_data$IRBB_Line]
com_xa3_data$Xa_gene <- irbb_to_xa[com_xa3_data$variable]

# Define the color palette for Rgene
rgene_colors <- c(
  "Xa4"  = "#9b5fe0", 
  "Xa5"  = "#16a4d8", 
  "Xa7"  = "#60dbe8", 
  "Xa10" = "#8bd346",
  "Xa13" = "#efdf48", 
  "Xa14" = "#f9a52c", 
  "Xa21" = "#d64e12"
)


# For clean-up
colset <- c("#ffff99","#111E6C", "#0F52BA", "#0000FF", "#FA8072", "#EA3C53", "#CD5C5C", "#B22222", 
           "#FF2400", "#960018",  "#C7EA46",  "#4F7942", "#0B6623", "palegreen", "yellow2", "wheat3")
strainlst <- c("Xoc","AXoo 1","AXoo 2","AXoo 3","AXoo 4","AXoo 5","AXoo 6","AXoo 7","AXoo 8","AXoo 9",
              "AXoo 10","AXoo 11","AXoo 12", "L1 Unresolved","L2 Unresolved","L3 Unresolved")

strain_colors <- setNames(colset, strainlst)

colorblind_palette <- c("#ffff99","#111E6C", "#0F52BA", "#0000FF", "#FA8072", "#EA3C53", "#CD5C5C", "#B22222", 
                        "#FF2400", "#960018",  "#C7EA46",  "#4F7942", "#0B6623", "palegreen", "yellow2", "wheat3")

#define the colors
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

# Data joins
joined_data <- merge(rice_data, recommended_genes_data, by = "country")

# Aggregate data by country and Rgene for the stacked bar chart
#country_data <- recommended_genes_data %>%
country_data <- joined_data %>%
  group_by(country, Rgene) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

# Aggregate data to calculate average effectiveness per country and Xa_gene, and count the number of isolates
country_effectiveness <- com_xa3_data %>%
  group_by(country, Xa_gene) %>%
  summarise(effectiveness = sum(perc, na.rm = TRUE),
            num_isolates = n()) %>%
  ungroup()

# Aggregate data
abundance_popn_data <- rice_data %>%
  group_by(year, country, AxooPopn) %>%
  summarise(num_isolates = n()) %>%
  mutate(percent_abundance = num_isolates / sum(num_isolates) * 100)

#####################
# Define UI
ui <- navbarPage(
  "PathoTracer", 
  id = "select1",  
  theme = shinytheme("flatly"),
  collapsible = T, 
  inverse = T,
  tags$head(tags$style(HTML('.navbar-static-top {background-color: #0e7837;}',
                            '.navbar-default .navbar-nav>.active>a {background-color: #0e7837;}',
                            '.plot-container {
                               border: 2px solid #ddd;
                               border-radius: 5px;
                               padding: 10px;
                               margin-bottom: 20px;
                             }'))),
  selected = "Disease maps",
  navbarMenu("Disease maps", 
             tabPanel("Bacterial Blight"),
             #tabPanel("Fungi"),
              
    ),
  
  tabPanel("Pest maps", "Contact us for more information"),
  
  tabPanel("More information"),
          # tags$div(E
          #   tags$h4("This application was built using R-Shiny. It is made available through the free tier of shinyapps.io."),
          #   tags$h4(
          #     HTML(paste("To use the app, ", tags$span(style="color:red", "select a map to display"), 
          #                ". Then, ", tags$span(style="color:red", "click on a province"), " to display more information.", sep = ""))
           # ),
           #  tags$h3(" "),
           #  tags$h5("Disclaimer of liability"), 
           #  tags$h5("The data contained in this application is for general information only. Any decision you make after viewing the content is your responsibility and is strictly at your own risk."),
           # tags$h3(" "),
           #  #tags$h5(  DT::dataTableOutput(citations)),
           #)
  
  # Metric Cards
  fluidRow(
    tags$head(tags$style('.card {
              box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
              transition: 0.3s;
              width: 100%;
              border-radius: 5px;
              padding: 14px;
              text-align: center;
              margin-bottom: 15px;
            }
            .card:hover {
              box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
            }
            .title {
              font-family: Arial;
              font-size: 20px;
              font-weight: bold;
              margin-bottom: 10px;
            }
            .value {
              font-size: 28px;
              font-family: Arial;
            }')),
    column(4,
           div(class = "card",
               div(class = "title", "Total Isolates"),
               div(class = "value", total_isolates)
           )
    ),
    column(4,
           div(class = "card",
               div(class = "title", "Total Unique Institutes"),
               div(class = "value", total_unique_institutes)
           )
    ),
    column(4,
           div(class = "card",
               div(class = "title", "Total Unique Countries"),
               div(class = "value", total_unique_countries)
           )
    )
  ),
  # Filter 
  sidebarLayout(
    sidebarPanel(class = "sidebar-panel",width=2,
      #selectInput("pathogens", "Select Pathogen", choices = c("Bacteria"), selected = "Bacteria"),
      #selectInput("disease", "Select Disease", choices = c("Bacterial Blight"), selected = "Bacterial Blight"),
      sliderInput("year", "Select Year Range", sep="",
                  min = min(rice_data$year, na.rm = TRUE), 
                  max = max(rice_data$year, na.rm = TRUE), 
                  value = c(min(rice_data$year, na.rm = TRUE), max(rice_data$year, na.rm = TRUE))),
      selectInput("country", "Select Country", 
                 choices = c(sort(unique(rice_data$country)),"All Countries" = "All")
                 #selected = "All"
                  )
    ),
    
    mainPanel(class = "main-panel",width = 10,
      tabsetPanel(
        tabPanel("Global Reports",
          tabsetPanel(
            tabPanel("Global Incidence Plot",
              leafletOutput("all_map")
              #plotOutput("recom_genes")
            ),
            tabPanel("Global Recommendations", 
                     div(class = "plot-container", plotOutput("effectivity_rgenes")),
                     div(class = "plot-container", plotOutput("effectivity_rgene_all_country"))
                     ),
          )
        ),
        
      tabPanel(
        "Per Country",
        tabsetPanel(
            tabPanel("Map", 
                     leafletOutput("map")
            ),
            #tabPanel("Province View", leafletOutput("province_choropleth")),
            tabPanel("Population Structure", 
                     plotOutput("axooPlot"),
                     plotOutput("axoo_population_plot")),
            tabPanel("Data Table", 
                     div(class = "data-table-container", DTOutput("table")),
            ),
            tabPanel("Plots", 
                     plotOutput("area_chart"),  # New plotOutput for area chart
                     plotOutput("province_plot")
            ),
            tabPanel("Per Country Recommendations", 
                     #plotOutput("effectivity_rgenes"),
                     plotOutput("effectivity_rgene_per_country"),
                     #plotOutput("recom_genes")
            )
        )
      )
      
    )
    )
    
    
    ),
  
  #Footer
  #tags$footer(
  #  HTML("2024 | Rice Breeding Initiative <br> International Rice Research Institute"),
  #  style = "background-color: #343a40; color: #ffffff; text-align: center; padding: 10px; position: fixed; bottom: 0; width: 100%;"
  #)
  
)

# Define Server logic
server <- function(input, output) {
  filtered_data <- reactive({
    if (input$country == "All") {
      data<- joined_data %>%
        filter(year >= input$year[1] & year <= input$year[2]) #%>%
        filter(!is.na(latitude) & !is.na(longitude))
    } else {
      data<- joined_data %>%
        filter(year >= input$year[1] & year <= input$year[2],  
               country == input$country
               ) %>%
        filter(!is.na(latitude) & !is.na(longitude))
    }
    data
  })
  
  output$area_chart <- renderPlot({
    area_data <- rice_data %>%
      filter(country == input$country)  %>%
      group_by(year, country, AxooPopn) %>%
      summarise(num_isolates = n()) %>%
      mutate(percent_abundance = num_isolates / sum(num_isolates) * 100)
    
    ggplot(area_data, aes(x = year, y = percent_abundance, fill = AxooPopn)) +
      geom_area(position = "stack") +
      scale_fill_manual(name = "Population ID",
                          values = c("#ffff99","#111E6C", "#0F52BA", "#0000FF", "#FA8072", "#EA3C53", "#CD5C5C", "#B22222", 
                          "#FF2400", "#960018",  "#C7EA46",  "#4F7942", "#0B6623", "palegreen", "yellow2", "wheat3"),
                          label = c("Xoc","AXoo 1","AXoo 2","AXoo 3","AXoo 4","AXoo 5","AXoo 6","AXoo 7","AXoo 8","AXoo 9",
                                    "AXoo 10","AXoo 11","AXoo 12", "L1 Unresolved","L2 Unresolved","L3 Unresolved")
      ) +
      facet_wrap(~country, scales = "free_y", ncol = 3) +
      labs(title = "Southeast Asia:", x = "Year", y = "Abundance (%)", fill = "Population ID") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18))  # Increase font size 
  })
  
  ## plot effectivity_rgenes_Axoo_popn
  output$effectivity_rgenes <-renderPlot(
    ggplot(genes_frequency_data, aes(x = Population, y = Frequency, fill = Xa_gene)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Frequency of Effective Xa Genes per AXoo Population", x = "AXoo Population", y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18))  # Increase font size 
  )
  
  output$effectivity_rgene_per_country <- renderPlot({
    data_effectivity_per_country <-  country_effectiveness %>%
      filter(country == input$country)
     
    ggplot(data_effectivity_per_country, aes(x = country, y  = effectiveness, fill = Xa_gene)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Effectiveness of Xa Genes per Country",
           x = "Country",
           y = "Frequency of Avirulent Isolate",
           caption = "Numbers inside bars represent the number of isolates") +
      #geom_text(aes(label = num_isolates), position = position_dodge(width = 0.9), vjust = -0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18))  # Increase font size 
  })
  
  output$effectivity_rgene_all_country <- renderPlot({
    
    ggplot(country_effectiveness, aes(x = country, y = effectiveness, fill = Xa_gene)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Effectiveness of Xa Genes per Country",
           x = "Country",
           y = "Frequency of Avirulent Isolate",
           caption = "Numbers inside bars represent the number of isolates") +
      #geom_text(aes(label = num_isolates), position = position_dodge(width = 0.9), vjust = -0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18))  # Increase font size 
  })
  
  ## recommended_genes_all
  output$recom_genes <- renderPlot({
    ggplot(country_data, aes(x = country, y = Value, fill = Rgene)) +
      geom_bar(stat = "identity") +
      #theme(axis.text.x = element_text(angle = 45, hjust = 1),
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18)) +  # Increase font size
      labs(title = "Recommended Genes per Country", 
           x = "Country", 
           y = "Percentage") +
      scale_fill_manual(name = "R genes", 
                        values = c("#9b5fe0", "#16a4d8", "#60dbe8", "#8bd346","#efdf48", "#f9a52c", "#d64e12"),
                        label = c("IRBB4"="Xa4", "IRBB5"="Xa5", "IRBB7"="Xa7", "IRBB10"="Xa10",
                                  "IRBB13"="Xa13", "IRBB14"="Xa14", "IRBB21"="Xa21")
                        )
  })
  
  
  
  # map all isolates
  output$all_map <- renderLeaflet({
    if (nrow(joined_data) > 0) {
      leaflet(joined_data) %>%
        #addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = mean(rice_data$longitude, na.rm = TRUE), lat = mean(rice_data$latitude, na.rm = TRUE), zoom = 3) %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          color = ~colorFactor(palette = strain_colors, domain = rice_data$AxooPopn)(AxooPopn),
          fillColor = ~colorFactor(palette = strain_colors, domain = rice_data$AxooPopn)(AxooPopn),
          popup = ~paste("Isolate Name:", isolatename,"<br/>Axoo Population:", AxooPopn, "<br/>District/Town:", district, "<br/>Province:", province, "<br/>Country:", country ),
          radius = 3 
         ) %>%
      addLegend(
        position = "bottomright",
        pal = colorFactor(palette = strain_colors, domain = rice_data$AxooPopn),
        values = ~AxooPopn,
        title = "Pathogen Population",
        opacity = 1,
        layerId = "legend"
      )
    } else {
      leaflet() %>%
        #addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 0, lat = 0, zoom = 2) %>%
        addPopups(lng = 0, lat = 0, "No data available for selected filters")
    }
  })
  
  
  ## recommended_genes_all
  output$recom_genes_per_country <- renderPlot({
    ggplot(country_data, aes(x = country, y = Value, fill = Rgene)) +
      geom_bar(stat = "identity") +
      #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18)) +  # Increase font size
      labs(title = "Recommended Genes per Country", 
           x = "Country", 
           y = "Value") +
      scale_fill_manual(values = rgene_colors)
  })
  
  
  
  # Plot for Axoo_population
  output$axoo_population_plot <- renderPlot({
    axoo_data <- filtered_data()
    
    ggplot(axoo_data, aes(x = AxooPopn, fill = AxooPopn)) +
      geom_bar() +
      #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18)) +  # Increase font size
      labs(title = "Total Isolates by pathogen Population", x = "Axoo_population", y = "Count") +
      scale_fill_manual(values = axoo_colors)
  })
  
  # AxooPlot (Percentage)
  output$axooPlot <- renderPlot({
    data_processed <- filtered_data() %>%
      filter(!is.na(AxooPopn)) %>%
      group_by(country, AxooPopn) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = count / sum(count) * 100)
    
    
    ggplot(data_processed, aes(y = country, x = percentage, fill = AxooPopn)) +
      geom_bar(stat = "identity") +
      #theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18)) +  # Increase font size
      labs(title = "Axoo Population Distribution", y = "Country", x = "Percentage (%)") + 
      scale_fill_manual(values = axoo_colors)
  })
  
  # Data tables
  output$table <- renderDT({
    data <- filtered_data() %>%
      select(samplecode, isolatename, year, district ,province, country, institute, sampletype, lineage, AxooPopn )  # Adjust according to actual column names
    
    datatable(data, options = list(autoWidth = TRUE, pageLength = 10))
  })
  
  #Province plot
  output$province_plot <- renderPlot({
    province_data <- filtered_data() %>%
      group_by(province) %>%
      summarise(count = n())
    
    ggplot(province_data, aes(x = reorder(province, -count), y = count, fill = province)) +
      geom_bar(stat = "identity") +
      #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18)) +  # Increase font size
      labs(title = "Total Isolates by Province", x = "Province", y = "Count")
  })

  
  # render the map 
  # markers are isolates
  output$map <- renderLeaflet({
    map_data <- filtered_data()
    
    # check if data$AxooPopn good
    #print(unique(map_data$AxooPopn))
    
    if (nrow(map_data) > 0) {
      leaflet(map_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = mean(map_data$longitude, na.rm = TRUE), lat = mean(map_data$latitude, na.rm = TRUE), zoom = 6) %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          popup = ~paste("Isolate Name:", isolatename,"<br/>Axoo Population:", AxooPopn, "<br/>District/Town:", district, "<br/>Province:", province, "<br/>Country:", country ),
          color = ~colorFactor(palette = strain_colors, domain = rice_data$AxooPopn)(AxooPopn),
          fillColor = ~colorFactor(palette = strain_colors, domain = rice_data$AxooPopn)(AxooPopn),
          fillOpacity = 0.8,
          radius = 7
          #clusterOptions = markerClusterOptions()
        )  %>%
       addLegend("bottomright", pal = colorFactor(palette = strain_colors, domain = map_data$AxooPopn), 
                    values = ~AxooPopn, title = "Pathogen Populations",
                    opacity = 1)
      
      
    } else {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 0, lat = 0, zoom = 2) %>%
        addPopups(lng = 0, lat = 0, "No data available for selected filters")
    }
  })
  
  # Calculate isolates per country
  output$isolates_per_country <- renderTable({
    metrics_data <- filtered_data()
    isolates_count <- metrics_data %>%
      group_by(country) %>%
      summarise(Isolates = n())
    
    isolates_count
  })
  
  # Calculate predominant Axoo_population per country, province
  output$predominant_axoo_population <- renderTable({
    metrics_data <- filtered_data()
    axoo_population <- metrics_data %>%
      group_by(province, country, AxooPopn) %>%
      summarise(Count = n()) %>%
      group_by(province, country) %>%
      top_n(1, Count) %>%
      arrange(desc(Count))
    
    axoo_population
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)