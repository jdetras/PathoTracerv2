library(shiny)
library(shinythemes)
library(slickR)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(sf)
library(readr)


###### Data Loading ######
# replace with data fetch from database
rice_data <- read.csv("data/latest-data-loading-28May-2024/all_rice_bb_data.csv")
rice_data$year <- as.numeric(rice_data$year)
rice_data$AxooPopn <- as.character(rice_data$AxooPopn)
rice_data$AxooPopn <- as.character(rice_data$AxooPopn)
rice_data$AxooPopn <- trimws(rice_data$AxooPopn)   # Remove any leading/trailing whitespace
recommended_genes_data <- read_csv("data/recommended_genes.csv") # based on prior data from DAle
#citations <- readRDS("data/latest-data-loading-28May-2024/package_citations.rds")
genes_frequency_data <- read.csv('data/latest-data-loading-28May-2024/293_IRBBfreq.csv')
com_xa3_data <- read.csv('data/latest-data-loading-28May-2024/com_xa3.csv')
recommended_variety<- read.csv('data/BLB-varieties-recom.csv')

# maps
#world_geojson <- st_read("data/maps/countries.geo.json")
world_geojson <- st_read("data/maps/countries.geojson")

# get all countries 
countries_of_interest <- unique(rice_data$country)

# Calculate Metrices
total_isolates <- nrow(rice_data)
total_unique_institutes <- n_distinct(rice_data$institute)
total_unique_countries <- n_distinct(rice_data$country)


# Binding Aggregations
irbb_to_xa <- c("IRBB4" = "Xa4", "IRBB5" = "Xa5", "IRBB7" = "Xa7", "IRBB10" = "Xa10", "IRBB13" = "Xa13", "IRBB14" = "Xa14", "IRBB21" = "Xa21")
genes_frequency_data$Xa_gene <- irbb_to_xa[genes_frequency_data$IRBB_Line]
com_xa3_data$Xa_gene <- irbb_to_xa[com_xa3_data$variable]

# Define the color palette for R-gene recommendation
# using Ian's color scheme
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
# use colorBrewer
colset <- c("#ffff99","#111E6C", "#0F52BA", "#0000FF", "#FA8072", "#EA3C53", "#CD5C5C", "#B22222", 
            "#FF2400", "#960018",  "#C7EA46",  "#4F7942", "#0B6623", "palegreen", "yellow2", "wheat3")
strainlst <- c("Xoc","AXoo 1","AXoo 2","AXoo 3","AXoo 4","AXoo 5","AXoo 6","AXoo 7","AXoo 8","AXoo 9",
               "AXoo 10","AXoo 11","AXoo 12", "L1 Unresolved","L2 Unresolved","L3 Unresolved")
strain_colors <- setNames(colset, strainlst)


# define the colors
# used Ian's color scheme
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

##### Data Aggregations #####
# Join two tables rice_data and gene_recommendations per pathotype population
joined_data <- merge(rice_data, recommended_genes_data, by = "country")

# Aggregate data by country and Rgene for the stacked bar chart
country_data <- joined_data %>%
  group_by(country, Rgene) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

# Aggregate data to calculate average effectiveness per country and Xa_gene, 
# and count the number of isolates
country_effectiveness <- com_xa3_data %>%
  group_by(country, Xa_gene) %>%
  summarise(effectiveness = sum(perc, na.rm = TRUE),
            num_isolates = n()) %>%
  ungroup()

# Aggregate data by year, country and pathogen popn
# calculate percentage
abundance_popn_data <- rice_data %>%
  group_by(year, country, AxooPopn) %>%
  summarise(num_isolates = n()) %>%
  mutate(percent_abundance = num_isolates / sum(num_isolates) * 100)

# Test aggregation
province_popn <- rice_data %>%
  group_by(country, province, AxooPopn)

# For debugging...
# Function to filter subset.txt based on [+] for a given IRBB_Line
#filter_irbb_line <- function(irbb_line) {
#  filtered_data <- recommended_variety %>%
#    filter(get(irbb_line) == "[+]")
#  return(filtered_data)
#}

# Function to get cultivars for each population based on their IRBB_Lines
#get_recommended_cultivars <- function(population, irbb_lines) {
#  selected_cultivars <- list()
#  for (irbb_line in irbb_lines) {
#    cultivars <- filter_irbb_line(irbb_line)
#    selected_cultivars[[irbb_line]] <- cultivars$Cultivar
#  }
#  return(selected_cultivars)
#}


##### User Interface #####
ui <- navbarPage(
  #theme = custom_theme,
  title = tags$div(
    tags$img(src = "pathoTracer_logo2.png",  height = "70px", style = "margin-right: 20px; float: right; padding-bottom: 20px;"),
    #"PathoTracer"
  ),
  theme = shinytheme("cosmo"),  # Use the appropriate Bootstrap theme
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.google.com/share?selection.family=Merriweather:ital,wght@0,300;0,400;0,700;0,900;1,300;1,400;1,700;1,900|Roboto+Serif:ital,opsz,wght@0,8..144,100..900;1,8..144,100..900|Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  navbarMenu("Home",
      tabPanel("About the Tool",
          fluidRow(style="background-color: #4CAF50;",
             column(12,
                    tags$div(
                      style = "position: relative; text-align: center; color: white;",
                      tags$img(src = "bacterial_blight_crop.jpg", style = "width: 100%;padding:0;"),
                      tags$div(
                        style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                        tags$h1("PathoTracer", class="banner-header", style = "color: white; font-size: 4em; font-weight: bold;"),
                        tags$h3("An informed-decision platform to reduce the risk of rice diseases", style = "font-size: 35px; font-weight: bold;")
                      )
                    )
             )
           ),

          ## About the tool
          fluidRow(
             column(12, style = "background-color: #4CAF50; color: white; padding: 10px 0;",
                    tags$h2("ABOUT THE TOOL", style = "text-align: center; font-weight: bold;")
             ),
             column(12, class="banner-text", style ="font-family: Poppins ;font-size: 25px; padding: 50px 120px; text-align: justify; text-justify: inter-word; ",
                    tags$p(tags$b("PathoTracer"), "is a decision support system that integrates early-season pathogen diagnostics and disease resistance profiles intended for use by public and private enterprises in accurately defining breeding priorities and in implementing coordinated actions to manage crop diseases in real-time.")
             )
           ),
          
          ## Testimonies 
          fluidRow(
            column(12, style = "padding: 20px 0;",
                   tags$h2("WHAT OUR PARTNERS ARE SAYING", style = "background-color: #4CAF50; color: white; padding: 10px 0; text-align: center; font-weight: bold; margin-bottom: 20px;"),
                   div(class="slick-carousel", slickROutput("testimonials_carousel", width = "90%"))
            )
          )
      ),
      tabPanel(tags$a(href="https://app.smartsheet.com/b/publish?EQBCT=7fd054ee4696420a91bd4a05e14f6fe0", "Dashboard"))
      
  ),

  navbarMenu("Disease maps", 
             tabPanel("Bacterial Blight", 
                      card(style="background-color: #45a049; color: white;",
                          div(style="font-family:'Poppins';font-size:15px;",
                              tags$p(tags$b("Bacterial Blight"), "is caused primarily by Xanthomonas oryzae pv. oryzae (Xoo). Asian strains of Xoo were found to have 3 major lineages which are further divided into 12 subpopulations based on 22,115 genome-wide SNP data. Navigate the Map tab below to see the distribution of each Asian Xoo.")
                          )
                      ),
                      fluidRow(
                        # Metric Cards
                        fluidRow( style="margin-left: 10px; margin-right: 10px;",
                          column(4,
                                 div(class = "card",style="background-color: #ececa3;height:15vh;",
                                     div(class = "title", "Total Genotyped Samples"),
                                     div(class = "value", total_isolates)
                                 )
                          ),
                          column(4,
                                 div(class = "card",style="background-color: #ececa3;height:15vh;",
                                     div(class = "title", "Participating Institutes"),
                                     div(class = "value", total_unique_institutes)
                                 )
                          ),
                          column(4,
                                 div(class = "card",style="background-color: #ececa3;height:15vh;",
                                     div(class = "title", "Participating Countries"),
                                     div(class = "value", total_unique_countries)
                                 )
                          )
                        ) #end of metrics card
                      ),
                      card( class="report", style="height: 80vh; border: 1px solid #ddd; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); padding: 20px; background-color: #fff; margin: 20px 0;",
                        tabsetPanel(
                          tabPanel("Global Reports",
                                   tabsetPanel(
                                     #tabPanel("Test Plot",
                                    #          leafletOutput("test_map")
                                     #),
                                     tabPanel("Global Incidence Plot",
                                              leafletOutput("all_map", height = "80vh")
                                              
                                    ),
                                     tabPanel("Global Recommendations", 
                                              card(
                                                div(class = "plot-container", plotOutput("effectivity_rgenes", height = "40vh")
                                                )
                                              ),
                  
                                              card(
                                                #div(class = "plot-container", plotOutput("effectivity_rgene_all_country", height = "40vh"))
                                              ),
                                      )
                                     ),
          
                          ),
                          tabPanel("Per Country",
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
                                                              choices = c(sort(unique(rice_data$country)))
                                                              #choices = c(sort(unique(rice_data$country)),"All Countries" = "All")
                                                              #selected = "All"
                                                  )
                                     ),
                                     
                                     mainPanel(class = "main-panel",width = 10,
                                               tabsetPanel(
                                                 tabPanel("Map", leafletOutput("map", height = "80vh")),
                                                 tabPanel("Pathogen Population Summary",
                                                          plotOutput("axooPlot", height = "40vh"),
                                                          plotOutput("axoo_population_plot", height = "40vh")
                                                 ),
                                                 tabPanel("Effective R-genes",
                                                          card(
                                                            plotOutput("effectivity_rgene_per_country", height = "40vh")
                                                          ),
                                                          card(style="background-color: #d9ecda;",
                                                               div(
                                                                 tags$p("Estimation of effective Xa genes for a particular country were based on 293 genome-sequenced Asian Xoo isolates with pathotype data using near-isogenic lines IRBB4, IRBB5, IRBB7, IRBB10, IRBB13, IRBB14, and IRBB21. The isolates were grouped into their corresponding Asian Xoo subpopulations, and the proportion of resistant IRBB lines were used to estimate for the genotyped samples submitted by each country.")
                                                               )
                                                          )
                                                 ),
                                                 tabPanel("Genotyped Samples Table", 
                                                          div(class = "data-table-container", DTOutput("table", height = "80vh"))
                                                 ),
                                                 tabPanel("Sample Submission Summary", 
                                                          plotOutput("area_chart", height = "80vh")
                                                 ),
                                                 tabPanel("Recommended varieties", 
                                                          #div(class = "data-table-container", DTOutput("population_tables", height = "80vh"))
                                                          "site under construction"
                                                 )
                                               )      
                                     ) # end of mainpanel
                                   ) #end of sidebarLayout 
                                   
                          ) #end of per country
                        ),
                      ),
             ), # end of tabPanel: Bacterial Blight
             
             tabPanel("Rice Blast", "Genotyping markers for blast is now available. To genotype your samples, please submit your samples using this ",tags$a(href="https://app.smartsheet.com/b/publish?EQBCT=7fd054ee4696420a91bd4a05e14f6fe0", "link")),
             tabPanel("Others", "Site is under construction")
  ),
  
  tabPanel("News",
           fluidRow(
             column(12,style="text-align:center;",
                    tags$h2("Our Impact", style = "background-color: #4CAF50; color: white; padding: 10px 0; text-align: center; font-weight: bold; margin-bottom: 10px;"),
                    div(class="slick-carousel-news", slickROutput("news_carousel", width = "90%"))
             )
           ),
           fluidRow(
             column(12,
                    tags$h2("Upcoming Events:", style = "background-color: #4CAF50; color: white; padding: 10px 0; text-align: center; font-weight: bold; margin-bottom: 20px;"),
                    #div(class="slick-carousel-news", slickROutput("events_carousel", width = "90%"))
                    card(style="align: center;",
                      div(
                        style = "padding: 10px 60px; border: 1px solid #ddd; margin-bottom: 20px;",
                        tags$p(tags$i("\"Hands-On Training on Biotic Stress Resistance Evaluation\""), style="font-size:20px;"),
                        tags$p(tags$a(href="https://education.irri.org/technology-transfer/hands-on-training-on-biotic-stress-resistance-evaluation/", "See more"), style="font-color: #880808;")
                      )
                    )
             )
             #column(6,style="text-align:right;",
             #       tags$h2("Announcements:", style = "background-color: #4CAF50; color: white; padding: 10px 0; text-align: center; font-weight: bold; margin-bottom: 20px;"),
             #       #div(class="slick-carousel-news", slickROutput("news_carousel", width = "90%"))
             #)
           )
  ),
  tabPanel("Contact Us",
           fluidRow(
             column(12,
                    tags$h2(tags$b(tags$u("Contact Us"))),
                    #tags$p(tags$b("Under construction."),"This section will contain contact information and a contact form for users to reach out."),
                    tags$p(""),
                    tags$h4(tags$b("Dr. Van Schepler-Luu")),
                    #tags$p("Group leader/Scientist II: Plant Pathology and Host Plant Resistance\n",
                    #        "Rice Breeding Innovation Department \n", 
                    #        "International Rice Research Institute (IRRI) \n",
                    #        "Consultative Group for International Agricultural Research (CGIAR) \n",
                    #        "Los Baños, Laguna 4031, Philippines \n",
                    #        "Phone: +63 (2) 8580 5600 ext. 2743 \n",
                    #        "Email: v.scheplerluu@irri.org")
                    tags$p(tags$b("Group leader/Scientist II: Plant Pathology and Host Plant Resistance")),
                    tags$p("Rice Breeding Innovation Department (RBI)"),
                    tags$p("International Rice Research Institute (IRRI)"),
                    tags$p("Los Baños, Laguna 4031, Philippines"),
                    tags$p("Phone: +63 (2) 8580 5600 ext. 2743"),
                    tags$p("Email: v.scheplerluu@irri.org")
                    
             )
           )
  ),
  
  #Footer
  #tags$footer(
  #  HTML("2024 | Rice Breeding Initiative <br> International Rice Research Institute"),
  #  class = "footer", style= "color: #ffffff; text-align: center; padding: 10px; position: absolute; bottom: 0; width: 100%;"
  #)
  
)

server <- function(input, output) {
  ## testimonies
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
  
  output$events_carousel <- renderSlickR({
    events <- list(
      div(
        style = "padding: 10px 60px; border: 1px solid #ddd; margin-bottom: 20px;",
        tags$p(tags$i("\"Hands-On Training on Biotic Stress Resistance Evaluation\""), style="font-size:30px;"),
        tags$p(tags$a(href="https://education.irri.org/technology-transfer/hands-on-training-on-biotic-stress-resistance-evaluation/", "See more"), style="font-color: #880808;")
      )
    )
    slickR(events)
  })
  
  filtered_data <- reactive({
    if (input$country == "All") {
      data<- joined_data %>%
        filter(year >= input$year[1] & year <= input$year[2]) %>%
        filter(!is.na(latitude) & !is.na(longitude))
    } else {
      data<- joined_data %>%
        filter( year >= input$year[1] & year <= input$year[2],  
          country == input$country
        ) %>%
        filter(!is.na(latitude) & !is.na(longitude))
    }
    data
  })
  
  ## for testing only. not for production
  ## subject to code refactor
  output$test_map <- renderLeaflet({
    #outline<- filtered_data[chull(latitude, longitude)]
    
    leaflet(filtered_data) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Positron (minimal)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
      # Overlay groups
      addCircles(
        ~ longitude,
        ~ latitude,
        ~ 10 ^ mag / 5,
        stroke = FALSE,
        group = "Quakes",
        fillColor = "tomato"
      ) %>%
      addPolygons(
        data = rice_data,
        lng = ~ longitude,
        lat = ~ latitude,
        fill = FALSE,
        weight = 2,
        color = "#FFFFCC",
        group = "Outline"
      ) %>%
      #addTiles() %>%
      addLayersControl(
        baseGroups = c(
          "OSM (default)",
          "Positron (minimal)",
          "World Imagery (satellite)"
        ),
        overlayGroups = c("Quakes", "Outline"),
        options = layersControlOptions(collapsed = TRUE)
      )
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPopups(lng = 0, lat = 0, "No data available for selected filters")
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
            plot.title = element_text(size = 18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size=16)
            )  # Increase font size 
  })
  
  filtered_rgenes_selected <- reactive({
    filtered_rgenes_data <- genes_frequency_data %>%
                            filter(Population == input$Population)
    filtered_rgenes_data
  })
  
  ## plot effectivity_rgenes_Axoo_popn
  output$effectivity_rgenes <-renderPlot({
    #data <- filtered_rgenes_selected()
    data <-genes_frequency_data
    
    ggplot(data, aes(x = Population, y = Frequency, fill = Xa_gene)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Frequency of effective xa-genes per AXoo population", x = "AXoo Population", y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 14),  # Increase font size
            axis.title.y = element_text(size = 14),  # Increase font size
            legend.title = element_text(size = 18),
            legend.text = element_text(size=16),
            plot.title = element_text(size = 18)) +  # Increase font size 
      scale_fill_manual(name = "R genes", 
                        values = c("#9b5fe0", "#16a4d8", "#60dbe8", "#8bd346","#efdf48", "#f9a52c", "#d64e12"),
                        label = c("IRBB4"="Xa4", "IRBB5"="Xa5", "IRBB7"="Xa7", "IRBB10"="Xa10",
                                  "IRBB13"="Xa13", "IRBB14"="Xa14", "IRBB21"="Xa21")
      )
  })
  
  output$effectivity_rgene_per_country <- renderPlot({
    data_effectivity_per_country <-  country_effectiveness %>%
      filter(country == input$country)
    
    
    ggplot(data_effectivity_per_country, aes(x = country, y  = effectiveness, fill = Xa_gene)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Effectiveness of xa-genes per country",
           x = "Country",
           y = "Frequency of Avirulent Isolate",
           caption = "") +
      #geom_text(aes(label = num_isolates), position = position_dodge(width = 0.9), vjust = -0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 14),  # Increase font size
            axis.title.y = element_text(size = 14),  # Increase font size
            plot.title = element_text(size = 18), # Increase font size 
            legend.title = element_text(size = 18),
            legend.text = element_text(size=16)
      ) +  # Increase font size 
      scale_fill_manual(name = "R genes", 
                        values = c("#9b5fe0", "#16a4d8", "#60dbe8", "#8bd346","#efdf48", "#f9a52c", "#d64e12"),
                        label = c("IRBB4"="Xa4", "IRBB5"="Xa5", "IRBB7"="Xa7", "IRBB10"="Xa10",
                                  "IRBB13"="Xa13", "IRBB14"="Xa14", "IRBB21"="Xa21")
      )
  })
  
  output$effectivity_rgene_all_country <- renderPlot({
    
    
    ggplot(country_effectiveness, aes(x = country, y = effectiveness, fill = Xa_gene)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Effectiveness of Xa Genes per Country",
           x = "Country",
           y = "Frequency of Avirulent Isolate",
           caption = "Numbers inside bars represent the number of samples") +
      #geom_text(aes(label = num_isolates), position = position_dodge(width = 0.9), vjust = -0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14),  # Increase font size
            axis.text.y = element_text(size = 14),  # Increase font size
            axis.title.x = element_text(size = 16),  # Increase font size
            axis.title.y = element_text(size = 16),  # Increase font size
            plot.title = element_text(size = 18), 
            legend.title = element_text(size = 18),
            legend.text = element_text(size=16))
            +  # Increase font size 
      scale_fill_manual(name = "R genes", 
                        values = c("#9b5fe0", "#16a4d8", "#60dbe8", "#8bd346","#efdf48", "#f9a52c", "#d64e12"),
                        label = c("IRBB4"="Xa4", "IRBB5"="Xa5", "IRBB7"="Xa7", "IRBB10"="Xa10",
                                  "IRBB13"="Xa13", "IRBB14"="Xa14", "IRBB21"="Xa21")
      )
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
            plot.title = element_text(size = 18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size=16)
            ) +  # Increase font size
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
    subset_countries <- world_geojson %>%
      filter(ADMIN %in% countries_of_interest)
    
    if (nrow(joined_data) > 0) {
      leaflet(joined_data) %>%
        #addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = mean(rice_data$longitude, na.rm = TRUE), lat = mean(rice_data$latitude, na.rm = TRUE), zoom = 3) %>%
        
        ## add countries
        addPolygons(
          data = subset_countries,
          color = "#444444", 
          weight = 1, 
          opacity = 1,
          fillOpacity = 0.7,
          #fillColor = "#F7AF13",
          fillColor = "#e88474",
          #fillColor = ~factor(ADMIN),
          popup = ~ADMIN
        )%>%
        
        ## add isolates
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          color = ~colorFactor(palette = strain_colors, domain = rice_data$AxooPopn)(AxooPopn),
          fillColor = ~colorFactor(palette = strain_colors, domain = rice_data$AxooPopn)(AxooPopn),
          popup = ~paste("Isolate Name:", isolatename,"<br/>Axoo Population:", AxooPopn, "<br/>District/Town:", district, "<br/>Province:", province, "<br/>Country:", country ),
          radius = 3 
        ) %>%
        addLegend(
          position = "topright",
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
            plot.title = element_text(size = 18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size=16)
            ) +  # Increase font size
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
            plot.title = element_text(size = 18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size=16)
            ) +  # Increase font size
      labs(title = "Total samples by pathogen Population", x = "Axoo population", y = "Count") +
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
            plot.title = element_text(size = 18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size=16)
            ) +  # Increase font size
      labs(title = "Axoo population distribution", y = "Country", x = "Percent Distribution(%)") + 
      scale_fill_manual(values = axoo_colors)
  })
  
  # Data tables
  output$table <- renderDT({
    data <- filtered_data() %>%
      select(samplecode, isolatename, year, district ,province, country, institute, sampletype, lineage, AxooPopn )  # Adjust according to actual column names
    #data %>% rename(isolatename = "Isolate Name", samplecode = "Sample Code")
    
    datatable(data, options = list(autoWidth = TRUE, pageLength = 10))
  })
  
  # Data tables
  output$all_samples <- renderDT({
    data <- rice_data() %>%
      select(samplecode, isolatename, year, district ,province, country, institute, sampletype, lineage, AxooPopn )  # Adjust according to actual column names
    data %>% rename(isolatename = "Isolate Name", samplecode = "Sample Code")
    
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
            plot.title = element_text(size = 18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size=16)
            ) +  # Increase font size
      labs(title = "Total Samples by Province", x = "Province", y = "Count")
  })
  
  
  # render the map 
  # markers are isolates
  output$map <- renderLeaflet({
    subset_countries <- world_geojson %>%
      filter(ADMIN %in% c(input$country))
    map_data <- filtered_data()
    
    
    if (nrow(map_data) > 0) {
      leaflet(map_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = mean(map_data$longitude, na.rm = TRUE), lat = mean(map_data$latitude, na.rm = TRUE), zoom = 6) %>%
        addPolygons(
          data = subset_countries,
          color = "#444444", 
          weight = 1, 
          opacity = 1,
          fillOpacity = 0.7,
          fillColor = "#e88474",
          popup = ~ADMIN
        )%>%
        addCircleMarkers(
          data = map_data,
          lng = ~longitude, lat = ~latitude,
          popup = ~paste("Isolate Name:", isolatename,"<br/>Axoo Population:", AxooPopn, "<br/>District/Town:", district, "<br/>Province:", province, "<br/>Country:", country ),
          color = ~colorFactor(palette = strain_colors, domain = rice_data$AxooPopn)(AxooPopn),
          fillColor = ~colorFactor(palette = strain_colors, domain = rice_data$AxooPopn)(AxooPopn),
          fillOpacity = 1,
          radius = 7
        )  %>%
        addLegend(position = "topright", pal = colorFactor(palette = strain_colors, domain = rice_data$AxooPopn), 
                  values = ~AxooPopn, title = "Pathogen population",
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
  
  # Calculate isolates per country
  #output$isolates_per_province <- renderDT({
  #  provinceData <- filtered_data()
  #  isolates_province <- provinceData %>%
  #    group_by(province) %>%
  #    summarise(Isolates = n())
  #    arrange(desc(Isolates))
  #})
  
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
  
  output$population_tables <- renderTable({
    population_list <- unique(genes_frequency_data$Population)
    
    # Create data tables for each population
    lapply(population_list, function(population) {
      irbb_lines <- genes_frequency_data %>%
        filter(Population == population) %>%
        pull(IRBB_Line)
      
      recommended_cultivars <- get_recommended_cultivars(population, irbb_lines)
      
      output_table <- do.call(rbind, lapply(names(recommended_cultivars), function(line) {
        data.frame(
          IRBB_Line = line,
          Cultivars = paste(recommended_cultivars[[line]], collapse = ", ")
        )
      }))
      
      datatable(output_table, options = list(autoWidth = TRUE, pageLength = 10))
      #data_table <- datatable(output_table)
      
      # Render DataTable for each population
      #tagList(
      #  h3(paste("Recommended Cultivars for Population:", population)),
      ##  data_table
      #)
    })
  })
  
}

shinyApp(ui = ui, server = server)
