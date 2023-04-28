library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(lwgeom)
library(sf)

# Dashboard
  ## Average stay length, Average Lead Time To Booking, repeat guest percent, cancellation rate
  ## hotel types, table of total guests by resort type
  ## Bookings by country world map
  ## INCLUDE the top 5 countries or something 
  ##***## INCLUDE THE DATA FRAME Size somewhere

# Bookings By Season
  ## bookings by year, cancellation by hotel type
  ## bookings by month, cancellation rate by month,
  ## bookings for each deposit type

# EDA 
  ## iframe for github repo or eda

#reading in our data 
hotel_bookings <- read.csv("../data/hotel_bookings.csv")

#Dashboard header
header <- dashboardHeader(
  title = "Hotel Bookings"
)

# Define UI 
ui <- dashboardPage(
  title = "Hotel Bookings",
  header,
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Bookings By Season", tabName = "bookings", icon = icon("calendar")),
      menuItem("EDA", tabName = "eda", icon = icon("bar-chart")),
      tags$div(
        style = "position: fixed; bottom: 0;",
        
        tags$p(
          "Created by: ", HTML("<br>"), "Dale Campbell",
        ),
        
        tags$a(
          href="https://github.com/campb223", target="_blank",
          icon("github"),
          "View Source on Github"
        )
      )
    )
  ),
  dashboardBody(
    tags$style(
      HTML(
        "
        .skin-purple .main-sidebar {
          background-color: #000000;
        }
        .main-sidebar {
          min-height: 100%;
        }
       .tab-content {
         padding: 10px 0px 0px 10px;
       }
       .dashboard-body {
          overflow-x: auto;
          overflow-y: auto;
          padding: 10px 0px 0px 10px;
       }
       .my-fluid-row {
          display: flex;
          align-items: center;
          margin-top: 20px;
       }
        "
      )
    ),
    tabItems(
      tabItem(tabName = "dashboard", 
              fluidRow(
                column(width = 3, 
                       valueBoxOutput("avg_stay_length_box", width = 12)
                ),
                column(width = 3, 
                       valueBoxOutput("avg_lead_time_box", width = 12)
                ),
                column(width = 3, 
                       valueBoxOutput("repeat_guest_box", width = 12)
                ),
                column(width = 3, 
                       valueBoxOutput("cancellation_rate_box", width = 12)
                )
              ),
              fluidRow(
                column(width = 6,
                       plotlyOutput("hotel_types_pie_chart", height = "200px")
                ),
                column(width = 6,
                       DTOutput("hotel_type_table")
                )
              ),
              fluidRow(class = "my-fluid-row",
                column(width = 12,
                       plotlyOutput("bookings_by_country_plot", height = "450px")
                ),
              )
      ),
      tabItem(tabName = "bookings",
              fluidRow(class = "my-fluid-row",
                       
              ),
              fluidRow(class = "my-fluid-row",
                       column(width = 8,
                              plotlyOutput("month_bar_plot")
                       ),
                       column(width = 4,
                              plotlyOutput("cancellation_by_hotel_type")
                       )
              )
      ),
      tabItem(tabName = "eda",
              fluidRow(
                  column(12 ),
              )
      )
    )
  ),
  skin='purple'
)

server <- function(input, output, session) {
  
  #### FIRST ROW ---------------------------------------------------------
  
  # Calculate the average stay length for Check-Out reservations only
  avg_stay_length <- reactive({
    filtered_bookings <- subset(hotel_bookings, reservation_status == "Check-Out")
    mean(filtered_bookings$stays_in_week_nights + filtered_bookings$stays_in_weekend_nights)
  })
  
  # Render the value boxes
  output$avg_stay_length_box <- renderValueBox({
    valueBox(paste(round(avg_stay_length(), 1), "Days"), "Average Stay", icon = icon("hotel"), color = "yellow")
  })
  
  # Calculate the average lead time to booking
  avg_lead_time <- reactive({
    mean(hotel_bookings$lead_time)
  })
  
  output$avg_lead_time_box <- renderValueBox({
    valueBox(paste(round(avg_lead_time(), 1), "Days"), "Average Booking Lead Time", icon = icon("calendar"), color = "blue")
  })
  
  # Calculate the repeat guest percentage
  repeat_guest_percent <- reactive({
    (sum(hotel_bookings$is_repeated_guest == 1) / nrow(hotel_bookings)) * 100
  })
  
  output$repeat_guest_box <- renderValueBox({
    valueBox(paste(round(repeat_guest_percent(), 1), "%"), "Repeat Guest Percentage", icon = icon("users"), color = "green")
  })
  
  # Calculate the cancellation rate
  cancellation_rate <- reactive({
    (sum(hotel_bookings$is_canceled == 1) / nrow(hotel_bookings)) * 100
  })
  
  output$cancellation_rate_box <- renderValueBox({
    valueBox(paste(round(cancellation_rate(), 1), "%"), "Cancellation Rate", icon = icon("times-circle"), color = "red")
  })
  
  ##### SECOND ROW ------------------------------------------------------
  
  # Create a pie chart of the hotel types
  output$hotel_types_pie_chart <- renderPlotly({
    hotel_types <- hotel_bookings %>%
      count(hotel, sort = TRUE)
    
    hotel_type_labels <- paste(hotel_types$hotel, scales::percent(hotel_types$n / sum(hotel_types$n)))
    
    plot_ly(hotel_types, labels = hotel_type_labels, values = ~n, type = "pie") %>%
      layout(title = "Hotel Types")
  })
  
  # Create a table showing the total number of guests by hotel type
  output$hotel_type_table <- renderDT({
    hotel_type_summary <- hotel_bookings %>%
      group_by(hotel) %>%
      summarise(total_adults = sum(adults),
                total_children = sum(children),
                total_babies = sum(babies)) %>%
      ungroup() %>%
      datatable(rownames = FALSE, options = list(scrollX = TRUE))
    return(hotel_type_summary)
  })
  
  ##### THIRD ROW ------------------------------------------------------------
  
  # Map of Netflix titles by country
  output$bookings_by_country_plot <- renderPlotly({
    
    # Read the world countries geometries
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Summarize the Netflix dataset by country and count the number of occurrences of each country
    hotel_summary <- hotel_bookings %>%
      group_by(country) %>%
      summarise(n = n()) # %>%
    
    # Join the Netflix summary data with the world geometries on both the name and admin columns
    world_data <- world %>%
      st_make_valid() %>%
      left_join(hotel_summary, by = c("iso_a3" = "country")) %>%
      st_as_sf()
    
    # Replace NA values with 0
    world_data$n[is.na(world_data$n)] <- 0
    
    # Calculate the centroids of the country polygons
    world_data$centroid <- st_centroid(world_data)
    
    # Extract the latitude and longitude coordinates
    world_data$lon <- st_coordinates(world_data$centroid)[, 1]
    world_data$lat <- st_coordinates(world_data$centroid)[, 2]
    
    # Create a color palette function
    pal <- colorRampPalette(c("white", "purple"))
    
    # Function for setting the aesthetics of the plot
    my_theme <- function () { 
      theme_bw() + theme(axis.text = element_blank(),
                         axis.title = element_text(size = 14),
                         strip.text = element_text(size = 14),
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank(), 
                         legend.position = "bottom",
                         panel.border = element_blank(), 
                         strip.background = element_rect(fill = 'white', colour = 'white'))
    }
    
    world_map <- ggplot(world_data) +
      geom_sf(aes(fill = n, text = paste0("Total films: ", n)), color = "grey", size = 0.5) +
      scale_fill_gradientn(colours = pal(5), na.value = 'black') + 
      scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
      scale_x_continuous(breaks = c()) +
      labs(title = "Releases By Country", x = NULL, y = NULL, caption = "Releases By Country") +
      my_theme() 
    
    
    ggplotly(world_map)
    
  })
  
  ###### BOOKINGS BY SEASON ----------------------------------------------------
  ## ROW ONE
  
  
  ## ROW TWO
  
  # Create a reactive object that counts the frequency of bookings and cancellations by month
  month_counts <- reactive({
    hotel_bookings %>%
      group_by(arrival_date_month) %>%
      summarize(n_bookings = sum(is_canceled == 0), n_cancellations = sum(is_canceled == 1)) %>%
      arrange(match(arrival_date_month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))
  })
  
  # Create a bar plot of the distribution of bookings and cancellations by month
  output$month_bar_plot <- renderPlotly({
    month_counts_df <- month_counts()
    
    plot_ly(data = month_counts_df, x = ~arrival_date_month, y = ~n_bookings, name = "Bookings", type = "bar") %>%
      add_trace(y = ~n_cancellations, name = "Cancellations", marker = list(color = "red")) %>%
      layout(title = "Bookings and Cancellations by Month", xaxis = list(title = ""), yaxis = list(title = ""))
  })
  
  output$cancellation_by_hotel_type <- renderPlotly({
    
    cancellation_summary <- hotel_bookings %>%
      filter(hotel %in% c("City Hotel", "Resort Hotel")) %>%
      group_by(hotel, is_canceled) %>%
      summarize(cancellation_type_count = n(), total_bookings = n()) %>%
      mutate(percent_cancellation = cancellation_type_count / total_bookings * 100) %>%
      ungroup()
    
    cancellation_summary$total_bookings[1] <- cancellation_summary$cancellation_type_count[1] + cancellation_summary$cancellation_type_count[2]
    cancellation_summary$total_bookings[2] <- cancellation_summary$cancellation_type_count[1] + cancellation_summary$cancellation_type_count[2]
    cancellation_summary$total_bookings[3] <- cancellation_summary$cancellation_type_count[3] + cancellation_summary$cancellation_type_count[4]
    cancellation_summary$total_bookings[4] <- cancellation_summary$cancellation_type_count[3] + cancellation_summary$cancellation_type_count[4]
    
    cancellation_summary$percent_cancellation[1] <- (cancellation_summary$cancellation_type_count[1] / cancellation_summary$total_bookings[1]) * 100
    cancellation_summary$percent_cancellation[2] <- (cancellation_summary$cancellation_type_count[2] / cancellation_summary$total_bookings[2]) * 100
    cancellation_summary$percent_cancellation[3] <- (cancellation_summary$cancellation_type_count[3] / cancellation_summary$total_bookings[3]) * 100
    cancellation_summary$percent_cancellation[4] <- (cancellation_summary$cancellation_type_count[4] / cancellation_summary$total_bookings[4]) * 100
    
    # Create the plot
    cancellation_summary %>%
      ggplot(. ,aes(x = hotel,
                    y = percent_cancellation,
                    label = percent_cancellation,
                    fill = is_canceled)) +
      geom_col(width = 0.5) +
      geom_text(size = 6, position = position_stack(vjust = 0.5),
                colour = "white", fontface = "bold") +
      labs(title ="Percent Cancellations by Hotel Type", x = "Type of Hotel",
           y = "Percentage of bookings (%)") +
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
  })
  
  
}

# Run the app
shinyApp(ui, server)