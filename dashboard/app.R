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
library(reactable)
library(htmltools)

#reading in our data 
hotel_bookings <- read.csv("hotel_bookings.csv")

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
      menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Bookings", tabName = "bookings", icon = icon("calendar")),
      menuItem("Pricing", tabName = "pricing", icon = icon("dollar")),
      tags$div(
        style = "position: fixed; bottom: 0;",
        
        tags$p(
          "Created by: ", HTML("<br>"), "Dale Campbell",
        ),
        
        tags$a(
          href="https://github.com/campb223/hotel_bookings", target="_blank",
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
          background-color: #f2f2f2;
       }
       .content-wrapper {
       background-color: #f2f2f2;
       }
       .my-fluid-row {
          display: flex;
          align-items: center;
          margin-top: 20px;
       }
       .my-fluid-row2 {
          display: flex;
          align-items: center;
       }
       .my-fluid-row3 {
          font-size: 16px;
          padding: 0px 30px 0px 30px;
       }
        "
      )
    ),
    tabItems(
      tabItem(tabName = "dashboard", class="dashboard-body",
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
              fluidRow(class = "my-fluid-row2",
                column(width = 3,
                       reactableOutput("dataset_exp")
                ),
                column(width = 6, 
                       plotlyOutput("hotel_type_table", height = "225px")
                ),
                column(width = 3,
                      plotlyOutput("hotel_types_pie_chart", height = "225px", width = "100%")
                ),
              ),
              fluidRow(class = "my-fluid-row",
                column(width = 9,
                       plotlyOutput("bookings_by_country_plot", height = "400px")
                ),
                column(width = 3,
                       reactableOutput("top_countries_table", height = "400px")
                )
              )
      ),
      tabItem(tabName = "bookings",
              fluidRow(class = "my-fluid-row2",
                       column(width = 8,
                              plotlyOutput("month_bar_plot", height="700px")
                       ), 
                       column(width = 4,
                         fluidRow(class = "my-fluid-row2",
                             column(width = 12,
                                    plotlyOutput("cancellation_by_hotel_type", height = "320px")
                             ),
                         ),
                           fluidRow(class = "my-fluid-row",
                             column(width = 12,
                                    plotlyOutput("dist_chan_counts", height = "380px")
                             )
                           ),
                         ),
                       )
              
      ),
      tabItem(tabName = "pricing",
              fluidRow(class = "my-fluid-row",
                       column(width = 12,
                              plotlyOutput("cost_bar_plot", height = "350px")
                       )
              ),
              fluidRow(class = "my-fluid-row",
                       column(width = 12,
                              plotlyOutput("adr_per_dist", height = "350px")
                       )
              ),
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
  
  # Create the reactable
  output$dataset_exp <- renderReactable({
    reactable(
      data = data.frame(
        Dataset = '<a href="https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand">Kaggle</a>',
        Features = "32",
        Entries = "119,390"
      ),
      columns = list(
        Dataset = colDef(name = "Dataset", html = TRUE),
        Features = colDef(name = "Features"),
        Entries = colDef(name = "Entries")
      ),
      style = list(
        backgroundColor = "#f2f2f2",
        fontSize = "16px"
      ),
      pagination = FALSE
    )
  })
  
  # Create the plot
  output$hotel_type_table <- renderPlotly({
    
    # Create a data frame with the hotel types and guest counts
    hotel_type_table <- data.frame(
      hotel = c("City Hotel", "Resort Hotel"),
      Adults = c(146838, 74798),
      Children = c(7248, 5155),
      Babies = c(392, 557)
    )
    
    # Add a new column that combines Children and Babies
    hotel_type_table$Kids <- hotel_type_table$Children + hotel_type_table$Babies
    
    # Reshape the data to long format
    hotel_type_table_long <- tidyr::pivot_longer(hotel_type_table, cols = c("Adults", "Kids"), names_to = "guest_type", values_to = "count")
    
    # Define the fill colors for the plot
    fill_colors <- c("lightgreen", "purple")
    
    # Create the plot
    p <- ggplot(hotel_type_table_long, aes(x = count/sum(count), y = hotel, fill = guest_type)) +
      geom_col(width = 0.5) +
      labs(title = "Hotel Guest Counts (%)", x = "", y = "") +
      scale_x_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = fill_colors, labels = c("Adults", "Kids")) +
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), # plot.background = element_rect(fill = "#f2f2f2"), 
            legend.background = element_rect(fill = "#f2f2f2")
      ) +
      labs(fill = "Guest Type") +
      geom_text(aes(x = count/sum(count), y = hotel, label = scales::percent(count/sum(count), accuracy = 1)), 
                position = position_stack(vjust = 0.75), size = 4, color = "black", fontface = "bold")
    
    # Turn off hover
    ggplotly(p, tooltip = c("label")) %>%
      layout(hovermode = FALSE, 
             plot_bgcolor = "#f2f2f2",
             paper_bgcolor = "#f2f2f2"
      )
  })
  
  # Create a pie chart of the hotel types
  output$hotel_types_pie_chart <- renderPlotly({
    hotel_types <- hotel_bookings %>%
      count(hotel, sort = TRUE)
    
    hotel_type_labels <- paste(hotel_types$hotel)
    hotel_type_perc <- paste(hotel_types$hotel, gsub("^(\\d+)(\\..*)?$", "\\1\\2", scales::percent(hotel_types$n / sum(hotel_types$n), accuracy = 0.1)))
    
    plot_ly(hotel_types, labels = hotel_type_labels, values = ~n, type = "pie",
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~hotel_type_perc
            ) %>%
      layout(
        title = "Hotel Types",
        plot_bgcolor = "#f2f2f2",
        paper_bgcolor = "#f2f2f2", 
        showlegend = FALSE,
        margin = list(l = 20, r = 20, t = 50, b = 0)
      )
  })
  
  ##### THIRD ROW ------------------------------------------------------------
  
  # Map of bookings by country
  output$bookings_by_country_plot <- renderPlotly({
    
    # Read the world countries geometries
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Summarize the bookings dataset by country and count the number of occurrences of each country
    hotel_summary <- hotel_bookings %>%
      filter(is_canceled == 0) %>%
      group_by(country) %>%
      summarise(total_bookings = n()) # %>%
    
    # Join the bookings summary data with the world geometries on both the name and admin columns
    world_data <- world %>%
      st_make_valid() %>%
      left_join(hotel_summary, by = c("iso_a3" = "country")) %>%
      st_as_sf()
    
    # Replace NA values with 0
    world_data$total_bookings[is.na(world_data$total_bookings)] <- 0
    
    # Calculate the centroids of the country polygons
    world_data$centroid <- st_centroid(world_data)
    
    # Extract the latitude and longitude coordinates
    world_data$lon <- st_coordinates(world_data$centroid)[, 1]
    world_data$lat <- st_coordinates(world_data$centroid)[, 2]
    
    # Create a color palette function
    pal <- colorRampPalette(c("white", "purple"))
    
    world_map <- ggplot(world_data) +
      geom_sf(aes(fill = total_bookings), color = "grey", size = 0.5) +
      scale_fill_gradientn(colours = pal(20000), na.value = 'black') + 
      geom_sf_text(aes(label = name), size = 0.75, color = "black") +
      scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
      scale_x_continuous(breaks = c()) +
      labs(title = NULL, x = NULL, y = NULL, fill = "Completed\nBookings\n") +
      theme(
        plot.background = element_rect(fill = "#f2f2f2"), 
        legend.background = element_rect(fill = "#f2f2f2"), 
        panel.background = element_rect(fill = "#f2f2f2"),
        strip.background = element_rect(fill = 'white', colour = 'white'),
        legend.position = "bottom",
        legend.box.just = "center",
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank()
        )
    
    
    ggplotly(world_map)
    
  })
  
  # Top 10 countries by hotel bookings
  output$top_countries_table <- renderReactable({
    
    # Add resource path
    addResourcePath(prefix = "img", directoryPath = normalizePath("img"))
    
    # Create a data frame with the countries and number of bookings
    booking_counts <- data.frame(
      country = c("Portugal", "United Kingdom", "France", "Spain", "Germany", 
                  "Ireland", "Italy", "Belgium", "Netherlands", "United States"),
      bookings = c(21071, 9676, 8481, 6391, 6069, 2543, 2433, 1868, 1717, 1596)
    )
    
    # Display the data frame as a reactable
    reactable(
      data = booking_counts,
      columns = list(
        country = colDef(name = "Country", cell = function(value) {
          image <- img(src = sprintf("img/%s.png", tolower(value)), style = "height: 20px; width: 30px;", alt = value)
          tagList(
            div(style = "display: inline-block; width: 45px;", image),
            value
          )
        }),
        bookings = colDef(name = "Bookings")
      ),
      style = list(
        backgroundColor = "#f2f2f2"
      ),
      pagination = TRUE
    )
  })
  
  ###### BOOKINGS BY SEASON ----------------------------------------------------
  ## ROW ONE
  
  # Define the order of the months
  month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  # Create a reactive object that counts the frequency of bookings and cancellations by month
  month_counts <- reactive({
    hotel_bookings %>%
      group_by(arrival_date_month) %>%
      summarize(n_bookings = sum(is_canceled == 0), n_cancellations = sum(is_canceled == 1)) %>%
      mutate(arrival_date_month = factor(arrival_date_month, levels = month_order)) %>%
      arrange(arrival_date_month)
  })
  
  # Create a bar plot of the distribution of bookings and cancellations by month
  output$month_bar_plot <- renderPlotly({
    month_counts_df <- month_counts()
    
    plot_ly(data = month_counts_df, x = ~arrival_date_month, y = ~n_bookings, name = "Completed", 
            type = "scatter", mode = "lines", line = list(color = "#00FF00", width = 4, colors = c("#00FF00", "red"))) %>%
      add_trace(y = ~n_cancellations, name = "Cancelled", type = "scatter", mode = "lines",
                line = list(color = "red", width = 4)) %>%
      layout(title = "Completed Bookings vs Cancellations by Month", xaxis = list(title = ""), 
             yaxis = list(title = ""), plot_bgcolor = "#f2f2f2", paper_bgcolor = "#f2f2f2")
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
    
    cancellation_summary$percent_cancellation[1] <- paste(round((cancellation_summary$cancellation_type_count[1] / cancellation_summary$total_bookings[1]) * 100, 1), "%")
    cancellation_summary$percent_cancellation[2] <- paste(round((cancellation_summary$cancellation_type_count[2] / cancellation_summary$total_bookings[2]) * 100, 1), "%")
    cancellation_summary$percent_cancellation[3] <- paste(round((cancellation_summary$cancellation_type_count[3] / cancellation_summary$total_bookings[3]) * 100, 1), "%")
    cancellation_summary$percent_cancellation[4] <- paste(round((cancellation_summary$cancellation_type_count[4] / cancellation_summary$total_bookings[4]) * 100, 1), "%")
    
    # Define the fill colors and labels for the plot
    fill_colors <- c("#00FF00", "red")
    fill_labels <- c("Completed", "Cancelled")
    
    # Create the plot
    p <- cancellation_summary %>%
      ggplot(. ,aes(x = hotel,
                    y = percent_cancellation,
                    label = percent_cancellation,
                    fill = factor(is_canceled, labels = fill_labels))) +
      geom_col(width = 0.5) +
      geom_text(size = 5, position = position_stack(vjust = 0.5),
                colour = "black", fontface = "bold") +
      labs(title ="Percent Cancellations by Hotel Type", x = "",
           y = "", fill = "Booking Status") +
      scale_fill_manual(values = fill_colors) +
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), 
            legend.background = element_rect(fill = "#f2f2f2"),
            axis.text.y = element_blank()) 
    
    ggplotly(p, tooltip = c("label")) %>%
      layout(hovermode = FALSE, 
             plot_bgcolor = "#f2f2f2",
             paper_bgcolor = "#f2f2f2"
      )
  })
  
  ## ROW TWO
  
  output$dist_chan_counts <- renderPlotly({
    dist_types <- hotel_bookings %>%
      count(distribution_channel, sort = TRUE)
    
    dist_type_labels <- paste(dist_types$distribution_channel)
    dist_type_perc <- paste(dist_types$distribution_channel, gsub("^(\\d+)(\\..*)?$", "\\1\\2", scales::percent(dist_types$n / sum(dist_types$n), accuracy = 0.1)))
    
    plot_ly(dist_types, labels = dist_type_labels, values = ~n, type = "pie", 
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~dist_type_perc
            ) %>%
      layout(title = list(text = "Booking Methods", x = 0.55),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             plot_bgcolor = "#f2f2f2",
             paper_bgcolor = "#f2f2f2", 
             showlegend = FALSE
      )
  })
  
  ###### Pricing ----------------------------------------------------
  ## ROW ONE
  
  # Convert the month column to a factor with the desired order of levels
  hotel_bookings$arrival_date_month <- factor(hotel_bookings$arrival_date_month, levels = month_order)
  
  # Define a function to format tick values with a dollar sign
  dollar_format <- function(x) {
    paste0("$", x)
  }
  
  # Create a line plot of the average daily rate by month for each hotel type
  output$cost_bar_plot <- renderPlotly({
    hotel_bookings %>%
      group_by(hotel, arrival_date_month) %>%
      summarise(avg_adr = mean(adr)) %>%
      plot_ly(x = ~arrival_date_month, y = ~round(avg_adr, 2), color = ~hotel, type = "scatter", mode = "lines+markers",
              line = list(width = 4)) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "", tickformat = "s", tickprefix = "$", tickvals = seq(40, 180, 20), tickformat = dollar_format),
             title = "Average Daily Rate by Month and Hotel Type",
             plot_bgcolor = "#f2f2f2",
             paper_bgcolor = "#f2f2f2")
  })
  
  ## ROW TWO
  
  # Define the order of the months
  month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  # Create a reactive object that calculates the average ADR by month and market segment
  adr_by_month_and_segment <- reactive({
    hotel_bookings %>%
      filter(market_segment %in% c("Corporate", "Direct", "Offline TA/TO", "Online TA")) %>%
      mutate(market_segment = ifelse(market_segment %in% c("Offline TA/TO", "Online TA"), "TA/TO", market_segment)) %>%
      group_by(arrival_date_month, market_segment) %>%
      summarize(mean_adr = mean(adr, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(arrival_date_month = factor(arrival_date_month, levels = month_order)) %>%
      arrange(arrival_date_month, market_segment)
  })
  
  # Define a function to format tick values with a dollar sign
  dollar_format <- function(x) {
    paste0("$", x)
  }
  
  # Create a scatter plot of the average ADR by month and market segment
  output$adr_per_dist <- renderPlotly({
    adr_by_month_and_segment_df <- adr_by_month_and_segment()
    
    plot_ly(data = adr_by_month_and_segment_df, x = ~arrival_date_month, y = ~round(mean_adr, 2), color = ~market_segment,
            type = "scatter", mode = "lines+markers") %>%
      layout(title = "Average Daily Rate by Month and Market Segment", xaxis = list(title = ""),
             yaxis = list(title = "", tickformat = "s", tickprefix = "$", tickvals = seq(60, 160, 20), tickformat = dollar_format),
             plot_bgcolor = "#f2f2f2", paper_bgcolor = "#f2f2f2")
  })
  
}

# Run the app
shinyApp(ui, server)