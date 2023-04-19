library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

## Average stay length, Average Lead Time To Booking, repeat guest percent, cancellation rate

# Bookings by country, hotel types, table of total guests by resort type
# bookings by year, bookings for each deposit type
# bookings by month, cancellation rate by month, cancellation by hotel type

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
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
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
        "
      )
    ),
    tabItems(
      tabItem(tabName = "summary", 
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
                column(width = 4,
                       plotlyOutput("bookings_by_country_plot", height = "200px")
                ),
                column(width = 4,
                       plotlyOutput("hotel_types_pie_chart", height = "200px")
                ),
                column(width = 4,
                       DTOutput("hotel_type_table")
                )
              ),
              fluidRow(
                column(width = 12,
                       #plotlyOutput("month_bar_plot")
                )
              )
      ),
      tabItem(tabName = "bookings",
              fluidRow(  ),
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
  
  # Calculate the average stay length
  avg_stay_length <- reactive({
    mean(hotel_bookings$stays_in_week_nights + hotel_bookings$stays_in_weekend_nights)
  })
  
  # Calculate the average lead time to booking
  avg_lead_time <- reactive({
    mean(hotel_bookings$lead_time)
  })
  
  # Calculate the repeat guest percentage
  repeat_guest_percent <- reactive({
    (sum(hotel_bookings$is_repeated_guest == 1) / nrow(hotel_bookings)) * 100
  })
  
  # Calculate the cancellation rate
  cancellation_rate <- reactive({
    (sum(hotel_bookings$is_canceled == 1) / nrow(hotel_bookings)) * 100
  })
  
  # Render the value boxes
  output$avg_stay_length_box <- renderValueBox({
    valueBox(paste(round(avg_stay_length(), 1), "Days"), "Average Stay", icon = icon("hotel"), color = "yellow")
  })
  
  output$avg_lead_time_box <- renderValueBox({
    valueBox(paste(round(avg_lead_time(), 1), "Days"), "Average Booking Lead Time", icon = icon("calendar"), color = "blue")
  })
  
  output$repeat_guest_box <- renderValueBox({
    valueBox(paste(round(repeat_guest_percent(), 1), "%"), "Repeat Guest Percentage", icon = icon("users"), color = "green")
  })
  
  output$cancellation_rate_box <- renderValueBox({
    valueBox(paste(round(cancellation_rate(), 1), "%"), "Cancellation Rate", icon = icon("times-circle"), color = "red")
  })
  
  ##### SECOND ROW ------------------------------------------------------
  
  # Create a sideways histogram of bookings by country
  output$bookings_by_country_plot <- renderPlotly({
    bookings_by_country <- hotel_bookings %>%
      count(country, sort = TRUE) %>%
      filter(!is.na(country))
    
    plot_ly(bookings_by_country, x = ~country, y = ~n, type = "histogram", orientation = "h", color = ~n, colors = "purple") %>%
      layout(title = "Bookings by Country", xaxis = list(title = "Number of Bookings"), yaxis = list(title = "Country"))
  })
  
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
  
  # Create a reactive object that counts the frequency of bookings by month
  month_counts <- reactive({
    hotel_bookings %>%
      count(arrival_date_month) %>%
      arrange(match(arrival_date_month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))
  })
  
  # Create a bar plot of the distribution of bookings by month
  output$month_bar_plot <- renderPlotly({
    month_counts_df <- month_counts()
    
    plot_ly(data = month_counts_df, x = ~arrival_date_month, y = ~n, type = "bar") %>%
      layout(title = "Distribution of Bookings by Month", xaxis = list(title = "Month"), yaxis = list(title = "Number of Bookings"))
  })
  
}

# Run the app
shinyApp(ui, server)