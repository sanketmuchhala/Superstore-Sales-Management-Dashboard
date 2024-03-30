library(shiny)
library(plotly)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)
library(fpp3)
library(tidyverse)
library(janitor)
library(imputeTS)
library(patchwork)
library(lubridate)
library(padr)
library(forecast)
library(ggplot2)
library(tsibble)
library(zoo)
library(ggplot2)

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("admin@superstoregroup.com", "user2"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "standard"),
  name = c("Admin", "User Two")
)



df <- read.csv("train.csv", stringsAsFactors = FALSE)
df<-clean_names(df)

df$order_date <- parse_date_time(df$order_date, orders = c("dmy_HMS", "dmy"))
df$ship_date <- parse_date_time(df$ship_date, orders = c("dmy_HMS", "dmy"))
# Convert date columns to datetime object
df$order_date <- as.POSIXct(df$order_date, tz = "UTC")
df$ship_date <- as.POSIXct(df$ship_date, tz = "UTC")

ui <- fluidPage(theme=shinytheme("yeti"),
                # add logout button UI
                navbarPage(
                  theme = "united",
                  "Superstore Sales Dashboard",
                  tabPanel("Home",
                           div(id="display_home",
                           fluidRow(
                             column(width = 12,
                                    tags$img(src = "https://www.bnymellon.com/us/en/solutions/capital-markets-liquidity-financing/equity-sales-and-trading/_jcr_content/root/container/container_969427196/container_344691320/container_copy_copy/container_copy/teaser_copy.coreimg.jpeg/1612800683655/stocklending-600x396.jpeg", height = 400, width = "100%")),
                             column(width = 12, class = "text-center",
                                    h2(style = "font-size: 24px;", "Sales Forecasting Dashboard")),
                             column(width = 12, class = "text-center",
                                    h2(style = "font-size: 20px;", "Predicting the Sales of the Future"))
                           ))%>%shinyjs::hidden(),
                          mainPanel(
                            htmlOutput("user_welcome"),
                            htmlOutput("user_heading"),
                            shinyauthr::loginUI(id = "login")
                          )
                  ),
                  
                  tabPanel("Region Wise Sales",
                           # sidebarPanel
                           div(id="display_content",
                               sidebarPanel(
                                 
                                 selectInput("Region_Type", "Select Region",
                                             choices = c("Central", "West", "East","South"), selected = "Central"),
                                 selectInput("Category_Type1", "Select Category Type",
                                             choices = c("Furniture", "Office Supplies", "Technology"), selected = "Technology"),
                                 numericInput("h1", "Days Forecasted", value = 10),
                                 sliderInput("n1",
                                             "Number of Days",
                                             value = c(60, 1460),
                                             min = 60,
                                             max = 1460),
                                 uiOutput("channel_groupings"))
                               
                           )%>%shinyjs::hidden(),
                           
                           
                           mainPanel(
                             
                             tags$html(h4( "Region wise sales")),
                             
                             plotlyOutput("sales_plot_region"),
                             plotlyOutput("sales_fc_plot_region")
                             
                             
                             # tableOutput("user_table")
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Segment Wise Sales", 
                           
                           div(id="display_content_segment",
                               sidebarPanel(
                                 
                                 selectInput("Segment_Type", "Select Segment",
                                             choices = c("Consumer", "Corporate", "Home Office"), selected = "Consumer"),
                                 selectInput("Category_Type2", "Select Category Type",
                                             choices = c("Furniture", "Office Supplies", "Technology"), selected = "Technology"),
                                 numericInput("h2", "Days Forecasted", value = 10),
                                 sliderInput("n2",
                                             "Number of Days",
                                             value = c(60, 1460),
                                             min = 60,
                                             max = 1460),
                                 uiOutput("segment_groupings")
                                 
                               ))%>%shinyjs::hidden(),
                           
                           mainPanel(
                             
                             tags$html(h4( "Segment wise sales")),
                             
                             plotlyOutput("sales_plot_segment"),
                             plotlyOutput("sales_fc_plot_segment")
                             
                           )
                  ),
                  
                  
                  tabPanel("Summary Statistics", 
                           
                           div(id="display_summary_stats",
                               sidebarPanel(
                                 selectInput("region", "Select Region:",
                                             choices = unique(df$region)),
                                 selectInput("segment", "Select Segment:",
                                             choices = unique(df$segment)),
                                 selectInput("state", "Select State:",
                                             choices = unique(df$state)),
                                 
                                 br(),
                                 hr(),
                                 selectInput(inputId = "category",
                                             label = "Select Category:",
                                             choices = c("Furniture", "Office Supplies", "Technology"),
                                             selected="Technology"),
                                 #selectInput("year", "Select a year:", choices = 2015:2018, selected = 2016),
                                 sliderInput("year", "Select a year:", min = 2015, max = 2018, value = 2016,sep = ""),
                                 
                                 hr(),
                                 
                               ))%>%shinyjs::hidden(),
                           
                           mainPanel(
                             
                             h4("Summary Statistics"),
                             verbatimTextOutput("summary"),
                             plotOutput("summary_histogram")
                           )
                  )
                ), # navbarPage
                
                div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                
                
)


server <- function(input, output, session) {
  
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  
  shiny::observe({
    req(credentials()$user_auth)
    shinyjs::show(id = "display_home")
  })
  
  shiny::observe({
    req(!credentials()$user_auth)
    shinyjs::hide(id = "display_home")
  })
  
  shiny::observe({
    req(credentials()$user_auth)
    shinyjs::show(id = "display_content")
  })
  
  shiny::observe({
    req(!credentials()$user_auth)
    shinyjs::hide(id = "display_content")
  })
  
  
  shiny::observe({
    req(credentials()$user_auth)
    shinyjs::show(id = "display_content_segment")
  })
  
  shiny::observe({
    req(!credentials()$user_auth)
    shinyjs::hide(id = "display_content_segment")
  })
  
  
  shiny::observe({
    req(credentials()$user_auth)
    shinyjs::show(id = "display_summary_stats")
  })
  
  shiny::observe({
    req(!credentials()$user_auth)
    shinyjs::hide(id = "display_summary_stats")
  })
  
  
  #Insert for data processing and model usage
  
  
  # Read the uploaded file for region
  data <- reactive({
    # Read the uploaded file
    data <- read.csv("train.csv", header = TRUE)
    data <- clean_names(data)
    data$order_date <- parse_date_time(data$order_date, orders = c("dmy_HMS", "dmy"))
    data$ship_date <- parse_date_time(data$ship_date, orders = c("dmy_HMS", "dmy"))
    # Convert date columns to datetime object
    data$order_date <- as.POSIXct(data$order_date, tz = "UTC")
    data$ship_date <- as.POSIXct(data$ship_date, tz = "UTC")
    
    # Filter data by region type
    data <- data %>% 
      filter(region == input$Region_Type) %>% 
      filter(category == input$Category_Type1)
    
    
    data <- data %>% 
      group_by(order_date) %>% 
      summarise(sales = sum(sales)) %>% 
      arrange((order_date))
    
    data<-data %>% mutate(day=row_number())
    
    data$sales <- as.numeric(data$sales)
    
    # Convert to tsibble format
    data<-data%>%as_tsibble(index=day)
    
    return(data)
  })
  
  fc <- reactive({
    data <- data()
    data <- data %>%
      filter(order_date >= tail(data$order_date, 1) - days(input$n1[2]))
    valid_sales <- data$sales[is.numeric(data$sales) & is.finite(data$sales)]
    mod <- nnetar(valid_sales, size = 7, decay = .01, trace = 0, skip = 0)
    
    # Determine end date based on aggregation level
    end_date <- tail(data$order_date, 1) + days(input$h1)
    
    # Generate forecast
    fc <- forecast(mod, h = input$h1)
    fc_df <- data.frame(index = seq(from = as.Date(tail(data$order_date, 1) + days(1)), 
                                    to = as.Date(end_date), by = "1 day"), 
                        mean = fitted(fc)[1:input$h1])
    
    # Remove NA values
    fc_df <- na.omit(fc_df)
    
    return(fc_df)
  })
  
  output$sales_fc_plot_region <- renderPlotly({
    req(credentials()$user_auth)
    data <- data()
    
    p <-ggplot() + 
      geom_line(data = data, aes(x = order_date, y = sales), color = "blue") +
      scale_x_datetime(name = "Date", 
                       date_breaks = "6 month", 
                       date_labels = "%b %Y") +
      scale_y_continuous(name = "Sales", breaks = seq(0, 20000, by = 2000)) +
      labs(title = "Sales Over Lifetime") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  
  # Sales plot and forecast plot based on the filtered data and forecast
  output$sales_plot_region <- renderPlotly({
    req(credentials()$user_auth)
    fc_df <- fc() %>%
      mutate(index = as.POSIXct(index))
    data <- data()
    last_year <- as.Date(tail(data$order_date, 1)) - months(6) 
    data_last_year <- data() %>% 
      filter(order_date >= last_year) %>% 
      mutate(order_date = as.POSIXct(order_date))
    
    p <- ggplot() + 
      geom_line(data = data_last_year, aes(x = order_date, y = sales), color = "blue") +
      geom_line(data = fc_df, aes(x = index, y = mean), color = "red") +
      scale_x_datetime(name = "Date", 
                       limits = c(as.POSIXct(last_year), as.POSIXct(as.Date(tail(data$order_date, 1)+ days(100)))), # Set x-axis limits to last year and today
                       date_breaks = "1 month", 
                       date_labels = "%b %Y") +
      scale_y_continuous(name = "Sales", breaks = seq(0, 10000, by = 1000)) +
      labs(title = "Sales and Forecast") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  
  
  
  # Read the uploaded file for segment
  data2 <- reactive({
    # Read the uploaded file
    data <- read.csv("train.csv", header = TRUE)
    data <- clean_names(data)
    data$order_date <- parse_date_time(data$order_date, orders = c("dmy_HMS", "dmy"))
    data$ship_date <- parse_date_time(data$ship_date, orders = c("dmy_HMS", "dmy"))
    # Convert date columns to datetime object
    data$order_date <- as.POSIXct(data$order_date, tz = "UTC")
    data$ship_date <- as.POSIXct(data$ship_date, tz = "UTC")
    
    # Filter data by region type
    data <- data %>% 
      filter(segment == input$Segment_Type) %>% 
      filter(category == input$Category_Type2)
    
    data <- data %>% 
      group_by(order_date) %>% 
      summarise(sales = sum(sales)) %>% 
      arrange((order_date))
    
    data<-data %>% mutate(day=row_number())
    
    data$sales <- as.numeric(data$sales)
    
    # Convert to tsibble format
    data<-data%>%as_tsibble(index=day)
    
    return(data)
  })
  
  fc2 <- reactive({
    data <- data2()
    data <- data %>%
      filter(order_date >= tail(data$order_date, 1) - days(input$n2[2]))
    valid_sales <- data$sales[is.numeric(data$sales) & is.finite(data$sales)]
    mod <- nnetar(valid_sales, size = 7, decay = .01, trace = 0, skip = 0)
    
    # Determine end date based on aggregation level
    end_date <- tail(data$order_date, 1) + days(input$h2)
    
    # Generate forecast
    fc <- forecast(mod, h = input$h2)
    fc_df <- data.frame(index = seq(from = as.Date(tail(data$order_date, 1) + days(1)), 
                                    to = as.Date(end_date), by = "1 day"), 
                        mean = fitted(fc)[1:input$h2])
    
    # Remove NA values
    fc_df <- na.omit(fc_df)
    
    return(fc_df)
  })
  
  output$sales_fc_plot_segment <- renderPlotly({
    req(credentials()$user_auth)
    data <- data2()
    
    p <-ggplot() + 
      geom_line(data = data, aes(x = order_date, y = sales), color = "blue") +
      scale_x_datetime(name = "Date", 
                       date_breaks = "6 month", 
                       date_labels = "%b %Y") +
      scale_y_continuous(name = "Sales", breaks = seq(0, 20000, by = 2000)) +
      labs(title = "Sales Over Lifetime") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  
  # Sales plot and forecast plot based on the filtered data and forecast
  output$sales_plot_segment <- renderPlotly({
    req(credentials()$user_auth)
    fc_df <- fc2() %>%
      mutate(index = as.POSIXct(index))
    data <- data2()
    last_year <- as.Date(tail(data$order_date, 1)) - months(6) 
    data_last_year <- data2() %>% 
      filter(order_date >= last_year) %>% 
      mutate(order_date = as.POSIXct(order_date))
    
    p <-ggplot() + 
      geom_line(data = data_last_year, aes(x = order_date, y = sales), color = "blue") +
      geom_line(data = fc_df, aes(x = index, y = mean), color = "red") +
      scale_x_datetime(name = "Date", 
                       limits = c(as.POSIXct(last_year), as.POSIXct(as.Date(tail(data$order_date, 1)+ days(100)))), # Set x-axis limits to last year and today
                       date_breaks = "1 month", 
                       date_labels = "%b %Y") +
      scale_y_continuous(name = "Sales", breaks = seq(0, 10000, by = 1000)) +
      labs(title = "Sales and Forecast") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Render pie chart for channel groupings
  output$channel_groupings <- renderUI({
    req(credentials()$user_auth)
    fig <- channel_groupingsfunc()
    plotlyOutput("channel_groupings_plot")
  })
  
  # Render pie chart plot
  output$channel_groupings_plot <- renderPlotly({
    fig <- channel_groupingsfunc()
    fig
  })
  
  
  
  channel_groupingsfunc <- function() {
    
    
    data <- df %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(all = round(n / sum(n) * 100, 2))
    
    fig <- data %>%
      plot_ly(labels = ~region, values = ~all, type = "pie")
    fig <- fig %>% layout(
      title = "",
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
    
  }
  
  
  # Render pie chart for segment groupings
  output$segment_groupings <- renderUI({
    req(credentials()$user_auth)
    fig <- segment_groupingsfunc()
    plotlyOutput("segment_groupings_plot")
  })
  
  # Render pie chart plot
  output$segment_groupings_plot <- renderPlotly({
    fig <- segment_groupingsfunc()
    fig
  })
  
  segment_groupingsfunc <- function() {
    
    
    data <- df %>%
      dplyr::group_by(segment) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(all = round(n / sum(n) * 100, 2))
    
    fig <- data %>%
      plot_ly(labels = ~segment, values = ~all, type = "pie")
    fig <- fig %>% layout(
      title = "",
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
    
  }
  
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    credentials()$info
  })
  
  output$user_heading <- renderText({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    HTML(paste0("<h2>Welcome, ", credentials()$info$name, "!</h2>"))
    
  })
  
  
  output$user_welcome <- renderText({
    # use req to only render results when credentials()$user_auth is TRUE
    req(!credentials()$user_auth)
    HTML(paste0("<h2>Welcome back!!</h2>"))
  })
  
  
  
  output$user_wel <- renderText({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    h1("this is region wise sales")
  })
  
  
  
  
  # filter the data based on user input
  filtered_data <- reactive({
    df %>%
      filter(region == input$region,
             segment == input$segment,
             state == input$state,
             year(order_date) == input$year,
             category==input$category)
  })
  
  # render the summary statistics
  output$summary <- renderPrint({
    req(credentials()$user_auth)
    summary(filtered_data()$sales)
  })
  
  output$summary_histogram <- renderPlot({
    req(credentials()$user_auth)
    data<-filtered_data() %>% mutate(order_date = ymd(order_date))
    ggplot(data = data, aes(x = order_date, y = sales)) +
      geom_bar(stat = "identity", color = "black", fill = "skyblue") +
      labs(title = "Histogram of Sales", x = "Date of Sales", y = "Amount of Sales") +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%m/%d/%Y")
  })
}








shinyApp(ui = ui, server = server)
