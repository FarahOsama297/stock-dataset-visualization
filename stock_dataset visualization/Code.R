library(shinyDarkmode)
library(tidyverse) 
library(MASS)
library(lubridate)
library(ggplot2)

roundFloat <- function(num) {
  decimal_part <- num - floor(num)
  
  if (decimal_part >= 0.5) {
    return(ceiling(num))
  } else {
    return(floor(num))
  }
}

remove_outliers <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], 0.25)
  Q3 <- quantile(data[[column_name]], 0.75)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data[[column_name]][which(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound)] <- NA
  
  return(data)
}

count_outliers <- function(column) {
  if(is.numeric(column)) {
    q1 <- quantile(column, 0.25, na.rm = TRUE)
    q3 <- quantile(column, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    outliers <- column[column < lower_bound | column > upper_bound]
    return(length(outliers))
  } else {
    return(0)
  }
}

remove <- function(data){
  numeric_cols <- sapply(data, is.numeric)
  
  for (col in names(data)[numeric_cols]){
    data <- remove_outliers(data, col)
    data <- na.omit(data)
  }
  rownames(data) <- NULL
  return(data)
}

data <- read.csv("C:/Users/hp/Downloads/stock_data.csv")
duplicated(data)
is.null(data)
str(data)
summary(data)
head(data)
data$Date <- ymd(data$Date)
data$Company <- as.numeric(factor(data$Company))

columns_to_adjust <- setdiff(names(data), c('Date', 'Company'))
for (col in columns_to_adjust){
  data[[col]] <- sapply(data[[col]], function(num) as.integer(roundFloat(num)))
}
str(data)
while(TRUE){
  temp <- data
  data <- remove(data)
  if(identical(temp, data)) break
}
view (data)

#polts
plot1 <- ggplot(data, aes(x = data$Close, y = data$Open)) +
  geom_line(color = "cyan") +
  labs(title = "Relation between open and close price")
print(plot1)

plot2 <- ggplot(data, aes(x = data$Date, y =data$Adj.Close)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Bar between Adj.Close and Date")
print(plot2)

plot3 <- ggplot(data, aes(x = data$Date, y = data$Volume)) +
  geom_point(color = "purple") +
  labs(title = "Volume Over Time")
print(plot3)

plot4 <- ggplot(data, aes(x = data$Adj.Close)) +
  geom_histogram(fill = "blue", bins = 30) +
  labs(title = "Adj.Close Histogram")
print(plot4)

plot5 <- ggplot(data, aes(x = data$Low, y = data$High )) +
  geom_boxplot(fill = "gold") +
  labs(title = "Boxplot of Low price by High price")
print(plot5)

plot6 <- ggplot(data, aes(x = Volume)) +
  geom_density(fill = "magenta") +
  labs(title = "Density Plot of Volume")
print(plot6)

plot7 <- ggplot(data, aes(x = data$High, y = data$Close, fill = data$Date)) +
  geom_area(fill = "black") +
  labs(title = "Area Plot: HighPrice vs ClosePrice by Date")
print(plot7)

plot8 <- ggplot(data, aes(x = data$Low, y = data$Open, fill = data$Date)) +
  geom_violin(fill = "bisque") +
  labs(title = "Violin Plot: Low vs Open by Date")
print(plot8)
#view(data)
##############################################################################################################################################
library(shiny)
library(shinydashboard)
library(shinyDarkmode)
library(plotly)
ui <- dashboardPage(
  dashboardHeader(title = "Stock Data"),
  dashboardSidebar(
    menuItem("Project Options", tabName = "project_options"),
    use_darkmode(),
    checkboxInput("togglemode", "Choose Dark Mode", value = FALSE),
    selectInput(inputId = "dropdown", label="Select Company", choices = unique(data$Company),multiple = TRUE),
    checkboxGroupInput(inputId = "checkboxx","Select Open Price",choices = unique(head(data$Open, 10))),
    dateRangeInput("date_rangee", "Date Range",start = min(data$Date, na.rm = TRUE),end = max(data$Date, na.rm = TRUE)),
    checkboxGroupInput("chartType", "Select Chart Type",
                       choices = c("Bar Chart", "Scatter Plot", "Area Plot", "Box Plot", "Histogram", "Violin Plot", "Density Plot","Pie Chart"), selected = "Histogram"),
    radioButtons("data_type", "Data Type", choices = c("Daily Data", "Cumulative Data"), selected = "Daily Data"),
    radioButtons("unitOption", "Select Unit Option", choices = c("Percentage Change", "Absolute Values"), selected = "Absolute Values")
    #checkboxGroupInput("category_filter", "Filter by Category", choices = unique(data$Company))
  ),
  dashboardBody(
    fluidRow(
      box(title = 'Instructions to use dashboard :',HTML("<b>Welcome to the Yahoo Finance Data Dashboard! Use the following Instructions to work effectively:</b><br>
                                        1.<b>Dark Mode:</b> Check the checkbox in the ‘Dark Mode’ to convert to dark mode.<br>
                                        2.<b> Choose:</b> Choose a specific company from ‘Select Company’ dropdown menu to focus on the chosen company.<br>
                                        3.⁠<b>Select Open Price </b> Select Open Price to filter data <br>
                                        4.<b>Select Date Range </b> Use the ‘Select Date Range’to filter data based on a specific time.<br>
                                        5.<b>Select Chart Type </b> Check the checkboxes on the ‘Select Chart Type’ to focus on the choosen chart for visualization.<br>
                                        6.<b>Select Data Type </b> Choose the way that you want to display data in it.<br>
                                        7.<b>Select Unit Option </b> Choose the percentage change if you want to know the percentage of every company <br>"),
          width = 13),
    ),
    fluidRow(
      box(title = "Filtered Data", status = "info", width = 14, tableOutput("selected_data_table"))
      #box(title = "Filtered Data By Company", status = "info", width = 14, tableOutput("selected_data_tablee"))
    ),
    fluidRow(
      conditionalPanel(
        condition = "input.chartType.indexOf('Bar Chart') !== -1",
        box(
          title = "Bar chart of Company:",
          plotlyOutput("barchart"),
          "Visualizes the Count of Company "
        )
      ),
      conditionalPanel(
        condition = "input.chartType.indexOf('Scatter Plot') !== -1",
        box(
          title = "Scatter Plot of High Price and Volume:",
          plotlyOutput("scatterplot"),
          "Visualizes the relation between High Price and Volume."
        )
      ),
      conditionalPanel(
        condition = "input.chartType.indexOf('Area Plot') !== -1",
        box(
          title = "Area Plot of Company:",
          plotlyOutput("areaplot"),
          "Visualizes the relation between High Price and Close Price"
        )
      ),
      conditionalPanel(
        condition = "input.chartType.indexOf('Box Plot') !== -1",
        box(
          title = "Box Plot of Low Price and Volume:",
          plotlyOutput("boxplot"),
          "Visualizes the relation between Company and Volume."
        )
      ),
      conditionalPanel(
        condition = "input.chartType.indexOf('Histogram') !== -1",
        box(
          title = "Histogram of Adj.Close Price:",
          plotlyOutput("histogram"),
          "Visualizes the distribution of Adj.Close Price ."
        )
      ),
      conditionalPanel(
        condition = "input.chartType.indexOf('Violin Plot') !== -1",
        box(
          title = "Violin Plot of Low Price and Volume:",
          plotlyOutput("violinplot"),
          "Visualizes the relation between Low Price and Volume"
        )
      ),
      conditionalPanel(
        condition = "input.chartType.indexOf('Density Plot') !== -1",
        box(
          title = "Density Plot of Volume:",
          plotlyOutput("densityplot"),
          "Visualizes the distribution of Volume ."
        )
      ),
      conditionalPanel(
        condition = "input.chartType.indexOf('Pie Chart') !== -1",
        box(
          title = "Pie Chart of Company distribution :",
          plotOutput("piechart"),
          "Visualizes the proportion of each Company in the dataset."
        )
      )
      
    )
  )
)
# Server
server <- function(input, output) {
  #make dark mode 
  darkmode_toggle(inputid = 'togglemode')
  #function to filtered data 
  filteredData <- reactive({
    df_filtered <- data %>%
      filter(
        Company %in% input$dropdown,
        Date >= input$date_rangee[1],
        Date <= input$date_rangee[2],
        Open %in% input$checkboxx
      )
    # Check the selected data type (Daily or Cumulative)
    if (input$data_type == "Daily Data") {
      df_filtered
    } else  {
      cumulative_data <- df_filtered
      
      # Compute cumulative sums for each numeric column (excluding Date)
      cumulative_data[, -1] <- apply(cumulative_data[, -1], 2, cumsum)
      cumulative_data
    }
    
  })
  # Render filtered data table
  output$selected_data_table <- renderTable({
    filteredData()
  })
  # Render the selected charts
  output$barchart <- renderPlotly({
    ggplot(filteredData(), aes(x = Company)) + geom_bar(fill = "lightblue")
  })
  output$scatterplot <- renderPlotly({
    ggplot(filteredData(), aes(x = High, y = Volume)) + geom_point()
  })
  
  output$areaplot<- renderPlotly({
    ggplot(filteredData(), aes(x = High, y = Close, fill = Date)) +geom_area(fill = "lightblue")
  })
  output$boxplot <- renderPlotly({
    ggplot(filteredData(), aes(x = Company, y = Volume )) + geom_boxplot()
  })
  output$histogram <- renderPlotly({
    ggplot(filteredData(), aes(x = Adj.Close)) + geom_histogram(binwidth = 1, fill = "lightblue")
  })
  output$violinplot <- renderPlotly({
    ggplot(filteredData(), aes(x = Low, y = Volume)) + geom_violin() 
  })
  output$densityplot<-renderPlotly({
    ggplot(filteredData(), aes(x = Volume)) +geom_density(fill = "magenta")
  })
  output$piechart<- renderPlot({
    if (input$unitOption == "Percentage Change") {
      # Create a pie chart for the absolute values distribution of ratings
      ggplot(filteredData(), aes(x = "", fill = as.factor(Company))) +
        geom_bar(width = 1, color = "white") +
        coord_polar(theta = "y") +
        scale_fill_brewer(palette = "Set3") +
        labs(fill = "Company") +
        geom_text(stat = "count", aes(label = after_stat(round(..count../sum(..count..)*100, 1))),
                  position = position_stack(vjust = 0.5))
    } else {
      # Create a pie chart for the percentage distribution of ratings
      ggplot(filteredData(), aes(x = "", fill = as.factor(Company))) +
        geom_bar(width = 1, color = "white") +
        coord_polar(theta = "y") +
        scale_fill_brewer(palette = "Set3") +
        labs(fill = "Company")+
        geom_text(stat = "count", aes(label = after_stat(round(..count.., 1))),
                  position = position_stack(vjust = 0.5))
    }
  })
  ### Reactive function for filtered data
  #filtered <- reactive({
  #  d_filtered <- data %>%
  #    filter(
  #      Company %in% input$category_filter
  #    )
  #})  
}
# Run the Shiny app
shinyApp(ui, server)