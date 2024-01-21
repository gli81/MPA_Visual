
#Reading important libraries
library(tidyverse)


#Load dataset from an RDS file named "optdata.rds"
settlement_data <- read_csv("Settlement_Data_updated.csv")
#-- Manipulate input data (specific to this dataset)



#---- Importing libraries for R Shiny app ----
library(shiny)

#---It sets up the user interface (UI) of a Shiny application, allowing users to select parent areas, 
#---indicators, file type, and download the selected data. It also provides placeholders for a 
#--- histogram plot and a table to display the data. ----

ui <- fluidPage(
  titlePanel("Measures between 2015 and 2021"),  # Page title
  sidebarLayout(
    sidebarPanel(
      # checkbox inputs for MPA 
      checkboxGroupInput(
        choices=unique(settlement_data$MPAName),
        inputId="select_MPA",  # Input ID used for server-side processing
        label="Select MPA",
        selected=NULL, # sets the default selected choices to NULL.
        inline=FALSE  # sets whether the checkbox options should appear inline or not.
        #multiple = TRUE  # Option to select multiple values is commented out
      ),
      
      # radio button inputs for index
      radioButtons(
        inputId="select_index",
        label="select index",
        choices=c("AvgMAIndex", "AvgMTIndex", "AvgFSIndex", "AvgPAIndex", "AvgSERate"),
        selected="AvgMAIndex",
        inline=FALSE
      ),
      
      # horizontal line
      hr(),
      radioButtons("dtype", "Select the file type for download", c("csv", "txt")),  # Radio button to select file type for download
      downloadButton("dwd_data", "Download selected data")  # Button to download selected data
    ),
    
    mainPanel(
      # epicurve goes here
      plotOutput("measures_histogram"),  # Histogram plot output
      tableOutput("table_data"),  # Table output
    )
  )
)


#---In summary, the plot_epicurve function takes data, parent areas, and indicators as input, 
#--- and generates an epicurve plot using the ggplot2 package ----

plot_epicurve <- function(data, MPA = NULL, index = NULL) {
  
  # Check if parentarea or indicators are NULL
  if (is.null(MPA) || is.null(index)) {
    # Return empty plot with no data if either are NULL
    return(ggplot() + theme_void())
  }
  
  # Filter data based on selected parentarea and indicators, and calculate 
  #summary statistics (confidence intervals)
  sub_data <- data %>%
    group_by(MPAName, TimePoint``) %>% # group by indicator as well
    summarize(mean_measure_norm = mean(measure_norm),
              sd_measure_norm = sd(measure_norm),
              n = n(),
              se = sd_measure_norm / sqrt(n)) %>%
    ungroup()# %>%
  #mutate(lower_ci = mean_measure_norm - 1.96 * se,
  #upper_ci = mean_measure_norm + 1.96 * se)
  #print(sub_data)
  all_comb <- expand.grid(
    year = unique(sub_data$year),
    parent_area = unique(sub_data$parent_area),
    indicator = unique(sub_data$indicator)
  )
  all_comb <- tibble(all_comb %>%
                       mutate(mean_measure_norm = 0,
                              sd_measure_norm = 0,
                              n = 0,
                              se = 0))
  merged_data <- full_join(sub_data, all_comb)
  merged_data <- merged_data %>%
    group_by(year, parent_area, indicator) %>%
    summarize(mean_measure_norm = max(mean_measure_norm, na.rm = T),
              sd_measure_norm = max(sd_measure_norm, na.rm = T),
              n = max(n, na.rm = T),
              se = max(se, na.rm = T)) %>%
    ungroup() %>%
    mutate(lower_ci = mean_measure_norm - 1.96 * se,
           upper_ci = mean_measure_norm + 1.96 * se)
  
  #print(merged_data)
  
  
  # Create bar plot with error bars and facet_wrap by parent_area
  p <- ggplot(merged_data, aes(x = year, y = mean_measure_norm, fill = indicator)) +
    geom_bar(stat = "identity", position = "dodge") +
    #geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.5)) +
    scale_fill_manual(values = c("#a2dab3", "#52b3c6", "#337dbb", "#253596")) +
    labs(title = "Measures between 2015 and 2021",
         x = "Year",
         y = "Measure",
         fill = "Indicator") +
    theme_bw() +
    facet_wrap(~ parent_area, scales = "free_y", ncol = 1)
  
  return(p)  # Return plot
  
}

#---In summary, the server function defines the reactive behavior of the Shiny app. 
#--- It filters the data based on user inputs, renders the epicurve plot and table using the filtered 
#--- data, and provides a download handler for downloading the filtered data. The functionality is 
#--- tied to the user interface (UI) elements defined in the code you provided. ----

server <- function(input, output, session) {
  
  # Define reactive data that filters optdata4 based on user input
  sub_data2 <- reactive({
    
    if (!is.null(input$select_parentarea) && !is.null(input$select_indicators)){
      sub_data3 <- optdata4 %>%
        filter(parent_area %in% input$select_parentarea) %>%
        filter(indicator %in% input$select_indicators)
    } 
  })
  
  # Render the epicurve plot using the filtered data
  output$measures_histogram <- renderPlot(
    plot_epicurve(sub_data2(), input$select_parentarea, input$select_indicators))
  
  # Render a table showing the filtered data
  output$table_data <- renderTable({
    if (is.null(input$select_parentarea) && is.null(input$select_indicators)) {
      return(NULL)
    } else {
      sub_data2()
    }
  })
  
  # Define a download handler for the filtered data
  output$dwd_data <- downloadHandler(
    # Set the filename for the downloaded file
    filename = function(){
      paste("sub_data2", input$select_parentarea, input$select_indicators, input$dtype, sep = ".")},
    # Write the filtered data to a file
    content = function(file){
      write.table(sub_data2(), file=file, sep=",", row.names = F)})
}

#---It calls the shinyApp function, passing the UI (user interface) and server functions as arguments, 
#--- and starts the Shiny app.
#--- The shinyApp function is the main function in the Shiny package that combines the UI and server 
#--- components to create a complete Shiny application. ----
shinyApp(ui = ui, server = server)                                                                                                                                            



