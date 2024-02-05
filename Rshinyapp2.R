
#Reading important libraries
library(tidyverse)
library(rlang)


#Load dataset from an RDS file named "optdata.rds"
settlement_data <- read_csv("Settlement_Data_updated.csv")
#-- Manipulate input data (specific to this dataset)



#---- Importing libraries for R Shiny app ----
library(shiny)

#---It sets up the user interface (UI) of a Shiny application, allowing users to select parent areas, 
#---indicators, file type, and download the selected data. It also provides placeholders for a 
#--- histogram plot and a table to display the data. ----

ui <- fluidPage(
  titlePanel("MPA Index ??"),  # Page title
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
      checkboxGroupInput(
        inputId="select_index",
        label="Select Index",
        choices=c("Material Index", "Marine Tenure Index", "Food Security Index", "Place Attachment Index", "School Enrollment Rate"),
        selected=NULL,
        inline=FALSE
      ),
      #radioButtons(
      #  inputId = "select_index",
      #  label="Select Index",
      #  choices=c("Material Index", "Marine Tenure Index", "Food Security Index", "Place Attachment Index", "School Enrollment Rate"),
      #  selected="Material Index",
      #  inline=FALSE
      #),
      
      checkboxGroupInput(
        choices=unique(settlement_data$TimePoint),
        inputId="select_Time",
        label="Select Time Point",
        selected=NULL,
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

plot_epicurve <- function(data, MPA=NULL, index=NULL, time=NULL) {
  
  ## Check if MPA or index are NULL
  if (is.null(MPA) || is.null(index) || is.null(time)) {
    # Return empty plot with no data if either are NULL
    return(ggplot() + theme_void())
  }
  
  ## Filter data based on selected parentarea and indicators, and calculate 
  ## summary statistics (confidence intervals)
  # selected_index = substr(index, start=4, stop=nchar(index))
  #sub_data <- data %>%
  #  select(MPAName, TimePoint, !!sym(index)) %>%
  #  group_by(MPAName, TimePoint) %>% # group by MPA, TimePoint
  #  summarize(AvgIndex = mean(!!sym(index), na.rm=T)) %>%
  #  ungroup() %>%
  #  filter(TimePoint %in% time)
  ## long table
  long_table <- data %>%
    pivot_longer(
      cols=c(`Material Index`,
             `Marine Tenure Index`,
             `Food Security Index`,
             `Place Attachment Index`,
             `School Enrollment Rate`),
      
      names_to="IndexName",
      values_to="IndexValue"
    ) %>%
    filter(IndexName %in% index) %>%
    filter(TimePoint %in% time) %>%
    filter(MPAName %in% MPA) %>%
    select(MPAName, TimePoint, IndexName, IndexValue)
  #print(table(long_table$IndexName))
  #print(long_table, n=500)
  if (length(index)==1){
    one_data <- long_table %>%
      group_by(MPAName, TimePoint) %>%
      summarize(AvgIndex = mean(IndexValue, na.rm=T))
  #print(one_data, n=500)
    p <- ggplot(one_data, aes(x=MPAName, y=AvgIndex, fill=TimePoint)) +
      geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8) +
      scale_fill_manual(values=c(
        "Baseline"="#a2dab3",
        "2 Year Post"="#52b3c6",
        "4 Year Post"="#337dbb",
        "7 Year Post"="#253596"
      )) + 
      labs(
        title=paste("Change of", long_table$IndexName[1], "in BHS MPA"),
        x='',
        y=long_table$IndexName[1],
        fill="Time Point"
      ) + 
      theme_bw() + 
      theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=0.8))
  } else {
    one_data<-long_table %>%
      group_by(MPAName, TimePoint, IndexName) %>%
      summarize(AvgIndex = mean(IndexValue, na.rm=T))
    p <- ggplot(one_data, aes(x=MPAName, y=AvgIndex, fill=TimePoint)) +
      geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8) +
      scale_fill_manual(values=c(
        "Baseline"="#a2dab3",
        "2 Year Post"="#52b3c6",
        "4 Year Post"="#337dbb",
        "7 Year Post"="#253596"
      )) + 
      labs(
        x='',
        fill="Time Point"
      ) + 
      theme_bw() + 
      theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=0.8)) + 
      facet_wrap(~ IndexName, scales="free_y", ncol = 1)
  }
  
  return(p)  # Return plot
  
}

#---In summary, the server function defines the reactive behavior of the Shiny app. 
#--- It filters the data based on user inputs, renders the epicurve plot and table using the filtered 
#--- data, and provides a download handler for downloading the filtered data. The functionality is 
#--- tied to the user interface (UI) elements defined in the code you provided. ----

server <- function(input, output, session) {
  
  # Define reactive data that filters settlement_data based on user input
  filtered_data <- reactive({
    if (!is.null(input$select_MPA) && !is.null(input$select_index) && !is.null(input$select_Time)){
      choose_MPA <- settlement_data %>%
        mutate(
          `Material Index` = MAIndex,
          `Marine Tenure Index` = MTIndex,
          `Food Security Index` = FSIndex,
          `Place Attachment Index` = PAIndex,
          `School Enrollment Rate` = SERate,
          Treatment = if_else(Treatment == 1, "MPA", "Non-MPA"),
          SettlementID = as.integer(round(SettlementID)),
          TimePoint = factor(TimePoint, levels=c("Baseline", "2 Year Post", "4 Year Post", "7 Year Post"))
        ) %>%
        select(SettlementID, SettlementName, MPAName, Treatment, TimePoint,
               `Material Index`,
               `Marine Tenure Index`,
               `Food Security Index`,
               `Place Attachment Index`,
               `School Enrollment Rate`
               )
      return(choose_MPA)
    } else {
      return(NULL)
    }
  })
  
  
  # Render the epicurve plot using the filtered data
  output$measures_histogram <- renderPlot(
    plot_epicurve(filtered_data(), input$select_MPA, input$select_index, input$select_Time))
  
  # Render a table showing the filtered data
  output$table_data <- renderTable({
    if (is.null(input$select_MPA) || is.null(input$select_index) || is.null(input$select_Time)) {
      return(NULL)
    } else if (is.null(filtered_data())) {
      return(NULL)
    }else {
      modified_data <- reactive(
        {
          modify <- settlement_data %>%
            mutate(
              `Material Index` = MAIndex,
              `Marine Tenure Index` = MTIndex,
              `Food Security Index` = FSIndex,
              `Place Attachment Index` = PAIndex,
              `School Enrollment Rate` = SERate,
              Treatment = if_else(Treatment == 1, "MPA", "Non-MPA"),
              SettlementID = as.integer(round(SettlementID)),
              TimePoint = factor(TimePoint, levels=c("Baseline", "2 Year Post", "4 Year Post", "7 Year Post"))
            ) %>%
            select(SettlementID, SettlementName, MPAName, Treatment, TimePoint, all_of(input$select_index)
            )
          return(modify)
        }
      )
      modified_data()
    }
  })
  
  # Define a download handler for the filtered data
  output$dwd_data <- downloadHandler(
    # Set the filename for the downloaded file
    filename = function(){
      paste("filtered_data", input$select_parentarea, input$select_indicators, input$dtype, sep = ".")},
    # Write the filtered data to a file
    content = function(file){
      write.table(filtered_data(), file=file, sep=",", row.names = F)})
}

#---It calls the shinyApp function, passing the UI (user interface) and server functions as arguments, 
#--- and starts the Shiny app.
#--- The shinyApp function is the main function in the Shiny package that combines the UI and server 
#--- components to create a complete Shiny application. ----
shinyApp(ui = ui, server = server)                                                                                                                                            



