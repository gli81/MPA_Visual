## Webapp


library(shiny)

ui <- basicPage(
  h1("R Shiny Bar Plot"),
  plotOutput("plot")
  
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    df <- read.csv("/Users/godwinanguzu/Documents/projects 2022/Marine_project/Rshiny_test/Rshiny_project1/df.csv", header = TRUE, sep = ",")
    
    #Plot
    ggplot(data=test_data, aes(x=category, y=value)) +
      geom_bar(stat="identity")
    
    #barplot(df$value)
    
  })
  
}


shinyApp(ui = ui, server = server)





