library(shiny)
library(tidyverse)

ui <- fluidPage(
   
   # Application title
   titlePanel("Link between Normal and Chi-square distribution"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        withMathJax("This is a simulation which shows: $$\\chi^2_k = Y^2_1 + Y^2_2 \\dots + Y^2_k,$$ with Y ∼ N(0, 1), k --- degrees of freedom"),
         sliderInput("df",
                     "Change number of samples / degrees of freedom:",
                     min = 1,
                     max = 60,
                     value = 1,
                     step = 1)
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot <- renderPlot({
     norm <- data_frame(value = NA, type = NA, id = NA)
     sapply(1:input$df, function(i){
       set.seed(i)
       new_df <- data_frame(value = rnorm(1000, 0, 1),
                            type = "Υ ∼ N(0, 1)",
                            id = i)
       norm <<- rbind(norm, new_df)
       })
     norm <- norm[-1,]
     norm %>% 
       mutate(value = value^2,
              row = rep(1:1000, input$df)) %>% 
       spread(id, value) %>% 
       select(1:input$df+2) %>% 
       mutate(value = rowSums(.),
              type = "∑Υ²",
              id = 0) %>% 
       select(value, type, id) ->
       norm_2
     
     set.seed(input$df)
     chi_2 <- data_frame(value = rchisq(1000, input$df),
                         type = paste("χ², df =", input$df),
                         id = 0)
     combined_df <- rbind(norm, norm_2, chi_2)
     combined_df %>% 
       mutate(type = factor(type, 
                            levels = c("Υ ∼ N(0, 1)",
                                       "∑Υ²",
                                       paste("χ², df =", input$df)))) %>% 
       ggplot(aes(value, fill = factor(id)))+
       geom_density(show.legend = FALSE, alpha = 0.2)+
       facet_wrap(~type, scales = "free")+
       theme_bw()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

