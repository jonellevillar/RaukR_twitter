load("twitter.name.source.RData")

library(ggplot2)
library(wordcloud2)
library(shiny)
ui <- fluidPage(
  h1("Twitter data research"),
  tags$img(height=50,width=50,
           src="/Users/yisun086/Desktop/RaukR/RauR_tweet/RaukR_twitter/www/logo.png"),
  #titlePanel("Twitter Research"),
  navbarPage("",
             tabPanel("Component 1",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("data_input",label="Select data",
                                      choices=c("Rstudio.source","Python.source")),
                          textInput(inputId="title",label="Write your plot title here",
                                    value="Barplot for Top 10 Twitter Source")
                        ),
                        
                        mainPanel(
                          plotOutput("plot_output")
                        ))),
             tabPanel("Component 2",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("data_input2",label="Select data",
                                      choices=c("rt.df.R.name")),
                          bootstrapPage(
                            numericInput('size', 'Size of wordcloud', n)
                            
                          )
                        ),
                        
                        mainPanel(
                          wordcloud2Output('wordcloud2')
                        ))
                      
                      
                      #bootstrapPage(
                      #numericInput('size', 'Size of wordcloud', n),
                      # wordcloud2Output('wordcloud2')
                      # )
             ),
             #mainPanel(
             #plotOutput("plot_output")
             #))),
             tabPanel("Component 3")
  )
)
server <- function(input,output){
  getdata1 <- reactive({ get(input$data_input) })
  getdata2 <- reactive({ get(input$data_input2) })
  output$plot_output <- renderPlot({
    ggplot(data=getdata1(),aes(x=source,y=Freq))+
      geom_bar(stat="identity",fill="steelblue")+
      ggtitle(isolate(input$title))+
      theme(
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=15),
        plot.title = element_text(color="steelblue", size=17, face="bold.italic")
      )
  })
  
  output$wordcloud2 <- renderWordcloud2({
    #wordcloud2(getdata2(), size=input$size)
    letterCloud(getdata2(), size=input$size,word = "R", color='random-light' , backgroundColor="black")
  })
}

shinyApp(ui=ui,server=server)

