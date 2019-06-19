setwd("/Users/yisun086/Desktop/RaukR/twitter_proj/RaukR_twitter/06_shiny")
load("twitter_R_Python_data.RData")
n <-3

library(ggplot2)
library(wordcloud2)
library(shiny)


ui <- fluidPage(
  fluidRow(
    column(3,tags$img(height=150,width=150,src="logo.png")),
    column(5,h1("Twitter: a study of bioinformatics tweets"))  
  ),
  
  #titlePanel("Twitter Research"),
  navbarPage("",
             tabPanel("Component 1",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("var_input",label="Select data",
                                      choices=c("screen_name","source","location")
                                      )
                        ),
                        
                        mainPanel(
                          plotOutput("plot_output_var")
                        )
                        )),
             tabPanel("Component 2",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("data_input2",label="Select data",
                                      choices=c("R.name","Python.name")
                                      ),
                          bootstrapPage(
                            numericInput('size', 'Size of wordcloud', n)
                                       ),
                          selectInput("data_input",label="Select data for source search",
                                      choices=c("Rstudio.source","Python.source")),
                          textInput(inputId="title",label="Write your plot title here",
                                    value="Barplot for Top 10 Twitter Source")
                        ),
                        
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("wordcloud", wordcloud2Output('wordcloud')),
                                      tabPanel("plot",plotOutput("plot") )
                          )
                          )
                        )
             ),

             tabPanel("Component 3")
  )
)
server <- function(input,output){
  getdata_var <- reactive({ get(input$var_input) })
  getdata2 <- reactive({ get(input$data_input2) })
  getdata1 <- reactive({ get(input$data_input) })
  
  output$plot_output_var<-renderPlot({
    bar_chart(getdata_var())
  }
    
  )
  
  
  ## worldcloud output
  output$wordcloud <- renderWordcloud2({
    #wordcloud2(getdata2(), size=input$size)
    letterCloud(getdata2(), size=input$size,word = "R", color='random-light' , backgroundColor="black")
  })
  ## barplot output
   output$plot <- renderPlot({
   ggplot(data=getdata1(),aes(x=source,y=Freq))+
   geom_bar(stat="identity",fill="steelblue")+
   ggtitle(isolate(input$title))+
   theme(
   axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=15),
   plot.title = element_text(color="steelblue", size=17, face="bold.italic")
   )
   })
  
 
}

shinyApp(ui=ui,server=server)