#A Shiny Movie Dashboard

# Libraries
library("shiny")
library("lubridate")
library(dplyr)
library(ggplot2)
library('ggiraph')

# Importing the dataset
sales <- read.csv(file.choose())
names(sales)

# Turning letters from Lower to Upper case.
lower_case = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
upper_case = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

# Turning the language characters into upper case.
upperise<-function(word, lower = lower_case, upper = upper_case){
  letters = strsplit(word, split = "")
  for(i in seq(1,length(letters[[1]]),1)){
    for(j in seq(1,length(lower_case),1)){
      if(letters[[1]][i] == lower_case[j]){
        letters[[1]][i] = upper_case[j]
      }
    }
  }
  return (paste0(letters[[1]][1],letters[[1]][2]))
}

sales$original_language = apply(matrix(sales$original_language, nrow = nrow(sales), ncol = 1), MARGIN = 1 ,FUN = upperise)


# If needed, shorten the Film Titles.
shorten_titles <- function(title, limit = 6){
  words = strsplit(title, split = " ")
  word_list = c()
  if (length(words[[1]]) > limit){
    words[[1]] = words[[1]][1:limit]
    for (i in seq(length(words[[1]]))){
      word_list[i] = words[[1]][i]
      word_list[(length(word_list))+1] = "..."
    }
  }
  else{
    for(i in seq(length(words[[1]]))){
      word_list[i] = words[[1]][i]
    } 
  }
  return(paste(word_list, collapse = " "))
}

sales$title1 = apply(matrix(sales$title, nrow = nrow(sales), ncol = 1), MARGIN = 1 ,FUN = shorten_titles)


# Creating a Year variable.
sales = 
  sales%>%
  mutate(year = year(as.Date(sales$release_date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
                             optional = FALSE)))

# Omitting Empty Film Genre Input.
unique(sales$genres)
which(sales$genres == "")#Problem
sales = sales%>%filter(genres!="")


#Tooltip for Interactive Text
sales$tooltip = paste("Film:",format(sales$title, big.mark = ","),'\n',
                      "Budget:", format(sales$budget, big.mark = ","),"\n",
                      "Revenue:", format(sales$revenue, big.mark = ","))

# Fonts, colors of tabs
css <- '.nav-tabs>li>a {
  font-family: "Times", sans-serif;
  color: black;}'


ui<- fluidPage(tags$head(tags$style(HTML(css))),
               titlePanel(h1("Film Stats",style = "font-family:Times;font-weight:bold")),
               fluidRow(
                 column(3,
                        wellPanel(
                          sliderInput("year",
                                      h5("Year:", style = "font-family:Times;font-weight:bold"),
                                      min = min(sales$year),
                                      max = max(sales$year), 
                                      value = c(1999,2000),
                                      width = '400px',
                                      ticks = FALSE,
                                      animate = TRUE,
                                      sep = ""
                          ),
                          selectInput("new_genres",
                                      h5("Genre:",style = "font-family:Times;font-weight:bold"),
                                      width = '200px',
                                      selected = "Comedy",
                                      choices = unique(as.character(sales$new_genres)))
                        )
                 ),
                 column(2,wellPanel(radioButtons('average', h5('Average Line:', style = "font-family:Times;font-weight:bold"), 
                                                 choices=c('On', 'Off'), inline=FALSE),
                                    style = "vertical-align:bottom")
                 )
               ),
               sidebar(
                 column(3,
                        wellPanel(
                          textInput("director", h5("Director(eg.Peckinpah)",style = "font-family:Times;font-weight:bold")),
                          textInput("cast", h5("Cast(eg.Moore)",style = "font-family:Times;font-weight:bold"))))),
               mainPanel(
                 wellPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel(h4("Popularity"), uiOutput("plots", width = "auto", height="auto")),
                               tabPanel(h4("Budget"), uiOutput("plots1",width = "1200px", height="1200px")),
                               tabPanel(h4("Revenue"), uiOutput("plots2", width = "1200px", height="1200px")),
                               tabPanel(h4("Scatter"), girafeOutput("scatter", width = "1000px", height = "1000px"))
                   ), style = "background:white"
                 )
               ) 
)

sales%>%
  filter(production_countries == "United Kingdom")

th<-theme(panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill = 'transparent',colour = 'black'),
          panel.grid = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = 'dotted', colour = 'black'),
          axis.title.x = element_text(face = 'bold', margin = margin(t = 25,b = 15),
                                      family = 'Times', size = 15, colour = 'black'),
          axis.title.y = element_text(face = 'bold', margin = margin(l = 0, r = 25), colour = 'black', 
                                      angle = 0, hjust = 0, size = 16),
          axis.text.y = element_text(face = 'bold', size = 11),
          axis.text.x = element_text(face = 'bold', size = 12, angle = 0),
          plot.title = element_text(hjust = 0,size = 20,face = 'bold', margin = margin(b = 30, t = 25), family = 'SF Pro Display',
                                    colour = 'black'),
          #plot.margin = margin(r = 5, t = 4, b = 0, l = 1),
          #plot.caption = element_text(size = 12, colour = '#00094B', face = 'bold.italic', margin = margin(t = 0, b = 0)),
          #plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(t=-15,b = 35),
          #colour = '#140038', family = 'SF Pro Display', face = 'plain'),
          axis.ticks.y = element_blank(),
          legend.position = 'none')

th1<-theme(panel.background = element_rect(fill = 'transparent'),
           plot.background = element_rect(fill = 'transparent',colour = 'white'),
           #panel.grid = element_blank(),
           axis.line.x = element_line(linetype = 'solid', arrow = arrow()),
           axis.line.y = element_line(linetype = 'solid', arrow = arrow()),
           axis.ticks = element_line(linetype = 'solid'),
           axis.ticks.length = unit(.25, "cm"),
           panel.grid.major.y =  element_line(linetype = 'dotted', colour = 'black'),
           panel.grid.major.x = element_line(linetype = 'dotted', colour = 'black'),
           axis.title.x = element_text(face = 'bold', margin = margin(t = 25,b = 15),
                                       size = 15, colour = 'black'),
           axis.title.y = element_text(face = 'bold', margin = margin(l = 0, r = 20), colour = 'black', 
                                       angle = 90, size = 15),
           axis.text.y = element_text(face = 'bold', size = 12, colour = "black"),
           axis.text.x = element_text(face = 'bold', size = 12, angle = 0, colour = "black"),
           plot.title = element_text(hjust = 0,size = 20,face = 'bold', margin = margin(b = 30, t = 25),
                                     colour = 'black'),
           #plot.margin = margin(r = 5, t = 4, b = 0, l = 1),
           #plot.caption = element_text(size = 12, colour = '#00094B', face = 'bold.italic', margin = margin(t = 0, b = 0)),
           #plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(t=-15,b = 35),
           #colour = '#140038', family = 'SF Pro Display', face = 'plain'),
           legend.position = 'bottom',
           legend.text = element_text(size = 12,colour = 'black'),
           legend.title = element_text(size = 14, colour = 'black'),
           legend.box.just = "center",
           legend.title.position = "top")


server<-function(input,output, session){
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$barplot1 = renderPlot({
    sales%>%
      filter((year>= input$year[1]) & (year <= input$year[2]) & (grepl(input$new_genres, genres)) &
               grepl(input$cast,cast) & grepl(input$director,director))%>%
      ggplot(aes(x = reorder(title1, popularity), y = popularity, fill = popularity))+
      geom_col(colour = "black", width = 0.3)+
      th+
      scale_fill_gradient(low = 'black', high = 'blue3')+
      labs(title = 'Most Popular Films',y = 'Popularity',x = 'Title')+
      coord_flip()#caption = '', subtitle = "Date Range :")
  })
  
  output$barplot11 = renderPlot({
    sales%>%
      filter((year>= input$year[1]) & (year <= input$year[2]) & (grepl(input$new_genres, genres)) &
               grepl(input$cast,cast) & grepl(input$director,director))%>%
      ggplot(aes(x = reorder(title1, popularity), y = popularity, fill = popularity))+
      geom_col(colour = "black", width = 0.3)+
      geom_hline(aes(yintercept = mean(popularity) ))+
      th+
      scale_fill_gradient(low = 'black', high = 'blue3')+
      labs(title = 'Most Popular Films',y = 'Popularity',x = 'Title')+
      coord_flip()#caption = '', subtitle = "Date Range :")
  })
  
  output$plots = renderUI({
    if(input$average == "On"){
      plotOutput('barplot11',width = "1000px", height="1200px")}
    else{plotOutput('barplot1',width = "1000px", height="1200px")}
  })
  
  output$barplot2 = renderPlot({
    sales%>%
      filter((year>= input$year[1]) & (year <= input$year[2]) & (grepl(input$new_genres, genres)) &
               grepl(input$cast,cast) & grepl(input$director,director))%>%
      ggplot(aes(x = reorder(title1, budget), y = budget/10^6, fill = budget))+
      geom_col(colour = "black", width = 0.3)+
      th+
      scale_fill_gradient(low = 'black', high = 'firebrick')+
      labs(title = 'Most Expensive Films',y = 'Budget, in million $',x = 'Title')+
      coord_flip()#caption = '', subtitle = "Date Range :")
  })
  
  output$barplot22 = renderPlot({
    sales%>%
      filter((year>= input$year[1]) & (year <= input$year[2]) & (grepl(input$new_genres, genres)) &
               grepl(input$cast,cast) & grepl(input$director,director))%>%
      ggplot(aes(x = reorder(title1, budget), y = budget/10^6, fill = budget))+
      geom_col(colour = "black", width = 0.3)+
      geom_hline(aes(yintercept = mean(budget)/10^6))+
      th+
      scale_fill_gradient(low = 'black', high = 'firebrick')+
      labs(title = 'Most Expensive Films',y = 'Budget, in million $',x = 'Title')+
      coord_flip()#caption = '', subtitle = "Date Range :")
  })
  
  output$plots1 = renderUI({
    if(input$average == "On"){
      plotOutput('barplot22',width = "1000px", height="1200px")}
    else{plotOutput('barplot2',width = "1000px", height="1200px")}
  })
  
  
  output$barplot3 = renderPlot({
    sales%>%
      filter((year>= input$year[1]) & (year <= input$year[2]) & (grepl(input$new_genres, genres)) &
               grepl(input$cast,cast) & grepl(input$director,director))%>%
      ggplot(aes(x = reorder(title1, revenue), y = revenue/10^6, fill = revenue))+
      geom_col(colour = "black", width = 0.3)+
      th+
      scale_fill_gradient(low = 'black', high = 'grey')+
      labs(title = 'Most Profitable Films',y = 'Revenue, in million $',x = 'Title')+
      coord_flip()#caption = '', subtitle = "Date Range :")
  })
  
  output$barplot33 = renderPlot({
    sales%>%
      filter((year>= input$year[1]) & (year <= input$year[2]) & (grepl(input$new_genres, genres)) &
               grepl(input$cast,cast) & grepl(input$director,director))%>%
      ggplot(aes(x = reorder(title1, revenue), y = revenue/10^6, fill = revenue))+
      geom_col(colour = "black", width = 0.3)+
      geom_hline(aes(yintercept = mean(revenue)/10^6))+
      th+
      scale_fill_gradient(low = 'black', high = 'grey')+
      labs(title = 'Most Profitable Films',y = 'Revenue, in million $',x = 'Title')+
      coord_flip()#caption = '', subtitle = "Date Range :")
  })
  
  output$plots2 = renderUI({
    if(input$average == "On"){
      plotOutput('barplot33',width = "1000px", height="1200px")}
    else{plotOutput('barplot3',width = "1000px", height="1200px")}
  })
  
  output$scatter = renderGirafe({
    x <- 
      sales%>%
      filter((year>= input$year[1]) & (year <= input$year[2]) & (new_genres == input$new_genres)&
               grepl(input$cast,cast) & grepl(input$director,director))%>%
      ggplot(aes(x = budget/(10^6), y = revenue/(10^6), size = vote_average, color = popularity, tooltip = tooltip,
      ))+
      geom_point_interactive()+
      geom_hline(aes(yintercept = mean(revenue)/10^6))+
      geom_vline(aes(xintercept = mean(budget)/10^6))+
      #geom_label_interactive(aes(tooltip = paste(title, year , sep = "\n")))+
      th1+
      labs(x = 'Budget, in million $', y = "Revenue, in million $", title = "Revenue versus Budget | Interactive")+
      scale_color_gradient("Popularity",low = 'black', high = 'firebrick')+
      scale_size("Vote Average")
    
    girafe(ggobj = x,height_svg = 10,
           width_svg = 12)
    
  })
  output$scatter1 = renderPlot({
    sales%>%
      filter((year>= input$year[1]) & (year <= input$year[2]) & (new_genres == input$new_genres)&
               grepl(input$cast,cast) & grepl(input$director,director))%>%
      ggplot(aes(x = budget/(10^6), y = revenue/(10^6), size = vote_average, color = popularity))+
      geom_point()+
      geom_hline(aes(yintercept = mean(revenue)/10^6))+
      geom_vline(aes(xintercept = mean(budget)/10^6))+
      th1+
      labs(x = 'Budget, in million $', y = "Revenue, in million $",title = "Revenue versus Budget")+
      scale_color_gradient("Popularity",low = 'black', high = 'firebrick')+
      scale_size("Vote Average")
  })
  output$plots3 = renderUI({
    if(input$average == "On"){
      plotOutput('scatter1',width = "800px", height="800px", hover = hoverOpts(id ="plot_hover"))}
    else{plotOutput('scatter',width = "800px", height="800px", hover = hoverOpts(id ="plot_hover"))}
  })
}

shinyApp(ui, server)
