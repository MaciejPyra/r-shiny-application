# Necessary libraries

library(shiny)
library(shinythemes)
library(ggplot2)
library(lubridate)
library(googleVis)
library(tidyverse)


# User Interface


ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(
    title=tags$h1(tags$p(tags$em(tags$strong("Analysis of new cases of Covid-19 infection in Poland   ")), icon(name = "microscope", lib = "font-awesome")))),
  navbarPage(
    title=tags$p(tags$em(tags$strong("Bookmarks")), style="color:red; font-size:120%"),
    tabPanel(
      title=tags$h1(tags$em(tags$strong("Infections in Poland in total")), style="color:yellow; font-size:180%"),
              sidebarLayout(
               sidebarPanel(
                 tags$h1("The application is used to track new Covid-19 infections in Poland as part of the daily reports of the Ministry of Health. The first case of the virus
                          on the territory of Poland was detected on March 4, 2020. That day is considered as the beginning of the epidemic in the country.", style="font-size:99%; color:red"),
                 wellPanel(selectInput(inputId = "miesiac", label = "Please select a day for analysis", choices = seq.Date(as.Date("2020-03-04"),
                                                                                                                   as.Date("2020-06-10"),
                                                                                                                   by="day"),
                               selected = as.Date("2020-05-10")),
                   tags$br(),
                   tags$hr(),
                   tags$br(),
                   tags$p(tags$strong("Please select infection range for the above day:")),
                   tags$br(),
                   sliderInput(inputId = "indicator", label= "Please select range", min=0, max=453, value=c(100, 200)),
                   tags$br(),
                   checkboxInput(inputId = "tick", label = "Show data source", value = FALSE),
                   verbatimTextOutput(outputId = "source")
                  )
                 ),

              mainPanel(tags$hr(),
                        tags$br(),
                        tableOutput(outputId = "gap")
                        ))
             ),

    tabPanel(
      title = tags$h1(tags$em(tags$strong("Daily increase in infections")), style="color:yellow; font-size:180%"),
        sidebarLayout(
          sidebarPanel(
            tags$h1("The tab shows the daily increases in the number of infected people with a distinction between individual voivodeships. To show the daily increases in the whole country, please choose Poland.daily.",
                    style="color:red; font-size:99%"),
            wellPanel(tags$br(),
                      selectInput(inputId = "region", label = "Please select a region for the line graph", choices = list("dolnoslaskie", "kujawsko-pomorskie", "lubuskie", "lodzkie",
                                                                                                                          "lubelskie", "malopolskie", "mazowieckie", "opolskie", "podlaskie",
                                                                                                                          "podkarpackie", "pomorskie", "swietokrzyskie", "slaskie",
                                                                                                                          "warminsko-mazurskie", "wielkopolskie", "zachodniopomorskie", "Poland.daily")),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$hr(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      selectInput(inputId = "region2", label = "Please select a region for the bar graph", choices = list("dolnoslaskie", "kujawsko-pomorskie", "lubuskie", "lodzkie",
                                                                                                                            "lubelskie", "malopolskie", "mazowieckie", "opolskie", "podlaskie",
                                                                                                                            "podkarpackie", "pomorskie", "swietokrzyskie", "slaskie",
                                                                                                                            "warminsko-mazurskie", "wielkopolskie", "zachodniopomorskie", "Poland.daily")),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
            )
          ),
          mainPanel(tags$hr(),
                    tags$br(),
                    plotOutput(outputId = "plot1"),
                    tags$br(),
                    tags$hr(),
                    tags$br(),
                    plotOutput(outputId = "plot2")
          )
        )
    ),

    tabPanel(
      title = tags$h1(tags$em(tags$strong("Cumulative number of infections")), style="color:yellow; font-size:180%"),
      sidebarLayout(
        sidebarPanel(
          tags$h1("The tab shows the cumulative number of infected for a given day with a distinction between individual voivodeships. To show the cumulative number for the whole country, please choose Poland.total.",
                 style="color:red; font-size:99%"),
          wellPanel(tags$br(),
                    selectInput(inputId = "region3", label = "Please select a region for the line graph", choices = list("dolnoslaskie", "kujawsko-pomorskie", "lubuskie", "lodzkie",
                                                                                                                        "lubelskie", "malopolskie", "mazowieckie", "opolskie", "podlaskie",
                                                                                                                        "podkarpackie", "pomorskie", "swietokrzyskie", "slaskie",
                                                                                                                        "warminsko-mazurskie", "wielkopolskie", "zachodniopomorskie", "Poland.total")),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$hr(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    selectInput(inputId = "region4", label = "Please select a region for the bar graph", choices = list("dolnoslaskie", "kujawsko-pomorskie", "lubuskie", "lodzkie",
                                                                                                                          "lubelskie", "malopolskie", "mazowieckie", "opolskie", "podlaskie",
                                                                                                                          "podkarpackie", "pomorskie", "swietokrzyskie", "slaskie",
                                                                                                                          "warminsko-mazurskie", "wielkopolskie", "zachodniopomorskie", "Poland.total")),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
          )
        ),
        mainPanel(tags$hr(),
                  tags$br(),
                  plotOutput(outputId = "plot3"),
                  tags$br(),
                  tags$hr(),
                  tags$br(),
                  plotOutput(outputId = "plot4")
                )
        )
      ),
    tabPanel(
      title=tags$h1(tags$em(tags$strong("Infections by individual voivodships - day")), style="color: yellow; font-size:180%"),
      sidebarLayout(
        sidebarPanel(
          tags$h1("The tab presents the increases in the number of infected people for a given day broken down into individual voivodeships. Analysis is shown in the form of an interactive map of Poland.
                   Please select a specific date and click the Update button.",
                  style="color:red; font-size:99%"),
          wellPanel(selectInput(inputId = "date_var1", label = "Please select a specific date", choices = seq.Date(as.Date("2020-03-04"),
                                                                                                               as.Date("2020-06-10"),
                                                                                                               by="day"),
                    selected = as.Date("2020-06-10")),
                    tags$br(),
                    tags$hr(),
                    tags$p(tags$strong("Press the button to generate the map:")),
                    tags$br(),
                    actionButton(inputId = "click", label = "Update")
          )),
        mainPanel(tags$hr(),
                  tags$h1(tags$em(tags$strong("Daily infections in individual voivodeships:")), style="color: yellow; font-size:150%"),
                  htmlOutput(outputId = "plot5")
                 )
        )
      ),
    tabPanel(
      title=tags$h1(tags$em(tags$strong("Infections by individual voivodships - range")), style="color: yellow; font-size:180%"),
      sidebarLayout(
        sidebarPanel(
          tags$h1("The tab presents the increases in the number of infected people within a defined period of days distinguishing between individual voivodships. Analysis is shown in the form of an interactive map of Poland.
                   Please define the date range and click the Update button.",
                  style="color:red; font-size:99%"),
          wellPanel(dateRangeInput(inputId = "range_var", label = "Please select a date range", start = as.Date("2020-06-03"), end=as.Date("2020-06-10"),
                                   min=as.Date("2020-03-06"), max=as.Date("2020-06-10"), language = "pl", weekstart = 1),
                    tags$br(),
                    tags$hr(),
                    tags$p(tags$strong("Press the button to generate the map:")),
                    tags$br(),
                    actionButton(inputId = "click2", label = "Update"))
        ),
        mainPanel(tags$hr(),
                  tags$h1(tags$em(tags$strong("infections in individual voivodeships for defined date range:")), style="color: yellow; font-size:150%"),
                  htmlOutput(outputId ="plot6"))
      )
    )


    
    )
)
    



# Server


server<-function(input, output){


table<-read.csv("Covid.csv", header=TRUE, sep=";")

Sys.setlocale("LC_TIME", "C")
table[,1]<-as.Date(table[,1], format="%d %B %Y")
colnames(table)[colnames(table)=="kujawsko.pomorskie"]<-"kujawsko-pomorskie"
colnames(table)[colnames(table)=="warminsko.mazurskie"]<-"warminsko-mazurskie"
table
maximum<-max(sapply(table[, -c(1, 18, 19)], function(x) {max(x)}))


#Table with daily increases


table1<-table[, 1:17]
table1
table_interaction<-pivot_longer(data=table1, cols=-Date, names_to="Voivodeship", values_to = "Infections")



table_interaction2<-reactive({
  table_interaction[which(table_interaction$Date==input$miesiac & table_interaction$Infections >= input$indicator[1] & table_interaction$Infections <= input$indicator[2]), -1]
})

output$gap <-renderTable({
  return(table_interaction2())
})



output$source<-renderText({
if(input$tick){
  link<-"https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Poland"
  return(link)
}
})


# Daily increases for all provinces


output$plot1<- renderPlot({
  wyk1<-(ggplot(data = table, mapping = aes(x=table$Date, y=table[,which(colnames(table)==input$region)]))
                    +
                      geom_line(size=2)
                    +
                      xlab("Day")
                    +
                      ylab("Daily number of new cases")
                    +
                      ggtitle(paste0("Daily increase in new infections in ", ifelse(input$region=="Poland.daily", "the whole country:", "voivodeship "), ifelse(input$region=="Poland.daily", "", input$region)))
                    +
                      scale_y_continuous(breaks = c(10, 30, 50, 75, 100, 200, 300, 400, 500, 600))
                    +
                      geom_hline(yintercept = c(10, 30, 50, 75, 100, 200, 300, 400, 500, 600), linetype=2)
                    +
                      theme_classic()
                    +
                      theme(axis.title = element_text(face=c("bold"), colour="red4", hjust = 0.5 ,size=15))
                    +
                      theme(title = element_text(hjust= 0.5, face="bold", colour = "red4", size=15), panel.background = element_rect(fill="darkolivegreen1"), plot.background = element_rect(fill="darkkhaki"))
                    
)
  return(wyk1)
})




output$plot2<- renderPlot({
op<-par(bg="antiquewhite")
barplot(table[,which(colnames(table)==input$region2)], ylab = "Daily number of new cases", xlab="Day", names.arg = table$Date, col="orange", ylim = c(0, 610), main = paste0("Daily increase in new infections in ", ifelse(input$region=="Poland.daily", "the whole country:", "voivodeship "), ifelse(input$region=="Poland.daily", "", input$region)), col.main="red", col.lab="red", font.main=12, font.lab=6, axes = FALSE)
axis(side=2, at=c(10, 50, 75, 100, 200, 300, 400, 500, 600))
abline(h=c(10, 30, 50, 75, 100, 200, 300, 400, 500, 600))
wyk2<-recordPlot()
par(op)
dev.off()
return(wyk2)
})


# The total cumulative number of all infections for individual voivodships


sums<-lapply(table[,2:17], function(x){cumsum(x)})


funkcja<-function(){
for (i in 1:16){
p<-unlist(sums[[i]])
nazwa<-names(sums)[i]
o<-data.frame(kolumna=p)
names(o)<-nazwa
if (i>1){
  b<-cbind(b, o)
} else{
  b<-o
}
}
return(b)
}


table_cumulation<-funkcja()
table_cumulation<-cbind(Date=table$Date, table_cumulation, Poland.total=table$Poland.total)



output$plot3<-renderPlot({
  wyk3<-(ggplot(data = table, mapping = aes(x=table_cumulation$Date, y=(table_cumulation[,which(colnames(table_cumulation)==input$region3)])))
        +
          geom_line(size=2)
        +
          xlab("Day")
        +
          ylab("Number of cases")
        +
          ggtitle(paste0("Cumulative number of infections in ", ifelse(input$region3=="Poland.total", "the whole country:", "voivodeship "), ifelse(input$region3=="Poland.total", "", input$region3)))
        +
          geom_hline(yintercept = c(300, 1000, 2000, 3000, 4000, 5000, 7500, 10000, 15000, 20000, 25000,28000), linetype=2)
        +
          scale_y_continuous(breaks = c(300, 1000, 2000, 3000, 4000, 5000, 7500, 10000, 15000, 20000, 25000,28000))
        +
          theme_classic()
        +
          theme(axis.title = element_text(face=c("bold"), colour="red4", hjust = 0.5 ,size=15))
        +
          theme(title = element_text(hjust= 0.5, face="bold", colour = "red4", size=15), panel.background = element_rect(fill="darkolivegreen1"), plot.background = element_rect(fill="darkkhaki"))
        
)
  return(wyk3)
})



output$plot4<-renderPlot({
  op<-par(bg="antiquewhite")
  barplot(table_cumulation[,which(colnames(table_cumulation)==input$region4)], ylab = "Number of cases", xlab="Day", names.arg = table_cumulation$Date, col="orange", main = paste0("Cumulative number of infections in ", ifelse(input$region4=="Poland.total", "the whole country:", "voivodeship "), ifelse(input$region4=="Poland.total", "", input$region4)), col.main="red", col.lab="red", font.main=12, font.lab=6, axes=FALSE)
  axis(side=2, at=c(100, 300, 600, 1000, 2000, 3000, 4000, 5000, 7500, 10000, 15000, 20000, 25000,28000))
  abline(h=c(100, 300, 600, 1000, 2000, 3000, 4000, 5000, 7500, 10000, 15000, 20000, 25000,28000))
  wyk4<-recordPlot()
  par(op)
  dev.off()
  return(wyk4)
})


# Infections chart depending on a day and province


table_interaction1<-reactive({
  table_interaction[which(table_interaction$Date==input$date_var1),]
})



reaction<-eventReactive(input$click, {
  GeoStates1<-gvisGeoChart(data=table_interaction1(), locationvar="Voivodeship", colorvar = "Infections", options=list(region="PL",
                                                                                                                         displayMode="regions",
                                                                                                                         resolution="provinces",
                                                                                                                         width=1000,
                                                                                                                         height=1000))
})


output$plot5<-renderGvis({
  return(reaction())
})


# Infections chart depending on a day range and province


reaction2<-eventReactive(input$click2, {
  data_start<-format(input$range_var[1])
  data_end<-(format(input$range_var[2]))
table_range1<-table1[which(table1$Date <= data_start),]
table_range2<-table1[which(table1$Date <= data_end),]
range1<-lapply(table_range1[,-1], function(x){sum(x)})
range2<-lapply(table_range2[,-1], function(x){sum(x)})
diff_var<-unlist(range2)-unlist(range1)
z<-as.vector(diff_var)
w<-names(range1)
table_final<-data.frame(Voivodeship=w ,Infections=z)


GeoStates2<-gvisGeoChart(data=table_final, locationvar="Voivodeship", colorvar = "Infections", options=list(region="PL",
                                                                                                                 displayMode="regions",
                                                                                                                 resolution="provinces",
                                                                                                                 width=1000,
                                                                                                                 height=1000))
return(GeoStates2)
})


output$plot6<-renderGvis({
  return(reaction2())
})


}

shinyApp(ui=ui, server=server)


