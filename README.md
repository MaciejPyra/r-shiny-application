# R Shiny Web Application

The aim of the project was to develop interactive web app in R programmic language using Shiny package. I prepared an application presenting the number of people infected with the Covid-19 virus in Poland during the first wave in 2020 (March - June). It is possible to analyze infections both nationwide and in relation to all 16 voivodships (provinces) in the country.

Please note that repo includes final source code and one Excel file called "Covid.csv" with source data which is necessary to perform analysis on the server side of application.


# Table of Contents

  * [Introduction](#intro)
     * [What is Shiny application?](#intro1)
     * [Data Source](#intro2)
     * [Technology Stack](#intro3)
  * [Program](#desc)
     * [How does it work?](#desc1)
     * [Application architecture](#desc2)


<a name="intro"></a>
<a name="intro1"></a>
## Intorduction
### What is Shiny application?

The Shiny application consists of two basic elements:
- the user interface which determines the form of results presentation on website,
- the server function which defines the actual operation of the entire application.
 
Both are arguments to the shinyApp function that actually runs the app. The script in which the application was written is called app.R (it is also possible to write the application in two different files: ui.r and server.r). Generally, shiny is designed to create very interactive websites and dashboard.

Importantly, the Shiny application tracks exactly what happens to the input parameters, so each time the input parameter is changed, the code in R on the server side will update all instructions in which the above variable appeared. Thanks to this, it is possible to view the results for various combinations of parameter values. Additionally, the results change interactively after modifying the values of individual controls.

<a name="intro2"></a>
### Data Source
Source data for infections analysis stems from https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Poland. It is Wikipedia article concerning about the spread of the Covid-19 pandemic in Poland. In turn, the abovementioned website got the data directly from daily reports of the Ministery of Health, government organization responsible for controlling the epidemic on the territory of Poland.

<a name="intro3"></a>
### Technology Stack
* R programmic language,
* R Shiny -> combines the computational power of R with the interactivity of the modern web app,
* tidyverse -> collection of R packages designed for data science, like ggplot2 for creating graphics,
* other popular R libraries, like lubridate -> to perform specific subtaks, for example to handle date-times in an easier way; 


<a name="desc"></a>
## Program

<a name="desc1"></a>
### How does it work?
User interface part of the app creates a web dashboard and accurately describes the entire appearance of ui. UI function describes, step by step, individual components appearing directly in the web document. The elements of the ui function are the input and output functions. The input part allows you to present elements on the HTML page that allow the user to select various data processing parameters.

On the other hand, the output functions embed in the html interface objects that were previously calculated as part of the server part of app.

In turn, server defines individual relations between the input elements (i.e. elements defined directly by the application user) and output (i.e. elements calculated by the application and prepared to be presented as part of the user interface). The server function determines what the entire application should do in relation to user commands. What is the most important, all calculations and actual data processing take place only on server side of the application.

<a name="desc2"></a>
### Application architecture
The application has been divided into five different tabs (navbarPage () instruction in the application UI). In each of the tabs there are appropriate input functions that allow the user to manually select respective parameter values. Additionally, there are output functions that represent the elements previously generated on the application server side.

Construction of the application:
- tab 1 - three input functions: selectInput () to select a specific date, sliderInput () to select the number of infections and chekboxInput () to define whether to show the data source. On the output element side there is an infection table presented by tableOutput () and a link to the data source presented by verbatimTextOutput ();
- tab 2 - two selectInput () input functions allowing to select a given province from the drop-down list and two graphs (one created in the ggplot2 package, the other in base R) presented by plotOutput ();
- tab 3 - two selectInput () input functions allowing to select a given province from the drop-down list and two graphs (one created in the ggplot2 package, the other in base R) presented by plotOutput ();
- tab 4 - two input functions - one selectInput () for selecting a specific date from the drop-down list and actionButton () for initiating a specific action, in this case generating a new interactive map of infections. On the output function side there is htmlOutput () which represents an element from the googleVis package (interactive map);
- tab 5 - two input functions - one dataRangeInput () for selecting a specific time interval from the calendar and actionButton () for initiating a specific action, in this case generating a new interactive map. On the output function side there is htmlOutput () which represents an element from the googleVis package (interactive map);

