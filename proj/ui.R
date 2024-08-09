
library(shiny)
library(shinythemes)

navbarPage(
  theme = shinytheme("sandstone"),
  title = "Filtrowanie szeregów",
  tabPanel("Porównanie spółek",
           #titlePanel(""),
           
           sidebarLayout(
             sidebarPanel(
               dateRangeInput(
                 inputId = "zakresDat",
                 label = "Wybierz okres: ",
                 start  = "2018-01",
                 end    = "2022-12",
                 min    = "2010-01",
                 max    = "2023-09",
                 format = "yyyy-mm",
                 startview = "decade"
               ),
               
               selectInput(
                 inputId = "wyborBadania",
                 label = "Badana wielkość:",
                 choices = c(
                   "kursy zamknięcia",
                   "średnie miesięczne",
                   "średnie ruchome miesięczne",
                   "średnie ruchome dwumiesięczne",
                   "logarytmiczne stopy zwrotu",
                   "średnie ruchome miesięczne dla log. stóp zwrotu",
                   "średnie ruchome dwumiesięczne dla log. stóp zwrotu",
                   "zwykłe stopy zwrotu (%)",
                   "średnie ruchome miesięczne dla zwykłych stóp zwrotu",
                   "średnie ruchome dwumiesięczne dla zwykłych stóp zwrotu"
                 ),
                 selected = "kursy zamknięcia"
               ),
               
               checkboxGroupInput(
                 inputId = "wyborSpolek",
                 label = "Wyświetlane spółki:",
                 choices = c("ATT", "GPW", "EUR", "BDX", "TEN", "CCC"),
                 selected = c("ATT", "GPW", "EUR", "BDX", "TEN", "CCC")
               )
             ),
             
             mainPanel(
               plotOutput("plot1")
             )
           )
         ),
  tabPanel("Analiza spółki",
           sidebarLayout(
             sidebarPanel(
               
               dateRangeInput(
                 inputId = "zakresDat2",
                 label = "Wybierz okres: ",
                 start  = "2018-01",
                 end    = "2022-12",
                 min    = "2010-01",
                 max    = "2023-09",
                 format = "yyyy-mm",
                 startview = "decade"
               ),
               
               selectInput(
                 inputId = "wyborZmiennej",
                 label = "Badana zmienna:",
                 choices = c("Zamknięcie a średnie ruchome",
                             "Logarytmiczna stopa zwrotu a średnie ruchome",
                             "Zwykła stopa zwrotu (%) a średnie ruchome"
                             ),
                 selected = "Zamknięcie a średnie ruchome"
               ),
               
               selectInput(
                 inputId = "wyborSpolki",
                 label = "Badana spółka:",
                 choices = c("ATT", "GPW", "EUR", "BDX", "TEN", "CCC")
               )
             ),
             
             mainPanel(
               plotOutput("plot2")
             )
           )
         )
)

