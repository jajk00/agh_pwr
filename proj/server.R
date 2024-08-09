
library(shiny)
library(dplyr)
library(zoo)
library(ggplot2)

att <- read.csv("att_d.csv", header = TRUE)
ccc <- read.csv("ccc_d.csv", header = TRUE)
ten <- read.csv("ten_d.csv", header = TRUE)
bdx <- read.csv("bdx_d.csv", header = TRUE)
gpw <- read.csv("gpw_d.csv", header = TRUE)
eur <- read.csv("eur_d.csv", header = TRUE)

att <- att %>%
  mutate(kod = "ATT") %>%
  select(kod, Data, Zamkniecie)

bdx <- bdx %>%
  mutate(kod = "BDX") %>%
  select(kod, Data, Zamkniecie)

ccc <- ccc %>%
  mutate(kod = "CCC") %>%
  select(kod, Data, Zamkniecie)

eur <- eur %>%
  mutate(kod = "EUR") %>%
  select(kod, Data, Zamkniecie)

gpw <- gpw %>%
  mutate(kod = "GPW") %>%
  select(kod, Data, Zamkniecie)

ten <- ten %>%
  mutate(kod = "TEN") %>%
  select(kod, Data, Zamkniecie)

dane <-bind_rows(att,bdx,ccc,eur,gpw,ten)

dane$kod <- as.factor(dane$kod)
dane$Data <- as.Date(dane$Data)

dane$YM <- format(dane$Data, "%Y-%B", tz = "GMT")
dane$YM <- as.yearmon(dane$YM, "%Y-%b")
daneM <- aggregate(Zamkniecie~YM + kod, data = dane, FUN = mean)

srednieM <- 



dane <- dane %>%
  group_by(kod) %>%
  mutate(MA1 = rollmean(Zamkniecie, k=23, fill=NA),
         MA2 = rollmean(Zamkniecie, k=45, fill=NA))

dane <- dane %>%
  group_by(kod) %>%
  mutate(log_ret = c(NA, diff(log(Zamkniecie), lag = 1)))

dane <- dane %>%
  group_by(kod) %>%
  mutate(log_ret_MA1 = rollmean(log_ret, k=23, fill=NA),
         log_ret_MA2 = rollmean(log_ret, k=45, fill=NA))

dane <- dane %>%
  group_by(kod) %>%
  mutate(ret = 100*c(NA, diff(Zamkniecie, lag = 1))/lag(Zamkniecie))

dane <- dane %>%
  group_by(kod) %>%
  mutate(ret_MA1 = rollmean(ret, k=23, fill=NA),
         ret_MA2 = rollmean(ret, k=45, fill=NA))

function(input, output, session) {
    
    output$plot1 <- renderPlot({
      spolki <- input$wyborSpolek
      poczatek <- input$zakresDat[1]
      koniec <- input$zakresDat[2]
      
      daneWykres <- dane %>%
        filter(kod %in% spolki) %>%
        filter(between(Data, poczatek, koniec))
      
      if(input$wyborBadania == "kursy zamknięcia") {
        ggplot(daneWykres, aes(x = Data, y = Zamkniecie, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Kursy zamknięcia wybranych spółek",
               x = "Data",
               y = "Kursy zamknięcia") +
          theme_minimal()
      }
      else if(input$wyborBadania == "średnie miesięczne") {
        # poczatekM <- as.yearmon(input$zakresDat[1], "%Y-%b")
        # koniecM <- as.yearmon(input$zakresDat[2], "%Y-%b")
        poczatekM <- as.yearmon(poczatek, "%Y-%b")
        koniecM <- as.yearmon(koniec, "%Y-%b")
        daneWykres <- daneM %>%
          filter(kod %in% spolki) %>%
          filter(between(YM, poczatekM, koniecM))
        
        ggplot(daneWykres, aes(x = YM, y = Zamkniecie, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Średnie miesięczne zamknięcia w wybranych spółkach",
               x = "Data",
               y = "Średnie miesięczne") +
          theme_minimal() +
          geom_point()
      } 
      else if(input$wyborBadania == "średnie ruchome miesięczne") {
        ggplot(daneWykres, aes(x = Data, y = MA1, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Średnie ruchome miesięczne zamknięcia w wybranych spółkach",
               x = "Data",
               y = "Średnie ruchome miesięczne") +
          theme_minimal() #+
          #geom_point()
      }
      else if(input$wyborBadania == "średnie ruchome dwumiesięczne") {
        ggplot(daneWykres, aes(x = Data, y = MA2, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Średnie ruchome dwumiesięczne zamknięcia w wybranych spółkach",
               x = "Data",
               y = "Średnie ruchome dwumiesięczne") +
          theme_minimal() #+
          #geom_point()
      }
      else if(input$wyborBadania == "logarytmiczne stopy zwrotu") {
        ggplot(daneWykres, aes(x = Data, y = log_ret, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Dzienne logarytmiczne stopy zwrotu w wybranych spółkach",
               x = "Data",
               y = "Logarytmiczne stopy zwrotu") +
          theme_minimal()
      }
      else if(input$wyborBadania == "średnie ruchome miesięczne dla log. stóp zwrotu") {
        ggplot(daneWykres, aes(x = Data, y = log_ret_MA1, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Średnie ruchome miesięczne dziennych logarytmicznych stóp zwrotu w wybranych spółkach",
               x = "Data",
               y = "Średnie ruchome miesięczne dla stóp zwrotu") +
          theme_minimal()
      }
      else if(input$wyborBadania == "średnie ruchome dwumiesięczne dla log. stóp zwrotu") {
        ggplot(daneWykres, aes(x = Data, y = log_ret_MA2, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Średnie ruchome dwumiesięczne dziennych logarytmicznych stóp zwrotu w wybranych spółkach",
               x = "Data",
               y = "Średnie ruchome dwumiesięczne dla stóp zwrotu") +
          theme_minimal()
      }
      else if(input$wyborBadania == "zwykłe stopy zwrotu (%)") {
        ggplot(daneWykres, aes(x = Data, y = ret, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Zwykłe dzienne stopy zwrotu w wybranych spółkach",
               x = "Data",
               y = "Stopy zwrotu (%)") +
          theme_minimal()
      }
      else if(input$wyborBadania == "średnie ruchome miesięczne dla zwykłych stóp zwrotu") {
        ggplot(daneWykres, aes(x = Data, y = ret_MA1, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Średnie ruchome miesięczne zwykłych dziennych stóp zwrotu w wybranych spółkach",
               x = "Data",
               y = "Średnie ruchome miesięczne dla stóp zwrotu (%)") +
          theme_minimal()
      }
      else if(input$wyborBadania == "średnie ruchome dwumiesięczne dla zwykłych stóp zwrotu") {
        ggplot(daneWykres, aes(x = Data, y = ret_MA2, group = kod, color = kod)) +
          geom_line() +
          labs(title = "Średnie ruchome dwumiesięczne zwykłych dziennych stóp zwrotu w wybranych spółkach",
               x = "Data",
               y = "Średnie ruchome dwumiesięczne dla stóp zwrotu (%)") +
          theme_minimal()
      }
      
      
    })
    
    output$plot2 <- renderPlot({
      spolka <- input$wyborSpolki
      poczatek <- input$zakresDat2[1]
      koniec <- input$zakresDat2[2]
      daneWykres <- dane %>%
        filter(kod == spolka) %>%
        filter(between(Data, poczatek, koniec))
      
      
      if(input$wyborZmiennej == "Zamknięcie a średnie ruchome") {
        plot(daneWykres$Data, daneWykres$Zamkniecie, type = "l",
             main="Wartości zamknięcia oraz miesięcznych i dwumiesięcznych \n średnich ruchomych zamknięcia",
             xlab="Data", ylab="Zamknięcie"
             )
        lines(daneWykres$Data, daneWykres$MA1, col="blue")
        lines(daneWykres$Data, daneWykres$MA2, col="red")
        legend(x = "topleft", 
               legend=c("zamknięcie", "średnia ruchoma miesięczna", "średnia ruchoma dwumiesięczna"),  
               col = c("black", "blue", "red"),
               lty=rep(1, 3), box.lty=1
               ) 
      }
      else if(input$wyborZmiennej == "Logarytmiczna stopa zwrotu a średnie ruchome") {
        plot(daneWykres$Data, daneWykres$log_ret, type = "l",
             main="Wartości dziennej logarytmicznej stopy zwrotu oraz \n miesięcznych i dwumiesięcznych średnich ruchomych dla stóp zwrotu",
             xlab="Data", ylab="Stopa zwrotu"
             )
        lines(daneWykres$Data, daneWykres$log_ret_MA1, col="blue")
        lines(daneWykres$Data, daneWykres$log_ret_MA2, col="red")
        legend(x = "topleft", 
               legend=c("stopa zwrotu", "średnia ruchoma miesięczna", "średnia ruchoma dwumiesięczna"),  
               col = c("black", "blue", "red"),
               lty=rep(1, 3), box.lty=1
        ) 
      }
      
      else if(input$wyborZmiennej == "Zwykła stopa zwrotu (%) a średnie ruchome") {
        plot(daneWykres$Data, daneWykres$ret, type = "l",
             main="Wartości zwykłej dziennej stopy zwrotu oraz \n miesięcznych i dwumiesięcznych średnich ruchomych dla stóp zwrotu",
             xlab="Data", ylab="Stopa zwrotu (%)"
        )
        lines(daneWykres$Data, daneWykres$ret_MA1, col="blue")
        lines(daneWykres$Data, daneWykres$ret_MA2, col="red")
        legend(x = "topright", 
               legend=c("stopa zwrotu", "średnia ruchoma miesięczna", "średnia ruchoma dwumiesięczna"),  
               col = c("black", "blue", "red"),
               lty=rep(1, 3), box.lty=1
        ) 
      }
    })

}
