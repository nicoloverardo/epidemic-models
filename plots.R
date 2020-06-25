contagiati_g1 <- vector("list", nrow(results))
contagiati_g2 <- vector("list", nrow(results))
contagiati_g3 <- vector("list", nrow(results))
contagiati_g1[1] <- 0
contagiati_g2[1] <- 0
contagiati_g3[1] <- 0

for (i in 2:nrow(results)){
  contagiati_g1[i] <- contagiati_g1[[i-1]]+(-(results$S1[i] - results$S1[i-1]))
  contagiati_g2[i] <- contagiati_g2[[i-1]]+(-(results$S2[i] - results$S2[i-1]))
  contagiati_g3[i] <- contagiati_g3[[i-1]]+(-(results$S3[i] - results$S3[i-1]))
}

contagiati_g1 <- do.call("rbind",contagiati_g1)
contagiati_g2 <- do.call("rbind",contagiati_g2)
contagiati_g3 <- do.call("rbind",contagiati_g3)

contagiati <- data.frame(contagiati_g1, contagiati_g2, contagiati_g3)

plot(results$time,
     contagiati$contagiati_g1/N[1],
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,1),
     lwd=2,
     col=2,
     main=paste("Contagiati",province))
lines(results$time,
      contagiati$contagiati_g2/N[2],
      col=3,
      lwd=2)
lines(results$time,
      contagiati$contagiati_g3/N[3],
      col=4,
      lwd=2)
legend("topright",
       legend=c("0-25","25-75", ">75"),
       col=c(2,3,4),
       lwd=2)

region <- "Valle d'Aosta"
y <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
y <- y[y$denominazione_regione==region,]
plot(seq(1,nrow(y)),
     y$totale_casi,
     type="l",
     xlab="days",
     ylab="Individuals",
     lwd=2,
     col=2,
     main=paste("Contagiati",region))


lines(seq(1,nrow(y)),
      y$totale_positivi,
      col=3,
      lwd=2)



### Plot alex

I1 <- results$I1
I2 <- results$I2
I3 <- results$I3

library(rjson)
library(plotly)
data <- ("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province-latest.json") 
data1 <- fromJSON(file=data)

p <-
  plot_ly(
    results,
    x = results$time,
    y =  I1+I2+I3,
    type = 'scatter',
    mode = 'line+markers',
    line = list(color = 'rgb(205, 12, 24)', width = 4)
  )
p <- p %>% add_trace(y =I1 , name = 'MEDIUM PREDICTION',mode = 'markers')
p <- p %>% add_trace(y =I2 , name = 'HIGH RISK PREDICTION',mode = 'markers')
p <- p %>% add_trace(y =I3 , name = 'LOW RISK PREDICTION',mode = 'markers')
p <- p %>% add_trace(y =I3 , name = 'LOW RISK PREDICTION',mode = 'markers')


p %>%
  layout(
    title = "Totacl Cases",
    xaxis = list(title = "Time"),
    yaxis = list (title = "Age Groups")
  )
p


xq <- c('High Risk', 'Medium Risk', 'Low Risk')
yq <- c(sum(results$D1), sum(results$D2), sum(results$D3))
text <- c('60 years and above', '30-60 years', '0-30')
data11 <- data.frame(xq, yq, text)

m <- plot_ly(data11, x = ~xq, y = ~yq, type = 'bar', text = text,
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5)))
m <- m %>% layout(title = "PREDICTED DEATH IN DIFFERENT AGE GROUPS",
                  xaxis = list(title = ""),
                  yaxis = list(title = ""))

m



l <- plot_ly(results, x = ~results$R1, y = ~results$S1, z = ~results$I1,
             marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
l <- l %>% add_markers()
l <- l %>% layout(scene = list(xaxis = list(title = 'High Risk Recovery'),
                               yaxis = list(title = 'High Risk Suspectible'),
                               zaxis = list(title = 'High Risk Infected')),
                  annotations = list(
                    x = 1.13,
                    y = 1.05,
                    text = 'Comparison High RISK SIR',
                    xref = 'Comparison',
                    yref = 'Comparison',
                    showarrow = FALSE
                  ))
l
