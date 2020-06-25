source("age-sird.R")
library(rjson)
library(plotly)

# Get predictions
results <- sirdModel()

### SIRD plots
par(mfrow<-c(1,1))
ymax = max(c(results$I1,results$I2, results$I3))

# I
plot(results$time,
     results$I1,
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,ymax),
     lwd=2,
     col=2,
     main="Age-structured SIR (I)")
lines(results$time,
      results$I2,
      col=3,
      lwd=2)
lines(results$time,
      results$I3,
      col=4,
      lwd=2)
legend("topright",
       legend=c("0-25","25-75", ">75"),
       col=c(2,3,4),
       lwd=2)

# S
plot(results$time,
     results$S1/N[1],
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,1),
     lwd=2,
     col=2,
     main="Age-structured SIR (S)")
lines(results$time,
      results$S2/N[2],
      col=3,
      lwd=2)
lines(results$time,
      results$S3/N[3],
      col=4,
      lwd=2)
legend("topright",
       legend=c("0-25","25-75", ">75"),
       col=c(2,3,4),
       lwd=2)

# R
plot(results$time,
     results$R1/N[1],
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,1),
     lwd=2,
     col=2,
     main="Age-structured SIR (R)")
lines(results$time,
      results$R2/N[2],
      col=3,
      lwd=2)
lines(results$time,
      results$R3/N[3],
      col=4,
      lwd=2)
legend("topright",
       legend=c("0-25","25-75", ">75"),
       col=c(2,3,4),
       lwd=2)

# D
plot(results$time,
     results$D1/N[1],
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,1),
     lwd=2,
     col=2,
     main="Age-structured SIR (D)")
lines(results$time,
      results$D2/N[2],
      col=3,
      lwd=2)
lines(results$time,
      results$D3/N[3],
      col=4,
      lwd=2)
legend("topright",
       legend=c("0-25","25-75", ">75"),
       col=c(2,3,4),
       lwd=2)

# ---------------------------------------------

# Other plots
get_contagiati_cumul <- function(results){
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
}

contagiati <- get_contagiati_cumul(results)

prov <- "Torino"
pcmprov <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
pcmprov <- pcmprov[pcmprov$denominazione_provincia==prov,]

mp <- plot_ly(results, 
              x = results$time, 
              y = (contagiati$contagiati_g1+contagiati$contagiati_g2+contagiati$contagiati_g3)/(N[1]+N[2]+N[3]),
              type = 'scatter',
              mode = 'lines+markers',
              line = list(color = 'rgb(205, 12, 24)', width = 4),
              name = "Prediction")
mp <- mp %>% add_trace(y = pcmprov$totale_casi/(N[1]+N[2]+N[3]), name = 'Real data', mode = 'markers')
mp <- mp %>%
  layout(
    title = paste("Cumulative cases",prov),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
mp

# --------------------------------------------

# Cumulative for region
reg <- "Piemonte"
pcmreg <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
pcmreg <- pcmreg[pcmreg$denominazione_regione==reg,]

res_torino <- sirdModel(province="Torino")
res_alessandria <- sirdModel(province="Alessandria")
res_asti <- sirdModel(province="Asti")
res_biella <- sirdModel(province="Biella")
res_cuneo <- sirdModel(province="Cuneo")
res_novara <- sirdModel(province="Novara")
res_vco <- sirdModel(province="Verbano-Cusio-Ossola")
res_vercelli <- sirdModel(province="Vercelli")

res_piemonte <- aggregate(. ~ time, rbind(res_torino,
                                          res_alessandria,
                                          res_asti,
                                          res_biella,
                                          res_cuneo,
                                          res_novara,
                                          res_vco,
                                          res_vercelli), sum)

contag_reg <- get_contagiati_cumul(res_piemonte)

lista_prov <- c("Torino", "Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Verbano-Cusio-Ossola", "Vercelli")
dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
N <- 0
for (p in lista_prov){
  data_prov <- dataistat[dataistat$Territorio == p,]
  N <- N + data_prov$Value[data_prov$Eta == "Total"]
}

mreg <- plot_ly(res_piemonte, 
              x = res_piemonte$time, 
              y = (contag_reg$contagiati_g1+contag_reg$contagiati_g2+contag_reg$contagiati_g3)/N,
              type = 'scatter',
              mode = 'lines+markers',
              line = list(color = 'rgb(205, 12, 24)', width = 4),
              name = "Prediction")
mreg <- mreg %>% add_trace(y = pcmreg$totale_casi/(N), name = 'Real data', mode = 'markers')
mreg <- mreg %>%
  layout(
    title = paste("Cumulative cases",reg),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
mreg

# --------------------------------------------
### Plot alex

I1 <- results$I1
I2 <- results$I2
I3 <- results$I3

data <- ("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province-latest.json") 
data1 <- fromJSON(file=data)

p <-
  plot_ly(
    results,
    x = results$time,
    y =  I1+I2+I3,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(color = 'rgb(205, 12, 24)', width = 4)
  )
p <- p %>% add_trace(y =I1 , name = 'MEDIUM PREDICTION',mode = 'markers')
p <- p %>% add_trace(y =I2 , name = 'HIGH RISK PREDICTION',mode = 'markers')
p <- p %>% add_trace(y =I3 , name = 'LOW RISK PREDICTION',mode = 'markers')
p <- p %>% add_trace(y =I3 , name = 'LOW RISK PREDICTION',mode = 'markers')


p <- p %>%
  layout(
    title = "Total Cases",
    xaxis = list(title = "Time"),
    yaxis = list (title = "Individuals")
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
