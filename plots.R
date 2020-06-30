source("age-sird.R")
library(plotly)

# Will need to increment this every day
days = 127

# Get predictions
province <- "Torino"
results <- sirdModel(province=province, days=days)

dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
data_prov <- dataistat[dataistat$Territorio == province,]
pop <- data_prov$Value[data_prov$Eta == "Total"]
class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]

# number in each age class
N <- pop*class_percent

### SIRD plots
par(mfrow<-c(1,1))
ymax = max(c(results$I1/N[1],results$I2/N[2], results$I3/N[3]))

# I
I <- plot_ly(results, 
              x = results$time, 
              y = results$I1/N[1],
              type = 'scatter',
              mode = 'lines',
              line = list(color = 'rgb(205, 12, 24)', width = 4),
              name = "0-25")
I <- I %>% add_trace(y = results$I2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
I <- I %>% add_trace(y = results$I3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
I <- I %>%
  layout(
    title = paste("Age-structured SIRD",province,"- Infected"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
I

# D
D <- plot_ly(results, 
             x = results$time, 
             y = results$D1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
D <- D %>% add_trace(y = results$D2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
D <- D %>% add_trace(y = results$D3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
D <- D %>%
  layout(
    title = paste("Age-structured SIRD",province,"- Deaths"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
D

# S
S <- plot_ly(results, 
             x = results$time, 
             y = results$S1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
S <- S %>% add_trace(y = results$S2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
S <- S %>% add_trace(y = results$S3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
S <- S %>%
  layout(
    title = paste("Age-structured SIRD",province,"- Susceptible"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
S

# R
R <- plot_ly(results, 
             x = results$time, 
             y = results$R1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
R <- R %>% add_trace(y = results$R2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
R <- R %>% add_trace(y = results$R3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
R <- R %>%
  layout(
    title = paste("Age-structured SIRD",province,"- Recovered"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
R

###### OLD PLOTS
# 
# # I
# plot(results$time,
#      results$I1/N[1],
#      type="l",
#      xlab="days",
#      ylab="% Individuals",
#      ylim=c(0,ymax),
#      lwd=2,
#      col=2,
#      main="Age-structured SIR (I)")
# lines(results$time,
#       results$I2/N[2],
#       col=3,
#       lwd=2)
# lines(results$time,
#       results$I3/N[3],
#       col=4,
#       lwd=2)
# legend("topright",
#        legend=c("0-25","25-75", ">75"),
#        col=c(2,3,4),
#        lwd=2)
# 
# # S
# plot(results$time,
#      results$S1/N[1],
#      type="l",
#      xlab="days",
#      ylab="% Individuals",
#      ylim=c(0,1),
#      lwd=2,
#      col=2,
#      main="Age-structured SIR (S)")
# lines(results$time,
#       results$S2/N[2],
#       col=3,
#       lwd=2)
# lines(results$time,
#       results$S3/N[3],
#       col=4,
#       lwd=2)
# legend("topright",
#        legend=c("0-25","25-75", ">75"),
#        col=c(2,3,4),
#        lwd=2)
# 
# # R
# plot(results$time,
#      results$R1/N[1],
#      type="l",
#      xlab="days",
#      ylab="% Individuals",
#      ylim=c(0,1),
#      lwd=2,
#      col=2,
#      main="Age-structured SIR (R)")
# lines(results$time,
#       results$R2/N[2],
#       col=3,
#       lwd=2)
# lines(results$time,
#       results$R3/N[3],
#       col=4,
#       lwd=2)
# legend("topright",
#        legend=c("0-25","25-75", ">75"),
#        col=c(2,3,4),
#        lwd=2)
# 
# # D
# plot(results$time,
#      results$D1/N[1],
#      type="l",
#      xlab="days",
#      ylab="% Individuals",
#      ylim=c(0,ymax),
#      lwd=2,
#      col=2,
#      main="Age-structured SIR (D)")
# lines(results$time,
#       results$D2/N[2],
#       col=3,
#       lwd=2)
# lines(results$time,
#       results$D3/N[3],
#       col=4,
#       lwd=2)
# legend("topright",
#        legend=c("0-25","25-75", ">75"),
#        col=c(2,3,4),
#        lwd=2)
#############

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

get_tot_population <- function(province){
  data_prov <- dataistat[dataistat$Territorio == province,]
  return(data_prov$Value[data_prov$Eta == "Total"])
}

results <- sirdModel(province="Torino", days=days)
contagiati <- get_contagiati_cumul(results)

prov <- "Torino"
pcmprov <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
pcmprov <- pcmprov[pcmprov$denominazione_provincia==prov,]

N <- get_tot_population(prov)

mp <- plot_ly(results, 
              x = results$time, 
              y = (contagiati$contagiati_g1+contagiati$contagiati_g2+contagiati$contagiati_g3)/N,
              type = 'scatter',
              mode = 'lines+markers',
              line = list(color = 'rgb(205, 12, 24)', width = 4),
              name = "Prediction")
mp <- mp %>% add_trace(y = pcmprov$totale_casi/N, name = 'Real data', mode = 'markers')
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

lista_prov <- c("Torino", "Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Verbano-Cusio-Ossola", "Vercelli")
res_reg <- as.data.frame(matrix(0, ncol = 13, nrow = days))
colnames(res_reg) <- c("time","S1","S2","S3","I1","I2","I3","R1","R2","R3","D1","D2","D3")

N <- 0
for (p in lista_prov){
  res <- sirdModel(province=p)
  res_reg <- aggregate(. ~ time, rbind(res_reg, res), sum)
  N <- N + get_tot_population(p)
}

# Need to remove first empty row, R is misterious
res_reg <- res_reg[-c(1),]

contag_reg <- get_contagiati_cumul(res_reg)

mreg <- plot_ly(res_reg, 
              x = res_reg$time, 
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

infplot <- plot_ly(results, 
                   x = results$time, 
                   y = results$I1+results$I2+results$I3,
                   type = 'scatter',
                   mode = 'lines+markers',
                   line = list(color = 'rgb(205, 12, 24)', width = 4),
                   name = "Prediction")
infplot <- infplot %>% add_trace(y = pcmreg$totale_positivi, name = 'Real data', mode = 'markers')
infplot <- infplot %>%
  layout(
    title = paste("Cumulative positive (infected)",reg),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
infplot

# --------------------------------------------
### Plot alex

I1 <- results$I1
I2 <- results$I2
I3 <- results$I3

#data <- ("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province-latest.json") 
#data1 <- fromJSON(file=data)

p <-
  plot_ly(
    results,
    x = results$time,
    y =  I1+I2+I3,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'rgb(205, 12, 24)', width = 4),
    name = "Total"
  )
p <- p %>% add_trace(y =I1 , name = 'Medium Risk', mode = 'lines', line = list(color="orange"))
p <- p %>% add_trace(y =I2 , name = 'High Risk', mode = 'lines', line = list(color="green"))
p <- p %>% add_trace(y =I3 , name = 'Low Risk', mode = 'lines', line = list(color="purple"))


p <- p %>%
  layout(
    title = paste("Total Cases -",prov),
    xaxis = list(title = "Days"),
    yaxis = list (title = "Individuals")
  )
p


xq <- c('High Risk', 'Medium Risk', 'Low Risk')
yq <- c(sum(results$D1), sum(results$D2), sum(results$D3))
text <- c('>75', '25-75', '0-25')
data11 <- data.frame(xq, yq, text)

m <- plot_ly(data11, x = ~xq, y = ~yq, type = 'bar', text = text,
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5)))
m <- m %>% layout(title = paste("Predicted deaths -",prov),
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
                    text = 'Comparison High RISK SIRD',
                    xref = 'Comparison',
                    yref = 'Comparison',
                    showarrow = FALSE
                  ))
l
