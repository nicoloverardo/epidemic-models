source("agesird.R")
library(plotly)

# Get data and setup
prov <- "Torino"
reg <- "Piemonte"
lista_prov <- c("Torino", "Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Verbano-Cusio-Ossola", "Vercelli")

pcmprov <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
pcmprov <- pcmprov[pcmprov$denominazione_provincia==prov,]
pcmreg <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
pcmreg <- pcmreg[pcmreg$denominazione_regione==reg,]
dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
data_prov <- dataistat[dataistat$Territorio == prov,]
pop <- data_prov$Value[data_prov$Eta == "Total"]
class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]
N <- pop*class_percent

# Download data from our official github repository
dataProvinces <- read.csv("https://raw.githubusercontent.com/CEEDS-DEMM/COVID-Pro-Dataset/master/deathsItaProv.csv")
dataProvinces <- dataProvinces[dataProvinces$Province == prov,]
# Number of days to predict
days <- nrow(dataProvinces)
pcmprov <- pcmprov[1:days,]
pcmreg <- pcmreg[1:days,]

# Get predictions
results <- sirdModel(province=prov, days=days)
resultsTD <- sirdModelTD(province=prov, days=days)
resultsTDw <- sirdModelTDweigthed(province=prov, days=days, dataProvinces=dataProvinces)

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
    title = paste("Age-structured SIRD",prov,"- Infected"),
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
    title = paste("Age-structured SIRD",prov,"- Deaths"),
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
    title = paste("Age-structured SIRD",prov,"- Susceptible"),
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
    title = paste("Age-structured SIRD",prov,"- Recovered"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
R

### SIRD TD plots
par(mfrow<-c(1,1))
ymax = max(c(resultsTD$I1/N[1],resultsTD$I2/N[2], resultsTD$I3/N[3]))

# I
I <- plot_ly(resultsTD, 
             x = resultsTD$time, 
             y = resultsTD$I1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
I <- I %>% add_trace(y = resultsTD$I2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
I <- I %>% add_trace(y = resultsTD$I3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
I <- I %>%
  layout(
    title = paste("Age-structured SIRD",prov,"- Infected"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
I

# D
D <- plot_ly(resultsTD, 
             x = resultsTD$time, 
             y = resultsTD$D1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
D <- D %>% add_trace(y = resultsTD$D2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
D <- D %>% add_trace(y = resultsTD$D3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
D <- D %>%
  layout(
    title = paste("Age-structured SIRD",prov,"- Deaths"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
D

# S
S <- plot_ly(resultsTD, 
             x = resultsTD$time, 
             y = resultsTD$S1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
S <- S %>% add_trace(y = resultsTD$S2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
S <- S %>% add_trace(y = resultsTD$S3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
S <- S %>%
  layout(
    title = paste("Age-structured SIRD",prov,"- Susceptible"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
S

# R
R <- plot_ly(resultsTD, 
             x = resultsTD$time, 
             y = resultsTD$R1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
R <- R %>% add_trace(y = resultsTD$R2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
R <- R %>% add_trace(y = resultsTD$R3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
R <- R %>%
  layout(
    title = paste("Age-structured SIRD",prov,"- Recovered"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
R

### SIRD TDw plots
par(mfrow<-c(1,1))
ymax = max(c(resultsTDw$I1/N[1],resultsTDw$I2/N[2], resultsTDw$I3/N[3]))

# I
I <- plot_ly(resultsTDw, 
             x = resultsTDw$time, 
             y = resultsTDw$I1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
I <- I %>% add_trace(y = resultsTDw$I2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
I <- I %>% add_trace(y = resultsTDw$I3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
I <- I %>%
  layout(
    title = paste("Age-structured SIRD",prov,"- Infected"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
I

# D
D <- plot_ly(resultsTDw, 
             x = resultsTDw$time, 
             y = resultsTDw$D1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
D <- D %>% add_trace(y = resultsTDw$D2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
D <- D %>% add_trace(y = resultsTDw$D3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
D <- D %>%
  layout(
    title = paste("Age-structured SIRD",prov,"- Deaths"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
D

# S
S <- plot_ly(resultsTDw, 
             x = resultsTDw$time, 
             y = resultsTDw$S1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
S <- S %>% add_trace(y = resultsTDw$S2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
S <- S %>% add_trace(y = resultsTDw$S3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
S <- S %>%
  layout(
    title = paste("Age-structured SIRD",prov,"- Susceptible"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
S

# R
R <- plot_ly(resultsTDw, 
             x = resultsTDw$time, 
             y = resultsTDw$R1/N[1],
             type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4),
             name = "0-25")
R <- R %>% add_trace(y = resultsTDw$R2/N[2], name = '25-75', mode = 'lines', line = list(color = "green"))
R <- R %>% add_trace(y = resultsTDw$R3/N[3], name = '>75', mode = 'lines', line = list(color = "blue"))
R <- R %>%
  layout(
    title = paste("Age-structured SIRD",prov,"- Recovered"),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
R

# ---------------------------------

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

contagiati <- get_contagiati_cumul(results)
contagiati_td <- get_contagiati_cumul(resultsTD)
contagiati_td_w <- get_contagiati_cumul(resultsTDw)

N <- get_tot_population(prov)

# Positive cases per province
mp <- plot_ly(results, 
              x = results$time, 
              y = (contagiati$contagiati_g1+contagiati$contagiati_g2+contagiati$contagiati_g3)/N,
              type = 'scatter',
              mode = 'lines',
              name = "Prediction")
mp <- mp %>% add_trace(y = (contagiati_td$contagiati_g1+contagiati_td$contagiati_g2+contagiati_td$contagiati_g3)/N, name = 'Prediction with time dep. beta', mode = 'lines')
mp <- mp %>% add_trace(y = (contagiati_td_w$contagiati_g1+contagiati_td_w$contagiati_g2+contagiati_td_w$contagiati_g3)/N, name = 'Prediction with time dep. beta and reduction', mode = 'lines')
mp <- mp %>% add_trace(y = pcmprov$totale_casi/N, name = 'Real data', mode = 'markers')
mp <- mp %>%
  layout(
    title = paste("Cumulative cases", prov),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
mp

# Cumulative for region
res_reg <- as.data.frame(matrix(0, ncol = 13, nrow = days))
res_reg_td <- as.data.frame(matrix(0, ncol = 13, nrow = days))
res_reg_td_w <- as.data.frame(matrix(0, ncol = 13, nrow = days))
colnames(res_reg) <- c("time","S1","S2","S3","I1","I2","I3","R1","R2","R3","D1","D2","D3")
colnames(res_reg_td) <- c("time","S1","S2","S3","I1","I2","I3","R1","R2","R3","D1","D2","D3")
colnames(res_reg_td_w) <- c("time","S1","S2","S3","I1","I2","I3","R1","R2","R3","D1","D2","D3")

Nt <- 0
for (p in lista_prov){
  res <- sirdModel(province=p, days=days)
  res_reg <- aggregate(. ~ time, rbind(res_reg, res), sum)
  
  res_td <- sirdModelTD(province=p, days=days)
  res_reg_td <- aggregate(. ~ time, rbind(res_reg_td, res_td), sum)
  
  res_td_w <- sirdModelTDweigthed(province=p, days=days, dataProvinces = dataProvinces)
  res_reg_td_w <- aggregate(. ~ time, rbind(res_reg_td_w, res_td_w), sum)
  
  Nt <- Nt + get_tot_population(p)
}

# Need to remove first empty row, R is mysterious
res_reg <- res_reg[-c(1),]
res_reg_td <- res_reg_td[-c(1),]
res_reg_td_w <- res_reg_td_w[-c(1),]
contag_reg <- get_contagiati_cumul(res_reg)
contag_reg_td <- get_contagiati_cumul(res_reg_td)
contag_reg_td_w <- get_contagiati_cumul(res_reg_td_w)

ip <- plot_ly(results, 
              x = results$time, 
              y = (results$I1+results$I2+results$I3)/N,
              type = 'scatter',
              mode = 'lines',
              name = "Prediction")
ip <- ip %>% add_trace(y = (resultsTD$I1+resultsTD$I2+resultsTD$I3)/N, name = 'Prediction with time dep. beta', mode = 'lines')
ip <- ip %>% add_trace(y = (resultsTDw$I1+resultsTDw$I2+resultsTDw$I3)/N, name = 'Prediction with time dep. beta and reduction', mode = 'lines')
ip <- ip %>% add_trace(y = (dataProvinces$New_cases)/N, name = 'Real data', mode = 'markers')
ip <- ip %>%
  layout(
    title = paste("Infected",prov),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
ip

mreg <- plot_ly(res_reg_td, 
                x = res_reg_td$time, 
                y = (contag_reg_td$contagiati_g1+contag_reg_td$contagiati_g2+contag_reg_td$contagiati_g3)/Nt,
                type = 'scatter',
                mode = 'lines',
                name = "Prediction with time dep. beta")
mreg <- mreg %>% add_trace(y = (contag_reg$contagiati_g1+contag_reg$contagiati_g2+contag_reg$contagiati_g3)/Nt,
                           name = 'Prediction', mode = 'lines')
mreg <- mreg %>% add_trace(y = (contag_reg_td_w$contagiati_g1+contag_reg_td_w$contagiati_g2+contag_reg_td_w$contagiati_g3)/Nt,
                           name = 'Prediction with time dep. beta and reduction', mode = 'lines')
mreg <- mreg %>% add_trace(y = pcmreg$totale_casi/(Nt), name = 'Real data', mode = 'markers')
mreg <- mreg %>%
  layout(
    title = paste("Cumulative cases",reg),
    xaxis = list(title = "Days"),
    yaxis = list (title = "% Individuals")
  )
mreg

infplot <- plot_ly(res_reg, 
                   x = res_reg$time, 
                   y = res_reg$I1+res_reg$I2+res_reg$I3,
                   type = 'scatter',
                   mode = 'lines',
                   name = "Prediction with time dep. beta")
infplot <- infplot %>% add_trace(y = res_reg_td$I1+res_reg_td$I2+res_reg_td$I3, name = 'Prediction', mode = 'lines')
infplot <- infplot %>% add_trace(y = res_reg_td_w$I1+res_reg_td_w$I2+res_reg_td_w$I3, name = 'Prediction with time dep. beta and reduction', mode = 'lines')
infplot <- infplot %>% add_trace(y = pcmreg$totale_positivi, name = 'Real data', mode = 'markers')
infplot <- infplot %>%
  layout(
    title = paste("Infected",reg),
    xaxis = list(title = "Days"),
    yaxis = list (title = "Individuals")
  )
infplot

dtplot <- plot_ly(res_reg, 
                  x = res_reg$time, 
                  y = res_reg$D1+res_reg$D2+res_reg$D3,
                  type = 'scatter',
                  mode = 'lines',
                  name = "Prediction with time dep. beta")
dtplot <- dtplot %>% add_trace(y = res_reg_td$D1+res_reg_td$D2+res_reg_td$D3, name = 'Prediction', mode = 'lines')
dtplot <- dtplot %>% add_trace(y = res_reg_td_w$D1+res_reg_td_w$D2+res_reg_td_w$D3, name = 'Prediction with time dep. beta and reduction', mode = 'lines')
dtplot <- dtplot %>% add_trace(y = pcmreg$deceduti, name = 'Real data', mode = 'markers')
dtplot <- dtplot %>%
  layout(
    title = paste("Cumulative deaths",reg),
    xaxis = list(title = "Days"),
    yaxis = list (title = "Individuals")
  )
dtplot


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
             marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = FALSE))
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
