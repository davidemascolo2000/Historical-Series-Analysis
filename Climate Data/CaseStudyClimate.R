## Time Series Mult Analysis 

## Carico le librerie
library(forecast)
library(quantmod)
library(imputeTS)
library(ggthemes)
library(VIM)
library(dygraphs)
library(plotly)
library(seasonal)
library(tseries)
library(dplyr)
library(lubridate)
library(plyr)
library(TSstudio)
library(gridExtra)
library(corrplot)
library(splines)



## Carico i dati
tr <- read.csv("DailyDelhiClimateTrain.csv", header = T)
ts <- read.csv("DailyDelhiClimateTest.csv", header = T)

## Unisco i dati
dat <- bind_rows(tr, ts)

## Info sui dati
str(dat); dim(dat)
## 1576 osservazioni per 4 variabili.
## La variabile di riferimento è la temperatura media(meantemp).
## Le altre variabili sono l'umidità, la velocità del vento e la
## pressione media.

## Formatto 
dat$date <- as.Date(dat$date)

## Periodo di osservazione
range(dat$date)

## Visualizzo
View(dat)

## Data Wrangling
## Valori mancanti
table(is.na(dat))

## Calcoliamo e trasformiamo alcune colonne
## Wday
dat$weekday <- wday(dat$date)
dat$weekday <- factor(case_when(dat$weekday == 1 ~ "Mon",
                                dat$weekday == 2 ~ "Tue",
                                dat$weekday == 3 ~ "Wed",
                                dat$weekday == 4 ~ "Thu",
                                dat$weekday == 5 ~ "Fry",
                                dat$weekday == 6 ~ "Sat",
                                dat$weekday == 7 ~ "Sun"))

## Month
dat$month <- factor(month(dat$date), levels = as.character(1:12),
                    labels = c("Jan", "Feb", "Mar", "Apr", "May",
                               "Jun", "Jul", "Aug", "Sep", "Oct",
                               "Nov", "Dec"),
                    ordered = T)

## YearMonth
dat$yearmonth <- factor(as.yearmon(dat$date))

## Week
dat$week <- as.numeric(format(dat$date, "%W"))

## Normalizzo le settimane, così che partono da 0 per ogni mese
dat <- ddply(dat, .(yearmonth), transform,
             monthweek = 1 + week-min(week))
dat$week <- NULL

## Year
dat$year <- factor(year(dat$date))

## Visualizzo il risultato finale
View(dat)


## EDA
## Calendar Heatmap
ggplot(dat, aes(monthweek, weekday, fill = meantemp)) +
  geom_tile(colour = "white") +
  facet_grid(year(dat$date) ~ month) +
  scale_fill_gradient(low = "yellow", high = "red") +
  xlab("Week of Month") +
  ylab("") +
  ggtitle("Calendar Heatmap: Daily Climate in Delhi (India)") +
  labs(fill = "MeanTemp")
## Una mappa di calore rappresenta i valori per una variabile di
## interesse principale su due variabili dell'asse come una griglia di
## quadrati colorati. Le variabili degli assi sono suddivise in
## intervalli come un grafico a barre o un istogramma ed il colore di
## ciascuna cella indica il valore della variabile principale nell'
## intervallo di celle corrispondente.
## Le mappe termiche del calendario sono utili quando i valori sono
## giornalieri o settimanali. Sono molto utili, in casi come questi,
## quando si vuole visualizzare il valore giornaliero per l'intera
## serie storica.
## tuttavia, sono meno utili se si vogliono vedere componenti come
## la stagionalità, la forma della serie, la stazionarietà e così via).
## La mappa termica in questione, mostra la distribuzione giornaliera
## della temperatura media nella città di Delhi, in India,
## raggruppata per mese e registrata in quattro
## anni e quattro mesi, ovvero dal gennaio 2013 all'aprile 2017.
## Ogni cella riporta un conteggio numerico, come in una tabella dati
## standard, ma il conteggio è accompagnato da un colore, con conteggi
## maggiori associati a colorazioni più scure.(La colorazione della 
## cella è personalizzabile).
## Dai colori più scuri si vede come le temperature medie sono
## più elevate nei mesi estivi, come ci si aspetta e vanno al di sotto
## dei venti gradi nei mesi di Dicembre, Gennaio e Febbraio.
## Tuttavia, essendo l'India un paese molto caldo, si nota come già
## dalla fine di Marzo le temperature, mediamente, iniziano a salire in
## maniera importante, fino ad arrivare nei mesi di Maggio, Giugno e 
## nelle prime settimane di Luglio in cui mediamente si superano i 
## 30 gradi. Inoltre, i mesi di Luglio ed Agosto sono anche quelli in
## cui si registrano le maggiori precipitazioni.

## Correlazione
(corr  <- cor(dat[ ,c(2:5)]))
## Graficamente
corrplot(corr, method = "color", col = col(200),  
         type = "lower", order = "hclust", 
         addCoef.col = "black",
         tl.col="black", tl.srt = 45,
         sig.level = 0.01, insig = "blank")

## Andamento della temperatura media, umidità e velocità del vento
ggplot(dat, aes(x = date)) +
  geom_line(aes(y = meantemp, col = "Meantemp")) +
  geom_line(aes(y = humidity, col = "Humidity")) +
  geom_line(aes(y = wind_speed, col = "Wind Speed")) +
  labs(title = "Evolution of Time Series",
       x = "Date", y = "Value")

## MeanPressure
ggplot(dat, aes(x = date)) +
  geom_line(aes(y = meanpressure, col = "Meanpressure")) +
  ggtitle("Evolution of MeanPressure")

## Confronto le distribuzioni
x11()
par(mfrow = c(2,2))
boxplot(dat$meantemp, main = "Meantemp", horizontal = T)
boxplot(dat$humidity, main = "Humidity", horizontal = T)
boxplot(dat$wind_speed, main = "Wind Speed", horizontal = T)
boxplot(dat$meanpressure, main = "Meanpressure", horizontal = T)

## Distribuzione
## MeanTemp
graph1 <- ggplot(dat, aes(sample = meantemp)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Distribution MeanTemp")
## Humidity
graph2 <- ggplot(dat, aes(sample = humidity)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Distribution Humidity")
## Wind Speed
graph3 <- ggplot(dat, aes(sample = wind_speed)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Distribution WindSpeed")
## MeanPressure
graph4 <- ggplot(dat, aes(sample = meanpressure)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Distribution MeanPressure")
## Vista d'insieme
grid.arrange(graph1, graph2, graph3, graph4)

## Temperatura media per ogni mese
detach("package:plyr", unload = T)
p1 <- dat %>%
  group_by(month) %>% 
  summarise(MeanTemp = mean(meantemp))
## Graficamente
ggplot(p1, aes(x = reorder(month, MeanTemp), y = MeanTemp)) +
         geom_bar(stat = "identity", width = .5, fill = "tomato3") +
  labs(title = "Order Bar Chart",
       subtitle = "Month Vs MeanTemp",
       x = "Month", caption = "Blue: Mean Month") +
  geom_hline(yintercept = mean(p1$MeanTemp), linetype = "dashed",
                              col = "blue") 

## ***************************************************** ##



## Trasformo i dati
X <- aggregate(dat[ ,c(2:5)], list(substr(dat$date, 1,7)), FUN = mean)
colnames(X)[1] <- "Date"

## Oggetto ts
X <- ts(X[ ,c(2:5)], start = c(2013-01), frequency = 12)
X 

## Ancora qualche analisi(Inserire dopo CalendarHeatMap nel report finale)
ts_surface(X[,1]) 

## Forza del trend e della stagionalità
## Estraggo le componenti
cmp <- decompose(X[,1])

## Forza del trend
Ft <- max(0, (1 - (var(cmp$random, na.rm = T) /
                     var(cmp$random + cmp$trend, na.rm = T))))
## Forza della stagionalità
Fs <- max(0, (1 - var(cmp$random, na.rm = T) / 
                var(cmp$random + cmp$seasonal, na.rm = T)))

rbind(Trend = Ft,
      Seasonality = Fs)

## GGsubseriesplot
ggsubseriesplot(X[,1]) +
  ggtitle("SubSeries Plot") +
  ylab("MeanTemp")

## Lags
ts_lags(X[,1])
## Non Interattivo
gglagplot(X[,1], set.lags = c(1:12),
          colour = F, do.lines = F) +
  geom_point() +
  ggtitle("Lag Plot")
## Si noti come il ritardo 12 della serie presenti un forte legame lineare
## positivo con la serie. Per questo motivo, si può approfondire sul
## ritardo della serie.
ts_lags(X[,1], lags = c(12,24,36,48))
## Non Interattivo
gglagplot(X[,1], set.lags = c(12, 24, 36, 48),
          colour = F, do.lines = F) +
  geom_point() + 
  ggtitle("Lag Plot")

## Correlogramma
ggAcf(X[,1], lag.max = 48) +
  ggtitle("ACF")
## Si noti come il ritardo 12 è più alto rispetto agli altri ritardi. Ciò
## è dovuto all'andamento stagionale dei dati e quindi i picchi tendono ad
## essere a distanza 12 tra di loro.
## Anche i minimi sono a distanza 12 tra di loro e data la componente
## stagionale, vengono osservati a partire dal ritardo 12.
## Inoltre, i valori delle autocorrelazioni tendono velocemente a zero,
## il che fa escludere la presenza di un trend, almeno marcato, all'interno
## della serie.
## Infine, i dati non sono stazionari e ciò lo si vede sia dal ritardo uno
## che è il più grande e positivo, ma anche per la presenza di stagionalità
## all'interno della serie.

## Decomposizione
## X11 ---> Scelta Migliore
x11 <- seas(X[,1], transform.function = "auto", x11 = "")
## Graficamente
autoplot(x11) +
  ggtitle("Decomposition of X11 time series")
## Analisi dei residui
checkresiduals(x11)

## X13
x13 <- seas(X[ ,1])
## Graficamente
autoplot(x13) +
  ggtitle("Decomposition of X13-Seats time series")
## Analisi dei residui
checkresiduals(x13)



## Model

## Modello Lineare(Season)
fit.lin.s <- tslm(X[,1] ~ season)
summary(fit.lin.s)
## Per un livello di alpha pari al 5%, rifiuto l'ipotesi che alcune stime
## associate alla stagionalità siano pari a zero. Per la stima associata
## al mese di Dicembre, non viene rifiutata l'ipotesi nulla.
## Graficamente
autoplot(X[,1]) +
  autolayer(fitted(fit.lin.s), series = "Linear") +
  ggtitle("Climate Delhi") +
  xlab("Year") + ylab("MeanTemp")
## Analisi dei residui
checkresiduals(fit.lin.s)
## Dal test di Breusch-Godfrey, il p-value è minore del livello di alpha
## al 5%, ciò porta a rifiutare l'ipotesi Nulla ed affermare che non vi è
## autocorrelazione tra i residui.

## Modello con stagionalità e trend
fit.lin.ts <- tslm(X[,1] ~ season + trend)
summary(fit.lin.ts)
## Graficamente
autoplot(X[,1]) +
  autolayer(fitted(fit.lin.ts), series = "Linear") +
  ggtitle("Climate Delhi") +
  xlab("Year") + ylab("MeanTemp")
## Analisi dei residui
checkresiduals(fit.lin.ts)
## P-value maggiore del livello di alpha al 5%, non rifiuto l'ipotesi H0.
## Vi è autocorrelazione tra i residui.

## Modello 2 predittori
fit2 <- tslm(X[,1] ~ humidity + wind_speed, data = X)
autoplot(X[,1]) +
  autolayer(fitted(fit2), series = "2 predictors") +
  ggtitle("Climate Delhi") +
  xlab("Year") + ylab("MeanTemp")
summary(fit2)
## Analisi dei residui
checkresiduals(fit2)

## Modello 2 predittori + season.
fit2s <- tslm(X[,1] ~ humidity + wind_speed + season, data = X)
summary(fit2s)
autoplot(X[,1]) +
  autolayer(fitted(fit2s), series = "2 predictors + season") +
  ggtitle("Climate Delhi") +
  xlab("Year") + ylab("MeanTemp")
## Analisi dei residui
checkresiduals(fit2s)

## Modello spline
## Fisso i nodi
knots <- quantile(X[,2], p = c(.25, .50, .75))
## Specificazione
fit.sp <- tslm(X[,1] ~ bs(humidity, knots = knots) +
                 season + trend, data = X)
summary(fit.sp)
## Graficamente
autoplot(X[,1]) +
  autolayer(fitted(fit.sp), series = "Cubic Spline") +
  ggtitle("Climate Delhi") +
  xlab("Year") + ylab("MeanTemp")
## Analisi dei Residui
checkresiduals(fit.sp)
## Test Normalità Residui
## Se il modello si è adattato bene, mi aspetto che la distribuzione degli
## errori sia normale.
## Shapiro-Wilks
shapiro.test(fit.sp$residuals)
## H0: X ~ Normale
## H1: X non è Normale.
## Il p-value associato alla statistica test è pari a 0.21, di conseguenza
## non si rifiuta l'ipotesi H0, quindi i residui hanno distribuzione
## normale.

## Per un'ulteriore verifica viene effettuato anche il test di Jarque-Bera
## che riprende le stesse ipotesi.
jarque.bera.test(fit.sp$residuals)
## Si conclude confermando la tesi del test precedente. La distribuzione
## dei residui è Normale.


## Confronto
CV(fit.lin.s); CV(fit.lin.ts); CV(fit2); CV(fit2s); CV(fit.sp)

## END


