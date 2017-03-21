library(tidyverse)
library(splitstackshape)
library(fitdistrplus)

#https://en.wikipedia.org/wiki/List_of_countries_by_sex_ratio
#https://www.cdc.gov/growthcharts/html_charts/wtageinf.htm
#https://www.cdc.gov/growthcharts/html_charts/wtageinf.htm

#0.52 probability of being a boy
#7.7827653935
#7lbs 13 oz

data <- read.csv("C:/Users/bharder/Desktop/data.csv")

data2 <- data.frame(x1 = data[rep(seq(nrow(data)), data$events), 1])

ggplot(data2, aes(x1)) +
  geom_histogram(binwidth = 1)

ggplot(data2, aes(x1)) + stat_ecdf()

descdist(data2$x1, discrete = TRUE)

fitwei <- fitdist(data2$x1, "weibull", discrete = TRUE)
fitnbinom <- fitdist(data2$x1, "nbinom", discrete = TRUE)
fitpoi <- fitdist(data2$x1, "pois", discrete = TRUE)

fitwei
plot(fitwei)

sims <- round(rweibull(100000, shape = 30.08901, scale = 280.56290),0)
pweibull(280, 30.08901, 280.56290)
pweibull(280, 30.08901, 280.56290, lower.tail = FALSE)
qweibull(0.99, 30.08901, 280.56290)
dweibull(280, 30.08901, 280.56290)

sims <- sims - 280

median(sims)
median(data2$x1)
