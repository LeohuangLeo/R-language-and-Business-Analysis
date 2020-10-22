library(tidyverse)
getwd()
setwd("/Users/leo/Desktop/R")

#Q1
financial.data <- read_csv("2017_financial index_163 comp.csv")
head(financial.data)
summary(financial.data[, 2:ncol(financial.data)])
new.financial.data <- financial.data %>%
  mutate(
    sales_margin_rate = roa/asset_turnover,
    profit_indicator = roa*(1+asset_growth_rate),
    t_roa = exp(roa/10) / (1+exp(roa/10))
  )

ggplot(new.financial.data) + geom_histogram(aes(x=roa))

ggplot(new.financial.data) + geom_histogram(aes(x=t_roa))

#Q2
set.seed(500)
library(nsprcomp)
nspca.model <- nscumcomp(
  new.financial.data[, 2:19], 
  k = 100, nneg = T,
  scale. = T)

var.exp <- tibble(
  pc = paste0("PC_", formatC(1:18, width=2, flag="0")),
  var = nspca.model$sdev^2,
  prop = (nspca.model$sdev)^2 / sum((nspca.model$sdev)^2),
  cum_prop = cumsum((nspca.model$sdev)^2 / sum((nspca.model$sdev)^2)))

plot_ly(
  x = var.exp$pc,
  y = var.exp$cum_prop,
  type = "bar"
) %>%
  layout(
    title = "Cumulative Proportion by Each Principal Component",
    xaxis = list(type = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Proportion'),
    margin = list(r = 30, t = 50, b = 70, l = 50)
  )

plot_ly(
  x = var.exp$pc,
  y = var.exp$cum_prop,
  type = "bar"
) %>%
  layout(
    title = "Cumulative Proportion by Each Principal Component",
    xaxis = list(type = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Proportion'),
    margin = list(r = 30, t = 50, b = 70, l = 50)
  )

library(reshape2)
ggplot(melt(nspca.model$rotation[, 1:7]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

nspca.score <- data.frame(nspca.model$x)
row.names(nspca.score) <- financial.data$comp_id
plot_ly(
  x = nspca.score[, 1],
  y = nspca.score[, 2],
  text = financial.data$comp_id,
  type = "scatter",
  mode = "markers"
) %>% layout(
  title = "PC 1 v.s. PC 2 Score: Scatter Plot",
  xaxis = list(title = 'Principal Component 1'),
  yaxis = list(title = 'Principal Component 2'),
  margin = list(r = 30, t = 50, b = 70, l = 50)
)
