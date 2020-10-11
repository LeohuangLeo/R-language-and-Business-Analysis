#Q1:
#1, 2

#Q2:
library(tidyverse)
df = read_csv('TWSE_Stock Data_2012-2017.csv')
ncol(df)
name_of_df <- colnames(df)
df.colnames[3:ncol(df)]
df.new <- df %>%
  gather(
    key = 'date',
    value = 'price',
    name_of_df[3:ncol(df)]
  )
head(df.new)

#Q3:
df.new.new <- df.new %>%
  spread(
    key = 'type',
    value = 'price'
  )
head(df.new.new)

#Q4:
df.new.new.new <- df.new.new %>%
  separate(
    col = 'date',
    into = c('year', 'month', 'day'),
    sep = '/',
    convert = TRUE
  )
head(df.new.new.new)
