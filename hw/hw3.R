##---A/B test Sample---

getwd()
library("tidyverse")
user.table <- read_csv("user_table.csv")
test.table <- read_csv("test_table.csv")
head(user.table)
head(test.table)
print(paste("user_table 中不重複使用者數量:",
            nrow(unique(select(user.table, user_id)))))
print(paste("test_table 中不重複使用者數量:",
            nrow(unique(select(test.table, user_id)))))
print(paste("重複出現在user_table與test_table中不重複使用者數量:",
            nrow(unique(
              inner_join(select(test.table, user_id),
                         select(user.table, user_id),
                         by = "user_id")))))
print(paste("實驗組次數:",
            sum(
              unique(test.table)$test == 1)
))
print(paste("對照組次數:",
            sum(
              unique(test.table)$test == 0)
))
test.data <- left_join(test.table,
                       user.table,
                       by = "user_id")

head(test.data)
test.data$date <- as.Date(test.data$date, format = "%Y/%m/%d")
for(i in c(3,4,6,7,9)){
  test.data[, i] <- as.factor(test.data[[i]])
}
head(test.data)
summary(test.data)

test.data %>%
  group_by(test) %>%
  summarize(mean_purchase_amount = mean(purchase_amount))

test.data %>%
  group_by(device) %>%
  summarize(mean_purchase_amount = mean(purchase_amount))

test.data %>%
  group_by(gender) %>%
  summarize(mean_purchase_amount = mean(purchase_amount))

t.test(test.data[test.data$test == 1, ]$purchase_amount,
       alternative = "greater")

aov.model <- aov(
  purchase_amount ~ test + country + device + gender + service,
  test.data)
summary(aov.model)

interaction.model <- aov(
  purchase_amount ~ test*country + test*device + test*service,
  test.data)
summary(interaction.model)

interaction.model <- aov(
  purchase_amount ~ test*country + device + service,
  test.data)
summary(interaction.model)

TukeyHSD(interaction.model, "test")

TukeyHSD(interaction.model, "country")

plot(TukeyHSD(interaction.model, "country"))

daily.purchase <- test.data %>%
  group_by(date, test) %>%
  summarise(purchase_amount = mean(purchase_amount))

daily.purchase
ggplot(daily.purchase, aes(x = date, y = purchase_amount, colour = test)) + 
  geom_point() + geom_line() +
  xlab("Date") + ylab("Purchase Amount") + ylim(c(30, 50)) +
  ggtitle("Time Series Plot of Purchase Amount: Test versus Control") +
  theme_bw()

ggplot(test.data, aes(purchase_amount, fill = test, colour = test)) +
  geom_density(alpha = 0.3) +
  xlab("Purchase Amount") + ylab("Density") +
  ggtitle("Density Plot of Purchase Amount: Test versus Control") +
  theme_bw()

ggplot(test.data, aes(x = country, y = purchase_amount)) +
  geom_boxplot() +
  xlab("Country") + ylab("Purchase Amount") +
  ggtitle("Boxplot of Purchase Amount by Country") +
  theme_bw()

ggplot(test.data, aes(x = country, y = purchase_amount, colour = test)) +
  geom_boxplot() +
  xlab("Country") + ylab("Purchase Amount") +
  ggtitle("Boxplot of Purchase Amount by Country: Test versus Control") +
  theme_bw()

#Q1
test.jp <- filter(test.data, country == 'JP')
test.jp %>%
  group_by(test) %>%
  summarise(mean_purchase_amount = mean(purchase_amount))

t.test(test.jp[test.jp$test == 1, ]$purchase_amount,
       test.jp[test.jp$test == 0, ]$purchase_amount,
       alternative = "greater")

t.test(test.jp$purchase_amount[test.jp$test == 1], 
       test.jp$purchase_amount[test.jp$test == 0],
       alternative = "less")

ggplot(test.jp, aes(purchase_amount, fill = test, colour = test)) +
  geom_density(alpha = 0.3) +
  xlab("Purchase Amount") + ylab("Density") +
  ggtitle("Density Plot of Purchase Amount: Test versus Control") +
  theme_bw()

#Q2
aov.model <- aov(
  purchase_amount ~ test + device + gender + service,
  test.data)
summary(aov.model)

ggplot(test.data, aes(x = service, y = purchase_amount)) +
  geom_boxplot() +
  xlab("Service") + ylab("Purchase Amount") +
  ggtitle("Boxplot of Purchase Amount by Service") +
  theme_bw()

TukeyHSD(interaction.model, "test")
plot(TukeyHSD(interaction.model, "test"))

TukeyHSD(interaction.model, "device")
plot(TukeyHSD(interaction.model, "device"))
