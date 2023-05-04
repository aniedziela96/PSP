install.packages("geomtextpath")
library(ggplot2)

names <- c('id', 'age', 'education', 'sex', 'income', 'sector')

income_data <- read.table("income_data.txt", col.names = names)
salary_good <- which(income_data$income > 0)
income_data_valid <- income_data[salary_good,]
outliers <- boxplot.stats(income_data_valid$income)$out
out_index <- which(income_data_valid$income %in% c(outliers))
income_data_without_out <- income_data_valid[-out_index,]

income_data_valid["sex"][income_data_valid["sex"] == 1] <- 'M'
income_data_valid["sex"][income_data_valid["sex"] == 2] <- 'F'



v <- seq(0, 250000, by=16000)

w <- c(v, max(income_data_valid$income))

ggplot(income_data_valid, aes(x=income)) + 
  geom_histogram(binwidth = 1000,
                 breaks=v,
                 color = 'black',
                 fill = 'white') +
  geom_vline(data = income_data_valid, aes(xintercept = mean(income)), col = "blue", lwd = 2)

ggplot(income_data_without_out, aes(x=income)) + 
  geom_histogram(binwidth = 8000,
                 color = 'black',
                 fill = 'white') +
  geom_vline(data = income_data_valid, aes(xintercept = mean(income)), col = "blue", lwd = 2)

ggplot(income_data_without_out, aes(y=income)) + 
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom = "errorbar")

ggplot(data=income_data_valid, aes(x=age, y=income)) +
  geom_bar(stat = "summary", fun = "mean")

ggplot(data=income_data_valid, aes(x=age, y=income)) +
  geom_bar(stat = "summary", fun = "median")

ggplot(data=income_data_valid, aes(x=sex, y=income)) +
  geom_bar(stat = "summary", fun = "mean")

ggplot(data=income_data_valid, aes(x=sex, y=income)) +
  geom_bar(stat = "summary", fun = "median")

ggplot(data=income_data_valid, aes(x=sector, y=income)) +
  geom_bar(stat = "summary", fun = "mean")

ggplot(data=income_data_valid, aes(x=sector, y=income)) +
  geom_bar(stat = "summary", fun = "median")

ggplot(data=income_data_valid, aes(x=education, y=income)) +
  geom_bar(stat = "summary", fun = "mean")

ggplot(data=income_data_valid, aes(x=education, y=income)) +
  geom_bar(stat = "summary", fun = "median") +
  scale_x_discrete(limits = c(1, 2, 3, 4, 5, 6))

ggplot(income_data_without_out, aes(x = income, fill = factor(sector))) + 
  scale_fill_manual(name = "sector", 
                    values = c("orange", "white", "pink"), 
                    labels = c("private", "public", "self-employed")) +
  geom_histogram(aes(fill = factor(sector)),
                 binwidth = 8000,
                 col = "black") +
  theme_bw()

ggplot(income_data_without_out, aes(x = income, fill = factor(sex))) + 
  scale_fill_manual(name = "sex", 
                    values = c("white", "pink"), 
                    labels = c("male", "female")) +
  geom_histogram(aes(fill = factor(sex)),
                 binwidth = 8000,
                 col = "black") +
  theme_bw()

ggplot(income_data_without_out, aes(x=income)) + 
  geom_histogram(binwidth = 8000,
                 color = 'black',
                 fill = 'white')


median(income_data_valid[which(income_data_valid$sector == 7),]$income)

ggplot(data=income_data_valid, aes(x=sector, y=income)) +
  geom_bar(stat = "summary", fun = "median") +
  labs(title = "Średnie zarobki w zależności od sektora zatrudnienia", x = 'sektor', y = 'średnie zarobki') + 
  theme_bw() +
  scale_x_discrete(labels=c("!","2","S"))

median(income_data_valid[which(income_data_valid$sex == 'F'),]$income)

length(which(income_data_valid$sex == 'F'))

sprintf(percentage_under_median_f, fmt = '%#.2f')
