# -------------------------------------------------------------------------- ###
# Soru 1a ----https://github.com/metinshahpalangov/finals-nav-m.git
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2a ----library(dplyr)

# Calculate the average fare for women
average_fare_women <- titanic %>%
  filter(sex == "female") %>%
  summarize(mean(fare))

# Calculate the average fare for men
average_fare_men <- titanic %>%
  filter(sex == "male") %>%
  summarize(mean(fare))

# Compare the two averages
average_fare_women - average_fare_men

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2b ----library(ggplot2)
library(tidyverse)

titanic_clean <- titanic %>%
  drop_na()

ggplot(data = titanic_clean, aes(x = sex, y = age, fill = sex)) +
  geom_boxplot() +
  labs(x = "Cinsiyet", y = "Yaş") +
  ggtitle("Cinsiyete Göre Yaşların Kutu Grafiği") +
  theme_minimal()

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2c ----library(ggplot2)
library(tidyverse)

ggplot(data = titanic, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "Age", y = "Density") +
  ggtitle("Histogram of Age") +
  theme_minimal()

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3a ----10 ve 13 değerlerini içeren bir vektörü döndürecektir.
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3b ----dat3 <- merge(dat1, dat2, all = TRUE)

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3c ----ggplot(data = dat, aes(x = x, y = y)) +
geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "x", y = "y")

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3d ----myresult değişkenine 2 ve 4 öğelerine sahip bir vektör atanacaktır.
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3e ----pnorm(q, mean = 0, sd = 1, lower.tail = TRUE)


# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3f ----simulate_dice <- function(num_rolls) {
rolls <- matrix(NA, nrow = num_rolls, ncol = 2)  # Boş bir matris oluşturma

for (i in 1:num_rolls) {
  dice1 <- sample(1:6, 1, replace = TRUE)  # İlk zar atışı
  dice2 <- sample(1:6, 1, replace = TRUE)  # İkinci zar atışı
  
  rolls[i, 1] <- dice1  # İlk zarın sonucunu matrise kaydetme
  rolls[i, 2] <- dice2  # İkinci zarın sonucunu matrise kaydetme
  
  cat("Atış", i, ": Zar 1 =", dice1, ", Zar 2 =", dice2, "\n")  # Atış sonuçlarını ekrana yazdırma
}

return(rolls)  # Sonuç matrisini döndürme

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3g ----titanic_kurtulan <- subset(titanic, survived == 1)
titanic_kurtulamayan <- subset(titanic, survived == 0)

t_test <- t.test(titanic_kurtulan$age, titanic_kurtulamayan$age, var.equal = TRUE)

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 4a ----library(tidyr)

dat <- data.frame(
  country = c("_Ingiltere", "Almanya"),
  '2018' = c(8000, 10000),
  '2019' = c(8100, 11000),
  '2020' = c(8500, 10200)
)

dat2 <- dat %>%
  pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "gdp")

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 5a ----library(ggplot2)

ggplot(dat, aes(x = cut, y = price)) +
  geom_bar(stat = "identity") +
  labs(x = "Cut", y = "Price")

# -------------------------------------------------------------------------- ###