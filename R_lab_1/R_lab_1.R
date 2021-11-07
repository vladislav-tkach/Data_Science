#1. Arithmetic mean, median

# sum(x_i) / n
mean(x)
# x_(n/2)
median(x)

#2. Variance, standard deviation,  coefficient of variation , range of variation, interquartile range

# sum((x_i - mean(x))^2)/(n - 1)
var(x)
# sqrt(sum((x_i - mean(x))^2)/n)
sd(x)
# 
sd(x) / mean(x)
#
max(x) - min(x)
# x_(3n/4) - x_(n/4)
IQR(x)

#3. Box plot

ggplot(data = qoli, mapping = aes(x = QualityOfLifeIndex, y = "")) +
  labs(title = "Violin and Box Plot", x = "Quality of Life Index", y = "Frequency") +
  geom_violin(width = 0.6, fill = "chartreuse3", color = NA) +
  geom_boxplot(width = 0.3, fill = "moccasin", size = 1,
               outlier.color = "red", outlier.size = 3, notch = TRUE,
               notchwidth = 0.85) +
  geom_jitter(height = 0.15, color = "red4", size = 1) +
  stat_boxplot(geom = 'errorbar', width = 0.1, size = 1) +
  theme(plot.title = element_text(hjust = 0.5))

#4. Extreme points , quartiles

quantile(x)

#5. 1st and 9th decile

quantile(x, probs = seq(.1, .9, by = .1))[1]
quantile(x, probs = seq(.1, .9, by = .1))[9]

#6. Skewness , kurtosis

#install.packages("moments")
library(moments)

# (1/n * sum((x_i - mean(x))^3))/((1/(n-1))(sum(x_i^2 - n * mean(x)^2)))^3/2
skewness(x, na.rm = TRUE)
# (1/n * sum((x_i - mean(x))^4))/((1/(n-1))(sum(x_i^2 - n * mean(x)^2)))^2
kurtosis(x, na.rm = TRUE)

#7. Build histogram with Sturges' , Scott , FD rule. Build hypothetical density function

# Sturges' rule
# k = 1 + log2(n)
Sturges_breaks <- pretty(range(x), n = nclass.Sturges(x), min.n = 1)

ggplot(data = qoli, mapping = aes(x = QualityOfLifeIndex)) +
  labs(title = "Sturges' Rule Histogram", x = "Quality of Life Index",
       y = "Frequency") +
  geom_histogram(breaks = Sturges_breaks, fill = "lightblue", color = "black",
                 size = 1) +
  theme(plot.title = element_text(hjust = 0.5))

# Scott's rule
# h = 3.5 * sd(x) * n^(-1/3)
Scotts_breaks <- pretty(range(x), n = nclass.scott(x), min.n = 1)

ggplot(data = qoli, mapping = aes(x = QualityOfLifeIndex)) +
  labs(title = "Scott's Rule Histogram", x = "Quality of Life Index",
       y = "Frequency") +
  geom_histogram(breaks = Scotts_breaks, fill = "lightgreen", color = "black",
                 size = 1) +
  theme(plot.title = element_text(hjust = 0.5))

# Freedman-Diaconis rule
# h = 2 * IQR(x) * n^(-1/3)
FD_breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)

ggplot(data = qoli, mapping = aes(x = QualityOfLifeIndex)) +
  labs(title = "Freedman-Diaconis Rule Histogram", x = "Quality of Life Index",
       y = "Frequency") +
  geom_histogram(breaks = FD_breaks, fill = "gold", color = "black",
                 size = 1) +
  geom_function(fun = function(x) 
  {dnorm(x, mean = mean(x), sd = sd(x)) * 3500}, colour = "red", size = 1.2) +
  theme(plot.title = element_text(hjust = 0.5))

#8. Q-Q diagram

ggplot(data = qoli, mapping = aes(sample = QualityOfLifeIndex)) +
  labs(title = "Q-Q Diagram", x = "Theoretical quantiles",
       y = "Selective quantiles") +
  stat_qq_band() +
  stat_qq_point() +
  stat_qq_line(color = "blue", alpha = 0.5, size = 1) +
  theme(plot.title = element_text(hjust = 0.5))

#9. P-P diagram

#install.packages("qqplotr")
library(qqplotr)

ggplot(data = qoli, mapping = aes(sample = QualityOfLifeIndex)) +
  labs(title = "P-P Diagram", x = "Theoretical quantiles",
       y = "Empirical distribution function") +
  stat_pp_band() +
  stat_pp_point() +
  stat_pp_line(color = "blue", alpha = 0.5, size = 1) +
  theme(plot.title = element_text(hjust = 0.5))

#10. Test

shapiro.test(x)

#install.packages("nortest")
library(nortest)

nortest::ad.test(x)
nortest::cvm.test(x)
nortest::lillie.test(x)
nortest::pearson.test(x)
nortest::sf.test(x)