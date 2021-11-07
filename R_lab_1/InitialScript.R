# Here countries' populations are the objects of observation, while countries'
# names and QOLI are research variables. Countries' names is nominal categorical
# data, QOLI is continuous data.

rm(list = ls())
graphics.off()

setwd("C:\\Projects\\Practice\\R_lab_1")

qoli <- read.csv(file = 'countries_quality_of_life_index.18-10-2021.csv')

x = qoli$"QualityOfLifeIndex"

#install.packages("ggplot2")
library(ggplot2)


ggplot(data = qoli,
       mapping = aes(x = reorder(Name, -QualityOfLifeIndex),
                     y = QualityOfLifeIndex)) +
  labs(title = "Quality of Life Index per Country", x = "Countries", y = "QOLI") +
  geom_bar(stat = "identity", width=0.5, position = position_dodge(width=0.5),
           aes(fill = QualityOfLifeIndex)) +
  scale_fill_gradientn(colors = c("red", "orange", "gold", "green", "green4"),
                       limits = c(75, 200), oob = scales::squish) +
  theme(axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))