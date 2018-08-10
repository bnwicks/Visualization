### Section 1: Introduction to Data Visualization and Distributions

library(dslabs)
library(tidyverse)

## 
data(murders)
head(murders)

## 1.2 Introduction to Distributions  
data("heights")
head(heights)

prop.table(table(heights$sex))

index <- heights$sex == "Male"
x <- heights$height[index]
head(x)

mean(x)
sd(x)

## 1.3 Quantiles, Percentiles, and Boxplots  
# Quantile-Quantile Plots
mean(x <= 69.5)

p <- seq(0.05, 0.95, 0.05)

observed_quantiles <- quantile(x, p)

theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))

plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p) 
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

## 2. Introduction to ggplot2
library(ggthemes)
library(ggrepel)

r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>% .$rate


plot_murders <- 
  murders %>% ggplot(aes(population/10^6, total, label=abb)) +
  geom_text_repel() +
  scale_x_continuous(trans = "log10") + # scale_x_log10
  scale_y_continuous(trans = "log10") + # scale_y_log10
  xlab("Pop in millions (log)") +
  ylab("Total Murders (log)") +
  ggtitle("US Gun Murders 2010") +
  geom_point(size = 3, aes(col = region)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  scale_color_discrete(name = "Region") +
  theme_economist()
  

plot_murders

# Other Examples
p <- heights %>% 
  filter(sex == "Male") %>%
  ggplot(aes(x = height)) 

p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male height in inches") +
  ggtitle("Histogram")
 
p + geom_density(fill = "blue")

p <- heights %>% 
  filter(sex == "Male") %>%
  ggplot(aes(sample = height)) 

p1 <- p + geom_qq()

params <- heights %>%
  filter(sex=="Male") %>%
  summarise(mean = mean(height), sd = sd(height))

p2 <- p + geom_qq(dparams = params) +
  geom_abline()

# grids of plots
library(gridExtra)
grid.arrange(p, p1,p2)
