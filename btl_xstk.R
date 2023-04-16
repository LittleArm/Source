install.packages("hrbrthemes")
install.packages("gganimate")
install.packages("gapminder")
install.packages("babynames")
install.packages("ggthemes")
install.packages("cowplot")
install.packages("ggplot2")
install.packages("dplyr")
library(hrbrthemes)
library(gganimate)
library(gapminder)
library(babynames)
library(ggthemes)
library(cowplot)
library(ggplot2)
library(dplyr)
library(readr)

####################################
#3.4
original_data <- read.csv("C:/Users/hienl/Downloads/machine.data", header = FALSE)
names(original_data) <- c("vendor name", "model name", "MYCT", "MMIN", "MMAX", "CACH", "CHMIN", "CHMAX", "PRP", "ERP")

####################################
#3.5.1
new_data <- subset(original_data, select=c('MYCT','MMIN','MMAX','CACH','CHMIN','CHMAX', 'PRP'))
#3.5.2
new_data %>% sapply(function(col) sum(is.na(col)))

####################################
#3.6.1.1
options(repr.plot.width = 14, repr.plot.height = 6)
x <- ggplot(data = new_data, mapping = aes(x = PRP)) +
  geom_histogram(bins = 40, fill = "orange", color = "#FF0000") +
  xlab("Relative performance") +
  ylab("Frequency") +
  ggtitle("Published Relative Performance Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")

plot_grid(a, nrow = 1, ncol = 1)
#3.6.1.2
options(repr.plot.width = 14, repr.plot.height = 6)
a <- ggplot(data = new_data, mapping = aes(x = MYCT)) +
  geom_histogram(bins = 40, fill = "blue", color = "cyan") +
  xlab("Machine Cycle Time (ns)") +
  ylab("Frequency") +
  ggtitle("Machine Cycle Time Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")
b <- ggplot(data = new_data, mapping = aes(x = MMIN)) +
  geom_histogram(bins = 40, fill = "blue", color = "cyan") +
  xlab("Minimum Main Memory Size (KB)") +
  ylab("Frequency") +
  ggtitle("Minimum Main Memory Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")
c <- ggplot(data = new_data, mapping = aes(x = MMAX)) +
  geom_histogram(bins = 40, fill = "#00CED1", color = "#7FFF00") +
  xlab("Maximum Main Memory Size (KB)") +
  ylab("Frequency") +
  ggtitle("Maximum Main Memory Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")
d <- ggplot(data = new_data, mapping = aes(x = CACH)) +
  geom_histogram(bins = 40, fill = "#00CED1", color = "#7FFF00") +
  xlab("Cache Size (KB)") +
  ylab("Frequency") +
  ggtitle("Cache Memory Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")
e <- ggplot(data = new_data, mapping = aes(x = CHMIN)) +
  geom_histogram(bins = 40, fill = "#4B0082", color = "#FF00FF") +
  xlab("Channels (units)") +
  ylab("Frequency") +
  ggtitle("Minimum Channels Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")
f <- ggplot(data = new_data, mapping = aes(x = CHMAX)) +
  geom_histogram(bins = 40, fill = "#4B0082", color = "#FF00FF") +
  xlab("Channels (units)") +
  ylab("Frequency") +
  ggtitle("Maximum Channels Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.position = "none")

plot_grid(a, b, c, d, e, f, nrow = 3, ncol = 2)

####################################

new_data$MYCT <- log(new_data$MYCT)
new_data$MMIN <- log(new_data$MMIN)
new_data$MMAX <- log(new_data$MMAX)
new_data$CACH <- log(new_data$CACH + 1)
new_data$CHMIN <- log(new_data$CHMIN + 1)
new_data$CHMAX <- log(new_data$CHMAX + 1)
new_data$PRP <- log(new_data$PRP)

####################################
#3.6.2
means <- new_data %>% sapply(mean)
medians <- new_data %>% sapply(median)
sds <- new_data %>% sapply(sd)
mins <- new_data %>% sapply(min)
maxs <- new_data %>% sapply(max)
summary <- as.data.frame(cbind(means, medians, sds, mins, maxs))
names(summary) <- c("mean", "median", "sd", "min", "max")

##### Histograms 3.6.1

####################################
#3.6.3
##### new_data2 <- new_data %>% cov() %>% as.data.frame()
new_data %>% cov() %>% as.data.frame()
##### new_data3 <- new_data %>% cor() %>% as.data.frame()
new_data %>% cor() %>% as.data.frame()
new_data %>% pairs()
pairs(new_data)

####################################
#3.7.1
linearModule <-lm(PRP~MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX,data=new_data)
summary(linearModule)
#3.7.2
linearModule$coefficients

SSE <- (linearModule$residuals ^ 2) %>% sum()
SSR <- ((linearModule$fitted.values - mean(new_data$PRP)) ^ 2) %>% sum()
SST <- ((new_data$PRP - mean(new_data$PRP)) ^ 2) %>% sum()
ss <- as.data.frame(c(SSE, SSR, SST), row.names = c("SSE", "SSR", "SST"))
names(ss) <- c("Value")
ss

