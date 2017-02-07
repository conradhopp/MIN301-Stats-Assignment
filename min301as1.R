#MIN301 Assignment 2 Q2
dataset <- read.csv("gold.csv", header = T) #import dataset


#Renaming Rocktype for simpler visualization
dataset$ROCKTYPE <- factor(dataset$ROCKTYPE,
                           levels = c(2170,2270,2370),
                           labels = c("Rock Type A", "Rock Type B", "Rock Type C"))

#Create subsets for each rocktype
rockA <- subset(dataset, dataset$ROCKTYPE == "Rock Type A")
rockB <- subset(dataset, dataset$ROCKTYPE == "Rock Type B")
rockC <- subset(dataset, dataset$ROCKTYPE == "Rock Type C")

#Create Histograms for each rocktype ----------------------------------------------------------------------------------------------
library(ggplot2)
#ROCKA
histA <- ggplot(rockA, aes(x = AU))
histA + geom_histogram(aes(y = ..count..,fill = ..count..),
                       binwidth = 0.02,
                       colour = "black") + xlim(c(0,2)) + xlab("AU Grade (ppm)") +
  ggtitle("Frequency Histogram of Gold in\nRock Type A") + 
  annotate("text", x = 1.75, y = 500, 
           label = "Min. 0.000\n1st Qu. 0.022\nMedian. 0.048\nMean. 0.09144\n3rd Qu. 0.095\nMax. 6.412")

#Rock B
histB <- ggplot(rockB, aes(x = AU))
histB+ geom_histogram(aes(y = ..count..,fill = ..count..),
                       binwidth = 0.02,
                       colour = "black") + xlim(c(0,2)) + xlab("AU Grade (ppm)") +
  ggtitle("Frequency Histogram of Gold in\nRock Type B")+
  annotate("text", x = 1.75, y = 125, 
           label = "Min. 0.000\n1st Qu. 0.094\nMedian. 0.180\nMean. 0.2461\n3rd Qu. 0.308\nMax. 15.7")
#Rock C
histC <- ggplot(rockC, aes(x = AU))
histC+ geom_histogram(aes(y = ..count..,fill = ..count..),
                      binwidth = 0.1,
                      colour = "black") + xlim(c(0,10)) + xlab("AU Grade (ppm)") +
  ggtitle("Frequency Histogram of Gold in\nRock Type C")+
  annotate("text", x = 8.75, y = 75, 
           label = "Min. 0.040\n1st Qu. 0.4815\nMedian. 0.696\nMean. 1.296\n3rd Qu. 1.142\nMax. 50.5")

#Probability Curves ----------------------------------------------------------------------------------------------------
dfA <- data.frame(x = rockA$AU)
ggplot(dfA, aes(x)) + stat_ecdf(geom = "line", color = "red") + 
  scale_x_continuous(trans = "log10") + xlab("Au Grade (ppm)") + ylab("Probability") +
                                              ggtitle("Rock Type A Probability Function")+
  geom_vline(xintercept = 0.022) + geom_vline(xintercept = 0.095) + annotate("text", x = 0.045, y = 0.85, label = "IQR")

dfB <- data.frame(x = rockB$AU)
ggplot(dfB, aes(x)) + stat_ecdf(geom = "line", color = "green") + 
  scale_x_continuous(trans = "log10") + xlab("Au Grade (ppm)") + ylab("Probability") +
  ggtitle("Rock Type B Probability Function")+
  geom_vline(xintercept = 0.094) + geom_vline(xintercept = 0.308) + annotate("text", x = 0.18, y = 0.85, label = "IQR")

dfC <- data.frame(x = rockC$AU)
ggplot(dfC, aes(x)) + stat_ecdf(geom = "line", color = "blue") + 
  scale_x_continuous(trans = "log10") + xlab("Au Grade (ppm)") + ylab("Probability") +
  ggtitle("Rock Type C Probability Function")+
  geom_vline(xintercept = 0.4815) + geom_vline(xintercept = 1.142) + annotate("text", x = 0.8, y = 0.85, label = "IQR")
#----------------------------------------------------------------------------------------------------------------------

#Create qqplots for each rock type
qplot(sample = AU, data = dataset, shape = ROCKTYPE, color = ROCKTYPE) +
  ggtitle("Quantile Quantile Plots for\nEach Rock Type")

qqnorm(rockA$AU, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       plot.it = T, datax = FALSE)

qqplot(rockB$AU, rockA$AU, plot.it = TRUE,
       xlab = "Rock Type B",
       ylab = "Rock Type A",
       main = "Q-Q Plot: Rock Type A vs B")

qqplot(rockB$AU, rockC$AU, plot.it = TRUE,
       xlab = "Rock Type B",
       ylab = "Rock Type C",
       main = "Q-Q Plot: Rock Type B vs C",
       col = "darkblue")

qqplot(rockA$AU, rockC$AU, plot.it = TRUE,
       xlab = "Rock Type A",
       ylab = "Rock Type C",
       main = "Q-Q Plot: Rock Type A vs C",
       col = "darkred")

