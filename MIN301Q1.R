#MIN301 Assignment 2 Q1
ashdat <- read.csv("ash_dat.csv", header = T)

ashdat<-ashdat[2:5]
ashdat <- head(ashdat,-3)

ggplot(ashdat, aes(ashdat$Fines)) + stat_ecdf(geom = "line", color = "green") + 
    ggtitle("Fines Probability Function") + xlab("Fines") + ylab("Probability")+
 geom_vline(xintercept = (quantile(ashdat$Fines, c(0.05,0.95))))
#other plots were created in the same syntax

Thickness<-data.frame(quantile(ashdat$Thickness, c(0.05,0.95)))
Ash<- data.frame(quantile(ashdat$Ash, c(0.05,0.95)))
Fines <- data.frame(quantile(ashdat$Fines, c(0.05,0.95)))
Heat <- data.frame(quantile(ashdat$Heat,c(0.05,0.95)))
df <- cbind(Thickness,Ash,Fines,Heat)
colnames(df) <- c("Thickness (m)","Ash (%)","Fines (%)","Heat (%)")
write.csv(df,"confidence_intervals_q1.csv")