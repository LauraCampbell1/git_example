install.packages("readr")
library(readr)
OC <- read_tsv("data/OystercatcherData.txt")
summary(OC)

source("Multiplot_script.R")

#set Month, FeedingType and FeedingPlot as factors.
OC$Month<- as.factor(OC$Month)
OC$FeedingType<- as.factor(OC$FeedingType)
OC$FeedingPlot<- as.factor(OC$FeedingPlot)
summary(OC)

library(ggplot2)
p1<- ggplot(data=OC, aes(x=FeedingType, y=ShellLength))+geom_boxplot()+theme_classic()
p1
p2<- ggplot(data=OC, aes(x=FeedingPlot, y=ShellLength))+geom_boxplot()+theme_classic()
p2
p3<- ggplot(data=OC, aes(x=Month, y=ShellLength))+geom_boxplot()+theme_classic()
p3

multiplot(p1, p2, p3, cols=2)

#Table function is an easy way of checking the number of observations per month, per feeding plot, and per feeding type 

table(OC$Month)
table(OC$FeedingPlot)
table(OC$FeedingType)

M1 <-lm(ShellLength~FeedingType*FeedingPlot*Month, data=OC)
print(summary(M1), digits=2)

drop1(M1, test = "F")


plot(M1)


#let's look at the residuals for each predictor separately

E1<- rstandard(M1) #extract standardised residuals
p1r<-ggplot(data=OC, aes(x=FeedingType, y=E1))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_classic()
p1r

p2r<-ggplot(data=OC, aes(x=FeedingPlot, y=E1))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_classic()
p2r

p3r<-ggplot(data=OC, aes(x=Month, y=E1))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_classic()
p3r


multiplot(p1r, p2r, p3r, cols = 2)


MyData <- expand.grid(
 FeedingType=levels(OC$FeedingType),
 FeedingPlot=levels(OC$FeedingPlot),
 Month=levels(OC$Month))

MyData


#do the actual prediction

P1<- predict(M1, newdata=MyData, se=TRUE)

#add the predicted values
MyData$Fit <-P1$fit
MyData$SE <- P1$se.fit
MyData$se.low <- P1$fit - 1.96 * P1$se.fit
MyData$se.up <- P1$fit + 1.96 * P1$se.fit
print(MyData, digits =3)

#let's visualise

library(magrittr)
library(dplyr)

MyData %>% 
  mutate(treatment = paste(FeedingType, FeedingPlot, Month)) %>% 
  ggplot(aes(x= treatment, y=Fit))+
  geom_pointrange(aes(ymin=se.low, ymax=se.up)) +
  labs(x="", y="Fitted Values")+
  coord_flip() +
  theme_classic()

p<- ggplot()
p<- p+ xlab ("Feeding type") + ylab("Shell length")
p<- p+ theme(text = element_text(size = 15)) +theme_bw()

#add points for the fitted values
p<- p+ geom_point(data= MyData,
                  aes(x = FeedingType,
                      y= MyData$Fit,
                      size=6),
                  col=("black"))
#add error bars
p<-p+geom_errorbar(data = MyData,
                   aes(x= FeedingType,
                ymax=se.up, 
                ymin=se.low), 
                width=0.2)
p
