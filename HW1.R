setwd("~/Desktop/Adv. Stats/HW1")
rm(list=ls())
workload<-readRDS("workload.rds")
dataframe <- subset(workload,select=c("Home workload"));dataframe
dataframe$'Home Workload Label' <- factor(dataframe$`Home workload`,
                                     levels = c(1,2,3,4,5,6),
                                     labels = c("Mom: FT; Dad: FT",
                                                "Mom: PT; Dad: FT",
                                                "Mom: N; Dad: FT",
                                                "Mom: FT; Dad: PT or NE",
                                                "Mom: NE; Dad: NE",
                                                "Other"));dataframe$'Home Workload Label' 
levels(dataframe$`Home Workload Label`)
library("dplyr")
n=nrow(dataframe);n

### Frequency
freq <-dataframe %>%
  group_by(`Home Workload Label`) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)); freq

### Relative Frequency
relative_frequency <- dataframe %>%
  group_by(`Home Workload Label`) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate('Relative Frequency'=round(Frequency/n,digits=2));


par(mar=c(5,5,5,2)) #Increase the plot section margin size
count<- table(dataframe$`Home Workload Label`); count

#Sample barplot with las & cex.names attributes
barplot(count, 
        main="Home Workload Distribution",
        xlab="Home Workload", 
        ylab="Frequency", 
        ylim=c(0,500),
        border="red", col="lightblue", las=2,cex.names=0.6,
)

#Barplot for homework
barplot(count, 
        main="Home Workload Distribution",
        xlab="Home Workload", 
        ylab="Frequency", 
        ylim=c(0,500),
        border="red", col="lightblue"
)

#Creating a Pie Chart
piepercent<- round(count/n*100, 1); piepercent
pie(piepercent, 
    labels=paste(piepercent,"%",sep=""),
    main="Home Workload Distribution (Percent)",
    col=rainbow(length(count)))
legend("topright", 
       c("Mom: FT; Dad: FT",
        "Mom: PT; Dad: FT",
         "Mom: NE; Dad: FT",
         "Mom: FT; Dad: PT or NE",
         "Mom: NE; Dad: NE",
         "Other"),
          cex=0.8,
       fill=rainbow(length(count))
)
setwd("~/Desktop/Adv. Stats/Lecture1/Homework1_June05")
rm(list=ls())
applSurvey<-readRDS("MBA.rds")
applSurvey$Universitylabel <- factor(applSurvey$University,
                                     levels=c(1,2,3,4),
                                     labels=c(1,2,3,4))

applSurvey$Degreelabel <- factor(applSurvey$Degree,
                                 levels=c(1,2,3,4),
                                 labels=c("BA", "B.Eng", "BBA", "Other"))

levels(applSurvey$Universitylabel)
levels(applSurvey$Degreelabel)
n=nrow(applSurvey);n #Total number of observations
addmargins(table(applSurvey$Universitylabel,applSurvey$Degreelabel))

#Row relative frequency
table(applSurvey$Universitylabel,applSurvey$Degreelabel) %>%
  prop.table(margin = 1) %>%
  round(2);

#Col relative frequency
table(applSurvey$Universitylabel,applSurvey$Degreelabel) %>%
  prop.table(margin = 2) %>%
  round(2);

#Side by side bar chart
count <- table(applSurvey$Degreelabel,applSurvey$Universitylabel); count
barplot(count,
        main="Undergraduate Applications & University Study ",
        xlab="University",
        ylab="Frequency",
        col=c("darkblue","red","green","yellow"),
        cex=1.2,cex.axis=1.2,cex.lab=1.2,
        legend=rownames(count),args.legend = list(title="Degree"), beside=T)
adultsEdu<-readRDS("GSS2014(1).rds")
dataset.Age <-subset(adultsEdu,select=c(CASEID,AGE,EDUC));dataset.Age
# Draw histogram
hist(dataset.Age$EDUC,
     breaks=10,
     freq=T,
     main = "Histogram of American Adults Educations",
     xlab = "Education",
     ylab = "Frequency",
     labels = "T",
     xlim = c(0,20),
     ylim = c(0,900),
     col = "grey",
     border = "red",
     cex=1.2, cex.lab=1.2,cex.axis=1.2,
     las=1,
)
rm(list=ls())
setwd("~/Desktop/Adv. Stats/Lecture1/Homework1_June05")
olympicsMedal <- readRDS("OlympicsMedal.rds")
library("dplyr")
#yearList <-ts(olympicsMedal$Year,
 #             start=c(1924, 1),
  #            end=c(2014,12),
   #           frequency=12)
CanadaMedallist<- ts(olympicsMedal$Canada,
          start=c(1924, 1), #1st month of 1976
          end=c(2014, 12),  #12th month of 2016
          frequency=1) 
plot(CanadaMedallist, 
     main="Time Series Graph: Gasoline Price1", 
     ylab="Number of medals",
     col="red",
     type="l", #line. Other options: p, o, b, h, s, S
     lwd=2, #line thickness 
     cex=1.2, cex.lab=1.2, cex.axis=1.2,
     las=1, #rotate the value of y-axis
)
USMedalList<- ts(olympicsMedal$'United States',
                     start=c(1924, 1), #1st month of 1976
                     end=c(2014, 12),  #12th month of 2016
                     frequency=1) 

#Time series graph
require(graphics)
ts.plot(USMedalList, CanadaMedallist, 
        main="Time Series Graph: US & Canada Olympics medal study",
        ylab="Total number of medals",
        lwd=1, #1.5
        col=c("red", "blue"),
        lty=c(1:3))
legend('topleft',legend=c("USMedalList","CanadaMedallist"),col=c("red","blue"),
       lwd=2,lty=1:3, bty="n") 

##Numerical descriptive 
adultsEduload<-readRDS("GSS2014(1).rds")
df_subset <- subset(adultsEduload, select=c(CASEID,RINCOME,CHILDS,EDUC))

mean(df_subset$RINCOME, na.rm=TRUE)
median(df_subset$RINCOME, na.rm=TRUE)
boxplot(df_subset$RINCOME, 
        main = "Box Plot on Income",
        xlab="Income (Dollars)",
        col="lightblue",
        horizontal=TRUE)
Range <- max(df_subset$RINCOME)-min(df_subset$RINCOME);Range
range(df_subset$RINCOME,na.rm=TRUE)

var(df_subset$RINCOME,na.rm=TRUE)
sd(df_subset$RINCOME, na.rm=TRUE)
#Coefficient of variance
coeff_var <- sd(df_subset$RINCOME,na.rm=TRUE)/mean(df_subset$RINCOME,na.rm=TRUE);coeff_var


plot(df_subset$RINCOME, df_subset$EDUC,
        main = "Box plot on Income",
        xlab = "Income (Dollars)",
        ylab = "Years of education",
        col = "blue",
        pch = 19,
        cex=1.2,cex.lab=1.2,cex.axis=1.2
)
cor(df_subset$RINCOME,df_subset$EDUC,use="pairwise")

quantile(df_subset$RINCOME,na.rm=TRUE)
quantile(df_subset$RINCOME, probs=c(0,.01,.05,.10,.15,.25,.30,.50,.75,.90,.95,.99,1),na.rm=TRUE)
quantile(df_subset$RINCOME, probs=c(0,.01,.05,.10,.15,.25,.30,.35,.45,.50,.55,.60,.65,.70,.75,.80,.85,.90,.95,.99,1),na.rm=TRUE)
