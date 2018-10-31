#Load packages
library(dplyr)
library(ggplot2)
library(tidyr)

life_expectancy <- UNdata_Export_20181030_160953818
head(life_expectancy,n=10)
tail(life_expectancy,n=10)

#Find out life expectancy
data1 <- life_expectancy %>%
  filter(`Year(s)` %in% c("1980-1985","1995-2000","2000-2005","2010-2015"))%>%
  mutate(Sub_Year=paste(Variant,`Year(s)`,sep="_"))%>%
  mutate(Sub_Year=gsub("-","_",Sub_Year))%>%
  select(-Variant,-`Year(s)`)%>%
  spread(Sub_Year,Value)%>%
  mutate(diff_2000_1980=Medium_1995_2000-Medium_1980_1985, diff_2015_2000=Medium_2010_2015-Medium_2000_2005)
  #Remove NA value
data2 <- na.omit(data1)

#Find out top 3 countries that improve life expectancy best and worst from 1980~2000
worst1980_2000 <- data2%>%
  arrange(diff_2000_1980)%>%
  head(3)

best1980_2000 <- data2%>%
  arrange(desc(diff_2000_1980))%>%
  head(3)

worst2015_2000 <- data2%>%
  arrange(diff_2015_2000)%>%
  head(3)

best2015_2000 <- data2%>%
  arrange(desc(diff_2015_2000))%>%
  head(3)

#Visualizing Data of countries improve best from 2000 to 2015 (MDGs effect)
ggplot(data2,aes(x=diff_2015_2000,y=diff_2000_1980,label=`Country or Area`,col=diff_2015_2000))+
  geom_point()+
  scale_x_continuous(limits=c(-8,18))+
  scale_y_continuous(limits=c(-15,18))+
  scale_color_gradient(low="red",high="blue")+
  geom_text(data=best2015_2000, size=2.5, col="black")+
  geom_text(data=worst2015_2000, size=2.5, col="black")+
  labs(title="Life Expectancy Best & Worst from 2000-2015",
       caption="Source:United Nations",
       x="2000~2015",
       y="1980~2000")+
  theme_bw()

#Visualizing Data of countries improve best from 1980 to 2000 (Pre MDGs)
ggplot(data2,aes(x=diff_2000_1980,y=diff_2015_2000,label=`Country or Area`,col=diff_2000_1980))+
  geom_point()+
  scale_x_continuous(limits=c(-16,20))+
  scale_y_continuous(limits=c(-10,18))+
  scale_color_gradient(low="red",high="blue")+
  geom_text(data=best1980_2000, size=2.5, col="black")+
  geom_text(data=worst1980_2000, size=2.5, col="black")+
  labs(title="Life Expectancy Best & Worst from 1980-2000",
       caption="Source:United Nations",
       x="1980~2000",
       y="2000~2015")+
  theme_bw()
