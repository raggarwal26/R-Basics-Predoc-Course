#R PREDOC Data Visualization problem set 
library(tidyverse)
rio = read.csv("https://raw.githubusercontent.com/BUSN32100/data_files/master/rio.csv",
               stringsAsFactors = FALSE)
View(rio)
class(rio)
#rio is a data frame 
dim(rio)
#dimensions: 11538 rows, and 12 columns 
colnames(rio)
#"id","name","nationality","sex", "date_of_birth" "height""weight""sport" "gold" "silver""bronze" year_of_birth"
summary(rio)
#missing data for height and weight for some players
#11538 athletes competed in the 2016 summer olympics 
class(rio$nationality)
rio$nationality<-as.factor(rio$nationality)
levels(rio$nationality)
summary(rio$nationality)
#USA brought the most athletes and they brough 567 athletes
summary(rio$nationality[rio$sex== 'female'])
#country with the most female athletes was USA (303) followed by, China (251), Australia(220), Brazil(216), and Germany(204)
summary(rio$nationality[rio$sex=='male'])
#country with the most male atheltes was Brazil (269) followed by USA(264), Germany(237), France(234), and Australia (211)
summary(rio$name)
rio$name<-as.factor(rio$name)
rio_name_count<-summary(rio$name)
sum(rio_name_count>1)
#22 athletes have the same name 
sum(rio$gold>0)
#number of gold medals awarded = 619 
sum(rio$silver>0)
#number of silver medals awarded = 634
sum(rio$bronze>0)
#693  bronze medals awarded
#the number of each type of medal awarded isn't equal. gold is awarded less than the other two. Perhaps this could be due to mutliple people being awarded silver/bronze for an event, also for ties there could be more silver medals awarded
rio$total<- rio[,9]+rio[,10]+rio[,11]
View(rio$total)
rio$name[which.max(rio$total)]
summary(rio$total)
#michael phelps had the most medals (6)
subset(rio, name== "Michael Phelps")
#5 gold, 1 silver 
ggplot(data= rio, mapping=aes(x=total))+geom_histogram(bins= 6, fill= 'white',color='red')
rio$age<- 2016-(rio$year_of_birth)
View(rio)  
rio$name[which.max(rio$age)]
subset(rio, name== "Julie Brougham")
#the oldest Olympic Athlete is Julie Brougham who was 62 years old at the 2016 Summer Olympics 
rio$name[which.min(rio$age)]
subset(rio, name== "Ana Iulia Dascal")
#the youngest Olympic Athlete is Ana Iulia Dascal at age of 14
ggplot(data=rio, mapping=aes(x=age))+geom_histogram(bins=25, fill="hotpink",color="black")
medal_winners<- subset(rio, total>0)
medal_winners$name[which.max(medal_winners$age)]
subset(rio, name== "Nick Skelton")
#out of medal winners, Nick Skelton was the oldest athelte at age 59
medal_winners$name[which.min(medal_winners$age)]
subset(medal_winners, name== "Qian Ren")
#the youngest medal winner was Qian Ren at the age of 15 
ggplot(data=medal_winners, mapping=aes(x=age))+geom_histogram(bins=25, fill="lightblue", color="black")
#key observations in the diffences between the whole dataset and the medal winners, the bins with the highest number of athletees were in the mid 20s, but in the adjusted for medal winners, it was very clearly late 20s, also there were less athletes on the two age extremes accounted for when adjusted for medal winners
#these findings make sense since younger athletes often have had less years of training, and usually older athletes are less agile, so the smaller distribution makes logical sense
summary(medal_winners)
subset(rio, name=="Michael Phelps")
summary(rio)
ggplot(data=rio, mapping=aes(x=age))+geom_histogram(bins=10, fill="orange",color="black")+facet_wrap(sport~.)
#across the board, most sports have a mode near the 20s-30s
badminton_players<- subset(rio, sport=="badminton")
badminton_players[which.max(badminton_players$age), c("name","age")]
#Oldest athelte in badminton Robert Mateusiak  age:40
badminton_players[which.min(badminton_players$age), c("name","age")]
#youngest athlete in badminton Akane Yamaguchi age 19
aquatics_athletes<-subset(rio, sport=="aquatics")
aquatics_athletes[which.max(aquatics_athletes$age), c("name","age")]
#Oldest athelte in Aquatics Camila Pedrosa  41
aquatics_athletes[which.min(aquatics_athletes$age), c("name","age")]
#youngest athlete in aquatics is Ana Iulia Dascal age 14
shooting_athletes<-subset(rio, sport=="shooting")
shooting_athletes[which.max(shooting_athletes$age), c("name","age")]
#oldest athlete in shooting is Paulo Bernardo Reichardt  56
shooting_athletes[which.min(shooting_athletes$age), c("name","age")]
#youngest athlete in shooting is Aislin Jones age 16
ggplot(data=rio, mapping=aes(x= height))+
  geom_boxplot(color="pink")+facet_wrap(~sport)
ggplot(data=rio, mapping=aes(x=weight))+
  geom_boxplot(fill="lightblue",color="black")+facet_wrap(~sport)
ggplot(data=rio, mapping=aes(x=height, y=weight, color=sex))+geom_point(alpha=0.1)+facet_wrap(~sport)
