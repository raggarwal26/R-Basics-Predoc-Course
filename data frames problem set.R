#R PREDOC course: data visualization 
library(tidyverse)
state.df=data.frame(state.x77, Region=state.region, Division=state.division)
?state.x77
summary(state.df)
View(state.df)
state.df$Abbr<-state.abb
View(state.df)
print(state.df[1:3,])
state.df$Region<-NULL
print(state.df[1:3,])
state.df$Center.x<- state.center$x
state.df$Center.y<- state.center$y
print(state.df[1:3,])
upated_state<- state.df%>% filter(Center.x< -100)
View(upated_state)
final_state_data<- upated_state%>% filter(Murder> 9)
View(final_state_data)
which.max(final_state_data$Life.Exp)
#california has the highest average life expectancy, based on the parameters of having a greater than 9% murder rate and having a longitudinal coordinates below -100

