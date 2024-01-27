#call needed package(s)
library(tidyverse)

#set your working directory first
#go to the file you want to open in the files tab in 
#the bottom right column, hit the wheel, more
#then select <<Set as working directory>>
#and the R with run the line below by itself

#If you know the path for your working directory write it as below 
#and comment my directory path out of setwd("")
setwd("~/ENVECON-118-SP24/4_Lecture/Lecture_5")

my_data<-read.csv("Lecture5.csv")

#see the top of the data
head(my_data)

#keep only year 1987 in the dataframe to use and 
#call it my_data2
my_data2<-filter(my_data,year==87)


scatter_Lect4<-ggplot(my_data2,aes(x=polpc,y=crmrte))+geom_point()
#add labels and titles to scatter_Lect4
scatter_Lect4<-scatter_Lect4+labs(x="X=Police Per Capita", y= "Y=Crime Rate",
                                  title="Scatter Plot of Y and X",
                                  subtitle="1987 Observations only (N=90)")

#show me the plot in the image window bottom right
scatter_Lect4


#regression from Lecture4
regLecture4<-lm(crmrte~polpc,my_data2)
#show output
summary(regLecture4)


#generate predicted crime rate using beta0_hat and beta1_hat
#estimated in the regression above. lm command creates several
#outputs that are associated with regLecture4
#one of them is regLecture4$fitted.values
#another is regLecture4$residuals
#and more, but for now lets focus on the fitted values

#add predicted values as an additional column
#to the data frame obsect my_data2
#we do this by mutating the data

my_data2<-mutate(my_data2,crmrte_hat=regLecture4$fitted.values)


#look what happens to my_data2 in the top right window
#It is now a datafreame of 90 obs/rows and 5 variables/columns 
#instead of the previous 4 columns/variables


#now we can plot predicted crimerate  from the nodel <<crmrte_hat>>
#and actual crime rate from the data <<crmrte>> and
#visually see if they are equal, which would mean they would 
#depict a 45 degree line starting at the origin, when the X axis
#values (crmrte) equal the y axis values (crmrte_hat)

#remember <<hat>> is a prediction from the model

#the scatter plot of crmrte and crmrte_hat
scatter_Lect5<-ggplot(my_data2,aes(x=crmrte,y=crmrte_hat))+geom_point()
#add labels and titles to scatter_Lect5
scatter_Lect5<-scatter_Lect5+labs(x="X=Crime Rate", y= "Y=Predicted Crime Rate",
                                  title="Scatter Plot of Y and X",
                                  subtitle="1987 Observations only (N=90)")

#show me the plot in the image window bottom right
scatter_Lect5



#the scatter plot of police per capita and crmrte_hat
scatter_Lect52<-ggplot(my_data2,aes(x=polpc,y=crmrte_hat))+geom_point()
#add labels and titles to scatter_Lect52
scatter_Lect52<-scatter_Lect52+labs(x="X=Police per Capita", y= "Y=Predicted Crime Rate",
                                    title="Scatter Plot of Y and X",
                                    subtitle="1987 Observations only (N=90)")

#show me the plot in the image window bottom right
scatter_Lect52



##now run a regression using all the data, that is my_data
#with N=630 observations, not just year 87 but all years
regLectureN630<-lm(crmrte~polpc,my_data)
#show results
summary(regLectureN630)

#compare the estimated intercept and slope for N=90
#with the ones for N=630


#they are different!



#also what happend to the standard errors of the estimates
#for the intercept beta0hat and the slope beta1hat
#from N=90 to N=630?

#the std errors dropped a lot from N=90 to when we use N=630
#increasing sample size helped make the estimated beta_hats
#more precisely estimated, with smaller stad errors


#last thing we will do in R studio in datahub
#generate fitted values and combine the fitted values 
#with the actual
#data in the same plot



#generate fitted values and add to my_data as additional column variable
#recall we call a column in a a dataframe with a $ sign and then say the column name

my_data<-mutate(my_data,crmrte_hat=regLectureN630$fitted.values)


#the scatter plot of police per capita and crmrte_hat
scatter_data_fittedVals<-ggplot(data=my_data)+
  geom_point(aes(x=polpc,y=crmrte,color="data"))+
  geom_point(aes(x=polpc,y=crmrte_hat,color="fitted by the model"))+
  labs(x="Police per Capita", y= "Crime Rate",
       title="Police per Capita and Fitted Values of Crime Rate (in Blue) and Crime Rate in Red",
       subtitle="All Observations, Full Sample (N=630)")

#show me the plot in the image window bottom right
scatter_data_fittedVals


