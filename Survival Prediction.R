
titanic = read.csv('C:/Users/rosha/Downloads/titanic.csv')
library(ggplot2)
library(plotly)
library(rpart.plot)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
ggplot(titanic,aes(x=Sex))+geom_bar()
ggplot(titanic,aes(x=Survived))+geom_bar()+theme_light()+
  labs(y='Passenger count', title='Titanic survival rates')
ggplot(titanic,aes(x=Sex,fill=Survived))+geom_bar()+theme_bw()+
  labs(y='Passenger count', title='Titanic survival rates')

ggplot(titanic,aes(x=Pclass,fill=Survived))+geom_bar()+theme_bw()+
  labs(y='Passenger count', title='Titanic survival rates')
ggplot(titanic,aes(x=Age))+geom_histogram(bindwidth=2)+theme_bw()+
  labs(y='Passenger count', title='Titanic survival rates')
ggplot(titanic,aes(x=Pclass,fill=Survived))+geom_bar()+theme_bw()+facet_wrap(~Pclass)
labs(y='Passenger count', title='Titanic survival rates')


ggplot(titanic,aes(x=Survived,y=Age))+geom_boxplot()+theme_bw()+
  labs(y='Passenger count', title='Titanic survival rates')
ggplot(titanic,aes(x=Age,fill=Survived))+geom_density(alpha=0.5)+theme_bw()+facet_wrap(Sex~Pclass)
labs(y='Passenger count', title='Titanic survival rates')
ggplot(titanic,aes(x=Age,fill=Survived))+geom_histogram(Bindwidth=5)+theme_bw()+facet_wrap(Sex~Pclass)
labs(y='Passenger count', title='Titanic survival rates')
titanic$Pclass=as.factor(titanic$Pclass)
titanic$Survived=as.factor(titanic$Survived)
titanic$Sex=as.factor(titanic$Sex)
#to fetch random data from yur dataset eg smple (5,3)=o/p 1,2,5 or 5,2,3 etc
s=sample(nrow(titanic),500) #where s is no of row of training data
s_train=titanic[s,] 
t_test=titanic[-s,] #fetching all the rows which is not present in training data
dtm=rpart(Survived~Pclass+Sex+Age,s_train,method='class')
rpart.plot(dtm,extra=101) #here extra=101 defines in leaf node which are correct and which are not correct
p=predict(dtm,t_test,type='class') #here running the test data in model dtm 
p
table(t_test$Survived, predicted=p)
#Accuracy=(Total positive+Total negative)/total 
#Here 303/387=0.816% 



