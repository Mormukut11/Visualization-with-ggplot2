movies<-read.csv(file.choose())
library("ggplot2")
library("data.table")
#df<-fread("Economist_Assignment_Data.csv",drop=1)
head(movies)
colnames(movies)<-c('name','genre','criticsratings','audienceratings','budgetmillions','releaseyear')
head(movies)
str(movies)
summary(movies)
levels(movies$genre)
movies$releaseyear<-as.factor(movies$releaseyear)
levels(movies$releaseyear)
#factor id used for unique identity have repetation
ggplot(data=movies,aes(x=criticsratings,y=audienceratings,color=genre,size=budgetmillions))+geom_point()+ facet_grid(.~genre)
#ggplot(data=mtcars,aes(x=factor(cyl),y=mpg))+geom_point(color='green')
#head(mtcars)
#str(mtcars)
#mapping of colors
q<-ggplot(data=movies,aes(x=criticsratings,y=audienceratings,color=genre,size=budgetmillions))#+geom_point()+geom_line()
typeof(q)
q+geom_point(aes(x=budgetmillions)+xlab("budget in millions"))#try
q+geom_line()+geom_point()
#setting of colors and size
r<-ggplot(data=movies,aes(x=criticsratings,y=audienceratings))+geom_point(color='red',size=3)
r+geom_point(aes(color='yellow'))#this is mapping take random balue
r+geom_point(aes(size=budgetmillions))
#alpha is used for transparency

z<-ggplot(data=movies,aes(x=criticsratings,y=audienceratings,color=genre))
#typeof(q)
z+geom_point(aes(x="releaseyear")+xlab("releaseyear"))

ggplot(data=movies,aes(x=budgetmillions))+
  geom_histogram(binwidth=10,color='blue',fill='green')
#total budget =300 no of histograms =300/10
#histogram is used for continuous but barplot is used for factor variable
ggplot(data=movies,aes(x=budgetmillions))+
  geom_histogram(binwidth=10,color='black',aes(fill=genre))


ggplot(data=movies,aes(x=criticsratings,y=audienceratings))+geom_point(color='red',size=3)+
  facet_grid(.~genre)


ggplot(data=movies,aes(x=budgetmillions))+
  geom_density(color='black',stack=10,aes(fill=genre))

ggplot(data=movies,aes(x=audienceratings))+
  geom_histogram(binwidth=10,color='black',fill='white')

ggplot(data=movies,aes(x=criticsratings))+
  geom_histogram(binwidth=10,color='black',fill='white')

ggplot(data=movies,aes(x=criticsratings))+
  geom_histogram(binwidth=10,color='black',fill='white')
#if the data is not continuous fatorial
head(mpg)
str(mpg)
levels(manufacturer)
ggplot(mpg,aes(x=class))+geom_bar()
class(mpg$manufacturer)
as.factor(mpg$manufacturer)
ggplot(mpg,aes(x=class))+geom_bar(aes(fill=drv))

#bar for categorical and histogram for continuous
#17/04/2017
ggplot(mpg,aes(x=class))+geom_bar(aes(fill=drv),position='dodge')
ggplot(mpg,aes(x=class))+geom_bar(aes(fill=drv),position='fill')#% count
ggplot(mpg,aes(x=class))+geom_bar(aes(fill=drv),position='stack')
#box plot
head(mtcars)
str(mtcars)
ggplot(mtcars,aes(x=factor(cyl),y=mpg))+geom_boxplot()#verticals lines viscors
# . dot represent outliers
ggplot(mtcars,aes(x=factor(cyl),y=mpg))+geom_boxplot(aes(fill=factor(cyl)),color='black')+theme_classic()

ggplot(data=movies,aes(x=genre,y=audienceratings,color=genre))+geom_boxplot(size=1.2,fill='pink')+geom_point()
ggplot(data=movies,aes(x=genre,y=audienceratings,color=genre))+geom_boxplot(size=1.2,fill='pink')+geom_jitter()

ggplot(data=movies,aes(x=genre,y=audienceratings,color=genre))+geom_boxplot(size=1.2,fill='pink',alpha=.6)+geom_jitter()


#18 april
ggplot(data=movies,aes(x=budgetmillions))+
  geom_histogram(binwidth=10,color='black',aes(fill=genre))+
  coord_cartesian(ylim=c(0,50))

#+facet_grid(.~genre)
ggplot(data=movies,aes(x=budgetmillions))+
  geom_histogram(binwidth=10,color='black',aes(fill=genre))+
  facet_grid(genre~.,scales='free')#+classic_theme()
#better form of ylim function
ggplot(data=movies,aes(x=criticsratings,y=audienceratings,color=genre))+geom_point(size=3)+
  facet_grid(genre~releaseyear)+ coord_cartesian(ylim=c(0,100))#+geom_smooth()

ggplot(data=movies,aes(x=criticsratings,y=audienceratings,color=genre))+geom_point(size=3)+
  geom_smooth(fill=NA)+facet_grid(genre~releaseyear)+xlim(0,100)+ylim(0,50)
help(geom_smooth)
#layer data,aes,geometry,facetgrid for partition,smooth,xlim,ylim,themelayer to change x and y
ggplot(data=movies,aes(x=criticsratings,y=audienceratings,color=genre))+geom_point(size=3)+
  geom_smooth(span=.3)+facet_grid(genre~releaseyear)+xlim(0,100)+ylim(0,50)
#geom_smooth(method='lm',formula=y~log(x),se=FALSE,color='red')

#geom_smooth()

ggplot(data=mtcars,aes(x=mpg,y=wt,color=cyl,size=hp))+
  geom_point()+geom_smooth(fill=NA)

ggplot(data=mtcars,aes(x=mpg,y=wt,color=cyl,size=hp))+
  geom_point()+geom_smooth(method='lm',se=F)

ggplot(data=mtcars,aes(x=mpg,y=wt,color=cyl))+
  geom_point()+geom_smooth(se=F,method='lm')+facet_wrap(~cyl)
#themes
library(ggthemes)
help(ggthemes)
z<-ggplot(data=mtcars,aes(x=mpg,y=wt,color=gear,size=hp))
z+geom_point()+theme_economist()
z+geom_point()+theme_wsj()
z+geom_point()+theme_fivethirtyeight()
z+geom_point()+theme_linedraw()
z+geom_point()+theme_excel()

#legend has text items

o<-ggplot(data=movies,aes(x=budgetmillions))
p<- o+geom_histogram(binwidth=10,color='black',aes(fill=genre))
p+xlab("money axis")+ylab("no of movies")+
  theme(axis.title.x=element_text(color="red",size=25),
        axis.title.y=element_text(color="blue",size=25),
        axis.text.x=element_text(color="red",size=15),
        axis.text.y=element_text(color="blue",size=15),
        legend.title=element_text(color="black",size=10),
        legend.text=element_text(color="black",size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color='red',size=15,family="roman"))+
  ggtitle("movies budget distribution")
help(theme)
#tibble and data frame
#data draw from sql
#time date analysis
