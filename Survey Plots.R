data <- read.csv('Survey on career interest of undergraduate.csv',header = TRUE,stringsAsFactors = FALSE)
head(data)
library(dplyr)
library(ggplot2)
library(tidyr)

##Rename Variables
new_data <- data %>% rename(University = What.is.your.University.) %>%
            rename(DegreeProgramme = What.is.your.Studying.Degree.Programme.) %>%
            rename(HigherStudies = Do.you.wish.to.do.higher.studies.) %>%
            rename(WorkPlace = Place.interest.to.work) %>%
            rename(Distance = How.far.should.you.hope.live.from.work.) %>%
            rename(Salary = Expecting.salary.range.) %>%
            rename(Sector = Wish.to.work.sector.) %>%
            rename(Field = Wish.to.work.industry.field.) %>%
            rename(job = Wish.to.work.industry.field.job.) %>%
            rename(Position =In.a.Department.which..position.would.u.prefer.)
           


##Correct Typos in Salary
new_data <- new_data %>% mutate(Salary=replace(Salary,Salary=='Gather than Rs. 150 000','Greater than Rs. 150 000')) %>%
  mutate(Salary=replace(Salary,Salary=='Geater than Rs. 150 000','Greater than Rs. 150 000'))

##Correct Typos in Distance
new_data <- new_data %>% mutate(Distance=replace(Distance,Distance=='Geater than 30 km','Greater than 30 km')) %>%
  mutate(Distance=replace(Distance,Distance=='Gather than 30 km','Greater than 30 km'))
new_data$Distance %>% head()

##Drop Na Values
new_data <- drop_na(new_data) 

##Sort Values By Salary

new_data <- new_data %>% arrange(Salary)

new_data$Salary <- factor(new_data$Salary,levels = c('Rs.25 000 to 50 000','Rs.50 000 to 100 000','Rs.100 000 to 150 000','Greater than Rs. 150 000'))
new_data$Distance <- factor(new_data$Distance,levels = c('Less than 5 km','5 km to 10 km','10 km to 30 km','Greater than 30 km'))
##Filter values
t1 <- new_data %>% filter(University=='SLIIT')
t2 <- new_data %>% filter(University=='University of Moratuwa' | University=='University of Colombo' | University=='University of Sri Jayewardenepura')


col1<-as.data.frame(table(t1$Salary)*100/nrow(t1))
col2<-as.data.frame(table(t2$Salary)*100/nrow(t2))
col1$name <- 'SLIIT'
col2$name <- 'Local Universities'


t3 <- rbind(col1,col2)
t3 <- t3 %>% rename(Sal=Var1)

## 1-Plot Salary
ggplot(t3) + geom_bar(aes(x = Sal, y = Freq, fill=name),
                      position = 'dodge', stat = 'identity')+
                xlab('Percentage(%)')+
                ylab('Salary')+ 
                ggtitle('SLIIT vs Local University Salary Expectations')+
                coord_flip()

##########################################################################
##########################################################################
##2-Commute Distance Plot
col1<-as.data.frame(table(t1$Distance)*100/nrow(t1))
col2<-as.data.frame(table(t2$Distance)*100/nrow(t2))
col1$name <- 'SLIIT'
col2$name <- 'Local Universities'


t3 <- rbind(col1,col2)
t3 <- t3 %>% rename(Dis=Var1)


ggplot(t3) + geom_bar(aes(x = Dis, y = Freq, fill=name),
                      position = 'dodge', stat = 'identity')+
  xlab('Percentage(%)')+
  ylab('Distance')+ 
  ggtitle('SLIIT vs Local University Commute Distance')+
  coord_flip()
######################################################################
######################################################################
##3-HigherStudies Plot
col1<-as.data.frame(table(t1$HigherStudies)*100/nrow(t1))
col2<-as.data.frame(table(t2$HigherStudies)*100/nrow(t2))
col1$name <- 'SLIIT'
col2$name <- 'Local Universities'


t3 <- rbind(col1,col2)
t3 <- t3 %>% rename(Hig=Var1)


ggplot(t3) + geom_bar(aes(x = Hig, y = Freq, fill=name),
                      position = 'dodge', stat = 'identity')+
  xlab('HigherStudies')+
  ylab('Percentage(%)')+ 
  ggtitle('SLIIT vs Local University Higher Studies')

###############################################################################
##############################################################################
###4-Expected Salary Vs Commute Distance Graph

ggplot(new_data,aes(x=Distance,y=Salary))+
  geom_bin2d()+
  scale_fill_gradient(high = 'red',low = 'blue')+
  xlab('Commute Distance')+
  ylab('Expected Salary')

##########################################################################
##########################################################################
##5-Position Plot
col1<-as.data.frame(table(t1$Position)*100/nrow(t1))
col2<-as.data.frame(table(t2$Position)*100/nrow(t2))
col1$name <- 'SLIIT'
col2$name <- 'Local Universities'


t3 <- rbind(col1,col2)
t3 <- t3 %>% rename(Pos=Var1)


ggplot(t3) + geom_bar(aes(x = Pos, y = Freq, fill=name),
                      position = 'dodge', stat = 'identity')+
  xlab('Position')+
  ylab('Percentage(%)')+ 
  ggtitle('SLIIT vs Local University job Position')
###############################################################################
##############################################################################
###6-Expected Salary Vs Field Graph

ggplot(new_data,aes(x=Field,y=Salary))+
  geom_point()+facet_grid(WorkPlace~.)+
  xlab('Field')+
  ylab('Salary')

######################################################################
######################################################################
##7-Immigration Plot
col1<-as.data.frame(table(t1$WorkPlace)*100/nrow(t1))
col2<-as.data.frame(table(t2$WorkPlace)*100/nrow(t2))
col1$name <- 'SLIIT'
col2$name <- 'Local Universities'


t3 <- rbind(col1,col2)
t3 <- t3 %>% rename(work=Var1)


ggplot(t3) + geom_bar(aes(x = work, y = Freq, fill=name),
                      position = 'dodge', stat = 'identity')+
  xlab('WorkPlace')+
  ylab('Percentage(%)')+ 
  ggtitle('SLIIT vs Local University Immigration')


###############################################################################
##############################################################################
###8-Expected Salary Vs WorkPlace Graph

ggplot(new_data,aes(x=WorkPlace,y=Salary))+
  geom_bin2d()+
  scale_fill_gradient(high = 'red',low = 'blue')+
  xlab('WorkPlace')+
  ylab('Expected Salary')


unique(new_data$WorkPlace)
colnames(new_data)



