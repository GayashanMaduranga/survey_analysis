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
           
##Correct Typos
new_data <- new_data %>% mutate(Salary=replace(Salary,Salary=='Gather than Rs. 150 000','Greater than Rs. 150 000')) %>%
  mutate(Salary=replace(Salary,Salary=='Geater than Rs. 150 000','Greater than Rs. 150 000'))

##Drop Na Values
new_data <- drop_na(new_data) 

##Sort Values By Salary

new_data <- new_data %>% arrange(Salary)

new_data$Salary <- factor(new_data$Salary,levels = c('Rs.25 000 to 50 000','Rs.50 000 to 100 000','Rs.100 000 to 150 000','Greater than Rs. 150 000'))


t1 <- new_data %>% filter(University=='SLIIT')
t2 <- new_data %>% filter(University=='University of Moratuwa' | University=='University of Colombo' | University=='University of Sri Jayewardenepura')


col1<-as.data.frame(table(t1$Salary)*100/nrow(t1))
col2<-as.data.frame(table(t2$Salary)*100/nrow(t2))
col1$name <- 'SLIIT'
col2$name <- 'Local Universities'


t3 <- rbind(col1,col2)
t3 <- t3 %>% rename(Sal=Var1)




ggplot(t3) + geom_bar(aes(x = Sal, y = Freq, fill=name),
                      position = 'dodge', stat = 'identity')+
                xlab('Percentage')+
                ylab('Salary')+ 
                ggtitle('SLIIT vs Local University Salary Expectations')+
                coord_flip()


colnames(data)


