data <- read.csv('Survey on career interest of undergraduate.csv',header = TRUE,stringsAsFactors = FALSE)
head(data)
library(dplyr)
library(ggplot2)

new_data <- data %>% rename(University = What.is.your.University.) %>%
            rename(DegreeProgramme = What.is.your.Studying.Degree.Programme.) %>%
            rename(HigherStudies = Do.you.wish.to.do.higher.studies.) %>%
            rename(WorkPlace = Place.interest.to.work) %>%
            rename(Distance = How.far.should.you.hope.live.from.work.) %>%
            rename(Salary = Expecting.salary.range.) %>%
            rename(Sector = Wish.to.work.sector.) %>%
            rename(Field = Wish.to.work.industry.field.) %>%
            rename(job = Wish.to.work.industry.field.job.) %>%
            rename(Position =In.a.Department.which..position.would.u.prefer.) %>%
            select(Position) %>% head()

colnames(data)
