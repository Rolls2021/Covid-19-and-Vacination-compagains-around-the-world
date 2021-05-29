library(tidyverse)

library(dplyr)

library(readr)
library(flexdashboard)
library(xts)
library(dygraphs)
library(plotly)
library(tmap)
library(shiny)



##importing data##
covid_19_data <- read_csv("covid-19-data.csv")
Data_about_covid <- read_csv("Data_about_covid.csv")
population<-read_csv("population.csv")    
countriesbygroup<-read_csv("countriesbygroup.csv")

## Cleanning data or data processing ###


##importing data##
covid_19_data <- read_csv("covid-19-data.csv")
Data_about_covid <- read_csv("Data_about_covid.csv")
population<-read_csv("population.csv")    
countriesbygroup<-read_csv("countriesbygroup.csv")
#####Cleaning Data####
population[population == "Russian Federation"] <- "Russia"
countriesbygroup[countriesbygroup == "Russian Federation"] <- "Russia"

Covid_19<-confirmeddeath%>%
  select(location,population,total_cases,total_deaths,total_vaccinations,new_cases ,new_deaths)%>%
  transmute('Country Name'= location,'Population'=population, 'Cases'= total_cases, 'Deaths'=total_deaths,'vaccinations'=total_vaccinations, 'cases_after_vaccinations'=new_cases, 'deaths_after_vaccinations'= new_deaths )


population19<-population%>%  # Population in 2019
  mutate(Population=`2019`)%>%
  select(`Country Name`,`Country Code`,Population)

countriesbygroup<-countriesbygroup%>%
  rename(`Country Name`=TableName)


##########Merging tibbles#####
df<-inner_join (inner_join(countriesbygroup,population19,by="Country Name"),Covid_19,by= "Country Name")
###Elimination of duplicating colum###
df<-df%>%
      select(`Country Code.x`,`Country Name`, Region,IncomeGroup,Cases,Deaths,Population.x,vaccinations,`cases_after_vaccinations`,`deaths_after_vaccinations`)
####Creating New variables###
df<-df%>%
  mutate(`Cases_rate`=Cases/Population.x,`Deaths_rate`=Deaths/Population.x,`Rate_of_vaccinated_people`=vaccinations/Population.x,`Rate_of_cases_after_vaccinations`=cases_after_vaccinations/Population.x ,`Rate_of_death_after_vaccinations`=deaths_after_vaccinations/Population.x)

####


##### Graphics#########

###Graphic1## 
##Scatter plot comparing the rate of covid-19 cases and death rate in countries.

ggplot(df,aes(x=Cases_rate, y = Deaths_rate,color= Region))+
  geom_point(size=6, shape=2)+
  geom_smooth(method = lm)+
  xlab("Cases_rate")+ ylab("Death_rate")+
  theme_light()
ggplot(df,aes(x=Cases_rate, y = Death_rate,color= IncomeGroup))+
  geom_point(size=6, shape=2)+
  geom_smooth(method = lm)+
  xlab("Cases_rate")+ ylab("Death_rate")+
  theme_light()


####Graphic2###
### Scatter plot comparing COVID-19 cases with the rate of covid-19 cases and vaccination rates in regions around the world.
ggplot(df,aes(x=Cases_rate, y =Rate_of_vaccinated_people ,color= IncomeGroup))+
  geom_point(size=6, shape=2)+
  geom_smooth(method = lm)+
  xlab("Cases_rate")+ ylab("Rate_of_vaccinated_people ")+
  theme_light()

ggplot(df,aes(x=Cases_rate, y =Rate_of_vaccinated_people ,color= Region))+
  geom_point(size=6, shape=2)+
  geom_smooth(method = lm)+
  xlab("Cases_rate")+ ylab("Rate_of_vaccinated_people ")+
  theme_light()

####Scatter plot comparing post-vaccination case rate and post-vaccination death rate in countries.raphic3


ggplot(df,aes(x =Rate_of_cases_after_vaccinations, y =Rate_of_death_after_vaccinations ,color= Region))+
  geom_point(size=6, shape=2)+
  geom_smooth(method = lm)+
  xlab("Rate_of_cases _after_vaccinations")+ ylab("Rate_of_death_after_vaccinations ")+
  theme_light()



ggplot(df,aes(x =Rate_of_cases_after_vaccinations, y =Rate_of_death_after_vaccinations ,color= IncomeGroup))+
  geom_point(size=6, shape=2)+
  geom_smooth(method = lm)+
  xlab("Rate_of_cases _after_vaccinations")+ ylab("Rate_of_death_after_vaccinations ")+
  theme_light()




