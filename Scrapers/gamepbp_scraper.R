library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(reshape2)
library(RCurl)
library(plotly)
library(ggplot2)
library(data.table)
library(rvest)
library(XML)
setwd('C:/Users/shavergc/Documents/nbagamelogs')

#Start out by generating the links to the pbp for the first four months of nba season
#Months
monthname<-c('october','november','december','january')
monthpage<-rep(NA,4) #October-Jan for 2018 season. Hardcoded for now
gamelist<-list() #initiate a blank list

for (i in 1:length(monthpage)){
  monthpage[i]<-paste('https://www.basketball-reference.com/leagues/NBA_2018_games-',monthname[i],'.html',sep='')



games<-read_html(monthpage[i])


urltobind<-games %>% html_nodes(".center a") %>% html_attr(name="href") %>% 
  strsplit("/2") %>% unlist %>%.[!odd(1:length(.))] %>% 
    data.frame("https://www.basketball-reference.com/boxscores/pbp/",.) 

colnames(urltobind)<-c('beginning','end')

gamelist[[i]]<-paste0(urltobind[,1],urltobind[,2])
}

pbplogstodate<-unlist(gamelist)
##################################################3
#Next will create a list of data frames of each pbp cleaned up


