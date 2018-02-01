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



#download list of every single active nba player

lastnameletter<-letters[seq( from = 1, to = 26 )]

#list of all last name pages
letterpage<-as.vector(rep(NA,26))
for (i in 1:length(lastnameletter)){
  letterpage[i]<-paste('http://www.basketball-reference.com/players/',lastnameletter[i],'/', sep='')
}

#active players on each page
playerlist<-list()
playernames<-list()

for(i in 1:length(letterpage)){
tryCatch({
playerlist[[i]]<-read_html(letterpage[i]) #get the html once
playernames[[i]]<-playerlist[[i]] #duplicate over to parse out names then will merge
  

playernames[[i]]<-playernames[[i]] %>% 
  html_nodes(xpath='//strong//a') %>%
    html_text() %>% 
    colsplit(" ",c('First','Last'))
  
  
playerlist[[i]]<-playerlist[[i]] %>% 
  html_nodes(xpath = '//strong//a/@href') %>%
             html_text() %>%
              colsplit(pattern="\\.",names=c('player','linkholder')) %>%
                mutate(linkholder=paste("http://www.basketball-reference.com",player,"/gamelog/2018/",sep='')) %>% 
                  subset(select=-(1)) %>% 
                    cbind(playernames[[i]]) },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

}

#scrape the gamelogs




gamelog<-list()
j<-1 #set a counter for the loop

for (i in 1:length(playerlist)){
  for (k in 1:length(playerlist[[i]]$linkholder)){
  tryCatch({
    gamelog[[j]] <- playerlist[[i]][k,'linkholder']%>%
      read_html() %>%
        html_nodes(xpath='//*[(@id = "pgl_basic")]') %>%
          html_table(fill=TRUE) %>% 
            as.data.frame() %>% 
            subset(Rk!='Rk')#remove the rows that are just repeated table headers
    
  attr(gamelog[[j]],'name')<-paste(playerlist[[i]][k,'Last'],playerlist[[i]][k,'First'], sep=' ')#add attributes i.e. labels to the game logs
  attr(gamelog[[j]],'url')<-playerlist[[i]][k,'linkholder']},
  
  error=function(e){cat('ERROR :',conditionMessage(e),'\n') 
      gamelog[[j]]<-NULL}
  )
    
    
  j<-j+1
  
  print(i)
  print(k)
  print(j)
  }
}    
    

#table of index and name
index<-vector()
name<-vector()
for (i in 1:length(gamelog)){
  tryCatch({index[i]<-i
  name[i]<-attributes(gamelog[[i]])$name},
  
  error=function(e){cat('ERROR :',conditionMessage(e),'\n') 
    name[i]<-NA}
  )
  print(i)
}

playerindex<-data.frame(index,name) %>% 
                na.omit()



#make a named list for app
userchoices<-as.list(playerindex$index)
names(userchoices)<-playerindex$name

#add a name column to each player gamelog
for(i in 1:dim(playerindex)[1]){
  gamelog[[playerindex$index[i]]]$name<-playerindex$name[[i]]
}


#subset based on seleced players
choices<-c(300,400,670)

userdata<-list()
for (i in 1:length(choices)){
  userdata[[i]]<-gamelog[[i]]
}

dim(rbindlist(userdata))

#process the data and add some variables
masterdf<-rbindlist(gamelog,use.name=TRUE,fill=TRUE) %>% data.frame()
masterdf$Date<-as.Date(masterdf$Date)


#cleanup masterdf and create dd and td variables..variables considered will be ast, pts, rbs, blocks, steals
masterdf[,c(1,2,3,10,12:32)]<-as.numeric(as.character(unlist(masterdf[,c(1,2,3,10,12:32)])))



#DD variable



for (i in 1:dim(masterdf)[1]){
  print(i)
  
  tryCatch({
    if(
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'PTS'] >=10)) || 
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'TRB'] >=10)) || 
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'STL'] >=10)) || 
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'BLK'] >=10)) || 
      ((masterdf[i,'PTS'] >=10) && (masterdf[i,'TRB'] >=10)) || 
      ((masterdf[i,'PTS'] >=10) && (masterdf[i,'STL'] >=10)) || 
      ((masterdf[i,'PTS'] >=10) && (masterdf[i,'BLK'] >=10)) || 
      ((masterdf[i,'TRB'] >=10) && (masterdf[i,'STL'] >=10)) || 
      ((masterdf[i,'TRB'] >=10) && (masterdf[i,'BLK'] >=10)) || 
      ((masterdf[i,'STL'] >=10) && (masterdf[i,'BLK'] >=10)) 
      
      
      
      
    ){masterdf[i,'DD']<-1}
    else{
      masterdf[i,'DD']<-0}
  },error=function(e){cat('ERROR :',conditionMessage(e),'\n')}
  )
}

#TD


for (i in 1:dim(masterdf)[1]){
  print(i)
  
  tryCatch({
    if(
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'PTS'] >=10) && (masterdf[i, 'TRB']>=10)) || 
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'PTS'] >=10) && (masterdf[i, 'STL']>=10)) ||
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'PTS'] >=10) && (masterdf[i, 'BLK']>=10)) ||
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'TRB'] >=10) && (masterdf[i, 'STL']>=10)) ||
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'TRB'] >=10) && (masterdf[i, 'BLK']>=10)) ||
      ((masterdf[i,'AST'] >=10) && (masterdf[i,'STL'] >=10) && (masterdf[i, 'BLK']>=10)) ||
      ((masterdf[i,'PTS'] >=10) && (masterdf[i,'TRB'] >=10) && (masterdf[i, 'STL']>=10)) ||
      ((masterdf[i,'PTS'] >=10) && (masterdf[i,'TRB'] >=10) && (masterdf[i, 'BLK']>=10)) ||
      ((masterdf[i,'PTS'] >=10) && (masterdf[i,'STL'] >=10) && (masterdf[i, 'BLK']>=10)) ||
      ((masterdf[i,'TRB'] >=10) && (masterdf[i,'STL'] >=10) && (masterdf[i, 'BLK']>=10)) 
      
      
      
      
      
    ){masterdf[i,'TD']<-1}
    else{
      masterdf[i,'TD']<-0}
  },error=function(e){cat('ERROR :',conditionMessage(e),'\n')}
  )
}


write.csv(masterdf,'201720gamelogs.csv', row.names=F)
