rm(list = ls())

##FUNKCJA - BANKIER
bankier<-function(n=8, sort_desc=F, filter_word = "brexit"){
  
  require(rvest)
  require(selectr)
  require(tidyverse)
  require(stringr)
  require(lubridate)
  
  art<-data.frame(matrix(ncol=3,nrow=10))
  colnames(art)<-c("Data publikacji","Tytu³", "Opis skrócony")
  arty<-NULL
  
  for (i in 1:n){
    html_main <- read_html(sprintf("https://www.bankier.pl/gielda/wiadomosci/%i",i))
    nodes <- html_nodes(html_main, "div")
    page_node <- html_nodes(html_main, "div.article")
    div_text <- html_text(page_node)
    div_text <- str_replace_all(div_text, "\n", "")
    div_text <- str_replace_all(div_text, "            ", " ")
    div_text<-str_split(div_text,"    ")
    
    for (j in 1:10){
      if (length(div_text[[j]])==7){
        art[j,1]<-substr(div_text[[j]][3],1,16)
        art[j,2]<-div_text[[j]][4]
        art[j,3]<-str_replace_all(div_text[[j]][5]," Czytaj dalej","")
      } else {
        art[j,1]<-substr(div_text[[j]][2],2,17)
        art[j,2]<-div_text[[j]][3]
        art[j,3]<-str_replace_all(div_text[[j]][4]," Czytaj dalej","")
      }
      
    }
    
    arty<-rbind(arty,art)
  }
  
  if (!is.null(filter_word)) {
    arty<-arty[union(grep(filter_word,arty[[2]]),grep(filter_word,arty[[3]])),]
  }
  
  if (sort_desc==T){
    arty<-arty[as_date(rev(order(ymd_hm(arty$`Data publikacji`)))),]
  }
  return(arty)
}

  

#############################################################################################
################## EXAMPLES

a<-bankier()
view(a)

b<-bankier(n=3, sort_desc = T, filter_word = NULL)
view(b)


#dodatkowy przyklad jakby nie bylo nic z brexitem:)
d<-bankier(n=5, sort_desc = T, filter_word = "cen")
view(d)
