

library(twitteR)
library(MASS)
library(gdata)
library(wordcloud)

Sys.setlocale("LC_TIME","us")

searchTag="#EkstraWeekend" # The search term. Besides the usual skipt words, add words in OokEruit

OokEruit<-c("sr11","SR11","3FM","3fm","ekstraweekend")
OverSlaWoorden<-c(OokEruit,"om","I'm","weer","met","van","naar","op","een","te","er","En","nu","is","De","1,","w/","de","at","niet","Nu","maar","het","RT","nog","in","of","(@","","|","dan","voor","wat","en")


tl1<-searchTwitter(searchTag,n=1200,since=NULL, until=NULL)
tl2<-twListToDF(tl1)
tl2$text<-gsub("\n"," ",tl2$text,ignore.case=FALSE,fixed=FALSE)
tl2$text<-gsub("\t"," ",tl2$text,ignore.case=FALSE,fixed=FALSE)

sources <- gsub("&lt;/a&gt;", "",  tl2$statusSource)
sources <- strsplit(sources, "&gt;")
tl2$statusSource<- sapply(sources, function(x) ifelse(length(x) > 1, x[2], x[1]))

write.table(tl2, file = "C:/Users/jvdt/Documents/MSaccessDB/tmp.txt", sep = "\t")
Sys.setlocale("LC_TIME","")

RemoveTags<-function(d)
{
	d<-gsub("!","", d)
	#d<-gsub(".","", d)
	d<-gsub(")","", d)
	d<-gsub("","", d)
	d<-gsub(",","", d)
	d<-gsub("#","", d)

	return(d)
}

#tl2<-read.table("C:/Users/jvdt/Documents/MSaccessDB/BasicJTdata.txt", sep = "\t",header=TRUE)

par(mfrow = c(2, 3))
pie(table(tl2$statusSource))
DagPatroon<-table(gsub(" ","",paste(substr(tl2$created, start=0, stop=10),paste(substr(tl2$created, start=12, stop=13),substr(tl2$created, start=15, stop=16)))))
plot(DagPatroon,xlab="Uur (GMT)",ylab="Aantal berichten sinds begin",type="l")

plot(3:length(table((table(tl2$screenName)))),table((table(tl2$screenName)))[3:length(table((table(tl2$screenName))))],xlab="aantal berichten",ylab="door aantal posters")
weekdag<-table(format(as.Date(substr(tl2$created, start=0, stop=10)),format="%w"))
plot(weekdag,type="l")

	wordlist<-NULL
	WordUsed<-NULL

for(i in 1:length(tl2$text))
{
	NewRow<-NULL
	tmp<-strsplit(RemoveTags(tl2$text[i])," ")
	tmp<-tmp[[1]][which(tmp[[1]] %in% OverSlaWoorden==FALSE)]

	listed<-wordlist %in% tmp
	AddListed<-rep(TRUE,length(setdiff(tmp,wordlist)))	
	wordlist<-c(wordlist,setdiff(tmp,wordlist))
	NewRow<-c(listed,AddListed)
	if(is.null(WordUsed))
	{
	WordUsed<-rbind(WordUsed,NewRow)
	}else{
	WordUsed<-cbind(WordUsed,matrix(FALSE,nrow(WordUsed),length(AddListed)))
	WordUsed<-rbind(WordUsed,NewRow)
	}
}
plot(sort(colSums(WordUsed[,which(colSums(WordUsed)>10)])),xlab="Woord",ylab="Aantal keer voorkomen")

w<-wordlist[which(colSums(WordUsed)>10)]
f<-colSums(WordUsed[,which(colSums(WordUsed)>10)])
wordcloud(w,f,max.words=20)
