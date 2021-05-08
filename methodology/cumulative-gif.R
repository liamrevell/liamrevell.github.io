library(covid19.Explorer)
data(Data)

states<-c("Alabama","Arizona","Arkansas","California","Colorado",
	"Connecticut","Delaware","Florida","Georgia","Idaho","Illinois",
	"Indiana","Iowa","Kansas","Kentucky","Louisiana",
	"Maine","Maryland","Massachusetts","Michigan","Minnesota",
	"Mississippi","Missouri","Montana","Nebraska","Nevada",
	"New Hampshire","New Jersey","New Mexico","New York (excluding NYC)",
	"New York City","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
	"Pennsylvania","Rhode Island","South Carolina","South Dakota",
	"Tennessee","Texas","Utah","Vermont","Virginia",
	"Washington","West Virginia","Wisconsin","Wyoming")
obj<-sapply(states,infection.estimator,data=Data,
	ifr=c(0.015,0.01,0.007,0.006,0.006,0.006,0.006),
	plot=FALSE,cumulative=TRUE)
ii<-grep("New York",colnames(obj))
dates<-as.Date(rownames(obj))
obj<-cbind(obj[,-ii],rowSums(obj[,ii]))
colnames(obj)[ncol(obj)]<-"New York"
pop<-as.matrix(state.deaths(data=Data,plot="States"))[,"2020"]
pop["New York"]<-pop["New York"]+pop["New York City"]
pop<-matrix(rep(pop[colnames(obj)],nrow(obj)),nrow(obj),ncol(obj),
	byrow=TRUE,dimnames=dimnames(obj))
daily<-obj/pop*100
cols<-rgb(colorRamp(c("blue","red"))(seq(0,1,length.out=100)),
	maxColorValue=255)
nticks<-10

png(file="covid19-%03d.png",width=10,height=6,units="in",res=125)

for(i in c(32:nrow(obj),rep(nrow(obj),30))){
	infections<-daily[i,]
	colors=setNames(
		rgb(colorRamp(c("blue","red"))(infections/(max(infections)+0.1)),
		maxColorValue=255),
		names(infections))
	dev.hold()
	par(mar=c(0.1,0.1,0.1,0.1))
	plot(NA,xlim=c(-125,-60),ylim=c(24,50),asp=1.3,
		xlab="",ylab="",
		axes=FALSE)
	for(j in 1:length(colors))
		maps::map("state",regions=names(colors)[j],
			fill=TRUE,add=TRUE,
			col=colors[j],border="white")
	LWD<-diff(par()$usr[1:2])/dev.size("px")[1]
	Y<-cbind(seq(24,50,length.out=nticks),
		seq(24,50,length.out=nticks))
	X<-cbind(rep(-63+LWD*10/2,nticks),
		rep(-63+LWD*10/2+0.5,nticks))
	for(k in 1:nrow(Y)) lines(X[k,],Y[k,])
	phytools::add.color.bar(50-24,cols,title="",lims=NULL,
		digits=2,direction="upwards",subtitle="",lwd=15,
		x=-63,y=24,
		prompt=FALSE)
	text(x=-65,y=37,"cumulative % SARS-CoV-2 infected",srt=90)
	for(k in 1:nticks){
		text(x=X[k,2],y=Y[k,2],
			paste(sprintf(if((max(infections)+0.1)>10) 
			"%1.1f" else "%1.2f",
			seq(0,max(infections)+0.1,
			length.out=nticks))[k],
			if(k==nticks) "%" else "",sep=""),
			pos=4,cex=if(k==nticks) 1.2 else 0.7)
	}
	text(x=-118,y=25,
		rownames(obj)[i],
		font.main=3,cex=1.5)
	dev.flush()
}


dev.off()

system("magick convert -delay 10 -loop 0 *.png cumulative-anim.gif")

file.remove(list.files(pattern=".png"))

