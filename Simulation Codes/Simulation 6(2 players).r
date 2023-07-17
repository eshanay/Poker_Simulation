face.order=c("A","K","Q","J","T","9","8","7","6","5","4","3","2")
#this is the actual order of card rankings followed in poker

#we define a function which identifies the starting pair into a pocket pair,
#suited pairs or off-suited pairs and also the denominations following face.order
pair.classifier=function(card.pair){
    face.value=intersect(face.order,Faces[(card.pair-1)%%13+1])
    suit.status=c("s","o")
    if(length(face.value)==1) return (paste(face.value,face.value,sep=""))
    else return (paste(face.value[1],face.value[2],
    suit.status[length(unique((card.pair-1)%/%13+1))],sep=""))
}

#creating a matrix of string elements of total 169 starting Hands following some arrangement
pair.matrix=matrix(numeric(1),nr=13,nc=13)
for(j in 1:13)
    for(i in 1:13) {
        if(i==j) pair.matrix[i,j]=paste(face.order[i],face.order[j],sep="")
        else if(i<j) pair.matrix[i,j]=paste(face.order[i],face.order[j],"s",sep="")
        else pair.matrix[i,j]=paste(face.order[j],face.order[i],"o",sep="")
    }

#a function to draw the heat map of the win rates of the starting Hands for n players
heat.map=function(prob.matrix,n){
    u=seq(0,1,length=13)
    par(mar=c(4,3,4,3))
    image(prob.matrix,col=heat.colors(256),yaxt="n",xaxt="n")
    for(i in 1:13)
    for(j in 1:13)
    text(u[i],u[j],labels=prob.matrix[i,j],adj=c(0.5,0.5),cex=0.7,col="blue")
    mtext("Unsuited",side=1,line=2)
    axis(side=1,at=u,padj=-1,labels=face.order,tick=0,cex.axis=0.8)
    mtext("Suited",side=2,line=2)
    axis(side=2,at=u,padj=0,las=2,hadj=0,labels=face.order,tick=0,cex.axis=0.8)
    mtext("Suited",side=3,line=2)
    axis(side=3,at=u,padj=1,labels=face.order,tick=0,cex.axis=0.8)
    mtext("Unsuited",side=4,line=2)
    axis(side=4,at=u,padj=0,las=2,hadj=0,labels=face.order,tick=0,cex.axis=0.8)
    mtext(paste("pre-Flop win rate in % for",n,"players"),side=3,line=3)
}

#simulating the pre-Flop win rates for each starting Hand in a game of 2 players
Players<-2
match.details=NULL
for(player in 1:Players){
    Starting.cards=apply(two.players[,(2*player-1):(2*player)],1,pair.classifier)
    match.details=cbind(match.details,Starting.cards)
}
match.details=cbind(match.details,two.players[,3*Players+(1:2)])
prob.matrix.2=freq.matrix.2=matrix(numeric(1),nr=13,nc=13)
for(j in 1:13)
    for(i in 1:13)
        for(r in 1:nrow(match.details))
            if(any(pair.matrix[i,j]==match.details[r,1:2])){
                freq.matrix.2[i,j]=freq.matrix.2[i,j]+1
                prob.matrix.2[i,j]=prob.matrix.2[i,j]+two.players[r,3*Players+2]*
                I(pair.matrix[i,j]==match.details[r,two.players[r,3*Players+1]])
            }
prob.matrix.2=prob.matrix.2/freq.matrix.2*100
heat.map(round(prob.matrix.2,1),Players)
sum(prob.matrix.2>50)
