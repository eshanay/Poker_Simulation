#a function which returns the best 5-card Hand from a total of 7 given cards
best.hand.find <- function(cards.data){
    cards.data=cards.data[which(cards.data[,ncol(cards.data)]==min(cards.data[,ncol(cards.data)])),]
    if(is.vector(cards.data)) return (c(cards.data,1,1)) #this hand has won
    else{
        win.state=0
        cards.ranks=NULL
        for(r in 1:nrow(cards.data)){
        cards.ranks=rbind(cards.ranks,as.integer(names(sort(table(cards.data[r,1:5]),decreasing=T))))
        if(any(cards.ranks[r,]==1) & !setequal(cards.ranks[r,],1:5))
            cards.ranks[r,]=replace(cards.ranks[r,],which(cards.ranks[r,]==1),14)
        if(length(cards.ranks[r,])==5) cards.ranks[r,]=sort(cards.ranks[r,],decreasing=T)
    }
    max.ranks=apply(cards.ranks,2,max)
    row.index=1:nrow(cards.ranks)
    for(c in 1:ncol(cards.ranks)){
        if(length(unique(cards.ranks[,c]))==1) next
        else {
            row.index=intersect(row.index,which(cards.ranks[,c]==max.ranks[c]))
            if(length(row.index)==1){
                win.state=1
                break
            }
            else next
        }
    }
    if(win.state==1) return (c(cards.data[row.index,],row.index,win.state))#this hand has won
    else return (c(cards.data[1,],1,win.state))#it's a tie
    }
}

#to simulate the chances of winning and tying with each hand, we first define a function
#which accepts the number of players in the game and a dataset containing repitition of the hole cards
#cards dealt to the players and the community cards dealt in the same round.
#to simulate the chances of winning and tying with each hand
Poker.game.results=function(drawn.cards,Players){
    result=matrix(numeric(1),nr=nrow(drawn.cards),nc=3*Players+2)
    for(simul in 1:nrow(result)){
        hole=matrix(drawn.cards[simul,1:(2*Players)],nr=Players,nc=2,byrow=T)
        community=drawn.cards[simul,(2*Players+1):(2*Players+5)]
        game=matrix(numeric(1),nr=Players,nc=6)
        for(player in 1:Players){
            options=t(apply(t(combn(c(hole[player,],community),5)),1,
            function(vec) c((vec-1)%%13+1,deal(vec))))
            options=options[order(options[,6]),]
            game[player,]=best.hand.find(options)[1:6]
        }
        hole=hole[order(game[,6]),]
        game=game[order(game[,6]),]
        outcome=best.hand.find(game)
        hole.cards=as.vector(t(hole))
        result[simul,]=c(as.vector(t(hole)),game[,6],outcome[7:8])
    }
    return (result)
}
max.players<-4
drawn.cards=t(replicate(1e5,sample(1:52,size=2*max.players+5)))

#for 2 players
two.players=Poker.game.results(drawn.cards,Players<-2)
better.hand=pmin(two.players[,2*Players+1],two.players[,2*Players+2])
win.prob.2=sapply(1:10,function(num) sum((better.hand==num)*(two.players[,3*Players+2]==1))*100/sum(apply(two.players[,2*Players+(1:Players)],1,function(vec) any(vec==num))))
tie.prob.2=sapply(1:10,function(num) sum((better.hand==num)*(two.players[,3*Players+2]==0))*100/sum(apply(two.players[,2*Players+(1:Players)],1,function(vec) any(vec==num))))

#for 3 players
three.players=Poker.game.results(drawn.cards,Players<-3)
better.hand=apply(three.players[,2*Players+(1:Players)],1,min)
win.prob.3=sapply(1:10,function(num) sum((better.hand==num)*(three.players[,3*Players+2]==1))*100/sum(apply(three.players[,2*Players+(1:Players)],1,function(vec) any(vec==num))))
tie.prob.3=sapply(1:10,function(num) sum((better.hand==num)*(three.players[,3*Players+2]==0))*100/sum(apply(three.players[,2*Players+(1:Players)],1,function(vec) any(vec==num))))

#for 4 players
four.players=Poker.game.results(drawn.cards,Players<-4)
better.hand=apply(four.players[,2*Players+(1:Players)],1,min)
win.prob.4=sapply(1:10,function(num) sum((better.hand==num)*(four.players[,3*Players+2]==1))*100/sum(apply(four.players[,2*Players+(1:Players)],1,function(vec) any(vec==num))))
tie.prob.4=sapply(1:10,function(num) sum((better.hand==num)*(four.players[,3*Players+2]==0))*100/sum(apply(four.players[,2*Players+(1:Players)],1,function(vec) any(vec==num))))

#bar diagram displaying the chances of having a win with each poker hand
par(mar=c(6,3,2,4),cex.main=0.9)
barplot(t(as.matrix(data.frame(unname(win.prob.2),unname(win.prob.3),unname(win.prob.4)))),
beside=T,main="Chances of winning with each poker Hand (in %)",ylab="Probability in %",
las=2,col=c("violet","green","orange"),names.arg=Hands,cex.names=0.85)
legend(42,60,title="Players",c("2","3","4"),fill = c("violet","green","orange"),xpd = T,cex=0.8)
