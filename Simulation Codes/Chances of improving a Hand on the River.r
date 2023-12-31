#The chances of improvement of best 5-card hand possible from Turn to River stage are stored in a vector
river.improve=numeric(7)

#simulating the chances of improving on the river from Flush draw to Flush
FD=hands.data[apply(hands.data[,1:6],1,function(cards) max(table((cards-1)%/%13+1))==4),]
Flush.count=sum(apply(FD,1,function(cards) max(table((cards-1)%/%13+1)))>4)
river.improve[1]<-round(Flush.count/nrow(FD)*100,1)

#simulating the chances of improving on the river from OESD to Straight but this time unlike the previous one,
#subdata cannot be used because the function deal is defined to work with 5 cards as arguments so I had to make
#is.OESD function go through the entore simulated 7-card draw dataset
oesd.cards=hands.data[apply(hands.data[,1:6],1,is.OESD)==1,]
straight.freq.oesd=sum(apply(oesd.cards,1,is.Straight))
river.improve[2]<-round(straight.freq.oesd/nrow(oesd.cards)*100,1)

#simulating the chances of improving on the river from Gutshot Draw to Straight
isd.cards=hands.data[apply(hands.data[,1:6],1,is.ISD)==1,]
straight.freq.isd=sum(apply(isd.cards,1,is.Straight))
river.improve[3]<-round(straight.freq.isd/nrow(isd.cards)*100,1)

#simulating the chances of improving on the river from a 3 of a kind to a 4 of a kind
triplet=hands.data[apply(hands.data[,1:6],1,function(cards) max(table((cards-1)%%13+1)))==3,]
quads.freq=sum(apply(triplet,1,function(cards) I(max(table((cards-1)%%13+1))==4)))
river.improve[4]<-round(quads.freq/nrow(triplet)*100,1)

#simulating the chances of improving on the river from two pairs to a full house
two.pairs=hands.data[apply(hands.data[,1:6],1,function(cards) sum(table((cards-1)%%13+1)==2)==2),]
full.house.freq=sum(apply(two.pairs,1,function(cards) max(table((cards-1)%%13+1))==3))
river.improve[5]<-round(full.house.freq/nrow(two.pairs)*100,1)

#simulating the chances of improving on the river from one pair to a 3 of a kind
one.pair=hands.data[apply(hands.data[,1:6],1,function(cards) I(max(table((cards-1)%%13+1))==2 & sum(table((cards-1)%%13+1)==2)==1)),]
triplet.freq=sum(apply(one.pair,1,function(cards) sum(table((cards-1)%%13+1)==3)==1))
river.improve[6]<-round(triplet.freq/nrow(one.pair)*100,1)

#simulating the chances of improving on the river from unpaired hole cards to a Pair with one of the hole cards
high.card=hands.data[apply(hands.data[,1:6],1,function(cards) max(table((cards-1)%%13+1))==1),]
one.paired.hole=sum(apply(high.card,1,function(cards){
    values=(cards-1)%%13+1
    I(any(values[7]==values[1:2]))
}))
river.improve[7]<-round(one.paired.hole/nrow(high.card)*100,1)

#for simultaneous comparison of these chances of improvement from the Flop-to-Turn and from the Turn-to-River we display it in a matrix
improvement=cbind("on Turn"=paste(turn.improve,"%",sep=""),"on River"=paste(river.improve,"%",sep=""))
your.hand=c("Flush draw","OESD","Gutshot Straight Draw","3 of a kind","2 pairs","1 pair","Unpaired cards")
improving.to=c("Flush","Straight","Straight","4 of a kind","Full house","3 of a kind","Pair(with hole card)")
cbind("Player's hand"=your.hand,"Improving to a"=improving.to,improvement)
