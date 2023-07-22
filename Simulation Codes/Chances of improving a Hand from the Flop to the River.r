#Chances of improvement of Hands from the flop stage to the river stage are stored in a vector
F.to.R.improve<-numeric(8)

#a flush draw can improve to a flush
FD=hands.data[apply(hands.data[,1:5],1,function(cards) max(table((cards-1)%/%13+1)))==4,]
Flush.count.FD=sum(apply(FD,1,function(cards) max(table((cards-1)%/%13+1)))>4)
F.to.R.improve[1]<-round(Flush.count.FD/nrow(FD)*100,1)

#a backdoor flush draw can improve to a flush
BFD=hands.data[apply(hands.data[,1:5],1,function(cards) max(table((cards-1)%/%13+1)))==3,]
Flush.count.BFD=sum(apply(BFD,1,function(cards) max(table((cards-1)%/%13+1)))>4)
F.to.R.improve[2]<-round(Flush.count.BFD/nrow(BFD)*100,1)

#an oesd can improve to a straight from the flop to the river
straight.freq.oesd=sum(apply(oesd.cards.flop,1,is.Straight))
F.to.R.improve[3]<-round(straight.freq.oesd/nrow(oesd.cards.flop)*100,1)

#a gutshot straight draw can improve to a straight from the flop to the river
straight.freq.isd=sum(apply(isd.cards.flop,1,is.Straight))
F.to.R.improve[4]<-round(straight.freq.isd/nrow(isd.cards.flop)*100,1)

#a 3 of a kind can improve to a four of a kind from the flop to the river
quads.freq=sum(apply(triplet.flop,1,function(cards) I(max(table((cards-1)%%13+1))==4)))
F.to.R.improve[5]<-round(quads.freq/nrow(triplet.flop)*100,1)

#two pairs can improve to a full house from the flop to the river
full.house.freq=sum(apply(two.pairs.flop,1,function(cards) I(max(table((cards-1)%%13+1))==3)))
F.to.R.improve[6]<-round(full.house.freq/nrow(two.pairs.flop)*100,1)

#one pair can improve to a 4 of a kind from the flop to the river
one.pair.flop=hands.data[apply(hands.data[,1:5],1,function(cards) I(max(table((cards-1)%%13+1))==2 & sum(table((cards-1)%%13+1)==2)==1)),]
quadruplet.freq=sum(apply(one.pair.flop,1,function(cards) max(table((cards-1)%%13+1))==4))
F.to.R.improve[7]<-round(quadruplet.freq/nrow(one.pair.flop)*100,3)

#one pair can improve to a 3 of a kind from the flop to the river
triplet.freq=sum(apply(one.pair.flop,1,function(cards){
    val=(cards-1)%%13+1
    paired.value=as.integer(names(which(table(val[1:5])==2)))
    return (I(sum(is.element(paired.value,val[6:7]))==1))
}))
F.to.R.improve[8]<-round(triplet.freq/nrow(one.pair.flop)*100,3)

#atlast I displayed these chances in a tabular format
improvement=paste(F.to.R.improve,"%",sep="")
your.hand=c("Flush draw","Backdoor Flush draw","OESD","Gutshot Straight Draw","3 of a kind","2 pairs","1 pair","1 pair")
improving.to=c("Flush","Flush","Straight","Straight","4 of a kind","Full house","4 of a kind","3 of a kind")
cbind("Player's hand on Flop"=your.hand,"Improvement on River"=improving.to,"Probability"=improvement)
