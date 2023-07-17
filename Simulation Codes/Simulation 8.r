#now the chances of improvng the best 5-card hand from the flop to the turn is stored in a vector
turn.improve=numeric(7)
#simulating the chances of improving on the turn from Flush draw to Flush
FD.flop=hands.data[apply(hands.data[,1:5],1,function(cards)
max(table((cards-1)%/%13+1))==4),]
Flush.count=sum(apply(FD.flop[,1:6],1,function(cards) max(table((cards-1)%/%13+1)))>4)
turn.improve[1]<-round(Flush.count/nrow(FD.flop)*100,1)

#simulating the chances of improving on the turn from OESD to Straight but since OESD is a complicated function,
#it's smarter to make it check through only those rows of simulated drawn cards, where there's a non-zero chance
#which is when the number of distint denominations in the hand is atleast 4 possible only in case of Flush, One pair and High card
subdata=hands.data[apply(hands.data[,1:5],1,function(cards)
is.element(deal(cards),c(5,9:10))),]
oesd.cards.flop=subdata[apply(subdata[,1:5],1,is.OESD)==1,]
straight.freq.oesd=sum(apply(oesd.cards.flop[,1:6],1,is.Straight))
turn.improve[2]<-round(straight.freq.oesd/nrow(oesd.cards.flop)*100,1)

#using the same logic that for a hand after the flop to be an Gutshot Draw
#it needs atleast 4 distinct cards, we simulate the chances of improving on the turn from Gutshot Straight draw to Straight
isd.cards.flop=subdata[apply(subdata[,1:5],1,is.ISD)==1,]
straight.freq.isd=sum(apply(isd.cards.flop[,1:6],1,is.Straight))
turn.improve[3]<-round(straight.freq.isd/nrow(isd.cards.flop)*100,1)

#simulating the chances of improving on the turn from a 3 of a kind to a 4 of a kind
triplet.flop=hands.data[apply(hands.data[,1:5],1,function(cards)
max(table((cards-1)%%13+1)))==3,]
quads.freq=sum(apply(triplet.flop[,1:6],1,function(cards)
I(max(table((cards-1)%%13+1))==4)))
turn.improve[4]<-round(quads.freq/nrow(triplet.flop)*100,1)

#simulating the chances of improving on the turn from two pairs to a full house
two.pairs.flop=hands.data[apply(hands.data[,1:5],1,function(cards)
sum(table((cards-1)%%13+1)==2)==2),]
full.house.freq=sum(apply(two.pairs.flop[,1:6],1,function(cards)
I(max(table((cards-1)%%13+1))==3)))
turn.improve[5]<-round(full.house.freq/nrow(two.pairs.flop)*100,1)

#simulating the chances of improving on the turn from one pair to a 3 of a kind
one.pair.flop=hands.data[apply(hands.data[,1:5],1,function(cards)
I(max(table((cards-1)%%13+1))==2 & sum(table((cards-1)%%13+1)==2)==1)),]
triplet.freq=sum(apply(one.pair.flop[,1:6],1,function(cards)
sum(table((cards-1)%%13+1)==3)==1))
turn.improve[6]<-round(triplet.freq/nrow(one.pair.flop)*100,1)

#simulating the chances of improving on the turn from unpaired hole cards to a Pair with one of the hole cards
high.card=hands.data[apply(hands.data[,1:5],1,function(cards) max(table((cards-1)%%13+1))==1),]
one.paired.hole=sum(apply(high.card[,1:6],1,function(cards){
    values=(cards-1)%%13+1
    I(any(values[6]==values[1:2]))
}))
turn.improve[7]<-round(one.paired.hole/nrow(high.card)*100,1)
