paired.indices=which(apply(starting,1,is.Pocket.Pair)==1)
Poc.Pair=hands.data[paired.indices,]
triplet.count<-sum(apply(Poc.Pair[,1:5],1,function(cards) {
    ranks=(cards-1)%%13+1
    sum(ranks[3:5]==ranks[1])==1
}))
fullhouse.count<-sum(apply(Poc.Pair[,1:5],1,function(cards){
    ranks=(cards-1)%%13+1
    I(setequal(unname(table(ranks[1:5])),c(3,2)) & sum(ranks[3:5]==ranks[1])==1)
}))
quadruplet.count<-sum(apply(Poc.Pair[,1:5],1,function(cards) max(table((cards-1)%%13+1))==4))
freq.dist<-c(triplet.count,fullhouse.count,quadruplet.count)
names(freq.dist)=Hands[c(7,4,3)]
cat("Probability(in %) of improving on the flop from Pocket Pair to a\n")
round(freq.dist/nrow(Poc.Pair)*100,2)
