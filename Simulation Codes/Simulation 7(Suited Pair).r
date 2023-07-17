suited.indices=which(apply(starting,1,is.Suited)==1)
Suited.Pair=hands.data[suited.indices,]
flush.variations=function(cards){
    suits=(cards-1)%/%13+1
    suits.freq=sort(unname(table(suits)),decreasing=T)
    if(suits.freq[1]==5) return (1) #having all 5 cards of the same suit
    else if(suits.freq[1]==4) return (2) #having maximum 4 cards of the same suit
    else if(sum(suits[3:5]==suits[1])==1) return (3) #having maximum 3 cards of the same suit
    else return (NA)
}
freq.dist=table(apply(Suited.Pair[,1:5],1,flush.variations))
names(freq.dist)=c("Flush","Flush draw","Backdoor Flush draw")
cat("Probability(in %) of improving on the flop from Suited Pair to a\n")
round(freq.dist/length(suited.indices)*100,2)
