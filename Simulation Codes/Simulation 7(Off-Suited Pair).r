Unpaired=hands.data[-union(paired.indices,suited.indices),]
pair.count=function(cards){
    values=(cards-1)%%13+1
    match1=sum(values[3:5]==values[1])
    match2=sum(values[3:5]==values[2])
    if(match1==1 & match2==1) return (2)
    else if(match1+match2==1) return (1)
    else return (NA)
}
freq.dist=table(apply(Unpaired[,1:5],1,pair.count))
names(freq.dist)=Hands[9:8]
cat("Probability(in %) of improving on the flop from unpaired cards to a\n")
round(freq.dist/nrow(Unpaired)*100,2)
