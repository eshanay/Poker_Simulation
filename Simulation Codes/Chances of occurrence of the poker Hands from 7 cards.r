Hands<-c("Royal Flush","Straight Flush","4 of a kind","Full House","Flush","Straight","3 of a kind","2 pairs","1 pair","High card")
Suits<- c("c","d","h","s")
Faces<-c("A","2","3","4","5","6","7","8","9","T","J","Q","K")

#the function below gives the Hand to which the 5-card combination input belongs
deal <- function(cards){
    suits=(cards-1)%/%13+1
    values=sort((cards-1)%%13+1)
    if(length(unique(suits))==1){ #if all 5 cards are of same suit
        if(setequal(values,c(1,10:13))) return (1) #RF
        else if(all(diff(values)==1)) return (2) #SF
        else return (5) #Flush
    }
    else{
        freq=sort(unname(table(values)))
        if(setequal(freq,c(1,4))) return (3) #4 of a kind
        else if(setequal(freq,c(2,3))) return (4) #Full House
        else if(all(diff(values)==1) | setequal(values,c(1,10:13))) return (6) #Straight
        else if(setequal(freq,c(1,1,3))) return (7) #3 of a kind
        else if(length(freq)==3 & setequal(freq,c(1,2,2))) return (8) #2 Pairs
        else if(length(freq)==4 & setequal(freq,c(1,1,1,2))) return (9) #1 Pair
        else return (10) # No Pair
    }
}

#now we repeat 5-card draws from a deck of 52 cards 1 million times
hands.data=t(replicate(1e6,sample(1:52,size=7)))
data=apply(hands.data,1,function(cards) min(apply(combn(cards,5),2,deal)))

#obtain the probability distribution of these Hands
probab=round(table(data)/length(data)*100,4)
names(probab)=Hands[as.integer(names(probab))]
cat("Probabilities(in %) of making each poker Hand at showdown\n")
probab
#obtaining the cumulative probability distribution of these Hands
cat("Probability(in %) of making a Hand as good as\n")           
cumsum(rev(probab))
