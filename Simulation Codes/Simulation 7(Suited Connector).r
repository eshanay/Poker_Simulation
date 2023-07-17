#function to check if the starting the cards are connectors in the range "45" to "TJ"
is.Connector=function(cards){
    sorted.cards=sort((cards-1)%%13+1)
    I(sorted.cards[1]>=4 & sorted.cards[1]<=10)*I(diff(sorted.cards)==1)
}
#function to check if the set of cards is a flush draw
is.Flush.Draw=function(cards) I(max(table((cards-1)%/%13+1))==4)

#function to check if any 5 out of the set of cards form a Straight
is.Straight=function(cards){
    values=unique((cards-1)%%13+1)
    if(length(values)==5) return (I(all(diff(sort(values))==1) | setequal(sort(values),c(1,10:13))))
    else if(length(values)>5){
        stacking=t(combn(values,5))
        flag=0
        for(r in 1:nrow(stacking))
        if(is.Straight(stacking[r,])==1) {
            flag=1
            break
        }
        return (flag)
    }
    else return (0)
}

#function to check if the hand of cards form an Open-Ended Straight Draw
is.OESD=function(cards){
    if(is.Straight(cards)==0){
        denoms=sort(unique((cards-1)%%13+1))
        if(length(denoms)==4) return (all(diff(denoms)==1))
        else if(length(denoms)>4) {
            quadro=t(combn(denoms,4))
            flag=0
            for(ro in 1:nrow(quadro))
                if(is.OESD(quadro[ro,])==1){
                    flag=1
                    break
                }
            return (flag)
        }
        else return (0)
    }
    else return (0)
}

#function to check if the set of cards form a Gutshot Straight draw
is.ISD=function(cards){
    if(is.Straight(cards)==0){
        denoms=unique((cards-1)%%13+1)
        if(length(denoms)>=4)
        {
            quadro=t(combn(denoms,4))
            flag=0
            for(ro in 1:nrow(quadro)){
                if(setequal(sort(quadro[ro,]),1:4) | setequal(sort(quadro[ro,]),c(1,11:13))){
                    flag=1
                    break
                }
                if(all(is.element(c(1,10),quadro[ro,])) & sum(is.element(11:13,quadro[ro,]))>=1)
                    quadro[ro,]=replace(quadro[ro,],which(quadro[ro,]==1),14)
                inside.miss=setdiff(min(quadro[ro,]):max(quadro[ro,]),quadro[ro,])
                if(length(inside.miss)==1 & all(diff(sort(c(quadro[ro,],inside.miss)))==1)){
                    flag=1
                    break
                }
            }
            return (flag)
        }
        else return (0)
    }
    else return (0)
}

#function to check if the hand of cards forms a Straight Draw
is.Straight.Draw=function(cards) is.OESD(cards)+is.ISD(cards)>=1

#simulating the chances of improving on the flop from Suited Connectors("45"-"TJ") to a Straight Draw and a Flush Draw
suit.con.indices=which(apply(Suited.Pair[,1:2],1,is.Connector)==1)
Suited.Connector=Suited.Pair[apply(Suited.Pair[,1:2],1,is.Connector)==1,]
Fl.Draw<-round(sum(apply(Suited.Connector[,1:5],1,function(cards)
is.Flush.Draw(cards)))/nrow(Suited.Connector)*100,2)
St.Draw<-round(sum(apply(Suited.Connector[,1:5],1,function(cards)
is.Straight.Draw(cards)))/nrow(Suited.Connector)*100,2)
prob.dist<-c(Fl.Draw,St.Draw)
names(prob.dist)<-c("Flush Draw","Straight Draw")
cat("Probability(in %) of improving on the flop from same-suited connectors in the range 45-JT to a\n")
prob.dist
