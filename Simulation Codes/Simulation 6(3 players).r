#simulating the pre-Flop win rates for each starting Hand in a game of 3 players
Players<-3
match.details=NULL
for(player in 1:Players){
    Starting.cards=apply(three.players[,(2*player-1):(2*player)],1,pair.classifier)
    match.details=cbind(match.details,Starting.cards)
}
match.details=cbind(match.details,three.players[,3*Players+(1:2)])
prob.matrix.3=freq.matrix.3=matrix(numeric(1),nr=13,nc=13)
for(j in 1:13)
    for(i in 1:13)
        for(r in 1:nrow(match.details))
            if(any(pair.matrix[i,j]==match.details[r,1:2])){
                freq.matrix.3[i,j]=freq.matrix.3[i,j]+1
                prob.matrix.3[i,j]=prob.matrix.3[i,j]+three.players[r,3*Players+2]*
                I(pair.matrix[i,j]==match.details[r,three.players[r,3*Players+1]])
            }
prob.matrix.3=prob.matrix.3/freq.matrix.3*100
heat.map(round(prob.matrix.3,1),Players)
#Spearman's Correlation Coefficient of the winning rates(in %) of distinct starting Hands under 2 and 3 players
cor(as.vector(prob.matrix.2),as.vector(prob.matrix.3),method="spearman")
