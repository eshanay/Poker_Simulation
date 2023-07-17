#simulating the pre-Flop win rates for each starting Hand in a game of 4 players
Players<-4
match.details=NULL
for(player in 1:Players){
    Starting.cards=apply(four.players[,(2*player-1):(2*player)],1,pair.classifier)
    match.details=cbind(match.details,Starting.cards)
}
match.details=cbind(match.details,four.players[,3*Players+(1:2)])
prob.matrix.4=freq.matrix.4=matrix(numeric(1),nr=13,nc=13)
for(j in 1:13)
    for(i in 1:13)
        for(r in 1:nrow(match.details))
            if(any(pair.matrix[i,j]==match.details[r,1:2])){
            freq.matrix.4[i,j]=freq.matrix.4[i,j]+1
            prob.matrix.4[i,j]=prob.matrix.4[i,j]+four.players[r,3*Players+2]*
            I(pair.matrix[i,j]==match.details[r,four.players[r,3*Players+1]])
        }
prob.matrix.4=prob.matrix.4/freq.matrix.4*100
heat.map(round(prob.matrix.4,1),Players)
#Spearman's Correlation Coefficient of the winning rates(in %) of distinct starting Hands under 2 and 4 players
cor(as.vector(prob.matrix.2),as.vector(prob.matrix.4),method="spearman")
#Spearman's Correlation Coefficient of the winning rates(in %) of distinct starting Hands under 3 and 4 players
cor(as.vector(prob.matrix.3),as.vector(prob.matrix.4),method="spearman")
