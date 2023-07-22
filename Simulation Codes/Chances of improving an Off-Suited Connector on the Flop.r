#simulating the chances of improving on the flop from Offsuited connectors("45"-"TJ") to an Open Ended Straight Draw or a Straight
off.con.indices=which(apply(Unpaired[,1:2],1,is.Connector)==1)
OffSuited.Connector=Unpaired[off.con.indices,]
oesd.count=sum(apply(OffSuited.Connector[,1:5],1,is.OESD))
straight.count=sum(apply(OffSuited.Connector[,1:5],1,is.Straight))
freq.dist=c(oesd.count,straight.count)
names(freq.dist)=c("OESD","Straight")
cat("Probability(in %) of improving on the flop from unpaired connectors 45o-JTo to a\n")
round(freq.dist/nrow(OffSuited.Connector)*100,2)
