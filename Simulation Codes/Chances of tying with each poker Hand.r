#bar diagram displaying the chances of having a tie with each poker hand
par(mar=c(6,3,2,4),cex.main=0.9)
barplot(t(as.matrix(data.frame(unname(tie.prob.2),unname(tie.prob.3),unname(tie.prob.4)))),
beside=T,main="Chances of tying with each poker Hand (in %)",ylab="Probability in %",
las=2,col=c("violet","green","orange"),names.arg=Hands,cex.names=0.85)
legend(38,max(c(tie.prob.2,tie.prob.3,tie.prob.4))/2,title="Players",c("2","3","4"),
fill = c("violet","green","orange"),xpd = T,cex=0.8)
