cat("Descriptive Statistics of the simulated win rates(in %) for a\n")
vec1=as.vector(prob.matrix.3*diag(nrow(prob.matrix.3)))
P.Pair.win.rate=vec1[vec1>0]
vec2=as.vector(prob.matrix.3*upper.tri(prob.matrix.3))
Suit.Pair.win.rate=vec2[vec2>0]
vec3=as.vector(prob.matrix.3*lower.tri(prob.matrix.3))
Off.Suit.win.rate=vec3[vec3>0]
desc.stats=rbind(round(summary(P.Pair.win.rate)[c("Mean","Min.","Max.")],2),round(summary(Suit.Pair.win.rate)[c("Mean","Min.","Max.")],2),round(summary(Off.Suit.win.rate)[c("Mean","Min.","Max.")],2))
rownames(desc.stats)=c("Pocket Pair","Suited","Unsuited")
desc.stats
