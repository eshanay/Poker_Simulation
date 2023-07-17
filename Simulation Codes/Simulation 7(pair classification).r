#utilizing the dataset of drawn cards from the Simulation 4.r
starting=hands.data[,1:2]
is.Pocket.Pair=function(starting) I(length(unique((starting-1)%%13+1))==1)
is.Suited=function(starting) I(length(unique((starting-1)%/%13+1))==1)
