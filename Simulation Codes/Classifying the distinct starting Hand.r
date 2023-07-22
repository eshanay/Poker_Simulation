#utilizing the dataset of drawn cards from Simulation Codes/Chances of occurrence of the poker Hands from 7 cards.r
starting=hands.data[,1:2]
is.Pocket.Pair=function(starting) I(length(unique((starting-1)%%13+1))==1)
is.Suited=function(starting) I(length(unique((starting-1)%/%13+1))==1)
