node_shapes <- function(trans,tradeguys,bbrefid){

library(DiagrammeR)


  freeagentsigning <- c("F ","Fa","Fb","Fc","Fo","Fv") #does not include free agent granted
  trades <- c("T ","Tp")
  drafted <- c("Da","Df")
  r5drafted <- c("D ","Dm")
  exitedbaseball <- c("Hv","Z ")
  released <- c("R ","Fg")
  # Initialize our edges (the arrows) - goes from 1-2, 2-3, etc
  if (length(trans$TRANS_ID)==1){
    fromedges=1
    toedges=1
  }  else {
    fromedges=seq(1,length(trans$TRANS_ID)-1)
    toedges = seq(2,length(trans$TRANS_ID))
  }
  # remove transactions we don't care about - Fg is released by team
  #trans <- subset(trans, TYPE != "Fg")
  trans <- subset(trans, TYPE != "Dn") #drafted but did not sign
  
  # Initialize the node vectors
  nodevector = c(1:nrow(trans))
  shapevector = vector(mode="character",nrow(trans))
  colorvector = vector(mode="character",nrow(trans))
  labelvector = vector(mode="character",nrow(trans))
  edgecolorvector=vector(mode="character",nrow(trans))
  arrowsizevector=vector(mode="character",nrow(trans))
  # Loop over all transactions for a player

  for (i in nodevector) {
    #Define the "arrow color"
    edgecolorvector[i]="black"
    arrowsizevector[i]=2
    # Player signs as a free agent
    if (trans$TYPE[i] %in% freeagentsigning) {
      shapevector[i] = "rect"
      colorvector[i] = "orange"
      labelvector[i] = paste("Signed by ",trans$TTEAM[i]," on ",trans[[1]][i])
    }
    # Player is traded
    else if (trans$TYPE[i] %in% trades) {
      shapevector[i]="rarrow"
      colorvector[i] = "yellow"
      labelvector[i] = paste("Traded to ",trans$TTEAM[i]," on ",trans[[1]][i])
      #Get the subset for the current transaction we care about
      currenttrade <- subset(tradeguys, TRANS_ID == trans$TRANS_ID[i])
      #Get the player we care about in the trade
      currentplayer <- subset(currenttrade, bbrefid == currenttrade$BBREFID)
      #Get the other dudes in the trade
      otherplayers <- subset(currenttrade, bbrefid != currenttrade$BBREFID)
      #Get the teams the player we care about is traded to and from
      to_team <- currentplayer$TTEAM
      from_team <- currentplayer$FROMT
      #Loop over other players
      for(j in 1:nrow(otherplayers)) {
      #  row <- dataFrame[j,]
      #  print(otherplayers[j,])
        
      #Create a new node for the player
        nodevector = append(nodevector,j+otherplayers$TRANS_ID[j])
        shapevector = append(shapevector,"rect")
        colorvector = append(colorvector,"white")
        labelvector = append(labelvector,otherplayers$BBREFID[j])
      #If other player is going along with our guy to the new team create arrow
        if (otherplayers$TTEAM[j]==to_team) {
          fromedges = append(fromedges,j+otherplayers$TRANS_ID[j])
          toedges = append(toedges,  i)
          arrowsizevector=append(arrowsizevector,.5)
          edgecolorvector=append(edgecolorvector,"gray")
        }  
        # For players traded for our guy create the reverse arrow
        else {
          fromedges = append(fromedges,j+otherplayers$TRANS_ID[j])
          toedges = append(toedges,i-1)
          fromedges = append(fromedges,i)
          toedges = append(toedges,j+otherplayers$TRANS_ID[j])
          arrowsizevector=append(arrowsizevector,.5)
          edgecolorvector=append(edgecolorvector,"gray")
          arrowsizevector=append(arrowsizevector,.5)
          edgecolorvector=append(edgecolorvector,"gray")
        }

      #End loop of traded players
      }
    }
    #Player is drafted
    else if (trans$TYPE[i] %in% drafted) {
      shapevector[i] = "cds"
      colorvector[i] = "green"
      labelvector[i] = paste("Drafted by ",trans$TTEAM[i]," on ",trans[[1]][i])
    }
    #Player is Rule 5 drafted
    else if (trans$TYPE[i] %in% r5drafted){
      shapevector[i] = "rarrow"
      colorvector[i] = "green"
      labelvector[i] = paste("Drafted by ",trans$TTEAM[i]," in the Rule 5 Draft on ",trans[[1]][i])
    }
    #Player retires
    else if(trans$TYPE[i] %in% exitedbaseball){
      shapevector[i] = "component"
      colorvector[i] = "red"
      labelvector[i] = paste("Retired on ",trans[[1]][i])
    }
    #Player is released
    else if(trans$TYPE[i] %in% released){
      shapevector[i] = "rectangle"
      colorvector[i] = "red"
      labelvector[i] = paste("Released on ",trans[[1]][i])
      #Here we need to make the arrow the same color as the background (just use white for now..will need to edit if we set background to be new color)
      edgecolorvector[i]="white"
    }
    #The "debug" option..if what happens to the player doesn't fit into our previous categories
    else{
      shapevector[i] = "circle"
      colorvector[i] = "grey"
      labelvector[i] = paste("reason is: ",trans$TYPE[i])
    }
    #End of for transaction for loop
  }

  # Create a node data frame
  nodes <-
    create_nodes(
      nodes = nodevector,
      label = labelvector,
      type = "lower",
      style = "filled",
      color = colorvector,
      shape = shapevector
    )
  #Create an edge data frame
  
  edges <-
    create_edges(
      from = fromedges,
      to = toedges,
      rel = "leading_to",
      color = edgecolorvector,
      arrowsize=arrowsizevector
    )
  #Create the graph
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges,
      node_attrs = "fontname = Helvetica"
#       edge_attrs = c("color = black",
#                      "arrowsize = 2")
    )
  #Plot the graph
  render_graph(graph)
#   dev.copy(png,'./myplot.png')
#   dev.off()
}