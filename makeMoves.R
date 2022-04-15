library(WheresCroc)

#function used to find all the possible moves from a node
possibleMoves <- function(edges,i){
pm=c(i)
indexs= which(edges==i)
for (ind in indexs){
  if (ind<=length(edges[,1])){
    pm[length(pm)+1]=edges[ind,2]
  }else{
    pm[length(pm)+1] = edges[ind-length(edges[,1]),1]
  }
}
return(pm)
}
#breadth first search function used to find shortes path
bfs <- function(moveInfo, positions, edges){
  goal <-moveInfo$mem$goal
  start = positions[3] #start pos
  queue= c(start)      #first node to be expanded
  visited=rep(FALSE, 40)
  visited[start] = TRUE 
  parents = rep(0,40) # except start, each node in a path has a parent.
  parents[start] = -1
  while (length(queue) != 0) { #queue will become zero once all nodes visited
    currentNode = head(queue, n=1)
    queue = setdiff(queue, c(currentNode))
    neighbors = possibleMoves(edges, currentNode)
    neighbors = setdiff(neighbors, c(currentNode)) #remove current node from path
    for (node in neighbors) {
      if (!(visited[node])) {
        queue = c(queue, node)
        parents[node] = currentNode
        visited[node]=TRUE
      }
    }
    
  }
  currentNode = goal #backtrack from goal to find shortest path
  path = numeric()
  while (currentNode != -1) {
    if (parents[currentNode] != -1) {
      path = c(c(currentNode), path)
    }
    currentNode = parents[currentNode]
  }
  
  return (path) 
}


#Param in makeMoves
#moveInfo = list(moves = c(move1, move2), mem), move i want to make
#readings = c(salinity, phosphate, nitrogen)
#positions = c(pos1, pos2, pos3)  %postion of tourists. Pos is negative if just eaten. eaten before that gives NA
#edges = matrix(ncol=2)  gives edges to/from for each surrounding waterhole
#probs = list(mat1, mat2, mat3), matrix contains mean and standard deviation of readings for salinity, phosphate and nitrogen respectively at each waterhole.  
myFunction <- function(moveInfo, readings, positions, edges, probs){
  #basic implementation:
  
  #If start of new game, set initial probabilities
  if(moveInfo$mem[[1]]==0 | moveInfo$mem[[1]]==1 ){
    moveInfo$mem[[1]]=2
    moveInfo$mem$probabability =list(c(rep((1),40)))
  }
  
  #if tourist is eaten
  if (!is.na(positions[1]) & positions[1]<0){
    goal = moveInfo$mem$goal = positions[1]*-1
    crocProb = c(rep(0,40))
    crocProb[positions[1]*-1]=1
    moveInfo$mem$probabability[[length(moveInfo$mem$probabability)+1]] <-crocProb
  } else  if (!is.na(positions[2]) & positions[2]<0){
    goal =moveInfo$mem$goal = positions[2]*-1
    crocProb = c(rep(0,40))
    crocProb[positions[2]*-1]=1
    moveInfo$mem$probabability[[length(moveInfo$mem$probabability)+1]] <-crocProb
  }
    else{

  crocProb = c(1:length(probs$salinity[,1]))
  #set probabilities for each hole based on distributions(probs), previous probabilities, and readings
  for (i in 1:length(probs$salinity[,1])){
    #see Markov Models slides page 15, the equation can be somewhat simplified since movement probs of crocs are equally ditributed
    probSal = dnorm(readings[1], mean=probs$salinity[i,1], sd =probs$salinity[i,2])
    probPhos = dnorm(readings[2], mean=probs$phosphate[i,1], sd =probs$phosphate[i,2])
    probNit = dnorm(readings[3], mean=probs$nitrogen[i,1], sd =probs$nitrogen[i,2])
    obsProb = probSal*probNit*probPhos #emission
    posMoves = possibleMoves(edges,i)
    s0Probs= moveInfo$mem$probabability[[length(moveInfo$mem$probabability)]][c(posMoves)]
    crocProb[i] = (1/length(posMoves))*sum(s0Probs)*obsProb
  }
  #croc is not where uneatn tourist is
  crocProb[positions[1]]=0
  crocProb[positions[2]]=0
  
  goal = moveInfo$mem$goal <-which.max(crocProb)
  
  }
  
  guardPos = positions[3]
  posMoves= possibleMoves(edges, guardPos)
  #if goal not in neghbors or standing on goal
  if(!(goal %in% posMoves)){
    foundPath=FALSE
    pathList=list(list())
    pathList=pathList[-1]
    for (move in posMoves){
      pathList[[length(pathList)+1]] <- list(move)
    }
  }else if(goal==guardPos){ #if standing on goal
    move1=0
    move2=posMoves[2]
    crocProb[goal] = 0
    foundPath=TRUE
  }else{#if goal in neigbors
    move1=goal
    move2 = 0
    crocProb[goal] = 0
    foundPath=TRUE
  }
  if (!foundPath){
  bestPath = bfs(moveInfo, positions,edges)
  move1=bestPath[1]
  move2=bestPath[2]
 # moveInfo$mem$routes[[length(moveInfo$mem$routes)+1]]  <- c(bestPath) #not used
  }
  moveInfo$mem$probabability[[length(moveInfo$mem$probabability)+1]] <-crocProb
  moveInfo$moves=c(move1,move2)
  return(moveInfo) 
}
runWheresCroc(myFunction)
testWC(myFunction, verbose=1)
