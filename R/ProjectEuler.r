##
##  PROJECT EULER
##

library(stringr)
library(gmp)
library(gregmisc)

sieve <- function(n){
  n <- as.integer(n)
  if(n > 1e6) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  for(i in last.prime:floor(sqrt(n)))
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    last.prime <- last.prime + min(which(primes[(last.prime+1):n]))
  }
  which(primes)
}
pellSequence <- function(n=2, x1=NULL, y1=NULL, limit=1e6){
  
  if(is.null(x1) && is.null(y1)){
    x1 <- 1
    while(x1 < 1e6){
      x1 <- x1 + 1
      y1 <- sqrt((x1^2 - 1) / n)
      if(round(y1) == y1) break
    }
    if(round(y1) != y1) stop("Couldn't calculate y1.")
  }
  
  if(x1 ^ 2 - n * y1 ^ 2 != 1) stop("x1, y1, and n are invalid.")
  
  x <- c(x1)
  y <- c(y1)
  
  while(x[length(x)] < limit){
    x <- c(x, x1 * x[length(x)] + n * y1 * y[length(y)])
    y <- c(y, x1 * y[length(y)] + y1 * x[length(x) - 1])
  }
  return(list(x=x, y=y))
}

problem30 <- function(){
  nums <- 10:354294
  ndxMatch <- sapply(nums,function(xxx){
    dgt <- as.numeric(unlist(strsplit(as.character(xxx), split="")))
    return(sum(dgt ^ 5)==xxx)
  })
  sum(nums[ndxMatch])
}

problem34 <- function(){
  digits <- data.frame(digit=0:9, factorial=factorial(0:9))
  allNums <- 10:2540160
  
  ndxMatch <- sapply(allNums, function(num){   
    dgt <- as.numeric(unlist(strsplit(as.character(num), split="")))
    sumFactorial <- sum(digits[match(dgt, digits[, "digit"]), "factorial"])
    return(sumFactorial==num)
  })
  sum(allNums[ndxMatch])
}

problem35 <- function(){
  
  badDigits <- as.character(c(0,2,4,5,6,8))
  
  ndx <- sapply(bunchOfPrimes[bunchOfPrimes > 100], function(num){
    
    dgts <- unlist(strsplit(as.character(num), split=""))
    if(sum(is.element(dgts, badDigits)) > 0) return(FALSE)
    
    for(nnn in 2:length(dgts)){
      temp <- paste(paste(dgts[nnn:(length(dgts))], collapse=""), 
        paste(dgts[1:(nnn-1)], collapse=""), sep="")
      if(!is.element(as.numeric(temp), bunchOfPrimes)) return(FALSE)
    }
    
    return(TRUE)
  })
  print(sum(ndx) + 13)
}

problem59 <- function(){
  xorCrypt <- function(x, y){
    return(as.integer(xor(as.raw(x), as.raw(y))))
  }
  xorEncryptAll <- function(input, key){
    
    encryption <- vapply(1:length(input), function(nnn){
      xorCrypt(input[nnn], key[(nnn %% length(key)) + 1])
    }, 0)
    
  }
  crackKey <- function(text, keyLength = 3){
    
    totalChars <- length(text)
    minChar <- min(text)
    if(minChar < 1) text <- text + abs(minChar) + 1
    maxChar <- max(text)
    
    charsFreq <- matrix(0, nrow=maxChar, ncol=keyLength)
    for(i in 1:totalChars){
      j <- i %% keyLength + 1
      charsFreq[text[i], j] <- charsFreq[text[i], j] + 1
    }
    key <- apply(charsFreq, 2, function(ccc){
      return(which(ccc==max(ccc)))
    })
    
    #most common character will be a space
    spaceAscii <- 32L
    key <- xorCrypt(as.integer(key), spaceAscii)
    
    if(minChar < 1) key <- key - (abs(minChar) + 1)
    
    return(key)
  }
  
  
  text <- as.vector(unlist(read.delim2("http://projecteuler.net/project/cipher1.txt", 
    sep=",", header=FALSE)))
  
  totalChars <- length(text)
  freq <- ddply(data.frame(Char=text), "Char", summarize, Count=length(Char))
  freq <- freq[order(freq$Count, decreasing=TRUE), ]
  freq$Proportion <- freq$Count / totalChars
  
  key <- crackKey(text)
  decryption <- xorEncryptAll(text, key)
  print(sum(decryption))
  
  print(rawToChar(as.raw(key)))
  print(rawToChar(as.raw(decryption)))
}

problem63 <- function(){
  
  print( sum ( sapply(c(1:10), function(i, n){
    return(nchar(paste(i ^ n)) == n)
  }, n=c(1:22) ) ))
    
}

problem67 <- function(){
  triangle <- readInput()
  
  largestRow <- as.vector(tail(triangle, 1))
  for (r in (dim(triangle)[1] - 1):1) {
    for (c in 1:dim(triangle)[2]) {
      largestRow[c] <- triangle[r, c] + max(largestRow[c], largestRow[c+ 1])
    }
    print(largestRow)
  }
}

problem79 <- function(){
  keylog <- unlist(read.csv("http://projecteuler.net/project/keylog.txt", header=FALSE))
  
  nums <- matrix(0, nrow=10, ncol=10)
  rownames(nums) <- 0:9
  colnames(nums) <- 0:9
  
  
  for(key in as.character(keylog)){
    dgts <- unlist(strsplit(key, ""))
    nums[dgts[1], dgts[2]] <- 1
    nums[dgts[1], dgts[3]] <- 1
    nums[dgts[2], dgts[3]] <- 1
    nums[dgts[2], dgts[1]] <- -1
    nums[dgts[3], dgts[1]] <- -1
    nums[dgts[3], dgts[2]] <- -1
  }
  
  nOrder <- names(sort(rowSums(nums)/ rowSums(abs(nums)), decreasing=TRUE))
  for(n in 1:length(nOrder)){
    if(sum(as.vector(nums[paste(nOrder[n]), ])==-1) > n-1 ) stop()
  }
  print(paste(nOrder, collapse=""))
}

problem83 <- function(){
  library(igraph)
  dat <- as.matrix(read.csv("http://projecteuler.net/project/matrix.txt", 
    header=FALSE))
  nNodes <- prod(dim(dat))
  
  nodesMat <- matrix(1:nNodes, nrow=dim(dat)[1], ncol=dim(dat)[2], byrow=TRUE)
  
  firstWeight <- dat[1, 1]
  
  #Set up adjacency list
  adjacencyList <- c()
  weights <- c()
  #Fill the adjancency list, row by col in dat
  for(i in 2:(dim(dat)[1] - 1)){
    for(j in 2:(dim(dat)[2] - 1)){
      adjacencyList <- c(adjacencyList, 
        nodesMat[i, j], nodesMat[i, j+1], 
        nodesMat[i, j], nodesMat[i+1, j], 
        nodesMat[i, j], nodesMat[i-1, j], 
        nodesMat[i, j], nodesMat[i, j-1])
      
      weights <- c(weights, dat[i, j+1], dat[i+1, j], dat[i-1, j], dat[i, j-1])
      
    }
    #Left edge - only down and right
    adjacencyList <- c(adjacencyList, nodesMat[i, 1], nodesMat[i+1, 1], 
      nodesMat[i, 1], nodesMat[i, 2])
    weights <- c(weights, dat[i+1, 1], dat[i, 2])
    
    #Right edge - only down
    adjacencyList <- c(adjacencyList, nodesMat[i, dim(dat)[2]], 
      nodesMat[i+1, dim(dat)[2]])
    weights <- c(weights, dat[i+1, dim(dat)[2]])
  }
  
  for(j in 1:(dim(dat)[2] - 1)){
    #Top edge - only right and down
    adjacencyList <- c(adjacencyList, 
      nodesMat[1, j], nodesMat[1, j + 1], 
      nodesMat[1, j], nodesMat[2, j])
    weights <- c(weights, dat[1, j+1], dat[2, j])
    
    #Bottom edge - only right
    adjacencyList <- c(adjacencyList, nodesMat[dim(dat)[1], j],
      nodesMat[dim(dat)[1], j + 1])
    weights <- c(weights, dat[dim(dat)[1], j+1])
  }
  
  #create the graph and assign edge weights
  gr <- graph(adjacencyList, directed=TRUE)
  E(gr)$weight <- weights
  
  dijkstra <- shortest.paths(graph=gr, v=1, to=nNodes, weights=weights, 
    mode="out")
  print(dijkstra + firstWeight)
}

problem84 <- function(){
  squares <- c('Go', 'Mediterranean', 'Community Chest', 'Baltic', 'Income Tax',
    'Reading Railroad', 'Oreintal', 'Chance', 'Vermont', 'Connecticut',
    'Jail', 'St Charles', 'Electric', 'States', 'Virginia', 
    'Pennsylvania Railroad', 'St James', 'Communtity Chest', 'Tennessee', 'New York',
    'Free Parking', 'Kentucky', 'Chance', 'Indiana', 'Illinois',
    'BO Railroad', 'Atlantic','Ventor', 'Water Works', 'Marvin Gardens',
    'Go To Jail', 'Pacific', 'North Carolina', 'Community Chest', 'Pennsylvania', 
    'Short Line Railroad', 'Chance', 'Park Place', 'Luxury Tax', 'Boardwalk')
  
  numSquares <- length(squares)
  diceSides <- 6
  
  #Markov for roll probabilities
  MarkovRoll <- matrix(0, nrow=numSquares, ncol=numSquares)
  colnames(MarkovRoll) <- squares
  rownames(MarkovRoll) <- squares
  
  diceProbs <- c(0:(diceSides), (diceSides-1):1) / diceSides^2
  possSquaresAdvance <- length(diceProbs)
  
  #Fill the roll probabilities; for each square j what is the probability of getting to it from i
  for(i in 1:(numSquares-1)){
    if(numSquares - i >= length(diceProbs)){
      MarkovRoll[i, (i+1):(i + length(diceProbs))] <- diceProbs
    }else{
      #Wrap around to the start of the board
      MarkovRoll[i, (i+1):numSquares] <- diceProbs[1:(numSquares - i)]
      MarkovRoll[i, 1:(length(diceProbs) - (numSquares - i))] <- 
        diceProbs[(numSquares - i + 1):length(diceProbs)]
    }
  }
  MarkovRoll[numSquares, 1:length(diceProbs)] <- diceProbs
  
  #If you roll 3 doubles in a row, go straight to jail
  # occurs anywhere on the board with the same probability
  threeDoubles <- 1 / diceSides ^ 3
  MarkovThreeDoubles <- diag(1 - threeDoubles, nrow=numSquares, ncol=numSquares)
  colnames(MarkovThreeDoubles) <- squares
  rownames(MarkovThreeDoubles) <- squares
  
  MarkovThreeDoubles[, "Jail"] <- MarkovThreeDoubles[, "Jail"] + threeDoubles
  
  
  #Create a stochastic matrix for all of the events that happen when a square is reached
  MarkovPostRoll <- diag(1, nrow=numSquares, ncol=numSquares)
  colnames(MarkovPostRoll) <- squares
  rownames(MarkovPostRoll) <- squares
  
  #Go to Jail, sends a player to Jail
  MarkovPostRoll["Go To Jail", ] <- 0
  MarkovPostRoll["Go To Jail", "Jail"] <- 1
  
  #Cards are drawn according to a uniform distribution with replacement
  
  #Community chest cards
  #Total = 16
  #No movement = 14
  # to Jail = 1
  # to Go = 1
  ndxCommChest <- c(1:numSquares)[squares=="Community Chest"]
  
  MarkovPostRoll[ndxCommChest, ] <- MarkovPostRoll[ndxCommChest, ] * 14 / 16
  MarkovPostRoll[ndxCommChest, "Jail"] <- 1 / 16
  MarkovPostRoll[ndxCommChest, "Go"] <- 1 / 16
  
  #Chace cards
  #Total = 16
  #No movement = 6
  # to Jail = 1
  # to Go = 1
  # to Reading Railroad = 1
  # to St Charles = 1
  # to Illinois = 1
  # to Boardwalk = 1
  # to nearest railroad
  # to nearest utility
  # back 3 spaces
  ndxChance <- c(1:numSquares)[squares=="Chance"]
  
  MarkovPostRoll[ndxChance, ] <- MarkovPostRoll[ndxChance, ] * 6 / 16
  MarkovPostRoll[ndxChance, "Jail"] <- MarkovPostRoll[ndxChance, "Jail"] + 1 / 16
  MarkovPostRoll[ndxChance, "Go"] <- MarkovPostRoll[ndxChance, "Go"] + 1 / 16
  MarkovPostRoll[ndxChance, "Reading Railroad"] <- MarkovPostRoll[ndxChance, "Reading Railroad"] + 1 / 16
  MarkovPostRoll[ndxChance, "St Charles"] <- MarkovPostRoll[ndxChance, "St Charles"] + 1 / 16
  MarkovPostRoll[ndxChance, "Illinois"] <- MarkovPostRoll[ndxChance, "Illinois"] + 1 / 16
  MarkovPostRoll[ndxChance, "Boardwalk"] <- MarkovPostRoll[ndxChance, "Boardwalk"] + 1 / 16
  
  #Nearest railroad logic (advance forward)
  MarkovPostRoll[8, 16] <- MarkovPostRoll[8, 16] + 2 / 16
  MarkovPostRoll[23, 26] <- MarkovPostRoll[23, 26] + 2 / 16
  MarkovPostRoll[37, 6] <- MarkovPostRoll[37, 6] + 2 / 16
  
  #Nearest utility logic (advance forward)
  MarkovPostRoll[8, 13] <- MarkovPostRoll[8, 13] + 1 / 16
  MarkovPostRoll[23, 29] <- MarkovPostRoll[23, 29] + 1 / 16
  MarkovPostRoll[37, 13] <- MarkovPostRoll[37, 13] + 1 / 16
  
  #Back 3 spaces
  MarkovPostRoll[ndxChance, ndxChance - 3] <- MarkovPostRoll[ndxChance, ndxChance - 3] + 
    diag(1 / 16, nrow=length(ndxChance), ncol=length(ndxChance))
  
  #Combine roll with all post roll events
  MarkovFullTurn <- MarkovRoll %*% MarkovThreeDoubles %*% MarkovPostRoll
  
  #   Get steady states probabilities (eigenvalue of 1)  
  #   eig <- eigen(MarkovFullTurn)
  
  #Step game forward
  forward <- c(1, rep(0, 39))
  
  for(t in 1:100){
    forward <- forward %*% MarkovFullTurn
  }
  forward <- as.vector(forward)
  names(forward) <- 0:39
  print(sort(forward, decreasing=TRUE)[1:5])
}

problem85 <- function(){
  best <- 2e6
  bestArea <- 0
  for(i in 1:2000){
    j <- 1
    numRectangles <- 0
    while(numRectangles < 2e6){
      j <- j + 1
      numRectanglesLast <- numRectangles
      numRectangles <- choose(i+1, 2) * choose(j+1, 2)
    }
    if(numRectangles - 2e6 < best){
      best <- numRectangles - 2e6
      bestArea <- i * j
    }
    if(2e6 - numRectanglesLast < best){
      best <- 2e6 - numRectanglesLast
      bestArea <- i * (j-1)
    }
  }
}

problem86 <- function(){
  
  start <- Sys.time()
  A <- cbind(c(1,2,2), c(-2,-1,-2), c(2,2,3))
  B <- cbind(c(1,2,2), c(2,1,2), c(2,2,3))
  C <- cbind(c(-1,-2,-2), c(2,1,2), c(2,2,3))
  primitiveTriples <- matrix(c(3,4,5))
  endRecurse <- 100
  limitMax <- 10000
  level <- 1
  begCol <- 1
  endCol <- 1
  while(level < endRecurse){
    for(col in begCol:endCol){
      triplet <- primitiveTriples[, col]
      if(min(triplet) < limitMax){
        primitiveTriples <- cbind(primitiveTriples, 
          A %*% triplet, B %*% triplet, C %*% triplet)
      }
    }
    level <- level + 1
    begCol <- endCol + 1
    endCol <- dim(primitiveTriples)[2]
  }
  
  tripleSides <- primitiveTriples[1:2, ]
  maxSides <- apply(primitiveTriples[1:2, ], 2, max)
  tripleSidesWithMax <- rbind(tripleSides, maxSides)
  tripleSidesWithMax <- tripleSidesWithMax[, order(tripleSidesWithMax[3, ])]
  
  M <- 1
  solutions <- 0
  target <- 1e6
  while(solutions < target){
    
    M <- M + max(ceiling(log(target - solutions, base=10)-3), 1)
    solutions <- 0
    col <- 0
    maxSide <- 1
    
    while(maxSide <= M * 2){
      
      col <- col + 1
      triplet <- tripleSidesWithMax[, col]
      
      maxSide <- triplet[3]
      
      solutions <- solutions + 
        getAllSolutions(len=triplet[1], baseheight=triplet[2], M=M) + 
        getAllSolutions(len=triplet[2], baseheight=triplet[1], M=M)
    }
  }
  time <- Sys.time() - start
  print(paste("M =", M))
  print(paste("It took", time))
  
  
  
  getAllSolutions <- function(len, baseheight, M){
    
    if(len > M) return(0)
    if(baseheight / 2 > len) return(0)
    
    solutions <- 0
    lll <- len
    bhbh <- baseheight
    if(baseheight <= len){
      while(len <= M){
        solutions <- solutions + floor(baseheight / 2)
        
        len <- len + lll
        baseheight <- baseheight + bhbh
      }
    }else{
      while(len <= M){
        solutions <- solutions + (1 + (len - floor((baseheight + 1) / 2)))
        
        len <- len + lll
        baseheight <- baseheight + bhbh
      }
    }
    return(solutions)
  } 
  
}

problem87 <- function(){
  allPrimes <- sieve(7072)
  
  squares <- allPrimes^2  #908
  cubes <- allPrimes^3
  cubes <- cubes[cubes < 50e6]  #73
  quarts <- allPrimes^4
  quarts <- quarts[quarts < 50e6]  #23
  
  dat <- expand.grid(quarts, cubes, squares)
  sums <- rowSums(dat)
  
  print(length(unique(sums[sums < 50e6])))  
}

problem91 <- function(){
  #For each node, count the right angles that it forms
  #0 to 50, 0 to 50
  sLength <- 50
  #Edge solutions
  edgeTotal <- 3 * sLength ^ 2
  #In the grid solutions
  print(edgeTotal + sum(apply(expand.grid(1:i, 1:j), 1, function(x){
    i <- x[1]; j <- x[2]
    levelTotal <- 0
    v1 <- c(i, j)
    
    #Find the normal to the first vector, only integer steps
    g <- gcd(i, j)
    intStepsX <- -j / g
    intStepsY <- i / g
    
    v2 <- v1 + c(intStepsX, intStepsY)  #Go left
    while(sum(v2 >= 0) == 2 && sum(v2 <= sLength) == 2){
      v2 <- v2 + c(intStepsX, intStepsY)
      levelTotal <- levelTotal + 1
    }
    v2 <- v1 - c(intStepsX, intStepsY)  #Go right
    while(sum(v2 >= 0) == 2 && sum(v2 <= sLength) == 2){
      v2 <- v2 - c(intStepsX, intStepsY)
      levelTotal <- levelTotal + 1
    }
    return(levelTotal)
  })))
}

problem93 <- function(){
  start <- Sys.time()
  operations <- c("+", "-", "*", "/")  
  
  mat <- c()
  for(i1 in 1:6){
    for(i2 in (i1 + 1):7){
      for(i3 in (i2 + 1):8){
        for(i4 in (i3 + 1):9){
          mat <- rbind(mat, c(i1,i2,i3,i4))
        }
      }
    }
  }
  
  consecutive <- apply(mat, 1, function(numbers){
    grid <- expand.grid(numbers, operations, numbers, operations, numbers, operations, numbers)
    
    ndx <- (grid[, 1] != grid[, 3] & grid[, 1] != grid[, 5] & grid[, 1] != grid[, 7] &
        grid[, 3] != grid[, 5] & grid[, 3] != grid[, 7] & grid[, 5] != grid[, 7] )
    
    grid <- grid[ndx, ]
    
    output <- apply(grid, 1, function(r){
      Reduce(as.character(r[6]), 
        c(Reduce(as.character(r[4]), 
          c(Reduce(as.character(r[2]), 
            c(as.integer(r[1]), as.integer(r[3]))), 
            as.integer(r[5]))),
          as.integer(r[7])))
    })
    
    outputParen <- apply(grid, 1, function(r){
      Reduce(as.character(r[6]), 
        c(Reduce(as.character(r[4]), 
          as.integer(r[5]), as.integer(r[7])),
          Reduce(as.character(r[2]), 
            c(as.integer(r[1]), as.integer(r[3])))))
      
    })
    
    output <- c(output, outputParen)
    
    output <- sort(unique(output[output > 0 & output == round(output)]))
    oLast <- 0
    for(o in output){
      if(o != oLast + 1) return(oLast)
      oLast <- o
    }
    return(oLast)
  })
  print(paste(mat[consecutive == max(consecutive), ], collapse=""))
  print(Sys.time() - start)
}

problem94 <- function(){
  pell <- pellSequence(n=3, limit=ceiling(1e9/3))
  sidesPlus1 <- (pell$x * 2 + 1) / 3 
  sidesMinus1 <- (pell$x * 2 - 1) / 3
  sidesPlus1 <- sidesPlus1[sidesPlus1 == round(sidesPlus1) & 
      sidesPlus1 != 1 & sidesPlus1 < 1e9/3]
  sidesMinus1 <- sidesMinus1[sidesMinus1 == round(sidesMinus1) & 
      sidesMinus1 != 1 & sidesMinus1 < 1e9/3]
  total <- sum(c(sidesPlus1 * 3 + 1, sidesMinus1 * 3 - 1))
  print(total)
  
  pellSequence <- function(n=2, x1=NULL, y1=NULL, limit=1e6){
    
    if(is.null(x1) && is.null(y1)){
      x1 <- 1
      while(x1 < 1e6){
        x1 <- x1 + 1
        y1 <- sqrt((x1^2 - 1) / n)
        if(round(y1) == y1) break
      }
      if(round(y1) != y1) stop("Couldn't calculate y1.")
    }
    
    if(x1 ^ 2 - n * y1 ^ 2 != 1) stop("x1, y1, and n are invalid.")
    
    x <- c(x1)
    y <- c(y1)
    
    while(x[length(x)] < limit){
      x <- c(x, x1 * x[length(x)] + n * y1 * y[length(y)])
      y <- c(y, x1 * y[length(y)] + y1 * x[length(x) - 1])
    }
    return(list(x=x, y=y))
  }
  
}

problem95 <- function(){
  limit <- 1e6
  divisorSum = rep(0, limit)
  #For each divisor up to half of the limit
  for(div in 1:(limit/2)){
    #For each multiple of that divisor up to the limit
    for(mult in seq((2 * div), limit, div)){
      divisorSum[mult] <- divisorSum[mult] + div
    }
  }
  
  allCounts <- c(0)
  for(num in 2:limit){
    divSum0 <- divisorSum[num]
    count <- 1
    
    if(divSum0 < limit && divSum0 > num){
      divSum <- c(num, divSum0)
      nextDivSum <- divisorSum[divSum0]
      count <- 2
      while(!is.element(nextDivSum, divSum)){
        divSum <- c(divSum, nextDivSum)
        nextDivSum <- divisorSum[divSum[length(divSum)]]
        count <- count + 1
        if(is.na(nextDivSum)){
          count <- 0
          break
        }
        if(nextDivSum > limit){
          count <- 0
          break
        }else if(nextDivSum == 0){
          count <- 0
          break
        }else if(nextDivSum < num){
          count <- 0
          break
        }
      }
      if(count > 0 && nextDivSum != num){
        count <- 0
      }
    }else if(divSum0 == num){
      count <- 0
    }else if(divSum0 >= limit){
      count <- 0
    }else{
      count <- allCounts[divSum0]
    }
    if(is.na(count)) browser()
    
    allCounts <- c(allCounts, count)
  }
  print(match(max(allCounts), allCounts))
}

problem98 <- function(){
  
  start <- Sys.time()
  words <- unlist(read.csv("http://projecteuler.net/project/words.txt", header=FALSE))
  chars <- nchar(words)
  found <- FALSE
  foundLevel <- 0
  level <- max(chars)
  anagrams <- c()
  
  while(level > 0){
    wordsSubset <- words[chars==level]
    
    chr <- vapply(wordsSubset, function(w){
      char <- unlist(strsplit(w, ""))
      return(paste(sort(char), collapse=""))
    }, "a")
    
    ndx <- c()
    for(c1 in 1:(length(chr)-1)){
      for(c2 in (c1+1):length(chr)){
        if(chr[c1]==chr[c2]){
          ndx <- rbind(ndx, c(c1, c2))
        }
      }
    }
    if(!is.null(ndx)){
      foundLevel <- level
      found <- TRUE
      
      for(n in 1:dim(ndx)[1]){
        anagrams <- rbind(anagrams, wordsSubset[ndx[n, ]])
      }
    }
    level <- level - 1
  }
  
  solution <- 0
  
  for(rrr in 1:dim(anagrams)[1]){
    a <- anagrams[rrr, ]
    digits <- nchar(a[1])
    
    if(solution / digits >= 10) break
    
    uChar <- unique(unlist(strsplit(a[1], "")))
    
    squares <- as.character ( c(ceiling(10 ^ ((digits-1) / 2)):
        (10 ^ (digits / 2))) ^ 2 )
    
    ndxGood <- sapply(squares, function(s){
      sChar <- length(unique(unlist(strsplit(s, ""))))
      return(sChar==length(uChar))
    })
    
    squares <- squares[ndxGood]
    
    chars1 <- unlist(strsplit(a[1], ""))
    chars2 <- unlist(strsplit(a[2], ""))
    
    #Map squares to first anagram and then rearrange letters for second anagram
    largestSquare <- sapply(squares, function(s){
      s <- unlist(strsplit(s, ""))
      names(chars1) <- s
      otherSquare <- as.integer(paste(names(chars1[match(chars2, chars1)]), 
        collapse=""))
      if(is.element(otherSquare, squares)){
        return(max(as.numeric(s), as.numeric(otherSquare)))
      }
      else return(0)
    })
    
    solution <- max(solution, as.numeric(largestSquare))
  }
  print(Sys.time() - start)
  print(solution)
}

problem100 <- function(){
  
  #Pell equation for approximating sqrt(1/2):
  # x ^ 2 - 1/2 * y ^ 2 = 1
  
  n <- 1/2
  
  x1 <- 3
  y1 <- 4
  
  x <- x1
  y <- y1
  allX <- x
  allY <- y
  
  while(y < 1e12){
    xlast <- x
    ylast <- y
    
    x <- x1 * xlast + n * y1 * ylast
    y <- x1 * ylast + y1 * xlast
    
    allX <- c(allX, x)
    allY <- c(allY, y)
  }
  
  midpoint <- mean(c(allX[length(allX)], allY[length(allY)]))
  
  totalDisks <- ceiling(midpoint)
  
  approxBlue <- sqrt(1/2) * totalDisks
  
}

problem101 <- function(){
  u <- function(n){
    return(1- n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10)
  }
  correctSeq <- u(1:10)
  
  FIT <- c(correctSeq[1])  #First term is just the constant
  
  FIT <- c(FIT, sapply(2:10, function(degree){
    nnn <- 1:degree
    kTerms <- correctSeq[nnn]
    
    frmla <- "kTerms ~ "
    for(d in nnn[nnn!=1]){
      frmla <- paste(frmla, paste("I(nnn^", (d-1), ")", sep=""), sep=" + ")
    }
    model <- lm(as.formula(frmla))
    return(predict(object=model, data.frame(nnn=(degree+1)))) #FIT
  }))
  
  print(sum(FIT))
}

problem103 <- function(){
  minimizeSpecialSumSet <- function(set){
    if(!is.specialSumSet(set)) stop("Input not a special sum set")
    
    n <- length(set)
    #Try plus or minus 2
    gr <- expand.grid(as.data.frame(matrix(rep(c(-2:2), n), ncol=n)))
    trySets <- apply(gr, 1, function(x){
      if(sum(x) >= 0) return(FALSE)
      newSet <- x + set
      if(!identical(newSet, sort(newSet))) return(FALSE)
      return(is.specialSumSet(newSet))
    })
    
    if(sum(trySets) == 0) return(set)
    
    rwSums <- rowSums(gr[trySets, ])
    output <- gr[trySets, ][rwSums == min(rwSums), ]
    if(dim(output)[1] > 1) return(output[1,] + set)
    else return(output + set)
  }
  is.specialSumSet <- function(set){
    n <- length(set)
    if(n < 3) return(TRUE)
    if(length(unique(set)) != n) return(FALSE)
    
    #If B contains more elements than C then S(B) > S(C).
    if(prod(sapply(2:ceiling(n/2), function(x) {
      return(sum(set[1:x]) > sum(set[(n-x+2):n]))
    }) ) != 1) return(FALSE)
    
    for(subSize in 1:floor(n/2)){
      #S(B) != S(C); that is, sums of subsets cannot be equal.
      
      #For each subset check if there is no problem
      subset1 <- combn(set, subSize)
      if(sum(apply(subset1, 2, function(s1){
        subset2 <- combn(set[!is.element(set, s1)], subSize)
        return(colSums(subset2) == sum(s1))
      })) != 0) return(FALSE)
    }
    return(TRUE)
  }
  
  start <- Sys.time()
  optimal6 <- c(11, 18, 19, 20, 22, 25)
  bOpt <- 20
  nearOptimal7 <- c(bOpt, bOpt + optimal6)
  is.specialSumSet(nearOptimal7)  #TRUE
  print(paste(minimizeSpecialSumSet(nearOptimal7) , collapse=""))
  print(Sys.time() - start)
}

problem105 <- function(){
  is.specialSumSet <- function(set){
    n <- length(set)
    if(n < 3) return(TRUE)
    if(length(unique(set)) != n) return(FALSE)
    
    #If B contains more elements than C then S(B) > S(C).
    if(prod(sapply(2:ceiling(n/2), function(x) {
      return(sum(set[1:x]) > sum(set[(n-x+2):n]))
    }) ) != 1) return(FALSE)
    
    for(subSize in 1:floor(n/2)){
      #S(B) != S(C); that is, sums of subsets cannot be equal.
      
      #For each subset check if there is no problem
      subset1 <- combn(set, subSize)
      if(sum(apply(subset1, 2, function(s1){
        subset2 <- combn(set[!is.element(set, s1)], subSize)
        return(colSums(subset2) == sum(s1))
      })) != 0) return(FALSE)
    }
    return(TRUE)
  }
  
  start <- Sys.time()
  dat <- unlist(read.delim("http://projecteuler.net/project/sets.txt", header=FALSE))
  print(sum(sapply(dat, function(x){
    x <- as.numeric(strsplit(x, split=",")[[1]])
    return(ifelse(is.specialSumSet(sort(x)), sum(x), 0))
  })))
  print(Sys.time() - start)
}

problem109 <- function(){
  spots <- data.frame(Name=c("0", as.character(c(1:20, 25)),
    paste("D", c(1:20, 25), sep=""), paste("T", c(1:20), sep="")),
    Value=c(0:20, 25, c(1:20, 25) * 2, c(1:20) * 3))
  checkoutDart <- c(1:20, 25) * 2
  leaves <- 100 - checkoutDart
  
  gr <- expand.grid(spots$Name, spots$Name)
  gr$combo <- apply(gr, 1, function(x) paste(sort(c(x[1], x[2])), collapse=",") )
  comboDarts <- data.frame(Darts=unique(gr$combo))
  comboDarts$value <- apply(comboDarts, 1, function(x){
    return(sum(sapply(strsplit(x, ",")[[1]], function(nm){
      return(spots$Value[spots$Name==nm])
    })))
  })
  
  print(sum(sapply(leaves, function(lll){
    return(sum(comboDarts$value < lll))
  })))
}

problem112 <- function(){
  proportion <- 0
  bouncy <- 0
  num <- 0
  
  while(proportion < .99){
    num <- num + 1
    
    if(is.bouncy(num)){
      bouncy <- bouncy + 1
    }
    
    proportion <- bouncy / num
  }
  
  is.bouncy <- function(num){
    dgts <- as.numeric(unlist(strsplit(as.character(num), "")))
    if(sum(sort(dgts) != dgts)==0) return(FALSE)    #check left to right
    if(sum(sort(dgts, decreasing=TRUE) != dgts)==0) return(FALSE) #check right to left
    return(TRUE)
  }
}

problem113 <- function(){
  
  sum(sapply(1:100, function(i){
    level <- choose(8 + i, i)     #LTR
    level <- level + choose(9 + i, i) - 1 #RTL
    level <- level - 9  #Duplicates
    return(level)
  }))
  
}

problem114 <- function(){
  blockSolutions <- function(contgBlocks, len){
    solutions <- 1 #Empty solution
    if(contgBlocks > len) return(solutions)
    
    for(start in 1:(len - contgBlocks + 1)){ #Where can it start
      for(blockLength in contgBlocks:(len - start + 1)){ #How far out can it go
        solutions <- solutions + 
          memoizedCall(what=blockSolutions, contgBlocks = contgBlocks, 
            len = len - start - blockLength)
      }
    }
    return(solutions)
  }
  blockSolutions(3, 50)
}

problem115 <- function(){
  blockSolutions <- function(contgBlocks, len){
    solutions <- 1 #Empty solution
    if(contgBlocks > len) return(solutions)
    
    for(start in 1:(len - contgBlocks + 1)){ #Where can it start
      for(blockLength in contgBlocks:(len - start + 1)){ #How far out can it go
        solutions <- solutions + 
          memoizedCall(what=blockSolutions, contgBlocks = contgBlocks, 
            len = len - start - blockLength)
      }
    }
    return(solutions)
  }
  
  solutions <- 0
  nnn <- 50
  while(solutions < 1e6){
    solutions <- memoizedCall(what=blockSolutions, contgBlocks = 50, 
      len = nnn)
    nnn <- nnn + 1
  }
}

problem120 <- function(){
  ##(a-1)^n + (a+1)^n expanded
  #   1: 2a
  #   2: 2a^2 + 2
  #   3: 2a^3 + 6a
  #   4: 2a^4 + 12a^2 + 2
  #   5: 2a^5 + 20a^3 + 10a
  
  ##divide by a^2
  #   1: 2 / a
  #   2: 2 + 2 / a^2
  #   3: 2a + 6 / a
  #   4: 2a^2 + 12 + 2 / a^2
  #   5: 2a^3 + 20a + 10 / a
  
  ##part that is not divisible
  #   1: 2 / a
  #   2: 2 / a^2
  #   3: 6 / a
  #   4: 2 / a^2
  #   5: 10 / a
  
  #Check only the odds, because they are the max
  #Odds * 2, then check the modulus
  
  #Overkill on the amount of possible exponents numbers
  possRemainder <- seq(1, 99999, by=2) * 2
  sum(sapply(3:1000, function(a) max(possRemainder %% a) * a))
}

problem121 <- function(){
  reds <- 1
  blues <- 1
  turns <- 15
  probBlue <- rep(0, turns)
  for(turn in 1:turns){
    probBlue[turn] <- blues / (reds + blues)
    reds <- reds + 1
  }
  probRed <- 1 - probBlue
  numWinner <- (floor(turns / 2) + 1):turns
  
  ndx <- c(1:turns)
  winningProb <- sum(sapply(numWinner, function(x){
    ndxBlue <- combn(ndx, x)
    return(sum(apply(ndxBlue, 2, function(y){
      prod(probBlue[y]) * prod(probRed[ndx[!is.element(ndx, y)]])
    })))
  }))
  print(floor(1 / winningProb))
}

problem124 <- function(){
  rad <- function(n) return(prod(unique(as.numeric(factorize(n)))))
  allNums <- c(1:100000)
  radicals <- sapply(allNums, rad)
  names(radicals) <- allNums
  sort(radicals)[10000]
}

problem145 <- function(){
  first <- c(1, 3, 5, 7, 9)
  last <- c(2, 4, 6, 8)
  noCarryover <- 0
  for(f in first){
    for(l in last){
      if(f + l < 10) noCarryover <- noCarryover + 1
    }
  }
  first <- c(1, 3, 5, 7, 9)
  last <- c(0, 2, 4, 6, 8)
  noCarryoverWith0 <- 0
  for(f in first){
    for(l in last){
      if(f + l < 10) noCarryoverWith0 <- noCarryoverWith0 + 1
    }
  }
  first <- c(0, 2, 4, 6, 8)
  last <- c(0, 2, 4, 6, 8)
  evenNoCarryover <- 0
  for(f in first){
    for(l in last){
      if(f + l < 10) evenNoCarryover <- evenNoCarryover + 1
    }
  }
  first <- c(1, 3, 5, 7, 9)
  last <- c(1, 3, 5, 7, 9)
  oddWithCarryover <- 0
  for(f in first){
    for(l in last){
      if(f + l > 10) oddWithCarryover <- oddWithCarryover + 1
    }
  }
  #1 digit
  total <- 0
  #2 digit - all but the carry over (and the reverses)
  total <- total + (noCarryover * 2)
  #3 digit - first odd, last odd, with carry over, middle digit no carry over
  total <- total + (noCarryover * 2) * 5
  #4 digit - first odd, last even, no carryover
  total <- total + (noCarryover * 2) * (noCarryoverWith0 * 2)
  #5 digit - no solutions because of carryover issues
  #6 digit - see 4 digit case
  total <- total + (noCarryover * 2) * (noCarryoverWith0 * 2) ^ 2
  #7 digit - first odd, last odd; next pair even no carryover;
  #          final pair odd no carryover; middle digit no carry over
  total <- total + (noCarryover * 2) * (evenNoCarryover * 2 - 5) * 
    (oddWithCarryover * 2) * 5
  #8 digit - see 4 digit case
  total <- total + (noCarryover * 2) * (noCarryoverWith0 * 2) ^ 3
  #9 digit - more carryover issues, 0 solutions
  print(total)
  
  #For checking
  reverseDigits <- function(x){
    as.numeric(paste(rev(strsplit(as.character(x),"")[[1]]),collapse="")) 
  }
  
  allOddDigits <- function(x){
    return(sum(as.numeric(strsplit(as.character(x),"")[[1]]) %% 2 == 0) == 0)
  }
}

problem205 <- function(){
  getDiceDistribution <- function(sides=c(1:6), nRolls=2){
    nSides <- length(sides)
    return( rowSums( vapply(1:nRolls, function(roll){
      rep(sides, times=nSides ^ (roll - 1), each=nSides ^ (nRolls - roll))
    }, rep(0, nSides ^ nRolls)) ) )
  }
  
  dPeter <- getDiceDistribution(sides=c(1:4), nRolls=9)
  dColin <- getDiceDistribution(sides=c(1:6), nRolls=6)
  
  distPeter <- as.data.frame(table(dPeter), stringsAsFactors=FALSE)
  names(distPeter) <- c("Total", "Peter")
  distColin <- as.data.frame(table(dColin), stringsAsFactors=FALSE)
  names(distColin) <- c("Total", "Colin")
  
  dist <- merge(distPeter, distColin, by="Total", all=TRUE)
  dist[is.na(dist)] <- 0
  dist$Peter <- dist$Peter / sum(dist$Peter)
  dist$Colin <- dist$Colin / sum(dist$Colin)
  dist$Total <- as.numeric(dist$Total)
  dist <- dist[order(dist$Total), ]
  
  dist$CumlColin <- cumsum(dist$Colin)
  sum(dist$Peter * (dist$CumlColin - dist$Colin))
}

problem206 <- function(){
  # 1_2_3_4_5_6_7_8_9_0
  # 100 * 1_2_3_4_5_6_7_8_9 : 10 * sqrt(1_2_3_4_5_6_7_9)
  # 1_2_3_4_5_6_7_8_9  #17 digits
  
  library(gmp)
  options(scipen=20)
  
  start <- Sys.time()
  digits <- data.frame(digit=c(0,2,4,6,8,10,12,14,16),
    num=c(9,8,7,6,5,4,3,2,1))
  allowed <- c()
  
  for(digit in digits$digit[digits$digit < 7]){
    if(length(allowed) != 0){
      lead <- 01:99 * 10^(digit-1) 
      iii <- c()
      for(l in lead){
        for(a in allowed){
          iii <- c(iii, l + a)
        }
      }
    }else{
      iii <- 1:9
    }
    ndx <- sapply(iii, function(i){
      dgts <- unlist(strsplit(as.character(i ^ 2), ""))
      l <- length(dgts)
      return(dgts[length(dgts)-digit]==digits$num[digits$digit==digit])
    })
    allowed <- iii[ndx]
  }
  
  lead <- 10:14 * 10^(7)
  poss <- c()
  for(l in lead){
    for(a in allowed){
      poss <- c(poss, l + a)
    }
  }
  
  squares <- as.bigz(poss) ^ 2
  ndx <- squares < 2*1e16 & squares > 1*1e16
  squares <- squares[ndx]
  poss <- poss[ndx]
  
  chars <- as.character(squares)
  xRegex <- "1[0-9]2[0-9]3[0-9]4[0-9]5[0-9]6[0-9]7[0-9]8[0-9]9"
  ndx <- grepl(xRegex, chars)
  print(poss[ndx] * 10) #Add the sqrt(100) back
  print(Sys.time() - start)
}
