ReturnCoins = function(M){
  pd = 50
  ds = 10
  dc = 20
  p = 5
  d = 2
  j = 0
  r = M
  pd = M %/% pd
  r = M-50*pd
  dc = r %/% dc
  r = r-20*dc
  ds = r %/% ds
  r = r-10*ds
  p = r %/% p
  r = r-5*p
  d = r %/% d
  r = r-2*d
  j = r
  
  return(c(pd,dc,ds,p,d,j))
}

ReturnCoins(67)


ReturnCoins2 = function(M,den){
  r = M
  res = c()
  for (i in 1:(length(den))){
    a = 0
    a = r %/% den[i]
    res = append(res,a)
    r = r-(den[i]*a)
  }
  return(res)
}

ReturnCoins2(67,c(40,25,15,4,1))


Chocolate = function(M,r=1,c=1){
  if (r == nrow(M)){
    return(M[r,c])
  }
  else{
    bars = M[r,c]
    down = Chocolate(M, r+1, c)
    diagonal = Chocolate(M, r+1, c+1)
    return(max(down, diagonal) + bars)
  }
}

M = array(2:18, dim=c(4,4))
Chocolate(M)


Chocolate2 = function(M,r=1,c=1){
  # Tohle nefunguje lol
  while(TRUE){
    if (r == nrow(M)){
      return(M[r,c])
    }
    else{
      bars = M[r,c]
      down
      down = Chocolate(M, r+1, c)
      diagonal = Chocolate(M, r+1, c+1)
      
    }
  }
  return(max(down, diagonal) + bars)
  
}

Chocolate2(M)


HanoiTowers = function(n, fromPeg=1, toPeg=1){
  if (n == 1){
    print(sprintf('Move disc from peg %s to peg %s', fromPeg, toPeg))
    return()
  }
  unusedPeg = 6 - fromPeg - toPeg
  HanoiTowers(n - 1, fromPeg, unusedPeg)
  print(sprintf('Move disc from peg %s to peg %s', fromPeg, toPeg))
  HanoiTowers(n - 1, unusedPeg, toPeg)
  return()
}

HanoiTowers(5,1,3)
