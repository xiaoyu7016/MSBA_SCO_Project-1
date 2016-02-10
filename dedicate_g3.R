
library(lpSolveAPI)

dedicate_g3 <- function(P,C,M,L)
{
  Mat <- matrix(0,length(L),length(P))
  for (j in 1:length(P))
  {
    Maturity = M[j]
    if (Maturity > 1) {
      #print(Maturity)
      Mat[1:Maturity - 1,j] <- C[j]
    }
    Mat[Maturity,j] <- 100 + C[j]
    if (sum(Maturity + 1) < nrow(Mat))
    {
      Mat[sum(Maturity + 1):nrow(Mat),j] = 0
    }
  }
  Mat[1,length(P)] = -1
  Mat[2,length(P)] = 1
  for (Col in length(P):ncol(Mat))
  {
    for (Row in 2:nrow(Mat))
    {
      if (Mat[Row,sum(Col - 1)] == 1)
      {
        Mat[Row,Col] = -1
      }
      if (Mat[sum(Row - 1),Col] == -1)
      {
        Mat[Row,Col] = 1
      }
    }
  }
  
  
  Proj <- make.lp(length(L),ncol(Mat))
  for (i in 1:ncol(Mat))
  {
    set.column(Proj,i,Mat[,i])
  }
  Price <- matrix(0,length(P))
  for (i in 1:length(P))
  {
    Price[i] = P[i]
  }
  set.objfn(Proj,Price)
  set.constr.type(Proj,rep("=",nrow(Mat)))
  set.rhs(Proj,L)
  lp.control(Proj,sense = 'min')
  sol = solve(Proj)
  dual = get.sensitivity.rhs(Proj)
  #print(dual)
  
  returns = c(Proj, dual)
  return (returns)

}
# Test

P = c(102,99,101,98,98,104,100,101,102,94)
C = c(102,99,101,98,98,104,100,101,102,94)
M = c(1,2,2,3,4,5,5,6,7,8)
L = c(12000,18000,20000,20000,16000,15000,12000,10000
Portfolio<-dedicate_g3(P,c,M,L))
get.objective(Portfolio[[1]])
get.variables(Portfolio[[1]])

