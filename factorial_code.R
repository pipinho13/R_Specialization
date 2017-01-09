
####First Function: Calculate the factorial of an integer using looping

Factorial_loop<-function(n=1) {
  stopifnot(n >= 0)
  if (n==0) {return(1)}
  else {
    
    product<-1
    for (i in 1:n) {
      
      product<-product*i
      
    }
    
    return(product)
    
  }

  
}


####Second Function: Calculate the factorial of an integer using reduce function

Factorial_reduce<-function(n=1) {
  require(purrr)
  stopifnot(n >= 0)
  if (n==0) {return(1)}
  reduce(seq_len(n), function(x,y) {
  
  as.numeric(x*y)
  }
  
  )
  
}





####Third Function:  a version that uses recursion to compute the factorial.

Factorial_func<-function(n=1) {
  stopifnot(n >= 0)
  if(n==0) {1} 
 
  else {n*Factorial_func(n-1)
    
  }
}



####Forth Function:  a version that uses memoization to compute the factorial..

Factorial_mem<-function(n=1) {

Fn<-c(1)
Factorial_mem_in<-function(n=1) {
 
  stopifnot(n >= 0)
  if(!is.na(Fn[n])) { Fn[n]}
  
  else {
    Fn[n]<<-n*Factorial_mem_in(n-1)
    return(Fn[n])
  }
  
}

}

library(microbenchmark)

##Comparison for n=100

microbenchmark(a <- Factorial_loop(100),
               b <- Factorial_reduce(100),
               c<-Factorial_func(100),
               d<-Factorial_mem(100)
               
)


##Comparison for n=10

microbenchmark(a <- Factorial_loop(10),
               b <- Factorial_reduce(10),
               c<-Factorial_func(10),
               d<-Factorial_mem(10)
               
)
