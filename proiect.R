generareTs <- function(s)
{
  t <- s
  lambda_const <- 23
  while (TRUE) {
    U1 <- runif(1)
    U2 <- runif(1)
    t <- t - (1/lambda_const) * log(U1)

    if(U2 <= lambda(t)/lambda_const)
    {
      return(t)
    }
  }
}

f_exponentiala <- function(n, lambda){
  U <- runif(n)
  return(-1/lambda * log(U))
}

# X~Norm(0,1), generarea a n valori dintr-o Norm(0, 1)
f_norm_one <- function(v=1)
{
  while(TRUE) {
    Y <- f_exponentiala(1, 1)
    U_1 <- runif(1)
    if (U_1 <= exp(-(Y-1)^2 / 2)) {
      X_modul <- Y
      U_2 <- runif(1)
      
      if (U_2 <= 1/2)
        X <- -X_modul
      else
        X <- X_modul
      
      return(X+4)
    }
  }
}

G2 <- function(n)
{
  rez <- sapply(1:n, f_norm_one)
  return(rez)
}

# Pois de parametru lambda
lambda <- function(t)
{
  if (t >= 0 && t <= 3)
    return (2*t^2+5)
  if (t > 3 && t < 4)
    return (12)
  if (t >= 4 && t <= 5)
    return (1/10 * exp(t) + t)
  if (t > 5)
    return (17)
}

pois <- function(l)
{
  U <- runif(1)
  i <- 0
  p <- exp(-l)
  f <- p
  while (TRUE)
  {
    if (U < f)
      return (i)
    p <- (l*p)/(i+1)
    f <- f + p
    i <- i + 1
  }
}

pois_N <- function(n,l)
{
  x <- sapply(1:n, function(x=1) {pois(l)} )
  return(x)
}

G1 <- function(n) {
  U <- runif(n)
  return ( (-2+sqrt(4+60*U)) / 6 )
}

patience <- function()
{
  return(runif(1))
}

next_client <- function()
{
  n <- nrow(clienti)
  #while(clienti[n,]$d == Inf)
  #{
  #  n <- n + 1
  #}
  return(n)
}

front_queue <- function()
{
  return( as.numeric(rownames(head(clienti[clienti$s==0 & clienti$d == Inf & clienti$lost == 0,],1))) )
}

first_to_leave <- function()
{
  x <- head(clienti[clienti$lost == 0 & clienti$s == 0,][order(clienti$p), ],1)
  if (!is.na(x$p))
    return(x)
  return(0)
}

nr_clienti_inainte = 0

simulare <- function(end,qlen) {
  
  t <- 0
  nr_clienti <- 0
  casa1 <- 0
  casa2 <- 0
  T0 <- generareTs(t)
  t_A <- T0
  t1 <- t2 <- Inf
  clienti[1,] <<- c(T0,Inf,0,T0+patience(),0)
  
  while (TRUE)
  {
    if (t > end)
    {
      break
    }
    else if (t > end - 1)
    {
      nr_clienti_inainte <<- nrow(clienti)
    }
    # Cazul 0
    # testeaza daca cineva s-a plictisit si nu a ajuns inca la vreo casa
    c_plictis <- first_to_leave()
    if (c_plictis != 0 && c_plictis$p <= min(t_A,t1,t2))
    {
      i <- as.numeric(rownames(c_plictis))
      clienti[i,]$lost <<- 1
      nr_clienti <- nr_clienti - 1
      print(cat("A plecat",i,nr_clienti))
    }
    
    print(cat(nr_clienti,casa1,casa2))
    # Cazul 1
    # Soseste un client, verificam daca poate fi servit imediat sau
    # intra in coada de asteptare
    if (t_A == min(t_A,t1,t2))
    {
      t <- t_A
      N <- next_client()
      t_A <- generareTs(t)
      clienti[nrow(clienti)+1,] <<- c(t_A,Inf,0,p=t_A+patience(),0)
      
      # daca ambele servere sunt libere clientul se duce la primul
      if (nr_clienti == 0)
      {
        nr_clienti <- 1
        casa1 <- N
        casa2 <- 0
        t1 <- t + G1(1)
        clienti[N,]$s <<- 1
        next
      }
      # daca serverul 1 este liber clientul il alege
      if (nr_clienti == 1 && casa1 == 0)
      {
        nr_clienti <- 2
        casa1 <- N
        t1 <- t + G1(1)
        clienti[N,]$s <<- 1
        next
      }
      # daca serverul 2 este liber clientul il alege
      if (nr_clienti == 1 && casa2 == 0)
      {
        nr_clienti <- 2
        casa2 <- N
        t2 <- t + G2(1)
        clienti[N,]$s <<- 2
        next
      }
      # daca ambele servere sunt ocupate atunci clientul intra in coada
      if (nr_clienti > 1)
      {
        if (nr_clienti < qlen) {
          nr_clienti <- nr_clienti + 1
        }
        else {
          clienti <<- clienti[-nrow(clienti),]
        }
        next
      }
    }
    
    # Cazul 2
    # Serverul 1 se elibereaza inainte de sosirea unui client nou
    # si inaintea serverului 2
    if (t1 < t_A && t1 <= t2)
    {
      t <- t1
      clienti[casa1,]$d <<- t 
      
      # daca am doar un client, serverul 1 se elibereaza
      if (nr_clienti == 1)
      {
        nr_clienti <- 0
        casa1 <- 0
        casa2 <- 0
        t1 <- Inf
        next
      }
      # daca am doi clienti eliberez serverul 1
      if (nr_clienti == 2)
      {
        nr_clienti <- 1
        casa1 <- 0
        t1 = Inf
        next
      }
      if (nr_clienti > 2)
      {
        nr_clienti <- nr_clienti - 1
        casa1 <- front_queue()
        t1 <- t + G1(1)
        clienti[casa1,]$s <<- 1
        next
      }
    }
    
    # Cazul 3
    # Serverul 2 se elibereaza inaintea serverului 1
    # si inainte de venirea unui nou cleint
    if (t2 < t_A && t2 <= t1)
    {
      t <- t2
      clienti[casa2,]$d <<- t 
      
      # daca am doar un client, serverul 2 se elibereaza
      if (nr_clienti == 1)
      {
        nr_clienti <- 0
        casa1 <- 0
        casa2 <- 0
        t2 <- Inf
        next
      }
      # daca am doi clienti eliberez serverul 2
      if (nr_clienti == 2)
      {
        nr_clienti <- 1
        casa2 <- 0
        t2 = Inf
        next
      }
      if (nr_clienti > 2)
      {
        nr_clienti <- nr_clienti - 1
        casa2 <- front_queue()
        t2 <- t + G2(1)
        clienti[casa2,]$s <<- 2
      }
    }
  }
}

clienti <- data.frame(a=numeric(),d=numeric(),s=numeric(),p=numeric(),lost=numeric())
simulare(9,10)

timp_total <- list()
timp_total_1 <- list()
timp_total_2 <- list()

for(i in 1:nrow(clienti)) {
  if ( is.finite(clienti[i,]$d) ) {
    if (clienti[i,]$s == 1)
      timp_total[i] <- timp_total_1[i] <- clienti[i,]$d - clienti[i,]$a
    if (clienti[i,]$s == 2)
      timp_total[i] <- timp_total_2[i] <- clienti[i,]$d - clienti[i,]$a
  }
}

sprintf("Timpul minim petrecut de un client in sistem este %f", min(unlist(timp_total)))
sprintf("Timpul maxim petrecut de un client in sistem este %f", max(unlist(timp_total)))
sprintf("Timpul mediu petrecut de un client in sistem este %f", mean(unlist(timp_total)))

sprintf("Timpul minim petrecut de un client in sistem pt server 1 este %f", min(unlist(timp_total_1)))
sprintf("Timpul maxim petrecut de un client in sistem pt server 1 este %f", max(unlist(timp_total_1)))
sprintf("Timpul mediu petrecut de un client in sistem pt server 1 este %f", mean(unlist(timp_total_1)))

sprintf("Timpul minim petrecut de un client in sistem pt server 2 este %f", min(unlist(timp_total_2)))
sprintf("Timpul maxim petrecut de un client in sistem pt server 2 este %f", max(unlist(timp_total_2)))
sprintf("Timpul mediu petrecut de un client in sistem pt server 2 este %f", mean(unlist(timp_total_2)))


C1 <- nrow(clienti[clienti$s == 1,])
C2 <- nrow(clienti[clienti$s == 2,])
sprintf("Numarul mediu de clienti serviti este %f",(C1+C2)/2)
sprintf("Numarul mediu de clienti serviti de server 1 este %f", C1/2)
sprintf("Numarul mediu de clienti serviti de server 2 este %f", C2/2)

clienti_pierduti <- subset(clienti, lost == 1)
sprintf("Primul moment de timp cand este pierdut un client este %f",head(clienti_pierduti[order(clienti_pierduti$p),],1)$p)

nr_pierduti <- nrow(clienti[clienti$lost == 1,])
sprintf("Numarul de clienti pierduti %f",nr_pierduti)

#clienti <- data.frame(a=numeric(),d=numeric(),s=numeric(),p=numeric(),lost=numeric())
#simulare(9,10)
#nr_pierduti2 <- nrow(clienti[clienti$lost == 1,])
sprintf("Numarul de clienti castigati prin prelungirea programului cu o ora %f",nrow(clienti) - nr_clienti_inainte)

clienti <- data.frame(a=numeric(),d=numeric(),s=numeric(),p=numeric(),lost=numeric())
simulare(8,20)
nr_pierduti3 <- nrow(clienti[clienti$lost == 1,])
sprintf("Numarul de clienti castigati prin prelungirea cozi %f",nr_pierduti-nr_pierduti3)
