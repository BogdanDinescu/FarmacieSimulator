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

t <- N_A <- C1 <- C2 <- 0
SS <- c(0,0,0,0)
T0 <- generareTs(t)
t_A <- T0
t1 <- t2 <- t_P <- Inf

A <- list()
D1 <- list()
D2 <- list()

while (TRUE)
{
  if (t > 8)
  {
    break
  }
  print(SS)
  # Cazul 0
  if (t_P == min(t_A,t1,t2,t_P))
  {
      t <- t_P
      if (SS[1] > 1) {
        SS[1] <- SS[1] - 1
        N_A <- N_A - 1
        print("A plecat")
        print(N_A)
      }
      t_P <- Inf
      next
  }
  
  # Cazul 1
  # Soseste un client, verificam daca poate fi servit imediat sau
  # intra in coada de asteptare
  if (t_A == min(t_A,t1,t2))
  {
    t <- t_A
    N_A <- N_A + 1
    T_t <- generareTs(t)
    t_A <- T_t
    A[N_A] <- t
    
    # daca ambele servere sunt libere clientul se duce la primul
    if (SS[1] == 0)
    {
      SS <- c(1,N_A,0)
      Y1 <- G1(1)
      t1 <- t + Y1
      next
    }
    # daca serverul 2 este liber clientul il alege
    if (SS[1] == 1 && SS[3] == 0)
    {
      SS[1] <- 2
      SS[3] <- N_A
      Y2 <- G2(1)
      t2 <- t + Y2
      next
    }
    # daca serverul 1 este liber clientul il alege
    if (SS[1] == 1 && SS[2] == 0)
    {
      SS[1] <- 2
      SS[2] <- N_A
      Y1 <- G1(1)
      t1 <- t + Y1
      next
    }
    # daca ambele servere sunt ocupate atunci clientul intra in coada
    if (SS[1] > 1)
    {
      if (SS[1] < 10) {
        SS[1] <- SS[1] + 1
        t_P <- t + 0.1
      } else
        N_A <- N_A - 1
    }
  }
  
  # Cazul 2
  # Serverul 1 se elibereaza inainte de sosirea unui client nou
  # si inaintea serverului 2
  if (t1 < t_A && t1 <= t2)
  {
    t <- t1
    C1 <- C1 + 1
    D1[SS[2]] <- t
    t_P <- Inf
    
    # daca am doar un client, serverul 1 se elibereaza
    if (SS[1] == 1)
    {
      SS <- c(0,0,0)
      t1 <- Inf
      next
    }
    # daca am doi clienti eliberez serverul 1
    if (SS[1] == 2)
    {
      SS[1] <- 1
      SS[2] <- 0
      t1 = Inf
      next
    }
    if (SS[1] > 2)
    {
      m = max(SS[2],SS[3])
      SS[1] <- SS[1] - 1
      SS[2] <- m + 1
      Y1 <- G1(1)
      t1 <- t + Y1
    }
  }
  
  # Cazul 3
  # Serverul 2 se elibereaza inaintea serverului 1
  # si inainte de venirea unui nou cleint
  if (t2 < t_A && t2 < t1)
  {
    t <- t2
    C2 <- C2+1
    D2[SS[3]] <- t
    t_P <- Inf
    
    if (SS[1] == 1)
    {
      SS <- c(0,0,0)
      t2 <- Inf
      next
    }
    if (SS[1] == 2)
    {
      SS[1] <- 1
      SS[3] <- 0
      t2 = Inf
      next
    }
    if (SS[1] > 2)
    {
      m = max(SS[2],SS[3])
      SS[1] <- SS[1] - 1
      SS[3] <- m + 1
      Y2 <- G2(1)
      t2 <- t + Y2
    }
  }
}

timp_total <- list()
timp_total_1 <- list()
timp_total_2 <- list()
for (i in 1:length(A)) {
  if ( !is.null( unlist(D1[i])) )
    timp_total[i] <- timp_total_1[i] <- (D1[[i]] - A[[i]])
  
  if ( !is.null( unlist(D2[i])) ) 
    timp_total[i] <- timp_total_2[i] <- (D2[[i]] - A[[i]])
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

sprintf("Numarul mediu de clienti serviti este %f",(C1+C2)/2)
sprintf("Numarul mediu de clienti serviti de server 1 este %f", C1/2)
sprintf("Numarul mediu de clienti serviti de server 2 este %f", C2/2)
