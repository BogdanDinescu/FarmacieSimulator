library(shiny)

ui <- fluidPage(
  titlePanel("Simulator Farmacie"),
  fluidRow(
    column(3,
           sliderInput("prog", "Program:",min = 0, max = 24, value = c(8,16))),
    column(3,
           numericInput("qlen",
                        label = "Lungimea cozii",
                        min = 1,
                        step = 1,
                        value = 10)),
    column(3,
           numericInput("nr_simulari",
                        label = "Numarul de simulari",
                        min = 1,
                        step = 1,
                        value = 1)),
    column(3,
           sliderInput("interval_timp",
                       label = "Intervalul de timp pentru medii(Cerinta 2):", 
                       min = 0,
                       max = 24,
                       value = c(8,16)))
  ),
  fluidRow(
    column(12, align="center",
      actionButton("go", "Start")
    )
  ),
  mainPanel(
    textOutput("timp_petrecut_minim_global"),
    textOutput("timp_petrecut_maxim_global"),
    textOutput("timp_petrecut_mediu_global"),
    textOutput("timp_petrecut_minim_server_1"),
    textOutput("timp_petrecut_maxim_server_1"),
    textOutput("timp_petrecut_mediu_server_1"),
    textOutput("timp_petrecut_minim_server_2"),
    textOutput("timp_petrecut_maxim_server_2"),
    textOutput("timp_petrecut_mediu_server_2"),
    textOutput("nr_cl_serv_1"),
    textOutput("nr_cl_serv_2"),
    textOutput("nr_total_med_serviti"),
    verbatimTextOutput("primii_plecati"),
    verbatimTextOutput("medie_clienti_pierduti"),
    verbatimTextOutput("profit_suplimentar"),
    plotOutput("plot_c_serviti"),
    plotOutput("firstPlot"),
    plotOutput("secondPlot"),
    plotOutput("plot2")
  )
)

server <- function(input, output) {
  # Functia care genereaza timpul la care va sosi urmatorul client(aka algoritmul cu inimioara)
  generareTs <- function(s)
  {
    t <- s
    lambda_const <- 23.5
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
  
  # Functia genereaza n variabile aleatoare repartizate exponential folosind metoda inversa(n si lambda alese la apelare)
  f_exponentiala <- function(n, lambda){
    U <- runif(n)
    return(-1/lambda * log(U))
  }
  
  # X~Norm(4,1), genereaza o variabila aleatoare repartizata Norm(4, 1)
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
  
  # genereaza timpul necesar servirii unui client de farmacistul 2(serverul 2) daca n = 1
  G2 <- function(n)
  {
    rez <- sapply(1:n, f_norm_one)
    return(rez)
  }
  
  # Functia de intensitate lambda(t), Pois de parametru lambda
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
  
  # Functia genereaza o variabila aleatoare de repartitie Poisson(lambda)
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
  
  # genereaza n v.a. rep poisson
  pois_N <- function(n,l)
  {
    x <- sapply(1:n, function(x=1) {pois(l)} )
    return(x)
  }
  
  # genereaza timpul necesar servirii unui client de farmacistul 1(serverul 1)
  G1 <- function(n) {
    U <- runif(n)
    return ((-2+sqrt(4+60*U)) / 6 )
  }
  
  # (Pentru Bonus)functia care genereaza cat timp are un client ramdare sa stea in coada
  patience <- function()
  {
    return(runif(1))
  }
  
  # Furnizeaza numarul de clienti
  next_client <- function()
  {
    n <- nrow(clienti)
    return(n)
  }
  
  # Returneaza urmatorul client care sta la coada(care nu a plecat si nu e servit)
  front_queue <- function()
  {
    return(as.numeric(rownames(head(clienti[clienti$s==0 & clienti$d == Inf & clienti$lost == 0,],1))))
  }
  
  # Returneaza timpul la care urmeaza sa plece primul client
  first_to_leave <- function()
  {
    x <- head(clienti[clienti$lost == 0 & clienti$s == 0,][order(clienti$p), ],1)
    if (!is.na(x$p))
      return(x)
    return(0)
  }
  
  # Verifica in ce interval a plecat un client si cu cat se incremnteaza profitul
  check_interval_profit <- function(timp)
  {
    if (timp < 10){
      x <- runif(1, 15, 30)
      profit <<- profit + x
    }
    else if (timp >= 10 && timp <=14) {
      x <- runif(1, 20, 60)
      profit <<- profit + x
    }
    else
    {
      x <- runif(1, 20, 40)
      profit <<- profit + x
    }
  }
  
  # Simuleaza programul farmaciei pentru o ora de deschidere si inchidere date, precum si lungimea cozii tot data
  simulare <- function(start,end,qlen) {
    
    t <- start  # timpul curent
    nr_clienti <- 0
    casa1 <- 0
    casa2 <- 0
    T0 <- generareTs(t)  # generam timpul la care va sosii primul client
    t_A <- T0
    t1 <- t2 <- Inf
    clienti[1,] <<- c(T0, Inf, 0, T0 + patience(), 0)
    
    while (TRUE)
    {
      if (t > end)
      {
        break
      }
      
      # Cazul 0
      # testeaza daca cineva s-a plictisit si nu a ajuns inca la un farmacist(server) pentru a fi servit
      c_plictis <- first_to_leave()
      if (c_plictis != 0 && c_plictis$p <= min(t_A, t1, t2))
      {
        i <- as.numeric(rownames(c_plictis))
        clienti[i,]$lost <<- 1
        nr_clienti <- nr_clienti - 1
      }
      
      # Cazul 1
      # Soseste un client, verificam daca poate fi servit imediat sau
      # intra in coada de asteptare
      if (t_A == min(t_A, t1, t2))
      {
        t <- t_A
        N <- next_client()
        t_A <- generareTs(t)
        clienti[nrow(clienti) + 1,] <<- c(t_A, Inf, 0, p = t_A + patience(), 0)
        flag <- 0
        
        # daca ambele servere sunt libere clientul se duce la primul
        if (nr_clienti == 0)
        {
          nr_clienti <- 1
          casa1 <- N
          casa2 <- 0
          t1 <- t + G1(1)
          clienti[N,]$s <<- 1
          flag <- 1
        }
        # daca serverul 1 este liber clientul il alege
        if (nr_clienti == 1 && casa1 == 0 && flag == 0)
        {
          nr_clienti <- 2
          casa1 <- N
          t1 <- t + G1(1)
          clienti[N,]$s <<- 1
          flag <- 1
        }
        # daca serverul 2 este liber clientul il alege
        if (nr_clienti == 1 && casa2 == 0 && flag == 0)
        {
          nr_clienti <- 2
          casa2 <- N
          t2 <- t + G2(1)
          clienti[N,]$s <<- 2
          flag <- 1
        }
        # daca ambele servere sunt ocupate atunci clientul intra in coada
        if (nr_clienti > 1 && flag == 0)
        {
          if (nr_clienti < qlen) {
            nr_clienti <- nr_clienti + 1
          }
          else {
            clienti <<- clienti[-nrow(clienti),]
          }
          flag <- 1
        }
      }
      
      # Cazul 2
      # Serverul 1 se elibereaza inainte de sosirea unui client nou
      # si inaintea serverului 2
      if (t1 < t_A && t1 <= t2)
      {
        t <- t1
        clienti[casa1,]$d <<- t
        check_interval_profit(t)
        flag2 <- 0
        
        # daca am doar un client, serverul 1 se elibereaza
        if (nr_clienti == 1 && flag2 == 0)
        {
          nr_clienti <- 0
          casa1 <- 0
          casa2 <- 0
          t1 <- Inf
          flag2 <- 1
        }
        # daca am doi clienti eliberez serverul 1
        if (nr_clienti == 2 && flag2 == 0)
        {
          nr_clienti <- 1
          casa1 <- 0
          t1 = Inf
          flag2 <- 1
        }
        if (nr_clienti > 2 && flag2 == 0)
        {
          nr_clienti <- nr_clienti - 1
          casa1 <- front_queue()
          t1 <- t + G1(1)
          clienti[casa1,]$s <<- 1
          flag2 <- 1
        }
      }
      
      # Cazul 3
      # Serverul 2 se elibereaza inaintea serverului 1
      # si inainte de venirea unui nou cleint
      if (t2 < t_A && t2 <= t1)
      {
        t <- t2
        clienti[casa2,]$d <<- t 
        check_interval_profit(t)
        flag3 <- 0
        
        # daca am doar un client, serverul 2 se elibereaza
        if (nr_clienti == 1 && flag3 == 0)
        {
          nr_clienti <- 0
          casa1 <- 0
          casa2 <- 0
          t2 <- Inf
          flag3 <- 1
        }
        # daca am doi clienti eliberez serverul 2
        if (nr_clienti == 2 && flag3 == 0)
        {
          nr_clienti <- 1
          casa2 <- 0
          t2 = Inf
          flag3 <- 1
        }
        if (nr_clienti > 2  && flag3 == 0)
        {
          nr_clienti <- nr_clienti - 1
          casa2 <- front_queue()
          t2 <- t + G2(1)
          clienti[casa2,]$s <<- 2
          flag3 <- 1
        }
      }
    }
  }
  
  observeEvent(input$go, {
    # CERINTA 1
    timp_total_g_min <- list()
    timp_total_g_max <- list()
    timp_total_g_mean <- list()
    timp_total_1_mins <- list()
    timp_total_1_maxs <- list()
    timp_total_1_means <- list()
    timp_total_2_mins <- list()
    timp_total_2_maxs <- list()
    timp_total_2_means <- list()
    # CERINTA 2
    numar_clienti_serviti_server_1 <- list()
    numar_clienti_serviti_server_2 <- list()
    numar_total_clienti_serviti <- list()
    # Cerinta 3
    lista_primii_plecati_fiecare_simulare <- list()
    # CERINTA 4
    lista_nr_clienti_pierduti <- list()
    # CERINTA 5
    lista_profituri <- list()
    # GRAFICE
    clienti_serviti <- list()
    timp_sosire_clienti_serviti <- list()
    timp_sosire_clienti_plecati <- list()
    
    for (nr_s in 1:input$nr_simulari) {
      clienti <<- data.frame(a=numeric(),d=numeric(),s=numeric(),p=numeric(),lost=numeric())
      profit <<- 0
      simulare(input$prog[1],input$prog[2],input$qlen)
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
      
      timp_total_g_min <- append(timp_total_g_min, min(unlist(timp_total)))
      timp_total_g_max <- append(timp_total_g_max, max(unlist(timp_total)))
      timp_total_g_mean <- append(timp_total_g_mean, mean(unlist(timp_total)))
      timp_total_1_mins <- append(timp_total_1_mins, min(unlist(timp_total_1)))
      timp_total_1_maxs <- append(timp_total_1_maxs, max(unlist(timp_total_1)))
      timp_total_1_means <- append(timp_total_1_means, mean(unlist(timp_total_1)))
      timp_total_2_mins <- append(timp_total_2_mins, min(unlist(timp_total_2)))
      timp_total_2_maxs <- append(timp_total_2_maxs, max(unlist(timp_total_2)))
      timp_total_2_means <- append(timp_total_2_means, mean(unlist(timp_total_2)))
      
      numar_clienti_serviti_server_1[nr_s] <- nrow(clienti[clienti$s == 1,])
      numar_clienti_serviti_server_2[nr_s] <- nrow(clienti[clienti$s == 2,])
      numar_total_clienti_serviti[nr_s] <- nrow(clienti[clienti$a>=input$interval_timp[1] & clienti$d<=input$interval_timp[2], ])
      
      lista_primii_plecati_fiecare_simulare[nr_s] <- head(clienti[order(clienti$p) & clienti$lost == 1,],1)$p
      
      lista_nr_clienti_pierduti[nr_s] <- nrow(clienti[clienti$lost == 1,])
      
      lista_profituri[nr_s] <- profit
      
      clienti_serviti[nr_s] <- nrow(clienti[clienti$d != Inf,])
      
      timp_sosire_clienti_serviti <- append(timp_sosire_clienti_serviti, clienti[clienti$lost==0 & clienti$d<Inf,]$a)
      timp_sosire_clienti_plecati <- append(timp_sosire_clienti_plecati, clienti[clienti$lost==1 | clienti$d==Inf,]$a)
    }
    
    # Cerinta 1
    # Afisarea timpului minim, maxim si mediu petrecut de clienti in farmacie,
    # dar si pentru fiecare server in parte
    # Global
    output$timp_petrecut_minim_global <- renderText({
      paste("Timpul minim petrecut de un client in sistem este ", min(unlist(timp_total_g_min)))
    })
    output$timp_petrecut_maxim_global <- renderText({
      paste("Timpul maxim petrecut de un client in sistem este ", max(unlist(timp_total_g_max)))
    })
    output$timp_petrecut_mediu_global <- renderText({
      paste("Timpul mediu petrecut de un client in sistem este ", mean(unlist(timp_total_g_mean)))
    })
    # Server 1
    output$timp_petrecut_minim_server_1 <- renderText({
      paste("Timpul minim petrecut de un client in sistem pt server 1 este ", min(unlist(timp_total_1_mins)))
    })
    output$timp_petrecut_maxim_server_1 <- renderText({
      paste("Timpul maxim petrecut de un client in sistem pt server 1 este ", max(unlist(timp_total_1_maxs)))
    })
    output$timp_petrecut_mediu_server_1 <- renderText({
      paste("Timpul mediu petrecut de un client in sistem pt server 1 este ", mean(unlist(timp_total_1_means)))
    })
    # Server 2
    output$timp_petrecut_minim_server_2 <- renderText({
      paste("Timpul minim petrecut de un client in sistem pt server 2 este ", min(unlist(timp_total_2_mins)))
    })
    output$timp_petrecut_maxim_server_2 <- renderText({
      paste("Timpul maxim petrecut de un client in sistem pt server 2 este ", max(unlist(timp_total_2_maxs)))
    })
    output$timp_petrecut_mediu_server_2 <- renderText({
      paste("Timpul mediu petrecut de un client in sistem pt server 2 este ", mean(unlist(timp_total_2_means)))
    })
    
    # CERINTA 2
    output$nr_cl_serv_1 <- renderText({
      paste("Numarul mediu de clienti serviti de serverul 1 este ", mean(unlist(numar_clienti_serviti_server_1)))
    })
    output$nr_cl_serv_2 <- renderText({
      paste("Numarul mediu de clienti serviti de serverul 2 este ", mean(unlist(numar_clienti_serviti_server_2)))
    })
    output$nr_total_med_serviti <- renderText({
      paste("Numarul mediu total de clienti serviti este ", mean(unlist(numar_total_clienti_serviti)))
    })
    
    # CERINTA 3
    output$primii_plecati <- renderText({
      paste("Primul moment de timp cand este pierdut un client(pentru fiecare simulare) este", lista_primii_plecati_fiecare_simulare)
    }, sep = "\n")
    
    # CERINTA 4
    output$medie_clienti_pierduti <- renderText({
      paste("Numarul mediu de clienti pierduti ", mean(unlist(lista_nr_clienti_pierduti)))
    })
    
    # CERINTA 5
    if (exists("profit_nou"))
      profit_vechi <<- profit_nou
    else
      profit_vechi <<- 0
    
    profit_nou <<- mean(unlist(lista_profituri))
    
    output$profit_suplimentar <- renderText({
      paste("Castigul mediu este ", round(profit_nou, 2),
            "\nCastigul mediu vechi este ",round(profit_vechi,2),
            "\nCastigul mediu suplimentar este ",round(profit_nou - profit_vechi,2))
    })
    
    # GRAFICE
    output$plot_c_serviti <- renderPlot({
      plot(1:input$nr_simulari, clienti_serviti, type = 'l', xlab = "Indicele simularii", ylab ="Numarul de clienti serviti", col = 'green', border = 'white')
      title("Numarul de clienti serviti pentru fiecare simulare")
    })
    
    output$firstPlot <- renderPlot({
      plot(1:input$nr_simulari, lista_nr_clienti_pierduti, type = 'l', xlab = "Indicele simularii", ylab ="Numarul de clienti pierduti", col = 'red', border = 'white')
      title("Numarul de clienti pierduti pentru fiecare simulare")
    })
    
    output$secondPlot <- renderPlot({
      par(mfrow=c(1,2))
      hist(unlist(timp_sosire_clienti_serviti),type="p", xlab = "", col="green", main = "Clienti serviti cu succes")
      hist(unlist(timp_sosire_clienti_plecati),type="p", xlab = "", col="red", main = "Clienti plecati")
    })
    
    output$plot2 <- renderPlot({
      plot(unlist(numar_clienti_serviti_server_1),type="l",col="blue",ylab="clienti serviti de 1 si 2", ylim = c(0, 30))
      lines(unlist(numar_clienti_serviti_server_2),type="l",col="orange",ylab="clienti serviti de 2")
    })
  })
}

shinyApp(ui = ui, server = server)
