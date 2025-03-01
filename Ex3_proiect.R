library(shiny)


# Define UI
ui <- navbarPage(
  title = "Funcții de repartiție",
  
  # Input-uri Exercitiul 1 (Normala de parametri 0 si 1)
  tabPanel("Exercitiul 1",
           sidebarLayout(
             sidebarPanel(
               numericInput("n1", "Numărul de valori per variabilă (n):", value = 10, min = 1), #valoare default 10, valoarea minima 1
               numericInput("m1", "Numărul de variabile i.i.d. (m):", value = 1, min = 1), #valoare minima si default 1
               #Pentru toate exercitiile se va proceda in felul urmator pentru alegerea variabilei a care functie de repartitie se va reprezenta
               #Se trimite o lista de "optiuni" pe care utilizatorul le va putea alege dintr-un dropdown. Fiecare valoare pe care user-ul o poate selecta are si o alta denumire "in spate"
               #Astfel, user-ul va avea variante intuitive din care poate alege, in timp ce in cod putem folosi celalta denumire pentru a trata adecvata selectia
               #Ex, user-ul alege ΣXi, iar codul va prelua valoarea sumX cu ajutorul careia va sti ce pasi trebuie parcursi pentru a reprezenta functia
               selectInput("var1", "Alege variabila:",
                           choices = list("X" = "X", "3X - 2" = "3X - 2", "X²" = "X^2",
                                          "ΣXi" = "sumX", "ΣXi²" = "sumX2"))
             ),
             mainPanel(
               plotOutput("plot1") #continutul propriu Exercitiului 1
             )
           )),
  
  # Input-uri Exercitiul 2 (Normala de parametri μ si σ^2)
  tabPanel("Exercitiul 2",
           sidebarLayout(
             sidebarPanel(
               numericInput("n2", "Numărul de valori per variabilă (n):", value = 10, min = 1),
               numericInput("m2", "Numărul de variabile i.i.d. (m):", value = 1, min = 1),
               #Singura diferenta fata de exercitiul anterior este ca acum userul poate selecta media si deviatia standard
               numericInput("mu2", "Media (μ):", value = 0),
               numericInput("sigma2", "Deviația standard (σ):", value = 1, min = 0),
               selectInput("var2", "Alege variabila:",
                           choices = list("X" = "X", "3X - 2" = "3X - 2", "X²" = "X^2",
                                          "ΣXi" = "sumX", "ΣXi²" = "sumX2"))
             ),
             mainPanel(
               plotOutput("plot2") #continutul propriu Exercitiului 2
             )
           )),
  
  # Input-uri Exercitiul 3 (Exponentiala)
  tabPanel("Exercitiul 3",
           sidebarLayout(
             sidebarPanel(
               numericInput("n3", "Numărul de valori per variabilă (n):", value = 10, min = 1),
               numericInput("m3", "Numărul de variabile i.i.d. (m):", value = 1, min = 1),
               #Variabila exponentiala, input pentru parametrul λ>0 (default=1)
               numericInput("lambda3", "Parametrul λ:", value = 1, min = 0),
               selectInput("var3", "Alege variabila:",
                           choices = list("X" = "X", "2 + 5X" = "2 + 5X", "X²" = "X^2",
                                          "ΣXi" = "sumX"))
             ),
             mainPanel(
               plotOutput("plot3") #continutul propriu Exercitiului 3
             )
           )),
  
  # Input-uri Exercitiul 4 (Poisson)
  tabPanel("Exercitiul 4",
           sidebarLayout(
             sidebarPanel(
               numericInput("n4", "Numărul de valori per variabilă (n):", value = 10, min = 1),
               numericInput("m4", "Numărul de variabile i.i.d. (m):", value = 1, min = 1),
               #Variabila repartizata Poisson, input pentru parametrul λ>0 (default=1)
               numericInput("lambda4", "Parametrul λ:", value = 1, min = 0),
               selectInput("var4", "Alege variabila:",
                           choices = list("X" = "X", "3X - 2" = "3X - 2", "X²" = "X^2",
                                          "ΣXi" = "sumX"))
             ),
             mainPanel(
               plotOutput("plot4") #continutul propriu Exercitiului 4
             )
           )),
  
  # Input-uri Exercitiul 5 (Binomiala - r incercari, p probabilitate succes per incercare)
  tabPanel("Exercitiul 5",
           sidebarLayout(
             sidebarPanel(
               numericInput("n5", "Numărul de valori per variabilă (n):", value = 10, min = 1),
               numericInput("m5", "Numărul de variabile i.i.d. (m):", value = 1, min = 1),
               #Input-uri pentru numarul de incercari si probabilitatea de succes 
               #Se putea face probabilitatea cu slider
               numericInput("r5", "Numărul de încercări (r):", value = 5, min = 1),
               numericInput("p5", "Probabilitatea de succes (p):", value = 0.5, min = 0, max = 1),
               selectInput("var5", "Alege variabila:",
                           choices = list("X" = "X", "5X - 4" = "5X - 4", "X³" = "X^3",
                                          "ΣXi" = "sumX"))
             ),
             mainPanel(
               plotOutput("plot5") #continutul propriu Exercitiului 5
             )
           ))
)

# Logica server
server <- function(input, output, session) {
  
  #Tratare cazuri in care avem doar o singura variabila X, respectiv m variabile X i.i.d.
  #In cadrul tuturor exercitiilor updatam lista de functii pe care user-ul o poate selecta
  #Daca m>1, se vor afisa doar functiile tip suma. Altfel, se afiseaza restul
  
  observeEvent(input$m1, { #un fel de event listener in R, verifica valoarea lui m1 si transmite lista adecvata de valori pentru reprezentarea grafica
    if (input$m1 == 1) {
      updateSelectInput(session, "var1", choices = list("X" = "X", "3X - 2" = "3X - 2", "X²" = "X^2"))
    } else {
      updateSelectInput(session, "var1", choices = list("ΣXi" = "sumX", "ΣXi²" = "sumX2"))
    }
  })
  
  #Acelasi princiupiu se aplica si pentru celelalte 4 exercitii
  observeEvent(input$m2, {
    if (input$m2 == 1) {
      updateSelectInput(session, "var2", choices = list("X" = "X", "3X - 2" = "3X - 2", "X²" = "X^2"))
    } else {
      updateSelectInput(session, "var2", choices = list("ΣXi" = "sumX", "ΣXi²" = "sumX2"))
    }
  })
  
  observeEvent(input$m3, { 
    if (input$m3 == 1) {
      updateSelectInput(session, "var3", choices = list("X" = "X", "2 + 5X" = "2 + 5X", "X²" = "X^2"))
    } else {
      updateSelectInput(session, "var3", choices = list("ΣXi" = "sumX"))
    }
  })
  
  observeEvent(input$m4, {
    if (input$m4 == 1) {
      updateSelectInput(session, "var4", choices = list("X" = "X", "3X - 2" = "3X - 2", "X²" = "X^2"))
    } else {
      updateSelectInput(session, "var4", choices = list("ΣXi" = "sumX"))
    }
  })
  
  observeEvent(input$m5, {
    if (input$m5 == 1) {
      updateSelectInput(session, "var5", choices = list("X" = "X", "5X - 4" = "5X - 4", "X³" = "X^3"))
    } else {
      updateSelectInput(session, "var5", choices = list("ΣXi" = "sumX"))
    }
  })
  
  generateCDF <- function(X, variable, dist, params = list(), isDiscrete = FALSE, isSum=FALSE) {
    #Transformarea variabilei X generate in functie de input
    
    #daca vrem ceva de tip suma, adunam valorile celor m variabile independente
    if(isSum){
      X <- rowSums(X)
    }
    else{ #Transformarile liniare pentru X variabila unica
      if (variable == "3X - 2") {
        X <- 3 * X - 2
      } else if (variable == "X^2") {
        X <- X^2
      } else if (variable == "2 + 5X") {
        X <- 2 + 5 * X
      } else if (variable == "X^3") {
        X <- X^3
      } else if (variable == "5X - 4") {
        X <- 5 * X - 4
      }
    }
    
    #Partea care urmeaza este cea cu reprezentarile functiilor de repartitie pt fiecare caz
    
    # plot-uri pt variabilele discrete (Ex 4 si 5)
    if (isDiscrete) {
      unique_vals <- sort(unique(X)) #asta cu unique de X are sens pt ca asa cum am vazut la 5 valorile se pot repeta
      cumulative_probs <- cumsum(table(X)) / length(X) #calculul probabilitatilor cumulative pentru valorile unice pe care le poate lua X
      
      
      plot(unique_vals, cumulative_probs, type = "s", main = paste("Funcția de repartiție cumulativă pentru", variable),
           xlab = "Valori", ylab = "Probabilitate cumulativă", col = "blue", lwd = 2)
    } else {
      
      # plot-uri pt variabilele continue
      #x_vals <- seq(min(X), max(X)) #probabil la fel ca sus, face valorile din X in ordine crescatoate
      x_vals<-X
      if (dist == "normal") { #aici salvez intr-o anumita variabila functia de repartitie adecvata (pnorm sau pexp) care este dupa afisata cu plot
        # y_vals <- pnorm(x_vals, mean = params$mean, sd = params$sd)
        y_vals<-ecdf(x_vals)
      } else if (dist == "exp") {
        #y_vals <- pexp(x_vals, rate = params$rate)
        y_vals<-ecdf(x_vals)
      }
      #In final dupa numeroase teste si exemple am decis sa ne rezumam la functia predefinita ecdf. In continuare avem si codul pentru a calcula efectiv valorile cu ajutorul functiilor specializate pnorm sau pexp 
      plot(y_vals, main=paste("Functia de repartitie cumulativa pentru", variable), verticals=TRUE, col=
             "blue", lwd=2, xlab="Valori", ylab = "Probabilitate cumulativa")
      #plot(x_vals, y_vals, main = paste("Funcția de repartiție cumulativă pentru", variable),
      # xlab = "Valori", ylab = "Probabilitate cumulativă", col = "blue", lwd = 2)
      #lines(x_vals,y_vals, col="blue")
    }
  }
  
  #Paginile de continut pentru fiecare subpunct
  
  #Pagina Exercitiul 1
  output$plot1 <- renderPlot({
    n <- input$n1
    m <- input$m1
    
    if(input$var1 %in% c("sumX", "sumX2")){ #cu ajutorul functie %in% verificam daca variabila ce trebuie reprezentata este de tip suma
      #in acest caz cream cu ajutorul functiei replicate m variabile a cate n valori fiecare. 
      #(Faptul ca folosim replicate ne asigura ca sunt identic distribuite pt ca practic replicam lista de probabilitati)
      X <- replicate(m, rnorm(n, mean=0, sd=1))
      
      #Parametrul isSum va fi util in functia de reprezentare pentru a sti sa adunam valorile variabilelor
      #Also nu sunt chiar sigur daca e ok, dar la toate cazurile cu m valori am inmultit parametrii cu m
      #Explicatie: If each X∼N(μ,σ2)X∼N(μ,σ2), then Sm∼N(mμ,mσ2)Sm​∼N(mμ,mσ2).
      generateCDF(X, input$var1, dist = "normal", params = list(mean = m*0, sd = m*1), isSum = TRUE)
    }
    else{
      X <- rnorm(n, mean = 0, sd = 1)
      generateCDF(X, input$var1, dist = "normal", params = list(mean = 0, sd = 1), isSum = FALSE)
    }
  })
  
  #Pagina Exercitiul 2
  output$plot2 <- renderPlot({
    n <- input$n2
    m <- input$m2
    mu <- input$mu2
    sigma <- input$sigma2
    
    if(input$var2 %in% c("sumX", "sumX2")){
      X <- replicate(m, rnorm(n, mean = mu, sd = sigma))
      #inmultim cu sqrt(m) pt ca practic ar fi m*deviatia^2 => sqrt(m)*deviatia
      generateCDF(X, input$var2, dist = "normal", params = list(mean = m*mu, sd = sqrt(m)*sigma), isSum = TRUE)
    }
    else{
      X <- rnorm(n, mean = mu, sd = sigma)
      generateCDF(X, input$var2, dist = "normal", params = list(mean = mu, sd = sigma), isSum = FALSE)
    }
  })
  
  #Pagina Exercitiul 3
  output$plot3 <- renderPlot({
    n <- input$n3
    m <- input$m3
    lambda <- input$lambda3
    
    if(input$var3 %in% c("sumX")){ #pentru exercitiile 3 4 5 avem doar sumX, nu si suma variabilelor la patrat
      X <- replicate(m, rexp(n, rate = lambda))
      
      #aici am aplicat aceeasi logica ca la restul prin inmultirea lui lambda cu m dar nu suntem total siguri.
      #Am gasit ca varianta corecta ar fi utilizarea unei functii de tip gamma: If each X∼Exp(λ)X∼Exp(λ), then Sm∼Gamma(m,λ)Sm​∼Gamma(m,λ), with a mean of m/λm/λ.
      #generateCDF(X, input$var3, dist = "exp", params = list(shape = m, rate = lambda), isSum = TRUE) #varianta cu gamma
      generateCDF(X, input$var3, dist = "exp", params = list(rate = m*lambda), isSum = TRUE)
    }
    else{
      X <- rexp(n, rate = lambda)
      generateCDF(X, input$var3, dist = "exp", params = list(rate = lambda), isSum = FALSE)
    }
  })
  
  #Pagina Exercitiul 4
  output$plot4 <- renderPlot({
    n <- input$n4
    m <- input$m4
    lambda <- input$lambda4
    
    if(input$var4 %in% c("sumX")){
      X <- replicate(m, rpois(n, lambda = lambda))
      #Xi​∼Pois(λ), Sm​=X1​+X2​+...+Xm​ => Sm​∼Pois(m*λ)
      generateCDF(X, input$var4, dist = "pois", params = list(lambda = m*lambda), isDiscrete = TRUE, isSum = TRUE)
    }
    else{
      X <- rpois(n, lambda = lambda)
      generateCDF(X, input$var4, dist = "pois", params = list(lambda = lambda), isDiscrete = TRUE, isSum = FALSE)
    }
  })
  
  #Pagina Exercitiul 5
  output$plot5 <- renderPlot({
    n <- input$n5
    m <- input$m5
    r <- input$r5
    p <- input$p5
    
    #Pana la urma din ce pricep aici n este nr de valori pe care le poate lua X, in timp ce r reprezinta un numar de incercari, fiecare avand o sanse de succes=p
    #Cele n valori din X sunt din multimea {0,1,2...r}. De ex pt n=10, r=15 si p=0.7 pot avea:
    #X=[6,6,8,2,2,2,6,5,5,4], unde fiecare probabilitate asociata va reprezenta sansele sa avem xi succese din r incercari
    
    if(input$var5 %in% c("sumX")){
      #folosim size=r pt a genera valori intre 0 si r (ca sa aiba sens)
      X <- replicate(m, rbinom(n, size = r, prob = p))
      #Xi​∼Bin(r,p), Sm​=X1​+X2​+...+Xm​ => Sm​∼Bin(m*r,p)
      generateCDF(X, input$var5, dist = "binom", params = list(size = m*r, prob = p), isDiscrete = TRUE, isSum = TRUE)
    }
    else{
      X <- rbinom(n, size = r, prob = p)
      generateCDF(X, input$var5, dist = "binom", params = list(size = r, prob = p), isDiscrete = TRUE, isSum = FALSE)
    }
  })
}

#Lansare aplicatie
shinyApp(ui = ui, server = server)
