install.packages("shiny")
install.packages("bslib")
install.packages("gganimate")

#librariile folosite
library(shiny) 
library(bslib)
#shiny si bslib pentru layout, cards, design etc
library(ggplot2)
#pentru grafice
library(gganimate)
#pentru animatii



ui <- page_navbar(
  
  #un navigation panel pentru fiecare formulare, 6 pagini, cate una pentru cele 5 formulari + relevanta negative binomiale
  
  #fiecare pagina la formulari este structurata pe 2 card-uri:
  #primul contine input-urile, cele 2 input-uri fixate si apoi un slider pentru a specifica de unda sa inceapa si unde sa se termine cel variabile
  #exemplu(pt formula 1 unde X = numarul de esecuri stiind r succese): 
  #avem un input pentru probabilitatea succesului de la 0 la 1
  #unul pentru numarul de succese de la 0 la 100
  #unu slider pentru esecuri unde putem alege intervalul lui k(numarul de esecuri)
  #astfel construim functia de masa pentru valurile k intre invervalul ales ( P(x == k) )
  #al doilea contine 2 card-uri, unul pentru functia de masa si unul pentru cea de repartitie
  #slider-urile sunt numite slider1|2|3 ( primul probabilitate, al doilea partea variabila, al treilea cea fixa) + 1/2/3/4/5(reprezentand numarul repartitie corespunzatoare)
  
  #de asemenea input-urile fixe au si posibilitatea de a trece automat prin ele intr-un interval dat (in general 100 secunde pentru probabilitati si 50 pentru celalalt)
  #acest lucru va merita vizualizarea live a modificariilor functiei de repartitie/masa cand se modifica probabilitatea/celalalt parametru fix -> animatia
  
  #De asemenea pentru fiecare card cu functia de masa/repartitie folosim numele plotMasa/plotRep + 1|2|3|4|5 ( numarul functiei respective ) pentru a face conexiunea cu server-ul
  
  #formula 1 : variabila X = nr de esecuri avand r succese
  nav_panel("k esecuri, r succese",
            layout_columns( 
              card( 
                
                sliderInput("slider11", "Probabilitate succes", 
                            min = 0, max = 1, value = 0.5 ,
                            step=0.01,
                            animate = animationOptions(interval = 100, loop = FALSE)),
                sliderInput( 
                  "slider21", "Slider esecuri", 
                  min = 0, max = 100, 
                  value = c(0,100)
                ),
                
                sliderInput("slider31", "Numar succese", 
                            min = 0, max = 100, value = 100, step=1, 
                            animate=animationOptions(interval=50, loop=FALSE) ), 
              ),
              card( 
                "Functia de masa si functia de repartitie", 
                layout_columns( 
                  card(plotOutput("plotMasa1")), 
                  card(plotOutput("plotRep1")) 
                ) 
              ), 
              col_widths = c(4, 8) 
            ) 
  ),
  #formula 2: X= numarul de succese avand n incercari
  nav_panel("n incercari, r succese", 
            layout_columns( 
              card( 
                sliderInput("slider12", "Probabilitate succes", 
                            min = 0, max = 1, value = 0.5,
                            step=0.01,
                            animate = animationOptions(interval = 100, loop = FALSE)), 
                sliderInput( 
                  "slider22", "Slider incercari", 
                  min = 0, max = 100, 
                  value = c(0,100) 
                ),
                
                sliderInput("slider32", "Numar succese", 
                            min = 0, max = 100, value = 100, step=1, 
                            animate=animationOptions(interval=50, loop=FALSE)), 
              ),
              card( 
                "Functia de masa si functia de repartitie", 
                layout_columns( 
                  card(plotOutput("plotMasa2")), 
                  card(plotOutput("plotRep2")) 
                ) 
              ), 
              col_widths = c(4, 8) 
            )
  ),
  #formula 3: X=numarul de incercari avand r esecuri
  nav_panel("n incercari, r esecuri", 
            layout_columns( 
              card( 
                sliderInput("slider13", "Probabilitate succes", 
                            min = 0, max = 1, value = 0.5,
                            step=0.01,
                            animate = animationOptions(interval = 100, loop = FALSE)), 
                sliderInput( 
                  "slider23", "Slider incercari", 
                  min = 0, max = 100, 
                  value = c(0,100) 
                ),
                
                sliderInput("slider33", "Numar esecuri", 
                            min = 0, max = 100, value = 100, step=1, 
                            animate=animationOptions(interval=50, loop=FALSE)), 
              ),
              card( 
                "Functia de masa si functia de repartitie", 
                layout_columns( 
                  card(plotOutput("plotMasa3")), 
                  card(plotOutput("plotRep3")) 
                ) 
              ), 
              col_widths = c(4, 8) 
            )),
  #formula 4: numarul de succese avand r esecuri
  nav_panel("k succese, r esecuri" , 
            layout_columns( 
              card( 
                sliderInput("slider14", "Probabilitate succes", 
                            min = 0, max = 1, value = 0.5,
                            step=0.01,
                            animate = animationOptions(interval = 100, loop = FALSE)), 
                sliderInput( 
                  "slider24", "Slider succese", 
                  min = 0, max = 100, 
                  value = c(0,100) 
                ),
                
                sliderInput("slider34", "Numar esecuri", 
                            min = 0, max = 100, value = 100, step=1, 
                            animate=animationOptions(interval=50, loop=FALSE)), 
              ),
              card( 
                "Functia de masa si functia de repartitie", 
                layout_columns( 
                  card(plotOutput("plotMasa4")), 
                  card(plotOutput("plotRep4")) 
                ) 
              ), 
              col_widths = c(4, 8) 
            )),
  #formula 5: X= numarul de succese avand n incercari
  nav_panel("k succese, n incercari" , 
            layout_columns( 
              card( 
                sliderInput("slider15", "Probabilitate succes", 
                            min = 0, max = 1, value = 0.5,
                            step=0.01,
                            animate = animationOptions(interval = 100, loop = FALSE)), 
                sliderInput( 
                  "slider25", "Slider succese", 
                  min = 0, max = 110, 
                  value = c(0,100) 
                ),
                
                sliderInput("slider35", "Numar incercari", 
                            min = 0, max = 100, value = 100, step=1, 
                            animate=animationOptions(interval=50, loop=FALSE)), 
              ),
              card( 
                "Functia de masa si functia de repartitie", 
                layout_columns( 
                  card(plotOutput("plotMasa5")), 
                  card(plotOutput("plotRep5")) 
                ) 
              ), 
              col_widths = c(4, 8) 
            )),
  
  nav_panel("Utilizari NB",
            card(
              card_header("Utilizarea repartitiei Negativ Binomiala:"),
              p("Exemple de utilizare în viața reală:"),
              tags$ul(
                tags$li("In astrofizica - se foloseste pentru predictii asupra numarului de galaxii intr-o regiune a spatiului."),
                tags$li("In domeniul Asigurarilor si Riscului - ajuta la modelarea numarului de pierderi/castiguri pe care o firma o poate atinge pana la aparitia unui anumit nivel de profit"),
                tags$li("In domeniul Marketing-ului - este utilizata pentru a prezice numrul de incercari (spre exemplu, evenimente promotional) de care este nevoie pentru a atinge un numar dorit de raspunuri (engagement din partea consumatorilor)"),
                tags$li("In  controlul calitatii - prin intermediul sau se calculeaza numarul de elemente defecte produse pana la atingerea unui numar acceptabil de elemente, fapt ce faciliteaza standardizarea si optimizarea procesului de productie"),
                tags$li("In studiul Biologiei si Biodiversitatii - aici, Negativ Binomiala are multiple utilizari: de la ramura geneticii, unde modeleaza date ale ARN-ului si ADN-ului, la epidemiologie, unde studiaza bolile infectioase (evenimentele de “super-spreading”). De asemenea, ajuta la calculul probabilitatii de disparitie a anumitor specii (spre exemplu, “Cate incercari de reproducere apar pana la nasterea a n urmasi sanatosi?”)")
              )
              
            )
  )
  
)


#am decis, pentru a vedea cu mai mare usurinta forma functiilor de masa/repartitie sa unim punctele printr-o linie

#pentru functia de repartitie, toate sunt calculate intr-un mod asemanator

server <- function(input, output, session) {
  
  
  #formula 1 : variabila X = nr de esecuri avand r succese
  output$plotMasa1<-renderPlot(
    {
      formula_unu <- function(k, r, p) # k-esec r-succes
      {
        return(choose(k+r-1, k) * ((1-p)^k) * (p^r))
      } #calculeaza probabilitatea sa avem k esecuri stiinf ca am avut r succese iar probabilitatea unui succes este p
      
      p<-input$slider11  # probabilitatea p
      k<-input$slider21[1]:input$slider21[2] # range-ul pentru esecuri
      r<-input$slider31 # numarul de succese
      
      plot(k, formula_unu(k, r,p),main="Functia de masa", xlab="k = numarul de esecuri",
           ylab=" probabilitatea sa avem k esecuri", col="red")
      lines(k, formula_unu(k, r,p),lwd = 2, col="red")
      
    })
  output$plotRep1<- renderPlot(
    {
      formula_unu <- function(k, r, p) # k-esec r-succes
      {
        return(choose(k+r-1, k) * ((1-p)^k) * (p^r))
      }#calculeaza probabilitatea sa avem k esecuri stiinf ca am avut r succese iar probabilitatea unui succes este p
      
      
      p<-input$slider11 # probabilitatea p
      k<-input$slider21[1]:input$slider21[2] # range-ul pentru esecuri
      r<-input$slider31 # numarul de succese
      formula_unu_rep <- function(x, r, p)
      {
        if(x<0)
        {
          #daca numarul de esecuri e mai mic ca 0 => imposibil
          return(0)
        }
        else
        {
          #suma pentru 0<=i<=x din P(X=i) => P(X<=x)
          return(sum(formula_unu(0:x,r,p)))
        } 
      }
      x<-k
      #facem un vector de lungimea range-ului primit de la input in care rezultat[i] = P(X<=i)
      results<-numeric(length(x)) 
      for(i in 1:length(x))
      {
        results[i] <- formula_unu_rep(x[i], r,p)
      }
      
      plot(x, results, ylim = c(0, 1),main="Functia de repartitie", xlab=" k = numarul de esecuri",
           ylab=" probabilitatea sa avem cel mult k esecuri", col="blue", type="s")
      #lines(x, results,lwd = 2, col="blue")
    })
  
  #formula 2: variabila X = nr de incercari avand r succese
  
  output$plotMasa2<-renderPlot(
    {
      formula_doi <- function(n, r, p) #n - incercari r-succes
      {
        if(n<r)
        {
          # daca numarul de incercari este mai mic decat numarul de succese => imposibil 
          # avem cel putin r incercari
          return(0)
        }
        else
        {
          return( choose(n-1, r-1)*(p^r) * ((1-p)^(n-r)))
        }
      }
      
      p<-input$slider12 #probabilitatea p
      r<-input$slider32 #numarul r de succese
      n<-input$slider22[1]:input$slider22[2] #range-ul numarului de incercari
      #de data aceasta pentru ca avem conditia de if in calculul functiei de masa trebuie sa procedam ca la functia de repartitie
      results <-numeric(length(n))
      for(i in 1:length(n))
      {
        results[i] = formula_doi(n[i], r, p)
      }
      
      plot(n, results,main="Functia de masa", xlab="n = numarul de incercari",
           ylab=paste(" probabilitatea sa avem n incercari pana la primele",r,"succese", sep=" "), col="red")
      lines(n, results,lwd = 2, col="red")
      
      
    })
  output$plotRep2<- renderPlot(
    {
      formula_doi <- function(n, r, p) #n - incercari r-succes
      {
        if(n<r)
        {
          return(0)
        }
        else
        {
          return( choose(n-1, r-1)*(p^r) * ((1-p)^(n-r)))
        }
      }
      
      p<-input$slider12 #  probaiblitatea
      r<-input$slider32 # numarul de succese
      n<-input$slider22[1]:input$slider22[2] # numarul de incercari
      
      formula_doi_rep <- function(x, r, p)
      {
        sum<-0
        for(i in 0:x){
          sum<-sum+formula_doi(i, r, p)}
        return(sum)
      } # functia de repartitie
      
      x<-n
      results<-numeric(length(x))
      for(i in 1:length(x))
      {
        results[i] <- formula_doi_rep(x[i], r,p)
      }
      
      plot(x, results, ylim = c(0, 1), type="s",main="Functia de repartitie", xlab="numarul de esecuri",
           ylab=paste(" probabilitatea sa avem cel mult n esecuri pana avem ", r, " succese", sep=""), col="blue")
      #lines(x, results,lwd = 2, col="blue")
      
    })
  
  #formula 3 -> X= numatul de incercari stiinf r esecuri
  output$plotMasa3<-renderPlot(
    {
      formula_trei <- function(n,r, p) #n-incercari r-esec
      {
        if(n<r)
        {
          #nu putem avea mai putine incercari decat esecuri
          return(0)
        }
        else
        {
          return( choose(n-1, r-1) * (p^(n-r)) * ((1-p)^r)  ) 
        }
      }
      
      p<-input$slider13 # probabilitate
      r<-input$slider33 # esecuri
      n<-input$slider23[1]:input$slider23[2]  # range-ul incercarilor
      
      results <- numeric(length(n))
      for(i in 1:length(n))
      {
        results[i] = formula_trei(n[i], r, p)
      }
      plot(n, results,main="Functia de masa", xlab="n=numarul de incercari",
           ylab=" probabilitatea sa avem n incercari", col="red")
      lines(n, results,lwd = 2, col="red")
      
      
    })
  output$plotRep3<- renderPlot(
    {
      formula_trei <- function(n,r, p) #n-incercari r-esec
      {
        if(n<r)
        {
          return(0)
        }
        else
        {
          return( choose(n-1, r-1) * (p^(n-r)) * ((1-p)^r)  ) 
        }
      }
      
      p<-input$slider13 # probabilitate
      r<-input$slider33 # escurile
      n<-input$slider23[1]:input$slider23[2] # range-ul incercarilor
      
      formula_trei_rep <- function(n, r, p)
      {
        sum<-0
        for(i in 0:n)
        {
          sum<-sum+formula_trei(i, r, p)
        }
        
        return(sum)
      }
      
      x<-input$slider23[1]:input$slider23[2]
      results<-numeric(length(x))
      for(i in 1:length(x))
      {
        results[i] <- formula_trei_rep(x[i], r,p)
      }
      plot(x, results, ylim = c(0, 1),main="Functia de repartitie", xlab="n=numarul de incercari",
           ylab=" probabilitatea sa avem cel mult n incercari", col="blue", type="s")
      # lines(x, results,lwd = 2, col="blue")
      
    })
  
  #formula 4: X=numarul de succese stiind r incercari
  output$plotMasa4<-renderPlot(
    {
      formula_patru <- function(k,r,p) #k-succes r-esec
      {
        return( choose( k+r-1, k ) * (p^k) * ((1-p)^r) )
      }
      
      p<-input$slider14 #  probabilitatea
      r<-input$slider34 #  incercarile
      k<-input$slider24[1]:input$slider24[2] # numarul de succese
      
      
      
      plot(k, formula_patru(k, r, p),main="Functia de masa", xlab="k=numarul de succese",
           ylab=" probabilitatea sa avem k succese", col="red")
      lines(k, formula_patru(k, r, p),lwd = 2, col="red")
      
      
    })
  output$plotRep4<- renderPlot(
    {
      formula_patru <- function(k,r,p) #k-succes r-esec
      {
        return( choose( k+r-1, k ) * (p^k) * ((1-p)^r) )
      }
      
      p<-input$slider14 # probabilitatea
      r<-input$slider34 # esecurile
      k<-input$slider24[1]:input$slider24[2] #numarul de succese
      
      formula_patru_rep <- function(x, r, p)
      {
        if(x<0)
        {
          return(0)
        }
        else
        {
          return(sum(formula_patru(0:x,r,p)))
        } 
      }
      
      x<-k
      results<-numeric(length(x))
      for(i in 1:length(x))
      {
        results[i] <- formula_patru_rep(x[i], r,p)
      }
      plot(x, results, ylim = c(0, 1),main="Functia de repartitie", xlab="k=numarul de succese",
           ylab=" probabilitatea sa avem cel mult k succese", col="blue", type="s")
      #lines(x, results,lwd = 2, col="blue")
      
    })
  
  #formula 5 : X=numarul de succese stiind n incercari
  output$plotMasa5<-renderPlot(
    {
      formula_cinci <- function(k,n,p) #k-succes n-incercari
      {
        if(k>n)
        {
          #nu putem avea mai multe succese decat incercari
          return(0)
        }
        else
        {
          return( choose( n, k ) * (p^k) * ((1-p)^(n-k)) )
        }
      }
      
      p<-input$slider15 # probabilitatea
      n<-input$slider35 # numarul de incercari
      k<-input$slider25[1]:input$slider25[2] # range pentru succese
      
      results <-numeric(length(k))
      for(i in 1:length(k))
      {
        results[i] = formula_cinci(k[i], n, p)
      }
      
      plot(k, results, xlab="k=numarul de succese",
           ylab=paste("probabilitatea sa avem k succese cu", n, "incercari" ,sep = " "), col="red")
      lines(k, results,lwd = 2, col="red")
      
      
    })
  output$plotRep5<- renderPlot(
    {
      formula_cinci <- function(k,n,p) #k succese, n incercari
      {
        if(k>n)
        {
          return(0)
        }
        else
        {
          return( choose( n, k ) * (p^k) * ((1-p)^(n-k)) )
        }
      }
      
      p<-input$slider15 # probabilitatea
      n<-input$slider35 # numarul de incercari
      k<-input$slider25[1]:input$slider25[2] # range pentru succese
      
      
      formula_cinci_rep <- function(x, n, p)
      {
        suma<-0
        for(i in 0:x)
        {
          suma<-suma+formula_cinci(i, n, p)
        }
        return(suma)
      } 
      
      x<-k
      results<-numeric(length(x))
      for(i in 1:length(x))
      {
        results[i] <- formula_cinci_rep(x[i], n,p)
      }
      plot(x, results,main="Functia de repartitie", xlab="k= numarul de succese",
           ylab=paste("probabilitatea sa avem k succese cu", n, "incercari", sep = " "), col="blue", type="s")
      #lines(x, results,lwd = 2, col="blue")
      
    })
  
}




shinyApp(ui, server)



