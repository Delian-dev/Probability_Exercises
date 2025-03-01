# Se consideră o activitate care presupune parcurgerea secvențială a n etape
# Timpul necesar finalizării etapei i de către o persoană A este o variabilă aleatoare 
# T_i ~ Exp(lambda_i). După finalizarea etapei i, A va trece in etapa i+1 cu probabilitatea
# alpha_i, sau va opri lucrul cu probabilitatea 1-alpha_i.Fie T timpul total petrecut de 
# persoana în realizarea activității respective.

# 1. Construiti un algoritm care simuleaza 10^6 valori pentru T si in baza acestora
# aproximati E[T]. Reprezentati grafic intr-o maniera adecvata valorilor obtinute pentru T.
# Ce puteti spune despre repartitia lui T?

#Parametrii
n <- 10 #numarul de etape
lambda <- c(1, 2, 0.5, 3, 2, 1.5, 2.1, 1.8, 1.2, 1.5) #parametrii distributiei exponentiale
alpha <- c(0.9, 0.4, 0.2, 0.5, 0.6, 0.7, 0.8, 0.9, 0.3) #probabilitatile de a trece la etapa urmatoare
N <- 10^6 #numarul de simulari

simulare_T <- function(n, lambda, alpha){
  timp_total <- 0
  for(i in 1:n){
    timp_etapa <- rexp(1, rate = lambda[i]) #generam timpul necesar pentru etapa i
    timp_total <- timp_total + timp_etapa
    etapa_finala <- i
    if(runif(1) > alpha[i] & i < n){ #daca nu trecem la etapa urmatoare
      break
    }
  }
  return(c(timp_total, etapa_finala))
}

rezultate <- replicate(N, simulare_T(n, lambda, alpha))
medie_estimata <- mean(rezultate[1,])
cat("E[T] estimat:", medie_estimata, "\n")

hist(rezultate[1,], breaks = 50, col = "lightblue", main = "Histograma lui T", xlab = "T")

# 2. Calculati E[T] si comparati rezultatul cu cel obtinut prin simulare.
medie_teoretica <- sum(sapply(2:n, function(i) {
  (1 / lambda[i]) * prod(alpha[1:(i-1)])
})) + (1 / lambda[1])
cat("E[T] teoretic:", medie_teoretica, "\n")
# Se observa ca media teoretica si cea estimata prin simulare sunt apropiate.

# 3. In baza simularilor de la 1) calculati probabilitatea ca persoana A sa finalizeze activitatea
probabilitate_finalizare <- sum(rezultate[2,] == n) / N
cat("Probabilitatea ca persoana A sa finalizeze activitatea:", probabilitate_finalizare, "\n")

# 4. In baza simularilor de la 1) calculati probabilitatea ca persoana A sa finalizeze activitatea
# in mai putin de s unitati de timp.
probabilitate_finalizare_s <- function(s, rezultate){
  return(sum(rezultate[1,] < s) / N)
}
cat("Probabilitatea ca persoana A sa finalizeze activitatea in mai putin de 5 unitati de timp:", 
    probabilitate_finalizare_s(1, rezultate), "\n")

# 5. Determinati timpul minim si maxim + rep grafica
timp_minim <- min(rezultate)
timp_maxim <- max(rezultate)
cat("Timpul minim:", timp_minim, "\n")
cat("Timpul maxim:", timp_maxim, "\n")

plot(density(rezultate[1,]), main = "Densitatea lui T", xlab = "T", col = "red")

# 6. Aproximati probabilitate ca A sa se opreasca inainte de etapa k
# Reprezentati grafic probabilitatile obtinute pentru 1<k<=n
probabilitate_oprire_etapa_k <- function(k, rezultate){
  return(sum(rezultate[2,] < k) / N)
}

for(k in 2:n){
  cat("Probabilitatea ca A sa se opreasca inainte de etapa", k, ":", 
      probabilitate_oprire_etapa_k(k, rezultate), "\n")
}

probabilitati_oprire <- sapply(2:n, function(k) probabilitate_oprire_etapa_k(k, rezultate))
plot(2:n, probabilitati_oprire, type = "b", col = "blue", main = "Probabilitatea opririi inainte de etapa k", xlab = "k", ylab = "Probabilitate")

#Cresc in functie de probabilitatile alpha_i
#In functie de cum sunt acestea cresterea este mai abrupta sau mai lina



