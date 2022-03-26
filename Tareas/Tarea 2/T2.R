# TAREA 2

# Funciones auxiliares
library(ggplot2)
library(plotly)
library(scales)
library(tidyverse)

##########################################

# CAPITULO 8

# P2

Fn_hat <- function(x0, xn) {
  mean(xn <= x0)
}

Fn_hat_gen <- function(x, xn){
  m <- length(x)
  if(m > 1){
    yhat <- Fn_hat(x[1], xn)
    for(i in 2:m) {
      yhat <- c(yhat, Fn_hat(x[i], xn))
    }
  }else{
    yhat <- Fn_hat(x, xn)
  }
  yhat
}

x = seq(-5, 5, length.out = 1000)
y = pnorm(x)
n = 100
alpha = .05
epsilon = sqrt((1/(2*n))*log(2/alpha))
contador_norm = 0

for(i in 1:1000){
  xn = rnorm(n)
  yhat = Fn_hat_gen(x, xn)
  Lx = yhat - epsilon
  Ux = yhat + epsilon
  if(all(Ux > y) & all(c(Lx < y))){
    contador_norm = contador_norm + 1
  }
}

y = pcauchy(x)
contador_cauchy = 0

for(i in 1:1000){
  xn = rcauchy(n)
  yhat = Fn_hat_gen(x, xn)
  Lx = yhat - epsilon
  Ux = yhat + epsilon
  if(all(Ux > y) & all(c(Lx < y))){
    contador_cauchy = contador_cauchy + 1
  }
}

print(paste('Qued? dentro de la banda', contador_norm, 'veces (Normal)'))
print(paste('Qued? dentro de la banda', contador_cauchy, 'veces (Cauchy)'))
#------------------------------------------------------------------------------
# Estimador plug-in asimetr?a
my_skewness<- function(x){
  x_bar <- mean(x)
  d <- x - x_bar
  m3 <- mean(d^3)
  m4 <- mean(d^2)^(3/2)
  m3/m4
}

n = 50
alpha = .05
B = 100000
valor_real = (exp(1)+2)*sqrt(exp(1)-1)
N = 1000

cobertura_normal = rep(F, N)
cobertura_cuantiles = rep(F, N)
cobertura_pivotal = rep(F, N)

for(i in 1:N){
  Y = rnorm(n)
  X = exp(Y)
  estimador = my_skewness(X)
  
  estimador_estrella = rep(NA, B) 
  for (t in 1:B) {
    xn_estrella = sample(X, size = n, replace = TRUE)
    estimador_estrella[t] = my_skewness(xn_estrella)
  }
  
  # Intervalo asint?tico Normal
  sd_bootstrap = sqrt((1/B)*sum((estimador_estrella - mean(estimador_estrella))^2))
  intervalo_normal = estimador + c(-1, 1)*qnorm(1-alpha/2)*sd_bootstrap
  if(intervalo_normal[1] < valor_real & valor_real < intervalo_normal[2]){
    cobertura_normal[i] = T
  }
  
  # Intervalo cuantiles
  intervalo_cuantiles = unname(quantile(estimador_estrella, probs = c(alpha/2, 1 - alpha/2)))
  if(intervalo_cuantiles[1] < valor_real & valor_real < intervalo_cuantiles[2]){
    cobertura_cuantiles[i] = T
  }
  
  # Intervalo Pivotal
  intervalo_pivotal = c(2*estimador-intervalo_cuantiles[2],2*estimador-intervalo_cuantiles[1])
  if(intervalo_pivotal[1] < valor_real & valor_real < intervalo_pivotal[2]){
    cobertura_pivotal[i] = T
  }
}

print(paste('Cobertura normal', sum(cobertura_normal)/N))
print(paste('Cobertura cuantiles', sum(cobertura_cuantiles)/N))
print(paste('Cobertura pivotal', sum(cobertura_pivotal)/N))

# P7

# Vamos a generar un conjunto de datos de tamaño 50 con $\theta=1$
# Bajo la distribución uniforme:

dt <- runif(50, 0, 1)

# (a) Encuentra la distribución de $\widehat{\theta}$. 
# Compara la verdadera distribución de $\widehat{\theta}$ con los histogramas 
#del bootstrap.

t.hat <- max(dt)

# FUNCIÓN DE THETHA HAT

T.hat <- function(x){
  if(x <= 0){
    return(0)
  }
  if(x>= 1){
    return(1)
  }else{
    return(x**50)
  }
}

x <- seq(0, 1, 0.01)

pdf('/Volumes/GoogleDrive/Mi unidad/Universidad/Ciencia de Datos/6to Semestre/2022_1-Metodos_Estadisticos/Tareas/Tarea 2/Graphs/Dist.pdf',
    width = 7,
    height = 5)

plot(x, sapply(x, T.hat), type = 's', 
     col = '#FF8E3F', lwd = 2.5, main = 'Distribución de Theta', xlim=c(0.8,1), 
     xlab = 'x', ylab = 'Densidad', 
     col.axis = "#5F5F5F", col.lab = "#5F5F5F", fg = "#5F5F5F")

dev.off()


# BOOSTRAP

# No parámetrico
B <- 10000

Tboot <- rep(NULL, B)
n <- length(dt)
for(i in 1:B){
  Xstar <- sample(dt, n, replace = TRUE)
  Tboot[i] <- max(Xstar)
}

se <- sd(Tboot)


pdf('/Volumes/GoogleDrive/Mi unidad/Universidad/Ciencia de Datos/6to Semestre/2022_1-Metodos_Estadisticos/Tareas/Tarea 2/Graphs/Hist_No-P.pdf',
    width = 5,
    height = 5)

hist(Tboot, #breaks = 'FD', 
     probability = TRUE,
     col = '#FF8E3F', main = '10, 000 Boostrap No-Paramétrco',
     border = 'white', xlab = 'x', ylab = 'Densidad', 
     col.axis = "#5F5F5F", col.lab = "#5F5F5F", fg = "#5F5F5F", 
     xlim=c(0.8,1))

dev.off()


# Paramétrico

Tboot_p <- rep(NULL, B)
n <- length(dt)
for(i in 1:B){
  Xstar <- runif(50, 0, t.hat)
  Tboot_p[i] <- max(Xstar)
}

se_p <- sd(Tboot)

pdf('/Volumes/GoogleDrive/Mi unidad/Universidad/Ciencia de Datos/6to Semestre/2022_1-Metodos_Estadisticos/Tareas/Tarea 2/Graphs/Hist_P.pdf',
    width = 5,
    height = 5)

hist(Tboot_p, #breaks = 'FD', 
     probability = TRUE,
     col = '#FF8E3F', main = '10, 000 Boostrap Paramétrco',
     border = 'white', xlab = 'x', ylab = 'Densidad', 
     col.axis = "#5F5F5F", col.lab = "#5F5F5F", fg = "#5F5F5F", 
     xlim=c(0.8,1))

dev.off()

# HIST

Dist <- rep(NULL, B)

for(i in 1:B){
  x <- runif(50, 0, 1)
  t.hat <- max(x)
  Dist[i] <- t.hat
}

pdf('/Volumes/GoogleDrive/Mi unidad/Universidad/Ciencia de Datos/6to Semestre/2022_1-Metodos_Estadisticos/Tareas/Tarea 2/Graphs/Dist_hist.pdf',
    width = 5,
    height = 5)

hist(Dist, #breaks = 'FD', 
     probability = TRUE,
     col = '#FF8E3F', main = 'Distrubución de Theta',
     border = 'white', xlab = 'x', ylab = 'Densidad', 
     col.axis = "#5F5F5F", col.lab = "#5F5F5F", fg = "#5F5F5F", 
     xlim=c(0.8,1))

dev.off()


# con muestreo:

pdf('/Volumes/GoogleDrive/Mi unidad/Universidad/Ciencia de Datos/6to Semestre/2022_1-Metodos_Estadisticos/Tareas/Tarea 2/Graphs/Dist.pdf',
    width = 7,
    height = 5)

plot(diff(sapply(x, T.hat)), type = 's', 
     col = '#FF8E3F', lwd = 2.5, main = 'Distribución de Theta', xlim=c(0.8,1), 
     xlab = 'x', ylab = 'Densidad', 
     col.axis = "#5F5F5F", col.lab = "#5F5F5F", fg = "#5F5F5F")

dev.off()

diff(sapply(x, T.hat))

sapply(x, T.hat)
