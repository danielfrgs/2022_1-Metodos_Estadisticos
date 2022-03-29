# TAREA 2

# Funciones auxiliares
library(ggplot2)
library(plotly)
library(scales)
library(tidyverse)

##########################################
# CAPITULO 7

# Ejercicio 7.3 ----------------------------------------------------------------

# Funci?n de distribuci?n emp?rica para un punto
Fn_hat <- function(x0, xn) {
  mean(xn <= x0)
}

# Funci?n de distribuci?n emp?rica para varios puntos
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

# Partici?n del eje x
x = seq(-5, 5, length.out = 1000)
# Evaluaci?n de la funci?n de distribuci?n real en la partici?n
y = pnorm(x)
# N?mero de observaciones
n = 100
# 1-confianza
alpha = .05
# epsilon para la banda de confianza
epsilon = sqrt((1/(2*n))*log(2/alpha))
# Contador de veces que la funci?n de distribuci?n real cae dentro de la 
# banda de confianza 
contador_norm = 0

# 1000 repeticiones
for(i in 1:1000){
  # Simular 100 observaciones de la normal(0,1)
  xn = rnorm(n)
  # Evaluar la funci?n de distribuci?n emp?rica en la partici?n
  yhat = Fn_hat_gen(x, xn)
  # Definir l?mites superior e inferior de la banda de confianza
  Lx = yhat - epsilon
  Ux = yhat + epsilon
  # Si la funci?n de distribuci?n real cae dentro de la banda de confianza, 
  # entonces aumenta el contador en 1
  if(all(Ux > y) & all(c(Lx < y))){
    contador_norm = contador_norm + 1
  }
  # Graficar la primera banda
  if(i == 1){
    plot(x, y, type = "l", lwd = 2, col = "blue", xlab = '', ylab = '')
    lines(x, Lx, type = "l", col = "green")
    lines(x, Ux, type = "l", col = "green")
    title('Banda de confianza al 95% para la funci?n de \n distribuci?n de una v.a. Normal(0,1)')
  }
}

# Repetir el experimento para una distribuci?n Cauchy
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
  if(i == 1){
    plot(x, y, type = "l", lwd = 2, col = "blue", xlab = '', ylab = '')
    lines(x, Lx, type = "l", col = "green")
    lines(x, Ux, type = "l", col = "green")
    title('Banda de confianza al 95% para la funci?n de \n distribuci?n de una v.a. Cauchy(0,1)')
  }
}

# Imprimir resultados
print(paste('Qued? dentro de la banda', contador_norm, 'veces (Normal)'))
print(paste('Qued? dentro de la banda', contador_cauchy, 'veces (Cauchy)'))

# Ejercicio 7.8 ----------------------------------------------------------------

# Descarga los datos
library(MASS)
datos = as.vector(t(geyser['waiting']))
# 1-confianza
alpha = .1

# Estima la media
estimador_media = mean(datos)
# Estima la desviaci?n est?ndar de la estimaci?n de la media
estimador_sigma = sqrt(1/(length(datos))*(sum((datos-mean(datos))^2)))
estimador_sigma_media = estimador_sigma/(sqrt(length(datos)))
# Da un intervalo de confianza para la media
intervalo = estimador_media + c(-1, 1)*qnorm(1-alpha/2)*estimador_sigma_media
# Estima la mediana
estimador_mediana = quantile(datos, probs = .5)

# Imprime las estimaciones solicitadas
print(paste('Estimaci?n de la media:', estimador_media))
print(paste('Estimaci?n del error est?ndar de la estimaci?n de la media', estimador_sigma_media))
print(paste('Intervalo de confianza del 90% de la media: (', intervalo[1], ',',
            intervalo[2],')'))
print(paste('Estimaci?n de la mediana:', estimador_mediana))


# CAPITULO 8


# Ejercicio 8.2 ----------------------------------------------------------------

# Estimador plug-in asimetr?a
my_skewness<- function(x){
  x_bar <- mean(x)
  d <- x - x_bar
  m3 <- mean(d^3)
  m4 <- mean(d^2)^(3/2)
  m3/m4
}

# N?mero de observaciones
n = 50
# 1-confianza
alpha = .05
# N?mero de muestras de la funci?n de distribuci?n emp?rica
B = 200
# Valor real de la asimetr?a 
valor_real = (exp(1)+2)*sqrt(exp(1)-1)
# N?mero de repeticiones
N = 1000

# Intervalos que indican si en la i-?sima repetici?n el valor real de la
# asimetr?a cae dentro del intervalo estimado 
cobertura_normal = rep(F, N)
cobertura_cuantiles = rep(F, N)
cobertura_pivotal = rep(F, N)

# Hacer N repeticiones
for(i in 1:N){
  # Simular n observaciones de la normal(0,1)
  Y = rnorm(n)
  # Definir X=e^Y
  X = exp(Y)
  # Estimar puntualmente a la asimetr?a
  estimador = my_skewness(X)
  
  # Vector que guarda las B estimaciones de la asimetr?a con las simulaciones de 
  # la funci?n de distribuci?n emp?rica
  estimador_estrella = rep(NA, B)
  # Estima la asimetr?a con las simulaciones de la funci?n de distribuci?n emp?rica
  for (t in 1:B) {
    xn_estrella = sample(X, size = n, replace = TRUE)
    estimador_estrella[t] = my_skewness(xn_estrella)
  }
  
  # Calcula los tres tipos de intervalo de confianza e indica si el valor real
  # cae dentro de dicho intervalo
  
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

# Devuelve la proporci?n en que el valor real de la asimetr?a cay? dentro del
# intervalo de confianza estimado
print(paste('Cobertura normal', sum(cobertura_normal)/N))
print(paste('Cobertura cuantiles', sum(cobertura_cuantiles)/N))
print(paste('Cobertura pivotal', sum(cobertura_pivotal)/N))

# Ejercicio 8.3 ----------------------------------------------------------------
#Supones que t_3 es una distribuci?n t de student con par?metro 3


#Creamos una muestra con esa distribuci?n
n <- 25
Xn=rt(n,df=3)

#Definimos nuestro funcional
Tn <- function(x){
  return ((quantile(x, 0.75)-quantile(x,0.25))/1.34)
}

#Simulaci?n Bootsrtap
B <- 100000
Tboot <- rep(NA, B)
for(i in 1:B){
  Xn_estrella <- sample(Xn, size=n, replace=TRUE)
  Tboot[i] <- Tn(Xn_estrella) 
}

#Error est?ndar estimado Bootstrap
se_boot <- sqrt((1/B)*sum((Tboot-mean(Tboot))^2))

#Intervalos de confianza
aalpha <- 0.05
Normal <- Tn(Xn)+c(-1,1)*qnorm(1-aalpha/2)*se_boot
Percentile <- quantile(Tboot, probs=c(aalpha/2, 1-aalpha/2))
Pivotal <- 2*Tn(Xn)-c(1,1)*quantile(Tboot, probs = c(1-aalpha/2, aalpha/2))

print('El intervalo Normal: ')
Normal
print('El intervalo Percentile: ')
Percentile
print('El intervalo Pivotal: ')
Pivotal

#Valor Te?rico
Tn(Xn)

# Ejercicio 8.7 -----------------------------------------------------------

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

plot(x, sapply(x, T.hat), type = 'l', 
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





pdf('/Volumes/GoogleDrive/Mi unidad/Universidad/Ciencia de Datos/6to Semestre/2022_1-Metodos_Estadisticos/Tareas/Tarea 2/Graphs/B_hist.pdf',
    width = 8,
    height = 6)

# Interpolación
p2 <- hist(Tboot, #breaks = 'FD', 
           probability = TRUE,
           col = '#BD2C5A', main = 'Histogramas Boostrap',
           border = 'white', xlab = 'x', ylab = 'Densidad', 
           col.axis = "#5F5F5F", col.lab = "#5F5F5F", fg = "#5F5F5F", 
           xlim=c(0.8,1))

p1 <- hist(Tboot_p, #breaks = 'FD', 
           probability = TRUE,
           col = '#FF8E3F', main = '10, 000 Boostrap Paramétrco',
           border = 'white', xlab = 'x', ylab = 'Densidad', 
           col.axis = "#5F5F5F", col.lab = "#5F5F5F", fg = "#5F5F5F", 
           xlim=c(0.8,1), add = T)                    # centered at 4
                   # centered at 6

lines(density(Dist), # density plot
      lwd = 2, # thickness of line
      col = "black")

legend("topleft", c("Boostrap 'ideal'", "Boostrap 'real'", "Distribución de Theta"), 
       lwd = c(5, 5, 3), col=c("#FF8E3F", "#BD2C5A", 'black'),
       xpd=TRUE, cex=0.7, bty='n')

dev.off()
