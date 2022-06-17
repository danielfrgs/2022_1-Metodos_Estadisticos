#----------------------- CAPITULO 10 - Pregunta 12

# Determinación de parametros
lambda0 <- 1
n <- 20
alpha <- 0.05

# Caluclo de cuantil alpha/2
z <- qnorm(1-alpha/2, 0, 1)

# Simulación

# Ciclo

# Inicializammos el contador
rechazo <- 0 
# Numero de experimentos
N <- 1000000

for (i in 0:N) {
  X <- rpois(n, lambda0)
  lambda_mle <- mean(X)
  rec <- abs(((lambda_mle - lambda0)) * sqrt(n/lambda_mle)) > z
  if(rec == TRUE){
    rechazo <- rechazo + 1
  }
}

# Tasa de rechazo
rechazo/N