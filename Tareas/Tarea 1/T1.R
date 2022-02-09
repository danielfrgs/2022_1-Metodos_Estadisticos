# TAREA 1 | Métodos Estadísticos

# ------------------------------------------------------------------------------
# PREGUNTA 1
# LECTURA DE DATOS

# Declaramos variable 'temp' como un archivo temporal
temp <- tempfile()
# Definimos el url del archivo zip
# URL de los datos: https://www.inegi.org.mx/programas/ccpv/2020/#Datos_abiertos
url_INEGI <- 'https://www.inegi.org.mx/contenidos/programas/ccpv/2020/datosabiertos/iter/iter_00_cpv2020_csv.zip'

download.file(url_INEGI, temp)

# Utilizamos la función 'unz' para extraer el archivo CSV y lo asignamos a la variable 'temp'
ArchivoCsv = unz(temp, "conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv")
# Introducimos los datos del CSV en la tabla PIB. Aqui le indicamos a la función
# que el archivo tiene cabecera y que el separador de campos es una coma.
inegi_censo <- read.csv(file=ArchivoCsv, header=TRUE, sep=",")

# 1.1 ---------------------------------------------------------------------------

# La suma de los subtotales por municipio se obtienen al principio de la localidad
# con la llave '0' en la columna 'LOC' y para que no tome el total del estado 
# 'MUN' != 0

inegi_censo_mun <- inegi_censo[(inegi_censo[,'LOC'] == 0) 
                             & (inegi_censo[,'MUN'] != 0),]

# Dimensión de los datos:
dim(inegi_censo_mun)
# Tamaño:
# First we load the libraries
library("pryr")
object_size(inegi_censo_mun)

# Para la Población de la República vamos a sumar la población total de cada Municipio
# 'POBTOT'

sum(inegi_censo_mun['POBTOT'])
# Fuente: https://www.inegi.org.mx/contenidos/saladeprensa/boletines/2021/EstSociodemo/ResultCenso2020_Nal.pdf

# 1.2 --------------------------------------------------------------------------

# Personas de 12 a 130 años de edad : P_12YMAS

# Recordemos:
# POBLACION ECONOMICAMENTE ACTIVA. Comprende a todas las personas de 12 años y 
# más que realizaron algún tipo de actividad económica (población ocupada), 
# o que buscaron activamente hacerlo (población desocupada abierta), en el periodo de referencia.

# Tenemos 3 Variables que nos dan este indicador:
# Personas de 12 a 130 años de edad que trabajaron, tenían trabajo pero no trabajaron o buscaron trabajo en la semana de referencia.	PEA
# Personas de 12 a 130 años de edad que trabajaron o que no trabajaron, pero sí tenían trabajo en la semana de referencia.	POCUPADA
# Personas de 12 a 130 años de edad que no tenían trabajo, pero buscaron trabajo en la semana de referencia. 	PDESOCUP


# Como queremos hacer esto de forma estatal seleccionamos los esados
# Luego seleccionaremos la CDMX de forma independiente:

Estatal <- inegi_censo[(inegi_censo[,'LOC'] == 0) 
                                     & (inegi_censo[,'MUN'] == 0),]

CDMX <- inegi_censo[(inegi_censo[,'LOC'] == 0) 
                       & (inegi_censo[,'ENTIDAD'] == 9),]

PEA_estatal <- Estatal[, c('ENTIDAD', 'NOM_ENT', 'P_12YMAS', 'PEA')]

PEA_cdmx <- CDMX[, c('ENTIDAD', 'NOM_ENT', 'MUN', 'NOM_MUN', 'P_12YMAS', 'PEA')]

# Convirtamos los datos a númericos:

# Estatal
PEA_estatal$P_12YMAS <- strtoi(PEA_estatal$P_12YMAS, base=0L)
PEA_estatal$PEA <- strtoi(PEA_estatal$PEA, base=0L)
# CDMX
PEA_cdmx$P_12YMAS <- strtoi(PEA_cdmx$P_12YMAS, base=0L)
PEA_cdmx$PEA <- strtoi(PEA_cdmx$PEA, base=0L)

# Vamos a sumar las poblaciones:

PEA_estatal$PEA_P <- PEA_estatal[, 'PEA'] / PEA_estatal[, 'P_12YMAS'] * 100

PEA_cdmx$PEA_P <- PEA_cdmx[, 'PEA'] / PEA_cdmx[, 'P_12YMAS'] * 100

# Gráficas
# ESTATAL

library(plotly)

fig <- plot_ly(
  x = PEA_estatal$NOM_ENT,
  y = PEA_estatal$PEA_P,
  name = "SF Zoo",
  type = "bar"
)

fig

# CDMX

fig <- plot_ly(
  x = PEA_cdmx$NOM_MUN,
  y = PEA_cdmx$PEA_P,
  name = "SF Zoo",
  type = "bar"
)

fig
