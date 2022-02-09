# TAREA 1 | Métodos Estadisticos

# PREGUNTA 1
# LECTURA DE DATOS

# Declaramos variable 'temp' como un archivo temporal
temp <- tempfile()
# Definimos el url del archivo zip
url_INEGI <- 'https://www.inegi.org.mx/contenidos/programas/ccpv/2020/datosabiertos/iter/iter_00_cpv2020_csv.zip'

download.file(url_INEGI, temp)

# Utilizamos la función 'unz' para extraer el archivo CSV y lo asignamos a la variable 'temp'
ArchivoCsv = unz(temp, "conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv")
# Introducimos los datos del CSV en la tabla PIB. Aqui le indicamos a la función
# que el archivo tiene cabecera y que el separador de campos es una coma.
inegi_censo <- read.csv(file=ArchivoCsv, header=TRUE, sep=",")