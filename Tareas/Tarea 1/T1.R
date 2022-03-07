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

## 1.1 ---------------------------------------------------------------------------

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

## 1.2 --------------------------------------------------------------------------

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

PEA_cdmx <- CDMX[, c('MUN', 'NOM_MUN', 'P_12YMAS', 'PEA')]

# Convirtamos los datos a númericos:

# Estatal
PEA_estatal$P_12YMAS <- strtoi(PEA_estatal$P_12YMAS, base=0L)
PEA_estatal$PEA <- strtoi(PEA_estatal$PEA, base=0L)
# CDMX
PEA_cdmx$P_12YMAS <- strtoi(PEA_cdmx$P_12YMAS, base=0L)
PEA_cdmx$PEA <- strtoi(PEA_cdmx$PEA, base=0L)

# Vamos a sumar las poblaciones:

PEA_estatal$PEA_P <- PEA_estatal[, 'PEA'] / PEA_estatal[, 'P_12YMAS']

PEA_cdmx$PEA_P <- PEA_cdmx[, 'PEA'] / PEA_cdmx[, 'P_12YMAS']

# Gráficas
# ESTATAL
library(ggplot2)
library(plotly)
library(scales)

# Cambiamso algunos nombres:
ent = c("Nacional", "Aguascalientes" , "Baja California", "Baja California Sur" ,
        "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
        "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", 
        "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
        "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", 
        "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")

PEA_estatal$NOM_ENT = ent

# Ordenemos los datos de forma desendente:
estatal_ord <- PEA_estatal[order(PEA_estatal$PEA_P), ]

df <- data.frame(estatal_ord$NOM_ENT, estatal_ord$PEA_P)

ggplot(df, aes(reorder(PEA_estatal$NOM_ENT, PEA_estatal$PEA_P), 
               PEA_estatal$PEA_P,
               fill = ifelse(PEA_estatal$NOM_ENT == "Nacional", "Highlighted", "Normal") )) +
  geom_bar(position= "stack", stat = 'identity') + theme_minimal() +
  #geom_text(aes(label = percent(PEA_estatal$PEA_P)), vjust = 0.5, angle=90, hjust=1)+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle=60, hjust=1, vjust = 1), 
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(labels = percent_format()) +
  #coord_flip(expand = FALSE) +
  scale_fill_manual(values=c("#F95C5C", "#F4C2A9")) +
  labs(y = "% de Población Economicamente Activa", x = "Entidad", title = 'Porcentaje de P.E.A. por Estado')
  

ggsave(
  'Graphs/PEA_1.pdf',
  width = 25*0.8,
  height = 15*0.8,
  units = c("cm"),
  dpi = 300,
)

# CDMX


list(PEA_cdmx$NOM_MUN)

entcdmx = c("Ciudad de México", "Azcapotzalco", "Coyoacán", "Cuajimalpa", "Gustavo A. Madero",
            "Iztacalco", "Iztapalapa", "Magdalena Contreras", "Milpa Alta", "Álvaro Obregón",
            "Tláhuac", "Tlalpan", "Xochimilco", "Benito Juárez", "Cuauhtémoc", "Miguel Hidalgo",
            "Venustiano Carranza" )

PEA_cdmx$NOM_MUN = entcdmx

# Agreguemos el valor Nacional

nacional <- PEA_estatal[1,]

names(nacional) <- c('MUN', 'NOM_MUN', 'P_12YMAS', 'PEA', 'PEA_P')

PEA_cdmx <- rbind(PEA_cdmx, nacional)

# GRAFICA

df <- data.frame(PEA_cdmx$NOM_MUN, PEA_cdmx$PEA_P)

ggplot(df, aes(reorder(PEA_cdmx$NOM_MUN, PEA_cdmx$PEA_P), 
               PEA_cdmx$PEA_P,
               fill = ifelse(PEA_cdmx$NOM_MUN == c("Ciudad de México", 'Nacional'), "Nacional", "Normal") )) +
  geom_bar(position= "stack", stat = 'identity') + theme_minimal() +
  #geom_text(aes(label = percent(PEA_estatal$PEA_P)), vjust = 0.5, angle=90, hjust=1)+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle=60, hjust=1, vjust = 1), 
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(labels = percent_format()) +
  #coord_flip(expand = FALSE) +
  scale_fill_manual(values=c("#F95C5C", "#F4C2A9")) +
  labs(y = "% de Población Economicamente Activa", x = "Delegación", title = 'Porcentaje de P.E.A. en la CDMX')

ggsave(
  'Graphs/PEA_CDMX.pdf',
  width = 25*0.8,
  height = 15*0.8,
  units = c("cm"),
  dpi = 300,
)
## 1.3 --------------------------------------------------------------------------

# Personas de 5 a 130 años de edad que hablan alguna lengua indígena.	P5_HLI
# Personas de 15 a 130 años de edad que no saben leer y escribir un recado.	P15YM_AN

# Se cotejan con:
# Personas de 5 a 130 años de edad.	P_5YMAS
# Personas de 15 a 130 años de edad.	P_15YMAS

# Recordemos que ya tenemos la base de datos de los municipios del país.

municipios <- inegi_censo_mun[, c('P5_HLI', 'P15YM_AN', 'P_5YMAS', 'P_15YMAS')]

# Estatal
municipios$P5_HLI <- strtoi(municipios$P5_HLI, base=0L)
municipios$P_5YMAS <- strtoi(municipios$P_5YMAS, base=0L)
municipios$P15YM_AN <- strtoi(municipios$P15YM_AN, base=0L)
municipios$P_15YMAS <- strtoi(municipios$P_15YMAS, base=0L)

# Calculamos porcentaje
municipios$P_LI <- municipios$P5_HLI / municipios$P_5YMAS
municipios$P_A <- municipios$P15YM_AN / municipios$P_15YMAS


# Diagrama de correlación

df <- data.frame(municipios$P_LI, municipios$P_A)

ggplot(df, aes(municipios$P_LI,  municipios$P_A)) +
    geom_point(alpha = 0.5, colour = "#F95C5C")+
    theme_minimal() +
    #geom_text(aes(label = percent(PEA_estatal$PEA_P)), vjust = 0.5, angle=90, hjust=1)+
    theme(legend.position = "none", 
          plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(labels = percent_format()) +
    labs(y = "% de Población Analfabeta", x = "% de Población que habla una Lengua Indígena", 
         title = 'Población Analfabeta contra Hablantes de una Lengua Indígena \na nivel Municipal') +
    geom_smooth(method=lm , color="#566573", fill="#85929E", se=TRUE)

ggsave(
  'Graphs/PALI_1.pdf',
  width = 25*0.8,
  height = 15*0.8,
  units = c("cm"),
  dpi = 300,
)
# Graficas Auxiliares

ggplot(municipios, aes(P_15YMAS, P_A)) +
  geom_point(alpha = 0.5, colour = "#F95C5C")+
  theme_minimal() +
  #geom_text(aes(label = percent(PEA_estatal$PEA_P)), vjust = 0.5, angle=90, hjust=1)+
  theme(legend.position = "none", 
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(labels = percent_format()) +
  #scale_x_continuous(labels = percent_format()) +
  labs(y = "% de Población Analfabeta", x = "Población de 15 años y más", 
       title = 'Población Analfabeta contra Población \na nivel Municipal')

ggsave(
  'Graphs/PA15_1.pdf',
  width =15*0.8,
  height = 15*0.8,
  units = c("cm"),
  dpi = 300,
)

ggplot(municipios, aes(P_5YMAS, P_LI)) +
  geom_point(alpha = 0.5, colour = "#F95C5C")+
  theme_minimal() +
  #geom_text(aes(label = percent(PEA_estatal$PEA_P)), vjust = 0.5, angle=90, hjust=1)+
  theme(legend.position = "none", 
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(labels = percent_format()) +
  #scale_x_continuous(labels = percent_format()) +
  labs(y = "% de Población que habla una Lengua Indígena", x = "Población de 5 años y más", 
       title = 'Población Hablante de una Lengua Indígena \ncontra Población a nivel Municipal')


ggsave(
  'Graphs/LI15_1.pdf',
  width = 15*0.8,
  height = 15 * 0.8,
  units = c("cm"),
  dpi = 300,
)


# Ejecutar para limpiar las variables de la primera parte
rm(list=ls()) 
# ------------------------------------------------------------------------------
# PREGUNTA 2
# LECTURA DE DATOS

# Declaramos variable 'temp' como un archivo temporal
#temp <- tempfile()
# Definimos el url del archivo zip
url_COVID <- 'https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip'
#options(timeout=500)
#download.file(url_COVID, temp)

# Utilizamos la función 'unz' para extraer el archivo CSV y lo asignamos a la variable 'temp'
#ArchivoCsv = unz(temp, "220209COVID19MEXICO.csv")
# Introducimos los datos del CSV en la tabla.

# AQUI MARCA ERROR NO SUPE SOLUCIONAR ASÍ QUE IMPORTE DIRETAMENTE DE FORMA LOCAL
#covid <- read.csv(file=ArchivoCsv, header=TRUE, sep=",")

#covid <- read.csv('/Users/danielfragoso/Downloads/220209COVID19MEXICO.csv')

# Posible alternativa de lectura. Para seleccionar los datos directamente
library("data.table")

covid <- fread('/Users/danielfragoso/Downloads/220227COVID19MEXICO.csv',
                  select = c("CLASIFICACION_FINAL", "FECHA_SINTOMAS", "TIPO_PACIENTE", "ENTIDAD_RES", "FECHA_DEF",
                             "EDAD"))


## 2.1 -------------------------------------------------------------------------

# Vamos a filtrar los datos:
# CLASIFICACION_FINAL : 1, 2, 3
# ENTIDAD_RES: 9

covid <- as.data.frame(covid)

covid_cdmx <- covid[(covid[,'CLASIFICACION_FINAL'] < 4) &
                    (covid[,'ENTIDAD_RES'] == 9),]

## 2.2 ------------------------------------------------------------------------

# a)
library(tidyverse)

covid_cdmx['CONT'] = 1

casos_mes <- covid_cdmx %>%
             group_by(month = lubridate::floor_date(FECHA_SINTOMAS, 'month')) %>%
             summarize(sumamary_variable = sum(CONT))

names(casos_mes) = c("month", 'N_casos')


ggplot(casos_mes, aes(x=month, y=N_casos, group=1)) +
  geom_line(color="#F95C5C", size=1.2) +
  geom_point(color="#F95C5C", alpha = 0.7, size=1.5) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust = 1), 
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(label=comma) +
  scale_x_date(date_labels = "%b/%y") +
  labs(y = "Números de Casos", x = "Meses", 
       title = 'Número de Contagios Registrados Mensualmente \nen la CDMX')

ggsave(
  'Graphs/COVID_NC.pdf',
  width = 25 * 0.8,
  height = 15 * 0.8,
  units = c("cm"),
  dpi = 300,
)
# b)

# Suma de 15 días
covid_cdmx$CASOS15 <- covid_cdmx$FECHA_SINTOMAS + 15
# Los pacientes hospitalizados son el numero 2
hosp <- covid_cdmx[(covid_cdmx[, 'TIPO_PACIENTE'] == 2), ]

hosp_mes <- hosp %>%
  group_by(month = lubridate::floor_date(CASOS15, 'month')) %>%
  summarize(sumamary_variable = sum(CONT))

names(hosp_mes) = c("month", 'N_hosp')
# Gráfica

ggplot(hosp_mes, aes(x=month, y=N_hosp, group=1)) +
  geom_line(color="#F95C5C", size=1.2) +
  geom_point(color="#F95C5C", alpha = 0.7, size=1.5) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust = 1), 
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(label=comma) +
  scale_x_date(date_labels = "%b/%y") +
  labs(y = "Número de Hospitalizaciones", x = "Meses", 
       title = 'Número de Hospitalizaciones Registradas Mensualmente \nen la CDMX')

ggsave(
  'Graphs/COVID_NH.pdf',
  width = 25 * 0.8,
  height = 15 * 0.8,
  units = c("cm"),
  dpi = 300,
)
# c)

# Vamos a generar un Merge Join para unir por mes 

porc_mes <- merge(x = casos_mes, y = hosp_mes, all = TRUE)

# Calculo de porcentaje

porc_mes$porcentaje <- porc_mes$N_hosp/porc_mes$N_casos

# Grafica

ggplot(porc_mes, aes(x=month, y=porcentaje, group=1)) +
  geom_line(color="#F95C5C", size=1.2) +
  geom_point(color="#F95C5C", alpha = 0.7, size=1.5) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust = 1), 
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(date_labels = "%b/%y") +
  labs(y = "% de Hospitalizados", x = "Meses", 
       title = 'Porcentaje de Hospitalizaciones Registradas Mensualmente \nen la CDMX')

ggsave(
  'Graphs/COVID_PH.pdf',
  width = 25*0.8,
  height = 15*0.8,
  units = c("cm"),
  dpi = 300,
)
# d)

# La muerte viene en el registro 'FECHA_DEF'
# Vamos a filtrar los datos para tener fechas válidas


def <- covid_cdmx[(covid_cdmx[, 'FECHA_DEF'] != '9999-99-99'), ]

# Convirtamos a fechas para poder agrupar

def$FECHA_DEF <- as.Date(def$FECHA_DEF)

def_mes <- def %>%
  group_by(month = lubridate::floor_date(FECHA_DEF, 'month')) %>%
  summarize(sumamary_variable = sum(CONT))

names(def_mes) = c("month", 'N_def')
# Gráfica

ggplot(def_mes, aes(x=month, y=N_def, group=1)) +
  geom_line(color="#F95C5C", size=1.2) +
  geom_point(color="#F95C5C", alpha = 0.7, size=1.5) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust = 1),
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(label=comma) +
  scale_x_date(date_labels = "%b/%y") +
  labs(y = "Número de Defunciones", x = "Meses", 
       title = 'Número de Defunciones Registradas Mensualmente \nen la CDMX')

ggsave(
  'Graphs/COVID_ND.pdf',
  width = 25*0.8,
  height = 15*0.8,
  units = c("cm"),
  dpi = 300,
)

# Ahora para porcentaje de defunciones.
# Vamos a generar un Merge Join para unir por mes 

porc_mes_def <- merge(x = casos_mes, y = def_mes, all = TRUE)

# Calculo de porcentaje

porc_mes_def$porcentaje <- porc_mes_def$N_def/porc_mes_def$N_casos

# Grafica

ggplot(porc_mes_def, aes(x=month, y=porcentaje, group=1)) +
  geom_line(color="#F95C5C", size=1.2) +
  geom_point(color="#F95C5C", alpha = 0.7, size=1.5) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust = 1), 
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(date_labels = "%b/%y") +
  labs(y = "% de Defunciones", x = "Meses", 
       title = 'Porcentaje de Defuciones Registradas Mensualmente \nen la CDMX')

ggsave(
  'Graphs/COVID_PD.pdf',
  width = 25*0.8,
  height = 15*0.8,
  units = c("cm"),
  dpi = 300,
)
## 2.3 -------------------------------------------------------------------------

# a)
# Usaremos el registro de 'EDAD' y agruparemos por grupos

covid_g1 <- covid_cdmx[(covid_cdmx[, 'EDAD'] >= 0) & (covid_cdmx[, 'EDAD'] < 20), ]

covid_g2 <- covid_cdmx[(covid_cdmx[, 'EDAD'] >= 20) & (covid_cdmx[, 'EDAD'] < 40), ]

covid_g3 <- covid_cdmx[(covid_cdmx[, 'EDAD'] >= 40) & (covid_cdmx[, 'EDAD'] < 60), ]

covid_g4 <- covid_cdmx[(covid_cdmx[, 'EDAD'] >= 60), ]

# b)

Agr_Mes_Sintomas <- function(dataframe1){
  mes <- dataframe1 %>%
    group_by(month = lubridate::floor_date(FECHA_SINTOMAS, 'month')) %>%
    summarize(casos = sum(CONT))
  
  return(mes)
}

Agr_Mes_Hosp <- function(dataframe1){
  mes <- dataframe1 %>%
    group_by(month = lubridate::floor_date(CASOS15, 'month')) %>%
    summarize(hosp = sum(CONT))
  
  return(mes)
}

# Vamos a hacer una segunda función que nos claucle el porcentaje

Porcentaje_Grupo <- function(dataframe1, nombre){
  # Los pacientes hospitalizados son el numero 2
  hosp <- dataframe1[(dataframe1[, 'TIPO_PACIENTE'] == 2), ]
  
  grupos_mes <- merge(x = Agr_Mes_Hosp(hosp),
                      y = Agr_Mes_Sintomas(dataframe1),
                      all = TRUE)
  
  grupos_mes$porcentaje <- grupos_mes$hosp/grupos_mes$casos
  
  names(grupos_mes) = c("month", 'hosp', 'casos', nombre)
  
  return(grupos_mes[, c('month', nombre)])
}

# Calculo

Grupos <- list(covid_g1, covid_g2, covid_g3, covid_g4)
Nombre_G <- c('P_1', 'P_2', 'P_3', 'P_4')

# Merge entre todos

grupo_porc <- Porcentaje_Grupo(Grupos[[1]], Nombre_G[1])
for (x in 2:4) {
  grupo_porc <- merge(x = grupo_porc,
                      y = Porcentaje_Grupo(Grupos[[x]], Nombre_G[x]),
                      all = TRUE)
}

# Gráfica

# Vamosa preparar los datos con tydyverse

grupo_porc <- grupo_porc %>%
  select(month, P_1, P_2, P_3, P_4) %>%
  gather(key = "variable", value = "value", -month)

cols <- c("#5CB85C", "#46B8DA", "#EEA236", "#F95C5C")


ggplot(grupo_porc, aes(x=month, y = value, color = variable)) +
  geom_line(size=1.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust = 1), 
        plot.title = element_text(family = "Helvetica", face = "bold", size = (14))) +
  scale_y_continuous(label = percent_format()) +
  scale_x_date(date_labels = "%b/%y") +
  labs(y = "% de Hospitalziaciones por Grupo", x = "Meses", 
       title = 'Porcentaje de Hospitalizaciones Registradas Mensualmente \n en la CDMX por Grupos de Edad') +
  scale_color_manual(values = cols) +
  scale_color_discrete(labels = c('0-20', '20-40', '40-60', '+60'))+
  guides(color = guide_legend(title = "Grupos de Edad"))

ggsave(
  'Graphs/COVID_GE.pdf',
  width = 25*0.8,
  height = 15*0.8,
  units = c("cm"),
  dpi = 300,
)

# Ejercicio 3 ------------------------------------------------------------------


# Funci?n que simula la variable aleatoria X = Ganancia del juego, pagando m
# para entrar y escogiendo el n?mero n, ?ste ?ltimo puede tomar el valor de 
# 'azar' para elegir n al azar
X <- function(m,n){
  # Si se eligi? la estrategia 'azar', se escoge un n?mero al azar del 1 al 6
  if(n=='azar'){
    n = sample(1:6,1)
  }
  # La ganancia empieza en -m que es lo que cost? entrar al juego
  ganancia = -m
  # Se tiran 3 dados
  for(i in 1:3){
    dado = sample(1:6,1)
    # Por cada cado que sale n, se gana m
    if(dado == n){
      ganancia = ganancia + m
    }
  }
  # Devolver la ganancia
  return(ganancia)
}

# Funci?n que estima la funci?n de probabilidad, la esperanza y la  arianza de
# la variable aleatoria X usando la estrategia de pagar m y escoger n, con
# N simulaciones
estima <- function(m,n,N){
  # Simula una muestra aleatoria de tama?no N de X
  Xs = c()
  for(i in 1:N){
    Xs = c(Xs,X(m,n))
  }
  print(paste('Estrategia: n =', n, 'm =', m))
  # Estima la funci?n de probabilidad
  for(i in c(-m,0,m,2*m)){
    print(paste('P( X=',i,')','= f_X(', i, ') = ', sum(Xs==i)/length(Xs)))
  }
  # Estima la esperanza
  print(paste('Esperanza: ', mean(Xs)))
  # Estima la varianza
  print(paste('Varianza: ', var(Xs)))
}

# Usa la funci?n estima(m,n,N) para responder lo solicitado en la tarea
# con N=10,000
estima(10,6,10000)
estima(115,6,10000)
estima(10, 'azar', 10000)

# Ejercicio 4 ------------------------------------------------------------------

# Funci?n que simula la variable aleatoria Y = N?mero de personas que cruzan el
# puente, dado que el puente tiene N pares de pelda?os y J jugadores
Y <- function(N,J){
  # La i-?sima entrada de la  variable 'estables' guarda el i-?simo pelda?o que
  # no se rompe al pisarlo, 1 es el de la derecha y 2 el de la izquierda
  estables = sample(1:2,N,replace = T)
  # Se empiezan con J jugadores
  jugadores = J
  # Se inicia en el "pelda?o" 0
  actual = 0
  # Mientras haya jugadores:
  while(jugadores > 0){
    # El jugador que lidera la fila escoge su pelda?o, izquierda (2) o derecha (1)
    eleccion = sample(1:2,1)
    # Si el pelda?o que escogi? no coincide con el pelda?o que no se rompe:
    if(eleccion != estables[actual+1]){
      # Se cae un jugador
      jugadores = jugadores -1
      # Si ya no hay m?s jugadores, entonces 0 personas lograron pasar el puente
      if(jugadores == 0){
        return(0)
      }
    }
    # Ya sea que el jugador se haya caido o no se avanza una posici?n, pues si
    # no se cay? significa que s? avanz?, y si se cay? entonces el de atr?s
    # ya sabe cu?l es el pelda?o correcto
    actual = actual + 1
    # Si ya se alcanz? el N-?simo pelda?o, todos los jugadores restantes pasan
    # el puente
    if(actual == N){
      return(jugadores)
    }
  } 
}

# Funci?n que estima lo solicitado en el ejercicio 4
estima2 <- function(N,J,size){
  # Simula una muestra aleatoria de tama?o 'size' de Y
  Ys = c()
  for(i in 1:size){
    Ys = c(Ys,Y(N,J))
  }
  print(paste('Pelda?os =', N, 'Jugadores =', J))
  # Estima la probabilidad de que al menos pasen la mitad de los jugadores
  print(paste('P( Al menos pasen la mitad de los jugadores ) =', sum(Ys >= J/2)/length(Ys)))
  # Estima el promedio de los jugadores que pasan
  promedio = mean(Ys)
  print(paste('Juadores que pasan en promedio: ', promedio))
  # Estima la probabilidad que que crucen menos que el promedio (el simulado)
  print(paste('P( Crucen menos jugadores que el promedio ) =', sum(Ys<promedio)/length(Ys)))
  # Estima el promedio de jugadores que cruzan condicionado a que cruzar?n m?s de 5
  print(paste('Promedio de jugadores que cruzan dado que cruzaron m?s de 5:', mean(Filter(function(x) x>5, Ys))))
  # Encuentra un intervalo al 90% de confianza de la cantidad de jugadores que 
  # cruzar? el puente
  cuantiles = quantile(Ys, probs = c(.05, .95),type = 1)
  print(paste('Intervalo de confianza del 90% de cu?ntos jugadores cruzar?n: [',cuantiles[1], ',', cuantiles[2], ']'))
}

# Usa la funci?n etima2(N, J, size) para responder las preguntas del ejercicio 4
estima2(18,16,10000)


