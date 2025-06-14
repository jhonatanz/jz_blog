library(tidyverse)
library(RMariaDB)

col_def <- cols(
  PUNT_INGLES = col_integer(),
  PUNT_MATEMATICAS = col_integer(),
  PUNT_SOCIALES_CIUDADANAS = col_integer(),
  PUNT_C_NATURALES = col_integer(),
  PUNT_LECTURA_CRITICA = col_integer(),
  PUNT_GLOBAL = col_integer(),
  .default = "c"
)
raw_saber11 <- read_csv(
  "~/Documents/R_projects/saber11/files/input/Resultados__nicos_Saber_11_20250313-001.csv",
  col_types = col_def
)


raw_saber12 <- raw_saber11 %>%
  mutate(ESTU_FECHANACIMIENTO = dmy(ESTU_FECHANACIMIENTO))%>%
  mutate(across(contains("COD"), as.integer))

rev_cod_NA <- raw_saber12 %>%
  select(contains("COD"))%>%
  map(~which(is.na(.x)))

# Creación de las tablas que no tienen Foreign Keys
# Tabla Departamentos
colu_deptos <- names(select(raw_saber11, contains("DEPTO")))
colu_deptos

extract_deptos <- function(df) {
  col_depto <- df %>%
    select(depto = "COLE_DEPTO_UBICACION", cod ="COLE_COD_DEPTO_UBICACION")
  est_depto <- df %>%
    select(depto = "ESTU_DEPTO_RESIDE", cod ="ESTU_COD_RESIDE_DEPTO")
  pres_depto <- df %>%
    select(depto = "ESTU_DEPTO_PRESENTACION", cod ="ESTU_COD_DEPTO_PRESENTACION")
  deptos <- rbind(col_depto, est_depto, pres_depto)%>%
    distinct()%>%
    arrange(cod)
}

deptos <- extract_deptos(raw_saber11)
deptos

# se observa que hay dos nombres para el departamento con el código 11, hay que unificarlos

raw_saber11 <- raw_saber11 %>%
  mutate(across(!contains("COD") & contains("DEPTO"), ~ ifelse(.x == "BOGOTÁ", "BOGOTA", .x)))

deptos <- extract_deptos(raw_saber11)

# también se observa que se tiene que el ultimo dato es NA y el código correspondiente también, esto puede 
# ser problemático para el uso en la base datos

# Se reemplazan los códigos de departamento NA al valor cero y los nombres de departamento NA a
# "No especificado" en el dataframe raw_saber11.

raw_saber11 <- raw_saber11 %>%
  mutate(across(contains("COD") & contains("DEPTO"), ~ ifelse(is.na(.x), 1L, .x))) %>%
  mutate(across(!contains("COD") & contains("DEPTO"), ~ ifelse(is.na(.x), "No especificado", .x)))

deptos <- extract_deptos(raw_saber11)

