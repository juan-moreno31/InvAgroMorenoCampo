# ==============================================================================
# SCRIPT: Integración del Censo Agropecuario 1960 con DIVIPOLA
# ==============================================================================
# Descripción:
# Normaliza y cruza individualmente las tablas del Censo 1960 (c1a, c15a, c16a)
# con el directorio DIVIPOLA, para luego consolidarlas en un único panel con
# la misma cantidad de observaciones (N).
# ==============================================================================

library(data.table)
library(stringr)

# ------------------------------------------------------------------------------
# 1. Funciones de limpieza y normalización
# ------------------------------------------------------------------------------
normalizar_texto <- function(x) {
  x <- toupper(x)
  # Pre-procesar Ñ para evitar problemas de transliteración en diferentes OS
  x <- gsub("Ñ", "N", x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- str_replace_all(x, "[^A-Z ]", " ")
  x <- str_squish(x)
  return(x)
}

# Diccionario histórico para municipios de 1960 que cambiaron de departamento
dict_deptos_historicos <- c(
  "BOLIVAR|COROZAL" = "SUCRE", "BOLIVAR|MAJAGUAL" = "SUCRE",
  "BOLIVAR|MORROA" = "SUCRE", "BOLIVAR|OVEJAS" = "SUCRE",
  "BOLIVAR|PALMITO" = "SUCRE", "BOLIVAR|SINCE" = "SUCRE",
  "BOLIVAR|SINCELEJO" = "SUCRE", "BOLIVAR|TOLU" = "SUCRE",
  "BOLIVAR|TOLUVIEJO" = "SUCRE", "BOLIVAR|SAMPUES" = "SUCRE",
  "BOLIVAR|SAN MARCOS" = "SUCRE", "BOLIVAR|SAN ONOFRE" = "SUCRE",
  "BOLIVAR|SAN PEDRO" = "SUCRE", "BOLIVAR|SUCRE" = "SUCRE",
  "BOLIVAR|SAN BENITO ABAD" = "SUCRE",
  "CORDOBA|CAIMITO" = "SUCRE", "CORDOBA|SAN MARCOS" = "SUCRE",
  "CALDAS|ARMENIA" = "QUINDIO", "CALDAS|CALARCA" = "QUINDIO",
  "CALDAS|CIRCASIA" = "QUINDIO", "CALDAS|FILANDIA" = "QUINDIO",
  "CALDAS|GENOVA" = "QUINDIO", "CALDAS|MONTENEGRO" = "QUINDIO",
  "CALDAS|PIJAO" = "QUINDIO", "CALDAS|QUIMBAYA" = "QUINDIO",
  "CALDAS|SALENTO" = "QUINDIO", "CALDAS|PEREIRA" = "RISARALDA",
  "CALDAS|APIA" = "RISARALDA", "CALDAS|BALBOA" = "RISARALDA",
  "CALDAS|BELEN DE UMBRIA" = "RISARALDA", "CALDAS|GUATICA" = "RISARALDA",
  "CALDAS|LA CELIA" = "RISARALDA", "CALDAS|MARSELLA" = "RISARALDA",
  "CALDAS|QUINCHIA" = "RISARALDA", "CALDAS|SANTA ROSA DE CABAL" = "RISARALDA",
  "CALDAS|SANTUARIO" = "RISARALDA", "CALDAS|LA TEBAIDA" = "QUINDIO",
  "CALDAS|LA VIRGINIA" = "RISARALDA", "CALDAS|MISTRATO" = "RISARALDA",
  "CALDAS|PUEBLORRICO" = "RISARALDA",
  "MAGDALENA|VALLEDUPAR" = "CESAR", "MAGDALENA|AGUACHICA" = "CESAR",
  "MAGDALENA|CHIRIGUANA" = "CESAR", "MAGDALENA|CURUMANI" = "CESAR",
  "MAGDALENA|EL BANCO" = "MAGDALENA", "MAGDALENA|PAILITAS" = "CESAR",
  "MAGDALENA|RIO DE ORO" = "CESAR", "MAGDALENA|TAMALAMEQUE" = "CESAR",
  "MAGDALENA|LA PAZ" = "CESAR", "MAGDALENA|MANAURE" = "CESAR",
  "MAGDALENA|AGUSTIN CODAZZI" = "CESAR", "MAGDALENA|GAMARRA" = "CESAR",
  "MAGDALENA|GONZALEZ" = "CESAR", "MAGDALENA|LA GLORIA" = "CESAR",
  "MAGDALENA|ROBLES" = "CESAR", "MAGDALENA|CHIMICHAGUA" = "CESAR",
  "BOYACA|AGUAZUL" = "CASANARE", "BOYACA|CHAMEZA" = "CASANARE",
  "BOYACA|HATO COROZAL" = "CASANARE", "BOYACA|LA SALINA" = "CASANARE",
  "BOYACA|MANI" = "CASANARE", "BOYACA|MONTERREY" = "CASANARE",
  "BOYACA|NUNCHIA" = "CASANARE", "BOYACA|OROCUE" = "CASANARE",
  "BOYACA|PAZ DE ARIPORO" = "CASANARE", "BOYACA|PORE" = "CASANARE",
  "BOYACA|SACAMA" = "CASANARE", "BOYACA|SAN LUIS DE PALENQUE" = "CASANARE",
  "BOYACA|TAMARA" = "CASANARE", "BOYACA|TRINIDAD" = "CASANARE",
  "BOYACA|YOPAL" = "CASANARE", "BOYACA|RECETOR" = "CASANARE"
)

# Diccionario de nombres de municipios
dict_mun <- c(
  "ANTIOQUIA|ANTIOQUIA" = "SANTA FE DE ANTIOQUIA",
  "ANTIOQUIA|SAN VICENTE" = "SAN VICENTE FERRER",
  "ANTIOQUIA|SAN PEDRO" = "SAN PEDRO DE LOS MILAGROS",
  "ANTIOQUIA|SAN ANDRES" = "SAN ANDRES DE CUERQUIA",
  "ANTIOQUIA|BOLIVAR" = "CIUDAD BOLIVAR",
  "ANTIOQUIA|CARMEN DE VIBORAL" = "EL CARMEN DE VIBORAL",
  "ANTIOQUIA|DON MATIAS" = "DONMATIAS",
  "ANTIOQUIA|SANTUARIO" = "EL SANTUARIO",
  "BOLIVAR|CARTAGENA" = "CARTAGENA DE INDIAS",
  "BOLIVAR|MOMPOS" = "SANTA CRUZ DE MOMPOX",
  "SUCRE|SINCE" = "SAN LUIS DE SINCE",
  "SUCRE|TOLU" = "SANTIAGO DE TOLU",
  "SUCRE|TOLUVIEJO" = "SAN JOSE DE TOLUVIEJO",
  "CESAR|ROBLES" = "LA PAZ",
  "CESAR|CHIMICHAGUA" = "CHIMICHAGUA",
  "BOYACA|GUICAN" = "GUICAN DE LA SIERRA",
  "BOYACA|LEIVA" = "VILLA DE LEYVA",
  "BOYACA|PANQUEVA" = "PANQUEBA",
  "BOYACA|PISVA" = "PISBA",
  "BOYACA|PUEBLOVIEJO" = "AQUITANIA",
  "RISARALDA|PUEBLORRICO" = "PUEBLO RICO",
  "CAUCA|LOPEZ MICAY" = "LOPEZ DE MICAY",
  "CAUCA|PAEZ BELALCAZAR" = "PAEZ",
  "CAUCA|PATIA EL BORDO" = "PATIA",
  "CAUCA|PIENDAMO" = "PIENDAMO TUNIA",
  "CAUCA|SANTANDER" = "SANTANDER DE QUILICHAO",
  "CORDOBA|PUEBLONUEVO" = "PUEBLO NUEVO",
  "CORDOBA|PURISIMA" = "PURISIMA DE LA CONCEPCION",
  "HUILA|SAN JOSE DE ISNOS" = "ISNOS",
  "NARINO|ALBAN SAN JOSE" = "ALBAN",
  "NARINO|ARBOLEDA BERRUECOS" = "ARBOLEDA",
  "NARINO|COLON GENOVA" = "COLON",
  "NARINO|EL TABLON" = "EL TABLON DE GOMEZ",
  "NARINO|ISCUANDE EL CHARCO" = "EL CHARCO",
  "NARINO|LOS ANDES SOTOMAYOR" = "LOS ANDES",
  "NARINO|MAGUI PAYAN" = "MAGUI",
  "NARINO|MALLAMA PIEDRANCHA" = "MALLAMA",
  "NARINO|SANTA CRUZ GUACHAVES" = "SANTACRUZ",
  "NARINO|TUMACO" = "SAN ANDRES DE TUMACO",
  "NORTE DE SANTANDER|VILLA ROSARIO" = "VILLA DEL ROSARIO",
  "NORTE DE SANTANDER|VILLACARO" = "VILLA CARO",
  "NORTE DE SANTANDER|CUCUTA" = "SAN JOSE DE CUCUTA",
  "TOLIMA|MARIQUITA" = "SAN SEBASTIAN DE MARIQUITA",
  "VALLE DEL CAUCA|CALI" = "SANTIAGO DE CALI",
  "SANTANDER|UMPALA" = "PIEDECUESTA" # Umpalá is now part of Piedecuesta
)

# ------------------------------------------------------------------------------
# 2. Carga del directorio DIVIPOLA
# ------------------------------------------------------------------------------
ruta_divi <- "../../CensoNacionalAgro/DIVIPOLA-_Códigos_municipios_20260913.csv"
divi <- fread(ruta_divi, encoding = "UTF-8", colClasses = "character")
setnames(divi, names(divi), sub("^[^A-Za-z0-9]+", "", names(divi)))
setnames(divi, c("Nombre Departamento", "Nombre Municipio", "Código Municipio"), 
         c("dep_div", "mun_div", "cod_mpio"), skip_absent = TRUE)

divi[, `:=`(
  dep_norm = normalizar_texto(dep_div),
  mun_norm = normalizar_texto(mun_div),
  cod_mpio = formatC(as.integer(cod_mpio), width = 5, flag = "0")
)]
divi_maestra <- unique(divi[, .(dep_norm, mun_norm, cod_mpio)])

# ------------------------------------------------------------------------------
# 3. Procesamiento y cruce individual
# ------------------------------------------------------------------------------
procesar_censo <- function(archivo) {
  dt <- fread(archivo, encoding = "UTF-8")
  
  # Excluir filas de totales o vacías
  dt <- dt[!is.na(municipio) & municipio != "" & toupper(municipio) != "TOTALES"]
  
  dt[, `:=`(
    mun_norm = normalizar_texto(municipio),
    dep_norm = normalizar_texto(depto)
  )]
  
  dt[, clave := paste(dep_norm, mun_norm, sep = "|")]
  dt[, dep_norm := ifelse(clave %in% names(dict_deptos_historicos), 
                          dict_deptos_historicos[clave], dep_norm)]
  dt[, clave := paste(dep_norm, mun_norm, sep = "|")]
  dt[, mun_norm := ifelse(clave %in% names(dict_mun), 
                          dict_mun[clave], mun_norm)]
  
  merged <- merge(dt, divi_maestra, by = c("dep_norm", "mun_norm"), all.x = TRUE)
  merged[, c("clave", "pagina", "nota") := NULL]
  
  return(merged)
}

archivos <- c("c1a" = "c1a_corregido.csv", 
              "c15a" = "c15a_corregido.csv", 
              "c16a" = "c16a_corregido.csv")

list_merged <- lapply(archivos, procesar_censo)

# Validar Ns individuales
cat("=== TAMAÑOS TRAS MERGE INDIVIDUAL (Antes de Consolidar) ===\n")
for (k in names(list_merged)) {
  dt <- list_merged[[k]]
  n_total <- nrow(dt)
  n_match <- sum(!is.na(dt$cod_mpio))
  cat(sprintf("%s: %d filas totales | %d cruzaron con DIVIPOLA (%.1f%%)\n", 
              k, n_total, n_match, 100 * n_match / n_total))
}

# ------------------------------------------------------------------------------
# 4. Consolidación de todos los censos
# ------------------------------------------------------------------------------
# Combinamos usando TODAS las filas de cada uno (Outer Join) por si acaso.
# Las llaves de unión serán cod_mpio, dep_norm y mun_norm.

c1a  <- list_merged[["c1a"]]
c15a <- list_merged[["c15a"]]
c16a <- list_merged[["c16a"]]

# Variables a cruzar
cols_merge <- c("cod_mpio", "dep_norm", "mun_norm", "depto", "municipio")

consolidado <- merge(c1a, c15a[, .SD, .SDcols = c(cols_merge, setdiff(names(c15a), names(c1a)))], by = cols_merge, all = TRUE)
consolidado <- merge(consolidado, c16a[, .SD, .SDcols = c(cols_merge, setdiff(names(c16a), names(consolidado)))], by = cols_merge, all = TRUE)

cat("\n=== RESULTADO DEL PANEL CONSOLIDADO ===\n")
cat(sprintf("N total de encuestas (municipios) en el panel unificado: %d\n", nrow(consolidado)))

fwrite(consolidado, "censo_1960_consolidado.csv", quote = TRUE)
cat("Archivo guardado en: censo_1960_consolidado.csv\n")
