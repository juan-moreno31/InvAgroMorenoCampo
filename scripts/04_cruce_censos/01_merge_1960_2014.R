# ==============================================================================
# SCRIPT 04/01: CRUCE CENSOS AGROPECUARIOS 1960 Y 2014
# ==============================================================================
# Descripción:
# Integra el panel consolidado del Censo Agropecuario de 1960 con el
# panel de corte transversal del Censo Nacional Agropecuario 2014, utilizando
# el código DIVIPOLA (cod_mpio).
#
# Se recuperan separadamente los datos geográficos/CEDE completos de 2014 y 
# las variables de tratamiento de baldíos (asumiendo que ausencia = 0 para
# los municipios que reportan Censo en 2014).
# ==============================================================================

library(data.table)

# 1. Rutas
ruta_1960   <- "CensoAgropecuario1960/CensoDigital/censo_1960_consolidado.csv"
ruta_cede   <- "datos/controles_cede_municipales.csv"
ruta_trat   <- "datos/cs_tratamiento_cna.csv"
ruta_cna14  <- "datos/cna2014_municipal.csv"
ruta_salida <- "datos/panel_censos_1960_2014.csv"

# 2. Lectura y limpieza inicial de 1960
cat(">>> Leyendo censo 1960 consolidado...\n")
censo_1960 <- fread(ruta_1960, encoding = "UTF-8", colClasses = list(character = "cod_mpio"))
censo_1960[, cod_mpio := formatC(as.integer(cod_mpio), width = 5, flag = "0")]

# --- Bloque B2: Excluir los 3 municipios huérfanos de Bolívar ---
censo_1960 <- censo_1960[!is.na(cod_mpio) & cod_mpio != "   NA"]

# --- Bloque A2: Colapsar Umpalá (68547) dentro de Piedecuesta ANTES del sufijo ---
cols_sum <- c("n_expl","sup_ha","n_admin","sup_admin","parcelas",
              "expl_informantes","total_vacunos","total_expl",
              "c1","c2_4","c5_9","c10_19","c20_49","c50_99",
              "c100_199","c200_499","c500")
cols_sum <- intersect(cols_sum, names(censo_1960)) # seguridad contra typos

# Para no colapsar los NAs, usamos una clave de agrupación segura
censo_1960[, grp := ifelse(cod_mpio == "   NA" | is.na(cod_mpio), paste0("NA_", .I), cod_mpio)]

censo_1960 <- censo_1960[, c(.(municipio = municipio[which.max(n_expl)],
                               depto = depto[1], dep_norm = dep_norm[1],
                               mun_norm = mun_norm[which.max(n_expl)]),
                             lapply(.SD, sum)),
                         by = .(cod_mpio, grp), .SDcols = cols_sum]
censo_1960[, grp := NULL]

# Añadir sufijos a las variables de 1960 (evitar choques de nombres)
cols_llave_1960 <- c("cod_mpio", "dep_norm", "mun_norm", "depto", "municipio")
cols_datos_1960 <- setdiff(names(censo_1960), cols_llave_1960)
setnames(censo_1960, cols_datos_1960, paste0(cols_datos_1960, "_1960"))

# --- Bloque A1: Preparar CNA 2014 (todos los municipios) ---
cna_2014 <- fread(ruta_cna14, encoding = "UTF-8", colClasses = list(character = "cod_mpio"))
cna_2014[, cod_mpio := formatC(as.integer(cod_mpio), width = 5, flag = "0")]
cna_2014 <- cna_2014[!is.na(cod_mpio) & cod_mpio != "   NA"]

# --- Cruce base (Left Join para mantener la N de 1960) ---
cat(">>> Realizando cruce espacial (Left Join) con CNA 2014...\n")
panel_unificado <- merge(censo_1960, cna_2014, by = "cod_mpio", all.x = TRUE)

# --- Bloque A1: Geografía y controles CEDE independientes (cobertura completa) ---
cat(">>> Realizando cruce espacial (Left Join) con CEDE...\n")
cede <- fread(ruta_cede, colClasses = list(character = "cod_mpio"))
cede[, cod_mpio := formatC(as.integer(cod_mpio), width = 5, flag = "0")]
cols_cede <- setdiff(names(cede), c("cod_mpio", names(panel_unificado)))
panel_unificado <- merge(panel_unificado, cede[, c("cod_mpio", cols_cede), with = FALSE],
                         by = "cod_mpio", all.x = TRUE)

# --- Bloque A1: Tratamiento (solo variables de baldíos, con 0 explícito) ---
cat(">>> Realizando cruce espacial (Left Join) con variables de tratamiento...\n")
tratamiento <- fread(ruta_trat, colClasses = list(character = "cod_mpio"))
tratamiento[, cod_mpio := formatC(as.integer(cod_mpio), width = 5, flag = "0")]

cols_trat <- c("ha_adj","n_res","anio_medio","antiguedad",
               "share_pre36","share_l200","share_pre61","share_incora",
               "share_l30","share_l160","tam_medio","tam_mediano","share_peq")
cols_trat <- intersect(cols_trat, names(tratamiento))
panel_unificado <- merge(panel_unificado,
                         tratamiento[, c("cod_mpio", cols_trat), with = FALSE],
                         by = "cod_mpio", all.x = TRUE)

# Rellenar con ceros si el municipio está en 2014 pero no en resoluciones de baldíos
sin_res <- panel_unificado[!is.na(n_upa) & is.na(ha_adj), which = TRUE]
panel_unificado[sin_res, `:=`(ha_adj = 0, n_res = 0)]

panel_unificado[, `:=`(
  tratado    = as.integer(ha_adj > 0),              
  log_ha_adj = ifelse(ha_adj > 0, log(ha_adj), NA_real_) 
)]
panel_unificado[, en_trat := !is.na(ha_adj)]

# --- Bloque A3: Añadir variables derivadas de 1960 ---
panel_unificado[, share_admin := fifelse(sup_ha_1960 > 0,
                                         sup_admin_1960 / sup_ha_1960, NA_real_)]
panel_unificado[, razon_admin := fifelse(n_admin_1960 > 0 & (n_expl_1960 - n_admin_1960) > 0,
    (sup_admin_1960/n_admin_1960) /
    ((sup_ha_1960 - sup_admin_1960)/(n_expl_1960 - n_admin_1960)), NA_real_)]
panel_unificado[, flag_expl_1960_menor20 := as.integer(total_expl_1960 < 20)]
panel_unificado[, flag_expl_1960_menor50 := as.integer(total_expl_1960 < 50)]

# --- Bloque B1: No enumerados en 1960 (ceros reales de enumeración) ---
no_enumerados <- c("05475","19318","19418","19809")
cols_1960_sufijadas <- grep("_1960$", names(panel_unificado), value = TRUE)
panel_unificado[cod_mpio %in% no_enumerados,
                (cols_1960_sufijadas) := lapply(.SD, function(x) NA_real_), .SDcols = cols_1960_sufijadas]

# Indicadores de control y muestra
panel_unificado[, `:=`(
  en_1960 = !is.na(n_expl_1960) | cod_mpio %in% no_enumerados,
  en_2014 = !is.na(n_upa),
  muestra_a = as.integer(n_upa >= 20)
)]

# 5. Estadísticas de cruce
cat("\n=========================================================================\n")
cat(" ESTADÍSTICAS DEL CRUCE 1960 - 2014\n")
cat("=========================================================================\n")
cat(sprintf("Total de filas en panel final: %d\n", nrow(panel_unificado)))
cat(sprintf("Municipios con datos perfectos 1960 + CNA 2014: %d\n", nrow(panel_unificado[en_1960 == TRUE & en_2014 == TRUE])))
cat(sprintf("Municipios con tratamiento cero real recuperados: %d\n", length(sin_res)))
cat(sprintf("Total de filas únicas por cod_mpio: %d\n", uniqueN(panel_unificado$cod_mpio)))
cat("=========================================================================\n\n")

# 6. Exportación
fwrite(panel_unificado, ruta_salida, quote = TRUE)
cat(sprintf(">>> Archivo consolidado guardado exitosamente en: %s\n", ruta_salida))
