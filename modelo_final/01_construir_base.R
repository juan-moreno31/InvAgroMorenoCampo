# ==============================================================================
# MODELO FINAL — 01: CONSTRUCCIÓN DE LA BASE ANALÍTICA MUNICIPAL
# ==============================================================================
# Moreno Sánchez & Campo Robledo — Titulación de baldíos y concentración de la
# tierra (CNA 2014).
#
# Ejecutar con el directorio de trabajo en la raíz del proyecto (AgroMorenoCampo).
#
# Correcciones frente a scripts/02_cna_municipal/09 y 10 (ver LEEME.md):
#   * Conflicto CNMH 1980–2012 agregado desde las fuentes para TODOS los
#     municipios DIVIPOLA. El panel anterior solo cubría la muestra B; los
#     municipios fuera de ella quedaban NA y luego se
#     imputaban como 0.
#   * Se elimina share_minifundio_1960: las clases c1_1960…c500_1960 del Censo
#     de 1960 son tamaños de HATO (número de cabezas de ganado), no de
#     explotación. Suman exactamente expl_informantes_1960.
#   * Controles de 1960 basados en tierra: tamaño medio de explotación
#     (sup_ha_1960 / n_expl_1960) y densidad ganadera (vacunos / ha).
#   * Se conservan los municipios de la muestra A con CERO adjudicaciones
#     (ceros verdaderos: todas las resoluciones emparejan con
#     DIVIPOLA), marcados con tratado = 0, para el chequeo de margen extensivo.
#   * Intensidad acotada: algunos municipios tienen área adjudicada > área municipal
#     (artefacto de segregaciones municipales). Se crea intens_c = min(intens, 1)
#     y log(1 + intens) para robustez.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

dir_out <- "modelo_final/datos"
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

pad5 <- function(x) formatC(as.integer(x), width = 5, flag = "0")

# ------------------------------------------------------------------------------
# 1. DIVIPOLA (universo y coordenadas)
# ------------------------------------------------------------------------------
divi <- fread("CensoNacionalAgro/DIVIPOLA-_Códigos_municipios_20260913.csv",
              encoding = "UTF-8", colClasses = "character")
setnames(divi, names(divi), sub("^[^A-Za-z0-9]+", "", names(divi)))
divi <- divi[, .(cod_mpio = pad5(`Código Municipio`),
                 departamento = `Nombre Departamento`,
                 municipio = `Nombre Municipio`,
                 lon_divi = as.numeric(gsub(",", ".", longitud)),
                 lat_divi = as.numeric(gsub(",", ".", Latitud)))]
divi <- divi[!duplicated(cod_mpio)]
cods_validos <- divi$cod_mpio

# ------------------------------------------------------------------------------
# 2. CNA 2014 + tratamiento (Villaveces y Sánchez, 2014) + controles CEDE
# ------------------------------------------------------------------------------
cna  <- fread("datos/cna2014_municipal.csv", colClasses = list(character = "cod_mpio"))
cst  <- fread("datos/cs_tratamiento_cna.csv", colClasses = list(character = "cod_mpio"))
cede <- fread("datos/controles_cede_municipales.csv", colClasses = list(character = c("cod_mpio", "cod_dpto")))
for (dt in list(cna, cst, cede)) dt[, cod_mpio := pad5(cod_mpio)]

vars_trat <- c("ha_adj", "n_res", "anio_medio", "antiguedad", "share_pre36", "share_l200",
               "share_pre61", "share_incora", "share_l30", "share_l160",
               "tam_medio", "tam_mediano", "share_peq")

# Universo: muestra A del CNA 2014 (>= 20 UPAs privadas)
base <- cna[n_upa >= 20]
base <- merge(base, cst[, c("cod_mpio", vars_trat), with = FALSE], by = "cod_mpio", all.x = TRUE)
base[, tratado := as.integer(!is.na(n_res))]
base[, muestra_b := as.integer(share_informado >= 0.70)]   # tenencia informada >= 70 % del área
base[tratado == 0, `:=`(ha_adj = 0, n_res = 0L)]

# Controles CEDE (predeterminados). area_mpio_ha ya viene en el CNA municipal.
if ("cod_dpto" %in% names(base)) base[, cod_dpto := NULL]  # se reconstruye abajo desde DIVIPOLA
base <- merge(base, cede[, !c("area_mpio_ha", "cod_dpto"), with = FALSE], by = "cod_mpio", all.x = TRUE)
base <- merge(base, divi, by = "cod_mpio", all.x = TRUE)
base[, cod_dpto := substr(cod_mpio, 1, 2)]
base[, `:=`(lon = lon_divi, lat = lat_divi)]

# Tratamiento
base[, intens        := ha_adj / area_mpio_ha]          # ha adjudicadas / ha del municipio
base[, intens_c      := pmin(intens, 1)]                # acotada a 1 (principal)
base[, log1p_intens  := log1p(intens)]
base[, antig_dec     := antiguedad / 10]                # décadas (ponderado por área, a 2014)
base[, log_tam_adj   := fifelse(tam_medio > 0, log(tam_medio), NA_real_)]  # tamaño medio adjudicado

# ------------------------------------------------------------------------------
# 3. Estructura agraria de 1960 (solo variables de TIERRA y de hato bien rotuladas)
# ------------------------------------------------------------------------------
p60 <- fread("datos/panel_censos_1960_2014.csv", colClasses = list(character = "cod_mpio"))
gh  <- fread("datos/gini_hato_1960.csv", colClasses = list(character = "cod_mpio"))
p60[, cod_mpio := pad5(cod_mpio)]; gh[, cod_mpio := pad5(cod_mpio)]

v60 <- p60[en_1960 == TRUE, .(
  cod_mpio,
  en_1960 = 1L,
  share_admin_1960     = sup_admin_1960 / sup_ha_1960,          # superficie bajo administrador
  log_tam_expl_1960    = log(sup_ha_1960 / n_expl_1960),        # tamaño medio de explotación
  log_dens_bovina_1960 = log(total_vacunos_1960 / sup_ha_1960)  # cabezas por ha censada
)]
v60 <- merge(v60, gh[, .(cod_mpio, gini_hato_1960 = gini_hato, estado_hato = estado)],
             by = "cod_mpio", all.x = TRUE)
v60[estado_hato != "ok", gini_hato_1960 := NA_real_]
base <- merge(base, v60, by = "cod_mpio", all.x = TRUE)
base[is.na(en_1960), en_1960 := 0L]

# ------------------------------------------------------------------------------
# 4. CNMH 1980–2012 desde las fuentes (todos los municipios DIVIPOLA)
# ------------------------------------------------------------------------------
anios_conf <- 1980:2012

arch_vic <- c(AS = "VictimasAS_202603.xlsx", MA = "VictimasMA_202603.xlsx",
              AB = "VictimasAB_202603.xlsx", AP = "VictimasAP_202603.xlsx",
              AT = "VictimasAT_202603.xlsx", MI = "VictimasMI_202603.xlsx")
lista <- list()
for (mod in names(arch_vic)) {
  dt <- as.data.table(read_excel(file.path("CensoNacionalAgro", arch_vic[[mod]]), col_types = "text"))
  col_yr  <- grep("A..o|Ano|Año", names(dt), value = TRUE)[1]
  col_cod <- grep("DANE", names(dt), value = TRUE)[1]
  dt[, `:=`(anio = suppressWarnings(as.integer(get(col_yr))),
            cod_mpio = suppressWarnings(pad5(get(col_cod))))]
  if ("Situación Actual de la Víctima" %in% names(dt)) dt <- dt[`Situación Actual de la Víctima` == "MUERTA"]
  lista[[mod]] <- dt[anio %in% anios_conf & cod_mpio %in% cods_validos, .(cod_mpio, mod = mod)]
}
vic <- rbindlist(lista)[, .(victimas_80_12 = .N, muertes_as_80_12 = sum(mod == "AS")), by = cod_mpio]

db <- as.data.table(read_excel("Bases-datos/CasosDB_202509.xlsx", col_types = "text"))
col_db_yr  <- names(db)[grep("A..o|Ano|Año", names(db))[1]]
col_db_cod <- names(db)[grep("DANE", names(db))[1]]
db[, `:=`(anio = suppressWarnings(as.integer(get(col_db_yr))),
          cod_mpio = suppressWarnings(pad5(get(col_db_cod))),
          prop_rural = suppressWarnings(as.numeric(`Propiedad Rural`)))]
db <- db[anio %in% anios_conf & cod_mpio %in% cods_validos,
         .(casos_db_80_12 = .N, casos_db_rural_80_12 = sum(prop_rural > 0, na.rm = TRUE)), by = cod_mpio]

base <- merge(base, vic, by = "cod_mpio", all.x = TRUE)
base <- merge(base, db,  by = "cod_mpio", all.x = TRUE)
# Ausencia en el registro del CNMH = cero eventos registrados (el registro es nacional).
for (v in c("victimas_80_12", "muertes_as_80_12", "casos_db_80_12", "casos_db_rural_80_12"))
  base[is.na(get(v)), (v) := 0L]

# ------------------------------------------------------------------------------
# 5. URT — Registro de Tierras Despojadas y Abandonadas Forzosamente (Ley 1448)
# ------------------------------------------------------------------------------
urt <- fread("datos/solicitudes_restitucion_urt_municipio.csv", colClasses = list(character = "cod_mpio"))
urt[, cod_mpio := pad5(cod_mpio)]
base <- merge(base, urt[, .(cod_mpio, n_solicitudes_urt, n_predios_urt)], by = "cod_mpio", all.x = TRUE)
base[is.na(n_solicitudes_urt), `:=`(n_solicitudes_urt = 0L, n_predios_urt = 0L)]

# Transformaciones (por 1.000 UPA para no confundir escala con intensidad)
base[, `:=`(
  log_urt_predios   = log1p(n_predios_urt),
  log_db_rural      = log1p(casos_db_rural_80_12),
  log_victimas      = log1p(victimas_80_12),
  urt_x1000upa      = 1000 * n_predios_urt / n_upa
)]

# ------------------------------------------------------------------------------
# 6. Guardar
# ------------------------------------------------------------------------------
setcolorder(base, c("cod_mpio", "departamento", "municipio", "cod_dpto", "tratado", "en_1960"))
fwrite(base, file.path(dir_out, "base_final.csv"))

cat(sprintf("Muestra A CNA 2014: %d municipios | tratados: %d | cero adjudicaciones: %d | en censo 1960: %d\n",
            nrow(base), sum(base$tratado), sum(base$tratado == 0), sum(base$en_1960)))
cat(sprintf("Intensidad > 1: %d municipios (acotados en intens_c)\n", sum(base$intens > 1, na.rm = TRUE)))
cat(sprintf("Faltantes de coordenadas: %d | de controles CEDE: %d\n",
            sum(is.na(base$lat)), sum(is.na(base$log_altitud))))
