# ==============================================================================
# SCRIPT 04/03: TRATAMIENTO DE BALDIOS POR PERIODO NORMATIVO
# ==============================================================================
# Periodizacion de Villaveces y Sanchez — los mismos autores de la serie de
# resoluciones. Usarla es preferible a inventar ventanas: es la periodizacion que
# el dato fue construido para soportar, y es citable.
#
# Fronteras: el anio que aparece en dos filas de la tabla original se asigna al
# periodo que INICIA en el, de modo que la particion no se solapa y suma 100%.
#
# Cambios respecto de 03_adjudicaciones_periodos.R:
#  1. Particion EXHAUSTIVA (antes los 4 periodos dejaban buena parte de las
#     hectareas en el error). Ojo: el Pacto de Chicoral 1973-1988 estaba
#     completamente fuera del esquema anterior.
#  2. NORMALIZA por area municipal (ha_adj esta correlacionada con area_mpio_ha).
#  3. NO sobreescribe el panel: escribe archivo aparte.
#
# Entradas : datos/agg_vs_1901_2013.csv, datos/panel_censos_1960_2014.csv
# Salida   : datos/tratamiento_periodos.csv
# ==============================================================================

suppressPackageStartupMessages(library(data.table))

PERIODOS <- list(
  p01_pre1905    = c(1901, 1904),  # se privilegia el latifundio
  p02_reyes      = c(1905, 1911),  # periodo Reyes, modernizacion
  p03_conserv    = c(1912, 1929),  # Republica Conservadora: baldios en grandes extensiones
  p04_funcsocial = c(1930, 1943),  # Ley 200/1936, funcion social de la propiedad
  p05_aparceria  = c(1944, 1958),  # se contrarresta la funcion social; se acepta aparceria
  p06_corporat   = c(1959, 1960),  # corporativismo
  p07_incora     = c(1961, 1972),  # Ley 135/1961, adjudicacion a colonos
  p08_chicoral   = c(1973, 1987),  # Pacto de Chicoral, compras para adjudicacion
  p09_freno      = c(1988, 1993),  # freno a la colonizacion dirigida
  p10_mercado    = c(1994, 2006),  # Ley 160/1994, mercado de tierras
  p11_contratos  = c(2007, 2009),  # contratos por proyectos de explotacion y riegos
  p12_restit     = c(2010, 2013)   # restitucion (la serie termina en 2013)
)
# Agrupaciones para estimacion: p01+p02 y p11+p12 tienen muy pocas ha cada uno
AGRUPA <- list(pA = c("p01_pre1905", "p02_reyes"), pZ = c("p11_contratos", "p12_restit"))

a <- fread("datos/agg_vs_1901_2013.csv", colClasses = list(character = "cod_mpio"))
a[, cod_mpio := formatC(as.integer(cod_mpio), width = 5, flag = "0")]
d <- fread("datos/panel_censos_1960_2014.csv", colClasses = list(character = "cod_mpio"))

cubierto <- sum(sapply(PERIODOS, function(w) a[anio >= w[1] & anio <= w[2], sum(ha_adj)]))
stopifnot(abs(cubierto - a[, sum(ha_adj)]) < 1e-6)
cat(sprintf(">>> Particion exhaustiva verificada (%d-%d): 100%% de las hectareas\n",
            min(a$anio), max(a$anio)))

trat <- a[, .(ha_total = sum(ha_adj), n_res_total = sum(n_res)), by = cod_mpio]
for (nm in names(PERIODOS)) {
  w <- PERIODOS[[nm]]
  tmp <- a[anio >= w[1] & anio <= w[2], .(x = sum(ha_adj)), by = cod_mpio]
  setnames(tmp, "x", paste0("ha_", nm)); trat <- merge(trat, tmp, by = "cod_mpio", all.x = TRUE)
}
tmp <- a[anio <= 1960, .(ha_hasta_1960 = sum(ha_adj)), by = cod_mpio]
trat <- merge(trat, tmp, by = "cod_mpio", all.x = TRUE)

out <- merge(d[, .(cod_mpio, area_mpio_ha, n_upa)], trat, by = "cod_mpio", all.x = TRUE)
cols_ha <- grep("^(ha_|n_res_)", names(out), value = TRUE)
for (j in cols_ha) set(out, which(is.na(out[[j]]) & !is.na(out$n_upa)), j, 0)

for (nm in names(AGRUPA))
  set(out, NULL, paste0("ha_", nm), rowSums(out[, paste0("ha_", AGRUPA[[nm]]), with = FALSE]))

# normalizacion por area municipal
for (j in grep("^ha_", names(out), value = TRUE)) {
  k <- sub("^ha_", "", j)
  set(out, NULL, paste0("r_", k), out[[j]] / out$area_mpio_ha)
  set(out, NULL, paste0("s_", k), asinh(out[[j]] / out$area_mpio_ha))
}
out[, `:=`(tratado      = as.integer(ha_total > 0),
           tratado_1960 = as.integer(ha_hasta_1960 > 0),
           flag_ratio_gt1 = as.integer(r_total > 1))]

cat("\n=== COMPOSICION DEL TRATAMIENTO ===\n")
tt <- out[, sum(ha_total, na.rm = TRUE)]
for (nm in names(PERIODOS)) {
  s <- out[, sum(get(paste0("ha_", nm)), na.rm = TRUE)]
  nz <- out[get(paste0("ha_", nm)) > 0, .N]
  cat(sprintf("  %-15s %d-%d : %12s (%4.1f%%)  %3d mpios\n", nm,
              PERIODOS[[nm]][1], PERIODOS[[nm]][2],
              format(round(s), big.mark = ","), 100 * s / tt, nz))
}
cat(sprintf("\n  tratados alguna vez: %d | tratados hasta 1960: %d | razon>1: %d\n",
            out[, sum(tratado, na.rm = TRUE)], out[, sum(tratado_1960, na.rm = TRUE)],
            out[, sum(flag_ratio_gt1, na.rm = TRUE)]))

fwrite(out[, !c("n_upa", "area_mpio_ha")], "datos/tratamiento_periodos.csv")
cat("\n>>> datos/tratamiento_periodos.csv  (el panel NO se toco)\n")
