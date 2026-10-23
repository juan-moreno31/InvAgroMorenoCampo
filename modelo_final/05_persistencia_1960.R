# ==============================================================================
# MODELO FINAL — 05: ¿TIEMPO O PERSISTENCIA DE LA ESTRUCTURA PREVIA?
# ==============================================================================
# Amenaza: los municipios titulados hace más tiempo podrían haber tenido YA
# predios más pequeños y más desiguales antes de la titulación. Entonces el
# gradiente de antigüedad reflejaría persistencia, no decaimiento.
#
# Problema de diseño: controlar por la estructura de 1960 es un mal control
# para las adjudicaciones ANTERIORES a 1960, porque esa estructura ya sería
# resultado de ellas. La prueba limpia separa:
#   * antig61_dec: antigüedad (décadas a 2014, ponderada por área) de lo
#     adjudicado DESDE 1961, para la que 1960 sí es línea base;
#   * intens61 e intens_pre61: intensidad posterior y anterior a 1961.
#
# Además, una parte de los municipios tratados no aparece en el Censo de 1960
# (frontera de colonización, fundados en su mayoría después de 1950). En ellos
# no hay línea base; se reporta el gradiente por separado.
#
# Ejecutar con el directorio de trabajo en la raíz del proyecto, después de 01.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

dir_res <- "modelo_final/resultados"
b <- fread("modelo_final/datos/base_final.csv", colClasses = list(character = c("cod_mpio", "cod_dpto")))
a <- fread("datos/agg_vs_1901_2013.csv", colClasses = list(character = "cod_mpio"))
a[, cod_mpio := formatC(as.integer(cod_mpio), width = 5, flag = "0")]

# Adjudicaciones desde 1961 y antes de 1961
p61 <- a[anio >= 1961 & ha_adj > 0,
         .(antig61_dec = (2014 - weighted.mean(anio, ha_adj)) / 10, ha_adj61 = sum(ha_adj)), by = cod_mpio]
pre <- a[anio <= 1960, .(ha_pre61 = sum(ha_adj)), by = cod_mpio]

d <- b[tratado == 1 & !is.na(antig_dec) & !is.na(gini)]
d <- merge(d, p61, by = "cod_mpio", all.x = TRUE)
d <- merge(d, pre, by = "cod_mpio", all.x = TRUE)
d[is.na(ha_adj61), ha_adj61 := 0]; d[is.na(ha_pre61), ha_pre61 := 0]
d[, `:=`(intens61 = pmin(ha_adj61 / area_mpio_ha, 1), intens_pre61 = pmin(ha_pre61 / area_mpio_ha, 1),
         lq10 = log(q10), lq50 = log(q50), lq90 = log(q90), lq99 = log(q99), l9910 = log(q99 / q10),
         frontera = 1L - en_1960)]

ctrl    <- "intens_c + log_area_mpio + log_altitud + log_dist_capital + anio_creacion + share_peq + log_tam_adj"
ctrl61  <- "intens61 + intens_pre61 + log_area_mpio + log_altitud + log_dist_capital + anio_creacion + share_peq + log_tam_adj"
e60     <- "gini_hato_1960 + share_admin_1960 + log_tam_expl_1960 + log_dens_bovina_1960"
reg     <- "share_l200 + share_incora + share_l30 + share_l160"
reg61   <- "share_l30 + share_l160"
ys      <- c("gini", "p90_share", "lq10", "lq50", "lq90", "lq99", "l9910")

fila <- function(y, x, rhs, data, etiqueta) {
  m <- feols(as.formula(paste(y, "~", x, "+", rhs, "| cod_dpto")), data = data, cluster = ~cod_dpto)
  data.table(especificacion = etiqueta, dependiente = y, variable = x,
             coef = coef(m)[x], se = se(m)[x], p = pvalue(m)[x], n = nobs(m))
}

# A. Muestra con Censo 1960: antigüedad post-1960 con línea base 1960
d60 <- d[en_1960 == 1 & !is.na(gini_hato_1960) & !is.na(log_tam_expl_1960) & !is.na(antig61_dec)]
resA <- rbindlist(lapply(ys, function(y) rbind(
  fila(y, "antig_dec",   ctrl, d60, "1960: antigüedad total, sin 1960"),
  fila(y, "antig_dec",   paste(ctrl, "+", e60), d60, "1960: antigüedad total, + estructura 1960"),
  fila(y, "antig61_dec", ctrl61, d60, "1960: antigüedad post-1960, sin 1960"),
  fila(y, "antig61_dec", paste(ctrl61, "+", e60), d60, "1960: antigüedad post-1960, + estructura 1960"),
  fila(y, "antig61_dec", paste(ctrl61, "+", e60, "+", reg61), d60, "1960: antigüedad post-1960, + 1960 + régimen")
)))

# B. Frontera vs. municipios con censo 1960 (antigüedad total)
resB <- rbindlist(lapply(ys, function(y) rbind(
  fila(y, "antig_dec", ctrl, d[frontera == 0], "Municipios en censo 1960"),
  fila(y, "antig_dec", ctrl, d[frontera == 1], "Frontera (no en censo 1960)"),
  fila(y, "antig_dec", paste(ctrl, "+", reg), d[frontera == 1], "Frontera, + régimen")
)))

res <- rbind(resA, resB)
cat("\n==== PERSISTENCIA 1960 Y FRONTERA ====\n"); print(res, digits = 3, nrows = 200)
fwrite(res, file.path(dir_res, "tabla12_persistencia_1960_frontera.csv"))

cat(sprintf("\nFrontera: %d de %d municipios tratados; año de fundación mediano %d (vs. %d)\n",
            sum(d$frontera), nrow(d), as.integer(median(d[frontera == 1]$anio_creacion)),
            as.integer(median(d[frontera == 0]$anio_creacion))))
cat("\nResultados guardados en", dir_res, "\n")
