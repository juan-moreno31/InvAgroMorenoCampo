# ==============================================================================
# MODELO FINAL — 02: ESTIMACIÓN
# ==============================================================================
# Corte transversal municipal, CNA 2014. MCO con efectos fijos de departamento.
#
#   Y_m = b1·Antigüedad_m (décadas) + b2·Intensidad_m + X_m'g + a_d + e_m
#
# Principios de la especificación (ver LEEME.md):
#   * Solo controles PREDETERMINADOS en la ecuación principal (geografía CEDE,
#     año de fundación y composición de la propia adjudicación).
#   * Sin share_menor5 (2014): es un componente mecánico del Gini de 2014.
#   * URT, CNMH y población rural de 1993 son posteriores al tratamiento: se
#     estiman como VARIABLES DEPENDIENTES (canales) y solo se agregan como
#     controles en una columna de robustez, rotulada como tal.
#   * Antigüedad vs. cohorte: con un solo corte, antigüedad = 2014 − año medio.
#     La columna (3) controla por la composición de régimen legal de lo
#     adjudicado; b1 queda identificado por el momento DENTRO de cada régimen.
#   * Inferencia: clúster por departamento, Conley 100 y 150 km y wild cluster
#     bootstrap restringido (WCR, pesos de Webb, 9.999 réplicas), porque hay
#     pocos clústeres (30 departamentos efectivos).
#
# Ejecutar con el directorio de trabajo en la raíz del proyecto.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

dir_res <- "modelo_final/resultados"
dir.create(dir_res, recursive = TRUE, showWarnings = FALSE)
set.seed(20141001)
options(warn = 1)

b <- fread("modelo_final/datos/base_final.csv",
           colClasses = list(character = c("cod_mpio", "cod_dpto")))

# Muestra principal: municipios con al menos una adjudicación (antigüedad definida)
d  <- b[tratado == 1 & !is.na(antig_dec) & !is.na(gini)]
# Submuestra con estructura agraria de 1960 completa
d60 <- d[en_1960 == 1 & !is.na(gini_hato_1960) & !is.na(share_admin_1960) & !is.na(log_tam_expl_1960)]

setFixest_dict(c(
  gini = "Gini de tierra 2014", p90_share = "Participación del 10% superior 2014",
  antig_dec = "Antigüedad de la titulación (décadas)",
  intens_c = "Intensidad de adjudicación (ha adj./ha mpio, acotada)",
  intens = "Intensidad de adjudicación (sin acotar)",
  log1p_intens = "log(1 + intensidad)",
  share_peq = "Proporción del área adjudicada en predios < 5 ha",
  log_tam_adj = "log tamaño medio adjudicado",
  share_l200 = "Proporción adjudicada 1936–1960", share_incora = "Proporción adjudicada 1961–1987",
  share_l30 = "Proporción adjudicada 1988–1993", share_l160 = "Proporción adjudicada 1994–2013",
  gini_hato_1960 = "Gini del hato bovino 1960", share_admin_1960 = "Superficie bajo administrador 1960",
  log_tam_expl_1960 = "log tamaño medio de explotación 1960",
  log_dens_bovina_1960 = "log cabezas bovinas por ha 1960",
  log_urt_predios = "log(1 + predios reclamados URT)",
  log_db_rural = "log(1 + casos CNMH daño a bienes rurales 1980–2012)",
  log_victimas = "log(1 + víctimas fatales CNMH 1980–2012)",
  log_pob_rural_1993 = "log población rural 1993",
  sin_adj = "Sin adjudicaciones (1 = sí)",
  log_area_mpio = "log área municipal", log_altitud = "log altitud",
  log_dist_capital = "log distancia a la capital", anio_creacion = "Año de fundación",
  cod_dpto = "Departamento"
))

geo     <- "log_area_mpio + log_altitud + log_dist_capital + anio_creacion"
compos  <- "share_peq + log_tam_adj"
regimen <- "share_l200 + share_incora + share_l30 + share_l160"   # base: pre-1936
est1960 <- "gini_hato_1960 + share_admin_1960 + log_tam_expl_1960 + log_dens_bovina_1960"
post    <- "log_urt_predios + log_db_rural + log_victimas + log_pob_rural_1993"

f <- function(y, rhs) as.formula(paste(y, "~", rhs, "| cod_dpto"))
est <- function(y, rhs, data) feols(f(y, rhs), data = data, cluster = ~cod_dpto)

guardar_tabla <- function(lista, archivo, titulo, headers = NULL) {
  etable(lista, tex = TRUE, file = file.path(dir_res, archivo), replace = TRUE,
         title = titulo, headers = headers, se.below = TRUE,
         signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
         fitstat = ~ n + r2 + wr2)
  print(etable(lista, headers = headers, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
               fitstat = ~ n + r2 + wr2))
}

# ==============================================================================
# TABLA 1 y 2 — ESPECIFICACIÓN PRINCIPAL (Gini y P90)
# ==============================================================================
rhs1 <- paste("antig_dec + intens_c +", geo)
rhs2 <- paste(rhs1, "+", compos)                 # <- ESPECIFICACIÓN PRINCIPAL
rhs3 <- paste(rhs2, "+", regimen)                # antigüedad dentro de régimen

hdr <- c("Base", "Principal", "+ Régimen legal")
t1 <- list(est("gini", rhs1, d), est("gini", rhs2, d), est("gini", rhs3, d))
t2 <- list(est("p90_share", rhs1, d), est("p90_share", rhs2, d), est("p90_share", rhs3, d))
cat("\n==== TABLA 1: GINI 2014 ====\n");  guardar_tabla(t1, "tabla1_gini.tex", "Titulación y Gini de tierra 2014", hdr)
cat("\n==== TABLA 2: P90 2014 ====\n");   guardar_tabla(t2, "tabla2_p90.tex",  "Titulación y participación del 10\\% superior", hdr)

# ==============================================================================
# TABLA 3 — SUBMUESTRA 1960: CONTROL POR ESTRUCTURA AGRARIA INICIAL
# ==============================================================================
t3 <- list(
  est("gini",      rhs2, d60),
  est("gini",      paste(rhs2, "+", est1960), d60),
  est("gini",      paste(rhs3, "+", est1960), d60),
  est("p90_share", paste(rhs2, "+", est1960), d60)
)
cat("\n==== TABLA 3: SUBMUESTRA 1960 ====\n")
guardar_tabla(t3, "tabla3_estructura_1960.tex", "Control por la estructura agraria de 1960",
              c("Gini: principal", "Gini: + 1960", "Gini: + 1960 + régimen", "P90: + 1960"))

# ==============================================================================
# TABLA 4 — VIOLENCIA Y RESTITUCIÓN COMO CANALES (variables dependientes)
# ==============================================================================
t4 <- list(
  est("log_urt_predios", rhs2, d),
  est("log_db_rural",    rhs2, d),
  est("log_victimas",    rhs2, d),
  est("gini",            paste(rhs2, "+", post), d)
)
cat("\n==== TABLA 4: CANALES POSTERIORES AL TRATAMIENTO ====\n")
guardar_tabla(t4, "tabla4_violencia_restitucion.tex",
              "Despojo, violencia y restitución: canales posteriores a la titulación",
              c("Predios URT", "Daño a bienes CNMH", "Víctimas CNMH", "Gini + post-tratamiento"))

# ==============================================================================
# TABLA 5 — ROBUSTEZ DE LA ESPECIFICACIÓN PRINCIPAL (Gini)
# ==============================================================================
bz <- copy(b[!is.na(gini)])
bz[, sin_adj := as.integer(tratado == 0)]
bz[tratado == 0, `:=`(antig_dec = 0, share_peq = 0, log_tam_adj = 0)]  # absorbidos por sin_adj

t5 <- list(
  est("gini", sub("intens_c", "intens", rhs2), d),
  est("gini", sub("intens_c", "log1p_intens", rhs2), d),
  est("gini", paste(rhs2, "+ sin_adj"), bz),
  est("gini", rhs2, d[muestra_b == 1]),
  est("gini_corr", rhs2, d),
  est("gini", rhs2, d[share_ha_etnica <= 0.5])
)
cat("\n==== TABLA 5: ROBUSTEZ ====\n")
guardar_tabla(t5, "tabla5_robustez.tex", "Robustez de la especificación principal (Gini 2014)",
              c("Intens. sin acotar", "log(1+intens.)", "Incluye ceros", "Muestra B",
                "Gini corregido", "Étnico <= 50%"))

# ==============================================================================
# TABLA 6 — INFERENCIA: clúster, Conley 100/150 km y wild cluster bootstrap
# ==============================================================================
# WCR (Cameron, Gelbach y Miller, 2008; MacKinnon y Webb, 2017): se impone
# H0: b_k = 0, se remuestrean los residuos restringidos con pesos de Webb por
# departamento y se compara el t de clúster original con su distribución.
# Los EF de departamento están anidados en el clúster, así que basta con
# trabajar con las variables centradas por departamento (FWL).
wcr_pvalue <- function(model, var, B = 9999) {
  dd   <- model.matrix(model, type = "rhs")
  yv   <- model.matrix(model, type = "lhs")
  g    <- model$fixef_id$cod_dpto
  demean <- function(v) v - ave(v, g)
  X <- apply(dd, 2, demean); y <- demean(as.numeric(yv))
  k <- which(colnames(X) == var)
  G <- sort(unique(g)); ng <- length(G); idx <- split(seq_along(g), g)

  t_crve <- function(X, y) {
    XtXi <- solve(crossprod(X)); bh <- XtXi %*% crossprod(X, y); e <- y - X %*% bh
    meat <- Reduce(`+`, lapply(idx, function(i) { s <- crossprod(X[i, , drop = FALSE], e[i]); s %*% t(s) }))
    V <- XtXi %*% meat %*% XtXi
    bh[k] / sqrt(V[k, k])
  }
  t0 <- t_crve(X, y)
  Xr <- X[, -k, drop = FALSE]
  br <- solve(crossprod(Xr), crossprod(Xr, y)); fr <- Xr %*% br; ur <- y - fr
  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  gpos <- match(g, G)
  tb <- vapply(seq_len(B), function(r) {
    w <- sample(webb, ng, replace = TRUE)
    t_crve(X, fr + w[gpos] * ur)
  }, numeric(1))
  mean(abs(tb) >= abs(t0))
}

inferencia <- function(model, etiqueta) {
  out <- rbindlist(lapply(c("antig_dec", "intens_c"), function(v) {
    se_cl  <- se(model)[v]
    se_c1  <- se(summary(model, vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 100)))[v]
    se_c2  <- se(summary(model, vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 150)))[v]
    p_cl   <- pvalue(model)[v]
    p_c1   <- pvalue(summary(model, vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 100)))[v]
    p_c2   <- pvalue(summary(model, vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 150)))[v]
    data.table(modelo = etiqueta, variable = v, coef = coef(model)[v],
               se_cluster = se_cl, p_cluster = p_cl,
               se_conley100 = se_c1, p_conley100 = p_c1,
               se_conley150 = se_c2, p_conley150 = p_c2,
               p_wcr_webb = wcr_pvalue(model, v))
  }))
  out
}

inf <- rbind(
  inferencia(t1[[2]], "Gini — principal"),
  inferencia(t1[[3]], "Gini — + régimen"),
  inferencia(t2[[2]], "P90 — principal"),
  inferencia(t3[[3]], "Gini — 1960 + régimen")
)
cat("\n==== TABLA 6: INFERENCIA ALTERNATIVA ====\n")
print(inf, digits = 4)
fwrite(inf, file.path(dir_res, "tabla6_inferencia.csv"))

# ==============================================================================
# MAGNITUDES ESTANDARIZADAS (especificación principal)
# ==============================================================================
mag <- rbindlist(lapply(list(list(t1[[2]], "gini"), list(t2[[2]], "p90_share")), function(z) {
  m <- z[[1]]; y <- z[[2]]
  rbindlist(lapply(c("antig_dec", "intens_c"), function(v) data.table(
    dependiente = y, variable = v, coef = coef(m)[v],
    sd_x = sd(d[[v]]), sd_y = sd(d[[y]]),
    efecto_1sd = coef(m)[v] * sd(d[[v]]),
    efecto_1sd_en_sd_y = coef(m)[v] * sd(d[[v]]) / sd(d[[y]])
  )))
}))
cat("\n==== MAGNITUDES ====\n"); print(mag, digits = 3)
fwrite(mag, file.path(dir_res, "magnitudes_estandarizadas.csv"))

# Coeficientes de todas las tablas en formato largo
todas <- c(setNames(t1, paste0("T1_", 1:3)), setNames(t2, paste0("T2_", 1:3)),
           setNames(t3, paste0("T3_", 1:4)), setNames(t4, paste0("T4_", 1:4)),
           setNames(t5, paste0("T5_", 1:6)))
coefs <- rbindlist(lapply(names(todas), function(n) {
  ct <- coeftable(todas[[n]])
  data.table(modelo = n, variable = rownames(ct), coef = ct[, 1], se_cluster = ct[, 2],
             p = ct[, 4], n = nobs(todas[[n]]))
}))
fwrite(coefs, file.path(dir_res, "coeficientes_todas_las_tablas.csv"))

cat("\nResultados guardados en", dir_res, "\n")
