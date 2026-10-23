# ==============================================================================
# MODELO FINAL — 04: REGRESIÓN CUANTÍLICA — FIRMA DISTRIBUTIVA DEL CANAL
# ==============================================================================
# Idea: el diseño del régimen y la reconcentración por mercado dejan huellas
# distintas en la DISTRIBUCIÓN, aunque produzcan el mismo efecto medio.
#
#   Diseño del régimen  -> la antigüedad desplaza toda la distribución por igual
#                          (efecto de localización): mismo coeficiente en todos
#                          los cuantiles.
#   Mercado de tierras  -> acumulación: la antigüedad pesa más donde la tierra
#                          ya está concentrada (cuantiles altos del Gini) y
#                          engorda la cola alta de tamaños de UPA (q90–q99),
#                          sin agrandar, o achicando, la cola baja.
#
# A. Regresión cuantílica (Koenker y Bassett, 1978) del Gini y del P90 entre
#    municipios, tau = 0,10…0,90, con dummies de departamento. Inferencia por
#    bootstrap de pares por departamento (clúster), B = 999. Se prueba la
#    igualdad de pendientes entre tau = 0,90 y tau = 0,10.
# B. Distribución intramunicipal de tamaños (CNA 2014): MCO-EF del log de los
#    cuantiles q10…q99 del tamaño de UPA sobre la antigüedad.
# Ambas, con y sin control por la composición de régimen legal.
#
# Ejecutar con el directorio de trabajo en la raíz del proyecto, después de 01.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(quantreg)
  library(fixest)
  library(ggplot2)
})

dir_res <- "modelo_final/resultados"
dir.create(dir_res, recursive = TRUE, showWarnings = FALSE)
set.seed(20141002)

b <- fread("modelo_final/datos/base_final.csv", colClasses = list(character = c("cod_mpio", "cod_dpto")))
d <- b[tratado == 1 & !is.na(antig_dec) & !is.na(gini)]
# Departamentos con un solo municipio no aportan a la variación intra-departamento
d <- d[, if (.N > 1) .SD, by = cod_dpto]

ctrl    <- c("intens_c", "log_area_mpio", "log_altitud", "log_dist_capital", "anio_creacion",
             "share_peq", "log_tam_adj")
regimen <- c("share_l200", "share_incora", "share_l30", "share_l160")
taus    <- c(0.10, 0.25, 0.50, 0.75, 0.90)
B       <- 999

# ==============================================================================
# A. REGRESIÓN CUANTÍLICA ENTRE MUNICIPIOS
# ==============================================================================
qr_fit <- function(y, xs, data, tau) {
  f <- as.formula(paste(y, "~", paste(c("antig_dec", xs), collapse = " + "), "+ factor(cod_dpto)"))
  suppressWarnings(coef(rq(f, tau = tau, data = data, method = "br"))["antig_dec"])
}

qr_boot <- function(y, xs, data) {
  est <- sapply(taus, function(t) qr_fit(y, xs, data, t))
  deps <- unique(data$cod_dpto)
  bt <- t(replicate(B, {
    s  <- sample(deps, length(deps), replace = TRUE)
    db <- rbindlist(lapply(seq_along(s), function(i) data[cod_dpto == s[i]][, cod_dpto := paste0(s[i], "_", i)]))
    sapply(taus, function(t) tryCatch(qr_fit(y, xs, db, t), error = function(e) NA_real_))
  }))
  se <- apply(bt, 2, sd, na.rm = TRUE)
  dif  <- est[length(taus)] - est[1]
  dif_b <- bt[, length(taus)] - bt[, 1]
  list(
    tabla = data.table(tau = taus, coef = est, se = se,
                       ic_inf = apply(bt, 2, quantile, 0.025, na.rm = TRUE),
                       ic_sup = apply(bt, 2, quantile, 0.975, na.rm = TRUE),
                       p = 2 * pnorm(-abs(est / se))),
    dif = data.table(dif_q90_q10 = dif, se = sd(dif_b, na.rm = TRUE),
                     p = 2 * pnorm(-abs(dif / sd(dif_b, na.rm = TRUE))))
  )
}

ols_ref <- function(y, xs, data) {
  m <- feols(as.formula(paste(y, "~", paste(c("antig_dec", xs), collapse = " + "), "| cod_dpto")),
             data = data, cluster = ~cod_dpto)
  c(coef = unname(coef(m)["antig_dec"]), se = unname(se(m)["antig_dec"]))
}

specs <- list(
  list(y = "gini",      xs = ctrl,              lab = "Gini — principal"),
  list(y = "gini",      xs = c(ctrl, regimen),  lab = "Gini — + régimen"),
  list(y = "p90_share", xs = ctrl,              lab = "P90 — principal"),
  list(y = "p90_share", xs = c(ctrl, regimen),  lab = "P90 — + régimen")
)

resA <- list(); difA <- list()
for (s in specs) {
  cat("Bootstrap cuantílico:", s$lab, "\n")
  r <- qr_boot(s$y, s$xs, d)
  o <- ols_ref(s$y, s$xs, d)
  resA[[s$lab]] <- cbind(especificacion = s$lab, r$tabla, mco = o["coef"], se_mco = o["se"])
  difA[[s$lab]] <- cbind(especificacion = s$lab, r$dif)
}
resA <- rbindlist(resA); difA <- rbindlist(difA)
cat("\n==== A. REGRESIÓN CUANTÍLICA: efecto de una década de antigüedad ====\n"); print(resA, digits = 3)
cat("\nPrueba de localización pura (pendiente tau 0,90 = tau 0,10):\n"); print(difA, digits = 3)
fwrite(resA, file.path(dir_res, "tabla10_cuantilica_gini_p90.csv"))
fwrite(difA, file.path(dir_res, "tabla10b_prueba_localizacion.csv"))

# ==============================================================================
# B. DISTRIBUCIÓN INTRAMUNICIPAL DE TAMAÑOS DE UPA (CNA 2014)
# ==============================================================================
pcts <- c(10, 25, 50, 75, 90, 95, 99)
for (p in pcts) d[, (paste0("lq", p)) := log(get(paste0("q", p)))]
d[, l_media := log(area_media)]

resB <- rbindlist(lapply(list(list(ctrl, "Principal"), list(c(ctrl, regimen), "+ régimen")), function(sp) {
  rbindlist(lapply(c(paste0("lq", pcts), "l_media"), function(y) {
    m <- feols(as.formula(paste(y, "~", paste(c("antig_dec", sp[[1]]), collapse = " + "), "| cod_dpto")),
               data = d, cluster = ~cod_dpto)
    data.table(especificacion = sp[[2]], dependiente = y,
               percentil = suppressWarnings(as.integer(sub("lq", "", y))),
               coef = coef(m)["antig_dec"], se = se(m)["antig_dec"], p = pvalue(m)["antig_dec"], n = nobs(m))
  }))
}))
resB[, `:=`(ic_inf = coef - 1.96 * se, ic_sup = coef + 1.96 * se, efecto_pct = 100 * (exp(coef) - 1))]
cat("\n==== B. log tamaño de UPA por percentil: efecto de una década de antigüedad ====\n"); print(resB, digits = 3)
fwrite(resB, file.path(dir_res, "tabla11_cuantiles_tamano_upa.csv"))

# Diferencia cola alta − cola baja (q99 − q10) con régimen: misma regresión sobre log(q99/q10)
d[, l_q99_q10 := log(q99 / q10)]; d[, l_q90_q50 := log(q90 / q50)]
dispB <- rbindlist(lapply(c("l_q99_q10", "l_q90_q50"), function(y) rbindlist(lapply(
  list(list(ctrl, "Principal"), list(c(ctrl, regimen), "+ régimen")), function(sp) {
    m <- feols(as.formula(paste(y, "~", paste(c("antig_dec", sp[[1]]), collapse = " + "), "| cod_dpto")),
               data = d, cluster = ~cod_dpto)
    data.table(dependiente = y, especificacion = sp[[2]], coef = coef(m)["antig_dec"],
               se = se(m)["antig_dec"], p = pvalue(m)["antig_dec"])
  }))))
cat("\nDispersión de tamaños (log de razones de percentiles):\n"); print(dispB, digits = 3)
fwrite(dispB, file.path(dir_res, "tabla11b_dispersion_tamanos.csv"))

# ==============================================================================
# FIGURAS
# ==============================================================================
col <- c("Principal" = "#2a6f97", "+ régimen" = "#c26a2e")
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_blank(),
        plot.caption = element_text(hjust = 0, colour = "grey40", size = 8))

fa <- copy(resA)
fa[, `:=`(dep = fifelse(grepl("^Gini", especificacion), "Gini de tierra 2014", "Participación del 10% superior"),
          esp = factor(fifelse(grepl("régimen", especificacion), "+ régimen", "Principal"), levels = c("Principal", "+ régimen")))]
gA <- ggplot(fa, aes(tau, coef, colour = esp, fill = esp)) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 2.2) +
  facet_wrap(~dep, scales = "free_y") +
  scale_colour_manual(values = col) + scale_fill_manual(values = col) +
  scale_x_continuous(breaks = taus) +
  labs(x = "Cuantil condicional (tau)", y = "Efecto de una década de antigüedad",
       caption = "Regresión cuantílica con dummies de departamento. IC 95 % por bootstrap de pares por departamento (B = 999).\nFuentes: DANE (2014), Villaveces Niño y Sánchez (2014), CEDE (2022).") +
  tema
ggsave(file.path(dir_res, "figura_cuantilica_gini_p90.png"), gA, width = 8.5, height = 4.3, dpi = 300)

fb <- resB[!is.na(percentil)]
fb[, especificacion := factor(especificacion, levels = c("Principal", "+ régimen"))]
gB <- ggplot(fb, aes(percentil, coef, colour = especificacion, fill = especificacion)) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 2.2) +
  scale_colour_manual(values = col) + scale_fill_manual(values = col) +
  scale_x_continuous(breaks = pcts) +
  labs(x = "Percentil del tamaño de UPA dentro del municipio (CNA 2014)",
       y = "Efecto de una década en log(tamaño)",
       caption = "MCO con EF de departamento, clúster por departamento, IC 95 %.\nFuentes: DANE (2014), Villaveces Niño y Sánchez (2014), CEDE (2022).") +
  tema
ggsave(file.path(dir_res, "figura_percentiles_tamano_upa.png"), gB, width = 7.5, height = 4.5, dpi = 300)

cat("\nResultados guardados en", dir_res, "\n")
