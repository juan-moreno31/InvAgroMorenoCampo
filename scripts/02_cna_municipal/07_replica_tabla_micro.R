suppressPackageStartupMessages({library(sandwich); library(lmtest)})
cs <- read.csv("datos/cs_tratamiento_cna.csv", colClasses=c(cod_mpio="character"))
cs$intens <- cs$ha_adj / cs$area_mpio_ha
cs$cod_dpto <- factor(cs$cod_dpto)
vars <- c("gini","p90_share","antiguedad","intens","share_peq","log_area_mpio","log_altitud","log_dist_capital","anio_creacion","cod_dpto")
d <- cs[complete.cases(cs[, vars]), ]
cat("N =", nrow(d), "\n")
# SE agrupados al estilo fixest: ajuste G/(G-1) * (n-1)/(n-K), K sin contar EF anidados en el clúster
se_fx <- function(m, d, kfe) {
  G <- length(unique(d$cod_dpto)); n <- nobs(m); K <- length(coef(m)) - kfe
  V <- vcovCL(m, cluster = ~cod_dpto, type = "HC0", cadjust = FALSE) * (G/(G-1)) * ((n-1)/(n-K))
  sqrt(diag(V))
}
fit <- function(f, lab) {
  m <- lm(f, data = d); kfe <- nlevels(droplevels(d$cod_dpto)) - 1
  s <- se_fx(m, d, kfe); b <- coef(m); k <- c("antiguedad","intens","share_peq")
  G <- length(unique(d$cod_dpto)); p <- 2*pt(-abs(b[k]/s[k]), df = G-1)
  r2 <- summary(m)$r.squared
  cat(sprintf("%-22s", lab), paste(sprintf("%s=%.4f (%.4f) p=%.3f", k, b[k], s[k], p), collapse=" | "), sprintf("R2=%.3f\n", r2))
}
fit(gini ~ antiguedad + intens + share_peq + cod_dpto, "(1) Gini sin controles")
fit(gini ~ antiguedad + intens + share_peq + log_area_mpio + log_altitud + log_dist_capital + anio_creacion + cod_dpto, "(2) Gini controles")
fit(p90_share ~ antiguedad + intens + share_peq + log_area_mpio + log_altitud + log_dist_capital + anio_creacion + cod_dpto, "(3) P90 controles")
