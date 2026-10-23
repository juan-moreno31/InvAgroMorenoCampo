# Réplica de persistencia (Latif_60 -> Gini 2014) y placebo (medidas 1960 ~ adjudicación post-1960). Correr desde la raíz del repo.
suppressPackageStartupMessages({library(sandwich)})
d <- read.csv("datos/panel_censos_1960_2014.csv", colClasses=c(cod_mpio="character"))
g <- read.csv("datos/gini_hato_1960.csv", colClasses=c(cod_mpio="character"))
t <- read.csv("datos/tratamiento_periodos.csv", colClasses=c(cod_mpio="character"))
d <- merge(d, g[, c("cod_mpio","gini_hato","estado")], by="cod_mpio", all.x=TRUE)
dup <- intersect(setdiff(names(t),"cod_mpio"), names(d)); d[dup] <- NULL
d <- merge(d, t, by="cod_mpio", all.x=TRUE)
fx <- function(f, dd, var) {
  dd <- dd[complete.cases(dd[, all.vars(f)]), ]
  tb <- table(dd$depto); dd <- dd[dd$depto %in% names(tb)[tb > 1], ]   # como fixest
  dd$depto <- factor(dd$depto); m <- lm(f, data=dd)
  G <- nlevels(dd$depto); n <- nobs(m); K <- length(coef(m)) - (G-1)
  V <- vcovCL(m, cluster=dd$depto, type="HC0", cadjust=FALSE)*(G/(G-1))*((n-1)/(n-K))
  b <- coef(m)[var]; s <- sqrt(diag(V))[var]
  sprintf("b=%.4f se=%.4f t=%.2f p=%.3f N=%d", b, s, b/s, 2*pt(-abs(b/s), G-1), n)
}
CB <- "log_altitud + log_dist_capital + log_area_mpio"
CF <- paste(CB, "+ log_pob_rural_1993 + anio_creacion")
m2014 <- d[d$muestra_a==1 & d$en_1960 %in% c(TRUE,"TRUE") & d$flag_n_menor20==0, ]
cat("== PERSISTENCIA: gini 2014 ~ medida 1960\n")
for (x in c("share_admin","gini_hato")) for (cc in c(CB,CF)) {
  cat(x, "| ctrl", ifelse(cc==CB,"B","F"), ":", fx(as.formula(paste("gini ~", x, "+", cc, "+ depto")), m2014, x), "\n")
  cat(x, "+ trat 10 periodos | ctrl", ifelse(cc==CB,"B","F"), ":", fx(as.formula(paste("gini ~", x, "+ s_pA+s_p03_conserv+s_p04_funcsocial+s_p05_aparceria+s_p06_corporat+s_p07_incora+s_p08_chicoral+s_p09_freno+s_p10_mercado+s_pZ +", cc, "+ depto")), m2014, x), "\n")
}
cat("== PLACEBO: medida 1960 ~ adjudicacion post-1960\n")
m60 <- d[d$en_1960 %in% c(TRUE,"TRUE") & d$estado=="ok" & !is.na(d$share_admin) & d$flag_expl_1960_menor20==0, ]
m60$s_post <- rowSums(m60[, c("s_p07_incora","s_p08_chicoral","s_p09_freno","s_p10_mercado","s_pZ")])
for (y in c("gini_hato","share_admin")) {
  cat(y, "~ s_post + PRE + ctrlB:", fx(as.formula(paste(y,"~ s_post + s_pA+s_p03_conserv+s_p04_funcsocial+s_p05_aparceria +", CB, "+ depto")), m60, "s_post"), "\n")
  cat(y, "~ s_post + ctrlB:", fx(as.formula(paste(y,"~ s_post +", CB, "+ depto")), m60, "s_post"), "\n")
  for (v in c("s_p07_incora","s_p08_chicoral","s_p09_freno","s_p10_mercado","s_pZ"))
    cat("   ", y, v, ":", fx(as.formula(paste(y,"~ s_pA+s_p03_conserv+s_p04_funcsocial+s_p05_aparceria+s_p07_incora+s_p08_chicoral+s_p09_freno+s_p10_mercado+s_pZ +", CB, "+ depto")), m60, v), "\n")
}
