# Muestras A y B del mapa. Correr desde la raiz del repo; despues: python scripts/04_cruce_censos/09_mapa_muestras.py
cs <- read.csv("datos/cs_tratamiento_cna.csv", colClasses=c(cod_mpio="character", cod_dpto="character"))
cs$intens <- cs$ha_adj/cs$area_mpio_ha
v <- c("gini","antiguedad","intens","share_peq","log_area_mpio","log_altitud","log_dist_capital","anio_creacion","cod_dpto")
a <- cs[complete.cases(cs[,v]),]; tb <- table(a$cod_dpto); a <- a[a$cod_dpto %in% names(tb)[tb>1],]
cat("Panel A (tabla principal):", nrow(a), "municipios,", length(unique(a$cod_dpto)), "departamentos\n")
d <- read.csv("datos/panel_censos_1960_2014.csv", colClasses=c(cod_mpio="character"))
t <- read.csv("datos/tratamiento_periodos.csv", colClasses=c(cod_mpio="character"))
dup <- intersect(setdiff(names(t),"cod_mpio"), names(d)); d[dup] <- NULL; d <- merge(d, t, by="cod_mpio", all.x=TRUE)
m <- d[d$muestra_a==1 & d$en_1960 %in% c(TRUE,"TRUE") & !is.na(d$gini) & !is.na(d$share_admin) & !is.na(d$area_mpio_ha) & d$flag_n_menor20==0,]
P10 <- c("s_pA","s_p03_conserv","s_p04_funcsocial","s_p05_aparceria","s_p06_corporat","s_p07_incora","s_p08_chicoral","s_p09_freno","s_p10_mercado","s_pZ")
CF <- c("log_altitud","log_dist_capital","log_area_mpio","log_pob_rural_1993","anio_creacion")
m <- m[complete.cases(m[, c("gini",P10,CF,"depto")]),]; tb <- table(m$depto); m <- m[m$depto %in% names(tb)[tb>1],]
cat("Panel B (censo 1960):", nrow(m), "municipios,", length(unique(m$depto)), "departamentos\n")
cat("B dentro de A:", sum(m$cod_mpio %in% a$cod_mpio), "\n")
write.csv(data.frame(cod_mpio=a$cod_mpio), "datos/muestraA.csv", row.names=FALSE)
write.csv(data.frame(cod_mpio=m$cod_mpio), "datos/muestraB.csv", row.names=FALSE)
write.csv(data.frame(cod_mpio=cs$cod_mpio), "datos/con_adjudicacion_cna.csv", row.names=FALSE)
