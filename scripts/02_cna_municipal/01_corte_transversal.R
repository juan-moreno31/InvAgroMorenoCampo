# ==============================================================================
# Preparación de Corte Transversal: CNA 2014, Controles CEDE y Tratamiento
# ==============================================================================
library(data.table)
library(readxl)
library(stringr)
library(ggplot2)

ruta_cna_raw <- "CensoNacionalAgro/S01_15(Unidad_productora).csv"
ruta_cede <- "CensoNacionalAgro/PANEL_CARACTERISTICAS_GENERALES(2021).xlsx"
ruta_divi <- "CensoNacionalAgro/DIVIPOLA-_Códigos_municipios_20260913.csv"
ruta_vs <- "Bases-datos/ResolucionesHistoricasdeBaldios con 2008.xlsx"

dir_datos <- "datos"
dir_graf <- "graficas"
if (!dir.exists(dir_graf)) dir.create(dir_graf, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Extracción y Agregación CNA 2014
# ------------------------------------------------------------------------------
cols_cna <- c("P_MUNIC", "TIPO_UC", "PRED_ETNICA", "S05_TENENCIA", "ENCUESTA", "P_S12P150A")
dt <- fread(ruta_cna_raw, select = cols_cna, colClasses = list(character = c("P_MUNIC", "ENCUESTA")))

upa <- dt[TIPO_UC == 1 & ENCUESTA != "999999999" & !is.na(P_S12P150A) & P_S12P150A > 0]
upa[, area_ha := P_S12P150A / 10000]

calc_ineq <- function(x) {
  n <- length(x)
  if (n == 0L) return(list(n_upa = 0L, ha_total = 0, gini = NA_real_, gini_corr = NA_real_, p90_share = NA_real_, p95_share = NA_real_, p99_share = NA_real_, p50_share = NA_real_, palma_ratio = NA_real_, area_mediana = NA_real_, area_media = NA_real_, ratio_asimetria = NA_real_, share_menor5 = NA_real_, share_5a50 = NA_real_, share_50a500 = NA_real_, share_mayor500 = NA_real_, q10 = NA_real_, q25 = NA_real_, q50 = NA_real_, q75 = NA_real_, q90 = NA_real_, q95 = NA_real_, q99 = NA_real_))
  s <- sum(x)
  if (s == 0) return(list(n_upa = n, ha_total = 0, gini = 0, gini_corr = 0, p90_share = 0, p95_share = 0, p99_share = 0, p50_share = 0, palma_ratio = NA_real_, area_mediana = 0, area_media = 0, ratio_asimetria = 1, share_menor5 = 1, share_5a50 = 0, share_50a500 = 0, share_mayor500 = 0, q10 = 0, q25 = 0, q50 = 0, q75 = 0, q90 = 0, q95 = 0, q99 = 0))
  
  x <- sort(x)
  g <- if (n >= 2L) (2 * sum(seq_len(n) * x)) / (n * s) - (n + 1) / n else 0
  g_corr <- if (n >= 2L) g * (n / (n - 1)) else 0
  
  s_share <- function(pct) sum(x[(n - max(1L, round(n * pct)) + 1L):n]) / s
  
  p40_s <- sum(x[1:max(1L, round(n * 0.40))]) / s
  palma <- if (p40_s > 0) s_share(0.10) / p40_s else NA_real_
  med <- median(x)
  
  list(
    n_upa = n, ha_total = s, gini = g, gini_corr = g_corr,
    p90_share = s_share(0.10), p95_share = s_share(0.05), p99_share = s_share(0.01),
    p50_share = sum(x[1:max(1L, round(n * 0.50))]) / s, palma_ratio = palma,
    area_mediana = med, area_media = s / n, ratio_asimetria = if (med > 0) (s / n) / med else NA_real_,
    share_menor5 = sum(x[x < 5]) / s, share_5a50 = sum(x[x >= 5 & x < 50]) / s,
    share_50a500 = sum(x[x >= 50 & x < 500]) / s, share_mayor500 = sum(x[x >= 500]) / s,
    q10 = as.numeric(quantile(x, 0.10, names = FALSE)), q25 = as.numeric(quantile(x, 0.25, names = FALSE)),
    q50 = med, q75 = as.numeric(quantile(x, 0.75, names = FALSE)),
    q90 = as.numeric(quantile(x, 0.90, names = FALSE)), q95 = as.numeric(quantile(x, 0.95, names = FALSE)),
    q99 = as.numeric(quantile(x, 0.99, names = FALSE))
  )
}

upa_part <- upa[PRED_ETNICA == 7]
res_part <- upa_part[, calc_ineq(area_ha), by = .(cod_mpio = P_MUNIC)]
res_part[, `:=`(flag_n_menor20 = as.integer(n_upa < 20), flag_n_menor50 = as.integer(n_upa < 50))]

gini_simple <- function(x) {
  n <- length(x); s <- sum(x)
  if (n < 2L || s == 0) return(0)
  (2 * sum(seq_len(n) * sort(x))) / (n * s) - (n + 1) / n
}

res_tot <- upa[, .(n_upa_total = .N, ha_total_censo = sum(area_ha), gini_total = gini_simple(area_ha)), by = .(cod_mpio = P_MUNIC)]

upa_inf <- upa[PRED_ETNICA == 7 & S05_TENENCIA != 99]
t_res <- upa_inf[, .(ha = sum(area_ha), n = .N), by = .(cod_mpio = P_MUNIC, S05_TENENCIA)]

ten_ha <- dcast(t_res, cod_mpio ~ S05_TENENCIA, value.var = "ha", fill = 0)
ten_n  <- dcast(t_res, cod_mpio ~ S05_TENENCIA, value.var = "n",  fill = 0)

for (k in c("1", "2", "3", "7", "8")) {
  if (!k %in% names(ten_ha)) ten_ha[, (k) := 0]
  if (!k %in% names(ten_n))  ten_n[, (k) := 0]
}

setnames(ten_ha, c("1", "2", "3", "7", "8"), c("ha_propia", "ha_arriendo", "ha_aparceria", "ha_ocup", "ha_adjudicada"), skip_absent = TRUE)
setnames(ten_n, c("1", "2", "3", "7", "8"), c("n_propia", "n_arriendo", "n_aparceria", "n_ocup", "n_adjudicada"), skip_absent = TRUE)

ten_ha[, ha_inf := rowSums(.SD), .SDcols = setdiff(names(ten_ha), "cod_mpio")]
ten_n[, n_inf := rowSums(.SD), .SDcols = setdiff(names(ten_n), "cod_mpio")]

ten <- merge(ten_ha[, .(cod_mpio, ha_propia, ha_ocup, ha_arriendo, ha_aparceria, ha_adjudicada, ha_inf)],
             ten_n[, .(cod_mpio, n_propia, n_ocup, n_arriendo, n_aparceria, n_adjudicada, n_inf)], by = "cod_mpio")

ten[, `:=`(
  share_propia = ha_propia / ha_inf, share_propia_ha = ha_propia / ha_inf, share_propia_upa = n_propia / n_inf,
  share_ocup_ha = ha_ocup / ha_inf, share_ocup_upa = n_ocup / n_inf, any_ocup = as.integer(n_ocup > 0),
  share_arriendo_ha = ha_arriendo / ha_inf, share_aparceria_ha = ha_aparceria / ha_inf
)]

# ------------------------------------------------------------------------------
# 2. Controles CEDE
# ------------------------------------------------------------------------------
df_raw <- as.data.table(read_excel(ruta_cede))
cols_predet <- c("codmpio", "ao_crea", "altura", "discapital", "areaoficialkm2", "gandina", "gcaribe", "gpacifica", "gorinoquia", "gamazonia")
cede_base <- unique(df_raw[!is.na(areaoficialkm2) & areaoficialkm2 > 0, ..cols_predet], by = "codmpio")

cede_base[, `:=`(
  cod_mpio = sprintf("%05d", as.integer(codmpio)), anio_creacion = as.integer(ao_crea),
  altitud = as.numeric(altura), dist_capital = as.numeric(discapital),
  area_mpio = as.numeric(areaoficialkm2), area_mpio_ha = as.numeric(areaoficialkm2) * 100
)]
cede_base[, `:=`(
  log_area_mpio = log(area_mpio), log_altitud = log(pmax(1, altitud)),
  log_dist_capital = log(pmax(1, dist_capital)), cod_dpto = substr(cod_mpio, 1, 2)
)]

if (any(is.na(cede_base$altitud))) {
  cede_base[, altitud_dpto := mean(altitud, na.rm = TRUE), by = cod_dpto]
  cede_base[is.na(altitud), altitud := altitud_dpto]
  cede_base[is.na(log_altitud), log_altitud := log(pmax(1, altitud))]
  cede_base[, altitud_dpto := NULL]
}

pob_1993 <- df_raw[ano == 1993, .(cod_mpio = sprintf("%05d", as.integer(codmpio)), pob_rural_1993 = as.numeric(pobl_rur))]
cede_base <- merge(cede_base, pob_1993, by = "cod_mpio", all.x = TRUE)
cede_base[, log_pob_rural_1993 := log(pmax(1, pob_rural_1993))]

cols_final_cede <- c("cod_mpio", "cod_dpto", "anio_creacion", "altitud", "log_altitud", "dist_capital", "log_dist_capital", "area_mpio", "area_mpio_ha", "log_area_mpio", "gandina", "gcaribe", "gpacifica", "gorinoquia", "gamazonia", "pob_rural_1993", "log_pob_rural_1993")
cede_clean <- cede_base[, ..cols_final_cede]

# Consolidación CNA - CEDE
cna_mpio <- merge(res_part, res_tot, by = "cod_mpio", all = TRUE)
cna_mpio <- merge(cna_mpio, ten, by = "cod_mpio", all.x = TRUE)
cna_mpio <- merge(cna_mpio, cede_clean[, .(cod_mpio, area_mpio_ha)], by = "cod_mpio", all.x = TRUE)

cna_mpio[is.na(ha_inf), `:=`(
  ha_propia = 0, ha_ocup = 0, ha_arriendo = 0, ha_aparceria = 0, ha_adjudicada = 0, ha_inf = 0,
  n_propia = 0L, n_ocup = 0L, n_arriendo = 0L, n_aparceria = 0L, n_adjudicada = 0L, n_inf = 0L, any_ocup = 0L
)]

cna_mpio[, `:=`(
  share_ha_etnica = pmax(0, (ha_total_censo - ha_total) / ha_total_censo),
  share_n_etnica = pmax(0, (n_upa_total - n_upa) / n_upa_total),
  share_informado = ifelse(ha_total > 0, ha_inf / ha_total, 0),
  ratio_censo_mpio = ha_total / area_mpio_ha,
  cod_dpto = substr(cod_mpio, 1, 2)
)]

fwrite(cna_mpio, file.path(dir_datos, "cna2014_municipal.csv"), quote = TRUE)

# ------------------------------------------------------------------------------
# 3. Tratamiento Histórico de Baldíos y Cruce DIVIPOLA
# ------------------------------------------------------------------------------
normalizar_texto <- function(x) {
  x <- gsub("¤", "N", x)
  x |> toupper() |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> str_replace_all("[^A-Z ]", " ") |> str_squish()
}

divi <- fread(ruta_divi, encoding = "UTF-8", colClasses = "character")
setnames(divi, names(divi), sub("^﻿", "", names(divi)))
setnames(divi, c("Nombre Departamento", "Nombre Municipio", "Código Municipio", "longitud", "Latitud"), c("dep_div", "mun_div", "cod_mpio", "lon_raw", "lat_raw"), skip_absent = TRUE)

divi[, `:=`(
  dep_norm = normalizar_texto(dep_div), mun_norm = normalizar_texto(mun_div),
  cod_mpio = formatC(as.integer(cod_mpio), width = 5, flag = "0"),
  lon = as.numeric(gsub(",", ".", lon_raw)), lat = as.numeric(gsub(",", ".", lat_raw))
)]

divi_maestra <- unique(divi[, .(dep_norm, mun_norm, cod_mpio, lon, lat)])

vs <- as.data.table(read_excel(ruta_vs, sheet = "Datos"))
setnames(vs, c("Departamento", "Municipio", "año", "Area", "id", "Modalidad"), c("dep", "mun", "anio", "area", "id", "modalidad"), skip_absent = TRUE)
vs[, `:=`(dep_norm = normalizar_texto(dep), mun_norm = normalizar_texto(mun))]

dict_dep <- c("NORTE SANTANDER" = "NORTE DE SANTANDER", "BOGOTA DC" = "BOGOTA D C", "ARCHIPIELAGO SAN ANDRES PROVIDENCIA Y SANTA CATAL" = "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA")
vs[, dep_clean := ifelse(dep_norm %in% names(dict_dep), dict_dep[dep_norm], dep_norm)]

dict_mun <- c(
  "SANTANDER|SAN VICENTE CHUCURI" = "SAN VICENTE DE CHUCURI", "SANTANDER|EL CARMEN CHUCURI" = "EL CARMEN DE CHUCURI",
  "SANTANDER|SABANA TORRES" = "SABANA DE TORRES", "SANTANDER|SAN JOSE MIRANDA" = "SAN JOSE DE MIRANDA",
  "CAUCA|SANTANDER QUILICHAO" = "SANTANDER DE QUILICHAO", "CAQUETA|BELEN LOS ANDAQUIES" = "BELEN DE LOS ANDAQUIES",
  "META|FUENTE ORO" = "FUENTE DE ORO", "META|SAN JUAN ARAMA" = "SAN JUAN DE ARAMA", "META|SAN CARLOS GUAROA" = "SAN CARLOS DE GUAROA",
  "META|BARRANCA UPIA" = "BARRANCA DE UPIA", "BOLIVAR|SAN MARTIN LOBA" = "SAN MARTIN DE LOBA", "BOLIVAR|BARRANCO LOBA" = "BARRANCO DE LOBA",
  "BOLIVAR|HATILLO LOBA" = "HATILLO DE LOBA", "BOLIVAR|EL CARMEN BOLIVAR" = "EL CARMEN DE BOLIVAR", "CASANARE|PAZ ARIPORO" = "PAZ DE ARIPORO",
  "CASANARE|SAN LUIS PALENQUE" = "SAN LUIS DE PALENQUE", "BOYACA|SAN LUIS GACENO" = "SAN LUIS DE GACENO", "BOYACA|SAN PABLO BORBUR" = "SAN PABLO DE BORBUR",
  "BOYACA|SAN MIGUEL SEMA" = "SAN MIGUEL DE SEMA", "BOYACA|SAN JOSE PARE" = "SAN JOSE DE PARE", "BOYACA|VILLA LEYVA" = "VILLA DE LEYVA",
  "VALLE DEL CAUCA|GUADALAJARA BUGA" = "GUADALAJARA DE BUGA", "CESAR|RIO ORO" = "RIO DE ORO", "CESAR|LA JAGUA IBIRICO" = "LA JAGUA DE IBIRICO",
  "NARINO|EL TABLON GOMEZ" = "EL TABLON DE GOMEZ", "NARINO|SAN PEDRO CARTAGO" = "SAN PEDRO DE CARTAGO", "ANTIOQUIA|SAN PEDRO URABA" = "SAN PEDRO DE URABA",
  "ANTIOQUIA|SAN JUAN URABA" = "SAN JUAN DE URABA", "ANTIOQUIA|EL CARMEN VIBORAL" = "EL CARMEN DE VIBORAL", "ANTIOQUIA|SANTA ROSA OSOS" = "SANTA ROSA DE OSOS",
  "ANTIOQUIA|SAN JOSE LA MONTANA" = "SAN JOSE DE LA MONTANA", "ANTIOQUIA|SANTAFE ANTIOQUIA" = "SANTA FE DE ANTIOQUIA",
  "MAGDALENA|SAN SEBASTIAN BUENAVISTA" = "SAN SEBASTIAN DE BUENAVISTA", "MAGDALENA|CERRO SAN ANTONIO" = "CERRO DE SAN ANTONIO",
  "MAGDALENA|SABANAS SAN ANGEL" = "SABANAS DE SAN ANGEL", "MAGDALENA|SANTA BARBARA PINTO" = "SANTA BARBARA DE PINTO",
  "CORDOBA|CIENAGA ORO" = "CIENAGA DE ORO", "CORDOBA|SAN ANDRES SOTAVENTO" = "SAN ANDRES DE SOTAVENTO", "RISARALDA|BELEN UMBRIA" = "BELEN DE UMBRIA",
  "RISARALDA|SANTA ROSA CABAL" = "SANTA ROSA DE CABAL", "ATLANTICO|CAMPO LA CRUZ" = "CAMPO DE LA CRUZ", "ATLANTICO|JUAN ACOSTA" = "JUAN DE ACOSTA",
  "ATLANTICO|PALMAR VARELA" = "PALMAR DE VARELA", "TOLIMA|VALLE SAN JUAN" = "VALLE DE SAN JUAN", "TOLIMA|CARMEN APICALA" = "CARMEN DE APICALA",
  "CHOCO|EL CARMEN ATRATO" = "EL CARMEN DE ATRATO", "SUCRE|SANTIAGO TOLU" = "SANTIAGO DE TOLU", "SUCRE|SAN JUAN BETULIA" = "SAN JUAN DE BETULIA",
  "CUNDINAMARCA|AGUA DIOS" = "AGUA DE DIOS", "CUNDINAMARCA|GUAYABAL SIQUIMA" = "GUAYABAL DE SIQUIMA", "CUNDINAMARCA|CARMEN CARUPA" = "CARMEN DE CARUPA",
  "CUNDINAMARCA|SAN JUAN RIO SECO" = "SAN JUAN DE RIOSECO", "CUNDINAMARCA|VILLA SAN DIEGO UBATE" = "VILLA DE SAN DIEGO DE UBATE",
  "NARINO|TUMACO" = "SAN ANDRES DE TUMACO", "NARINO|CUASPUD" = "CUASPUD CARLOSAMA", "NORTE DE SANTANDER|CUCUTA" = "SAN JOSE DE CUCUTA",
  "VALLE DEL CAUCA|CALI" = "SANTIAGO DE CALI", "BOLIVAR|CARTAGENA" = "CARTAGENA DE INDIAS", "BOLIVAR|MOMPOS" = "SANTA CRUZ DE MOMPOX",
  "BOLIVAR|ARROYO HONDO" = "ARROYOHONDO", "TOLIMA|MARIQUITA" = "SAN SEBASTIAN DE MARIQUITA", "PUTUMAYO|LEGUIZAMO" = "PUERTO LEGUIZAMO",
  "CAUCA|LOPEZ" = "LOPEZ DE MICAY", "CAUCA|PIENDAMO" = "PIENDAMO TUNIA", "CAUCA|SOTARA" = "SOTARA PAISPAMBA", "BOYACA|GUICAN" = "GUICAN DE LA SIERRA",
  "MAGDALENA|CHIBOLO" = "CHIVOLO", "MAGDALENA|PIJIO DEL CARMEN" = "PIJINO DEL CARMEN", "MAGDALENA|PIJI O DEL CARMEN" = "PIJINO DEL CARMEN",
  "CORDOBA|PURISIMA" = "PURISIMA DE LA CONCEPCION", "GUAINIA|BARRANCO MINAS" = "BARRANCOMINAS", "CESAR|MANAURE" = "MANAURE BALCON DEL CESAR",
  "SUCRE|SINCE" = "SAN LUIS DE SINCE", "SUCRE|TOLU VIEJO" = "SAN JOSE DE TOLUVIEJO", "ANTIOQUIA|DON MATIAS" = "DONMATIAS",
  "ANTIOQUIA|SAN ANDRES" = "SAN ANDRES DE CUERQUIA", "ANTIOQUIA|SAN VICENTE" = "SAN VICENTE FERRER", "ANTIOQUIA|SAN PEDRO" = "SAN PEDRO DE LOS MILAGROS",
  "BOGOTA D C|BOGOTA" = "BOGOTA D C", "BOGOTA D C|BOSA" = "BOGOTA D C", "CUNDINAMARCA|BOGOTA" = "BOGOTA D C",
  "CUNDINAMARCA|BOGOTA LOCALIDAD SUMAPAZ" = "BOGOTA D C", "CUNDINAMARCA|BOGOTA LOCALIDAD SUMAPAZ TB" = "BOGOTA D C"
)

vs[, clave_busqueda := paste(dep_clean, mun_norm, sep = "|")]
vs[, mun_clean := ifelse(clave_busqueda %in% names(dict_mun), dict_mun[clave_busqueda], mun_norm)]
vs[mun_clean == "BOGOTA D C", dep_clean := "BOGOTA D C"]

vs_cod <- merge(vs, divi_maestra, by.x = c("dep_clean", "mun_clean"), by.y = c("dep_norm", "mun_norm"), all.x = TRUE)

trat_mpio <- vs_cod[, .(
  ha_adj = sum(area, na.rm = TRUE), n_res = .N,
  anio_medio = weighted.mean(anio, w = area), antiguedad = 2014 - weighted.mean(anio, w = area),
  share_pre36 = sum(area[anio < 1936], na.rm = TRUE) / sum(area, na.rm = TRUE),
  share_l200 = sum(area[anio %in% 1936:1960], na.rm = TRUE) / sum(area, na.rm = TRUE),
  share_pre61 = sum(area[anio < 1961], na.rm = TRUE) / sum(area, na.rm = TRUE),
  share_incora = sum(area[anio %in% 1961:1987], na.rm = TRUE) / sum(area, na.rm = TRUE),
  share_l30 = sum(area[anio %in% 1988:1993], na.rm = TRUE) / sum(area, na.rm = TRUE),
  share_l160 = sum(area[anio >= 1994], na.rm = TRUE) / sum(area, na.rm = TRUE),
  tam_medio = sum(area, na.rm = TRUE) / .N, tam_mediano = median(area, na.rm = TRUE),
  share_peq = sum(area[area < 5], na.rm = TRUE) / sum(area, na.rm = TRUE),
  lon = first(lon), lat = first(lat)
), by = cod_mpio]

cna_muestra_a <- cna_mpio[n_upa >= 20]
cs <- merge(trat_mpio, cna_muestra_a, by = "cod_mpio")
cs <- merge(cs, cede_clean[, !c("cod_dpto", "area_mpio_ha"), with = FALSE], by = "cod_mpio", all.x = TRUE)

cs[, `:=`(
  cod_dpto = substr(cod_mpio, 1, 2), muestra_a = as.integer(n_upa >= 20),
  muestra_b = as.integer(n_upa >= 20 & share_informado >= 0.70),
  cobertura = ha_adj / ha_total, intens = ha_adj / area_mpio_ha
)]

fwrite(cs, file.path(dir_datos, "cs_tratamiento_cna.csv"), quote = TRUE)

plot_agenda <- ggplot(cs, aes(x = antiguedad, y = gini)) +
  geom_point(alpha = 0.35, color = "#4A2810", size = 2) +
  geom_smooth(method = "lm", color = "#1F2937", fill = "#D1D5DB", linewidth = 0.9) +
  labs(title = "Persistencia de la Adjudicación Histórica y Concentración de la Tierra",
       subtitle = sprintf("Municipios con adjudicación de baldíos (1901–2013) y microdatos CNA 2014 (N = %d)", nrow(cs)),
       caption = "Fuente: Villaveces & Sánchez (CEDE) y DANE (CNA 2014). Excluye resguardos étnicos y residuo censal.",
       x = "Antigüedad promedio de titulación formal (años hasta 2014, ponderado por superficie)",
       y = "Gini municipal (CNA 2014)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13), plot.subtitle = element_text(color = "gray30", size = 10), plot.caption = element_text(size = 8, color = "gray40", face = "italic"), panel.grid.minor = element_blank())

ggsave(file.path(dir_graf, "agenda_antiguedad_gini.png"), plot_agenda, width = 8.5, height = 5.2, dpi = 300)
