# ==============================================================================
# Construcción del Panel Municipal de Conflicto Armado y Adjudicación de Tierras (1945–2024)
# ==============================================================================
library(data.table)
library(readxl)
library(stringr)
library(ggplot2)

ruta_db <- "Bases-datos/CasosDB_202509.xlsx"
ruta_vs <- "Bases-datos/ResolucionesHistoricasdeBaldios con 2008.xlsx"
ruta_cs <- "datos/cs_tratamiento_cna.csv"
ruta_divi <- "CensoNacionalAgro/DIVIPOLA-_Códigos_municipios_20260913.csv"
dir_cna <- "CensoNacionalAgro"
dir_datos <- "datos"
dir_graf <- "graficas"

normalizar_texto <- function(x) {
  x <- gsub("¤", "N", x)
  x |> toupper() |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> str_replace_all("[^A-Z ]", " ") |> str_squish()
}

divi <- fread(ruta_divi, encoding = "UTF-8", colClasses = "character")
setnames(divi, names(divi), sub("^[^A-Za-z0-9]+", "", names(divi)))
setnames(divi, c("Nombre Departamento", "Nombre Municipio", "Código Municipio", "longitud", "Latitud"), c("dep_div", "mun_div", "cod_mpio", "lon_raw", "lat_raw"), skip_absent = TRUE)

divi[, `:=`(
  dep_norm = normalizar_texto(dep_div), mun_norm = normalizar_texto(mun_div),
  cod_mpio = formatC(as.integer(cod_mpio), width = 5, flag = "0"),
  lon = as.numeric(gsub(",", ".", lon_raw)), lat = as.numeric(gsub(",", ".", lat_raw))
)]

divi_maestra <- unique(divi[, .(dep_norm, mun_norm, cod_mpio, lon, lat, departamento = dep_div, municipio = mun_div)])
divi_maestra <- divi_maestra[!duplicated(cod_mpio)]
divi_validos <- divi_maestra$cod_mpio

vs <- as.data.table(read_excel(ruta_vs, sheet = "Datos"))
setnames(vs, c("Departamento", "Municipio", "año", "Area"), c("dep", "mun", "anio", "area"), skip_absent = TRUE)
vs[, `:=`(dep_norm = normalizar_texto(dep), mun_norm = normalizar_texto(mun))]

dict_dep <- c("NORTE SANTANDER" = "NORTE DE SANTANDER", "BOGOTA DC" = "BOGOTA D C", "ARCHIPIELAGO SAN ANDRES PROVIDENCIA Y SANTA CATAL" = "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA")
vs[, dep_clean := ifelse(dep_norm %in% names(dict_dep), dict_dep[dep_norm], dep_norm)]

dict_mun <- c(
  "SANTANDER|SAN VICENTE CHUCURI" = "SAN VICENTE DE CHUCURI", "SANTANDER|EL CARMEN CHUCURI" = "EL CARMEN DE CHUCURI", "SANTANDER|SABANA TORRES" = "SABANA DE TORRES",
  "SANTANDER|SAN JOSE MIRANDA" = "SAN JOSE DE MIRANDA", "CAUCA|SANTANDER QUILICHAO" = "SANTANDER DE QUILICHAO", "CAQUETA|BELEN LOS ANDAQUIES" = "BELEN DE LOS ANDAQUIES",
  "META|FUENTE ORO" = "FUENTE DE ORO", "META|SAN JUAN ARAMA" = "SAN JUAN DE ARAMA", "META|SAN CARLOS GUAROA" = "SAN CARLOS DE GUAROA", "META|BARRANCA UPIA" = "BARRANCA DE UPIA",
  "BOLIVAR|SAN MARTIN LOBA" = "SAN MARTIN DE LOBA", "BOLIVAR|BARRANCO LOBA" = "BARRANCO DE LOBA", "BOLIVAR|HATILLO LOBA" = "HATILLO DE LOBA",
  "BOLIVAR|EL CARMEN BOLIVAR" = "EL CARMEN DE BOLIVAR", "CASANARE|PAZ ARIPORO" = "PAZ DE ARIPORO", "CASANARE|SAN LUIS PALENQUE" = "SAN LUIS DE PALENQUE",
  "BOYACA|SAN LUIS GACENO" = "SAN LUIS DE GACENO", "BOYACA|SAN PABLO BORBUR" = "SAN PABLO DE BORBUR", "BOYACA|SAN MIGUEL SEMA" = "SAN MIGUEL DE SEMA",
  "BOYACA|SAN JOSE PARE" = "SAN JOSE DE PARE", "BOYACA|VILLA LEYVA" = "VILLA DE LEYVA", "VALLE DEL CAUCA|GUADALAJARA BUGA" = "GUADALAJARA DE BUGA",
  "CESAR|RIO ORO" = "RIO DE ORO", "CESAR|LA JAGUA IBIRICO" = "LA JAGUA DE IBIRICO", "NARINO|EL TABLON GOMEZ" = "EL TABLON DE GOMEZ",
  "NARINO|SAN PEDRO CARTAGO" = "SAN PEDRO DE CARTAGO", "ANTIOQUIA|SAN PEDRO URABA" = "SAN PEDRO DE URABA", "ANTIOQUIA|SAN JUAN URABA" = "SAN JUAN DE URABA",
  "ANTIOQUIA|EL CARMEN VIBORAL" = "EL CARMEN DE VIBORAL", "ANTIOQUIA|SANTA ROSA OSOS" = "SANTA ROSA DE OSOS", "ANTIOQUIA|SAN JOSE LA MONTANA" = "SAN JOSE DE LA MONTANA",
  "ANTIOQUIA|SANTAFE ANTIOQUIA" = "SANTA FE DE ANTIOQUIA", "MAGDALENA|SAN SEBASTIAN BUENAVISTA" = "SAN SEBASTIAN DE BUENAVISTA",
  "MAGDALENA|CERRO SAN ANTONIO" = "CERRO DE SAN ANTONIO", "MAGDALENA|SABANAS SAN ANGEL" = "SABANAS DE SAN ANGEL", "MAGDALENA|SANTA BARBARA PINTO" = "SANTA BARBARA DE PINTO",
  "CORDOBA|CIENAGA ORO" = "CIENAGA DE ORO", "CORDOBA|SAN ANDRES SOTAVENTO" = "SAN ANDRES DE SOTAVENTO", "RISARALDA|BELEN UMBRIA" = "BELEN DE UMBRIA",
  "RISARALDA|SANTA ROSA CABAL" = "SANTA ROSA DE CABAL", "ATLANTICO|CAMPO LA CRUZ" = "CAMPO DE LA CRUZ", "ATLANTICO|JUAN ACOSTA" = "JUAN DE ACOSTA",
  "ATLANTICO|PALMAR VARELA" = "PALMAR DE VARELA", "TOLIMA|VALLE SAN JUAN" = "VALLE DE SAN JUAN", "TOLIMA|CARMEN APICALA" = "CARMEN DE APICALA",
  "CHOCO|EL CARMEN ATRATO" = "EL CARMEN DE ATRATO", "SUCRE|SANTIAGO TOLU" = "SANTIAGO DE TOLU", "SUCRE|SAN JUAN BETULIA" = "SAN JUAN DE BETULIA",
  "CUNDINAMARCA|AGUA DIOS" = "AGUA DE DIOS", "CUNDINAMARCA|GUAYABAL SIQUIMA" = "GUAYABAL DE SIQUIMA", "CUNDINAMARCA|CARMEN CARUPA" = "CARMEN DE CARUPA",
  "CUNDINAMARCA|SAN JUAN RIO SECO" = "SAN JUAN DE RIOSECO", "CUNDINAMARCA|VILLA SAN DIEGO UBATE" = "VILLA DE SAN DIEGO DE UBATE", "NARINO|TUMACO" = "SAN ANDRES DE TUMACO",
  "NARINO|CUASPUD" = "CUASPUD CARLOSAMA", "NORTE DE SANTANDER|CUCUTA" = "SAN JOSE DE CUCUTA", "VALLE DEL CAUCA|CALI" = "SANTIAGO DE CALI",
  "BOLIVAR|CARTAGENA" = "CARTAGENA DE INDIAS", "BOLIVAR|MOMPOS" = "SANTA CRUZ DE MOMPOX", "BOLIVAR|ARROYO HONDO" = "ARROYOHONDO",
  "TOLIMA|MARIQUITA" = "SAN SEBASTIAN DE MARIQUITA", "PUTUMAYO|LEGUIZAMO" = "PUERTO LEGUIZAMO", "CAUCA|LOPEZ" = "LOPEZ DE MICAY", "CAUCA|PIENDAMO" = "PIENDAMO TUNIA",
  "CAUCA|SOTARA" = "SOTARA PAISPAMBA", "BOYACA|GUICAN" = "GUICAN DE LA SIERRA", "MAGDALENA|CHIBOLO" = "CHIVOLO", "MAGDALENA|PIJIO DEL CARMEN" = "PIJINO DEL CARMEN",
  "MAGDALENA|PIJI O DEL CARMEN" = "PIJINO DEL CARMEN", "CORDOBA|PURISIMA" = "PURISIMA DE LA CONCEPCION", "GUAINIA|BARRANCO MINAS" = "BARRANCOMINAS",
  "CESAR|MANAURE" = "MANAURE BALCON DEL CESAR", "SUCRE|SINCE" = "SAN LUIS DE SINCE", "SUCRE|TOLU VIEJO" = "SAN JOSE DE TOLUVIEJO",
  "ANTIOQUIA|DON MATIAS" = "DONMATIAS", "ANTIOQUIA|SAN ANDRES" = "SAN ANDRES DE CUERQUIA", "ANTIOQUIA|SAN VICENTE" = "SAN VICENTE FERRER",
  "ANTIOQUIA|SAN PEDRO" = "SAN PEDRO DE LOS MILAGROS", "BOGOTA D C|BOGOTA" = "BOGOTA D C", "BOGOTA D C|BOSA" = "BOGOTA D C", "CUNDINAMARCA|BOGOTA" = "BOGOTA D C",
  "CUNDINAMARCA|BOGOTA LOCALIDAD SUMAPAZ" = "BOGOTA D C", "CUNDINAMARCA|BOGOTA LOCALIDAD SUMAPAZ TB" = "BOGOTA D C"
)

vs[, clave_busqueda := paste(dep_clean, mun_norm, sep = "|")]
vs[, mun_clean := ifelse(clave_busqueda %in% names(dict_mun), dict_mun[clave_busqueda], mun_norm)]
vs[mun_clean == "BOGOTA D C", dep_clean := "BOGOTA D C"]

vs_cod <- merge(vs, divi_maestra[, .(dep_norm, mun_norm, cod_mpio)], by.x = c("dep_clean", "mun_clean"), by.y = c("dep_norm", "mun_norm"), all.x = TRUE)

agg_vs <- vs_cod[, .(ha_adj = sum(area, na.rm = TRUE), n_res = .N), by = .(cod_mpio, anio = as.integer(anio))]

activos_pre61 <- unique(agg_vs[anio <= 1960, cod_mpio])
activos_pre88 <- unique(agg_vs[anio <= 1987, cod_mpio])
activos_pre94 <- unique(agg_vs[anio <= 1993, cod_mpio])

fwrite(agg_vs, file.path(dir_datos, "agg_vs_1901_2013.csv"), quote = TRUE)

archivos_vic <- list(
  AS = file.path(dir_cna, "VictimasAS_202603.xlsx"), MA = file.path(dir_cna, "VictimasMA_202603.xlsx"),
  AB = file.path(dir_cna, "VictimasAB_202603.xlsx"), AP = file.path(dir_cna, "VictimasAP_202603.xlsx"),
  AT = file.path(dir_cna, "VictimasAT_202603.xlsx"), MI = file.path(dir_cna, "VictimasMI_202603.xlsx")
)

lista_muertes <- list()
for (mod in names(archivos_vic)) {
  dt <- as.data.table(read_excel(archivos_vic[[mod]], col_types = "text"))
  col_yr <- grep("A..o|Ano|Año", names(dt), value = TRUE)[1]
  col_cod <- grep("DANE", names(dt), value = TRUE)[1]
  
  dt[, `:=`(modalidad = mod, anio = suppressWarnings(as.integer(get(col_yr))), cod_mpio = suppressWarnings(formatC(as.integer(get(col_cod)), width = 5, flag = "0")))]
  if ("Situación Actual de la Víctima" %in% names(dt)) dt <- dt[`Situación Actual de la Víctima` == "MUERTA"]
  dt[, es_civil := if ("Calidad de la Víctima o la Baja" %in% names(dt)) `Calidad de la Víctima o la Baja` == "CIVIL" else TRUE]
  
  lista_muertes[[mod]] <- dt[anio >= 1945 & anio <= 2024 & cod_mpio %in% divi_validos, .(modalidad, anio, cod_mpio, es_civil)]
}

todas_muertes <- rbindlist(lista_muertes)
agg_muertes <- todas_muertes[, .(
  victimasTT = .N, victimas_civiles = sum(es_civil == TRUE, na.rm = TRUE),
  muertes_as = sum(modalidad == "AS"), muertes_ma = sum(modalidad == "MA"),
  muertes_ab = sum(modalidad == "AB"), muertes_otras = sum(modalidad %in% c("AP", "AT", "MI"))
), by = .(cod_mpio, anio)]

db_dt <- as.data.table(read_excel(ruta_db))
col_db_yr <- names(db_dt)[grep("A..o|Ano|Año", names(db_dt))[1]]
col_db_cod <- names(db_dt)[grep("DANE", names(db_dt))[1]]
col_db_vic <- names(db_dt)[grep("ctimas|Victimas", names(db_dt))[1]]

db_dt[, `:=`(
  anio = suppressWarnings(as.integer(get(col_db_yr))), cod_mpio = suppressWarnings(formatC(as.integer(get(col_db_cod)), width = 5, flag = "0")),
  victimas = as.numeric(get(col_db_vic)), prop_rural = as.numeric(`Propiedad Rural`)
)]

db_clean <- db_dt[anio >= 1945 & anio <= 2024 & !is.na(anio) & cod_mpio %in% divi_validos]
agg_db <- db_clean[, .(casos_db = .N, bienes_rur_db = sum(prop_rural, na.rm = TRUE), casos_db_rural = sum(prop_rural > 0, na.rm = TRUE)), by = .(cod_mpio, anio)]

cs <- fread(ruta_cs, colClasses = list(character = c("cod_mpio", "cod_dpto")))
cs[, cod_dpto := substr(cod_mpio, 1, 2)]
if ("muestra_b" %in% names(cs)) cs <- cs[muestra_b == 1]

anios_panel <- 1945:2024
grid_904 <- CJ(cod_mpio = cs$cod_mpio, anio = anios_panel)

panel <- merge(grid_904, agg_vs, by = c("cod_mpio", "anio"), all.x = TRUE)
panel[is.na(ha_adj), ha_adj := 0]
panel[is.na(n_res), n_res := 0L]
panel[, any_adj := as.integer(n_res > 0)]

panel <- merge(panel, agg_muertes, by = c("cod_mpio", "anio"), all.x = TRUE)
for (col in c("victimasTT", "victimas_civiles", "muertes_as", "muertes_ma", "muertes_ab", "muertes_otras")) panel[is.na(get(col)), (col) := 0L]
panel[, any_muerte := as.integer(victimasTT > 0)]

panel <- merge(panel, agg_db, by = c("cod_mpio", "anio"), all.x = TRUE)
for (col in c("casos_db", "bienes_rur_db", "casos_db_rural")) panel[is.na(get(col)), (col) := 0L]

panel[, `:=`(total_eventos_conflicto = muertes_as + casos_db, any_conflicto = as.integer((victimasTT + casos_db) > 0))]

pre45 <- agg_vs[anio < 1945, .(ha_pre45 = sum(ha_adj, na.rm = TRUE), n_pre45 = sum(n_res, na.rm = TRUE)), by = cod_mpio]
panel <- merge(panel, pre45, by = "cod_mpio", all.x = TRUE)
panel[is.na(ha_pre45), `:=`(ha_pre45 = 0, n_pre45 = 0L)]

panel[order(cod_mpio, anio), `:=`(
  ha_adj_acum = ha_pre45 + cumsum(ha_adj), n_res_acum = n_pre45 + cumsum(n_res),
  victimasTT_acum = cumsum(victimasTT), victimas_civiles_acum = cumsum(victimas_civiles),
  muertes_as_acum = cumsum(muertes_as), casos_db_acum = cumsum(casos_db),
  casos_db_rural_acum = cumsum(casos_db_rural)
), by = cod_mpio]

panel[, `:=`(
  d_incora_1961 = as.integer(anio == 1961), d_ley30_1988 = as.integer(anio == 1988),
  d_ley160_1994 = as.integer(anio == 1994), post_incora = as.integer(anio >= 1961),
  post_ley30 = as.integer(anio >= 1988), post_ley160 = as.integer(anio >= 1994),
  activo_pre61 = as.integer(cod_mpio %in% activos_pre61), activo_pre88 = as.integer(cod_mpio %in% activos_pre88),
  activo_pre94 = as.integer(cod_mpio %in% activos_pre94)
)]

expo_pre61 <- agg_vs[anio %in% 1951:1960, .(ha_pre61 = sum(ha_adj, na.rm = TRUE)), by = cod_mpio]
panel <- merge(panel, expo_pre61, by = "cod_mpio", all.x = TRUE)
panel[is.na(ha_pre61), ha_pre61 := 0]
panel[, expo_incora := as.numeric(scale(log(1 + ha_pre61)))]

cols_cs_merge <- setdiff(names(cs), c("ha_adj", "n_res"))
panel <- merge(panel, cs[, ..cols_cs_merge], by = "cod_mpio", all.x = TRUE)
panel <- merge(panel, divi_maestra[, .(cod_mpio, departamento, municipio)], by = "cod_mpio", all.x = TRUE)
panel[, cod_dpto := substr(cod_mpio, 1, 2)]

fwrite(panel, file.path(dir_datos, "panel_municipal_conflicto_tierras.csv"), quote = TRUE)

serie_anual <- panel[, .(total_ha = sum(ha_adj) / 1000, total_vicTT = sum(victimasTT), total_db_rural = sum(casos_db_rural)), by = anio]
p_evol <- ggplot(serie_anual, aes(x = anio)) +
  geom_col(aes(y = total_vicTT, fill = "Víctimas fatales"), alpha = 0.40) +
  geom_line(aes(y = total_db_rural * 30, color = "Daños propiedad (x30)"), linewidth = 0.9) +
  geom_line(aes(y = total_ha * 20, color = "Baldíos adjudicados (x20)"), linewidth = 1, linetype = "dashed") +
  scale_y_continuous(name = "Víctimas fatales", sec.axis = sec_axis(~ . / 20, name = "Miles de ha adjudicadas")) +
  scale_fill_manual(name = "", values = c("Víctimas fatales" = "#991B1B")) +
  scale_color_manual(name = "", values = c("Daños propiedad (x30)" = "#D97706", "Baldíos adjudicados (x20)" = "#1E3A8A")) +
  geom_vline(xintercept = c(1961, 1988, 1994), linetype = "dotted", color = "gray20") +
  theme_minimal()

ggsave(file.path(dir_graf, "panel_evolucion_conflicto_tierras.png"), p_evol, width = 9.5, height = 5.6, dpi = 300)
