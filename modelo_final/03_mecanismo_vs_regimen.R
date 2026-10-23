# ==============================================================================
# MODELO FINAL — 03: ¿DECAIMIENTO POR MERCADO O DISEÑO DEL RÉGIMEN?
# ==============================================================================
# Con un solo corte (CNA 2014), la antigüedad de la titulación y el régimen
# legal bajo el que se adjudicó son casi la misma variable: el régimen explica
# el 96 % de la antigüedad. Este script contrasta dos predicciones que sí
# difieren entre las dos lecturas:
#
#   A. HETEROGENEIDAD POR MERCADO. Si el gradiente es un decaimiento por
#      transacciones de mercado, debe ser más fuerte donde el mercado de tierras
#      es más activo: cerca de la capital departamental y en municipios más
#      poblados. Si es diseño del régimen, no tiene por qué depender de eso.
#      Las interacciones se estiman CONTROLANDO por la composición de régimen,
#      así que no las explica el régimen en promedio.
#
#   B. FORMA DEL GRADIENTE. Decaimiento => pendiente suave dentro de cada
#      régimen. Diseño => saltos en los cortes legales (1936, 1961, 1988, 1994)
#      y pendiente plana dentro de cada tramo. Se compara una tendencia lineal
#      en el año medio de adjudicación con escalones por régimen dominante.
#
# Ejecutar con el directorio de trabajo en la raíz del proyecto, después de 01.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
})

dir_res <- "modelo_final/resultados"
dir.create(dir_res, recursive = TRUE, showWarnings = FALSE)

b <- fread("modelo_final/datos/base_final.csv", colClasses = list(character = c("cod_mpio", "cod_dpto")))
d <- b[tratado == 1 & !is.na(antig_dec) & !is.na(gini)]

geo     <- "intens_c + log_area_mpio + log_altitud + log_dist_capital + anio_creacion + share_peq + log_tam_adj"
regimen <- "share_l200 + share_incora + share_l30 + share_l160"
est <- function(fml, data = d) feols(as.formula(fml), data = data, cluster = ~cod_dpto)
sig <- c("***" = 0.01, "**" = 0.05, "*" = 0.10)

# Moderadores (predeterminados), estandarizados para leer la interacción por 1 DE
d[, z_cercania := -(log_dist_capital - mean(log_dist_capital)) / sd(log_dist_capital)]  # + = más cerca
d[, z_pob93    := (log_pob_rural_1993 - mean(log_pob_rural_1993, na.rm = TRUE)) / sd(log_pob_rural_1993, na.rm = TRUE)]
d[, cerca      := as.integer(log_dist_capital <= median(log_dist_capital))]

setFixest_dict(c(
  gini = "Gini de tierra 2014", p90_share = "Participación del 10% superior 2014",
  antig_dec = "Antigüedad (décadas)", z_cercania = "Cercanía a la capital (DE)",
  z_pob93 = "Población rural 1993 (DE)", cerca = "Cerca de la capital (bajo la mediana)",
  anio_medio_dec = "Año medio de adjudicación (décadas)", cod_dpto = "Departamento",
  share_l200 = "Prop. adjudicada 1936–1960", share_incora = "Prop. adjudicada 1961–1987",
  share_l30 = "Prop. adjudicada 1988–1993", share_l160 = "Prop. adjudicada 1994–2013"
))

# Nota: z_cercania es una transformación lineal de log_dist_capital, así que
# fixest descarta log_dist_capital por colinealidad en esas columnas (es esperado).

# ==============================================================================
# A. HETEROGENEIDAD POR MERCADO (con régimen controlado)
# ==============================================================================
tA <- list(
  est(paste("gini ~ antig_dec*z_cercania +", geo, "+", regimen, "| cod_dpto")),
  est(paste("gini ~ antig_dec*cerca +",      geo, "+", regimen, "| cod_dpto")),
  est(paste("gini ~ antig_dec*z_pob93 +",    geo, "+", regimen, "| cod_dpto"), d[!is.na(z_pob93)]),
  est(paste("p90_share ~ antig_dec*z_cercania +", geo, "+", regimen, "| cod_dpto")),
  # Versión estricta: el efecto del régimen también puede variar con la cercanía
  est(paste("gini ~ antig_dec*z_cercania +", geo, "+ (", regimen, ")*z_cercania | cod_dpto"))
)
hA <- c("Cercanía", "Cerca (dummy)", "Población 1993", "P90: cercanía", "Estricto: régimen × cercanía")
cat("\n==== A. HETEROGENEIDAD POR MERCADO ====\n")
print(etable(tA, headers = hA, keep = "%antig_dec|z_cercania|cerca|z_pob93", signif.code = sig,
             fitstat = ~ n + wr2))
etable(tA, headers = hA, keep = "%antig_dec|z_cercania|cerca|z_pob93", signif.code = sig,
       fitstat = ~ n + wr2, tex = TRUE, replace = TRUE,
       file = file.path(dir_res, "tabla7_heterogeneidad_mercado.tex"),
       title = "Heterogeneidad del gradiente de antigüedad según acceso al mercado (régimen controlado)",
       notes = "Todas las columnas controlan por intensidad, geografía, composición de la adjudicación y proporción del área adjudicada por régimen legal. La columna 5 agrega régimen × cercanía.")

# Efecto marginal de la antigüedad lejos (-1 DE) y cerca (+1 DE) de la capital
m <- tA[[1]]; V <- vcov(m)
ef <- rbindlist(lapply(c(-1, 0, 1), function(z) {
  g <- c(1, z); nm <- c("antig_dec", "antig_dec:z_cercania")
  data.table(cercania_DE = z, efecto = sum(g * coef(m)[nm]),
             se = sqrt(as.numeric(t(g) %*% V[nm, nm] %*% g)))
}))
ef[, `:=`(ic_inf = efecto - 1.96 * se, ic_sup = efecto + 1.96 * se)]
cat("\nEfecto marginal de una década de antigüedad sobre el Gini, según cercanía:\n"); print(ef, digits = 3)

# ==============================================================================
# B. FORMA DEL GRADIENTE: TENDENCIA SUAVE VS. ESCALONES POR RÉGIMEN
# ==============================================================================
cortes <- c(-Inf, 1935.5, 1960.5, 1987.5, 1993.5, Inf)
etiq   <- c("pre-1936", "1936-1960", "1961-1987", "1988-1993", "1994-2013")
d[, regimen_medio := factor(cut(anio_medio, cortes, labels = etiq), levels = etiq)]
d[, anio_medio_dec := (anio_medio - 1980) / 10]
cat("\nMunicipios por régimen del año medio de adjudicación:\n"); print(table(d$regimen_medio))

tB <- list(
  est(paste("gini ~ anio_medio_dec +", geo, "| cod_dpto")),
  est(paste("gini ~ i(regimen_medio, ref = 'pre-1936') +", geo, "| cod_dpto")),
  est(paste("gini ~ anio_medio_dec + i(regimen_medio, ref = 'pre-1936') +", geo, "| cod_dpto")),
  est(paste("p90_share ~ anio_medio_dec + i(regimen_medio, ref = 'pre-1936') +", geo, "| cod_dpto"))
)
hB <- c("Solo tendencia", "Solo escalones", "Tendencia + escalones", "P90: ambos")
cat("\n==== B. FORMA DEL GRADIENTE ====\n")
print(etable(tB, headers = hB, keep = "%anio_medio_dec|regimen_medio", signif.code = sig, fitstat = ~ n + wr2))
etable(tB, headers = hB, keep = "%anio_medio_dec|regimen_medio", signif.code = sig,
       fitstat = ~ n + wr2, tex = TRUE, replace = TRUE,
       file = file.path(dir_res, "tabla8_tendencia_vs_escalones.tex"),
       title = "Forma del gradiente: tendencia continua frente a escalones por régimen legal",
       notes = "Año medio de adjudicación ponderado por área (signo opuesto a la antigüedad). Base de los escalones: pre-1936. Controles como en la especificación principal.")

pruebas <- data.table(
  prueba = c("Escalones = 0 | tendencia (Gini)", "Tendencia = 0 | escalones (Gini)",
             "Escalones = 0 | tendencia (P90)",  "Tendencia = 0 | escalones (P90)"),
  stat = c(wald(tB[[3]], keep = "regimen_medio", print = FALSE)$stat, tstat(tB[[3]])["anio_medio_dec"]^2,
           wald(tB[[4]], keep = "regimen_medio", print = FALSE)$stat, tstat(tB[[4]])["anio_medio_dec"]^2),
  p    = c(wald(tB[[3]], keep = "regimen_medio", print = FALSE)$p, pvalue(tB[[3]])["anio_medio_dec"],
           wald(tB[[4]], keep = "regimen_medio", print = FALSE)$p, pvalue(tB[[4]])["anio_medio_dec"])
)
cat("\nPruebas de forma:\n"); print(pruebas, digits = 3)
fwrite(rbind(pruebas, data.table(prueba = paste0("Efecto marginal antig., cercanía = ", ef$cercania_DE, " DE"),
                                 stat = ef$efecto / ef$se, p = 2 * pnorm(-abs(ef$efecto / ef$se)))),
       file.path(dir_res, "tabla9_pruebas_mecanismo.csv"))

# ==============================================================================
# FIGURA: Gini residualizado por año medio de adjudicación, con cortes legales
# ==============================================================================
r <- feols(as.formula(paste("gini ~", geo, "| cod_dpto")), d)
d[obs(r), gini_res := resid(r)]
d[, bin := cut(anio_medio, breaks = unique(quantile(anio_medio, seq(0, 1, length.out = 31))), include.lowest = TRUE)]
bins <- d[!is.na(gini_res), .(x = mean(anio_medio), y = mean(gini_res), n = .N), by = bin]

fig <- ggplot(bins, aes(x, y)) +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.4) +
  geom_vline(xintercept = c(1936, 1961, 1988, 1994), colour = "grey65", linetype = "22", linewidth = 0.4) +
  annotate("text", x = c(1936, 1961, 1988, 1994), y = max(bins$y) * c(1.12, 1.12, 1.12, 0.98),
           label = c("Ley 200", "Ley 135", "Ley 30", "Ley 160"), size = 3, colour = "grey35", hjust = -0.08) +
  geom_point(colour = "#2a6f97", size = 2.4) +
  geom_smooth(data = d[!is.na(gini_res)], aes(anio_medio, gini_res), method = "lm", formula = y ~ x,
              se = FALSE, colour = "#2a6f97", linewidth = 0.6, linetype = "solid", alpha = 0.6) +
  labs(x = "Año medio de adjudicación (ponderado por área)",
       y = "Gini de tierra 2014 (residual)",
       caption = "Promedios por 30 grupos de igual tamaño. Residuos de la especificación principal sin la antigüedad.\nLíneas punteadas: cortes legales. Fuentes: DANE (2014), Villaveces Niño y Sánchez (2014), CEDE (2022).") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, colour = "grey40", size = 8))
ggsave(file.path(dir_res, "figura_gradiente_cortes_legales.png"), fig, width = 7.5, height = 4.5, dpi = 300)

cat("\nResultados guardados en", dir_res, "\n")
