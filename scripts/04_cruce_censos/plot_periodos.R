suppressPackageStartupMessages({
  library(ggplot2)
  library(data.table)
})

# Coeficientes del Modelo de Periodos de 05_corte_b_2014.R (N = 659, FE Depto, Cluster SE)
df <- data.frame(
  periodo = factor(c(
    "Pre-1912 (Latifundio)",
    "Rep. Conservadora (1912-29)",
    "Ley 200 / Fun. Social (1930-43)",
    "Aparcería / Violencia (1944-58)",
    "Corporativismo (1959-60)",
    "INCORA / Ley 135 (1961-72)",
    "Pacto de Chicoral (1973-87)",
    "Freno Colonización (1988-93)",
    "Mercado de Tierras / L160 (1994-06)"
  ), levels = rev(c(
    "Pre-1912 (Latifundio)",
    "Rep. Conservadora (1912-29)",
    "Ley 200 / Fun. Social (1930-43)",
    "Aparcería / Violencia (1944-58)",
    "Corporativismo (1959-60)",
    "INCORA / Ley 135 (1961-72)",
    "Pacto de Chicoral (1973-87)",
    "Freno Colonización (1988-93)",
    "Mercado de Tierras / L160 (1994-06)"
  ))),
  beta = c(-0.0150, 0.0000, -0.1471, 0.0758, -0.0537, -0.0681, -0.0661, -0.2013, -0.0704),
  se   = c(0.0199, 0.0660, 0.0573, 0.0532, 0.0548, 0.0261, 0.0361, 0.1256, 0.0142)
)

df$ci_lo90 <- df$beta - 1.645 * df$se
df$ci_hi90 <- df$beta + 1.645 * df$se
df$ci_lo95 <- df$beta - 1.96 * df$se
df$ci_hi95 <- df$beta + 1.96 * df$se

df$categoria <- ifelse(df$beta < 0 & df$ci_hi90 < 0, "Reduce Gini (Desconcentra)",
                ifelse(df$beta > 0 & df$ci_lo90 > 0, "Aumenta Gini (Reconcentra)", "Efecto no significativo"))

p <- ggplot(df, aes(x = beta, y = periodo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_segment(aes(x = ci_lo95, xend = ci_hi95, yend = periodo), color = "gray60", linewidth = 0.7) +
  geom_segment(aes(x = ci_lo90, xend = ci_hi90, yend = periodo), color = "#182B49", linewidth = 1.6) +
  geom_point(aes(color = categoria), size = 2.8) +
  scale_color_manual(values = c(
    "Reduce Gini (Desconcentra)" = "#1B7837",
    "Efecto no significativo" = "#5A6B7C",
    "Aumenta Gini (Reconcentra)" = "#B2182B"
  )) +
  labs(
    title = "Efecto de Adjudicaciones por Periodo Histórico sobre Gini 2014",
    subtitle = "Efectos fijos departamentales y controles. Barras: IC 90% (gruesa) y 95% (delgada)",
    x = "Coeficiente estimado",
    y = "",
    color = "Resultado"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#182B49"),
    plot.subtitle = element_text(size = 8.5, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8),
    axis.text.y = element_text(size = 8.5, face = "bold", color = "black"),
    axis.text.x = element_text(size = 8.5),
    panel.grid.minor = element_blank()
  )

ggsave("Presentación/periodos_historicos_gini.png", p, width = 7.0, height = 4.2, dpi = 200)
cat("Gráfica guardada con éxito en Presentación/periodos_historicos_gini.png\n")
