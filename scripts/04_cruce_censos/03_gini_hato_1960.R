# ==============================================================================
# SCRIPT 04/02: GINI DE HATO MUNICIPAL, CENSO AGROPECUARIO 1960
# ==============================================================================
# Estima el Gini de concentracion de ganado vacuno por municipio a partir de:
#   - Cuadro 16-A: numero de explotaciones en 9 clases de tamano de hato
#   - Cuadro 15-A: total de vacunos del municipio
#
# Metodo. Las 9 clases dan la distribucion de FRECUENCIAS pero no las medias de
# clase, y la ultima clase ("500 y mas") es abierta: su media es el supuesto que
# mas mueve el Gini. En vez de suponerla, se despeja.
#
# Dentro de cada clase se asume pmf(x) proporcional a x^(-alpha) sobre el soporte
# entero de la clase, con un unico alpha compartido por todas las clases del
# municipio. alpha se resuelve por biseccion imponiendo
#       sum_k n_k * m_k(alpha) = total_vacunos   (dato observado del 15-A)
#
# Ventajas sobre alternativas:
#   - m_k(alpha) cae SIEMPRE dentro de [L_k, U_k]: no produce medias negativas,
#     que es lo que ocurre al volcar el residuo en la clase abierta.
#   - alpha queda identificado por el total observado, no supuesto.
#   - Con alpha se reconstruye la distribucion implicita sobre todo el soporte
#     entero, y el Gini se calcula EXACTO sobre ella (sin trapecio ni agrupamiento).
#
# Entradas : datos/panel_censos_1960_2014.csv
# Salidas  : datos/gini_hato_1960.csv
# ==============================================================================

library(data.table)

CLASES <- data.table(
  col = c("c1","c2_4","c5_9","c10_19","c20_49","c50_99","c100_199","c200_499","c500"),
  L   = c(1, 2, 5, 10, 20,  50, 100, 200, 500),
  U   = c(1, 4, 9, 19, 49,  99, 199, 499,  NA)
)
CAP <- 20000L   # techo operativo de la clase abierta

SOPORTE <- lapply(seq_len(nrow(CLASES)), function(k)
  as.numeric(seq.int(CLASES$L[k], if (is.na(CLASES$U[k])) CAP else CLASES$U[k])))
LOGSOP  <- lapply(SOPORTE, log)

# --- media de cada clase bajo pmf(x) ~ x^(-alpha), exacta sobre el soporte entero
medias_clase <- function(alpha) {
  vapply(seq_along(SOPORTE), function(k) {
    lg <- LOGSOP[[k]]; s <- SOPORTE[[k]]
    w  <- exp(-alpha * lg - max(-alpha * lg))     # estabilizado
    sum(w * s) / sum(w)
  }, numeric(1))
}

# --- alpha tal que sum(n_k * m_k(alpha)) = T.  total(alpha) es decreciente en alpha
resolver_alpha <- function(n, T, lo = -25, hi = 25, tol = 1e-9, iter = 200) {
  f <- function(a) sum(n * medias_clase(a)) - T
  if (f(lo) < 0) return(list(alpha = NA_real_, estado = "T_mayor_que_maximo"))
  if (f(hi) > 0) return(list(alpha = NA_real_, estado = "T_menor_que_minimo"))
  for (i in seq_len(iter)) {
    mid <- (lo + hi) / 2; fm <- f(mid)
    if (abs(fm) < tol) break
    if (fm > 0) lo <- mid else hi <- mid
  }
  list(alpha = (lo + hi) / 2, estado = "ok")
}

# --- Gini exacto de una distribucion discreta ponderada (x ascendente)
gini_ponderado <- function(x, w) {
  W <- sum(w); sx <- sum(w * x)
  if (W <= 0 || sx <= 0) return(NA_real_)
  S <- cumsum(w)
  sum(w * x * (2 * S - w)) / (W * sx) - 1
}

gini_hato_municipio <- function(n, T) {
  vac <- list(gini_hato = NA_real_, gini_hato_mid = NA_real_, alpha = NA_real_,
              m_top = NA_real_, share_top = NA_real_, p90_hato = NA_real_,
              estado = "sin_datos")
  if (!is.finite(T) || T <= 0 || sum(n, na.rm = TRUE) < 1) return(vac)

  # referencia: puntos medios, sin usar T
  mid <- ifelse(is.na(CLASES$U), 750, (CLASES$L + CLASES$U) / 2)
  g_mid <- gini_ponderado(mid, n)

  r <- resolver_alpha(n, T)
  if (is.na(r$alpha)) { vac$gini_hato_mid <- g_mid; vac$estado <- r$estado; return(vac) }

  a <- r$alpha; m <- medias_clase(a)
  xs <- list(); ws <- list()
  for (k in seq_along(SOPORTE)) {
    if (n[k] <= 0) next
    lg <- LOGSOP[[k]]
    w  <- exp(-a * lg - max(-a * lg)); w <- w / sum(w) * n[k]
    xs[[length(xs) + 1]] <- SOPORTE[[k]]; ws[[length(ws) + 1]] <- w
  }
  x <- unlist(xs); w <- unlist(ws)
  o <- order(x); x <- x[o]; w <- w[o]

  S <- cumsum(w); i <- findInterval(0.90 * sum(w), S) + 1L
  p90 <- sum(w[i:length(w)] * x[i:length(x)]) / sum(w * x)

  list(gini_hato = gini_ponderado(x, w), gini_hato_mid = g_mid, alpha = a,
       m_top = m[length(m)], share_top = n[length(n)] * m[length(m)] / T,
       p90_hato = p90, estado = "ok")
}

# ------------------------------------------------------------------ ejecucion
ruta_in  <- "datos/panel_censos_1960_2014.csv"
ruta_out <- "datos/gini_hato_1960.csv"

d <- fread(ruta_in, colClasses = list(character = "cod_mpio"))
cols_n <- paste0(CLASES$col, "_1960")
stopifnot(all(cols_n %in% names(d)), "total_vacunos_1960" %in% names(d))

res <- rbindlist(lapply(seq_len(nrow(d)), function(i) {
  n <- as.numeric(d[i, ..cols_n])
  gini_hato_municipio(n, as.numeric(d[i]$total_vacunos_1960))
}))

out <- cbind(d[, .(cod_mpio, depto, municipio, total_expl_1960, total_vacunos_1960)], res)
fwrite(out, ruta_out)

cat("\n=== GINI DE HATO 1960 ===\n")
print(out[, .N, by = estado])
v <- out[estado == "ok"]
cat(sprintf("\ncalculado en %d de %d municipios\n", nrow(v), nrow(out)))
cat(sprintf("Gini:  p5=%.3f  p25=%.3f  p50=%.3f  p75=%.3f  p95=%.3f\n",
            quantile(v$gini_hato, .05), quantile(v$gini_hato, .25),
            median(v$gini_hato), quantile(v$gini_hato, .75), quantile(v$gini_hato, .95)))
cat(sprintf("alpha: p5=%.2f  p50=%.2f  p95=%.2f\n",
            quantile(v$alpha, .05), median(v$alpha), quantile(v$alpha, .95)))
cat(sprintf("media de la clase abierta: p50=%.0f cabezas (el supuesto ingenuo de 750 se queda corto)\n",
            median(v$m_top, na.rm = TRUE)))

# --- validacion contra el proxy
if ("share_admin" %in% names(d)) {
  vv <- merge(v, d[, .(cod_mpio, share_admin)], by = "cod_mpio")
  vv <- vv[!is.na(share_admin)]
  cat(sprintf("\nVALIDACION  gini_hato vs share_admin (n=%d): Pearson=%.3f  Spearman=%.3f\n",
              nrow(vv), cor(vv$gini_hato, vv$share_admin),
              cor(vv$gini_hato, vv$share_admin, method = "spearman")))
}
