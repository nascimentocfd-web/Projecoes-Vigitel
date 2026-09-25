################################################################################
# 07_especificacoes_modelos.R — Parâmetros efetivamente ajustados em cada série
#
# Responde ao pedido de reprodutibilidade dos avaliadores: registra, para as 77
# séries (treino 2006–2019), a ordem (p,d,q) e a presença de drift do ARIMA,
# os parâmetros de suavização do Holt, o rho do Prais-Winsten e o número de
# pontos de mudança do Prophet. Reajusta os mesmos modelos do 03 (mesmos dados
# e configurações), portanto os resultados são idênticos.
# Saídas: saidas/especificacoes_modelos.csv (material suplementar),
#         saidas/versoes_pacotes.txt (para a seção de métodos)
################################################################################

source("00_config.R")
prevalencias <- readRDS(file.path(DIR_SAIDA, "prevalencias.rds"))

especificar <- function(d, chave) {
  tr <- d |> filter(ano <= ANO_FIM_TREINO) |> arrange(ano)
  y  <- ts(tr$prev, start = min(tr$ano), frequency = 1)

  fa <- auto.arima(y)
  ord <- arimaorder(fa)
  cf <- names(coef(fa))

  fh <- holt(y, h = 1)$model

  mp <- pw(prev ~ ano, tr)
  rho_pw <- tryCatch(tail(as.numeric(mp$rho[, 1]), 1), error = function(e) NA_real_)

  mprof <- suppressMessages(prophet(
    data.frame(ds = as.Date(paste0(tr$ano, "-01-01")), y = tr$prev),
    yearly.seasonality = FALSE, weekly.seasonality = FALSE,
    daily.seasonality = FALSE, interval.width = 0.95))

  tibble(
    anos_treino   = paste0(min(tr$ano), "–", max(tr$ano)),
    n_treino      = nrow(tr),
    arima_p = ord[["p"]], arima_d = ord[["d"]], arima_q = ord[["q"]],
    arima_drift   = "drift" %in% cf,
    arima_const   = any(c("intercept", "mean") %in% cf),
    arima_rotulo  = paste0("ARIMA(", ord[["p"]], ",", ord[["d"]], ",", ord[["q"]], ")",
                           ifelse("drift" %in% cf, " com drift",
                                  ifelse(any(c("intercept", "mean") %in% cf), " com constante", ""))),
    arima_aicc    = fa$aicc,
    holt_alpha    = unname(fh$par["alpha"]),
    holt_beta     = unname(fh$par["beta"]),
    prais_rho     = rho_pw,
    prophet_n_changepoints = mprof$n.changepoints,
    prophet_changepoint_prior_scale = mprof$changepoint.prior.scale
  )
}

especificacoes <- prevalencias |>
  group_by(across(all_of(CHAVES))) |>
  group_modify(especificar) |>
  ungroup() |>
  arrange(match(indicador, indicadores$var), match(grupo, NIVEIS_GRUPO))

fwrite(especificacoes, file.path(DIR_SAIDA, "especificacoes_modelos.csv"))

cat("\nModelos ARIMA selecionados (frequência nas 77 séries):\n")
print(especificacoes |> count(arima_rotulo, sort = TRUE))
cat("\nHolt — alfa e beta (mediana e intervalo):\n")
print(summary(especificacoes[, c("holt_alpha", "holt_beta")]))

# Versões para a seção de métodos
sink(file.path(DIR_SAIDA, "versoes_pacotes.txt"))
cat(R.version.string, "\n")
for (p in c("survey", "forecast", "prophet", "prais", "ggplot2"))
  cat(p, as.character(packageVersion(p)), "\n")
sink()
cat("\nVersões salvas em saidas/versoes_pacotes.txt\n")
