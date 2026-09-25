################################################################################
# 03_predicoes.R — Treino até 2019, projeção 2020–2024, 5 modelos + ensemble
#
# Substitui 3_predicoes.R e todos os 3_2_*.R: UMA função aplicada às 77 séries.
# Cada modelo também devolve o intervalo de predição de 95% (li, ls), usado
# para calcular a cobertura. O Prais-Winsten não fornece intervalo de predição.
################################################################################

source("00_config.R")
prevalencias <- readRDS(file.path(DIR_SAIDA, "prevalencias.rds"))

vazio  <- function() data.frame(ano = ANOS_TESTE, pred = NA_real_, li = NA_real_, ls = NA_real_)
seguro <- function(expr, rotulo, modelo) {
  tryCatch(expr, error = function(e) {
    warning(sprintf("%s | %s: %s", rotulo, modelo, conditionMessage(e)), call. = FALSE)
    vazio()
  })
}

prever_serie <- function(d, chave) {
  rotulo <- paste(unlist(chave), collapse = " | ")
  message("Ajustando: ", rotulo)
  tr <- d |> filter(ano <= ANO_FIM_TREINO) |> arrange(ano)
  h  <- length(ANOS_TESTE)
  y  <- ts(tr$prev, start = min(tr$ano), frequency = 1)
  nd <- data.frame(ano = ANOS_TESTE)

  res <- list(
    LM = seguro({
      p <- predict(lm(prev ~ ano, data = tr), nd, interval = "prediction", level = 0.95)
      data.frame(ano = ANOS_TESTE, pred = p[, "fit"], li = p[, "lwr"], ls = p[, "upr"])
    }, rotulo, "LM"),

    ARIMA = seguro({
      f <- forecast(auto.arima(y), h = h, level = 95)
      data.frame(ano = ANOS_TESTE, pred = as.numeric(f$mean),
                 li = as.numeric(f$lower), ls = as.numeric(f$upper))
    }, rotulo, "ARIMA"),

    HOLT = seguro({
      f <- holt(y, h = h, level = 95)
      data.frame(ano = ANOS_TESTE, pred = as.numeric(f$mean),
                 li = as.numeric(f$lower), ls = as.numeric(f$upper))
    }, rotulo, "HOLT"),

    PROPHET = seguro({
      mp <- suppressMessages(prophet(
        data.frame(ds = as.Date(paste0(tr$ano, "-01-01")), y = tr$prev),
        yearly.seasonality = FALSE, weekly.seasonality = FALSE,
        daily.seasonality = FALSE, interval.width = 0.95))
      fp <- predict(mp, data.frame(ds = as.Date(paste0(ANOS_TESTE, "-01-01"))))
      data.frame(ano = ANOS_TESTE, pred = fp$yhat, li = fp$yhat_lower, ls = fp$yhat_upper)
    }, rotulo, "PROPHET"),

    PRAIS = seguro({
      m <- pw(prev ~ ano, tr)
      data.frame(ano = ANOS_TESTE, pred = as.numeric(predict(m, newdata = nd)),
                 li = NA_real_, ls = NA_real_)
    }, rotulo, "PRAIS")
  )

  out <- bind_rows(res, .id = "modelo")
  ens <- out |>
    group_by(ano) |>
    summarise(pred = mean(pred), .groups = "drop") |>
    mutate(modelo = "ENSEMBLE", li = NA_real_, ls = NA_real_)
  bind_rows(out, ens)
}

set.seed(SEMENTE)
predicoes <- prevalencias |>
  group_by(across(all_of(CHAVES))) |>
  group_modify(prever_serie) |>
  ungroup() |>
  left_join(prevalencias |> select(all_of(CHAVES), ano, prev, ic_inf, ic_sup, interpolado),
            by = c(CHAVES, "ano"))

saveRDS(predicoes, file.path(DIR_SAIDA, "predicoes.rds"))
fwrite(predicoes, file.path(DIR_SAIDA, "predicoes.csv"))
