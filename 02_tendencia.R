################################################################################
# 02_tendencia.R — Tendência 2006–2024 por Prais-Winsten (Tabela 1)
# Saídas: coeficiente bruto (p.p./ano, como na versão atual do artigo) e
# variação percentual anual (VPA/APC) com IC 95%, via log10 (Antunes & Cardoso, 2015).
################################################################################

source("00_config.R")
prevalencias <- readRDS(file.path(DIR_SAIDA, "prevalencias.rds"))

ajustar_tendencia <- function(d, ...) {
  d  <- arrange(d, ano)
  m  <- pw(prev ~ ano, d)
  ml <- pw(log10(prev) ~ ano, d)
  cf <- summary(m)$coefficients
  cl <- summary(ml)$coefficients
  b  <- cl["ano", 1]; se <- cl["ano", 2]
  tq <- qt(0.975, df = nrow(d) - 2)
  # Diagnóstico: autocorrelação de 1ª ordem dos resíduos do MQO. Valores
  # próximos de 1 indicam que a tendência linear não descreve bem a série
  # (ex.: queda seguida de subida) e o coeficiente do Prais-Winsten fica instável.
  e  <- residuals(lm(prev ~ ano, data = d)); n <- length(e)
  el <- residuals(lm(log10(prev) ~ ano, data = d))
  tibble(
    ano_ini = min(d$ano), ano_fim = max(d$ano), n_anos = nrow(d),
    rho_mqo = sum(e[-1] * e[-n]) / sum(e[-n]^2),
    rho_mqo_log = sum(el[-1] * el[-n]) / sum(el[-n]^2),
    beta = cf["ano", 1], ep_beta = cf["ano", 2], p_valor = cf["ano", 4],
    vpa = (10^b - 1) * 100,
    vpa_inf = (10^(b - tq * se) - 1) * 100,
    vpa_sup = (10^(b + tq * se) - 1) * 100,
    p_vpa = cl["ano", 4]
  )
}

tendencia <- prevalencias |>
  group_by(across(all_of(CHAVES))) |>
  group_modify(ajustar_tendencia) |>
  ungroup() |>
  mutate(instavel = rho_mqo >= LIMITE_RHO | rho_mqo_log >= LIMITE_RHO,
         classificacao = case_when(p_vpa >= 0.05 ~ "estacionária",
                                   vpa > 0 ~ "crescente", TRUE ~ "decrescente"))

print(tendencia, n = 20)
if (any(tendencia$instavel)) {
  cat("\nATENÇÃO: séries com autocorrelação próxima de 1 (coeficiente instável):\n")
  print(tendencia |> filter(instavel) |> select(all_of(CHAVES), beta, vpa, rho_mqo, rho_mqo_log))
}
saveRDS(tendencia, file.path(DIR_SAIDA, "tendencia.rds"))
fwrite(tendencia, file.path(DIR_SAIDA, "tendencia.csv"))
