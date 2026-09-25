################################################################################
# 04_metricas.R — Desempenho preditivo e comparação estatística entre modelos
#
# - RMSE, MAE, MAPE, MASE e cobertura do IC 95% (2022 interpolado excluído)
# - Melhor modelo por série (empates = mesmo RMSE com 3 casas decimais)
# - Teste de Friedman + pós-teste de Nemenyi (ranking dos modelos nas 77 séries)
#
# Sobre o Diebold-Mariano: com 4 anos de teste por série o teste não tem poder
# e não é interpretável série a série; por isso a comparação formal é feita
# pelo Friedman/Nemenyi sobre o conjunto de séries.
################################################################################

source("00_config.R")
prevalencias <- readRDS(file.path(DIR_SAIDA, "prevalencias.rds"))
predicoes    <- readRDS(file.path(DIR_SAIDA, "predicoes.rds"))

# Escala do MASE: erro médio do passeio aleatório (naive) no período de treino
escala <- prevalencias |>
  filter(ano <= ANO_FIM_TREINO) |>
  arrange(ano) |>
  group_by(across(all_of(CHAVES))) |>
  summarise(escala_mase = mean(abs(diff(prev))), .groups = "drop")

avaliacao <- if (EXCLUIR_2022_DO_TESTE) filter(predicoes, !interpolado) else predicoes

metricas <- avaliacao |>
  group_by(across(all_of(CHAVES)), modelo) |>
  summarise(
    n_anos_teste   = n(),
    RMSE           = sqrt(mean((prev - pred)^2)),
    MAE            = mean(abs(prev - pred)),
    MAPE           = 100 * mean(abs(prev - pred) / prev),
    cobertura_ic95 = mean(prev >= li & prev <= ls),
    .groups = "drop"
  ) |>
  left_join(escala, by = CHAVES) |>
  mutate(MASE = MAE / escala_mase) |>
  group_by(across(all_of(CHAVES))) |>
  mutate(rank_com_ensemble = rank(round(RMSE, 3), ties.method = "min")) |>
  ungroup()

rk <- metricas |>
  filter(modelo %in% MODELOS_BASE) |>
  group_by(across(all_of(CHAVES))) |>
  mutate(rank_base = rank(round(RMSE, 3), ties.method = "min")) |>
  ungroup() |>
  select(all_of(CHAVES), modelo, rank_base)

metricas <- left_join(metricas, rk, by = c(CHAVES, "modelo"))

saveRDS(metricas, file.path(DIR_SAIDA, "metricas.rds"))
fwrite(metricas, file.path(DIR_SAIDA, "metricas.csv"))

# ---------------------------------------------------------------------------
# Frequência de melhor desempenho (base da Tabela 3)
# ---------------------------------------------------------------------------
melhores <- filter(metricas, rank_base == 1)
empates  <- melhores |> count(across(all_of(CHAVES))) |> filter(n > 1)
cat("\nSéries com empate no melhor RMSE:", nrow(empates), "\n")
print(empates)

cat("\nVitórias por modelo (empates contam para todos os empatados):\n")
print(melhores |> count(modelo, sort = TRUE))

cat("\nVezes em que o ENSEMBLE teria o menor RMSE entre os 6:\n")
print(metricas |> filter(modelo == "ENSEMBLE") |> count(rank_com_ensemble == 1))

# ---------------------------------------------------------------------------
# Friedman + Nemenyi
# ---------------------------------------------------------------------------
matriz_rmse <- function(df) {
  w <- df |>
    filter(modelo %in% MODELOS_BASE) |>
    mutate(serie = paste(indicador, grupo, sep = " | ")) |>
    select(serie, modelo, RMSE) |>
    pivot_wider(names_from = modelo, values_from = RMSE)
  m <- as.matrix(w[, MODELOS_BASE]); rownames(m) <- w$serie
  m[complete.cases(m), , drop = FALSE]
}

arq_testes <- file.path(DIR_SAIDA, "testes_friedman_nemenyi.txt")
sink(arq_testes)
m_geral <- matriz_rmse(metricas)
cat("===== TODAS AS SÉRIES (n =", nrow(m_geral), ") =====\n")
cat("Ranking médio (1 = melhor):\n")
print(round(sort(colMeans(t(apply(m_geral, 1, rank)))), 2))
print(friedman.test(m_geral))
print(summary(frdAllPairsNemenyiTest(m_geral)))

p_friedman <- c()
for (v in indicadores$var) {
  m_ind <- matriz_rmse(filter(metricas, indicador == v))
  cat("\n\n=====", v, "(n =", nrow(m_ind), ") =====\n")
  print(round(sort(colMeans(t(apply(m_ind, 1, rank)))), 2))
  ft <- friedman.test(m_ind)
  print(ft)
  p_friedman[v] <- ft$p.value
}

# Correção para múltiplas comparações: 7 testes de Friedman (um por indicador)
cat("\n\n===== FRIEDMAN POR INDICADOR: p-valores corrigidos (Holm) =====\n")
print(data.frame(p_bruto = signif(p_friedman, 3),
                 p_holm  = signif(p.adjust(p_friedman, method = "holm"), 3)))
sink()

# Sensibilidade: vitórias com margem clara sobre o 2º colocado
margens <- metricas |>
  filter(modelo %in% MODELOS_BASE) |>
  group_by(across(all_of(CHAVES))) |>
  arrange(RMSE, .by_group = TRUE) |>
  summarise(melhor = first(modelo), margem = RMSE[2] - RMSE[1], .groups = "drop") |>
  left_join(prevalencias |> filter(ano %in% ANOS_TESTE, !interpolado) |>
              group_by(across(all_of(CHAVES))) |> summarise(ep_medio = mean(ep), .groups = "drop"),
            by = CHAVES)
cat("\nVitórias com margem >= 0,1 p.p.:\n")
print(margens |> filter(margem >= 0.1) |> count(melhor, sort = TRUE))
cat("\nVitórias com margem >= erro-padrão amostral médio da série:\n")
print(margens |> filter(margem >= ep_medio) |> count(melhor, sort = TRUE))
fwrite(margens, file.path(DIR_SAIDA, "margens_melhor_modelo.csv"))
cat("\nTestes salvos em", arq_testes, "\n")

# Ranking médio por indicador (usado no heatmap/figura-síntese)
ranking_medio <- metricas |>
  filter(modelo %in% MODELOS_BASE) |>
  group_by(indicador, modelo) |>
  summarise(rank_medio = mean(rank_base), vitorias = sum(rank_base == 1), .groups = "drop")
fwrite(ranking_medio, file.path(DIR_SAIDA, "ranking_medio_por_indicador.csv"))
