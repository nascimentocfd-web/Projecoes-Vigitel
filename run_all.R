################################################################################
# run_all.R — Executa o pipeline completo, na ordem.
#
# Coloque todos os scripts e o arquivo de microdados na mesma pasta (ou ajuste
# ARQ_MICRODADOS em 00_config.R), abra o projeto do RStudio nessa pasta e rode:
#     source("run_all.R")
#
# Só o 01 usa os microdados (a parte pesada). Depois dele, qualquer etapa pode
# ser rodada sozinha, porque cada script lê os resultados salvos em "saidas/".
################################################################################

etapas <- c("01_prevalencias.R", "02_tendencia.R", "03_predicoes.R",
            "04_metricas.R", "05_tabelas.R", "06_graficos.R",
            "07_especificacoes_modelos.R")

for (e in etapas) {
  message("\n==================== ", e, " ====================")
  source(e, encoding = "UTF-8")
}
