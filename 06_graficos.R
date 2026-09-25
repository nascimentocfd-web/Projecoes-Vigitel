################################################################################
# 06_graficos.R — Figuras do artigo (substitui os gráficos dos 3_*.R e o 4_1)
#
# Para cada indicador, duas figuras (como no artigo):
#   A) Total + 5 regiões    B) sexo + faixas etárias
# Cada painel: série observada com IC 95% (faixa), 2022 interpolado marcado com
# círculo vazado, período de teste sombreado e as 5 projeções. O título de cada
# painel informa o melhor modelo e o seu RMSE.
# Figura-síntese extra: heatmap do ranking dos modelos em todas as 77 séries.
################################################################################

source("00_config.R")
prevalencias <- readRDS(file.path(DIR_SAIDA, "prevalencias.rds"))
predicoes    <- readRDS(file.path(DIR_SAIDA, "predicoes.rds"))
metricas     <- readRDS(file.path(DIR_SAIDA, "metricas.rds"))

DIR_FIG <- file.path(DIR_SAIDA, "figuras")
rot_en  <- setNames(indicadores$rotulo_en, indicadores$var)

# Rótulo do painel: "North\nBest: ARIMA (RMSE 2.48)"
rotulos_painel <- metricas |>
  filter(rank_base == 1) |>
  group_by(across(all_of(CHAVES))) |>
  summarise(melhor = paste(ROTULO_MODELO_EN[modelo], collapse = "/"), rmse = min(RMSE), .groups = "drop") |>
  mutate(painel = sprintf("%s\nBest: %s (RMSE %.2f)",
                          ROTULO_GRUPO_EN[grupo], melhor, rmse))

rotular <- function(df) df |>
  left_join(select(rotulos_painel, all_of(CHAVES), painel), by = CHAVES) |>
  mutate(painel = factor(painel, levels = unique(painel[order(match(grupo, NIVEIS_GRUPO))])))

tema_artigo <- theme_bw(base_size = 9, base_family = "serif") +
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.spacing.y = unit(0.05, "cm"), legend.margin = margin(1, 1, 1, 1),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Linhas: cor + tipo de linha (LM tracejado, para não se confundir com o PRAIS,
# que costuma ficar sobreposto). As projeções começam em 2020: modelos de
# tendência projetam a partir da reta ajustada, não do último valor observado,
# então ligar a projeção ao ponto de 2019 criaria "saltos" artificiais.
TIPOS_LINHA <- c(LM = "22", ARIMA = "solid", HOLT = "solid", PROPHET = "solid", PRAIS = "solid")

# Função genérica: obs e prd precisam ter a coluna "painel" (fator)
grafico_series <- function(obs, prd, rotulo_y) {
  prd <- prd |> mutate(modelo = factor(modelo, levels = MODELOS_BASE))
  impar <- n_distinct(obs$painel) %% 2 == 1

  # Número ímpar de painéis: legenda no espaço vazio, ancorada na base da figura,
  # com as legendas empilhadas e compactas (modelos em 3 colunas).
  pos_legenda <- if (impar) {
    theme(legend.position = "inside",
          legend.position.inside = c(0.75, 0.01),
          legend.justification = c(0.5, 0),
          legend.box = "vertical",
          legend.box.just = "left",
          legend.spacing.y = unit(0.1, "cm"))
  } else theme()
  guia_modelo <- guide_legend(order = 3, ncol = if (impar) 2 else NULL,
                              override.aes = list(linewidth = 0.8),
                              keywidth = unit(1.2, "cm"))

  ggplot() +
    annotate("rect", xmin = ANO_FIM_TREINO + 0.5, xmax = max(ANOS_TESTE) + 0.5,
             ymin = -Inf, ymax = Inf, fill = "grey96") +
    geom_vline(xintercept = ANO_FIM_TREINO + 0.5, linetype = "dashed",
               colour = "grey55", linewidth = 0.3) +
    geom_ribbon(data = obs, aes(ano, ymin = ic_inf, ymax = ic_sup, fill = "95% CI"),
                alpha = 0.30) +
    geom_line(data = filter(prd, modelo != "LM"),
              aes(ano, pred, colour = modelo, linetype = modelo), linewidth = 0.6) +
    geom_line(data = filter(prd, modelo == "LM"),
              aes(ano, pred, colour = modelo, linetype = modelo), linewidth = 0.7) +
    geom_line(data = obs, aes(ano, prev), linewidth = 0.7) +
    geom_point(data = obs, aes(ano, prev, shape = interpolado), size = 1.5, fill = "white") +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21),
                       labels = c(`FALSE` = "Observed", `TRUE` = "Interpolated (2022)"),
                       name = NULL) +
    scale_fill_manual(values = c("95% CI" = "grey60"),
                      labels = c("95% CI" = "95% CI (observed)"), name = NULL) +
    scale_colour_manual(values = CORES_MODELOS[MODELOS_BASE], labels = ROTULO_MODELO_EN[MODELOS_BASE],
                        name = "Model", drop = FALSE) +
    scale_linetype_manual(values = TIPOS_LINHA, labels = ROTULO_MODELO_EN[MODELOS_BASE],
                          name = "Model", drop = FALSE) +
    guides(shape = guide_legend(order = 1, direction = "horizontal"),
           fill = guide_legend(order = 2, direction = "horizontal",
                               keywidth = unit(0.6, "cm"), keyheight = unit(0.35, "cm")),
           colour = guia_modelo, linetype = guia_modelo) +
    scale_x_continuous(breaks = seq(2006, 2024, 2)) +
    facet_wrap(~ painel, ncol = 2, scales = "free_y") +
    labs(x = NULL, y = rotulo_y) +
    tema_artigo + pos_legenda
}

grafico_indicador <- function(v, tipos) {
  obs <- prevalencias |> filter(indicador == v, tipo_grupo %in% tipos) |> rotular()
  prd <- predicoes |> filter(indicador == v, tipo_grupo %in% tipos,
                             modelo %in% MODELOS_BASE) |> rotular()
  grafico_series(obs, prd, paste0(rot_en[v], " (%)"))
}

# Fig 1 do artigo: população total, um painel por indicador
grafico_total <- function() {
  rot <- rotulos_painel |>
    filter(tipo_grupo == "total") |>
    mutate(painel = sprintf("%s\nBest: %s (RMSE %.2f)", rot_en[indicador], melhor, rmse))
  niveis <- rot$painel[match(indicadores$var, rot$indicador)]
  add <- function(df) df |>
    filter(tipo_grupo == "total") |>
    left_join(select(rot, indicador, painel), by = "indicador") |>
    mutate(painel = factor(painel, levels = niveis))
  obs <- add(prevalencias)
  prd <- add(filter(predicoes, modelo %in% MODELOS_BASE))
  grafico_series(obs, prd, "Prevalence (%)")
}

salvar <- function(g, nome, altura_cm) {
  arq <- file.path(DIR_FIG, nome)
  ggsave(paste0(arq, ".png"), g, width = 17, height = altura_cm, units = "cm", dpi = 300)
  ggsave(paste0(arq, ".tiff"), g, width = 17, height = altura_cm, units = "cm", dpi = 300,
         compression = "lzw")
}

for (k in seq_len(nrow(indicadores))) {
  v <- indicadores$var[k]
  salvar(grafico_indicador(v, c("total", "regiao")), sprintf("fig_%02d_%s_regioes", 2 * k - 1, v), 20)
  salvar(grafico_indicador(v, c("sexo", "faixa")),   sprintf("fig_%02d_%s_sexo_idade", 2 * k, v), 20)
  message("Figuras salvas: ", v)
}

salvar(grafico_total(), "fig_artigo_1_total_indicadores", 22)
message("Figura 1 do artigo salva")

# ---------------------------------------------------------------------------
# Figura-síntese: "Does one model fit all?"
# ---------------------------------------------------------------------------
hm <- metricas |>
  filter(modelo %in% MODELOS_BASE) |>
  mutate(modelo = factor(modelo, levels = MODELOS_BASE),
         grupo_en = factor(ROTULO_GRUPO_EN[grupo], levels = rev(ROTULO_GRUPO_EN[NIVEIS_GRUPO])),
         ind_en = factor(rot_en[indicador], levels = indicadores$rotulo_en))

g_hm <- ggplot(hm, aes(modelo, grupo_en, fill = factor(rank_base, levels = 1:5))) +
  geom_tile(colour = "white") +
  geom_text(aes(label = rank_base), size = 2.6, family = "serif") +
  scale_fill_manual(values = c("1" = "#1B7837", "2" = "#7FBF7B", "3" = "#D9F0D3",
                               "4" = "#F7F7F7", "5" = "#E7D4E8"),
                    name = "Rank\n(1 = lowest RMSE)", drop = FALSE) +
  scale_x_discrete(labels = ROTULO_MODELO_EN) +
  facet_wrap(~ ind_en, ncol = 4, labeller = label_wrap_gen(22)) +
  labs(x = NULL, y = NULL) +
  tema_artigo +
  theme(panel.grid = element_blank(),
        panel.spacing.y = unit(2.2, "cm"),   # espaço para os rótulos do eixo x do painel superior direito
        legend.position = "inside", legend.position.inside = c(0.88, 0.25),
        legend.direction = "vertical")

salvar(g_hm, "fig_sintese_ranking_modelos", 18)
