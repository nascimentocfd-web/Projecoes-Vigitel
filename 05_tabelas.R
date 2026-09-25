################################################################################
# 05_tabelas.R — Tabelas do artigo e suplementares, em inglês, em Word
#   Artigo:       Table 1 (APC, Prais-Winsten) e Table 2 (resumo do desempenho)
#   Suplementar:  RMSE por série (um bloco por indicador) e frequência de
#                 melhor desempenho por grupo
################################################################################

source("00_config.R")
tendencia <- readRDS(file.path(DIR_SAIDA, "tendencia.rds"))
metricas  <- readRDS(file.path(DIR_SAIDA, "metricas.rds")) |>
  filter(modelo %in% MODELOS_BASE)                      # sem ENSEMBLE

rot_ind <- setNames(indicadores$rotulo_en, indicadores$var)
ordenar <- function(df) df |>
  mutate(estrato  = unname(ROTULO_ESTRATO_EN[tipo_grupo]),
         subgrupo = unname(ROTULO_GRUPO_EN[grupo])) |>
  arrange(match(grupo, NIVEIS_GRUPO))

ft_base <- function(ft) ft |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 8, part = "all") |>
  align(align = "center", part = "all") |>
  align(j = 1:2, align = "left", part = "body") |>
  merge_v(j = 1) |>
  theme_booktabs() |>
  autofit()

# ---------------------------------------------------------------------------
# Table 1 — Annual percent change (APC) and 95% CI
# ---------------------------------------------------------------------------
t1_long <- tendencia |> ordenar()
grade <- t1_long |> distinct(estrato, subgrupo, grupo)

t1 <- grade |> select(estrato, subgrupo)
negrito_t1 <- list()
for (v in indicadores$var) {
  d <- t1_long |> filter(indicador == v)
  d <- d[match(grade$grupo, d$grupo), ]
  t1[[rot_ind[v]]] <- paste0(fmt_num(d$vpa, 2), " (", fmt_num(d$vpa_inf, 2), "; ",
                             fmt_num(d$vpa_sup, 2), ")", ifelse(d$rho_mqo_log >= LIMITE_RHO, "†", ""))
  negrito_t1[[rot_ind[v]]] <- which(d$p_vpa < 0.05)
}
names(t1)[1:2] <- c("Stratum", "Subgroup")
ft1 <- flextable(t1) |> ft_base()
for (col in names(negrito_t1)) if (length(negrito_t1[[col]]))
  ft1 <- bold(ft1, i = negrito_t1[[col]], j = col, part = "body")
ft1 <- ft1 |> width(j = 1, width = 0.8) |> width(j = 2, width = 1.0) |> width(j = 3:9, width = 1.05)

nota_t1 <- paste(
  "APC: annual percent change, estimated by Prais-Winsten regression of the log10-transformed prevalence; 95% CI in parentheses.",
  "Bold: statistically significant trend (p < 0.05).",
  "Series start in 2008 (fruit and vegetable intake) and 2009 (leisure-time physical activity).",
  "† First-order autocorrelation of residuals close to 1 (≥ 0.95): the linear trend fits the series poorly and the estimate is unstable; interpret with caution.")

# ---------------------------------------------------------------------------
# Table 2 — Predictive performance by indicator
# (frequency of best performance, mean rank and Friedman test, Holm-adjusted)
# ---------------------------------------------------------------------------
matriz_rmse <- function(df) {
  w <- df |> select(indicador, grupo, modelo, RMSE) |>
    pivot_wider(names_from = modelo, values_from = RMSE)
  m <- as.matrix(w[, MODELOS_BASE])
  m[complete.cases(m), , drop = FALSE]
}
resumir <- function(d) {
  m <- matriz_rmse(d)
  list(vit = sapply(MODELOS_BASE, function(mm) sum(d$modelo == mm & d$rank_base == 1, na.rm = TRUE)),
       rk  = colMeans(t(apply(m, 1, rank)))[MODELOS_BASE],
       p   = friedman.test(m)$p.value,
       n   = nrow(m))
}
res_ind <- lapply(indicadores$var, function(v) resumir(filter(metricas, indicador == v)))
res_tot <- resumir(metricas)
p_holm  <- p.adjust(sapply(res_ind, `[[`, "p"), method = "holm")

COL_VIT <- paste0("Frequency of best performance__", ROTULO_MODELO_EN[MODELOS_BASE])
COL_RK  <- paste0("Mean rank__", ROTULO_MODELO_EN[MODELOS_BASE])
linha_resumo <- function(rotulo, r, p_txt) {
  as.data.frame(c(list(Indicator = unname(rotulo)),
                  setNames(as.list(as.character(r$vit)), COL_VIT),
                  setNames(as.list(fmt_num(r$rk, 2)), COL_RK),
                  list(`Friedman__p` = p_txt)), check.names = FALSE)
}
lista_res <- c(res_ind, list(res_tot))
t2 <- bind_rows(
  lapply(seq_along(res_ind), function(k)
    linha_resumo(rot_ind[indicadores$var[k]], res_ind[[k]], fmt_p(p_holm[k]))),
  linha_resumo(paste0("All series (n = ", res_tot$n, ")"), res_tot, fmt_p(res_tot$p)))

ft2 <- flextable(t2) |>
  separate_header(split = "__") |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 8, part = "all") |>
  align(align = "center", part = "all") |>
  align(j = 1, align = "left", part = "body") |>
  theme_booktabs() |>
  hline(i = nrow(t2) - 1, border = officer::fp_border(width = 0.5)) |>
  autofit()
for (i in seq_along(lista_res)) {
  r <- lista_res[[i]]
  ft2 <- bold(ft2, i = i, j = COL_RK[which(round(r$rk, 2) == min(round(r$rk, 2)))])
  ft2 <- bold(ft2, i = i, j = COL_VIT[which(r$vit == max(r$vit))])
}
ft2 <- ft2 |> width(j = 1, width = 1.9) |> width(j = 2:11, width = 0.7) |> width(j = 12, width = 0.6)

nota_t2 <- paste(
  "Frequency of best performance: number of subgroups (n = 11 per indicator) in which the model had the lowest RMSE;",
  "ties were credited to all tied models, so totals may exceed 11.",
  "Mean rank: average position of the model across subgroups (1 = lowest RMSE). Bold: highest frequency and best mean rank.",
  "p: Friedman test; by indicator, adjusted for multiple comparisons with the Holm method; in the last row, single test without adjustment.",
  "RMSE computed for the observed years of the test period (2020, 2021, 2023 and 2024).")

# ---------------------------------------------------------------------------
# Supplementary — RMSE of each model in each series (one block per indicator)
# ---------------------------------------------------------------------------
tabelas_rmse <- lapply(indicadores$var, function(v) {
  d <- metricas |> filter(indicador == v) |> ordenar()
  w <- d |> select(estrato, subgrupo, modelo, RMSE) |>
    pivot_wider(names_from = modelo, values_from = RMSE) |>
    select(estrato, subgrupo, all_of(MODELOS_BASE))
  vit <- d |> filter(rank_base == 1) |> count(modelo)
  linha_freq <- as.list(setNames(rep("", ncol(w)), names(w)))
  linha_freq$estrato <- "Frequency of best performance"
  for (mm in MODELOS_BASE) linha_freq[[mm]] <- as.character(sum(vit$n[vit$modelo == mm]))

  wf <- w |> mutate(across(all_of(MODELOS_BASE), ~ fmt_num(.x, 3)))
  wf <- bind_rows(wf, as.data.frame(linha_freq, check.names = FALSE))
  ft <- flextable(wf) |>
    set_header_labels(values = c(estrato = "Stratum", subgrupo = "Subgroup",
                                 as.list(ROTULO_MODELO_EN[MODELOS_BASE]))) |>
    ft_base()
  for (i in seq_len(nrow(w))) {
    rmse_i <- round(unlist(w[i, MODELOS_BASE]), 3)
    ft <- bold(ft, i = i, j = MODELOS_BASE[which(rmse_i == min(rmse_i, na.rm = TRUE))])
  }
  ft
})
names(tabelas_rmse) <- indicadores$var

# ---------------------------------------------------------------------------
# Supplementary — Frequency of best performance by subgroup (n = 77 series)
# ---------------------------------------------------------------------------
t3 <- metricas |> filter(rank_base == 1) |>
  count(tipo_grupo, grupo, modelo) |>
  complete(nesting(tipo_grupo, grupo), modelo = MODELOS_BASE, fill = list(n = 0)) |>
  pivot_wider(names_from = modelo, values_from = n) |>
  ordenar() |>
  select(estrato, subgrupo, all_of(MODELOS_BASE))
tot <- colSums(t3[, MODELOS_BASE])
n_series <- nrow(distinct(metricas, across(all_of(CHAVES))))
t3f <- t3 |> mutate(across(all_of(MODELOS_BASE), as.character))
t3f <- bind_rows(
  t3f,
  as.data.frame(as.list(c(estrato = "Total, n (%)", subgrupo = "",
    setNames(paste0(tot, " (", fmt_num(100 * tot / n_series, 1), ")"), MODELOS_BASE))), check.names = FALSE),
  as.data.frame(as.list(c(estrato = "Rank", subgrupo = "",
    setNames(as.character(rank(-tot, ties.method = "min")), MODELOS_BASE))), check.names = FALSE))
ft3 <- flextable(t3f) |>
  set_header_labels(values = c(estrato = "Stratum", subgrupo = "Subgroup",
                               as.list(ROTULO_MODELO_EN[MODELOS_BASE]))) |>
  ft_base()

nota_empate <- "Bold: lowest RMSE among the five models. Ties were credited to all tied models. RMSE computed for 2020, 2021, 2023 and 2024 (interpolated 2022 value excluded)."

# ---------------------------------------------------------------------------
# Word documents (landscape)
# ---------------------------------------------------------------------------
paisagem <- prop_section(page_size = page_size(orient = "landscape"))
add_titulo <- function(doc, txt) body_add_fpar(doc, fpar(ftext(txt,
  fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")),
  fp_p = fp_par(padding.bottom = 6)))
add_nota <- function(doc, txt) body_add_fpar(doc, fpar(ftext(txt,
  fp_text(font.size = 9, font.family = "Times New Roman")),
  fp_p = fp_par(padding.top = 4)))

doc <- read_docx() |>
  body_set_default_section(paisagem) |>
  add_titulo("Table 1. Annual percent change (APC) in the prevalence of the indicators by population subgroup, Vigitel, 2006–2024.") |>
  body_add_flextable(ft1) |>
  add_nota(nota_t1) |>
  body_add_break() |>
  add_titulo("Table 2. Predictive performance of the models by indicator in the test period (2020, 2021, 2023 and 2024), Vigitel.") |>
  body_add_flextable(ft2) |>
  add_nota(nota_t2)
print(doc, target = file.path(DIR_SAIDA, "tabelas_artigo.docx"))

sup <- read_docx() |> body_set_default_section(paisagem)
for (k in seq_along(indicadores$var)) {
  v <- indicadores$var[k]
  sup <- sup |>
    add_titulo(paste0("Supplementary table (RMSE, ", k, "/7). RMSE of the models by population subgroup in the test period: ",
                      tolower(rot_ind[v]), ".")) |>
    body_add_flextable(tabelas_rmse[[v]]) |>
    add_nota(nota_empate) |>
    body_add_break()
}
sup <- sup |>
  add_titulo("Supplementary table (frequency). Frequency of best performance of each model by population subgroup (77 series).") |>
  body_add_flextable(ft3) |>
  add_nota("Ties were credited to all tied models. Rank: position of the model by total frequency of best performance.")
print(sup, target = file.path(DIR_SAIDA, "tabelas_suplementares.docx"))

cat("Tables saved in saidas/tabelas_artigo.docx and saidas/tabelas_suplementares.docx\n")
