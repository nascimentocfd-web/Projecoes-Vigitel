################################################################################
# 01_prevalencias.R — Prevalências anuais ponderadas (7 indicadores × 11 grupos)
#
# Substitui 1_.R e todos os 2_*.R.
# Economia de memória: lê só as colunas necessárias, calcula tudo numa passada
# e salva UMA tabela longa pequena. Os microdados são descartados no final;
# os demais scripts só usam a tabela salva.
################################################################################

source("00_config.R")

# ---------------------------------------------------------------------------
# 1. Leitura (somente colunas necessárias)
# ---------------------------------------------------------------------------
colunas <- c("ano", "cidade", "q6", "q7", "pesorake2025", indicadores$var)

if (grepl("\\.rds$", ARQ_MICRODADOS, ignore.case = TRUE)) {
  vig <- as.data.table(readRDS(ARQ_MICRODADOS))[, ..colunas]
} else {
  vig <- fread(ARQ_MICRODADOS, select = colunas, sep = SEP_CSV, dec = DEC_CSV,
               encoding = "UTF-8", showProgress = FALSE)
}

if (!is.numeric(vig$pesorake2025))
  stop("pesorake2025 não é numérico: ajuste DEC_CSV/SEP_CSV em 00_config.R")
vig[, q6 := as.numeric(q6)]

# ---------------------------------------------------------------------------
# 2. Indicadores -> 0/1 (confira no console se os níveis estão corretos)
# ---------------------------------------------------------------------------
for (i in seq_len(nrow(indicadores))) {
  v <- indicadores$var[i]
  cat("\n---", v, "(nível de interesse:", indicadores$nivel[i], ")\n")
  print(table(vig[[v]], useNA = "ifany"))
  # Aceita os dois formatos: código numérico (1 = categoria de interesse)
  # ou rótulo em texto ("Sim"/"Ativo") das bases antigas
  x <- tolower(trimws(as.character(vig[[v]])))
  sim <- c("1", tolower(indicadores$nivel[i]))
  set(vig, j = v, value = ifelse(is.na(x) | x == "", NA_real_, as.numeric(x %in% sim)))
  if (isTRUE(all(vig[[v]] == 0, na.rm = TRUE)))
    stop("Indicador ", v, " ficou todo igual a 0: confira o nível em 00_config.R")
}

# ---------------------------------------------------------------------------
# 3. Estratos
# ---------------------------------------------------------------------------
# Bases novas trazem cidade e sexo como códigos; bases antigas, como texto
vig[, cidade := tolower(trimws(as.character(cidade)))]
vig[cidade %in% names(CODIGOS_CIDADE), cidade := unname(CODIGOS_CIDADE[cidade])]
vig[, q7 := tolower(trimws(as.character(q7)))]
vig[q7 %in% names(CODIGOS_SEXO), q7 := unname(CODIGOS_SEXO[q7])]
vig[, `:=`(
  total  = "Total",
  regiao = unname(mapa_regiao[cidade]),
  sexo   = fcase(q7 == "masculino", "Masculino", q7 == "feminino", "Feminino"),
  faixa  = fcase(q6 < 35, "18 a 34 anos", q6 < 60, "35 a 59 anos", q6 >= 60, "60 anos ou mais")
)]

if (anyNA(vig$regiao))
  stop("Cidades sem região: ", paste(unique(vig$cidade[is.na(vig$regiao)]), collapse = ", "))
if (all(is.na(vig$sexo)))
  stop("Sexo não reconhecido: confira a codificação de q7")
print(vig[, .N, keyby = .(ano)])  # nenhum registro em 2022 é o esperado

vig[, c("cidade", "q6", "q7") := NULL]

# ---------------------------------------------------------------------------
# 4. Desenho amostral (um único desenho; estratos como DOMÍNIOS via svyby)
#    Isso é o correto para variância em subpopulações, em vez de criar um
#    svydesign separado para cada subconjunto de dados.
# ---------------------------------------------------------------------------
des <- svydesign(ids = ~1, weights = ~pesorake2025, data = as.data.frame(vig))
rm(vig); invisible(gc())

estimar <- function(v, g, ano_ini) {
  d <- des[des$variables$ano >= ano_ini, ]
  r <- svyby(as.formula(paste0("~", v)), as.formula(paste0("~ano + ", g)),
             d, svymean, na.rm = TRUE)
  data.frame(indicador = v, tipo_grupo = g, grupo = as.character(r[[g]]),
             ano = r$ano, prev = 100 * r[[v]], ep = 100 * as.numeric(SE(r)),
             stringsAsFactors = FALSE)
}

tarefas <- expand.grid(i = seq_len(nrow(indicadores)), g = names(grupos),
                       stringsAsFactors = FALSE)

prevalencias <- bind_rows(lapply(seq_len(nrow(tarefas)), function(k) {
  i <- tarefas$i[k]; g <- tarefas$g[k]
  message(sprintf("[%02d/%d] %s × %s", k, nrow(tarefas), indicadores$var[i], g))
  estimar(indicadores$var[i], g, indicadores$ano_inicio[i])
}))
rm(des); invisible(gc())

# ---------------------------------------------------------------------------
# 5. IC 95% e interpolação de 2022 (automática, sem digitar valores)
# ---------------------------------------------------------------------------
prevalencias <- prevalencias |>
  mutate(ic_inf = prev - 1.96 * ep, ic_sup = prev + 1.96 * ep, interpolado = FALSE)

interpolar <- function(d, ...) {
  if (ANO_SEM_DADO %in% d$ano) return(d)
  a <- d[d$ano == ANO_SEM_DADO - 1, ]
  b <- d[d$ano == ANO_SEM_DADO + 1, ]
  if (nrow(a) != 1 || nrow(b) != 1) return(d)
  novo <- a
  novo$ano <- ANO_SEM_DADO
  for (col in c("prev", "ep", "ic_inf", "ic_sup")) novo[[col]] <- (a[[col]] + b[[col]]) / 2
  novo$interpolado <- TRUE
  arrange(bind_rows(d, novo), ano)
}

prevalencias <- prevalencias |>
  group_by(across(all_of(CHAVES))) |>
  group_modify(interpolar) |>
  ungroup() |>
  arrange(match(indicador, indicadores$var), match(grupo, NIVEIS_GRUPO), ano)

# ---------------------------------------------------------------------------
# 6. Checagens e gravação
# ---------------------------------------------------------------------------
resumo <- prevalencias |> count(indicador, name = "n_linhas")
print(resumo)
cat("Número de séries:", nrow(distinct(prevalencias, across(all_of(CHAVES)))), "(esperado: 77)\n")

saveRDS(prevalencias, file.path(DIR_SAIDA, "prevalencias.rds"))
fwrite(prevalencias, file.path(DIR_SAIDA, "prevalencias.csv"))
