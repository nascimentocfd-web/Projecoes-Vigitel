################################################################################
# 00_config.R — Configurações gerais, pacotes e funções auxiliares
# Projeto: Predictive Models for Health Risk Behaviors ... (Vigitel 2006–2024)
# Todos os outros scripts começam com source("00_config.R").
################################################################################

pacotes <- c("data.table", "survey", "dplyr", "tidyr", "purrr", "forecast",
             "prophet", "prais", "ggplot2", "flextable", "officer", "PMCMRplus")
faltando <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltando) > 0) install.packages(faltando)
suppressPackageStartupMessages(invisible(lapply(pacotes, library, character.only = TRUE)))

# ---------------------------------------------------------------------------
# Caminhos
# ---------------------------------------------------------------------------
ARQ_MICRODADOS <- "vigitel-2006-2024-peso-rake.csv"  # .csv ou .rds
SEP_CSV <- "auto"   # separador do CSV ("auto", ";" ou ",")
DEC_CSV <- "."      # troque para "," se o peso vier como texto (arquivo de read.csv2)
DIR_SAIDA <- "saidas"
dir.create(file.path(DIR_SAIDA, "figuras"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Parâmetros do estudo
# ---------------------------------------------------------------------------
ANO_FIM_TREINO <- 2019
ANOS_TESTE     <- 2020:2024
ANO_SEM_DADO   <- 2022   # interpolado linearmente (média de 2021 e 2023)
EXCLUIR_2022_DO_TESTE <- TRUE  # 2022 é interpolado: não entra no RMSE/MAE
SEMENTE <- 2025                # reprodutibilidade dos intervalos do Prophet
LIMITE_RHO <- 0.95             # acima disso a tendência é sinalizada como instável

MODELOS_BASE <- c("LM", "ARIMA", "HOLT", "PROPHET", "PRAIS")

# ---------------------------------------------------------------------------
# Indicadores (nomes e rotinas conferidos no dicionário oficial 2006-2024,
# aba "Indicadores_Vigitel").
# - obesid_i: obesidade com imputação de peso/altura (IMC_i >= 30), conforme
#   orientação oficial para indicadores de estado nutricional.
# - flvreco: consumo recomendado (>= 5 porções/dia de frutas e hortaliças em
#   >= 5 dias/semana), indicador definido pela equipe para o artigo.
# - nivel: rótulo da categoria de interesse nas bases ANTIGAS (com rótulos).
#   Nas bases NOVAS (ago/2026, sem rótulos) a categoria de interesse é o código 1.
#   O script 01 reconhece os dois formatos automaticamente.
# ---------------------------------------------------------------------------
indicadores <- data.frame(
  var        = c("obesid_i", "fumante", "ativo_livre", "flvreco", "alcabu", "hart", "diab"),
  nivel      = c("Sim", "Sim", "Ativo", "Sim", "Sim", "Sim", "Sim"),
  ano_inicio = c(2006, 2006, 2009, 2008, 2006, 2006, 2006),
  rotulo_pt  = c("Obesidade", "Tabagismo", "Atividade física no tempo livre",
                 "Consumo recomendado de frutas e hortaliças",
                 "Consumo episódico pesado de bebidas alcoólicas",
                 "Diagnóstico de hipertensão", "Diagnóstico de diabetes"),
  rotulo_en  = c("Obesity", "Smoking", "Leisure-time physical activity",
                 "Recommended fruit and vegetable intake", "Heavy episodic drinking",
                 "Self-reported hypertension", "Self-reported diabetes"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Estratos
# ---------------------------------------------------------------------------
grupos <- c(total = "Total", regiao = "Região", sexo = "Sexo", faixa = "Faixa etária")

mapa_regiao <- c(
  "aracaju" = "Nordeste", "fortaleza" = "Nordeste", "joao pessoa" = "Nordeste",
  "maceio" = "Nordeste", "natal" = "Nordeste", "recife" = "Nordeste",
  "salvador" = "Nordeste", "sao luis" = "Nordeste", "teresina" = "Nordeste",
  "belem" = "Norte", "boa vista" = "Norte", "macapa" = "Norte", "manaus" = "Norte",
  "porto velho" = "Norte", "rio branco" = "Norte", "palmas" = "Norte",
  "belo horizonte" = "Sudeste", "rio de janeiro" = "Sudeste",
  "sao paulo" = "Sudeste", "vitoria" = "Sudeste",
  "campo grande" = "Centro-Oeste", "cuiaba" = "Centro-Oeste",
  "goiania" = "Centro-Oeste", "distrito federal" = "Centro-Oeste",
  "curitiba" = "Sul", "florianopolis" = "Sul", "porto alegre" = "Sul"
)

# Códigos das bases sem rótulos (dicionário oficial 2006-2024)
CODIGOS_CIDADE <- c(
  "1" = "aracaju", "2" = "belem", "3" = "belo horizonte", "4" = "boa vista",
  "5" = "campo grande", "6" = "cuiaba", "7" = "curitiba", "8" = "florianopolis",
  "9" = "fortaleza", "10" = "goiania", "11" = "joao pessoa", "12" = "macapa",
  "13" = "maceio", "14" = "manaus", "15" = "natal", "16" = "palmas",
  "17" = "porto alegre", "18" = "porto velho", "19" = "recife", "20" = "rio branco",
  "21" = "rio de janeiro", "22" = "salvador", "23" = "sao luis", "24" = "sao paulo",
  "25" = "teresina", "26" = "vitoria", "27" = "distrito federal"
)
CODIGOS_SEXO <- c("1" = "masculino", "2" = "feminino")

NIVEIS_GRUPO <- c("Total", "Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul",
                  "Masculino", "Feminino", "18 a 34 anos", "35 a 59 anos", "60 anos ou mais")
ROTULO_GRUPO_EN <- c("Total" = "Total", "Norte" = "North", "Nordeste" = "Northeast",
                     "Centro-Oeste" = "Central-West", "Sudeste" = "Southeast", "Sul" = "South",
                     "Masculino" = "Men", "Feminino" = "Women",
                     "18 a 34 anos" = "18–34 years", "35 a 59 anos" = "35–59 years",
                     "60 anos ou mais" = "60+ years")

# Paleta Okabe-Ito (acessível a daltônicos)
CORES_MODELOS <- c(LM = "#E69F00", ARIMA = "#56B4E9", HOLT = "#009E73",
                   PROPHET = "#CC79A7", PRAIS = "#D55E00", ENSEMBLE = "#000000")

CHAVES <- c("indicador", "tipo_grupo", "grupo")

# Rótulos em inglês (artigo em inglês) para tabelas e figuras
ROTULO_MODELO_EN <- c(LM = "Linear regression", ARIMA = "ARIMA", HOLT = "Holt",
                      PROPHET = "Prophet", PRAIS = "Prais-Winsten")
ROTULO_ESTRATO_EN <- c(total = "Total", regiao = "Region", sexo = "Sex", faixa = "Age group")
MARCA_DECIMAL <- "."   # "." para o artigo em inglês; "," para textos em português

# ---------------------------------------------------------------------------
# Funções auxiliares
# ---------------------------------------------------------------------------
# Prais-Winsten sem imprimir as iterações no console
pw <- function(formula, dados) {
  m <- NULL
  invisible(capture.output(m <- prais_winsten(formula, data = as.data.frame(dados), index = "ano")))
  m
}

fmt_num <- function(x, d = 2) ifelse(is.na(x), "", formatC(x, format = "f", digits = d,
                                                            decimal.mark = MARCA_DECIMAL))
fmt_p   <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, paste0("<0", MARCA_DECIMAL, "001"),
                              formatC(p, format = "f", digits = 3, decimal.mark = MARCA_DECIMAL)))
