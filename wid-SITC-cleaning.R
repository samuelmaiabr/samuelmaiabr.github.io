# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
# WID e SITC CLEANING ####
### ~ ####
# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #


# =========== # =========== # =========== # =========== # =========== # =========== #
## 0. PACOTES E CONFIGURANDO MÁQUINA ####

### 0.1 Pacotes ####

install.packages(c(
    "dplyr", "haven", "tidyverse", "ggplot2", "readr", "data.table", "furrr", 
    "here", "tictoc", "showtext", "countrycode", "tikzDevice", "stringr"
))

library(dplyr)
library(haven)
library(tidyverse)
library(ggplot2)
library(readr)
library(data.table)
library(furrr)  # Para processamento paralelo
library(here)   # Para gerenciar caminhos
library(tictoc) # Para medir o tempo de execução
library(showtext)
library(countrycode)
library(tikzDevice)
library(stringr)

showtext_auto()

# Limpar o ambiente para evitar conflitos
rm(list = ls())
gc()

#### 0.2 Configurando processamento paralelo ####
# Definir o número de núcleos para processamento paralelo
plan(multisession, workers = 4)
# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #


# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####

# WID ####
# =========== # =========== # =========== # =========== # =========== # =========== #


# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
## 1. CARREGAR DADOS ####

# Caminho para a pasta com os arquivos
wid_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/wid_all_data"

# Listar todos os arquivos CSV disponíveis
wid_files <- list.files(wid_path, pattern = "WID_data_.*\\.csv", full.names = TRUE)
cat("Arquivos disponíveis no WID:\n", paste(wid_files, collapse = "\n"))

# Primeiros listados
print(head(wid_files))  
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
## 2. PROCESSAR E COMBINAR WID ####

# O total da WID são 778 arquivos .csv, cada um com dados para mais de 270 e territórios, mais de 1700 variáveis e quase 57 milhões de observações.

## Primeiro, para baixar bd: <https://wid.world/data/>.

# Segundo, reuniremos todos esses arquivos num único df. Dado o tamanho do arquivo, usaremos ferramentas como processamento paralelo (comando "future_map_dfr()").

# Finalmente, selecionaremos apenas aquelas variáveis que nos interessam, o que reduzirá bastante o arquivo. 

### 2.1 Combinando WID ####

#### 2.1.1 Função para processar cada arquivo ####
wid_full <- read_csv("/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/wid_combined.csv")

process_wid_file <- function(file) {
    message("Processando: ", file)
    
    # Ler os dados, forçando tipos de colunas
    data <- read_delim(file, delim = ";", 
        col_types = cols(
            country = col_character(),
            variable = col_character(),
            percentile = col_character(),  # Manter percentis como string
            year = col_double(),  # Ano como número
            value = col_double(),  # Valores numéricos
            age = col_character(),  # Idade como string
            pop = col_character()   # Unidade populacional como string
        ), 
        show_col_types = FALSE)
    
    # Preservar os percentis como STRINGS para manter casas decimais
    data <- data %>%
        mutate(
            lower_percentile = str_extract(percentile, "^p[0-9\\.]+") %>% 
                str_remove("^p"),  # Mantém como string
            
            upper_percentile = str_extract(percentile, "p[0-9\\.]+$") %>% 
                str_remove("^p")  # Mantém como string
        ) %>%
        select(country, variable, lower_percentile, upper_percentile, year, value, age, pop)
    
    return(data)
}


#### 2.1.2 Testar com uma amostra menor ####
# Pegar os primeiros 5 arquivos para teste
wid_test_files <- wid_files[1:5]

# Rodar o processamento de teste com 5 arquivos e medir tempo total
tic("Processamento de Teste com 5 arquivos")
wid_test <- future_map_dfr(wid_test_files, process_wid_file)
toc()  # Finaliza contagem de tempo

# Verificar a contagem de percentis processados corretamente
wid_test %>%
    count(lower_percentile, upper_percentile) %>%
    arrange(lower_percentile, upper_percentile) %>%
    print(n = Inf)  # Para visualizar todas as linhas


#### 2.1.3 Processar TODOS os arquivos #####
tic("Processamento Completo WID")
wid_full <- future_map_dfr(wid_files, process_wid_file)
toc()  # Finaliza contagem de tempo

#### 2.1.4 Testar estrutura final #####
glimpse(wid_full)

unique(wid_full$lower_percentile)
unique(wid_full$upper_percentile)


#### 2.1.5 Validar variáveis de interesse ####
wid_full %>%
    filter(variable %in% c("sptincj992", "sdiincj992")) %>%
    count(lower_percentile, upper_percentile) %>%
    print(n = Inf)  # Exibir tudo para checagem



#### 2.1.6 Salvar #####
write_csv(wid_full, "wid_combined.csv")
saveRDS(wid_full, "wid_combined.rds")


#### 2.1.7 Testar reabertura #####
# Para CSV
wid_test_csv <- read_csv("wid_combined.csv")
glimpse(wid_test_csv)

# Para RDS
wid_test_rds <- readRDS("wid_combined.rds")
glimpse(wid_test_rds)
# =========== # =========== # =========== # =========== # =========== # =========== #


### 2.2 Selecionando variáveis #####

#### 2.2.1 Introduzindo o WID #####

# DINA income concepts are distributed income concepts that are consistent with national accounts aggregates.
# Preciso extrair as variáveis do WID que são importantes para minha análise:
# Gini, Razão de Palma (10/50) e parcelas apropriadas pelo topo (10%, 1%, etc.)

# De acordo com  código de variáveis do WID (<https://wid.world/codes-dictionary/>), as variáveis são  assim:
# g [gini], r [Palma Ratio], s [share]

# Pretax income
# ptinc 	(=) pretax national income

# Post-tax income:
# diinc 	(=) post-tax national income 	post-tax national income
# cainc 	(=) post-tax disposable income 	post-tax disposable income

# Factor income
# fainc 	(=) factor national income 	factor national income
# flinc 	(=) labor factor income 	labor factor income

# Fiscal income
# fiinc 	(=) fiscal income

# Percentis: p0p100, p0p50, p50p90, p99p100,99p100, p99.99p100


#### 2.2.2 Carregar dados compilados #####
wid_full <- read_csv("/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/wid_combined.csv")

# Selecionando os percentis
# Obter todos os percentis únicos da base (incluindo decimais)
unique_lower_percentiles <- unique(wid_full$lower_percentile)
unique_upper_percentiles <- unique(wid_full$upper_percentile)

# Criar uma lista de subconjuntos dividindo a base por percentis
wid_splits <- split(wid_full, wid_full$lower_percentile)

# Função de filtragem para cada subconjunto
filter_function <- function(df) {
    df %>%
        filter(
            as.character(lower_percentile) %in% as.character(unique_lower_percentiles) &
                as.character(upper_percentile) %in% as.character(unique_upper_percentiles)
        )
}

# Aplicar a função em paralelo
wid_filtered <- future_map_dfr(wid_splits, filter_function)

# Salvar os dados filtrados
write_csv(wid_filtered, "wid_filtered.csv")
saveRDS(wid_filtered, "wid_filtered.rds")


# Variáveis relacionadas a desigualdade, GDP e fatores de conversão
selected_variables_concepts <- c("sptinc", "sdiinc", "scainc", "gptinc", "gptinc", "gdiinc", "gfiinc", "rptinc")  # Desigualdade
selected_percentiles <- selected_percentiles <- c("0", "50", "90", "99", "99.9", "99.99", "99.999", "99.9999", "99.99999", "100")      # Percentis
selected_gdp_population <- c("agdproi992", "npopuli992")                                     # GDP e População
conversion_variables <- c("xlcuspi999", "xlcusxi999")                                        # Conversores PPP e Mercado

# Combinação de todas as variáveis selecionadas
all_selected_variables <- c(selected_variables_concepts, selected_percentiles, selected_gdp_population, conversion_variables)


#### 2.2.3. Filtragem Inicial ####
wid_filtered <- wid_full %>%
    filter(
        # Variáveis de interesse
        str_sub(variable, 1, 6) %in% selected_variables_concepts |
            variable %in% selected_gdp_population |
            variable %in% conversion_variables,
        
        # Aplicação de regras específicas
        (
            # Variáveis de shares precisam de percentis
            str_sub(variable, 1, 6) %in% c("sptinc", "sdiinc", "scainc") &
                as.character(lower_percentile) %in% selected_percentiles &
                as.character(upper_percentile) %in% c("50", "90", "100")
        ) |
            (
                # Gini e Palma não precisam de percentis
                str_sub(variable, 1, 6) %in% c("gptinc", "gptinc", "gdiinc", "gfiinc", "rptinc") &
                    as.character(lower_percentile) == "0" &
                    as.character(upper_percentile) == "100"
            ) |
            (
                # GDP, população e fatores de conversão não requerem percentis
                variable %in% selected_gdp_population | variable %in% conversion_variables
            )
    )

glimpse(wid_filtered)

# Salvar
write_csv(wid_filtered, "wid_filtered.csv")
saveRDS(wid_filtered, "wid_filtered.rds")



#### 2.2.4 Gráficos de exploração ####

# Para caso o tikz esteja ativo
if (dev.cur() != 1) dev.off() # Fecha qualquer dispositivo gráfico anterior
dev.new() # Cria uma nova janela para o gráfico


##### Share de renda #####
# Filtrar dados para o Brasil, França e EUA, e participação dos 1% mais ricos
df_share_top1_all <- wid_filtered %>%
    filter(
        country %in% c("BR", "FR", "US"),
        variable == "sptincj992",
        lower_percentile == 99.000,  # Filtra para 99.000 especificamente
        upper_percentile == 100,     # Filtra para 100 especificamente
        year >= 1820
    ) %>%
    select(country, year, value, lower_percentile, upper_percentile)

unique(wid_filtered$variable)
glimpse(df_share_top1_all)

# Plotemos
graph_df_share_top1 <- ggplot(df_share_top1_all, aes(x = year, y = value * 100, color = country, group = country)) +
    geom_line(linewidth = 0.9) +  # Use 'linewidth' ao invés de 'size'
    geom_point(size = 1.5, alpha = 0.6) +  # Para os pontos
    theme_minimal() +
    labs(
        title = "Participação dos 1\\% mais ricos (Brasil, França, EUA, 1820-presente)",  # Escapar %
        x = "Ano",
        y = "Participação dos 1\\% mais ricos"
    ) +
    scale_y_continuous(
        limits = c(0, 50),  # Limite de 0 a 100 para o eixo Y
        breaks = seq(0, 100, 10),  # Intervalos de 10 em 10
        labels = function(x) paste0(x, "\\%")  # Escapa o símbolo de porcentagem
    ) +
    scale_x_continuous(breaks = seq(min(df_share_top1_all$year), max(df_share_top1_all$year), by = 10)) +  # Intervalo de 25 anos no eixo X
    theme(
        plot.title = element_text(hjust = 0.5),  # Título centralizado
        axis.text.x = element_text(angle = 45, hjust = 1),  # Ajusta o ângulo do eixo X
        legend.title = element_blank(),  # Remove o título da legenda
        legend.position = "bottom"
    )
# printar
print(graph_df_share_top1)

# salvar: caminho de saída
output_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/writing/graph_share_top1.tex"

# abrir o tikz
tikz(output_path, width = 8, height = 6)
# gerar o gráfico para o TikZ
print(graph_df_share_top1)
# fechar o dispositivo gráfico
dev.off()


##### Gini #####
# Filtrar dados para o Brasil, França e EUA, e participação dos 1% mais ricos
df_gini_all <- wid_filtered %>%
    filter(
        country %in% c("BR", "FR", "US"),
        variable == "gptincj992",
        year >= 1820
    ) %>%
    select(country, year, value)

glimpse(df_gini_all)

# Plotemos
graph_gini <- ggplot(df_gini_all, aes(x = year, y = value, color = country, group = country)) +  # Não multiplicar por 100 agora
    geom_line(linewidth = 1) +  # Linha para os valores de Gini
    geom_point(size = 1.5, alpha = 0.6) +  # Adiciona pontos na linha para melhor visualização
    theme_minimal() +
    labs(
        title = "Índice de Gini (Brasil, França, EUA, 1900-presente)",
        y = "Índice de Gini",  # Não mais com '%'
        x = NULL  # Remove o título do eixo X
    ) +
    scale_y_continuous(
        limits = c(0, 1),  # Ajusta o eixo Y para 0 a 1
        breaks = seq(0, 1, 0.1),  # Intervalos de 0.1 para o eixo Y
        labels = scales::label_number()  # Formatação de número simples
    ) +
    scale_x_continuous(
        breaks = seq(min(df_gini_all$year), max(df_gini_all$year), by = 10)  # Intervalos de 10 anos no eixo X
    ) +
    theme(
        plot.title = element_text(hjust = 0.5),  # Título centralizado
        axis.text.x = element_text(angle = 45, hjust = 1),  # Ajusta o ângulo do eixo X
        legend.title = element_blank(),  # Remove o título da legenda
        legend.position = "bottom",
        axis.title.x = element_blank()  # Remove o título do eixo X
    )

# Exibe o gráfico
print(graph_gini)

# salvar: caminho de saída
output_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/writing/graph_gini.tex"

# abrir o tikz
tikz(output_path, width = 8, height = 6)
# gerar o gráfico para o TikZ
print(graph_gini)
# fechar o dispositivo gráfico
dev.off()



#### 2.2.5 Cálculo do GDP per Capita Ajustado ####

# Criar base temporária para cálculo do GDP per capita
wid_gdp_pop_calc <- wid_filtered %>%
    filter(variable %in% c(selected_gdp_population, conversion_variables)) %>%
    select(country, year, variable, value) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    
    # Limpeza inicial dos dados para evitar distorções
    filter(
        !is.na(agdproi992) & !is.na(npopuli992) & !is.na(xlcuspi999) & !is.na(xlcusxi999),
        xlcuspi999 > 0.01 & xlcusxi999 > 0.01,    # Evitar divisões por números muito pequenos
        agdproi992 > 1000 & npopuli992 > 1000     # Evitar valores irrealistas
    ) %>%
    
    # Calcular variáveis ajustadas
    mutate(
        gdppp = agdproi992 / npopuli992,          # GDP per capita bruto
        gdppp_usd_ppp = gdppp / xlcuspi999,      # Ajustado para PPP (USD PPP)
        gdppp_usd_market = gdppp / xlcusxi999,   # Ajustado para taxa de mercado (USD)
        ppp_vs_market = xlcuspi999 / xlcusxi999  # Razão PPP/Mercado
    ) %>%
    select(country, year, gdppp, gdppp_usd_ppp, gdppp_usd_market, ppp_vs_market)  # Selecionar apenas colunas calculadas

# Mesclar os cálculos de GDP de volta à base original
wid_gdp_pop <- wid_filtered %>%
    left_join(wid_gdp_pop_calc, by = c("country", "year"))

# Verificar a estrutura final para garantir que todas as variáveis foram mantidas
glimpse(wid_gdp_pop)


#### 2.2.6 Detecção e Correção de Outliers ####
# Detectar outliers usando o método IQR
iqr_value <- IQR(wid_gdp_pop$gdppp_usd_ppp, na.rm = TRUE)
q1 <- quantile(wid_gdp_pop$gdppp_usd_ppp, 0.25, na.rm = TRUE)
q3 <- quantile(wid_gdp_pop$gdppp_usd_ppp, 0.75, na.rm = TRUE)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value

# Ajustar os valores de GDP per capita fora dos limites
wid_gdp_pop_cleaned <- wid_gdp_pop %>%
    mutate(
        gdppp_usd_ppp_adj = ifelse(gdppp_usd_ppp < lower_bound | gdppp_usd_ppp > upper_bound, NA, gdppp_usd_ppp)
    ) %>%
    filter(!is.na(gdppp_usd_ppp_adj))  # Remover outliers detectados

#### 2.2.7 Visualizando a distribuição final ####

# Gráfico de distribuição em logarítmica

# Para caso o tikz esteja ativo
if (dev.cur() !=1) dev.off()
dev.new()
# Criar o histograma com ajustes
ggplot(wid_gdp_pop_cleaned, aes(x = gdppp_usd_ppp_adj)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    scale_x_log10(
        breaks = c(100, 1000, 10000, 100000, 1000000), 
        labels = scales::comma_format()
    ) +
    scale_y_continuous(labels = scales::comma) +  # Evita notação científica no eixo Y
    labs(
        title = "Distribuição do PIB per capita ajustado (log)",
        x = "PIB per capita ajustado (log)",
        y = "Frequência"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),  # Centraliza o título
        axis.text.x = element_text(angle = 0, hjust = 0.5),  # Mantém alinhamento do eixo X
        axis.text.y = element_text(size = 10)  # Ajusta tamanho do texto no eixo Y
    )

# Salvar diretamente no tikz
# Definir o caminho do arquivo de saída
output_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/visualizations/graph_gdp_ppp_histogram.tex"

# Abrir o dispositivo tikz
tikz(output_path, width = 8, height = 6)

# Criar o histograma com ajustes
ggplot(wid_gdp_pop_cleaned, aes(x = gdppp_usd_ppp_adj)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    scale_x_log10(
        breaks = c(100, 1000, 10000, 100000, 1000000), 
        labels = scales::comma_format()
    ) +
    scale_y_continuous(labels = scales::comma) +  # Evita notação científica no eixo Y
    labs(
        title = "Distribuição do PIB per capita ajustado (log)",
        x = "PIB per capita ajustado (log)",
        y = "Frequência"
    ) +
    theme_minimal(base_family = "lmodern") +  # Fonte LaTeX
    theme(
        plot.title = element_text(hjust = 0.5),  # Centraliza o título
        axis.text.x = element_text(angle = 0, hjust = 0.5),  # Mantém alinhamento do eixo X
        axis.text.y = element_text(size = 10)  # Ajusta tamanho do texto no eixo Y
    )
# Fechar o dispositivo gráfico para salvar o arquivo
dev.off()


# Verificar estatísticas descritivas
summary(wid_gdp_pop_cleaned$gdppp_usd_ppp_adj)
summary(wid_gdp_pop_cleaned$gdppp_usd_market)

# Salvar a base de dados limpa e ajustada
write_csv(wid_gdp_pop_cleaned, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/processed_wid_gdp_cleaned.csv")
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
# SITC ####
### ~ ####


# =========== # =========== # =========== # =========== # =========== # =========== #
## 3. CARREGAR DADOS ####
# =========== # =========== # =========== # =========== # =========== # =========== #

### 3.1 Importando SITC R-2 para 1962–2008 ####

# Para baixar, visite: <https://doi.org/10.7910/DVN/H8SFD2>.

# Caminho dos arquivos SITC
sitc_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/complexity/International-Trade-Data-SITC-Rev-2/"
sitc_files <- list.files(sitc_path, pattern = "sitc_country_country_product_year_4_.*\\.dta", full.names = TRUE)


### 3.2 Função para carregar, limpar e agregar os dados #####
process_sitc_file <- function(file) {
    message("Processando: ", file)
    
    read_dta(file) %>%
        select(country_id, year, export_value, import_value, eci, pci, coi) %>%  # Adicionado pipe aqui
        filter(year >= 1962 & year <= 2008) %>%
        group_by(country_id, year) %>%
        summarise(
            total_export = sum(export_value, na.rm = TRUE),
            total_import = sum(import_value, na.rm = TRUE),  # Adicionado
            avg_eci = mean(eci, na.rm = TRUE),
            avg_pci = mean(pci, na.rm = TRUE),  # Adicionado
            avg_coi = mean(coi, na.rm = TRUE),  # Adicionado
            .groups = "drop"
        )
}
# =========== # =========== # =========== # =========== # =========== # =========== #





# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 4 PROCESSANDO ARQUIVOS ####

# Processando em blocos para evitar estouro de memória 
batch_size <- 3  # Definir número de arquivos por bloco
num_batches <- ceiling(length(sitc_files) / batch_size)

# Lista para armazenar os resultados
sitc_results <- list()

# Loop para processar em blocos
tic("Processamento Completo SITC")
for (i in seq(1, length(sitc_files), by = batch_size)) {
    batch_files <- sitc_files[i:min(i + batch_size - 1, length(sitc_files))]
    message("Processando lote: ", i, " a ", min(i + batch_size - 1, length(sitc_files)))
    
    batch_data <- future_map_dfr(batch_files, process_sitc_file)
    sitc_results <- append(sitc_results, list(batch_data))
}
toc()
# =========== # =========== # =========== # =========== # =========== # =========== #




# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 5 UNINDO ####
sitc_final <- bind_rows(sitc_results)

# Checar estrutura reduzida
glimpse(sitc_final)


# Garantir que SITC tenha apenas uma linha por país-ano
sitc_aggregated <- sitc_final %>%
    group_by(country_id, year) %>%
    summarise(
        total_export = sum(total_export, na.rm = TRUE),
        avg_eci = mean(avg_eci, na.rm = TRUE),  # Média do índice de complexidade
        .groups = "drop"
    )

# Verificar estrutura após agregação
glimpse(sitc_aggregated)
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 6 FILTRANDO: POR E EXPORTAÇÕES e exportações #####

### 6.1 Exportações ######
# Calcular exportações totais por país
export_filtered <- sitc_aggregated %>%
    group_by(country_id) %>%
    summarise(total_export_sum = sum(total_export, na.rm = TRUE), .groups = "drop") %>%
    filter(total_export_sum > 1e9)  # Filtro de exportações > 1 bilhão de dólares
print(export_filtered)


### 6.2 População ######

# Tabela do WB com países e população: https://data.worldbank.org/indicator/SP.POP.TOTL>.

# Importar dados
population_data <- read_csv(
    "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/population-wb.csv",
    skip = 3,       # Ignorar as três primeiras linhas
    col_names = TRUE # Linha 4 será usada como cabeçalho
)
colnames(population_data) # Listar os nomes das colunas
head(population_data)      # Mostrar as primeiras linhas dos dados

# Remover coluna `...69`
population_data_clean <- population_data %>%
    select(-`...69`)

# Manter apenas Country Name, Country Code e os anos:
population_data_clean <- population_data_clean %>%
    select(`Country Name`, `Country Code`, starts_with("196"), starts_with("200"))

# Converter formato longo
population_data_long <- population_data_clean %>%
    pivot_longer(
        cols = matches("^(19|20)"),  # Seleciona colunas começando com '19' ou '20'
        names_to = "year",           # Nova coluna para anos
        values_to = "population"     # Nova coluna para valores populacionais
    ) %>%
    mutate(
        year = as.numeric(year),         # Converter o ano para numérico
        population = as.numeric(population) # Converter população para numérico
    )
glimpse(population_data_long)


# Criar uma lista com os códigos das regiões agregadas
region_codes <- c("AFE", "AFW", "CEB", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS", "FCS", "HIC", "HPC",
    "IBD", "IBT", "IDA", "IDB", "IDX", "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY",
    "LTE", "MEA", "MIC", "MNA", "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SSA", "SST",
    "TCA", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "VIR", "WLD")

# Filtrar os dados para manter apenas países individuais
population_filtered <- population_data_long %>%
    filter(year == 2008 & population >= 1.5e6) %>%
    filter(!`Country Code` %in% region_codes) %>%  # Excluir regiões agregadas
    select(`Country Code`, population)

# Verificar os primeiros países da lista filtrada
head(population_filtered)

# Listar os países restantes
unique(population_filtered$`Country Code`)

extra_regions_to_remove <- c("ARB", "EMU", "EUU", "SAS", "SSF")

# Filtrar os dados novamente para remover essas regiões agregadas restantes
population_filtered <- population_filtered %>%
    rename(country_id = `Country Code`)  # Ajusta o nome da coluna para corresponder ao SITC
# =========== # =========== # =========== # =========== # =========== # =========== #




# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 7 INTEGRAR À PIPELINE #####

### 7.1 Lidando com NAs #####
countrycode(c(200, 530, 582, 592, 720, 868, 890, 891, 999), "iso3n", "country.name")

# Converter códigos dos países
sitc_aggregated <- sitc_final %>%
    mutate(country_id = as.numeric(country_id)) %>%  # Garantir que é numérico
    filter(!country_id %in% c(200, 530, 582, 592, 720, 868, 890, 891, 999)) %>%  # Remover códigos inválidos
    mutate(country_id = countrycode(country_id, "iso3n", "iso3c"))  # Converter para ISO-3 alfabético

unique(sitc_aggregated$country_id)  # Deve mostrar "BRA", "USA", "FRA", etc.


### 7.2 `Left_join` com população #####
sitc_filtered <- sitc_aggregated %>%
    left_join(population_filtered, by = "country_id") %>%
    filter(!is.na(population))  # Remover países sem população válida
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### 8 TESTAR INTEGRAÇÃO #####
glimpse(sitc_filtered)  # Estrutura dos dados finais
nrow(sitc_filtered)  # Deve ser maior que 0
unique(sitc_filtered$country_id)  # Deve conter países esperados

check_vars <- c("country_id", "year", "export_value", "import_value", "eci", "pci", "coi")
validate_vars <- function(file) {
    cols <- colnames(read_dta(file))
    setdiff(check_vars, cols)  # Retorna variáveis faltantes
}
sapply(sitc_files, validate_vars)


unique_countries <- unique(sitc_filtered$country_id)
print(unique_countries)  # Certifique-se de que inclui os países esperados
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
### 9 SALVANDO #####
write_csv(sitc_filtered, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/sitc_selected.csv")

# Salvar como RDS
saveRDS(sitc_filtered, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/sitc_selected.rds")
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### 10 GRÁFICOS DE EXPLORAÇÃO #####

#### 10.1 Evolução dos 10 maiores exportadores #####
# Filtrar os países
top_countries <- sitc_filtered %>%
    group_by(country_id) %>%
    summarise(total_export_sum = sum(total_export, na.rm = TRUE)) %>%
    arrange(desc(total_export_sum)) %>%
    slice_head(n = 10) %>%
    pull(country_id)

# Para caso o tikz esteja ativo
if (dev.cur() != 1) dev.off()
dev.new()


# Gráfico para os 10 maiores exportadores
ggplot(sitc_filtered %>% filter(country_id %in% top_countries), 
    aes(x = year, y = total_export, color = country_id, group = country_id)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(
        labels = scales::label_number(),  # Exibe os valores em escala compacta (e.g., bilhões como 1B)
        breaks = seq(0, 1.5e12, by = 2.5e11) # Define os intervalos dos breaks
    ) +
    labs(
        title = "Evolução da exportação total (top 10 países)",
        x = "ano",
        y = "exportação Total",
        color = "país"
    ) +
    theme_minimal()



#### 10.2 Exportação por região #####
# Barras empilhadas: adicionar agrupamento por região
sitc_region <- sitc_filtered %>%
    mutate(region = countrycode(country_id, "iso3c", "region"))

# Resumir por região e ano
region_exports <- sitc_region %>%
    group_by(region, year) %>%
    summarise(total_export = sum(total_export, na.rm = TRUE), .groups = "drop")

# Gráfico de barras empilhadas
ggplot(region_exports, aes(x = year, y = total_export, fill = region)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
        labels = scales::label_number(),  # Formato de números compactos
        breaks = seq(0, 1.5e13, by = 2.5e12) # Define os intervalos dos breaks
    ) +
    labs(
        title = "exportação total por região",
        x = "ano",
        y = "exportação total",
        fill = "região"
    ) +
    theme_minimal()
# =========== # =========== # =========== # =========== # =========== # =========== #




# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
# MERGING ####
### ~ ####
# =========== # =========== # =========== # =========== # =========== # =========== #

sitc_filtered <- read_csv("/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/sitc_filtered.csv")

# =========== # =========== # =========== # =========== # =========== # =========== #
## 11 COMPATIBILIZAR WID COM WITC ####

# Verificar quais variáveis estão presentes no dataset
colnames(wid_gdp_pop_cleaned)
glimpse(wid_gdp_pop_cleaned)

### 12 Lista de códigos a excluir ####
region_codes <- c("CN-RU", "CN-UR", "OA-MER", "OB-MER", "OC-MER", "OD-MER", "OE-MER", "OH",
    "OI-MER", "OJ-MER", "QB-MER", "QD-MER", "QE-MER", "QF-MER", "QJ-MER", "QK-MER", "QL-MER",
    "QM-MER", "QN-MER", "QO-MER", "QP-MER", "QS-MER", "QT-MER", "QU-MER", "QV-MER", "QW-MER",
    "QX-MER", "QY-MER", "WO", "ZZ", "US-AK", "US-CA", "US-TX", "DE-BD", "DE-BY", "DE-HB", 
    "DE-HE", "DE-HH", "DE-PR", "DE-SN", "DE-WU", "US-AL",  "US-AR",  "US-AZ",  "US-CO", 
    "US-CT",  "US-DC",  "US-DE",  "US-FL",  "US-GA", "US-HI", "US-IA",  "US-ID",  "US-IL",
    "US-IN",  "US-KS",  "US-KY",  "US-LA",  "US-MA", "US-MD",  "US-ME",  "US-MI",  "US-MN",
    "US-MO",  "US-MS",  "US-MT",  "US-NC",  "US-ND", "US-NE",  "US-NH",  "US-NJ",  "US-NM",
    "US-NV",  "US-NY",  "US-OH", "US-OK",  "US-OR", "US-PA",  "US-RI",  "US-SC",  "US-SD",
    "US-TN",  "US-UT",  "US-VA",  "US-VT",  "US-WA", "US-WI", "US-WV",  "US-WY", "WO-MER",
    "XA-MER", "XB-MER", "XF-MER", "XL-MER", "XM-MER", "XN-MER", "XR-MER", "XS-MER",
    "DD", "KS", "OA", "OB", "OC", "OD", "OE", "OI", "OJ", "QB", "QD", "QE", "QF", "QJ", "QK",
    "QL", "QM", "QN", "QO", "QP", "QS", "QT", "QU", "QV", "QW", "QX", "QY", "XA", "XB", "XF",
    "XL", "XM", "XN", "XR", "XS"
)


# Remover as regiões do conjunto de dados
wid_countries <- wid_gdp_pop_cleaned %>%
    filter(!country %in% region_codes)  # Excluir as regiões

# Selecionar apenas os dados para o período relevante (1962-2008)
wid_countries <- wid_countries %>%
    filter(year >= 1962 & year <= 2008)

# Verificar a conversão e estrutura dos dados
glimpse(wid_countries)


#### 12.2 Converter códigos de país de 2 para 3 dígitos (ISO3) #####
wid_countries <- wid_countries %>%
    mutate(country_id = countrycode(country, "iso2c", "iso3c"))

# Verifique se as variáveis 'country_id' foram convertidas corretamente
unique(wid_countries$country_id)


#### 12.3 Manter apenas os países no sitc_filtered ####

wid_filtered <- wid_countries %>%
    filter(country_id %in% unique(sitc_filtered$country_id))

# Verificar o resultado
glimpse(wid_filtered)
unique(wid_filtered$country_id)


## 13 Merging ####

##### 13.1 Estrutura das duas bases ####
glimpse(wid_filtered)
glimpse(sitc_filtered)


##### 13.2 Realizando o merge (left_join ou inner_join) ####
combined_data <- sitc_filtered %>%
    left_join(wid_filtered, by = c("country_id", "year"))

# Verificar o resultado da combinação
glimpse(combined_data)

##### 13.3 Ajustes: remover e add colunas ####

# Remover a coluna 'country'
combined_data <- combined_data %>%
    select(-country)

# Renomear as colunas
combined_data <- combined_data %>%
    rename(
        wid_variable = variable,
        wid_value = value,
        wid_pop = pop
    )

# adicionar gdp
combined_data <- combined_data %>%
    mutate(
        gdp_ppp = gdppp_usd_ppp,         # GDP ajustado por PPP
        gdp_market = gdppp_usd_market    # GDP ajustado pela taxa de mercado
    ) %>%
    select(-gdppp_usd_ppp, -gdppp_usd_market)  # Remover colunas antigas, se necessário

# Criar a variável "decade"
combined_data <- combined_data %>%
    mutate(decade = case_when(
        year >= 1960 & year < 1970 ~ "1960",
        year >= 1970 & year < 1980 ~ "1970",
        year >= 1980 & year < 1990 ~ "1980",
        year >= 1990 & year < 2000 ~ "1990",
        year >= 2000 & year < 2010 ~ "2000",
        TRUE ~ NA_character_
    ))

# Checar as mudanças
glimpse(combined_data)
str(combined_data)
colnames(combined_data)


#### 13.4 Estatísticas descritivas ####
summary_stats <- summary(combined_data)  # Para obter uma visão geral de todas as variáveis
print(summary_stats)

summary_stats_nona <- summary(combined_data, na.rm = TRUE)
print(summary_stats_nona)

# Salvar as estatísticas descritivas em um arquivo CSV
write.csv(summary_stats, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/summary_stats", row.names = TRUE)

#### 13.5 Salvar a base combinada ####
write_csv(combined_data, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/combined_sitc_wid.csv")
saveRDS(combined_data, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/combined_sitc_wid.rds")
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
## 14 EXPLORAÇÃO: N DE OBSERVAÇÕES E NAs ####

# Vamos explorar a base para conhecer fatores como NAs, número de observações e diferenças na presença de dados por décadas


### 14.1 Carregando a base ####
combined_data <- readRDS("/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/combined_sitc_wid.rds")


### 14.2. Lidando com NAs ##### 
# Contagem de NAs por coluna
colSums(is.na(combined_data))

# Proporção de NAs por coluna
colMeans(is.na(combined_data)) * 100  # Em porcentagem

# Quantificar os NAs e documentar
na_counts <- colSums(is.na(combined_data))
na_percent <- colMeans(is.na(combined_data)) * 100
na_report <- data.frame(Variable = names(na_counts), NA_Count = na_counts, NA_Percentage = na_percent)

# Exportar para um CSV para documentação
write.csv(na_report, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/writing/NA_Report.csv", row.names = FALSE)

# Exibir na tela
print(na_report)
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### 14.3 Análise bivariada ####

#### 14.3.1 Definir variáveis relevantes ####
# Diretório para salvar os resultados

output_dir <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/analyses"
dir.create(output_dir, showWarnings = FALSE)


# Décadas e variáveis de desigualdade
decades <- c("1960", "1970", "1980", "1990", "2000")
share_variables <- c("sptincj992", "sdiincj992", "sptinci992", "sdiinci992")
gini_variables <- c("gptincj992", "gptinci992", "gfiincj992", "gdiincj992", "gfiinci992", "gdiinci992")
palma_variables <- c("rptincj992", "rptinci992")

# Listas para armazenar resultados
regression_results_na_removed <- list()  # Com remoção de NAs
regression_results_na_kept <- list()  # Mantendo NAs


#### 14.3.2 Função para análise bivariada com e sem NAs ####

# Serão gerados dois plots para cada análise - um com e outro sem NAs
analyze_bivariate <- function(data, variables, category_name, store_list_name, remove_na) {
    for (decade in decades) {
        print(paste("Processando", category_name, "para a década:", decade))
        
        # Filtrar os dados para a década correta
        decade_data <- data %>%
            filter(as.character(decade) == as.character(!!decade))
        
        if (nrow(decade_data) == 0) {
            print(paste("Nenhum dado para a década", decade, "em", category_name))
            next  # Pula para a próxima década
        }
        
        for (variable in variables) {
            # Filtrar os dados para a variável específica
            var_data <- decade_data %>%
                filter(wid_variable == variable)
            
            if (remove_na) {
                var_data <- var_data %>% filter(!is.na(avg_eci) & !is.na(wid_value))  # Remover NAs essenciais
            }
            
            if (nrow(var_data) > 0) {
                print(paste("Regressão para", category_name, "(", variable, ") na década:", decade, "com", nrow(var_data), "observações"))
                
                # Criar pasta separada para os gráficos (com ou sem NAs)
                graph_dir <- file.path(output_dir, ifelse(remove_na, "graphs_na_removed", "graphs_na_kept"))
                dir.create(graph_dir, showWarnings = FALSE)
                
                # Criar gráfico
                plot <- ggplot(var_data, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
                    geom_point(alpha = 0.6) +
                    geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
                    labs(
                        title = paste("ECI vs", category_name, "(", variable, ") - Década:", decade),
                        x = "Economic Complexity Index (avg_eci)",
                        y = category_name,
                        color = "Percentil"
                    ) +
                    theme_minimal()
                
                # Salvar gráfico
                ggsave(
                    filename = file.path(graph_dir, paste0(category_name, "_scatter_", decade, "_", variable, ".png")),
                    plot = plot,
                    width = 8, height = 6
                )
                
                # Regressão bivariada
                lm_model <- lm(wid_value ~ avg_eci, data = var_data)
                
                # **Armazenar resultados corretamente**
                if (store_list_name == "regression_results_na_removed") {
                    regression_results_na_removed[[paste(category_name, decade, variable, sep = "_")]] <<- summary(lm_model)
                } else if (store_list_name == "regression_results_na_kept") {
                    regression_results_na_kept[[paste(category_name, decade, variable, sep = "_")]] <<- summary(lm_model)
                }
                
            } else {
                print(paste(" Nenhum dado para", category_name, "(", variable, ") na década:", decade))
            }
        }
    }
}

#### 14.3.3 Executar a análise para cada índice de desigualdade ####

# Com remoção de NAs
analyze_bivariate(combined_data, share_variables, "Share de renda", "regression_results_na_removed", remove_na = TRUE)
analyze_bivariate(combined_data, gini_variables, "Gini", "regression_results_na_removed", remove_na = TRUE)
analyze_bivariate(combined_data, palma_variables, "Razão de Palma", "regression_results_na_removed", remove_na = TRUE)

# Mantendo NAs
analyze_bivariate(combined_data, share_variables, "Share de renda", "regression_results_na_kept", remove_na = FALSE)
analyze_bivariate(combined_data, gini_variables, "Gini", "regression_results_na_kept", remove_na = FALSE)
analyze_bivariate(combined_data, palma_variables, "Razão de Palma", "regression_results_na_kept", remove_na = FALSE)


### Salvar os resultados das regressões
# Com remoção de NAs
results_file_na_removed <- file.path(output_dir, "bivariate_regression_na_removed.txt")
sink(results_file_na_removed)
if (length(regression_results_na_removed) > 0) {
    for (key in names(regression_results_na_removed)) {
        cat("Resultado para:", key, "\n")
        print(regression_results_na_removed[[key]])
        cat("\n----------------------\n")
    }
} else {
    cat("Nenhuma regressão foi realizada após remoção de NAs.\n")
}
sink()

# Mantendo NAs
results_file_na_kept <- file.path(output_dir, "bivariate_regression_na_kept.txt")
sink(results_file_na_kept)
if (length(regression_results_na_kept) > 0) {
    for (key in names(regression_results_na_kept)) {
        cat("Resultado para:", key, "\n")
        print(regression_results_na_kept[[key]])
        cat("\n----------------------\n")
    }
} else {
    cat("Nenhuma regressão realizada mantendo NAs.\n")
}
sink()

print("Análises concluídas")
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### 15. CONTANDO OBSERVAÇÕES ####

# Criar diretório de saída
output_dir <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

#### 15.1 Criar coluna de década ####

# Adicionar uma nova coluna de década baseada na coluna `year`
wid_full <- wid_full %>%
    mutate(decade = floor(year / 10) * 10)  # Transforma o ano na década correspondente (ex: 1987 → 1980)

#### 15.2 Contagem de observações por unidade e década ####

# Filtrar e contar observações por década para cada conjunto de variáveis
count_by_decade <- function(variables, category_name) {
    wid_full %>%
        filter(variable %in% variables) %>%
        count(decade, variable) %>%
        mutate(category = category_name)
}

# Contar observações para cada conjunto de variáveis
share_counts <- count_by_decade(c("sptincj992", "sdiincj992", "sptinci992", "sdiinci992"), "Share de Renda")
gini_counts <- count_by_decade(c("gptincj992", "gptinci992", "gfiincj992", "gdiincj992", "gfiinci992", "gdiinci992"), "Gini")
palma_counts <- count_by_decade(c("rptincj992", "rptinci992"), "Razão de Palma")

# Unir todos os conjuntos de dados
all_counts <- bind_rows(share_counts, gini_counts, palma_counts)

# diretório de saída
output_dir_vis <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/visualizations"
dir.create(output_dir_vis, showWarnings = FALSE, recursive = TRUE)


ggplot(all_counts, aes(x = factor(decade), y = n, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ category, scales = "free_y") +
    labs(title = "Distribuição de observações por década e variável",
        x = "Década",
        y = "Número de observações",
        fill = "Variável") +
    theme_minimal(base_family = "lmodern") +  # Fonte LaTeX
    theme(
        plot.title = element_text(hjust = 0.5),  # Centraliza o título
        axis.text.x = element_text(angle = 45, hjust = 1),  # Inclina os rótulos do eixo X
        axis.text.y = element_text(size = 10)  # Ajusta tamanho do texto do eixo Y
    ) +
    scale_y_continuous(labels = scales::comma)

# Configurar `tikzDevice`
options(tikzDefaultEngine = "xetex")  # Para evitar problemas com Unicode

# Abrir o TikZ para exportar o gráfico
tikz(output_path, width = 8, height = 6)

# Criar gráfico de barras empilhadas para cada categoria
ggplot(all_counts, aes(x = factor(decade), y = n, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ category, scales = "free_y") +
    labs(title = "Distribuição de observações por década e variável",
        x = "Década",
        y = "Número de observações",
        fill = "Variável") +
    theme_minimal(base_family = "lmodern") +
    theme(
        plot.title = element_text(hjust = 0.5),  # Centraliza o título
        axis.text.x = element_text(angle = 45, hjust = 1),  # Inclina os rótulos do eixo X
        axis.text.y = element_text(size = 10)  # Ajusta tamanho do texto do eixo Y
    ) +
    scale_y_continuous(labels = scales::comma)

# Fechar o dispositivo gráfico para salvar
dev.off()

# Veja como o número de observações para as décadas de 1960 e 1970 é muito baixo para todas as variáveis.
# O número de obs para todas as variáveis cuja unidade é "i" é muito baixo se comparados às variáveis "j".



### 16. COMPARAÇÃO DE RESULTADOS COM E SEM NAs ####

# Listas para armazenar os coeficientes
comparison_results <- list()

# Extrair coeficientes de um modelo
extract_coefficients <- function(model_summary) {
    if (is.null(model_summary)) return(NA)
    coefs <- coef(model_summary)
    return(data.frame(
        Term = rownames(coefs),
        Estimate = coefs[, 1],
        StdError = coefs[, 2],
        tValue = coefs[, 3],
        pValue = coefs[, 4]
    ))
}

# Criar df de comparação
for (key in names(regression_results_na_removed)) {
    if (key %in% names(regression_results_na_kept)) {
        # Extrair coeficientes de ambos os modelos
        model_removed <- extract_coefficients(regression_results_na_removed[[key]])
        model_kept <- extract_coefficients(regression_results_na_kept[[key]])
        
        if (!is.null(model_removed) & !is.null(model_kept)) {
            model_removed$Type <- "NAs Removidos"
            model_kept$Type <- "NAs Mantidos"
            
            # Combinar em um dataframe
            model_comparison <- bind_rows(model_removed, model_kept) %>%
                mutate(Model = key)
            
            # Armazenar na lista
            comparison_results[[key]] <- model_comparison
        }
    }
}

# Transformar lista em um df
comparison_df <- bind_rows(comparison_results, .id = "Model")

# Exibir os primeiros resultados
print(comparison_df)

# Salvar para análise
write_csv(comparison_df, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/comparison_df.csv")

print("Comparação de regressões com e sem NAs concluída")



# Ajustar os nomes das colunas para facilitar a leitura
colnames(comparison_df) <- c("Term", "Estimate", "StdError", "tValue", "pValue", "Type", "Model")

# Filtrar para manter apenas os coeficientes de avg_eci
comparison_df <- comparison_df %>% filter(Term == "avg_eci")

colnames(comparison_df)

# Resumo estatístico das diferenças entre os modelos
summary_table <- comparison_df %>%
    group_by(Type) %>%
    summarise(
        Mean_Estimate = mean(Estimate, na.rm = TRUE),
        SD_Estimate = sd(Estimate, na.rm = TRUE),
        Mean_pValue = mean(pValue, na.rm = TRUE),
        SD_pValue = sd(pValue, na.rm = TRUE),
        n = n()
    )

print(summary_table)


# Graficando
options(tikzDefaultEngine = "xetex")

# Definir o caminho e nome do arquivo
output_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/visualizations/graph_coefficients_na_nnas.tex"
# Abrir tikz
tikz(output_path, width = 8, height = 6)
# Gráficos comparando os coeficientes estimados
ggplot(comparison_df, aes(x = Type, y = Estimate, fill = Type)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    labs(
        title = "Comparação dos ceficientes de avg\\_eci (NAs removidos vs mantidos)",
        y = "Coeficiente estimado",
        x = "Tipo de análise"
    )
# Fechar o dispositivo para salvar
dev.off()


# Graficando 2
options(tikzDefaultEngine = "xetex")

# Definir o caminho e nome do arquivo
output_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/visualizations/graph_p-values_na_nnas.tex"
# Abrir TikZ
tikz(output_path, width = 8, height = 6)
# Criar gráficos comparando os p-values
ggplot(comparison_df, aes(x = Type, y = pValue, fill = Type)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +  # Nível de significância de 5%
    geom_hline(yintercept = 0.10, linetype = "dashed", color = "green") +  # Nível de significância de 10%
    labs(
        title = "Comparação dos p-values (NAs removidos vs mantidos)",
        y = "p-value",
        x = "Tipo de análise"
    )
# Fechar o dispositivo para salvar
dev.off()
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####

## 17. ÚLTIMA SELEÇÃO ####

### 17.1 Selcionar unidade j e tirar NAs ####

# Variáveis de interesse
selected_variables <- c("sptincj992", "sdiincj992", 
    "gptincj992", "gptinci992", "gfiincj992", "gdiincj992", 
    "rptincj992")

# Filtrar o dataframe
combined_data <- combined_data %>%
    filter(wid_variable %in% selected_variables) %>%
    drop_na(wid_value)  # Remove linhas com NA em wid_value

# Exibir estrutura do dataframe filtrado
glimpse(combined_data)


### 17.2 Salvar ####
write.csv(combined_data, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/combined_sitc_wid.rds", row.names = FALSE)

saveRDS(combined_data, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/combined_sitc_wid.rds")
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
## 18. EXPLORAÇÃO: SURVEYS À RENDA NACIONAL  ####

# Vamos construir a série da renda total divindo por seus tipos (survey, fiscal, admin wages)

# Decomposição da renda primária nos setores:
# `prigo`: setor governamental (net primary income of general government)
# `prico`: setor corporativo (net primary income of corporations)
# `prihn`: setor doméstico (households and NPISH)

### 18.1 Carregar e filtrar dados ####

#### 18.1.1 Brasil #####

# Filtrar os dados para o Brasil
df_br <- wid_full %>%
    filter(country == "BR", 
        year >= 2000, year <= 2020,
        variable %in% c("aprigoi992", "apricoi992", "aprihni992", "asschni992")) %>%
    select(country, year, variable, value)

glimpse(df_br)

# Somar os componentes da renda nacional
total_income_br <- df_br %>%
    filter(variable %in% c("aprigoi992", "apricoi992", "aprihni992", "asschni992")) %>%
    group_by(year) %>%
    summarise(total = sum(value, na.rm = TRUE))

# Normalizar apenas as variáveis de renda nacional
df_br <- df_br %>%
    left_join(total_income_br, by = "year") %>%
    mutate(value = ifelse(variable %in% c("aprigoi992", "apricoi992", "aprihni992", "asschni992"),
        (value / total) * 100,  # Transforma em % do total da renda nacional
        value))  # Mantém os valores de survey income originais

# Série da renda disponível capturada por survey (estimada)
survey_income_estimated_br <- data.frame(
    year = seq(2000, 2020, 3),
    value = c(50, 48, 45, 44.5, 44, 43.5, 43),
    variable = "surveyIncome"
)


#### 18.1.2 México #####
df_mx <- wid_full %>%
    filter(country == "MX", 
        year >= 2000, year <= 2020,
        variable %in% c("aprigoi992", "apricoi992", "aprihni992", "asschni992")) %>%
    select(country, year, variable, value)

# Somar os componentes da renda nacional
total_income_mx <- df_mx %>%
    filter(variable %in% c("aprigoi992", "apricoi992", "aprihni992", "asschni992")) %>%
    group_by(year) %>%
    summarise(total = sum(value, na.rm = TRUE))

# Normalizar apenas as variáveis de renda nacional
df_mx <- df_mx %>%
    left_join(total_income_mx, by = "year") %>%
    mutate(value = ifelse(variable %in% c("aprigoi992", "apricoi992", "aprihni992", "asschni992"),
        (value / total) * 100,  # Transforma em % do total
        value))

# Série da renda disponível capturada por survey (estimada)
survey_income_estimated_mx <- data.frame(
    year = seq(2000, 2018, 3),  # Agora são 7 anos
    value = c(29, 31.5, 30, 31.2, 27, 26.9, 27),  # 7 valores
    variable = "surveyIncome"
)


### 18.2 Gerar gráfico ####

# Ajustar a paleta de cores
color_palette <- c(
    "aprigoi992" = "goldenrod",       # Governo geral
    "apricoi992" = "brown",           # Corporações
    "aprihni992" = "steelblue",       # Setor doméstico
    "surveyIncome" = "black",        # Renda de surveys
    "asschni992" = "darkgray"         # Social contributions paid by households
)


#### 18.2.1 Brasil ####

# Gerar o gráfico
grafico_br <- ggplot(data = df_mx, aes(x = year, y = value, fill = variable)) +
    geom_area(position = "stack", alpha = 0.7) +  # Empilhar áreas
    
    geom_line(data = survey_income_estimated_br, aes(x = year, y = value, color = "surveyIncome", group = 1), 
        size = 1, linetype = "solid") +  # Linha para survey income
    
    geom_point(data = survey_income_estimated_br, aes(x = year, y = value, color = "surveyIncome"), 
        size = 1) +  # Pontos para survey income
    
    scale_fill_manual(values = color_palette) +  # Definir a paleta de cores para as áreas
    scale_color_manual(values = c("surveyIncome" = "black")) +  # Definir a cor para a linha e os pontos
    scale_y_continuous(breaks = seq(0, 100, 20)) +  # Ajustar o eixo Y corretamente
    theme_minimal(base_family = "lmodern") +
    labs(
        title = "Brasil: de surveys à renda nacional",
        x = "", 
        y = "% da renda nacional bruta"
    ) +
    theme(
        legend.position = "bottom",  # Coloca a legenda na parte inferior
        plot.title = element_text(hjust = 0.5),  # Título centralizado
        legend.title = element_blank(),  # Remove o título da legenda
        axis.title.x = element_blank()  # Remove o título do eixo X
    ) +
    guides(
        fill = guide_legend(title = "Componente da Renda"),  # Guia da legenda para as áreas
        color = guide_legend(title = "Fonte de Renda")  # Guia da legenda para as linhas
    ) +
    scale_color_manual(
        values = c(
            "aprigoi992" = "goldenrod", 
            "apricoi992" = "brown", 
            "aprihni992" = "steelblue", 
            "surveyIncome" = "black", 
            "asschni992" = "darkgray"
        ),
        breaks = c("aprigoi992", "apricoi992", "aprihni992", "asschni992", "surveyIncome"),
        labels = c(
            "aprigoi992" = "governo geral",
            "apricoi992" = "corporações",
            "aprihni992" = "setor domiciliar",
            "asschni992" = "contribuições para a seguridade",
            "surveyIncome" = "renda de surveys"
        )
    ) + 
    scale_fill_manual(
        values = c(
            "aprigoi992" = "goldenrod", 
            "apricoi992" = "brown", 
            "aprihni992" = "steelblue", 
            "asschni992" = "darkgray"
        ),
        breaks = c("aprigoi992", "apricoi992", "aprihni992", "asschni992"),
        labels = c(
            "aprigoi992" = "governo geral",
            "apricoi992" = "corporações",
            "aprihni992" = "setor domiciliar",
            "asschni992" = "contribuições para a seguridade"
        )
    )

# Exibe o gráfico
br <- print(grafico_br)
ggsave("/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/writing/br_surveysrenda.pdf", plot = br, width = 8, height = 6)






options(tikzDefaultEngine = "xetex")

output_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/visualizations/grafico_br.tex"


# Criar e salvar o gráfico
grafico_br <- ggplot(data = df_br, aes(x = year, y = value, fill = variable)) +
    geom_area(position = "stack", alpha = 0.7) +  # Empilha corretamente as áreas
    
    geom_line(data = survey_income_estimated_br, aes(x = year, y = value, color = variable, group = 1), 
        size = 1, linetype = "solid") +  # Linha para survey income
    
    geom_point(data = survey_income_estimated_br, aes(x = year, y = value, color = variable), 
        size = 1) +  # Pontos para survey income
    
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = color_palette, 
        breaks = names(color_palette),
        labels = c(
            "aprigoi992" = "General government",
            "apricoi992" = "Corporations",
            "aprihni992" = "Household sector",
            "asschni992" = "Social contributions",
            "surveyIncome" = "Survey income"
        )) +
    scale_y_continuous(breaks = seq(0, 100, 20)) +  # Ajustar o eixo Y corretamente
    theme_minimal() +
    labs(title = "Brasil: de surveys à renda nacional",
        x = "", 
        y = "% da renda nacional bruta") +
    theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
    )
print(grafico_br)


# Fechar o dispositivo gráfico
dev.off()


#### 18.2.2 México ####


grafico_mx <- ggplot(data = df_mx, aes(x = year, y = value, fill = variable)) +
    geom_area(position = "stack", alpha = 0.7) +  # Empilha corretamente as áreas
    
    geom_line(data = survey_income_estimated_mx, aes(x = year, y = value, color = variable, group = 1), 
        size = 1, linetype = "solid") +  # Linha para survey income
    
    geom_point(data = survey_income_estimated_mx, aes(x = year, y = value, color = variable), 
        size = 1) +  # Pontos para survey income
    
    scale_fill_manual(values = color_palette) +
scale_color_manual(values = color_palette, 
    breaks = c("apricoi992", "aprigoi992", "aprihni992", "asschni992", "surveyIncome"),
    labels = c(
        "Corporações",
        "Governo geral",
        "Setor domiciliar",
        "Contribuições para a seguridade",
        "Renda de surveys"
    )
)
 +
    scale_y_continuous(breaks = seq(0, 100, 20)) +  # Ajustar o eixo Y corretamente
    theme_minimal(base_family = "lmodern") +  # Fonte LaTeX
    labs(title = "México: de surveys à renda nacional",
        x = "", y = "% da renda nacional bruta") +
    theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
    )

print(grafico_mx)
unique(df_mx$variable)

str(df_mx$variable)
str(survey_income_estimated_mx$variable)






options(tikzDefaultEngine = "xetex")

output_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/visualizations/grafico_mx.tex"

# Abrir TikZ
tikz(output_path, width = 8, height = 6)

# Criar e salvar o gráfico
survey_income_estimated_mx$variable <- factor(survey_income_estimated_mx$variable, 
    levels = c("apricoi992", "aprigoi992", "aprihni992", "asschni992", "surveyIncome"),
    labels = c("Corporações", "Governo geral", "Setor domiciliar", "Contribuições para a seguridade", "Renda de surveys"))


color_palette <- c(
    "Corporações" = "#1f78b4",
    "Governo geral" = "#33a02c",
    "Setor domiciliar" = "#e31a1c",
    "Contribuições para a seguridade" = "#ff7f00",
    "Renda de surveys" = "#6a3d9a"
)

df_mx$variable <- factor(df_mx$variable, levels = names(color_palette))
survey_income_estimated_mx$variable <- factor(survey_income_estimated_mx$variable, levels = names(color_palette))



grafico_mx <- ggplot(data = df_mx, aes(x = year, y = value, fill = variable)) +
    geom_area(position = "stack", alpha = 0.7) +  
    
    geom_line(data = survey_income_estimated_mx, 
        aes(x = year, y = value, color = variable), 
        size = 1, linetype = "solid") +
    
    geom_point(data = survey_income_estimated_mx, 
        aes(x = year, y = value, color = variable), 
        size = 1) +
    
    scale_fill_manual(values = color_palette, labels = levels(df_mx$variable)) +
    scale_color_manual(values = color_palette, labels = levels(df_mx$variable)) +
    
    scale_y_continuous(breaks = seq(0, 100, 20)) +
    theme_minimal(base_family = "lmodern") +
    labs(title = "México: de surveys à renda nacional",
        x = "", y = "% da renda nacional bruta") +
    theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
    )

print(grafico_mx)




grafico_mx <- ggplot(data = df_mx, aes(x = year, y = value, fill = variable)) +
    geom_area(position = "stack", alpha = 0.7) +  # Empilha corretamente as áreas +
    geom_line(data = survey_income_estimated_mx, aes(x = year, y = value, color = variable, group = 1), 
        size = 1, linetype = "solid") +  # Linha para survey income
    
    geom_point(data = survey_income_estimated_mx, aes(x = year, y = value, color = variable), 
        size = 1) +  # Pontos para survey income
    
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = color_palette, 
        breaks = names(color_palette),
        survey_income_estimated_mx$variable <- factor(survey_income_estimated_mx$variable, 
            levels = c("apricoi992", "aprigoi992", "aprihni992", "asschni992", "surveyIncome"),
            labels = c("Corporações", "Governo geral", "Setor domiciliar", "Contribuições para a seguridade", "Renda de surveys")) +
    scale_y_continuous(breaks = seq(0, 100, 20)) +  # Ajustar o eixo Y corretamente
    theme_minimal() +
    labs(title = "México: de surveys à renda nacional",
        x = "", y = "\\% da renda nacional bruta") +
    theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
    )

# Fechar o dispositivo gráfico
dev.off()

# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
## FONTES #####

# WID
## Baixar bd: <https://wid.world/data/>.
## Codes Dictionary: <https://wid.world/codes-dictionary/>.
## Para detalhes da construção da DINA, cf. baixe o manual <https://wid.world/document/distributional-national-accounts-guidelines-2020-concepts-and-methods-used-in-the-world-inequality-database/>.

# WB
# Baixar bd: <https://data.worldbank.org/indicator/SP.POP.TOTL>.

# SITC, Rev. 2
## Baixar bd: https://doi.org/10.7910/DVN/H8SFD2
## Para detalhes da construção do Atlas Data: <https://atlas.hks.harvard.edu/about-data>.
## "The Atlas of Economic Complexity," Center for International Development at Harvard University, <http://www.atlas.hks.harvard.edu/>.
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
# FIM #####
# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #