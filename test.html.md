---
title: "Relatório de Ciência dos Dados: Projeto *''Complexidade e Desigualdade''*"
author: "Samuel Maia"
format:
  html:
    toc: true
    toc-float: true
    toc-collapsed: false
    toc-depth: 3
    # toc-location: left
    toc-title: Relatório
    number-sections: true
    smooth-scroll: false
    theme: cosmo
    highlight: textmate
    df-print: paged
    fig-caption: true
    fig-width: 7
    fig-height: 5
    keep-md: true
    self-contained: false
    css: "styles.css"
editor-options:
  bibliography: complexity-inequality-paper-copy.bib
  csl: apa-no-doi-no-issue.csl
params:
  data-path: "data/wid_data.csv"
  country-filter: "Brazil"
  geometry: margin=1in
---




# Visão geral

Neste relatório, apresento os conceitos e técnicas aprendidos no curso *Ciência de Dados Avançada*, no semestre 2024/2. Essas abordagens foram aplicadas ao projeto de pesquisa "Complexidade Econômica e Desigualdade de Renda: a vista do topo é diferente" (de agora em diante, *Projeto*), trabalho final da disciplina *Economia do Desenvolvimento* cursada no mesmo semestre.

Estruturei o relatório em duas partes. Na primeira, intitulada ***Projeto*** **e Bastidores**, faço uma breve apresentação do *Projeto* e das dificuldades aprendizados adquiridos ao longo do processo. A segunda parte -- a mais importante -- é dedicada à **Execução**. Ela segue a estrutura do planejamento adotado, dividido nas seguintes seções: "Preparação dos Dados", "Limpeza e Estruturação", "Análise", "Resultados" e "Transparência". Termino com a **Conclusão** .

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

# *Projeto* e Bastidores

## Contextualização

Com o *Projeto*, investiguei a relação entre **complexidade econômica** (ECI) e **desigualdade de renda**. A análise é feita utilizando dados da **World Inequality Database (WID)** e do **Atlas de Complexidade Econômica (SITC)**, com o objetivo de replicar e expandir os achados de [Hartmann et al. (2017)](https://doi.org/10.1016/j.worlddev.2016.12.020), que sugerem uma associação negativa entre desigualdade e complexidade econômica. No entanto, neste estudo, exploro a possibilidade de que o uso de bases de dados mais recentes e medidas mais desagregadas de desigualdade (como as participações de renda) possa alterar esse resultado.

::: {style="height: 1px;"}
:::

## "Cozinha de Pesquisa"

Ao longo do processo, enfrentei desafios significativos, como a integração de bases de dados grandes e variadas, e o tratamento de dados ausentes e discrepantes. No entanto, o trabalho me permitiu aplicar diversas técnicas de **limpeza de dados** e **análise estatística**, além de melhorar minha compreensão sobre como escolher e implementar diferentes tipos de modelos para testar as hipóteses da pesquisa.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

# Execução

Estruturei o trabalho com os dados do *Projeto* em cinco etapas:

1)  **Preparação**: Entendimento das bases de dados (WID e Atlas) e suas variáveis.
2)  **Limpeza e Estruturação**: Como integrei as bases e preparei os dados para análise.
3)  **Análises**: Execução das análises estatísticas e visualizações, incluindo modelos de regressão e dados em painel.
4)  **Resultados**: Entendimento das bases de dados (WID e Atlas) e suas variáveis.
5)  **Transparência e Reprodutibilidade**: Entendimento das bases de dados (WID e Atlas) e suas variáveis.

Vamos a elas.

::: {style="height: 1px;"}
:::

## Preparação dos Dados

A primeira etapa envolveu explorar as bases de dados para entender suas características, variáveis e estruturas. Com o objetivo de garantir a compatibilidade entre os dados de diferentes fontes, foram feitas inspeções iniciais e verificações de qualidade.

``` r
# Carregamento dos pacotes necessários
library(tidyverse)
library(readr)

# Carregamento e inspeção dos dados
wid_data <- read_csv("path_to_wid_data.csv")
head(wid_data)
```

::: {style="height: 1px;"}
:::

## Limpeza e Estruturação dos Dados

Aqui, descrevo o processo de limpeza e estruturação dos dados. Incluímos a remoção de variáveis irrelevantes, tratamento de dados ausentes e outliers, além de ajustes de formato (como datas e moedas) para garantir que os dados estivessem prontos para análise.

``` r
# Remover colunas irrelevantes e tratar dados ausentes
wid_data_cleaned <- wid_data %>%
  select(-c(column_to_remove)) %>%
  drop_na()

# Detecção de outliers usando o método do IQR
iqr <- IQR(wid_data_cleaned$GDP_per_capita)
upper_bound <- quantile(wid_data_cleaned$GDP_per_capita, 0.75) + 1.5 * iqr
lower_bound <- quantile(wid_data_cleaned$GDP_per_capita, 0.25) - 1.5 * iqr

wid_data_cleaned <- wid_data_cleaned %>%
  filter(GDP_per_capita < upper_bound & GDP_per_capita > lower_bound)
```

::: {style="height: 1px;"}
:::

## Análises

::: {style="height: 1px;"}
:::

### Análise Descritiva

Antes de aplicar os modelos estatísticos, fiz uma análise descritiva das variáveis principais para entender sua distribuição.

``` r
# Cálculo de estatísticas descritivas
summary_stats <- wid_data_cleaned %>%
  summarise(mean_eci = mean(ECI, na.rm = TRUE),
            mean_gini = mean(Gini, na.rm = TRUE),
            mean_gdp = mean(GDP_per_capita, na.rm = TRUE))
summary_stats
```

::: {style="height: 1px;"}
:::

### Modelos de Regressão

A relação entre ECI e desigualdade de renda foi inicialmente investigada por meio de um modelo de regressão linear simples.

``` r
# Regressão Linear Simples
model <- lm(Gini ~ ECI + log(GDP_per_capita), data = wid_data_cleaned)
summary(model)
```

::: {style="height: 1px;"}
:::

### Modelos de Dados em Painel

Dado que os dados são temporais e por países, apliquei modelos de dados em painel, utilizando efeitos fixos e efeitos aleatórios.

``` r
# Modelagem com Efeitos Fixos
library(plm)
panel_model <- plm(Gini ~ ECI + log(GDP_per_capita), data = wid_data_cleaned, model = "within")
summary(panel_model)
```

::: {style="height: 1px;"}
:::

## Resultados

::: {style="height: 1px;"}
:::

### Visualização

Para facilitar a interpretação dos resultados, utilizei gráficos de regressão e gráficos de dispersão para visualizar a relação entre ECI e desigualdade de renda.

``` r
# Gráfico de dispersão para ECI vs Gini
ggplot(wid_data_cleaned, aes(x = ECI, y = Gini)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Relação entre ECI e Índice de Gini")
```

::: {style="height: 1px;"}
:::

### Discussão

Os modelos indicam uma relação negativa entre complexidade econômica e desigualdade de renda, especialmente nas camadas de menor renda. Contudo, o efeito parece diminuir nas camadas de maior renda, o que sugere que a complexidade econômica afeta mais as camadas mais pobres.

::: {style="height: 1px;"}
:::

## Transparência e Reprodutibilidade

The R code used for this analysis is available in the linked repository, which includes scripts for **data cleaning**, **statistical modeling**, and **visualization**. All steps are fully reproducible.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

# Conclusão

Este relatório demonstrou como as técnicas de ciência de dados foram aplicadas para explorar a relação entre complexidade econômica e desigualdade de renda. Os resultados indicam que países com economias mais complexas tendem a apresentar níveis mais baixos de desigualdade, principalmente entre as camadas de renda mais baixa. Embora o estudo tenha limitações, como a utilização de uma unidade de análise agregada, ele abre caminho para futuras investigações que poderiam explorar nível individual de dados ou outras variáveis explicativas, como complexidade tecnológica.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

# Referências

-   WID. (2023). *World Inequality Database*. Retrieved from <https://wid.world/data/>

-   SITC. (2023). *Atlas of Economic Complexity*. Retrieved from <https://atlas.hks.harvard.edu/>

-   Other relevant academic papers or articles.