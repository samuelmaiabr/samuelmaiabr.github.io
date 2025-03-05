---
title: "Relatório de Ciência dos Dados: Projeto *'Complexidade e Desigualdade'*"
author:
  - name: Samuel Maia
    affiliation: Cedeplar (UFMG)
    email: samuelmaiabr@gmail.com
bibliography: complexity-inequality-paper-copy.bib
csl: apa.csl
lang: pt
timezone: "America/Sao_Paulo"
format:
  html:
    css: "styles.css"
    toc: true
    toc-float: false
    toc-collapsed: false
    toc-depth: 3
    # toc-location: left
    toc-title: Relatório de Ciência dos Dados
    number-sections: true
    smooth-scroll: false
    theme: cosmo
    highlight: textmate
    df-print: paged
    fig-caption: true
    fig-width: 7
    fig-height: 5
    keep-md: true
    self-contained: true
    email-obfuscation: none
params:
  data-path: "data/wid_data.csv"
  geometry: margin=1in
---





# Visão geral

Neste relatório, apresento os conceitos e técnicas aprendidos no curso *Ciência de Dados Avançada*, no semestre 2024/2. Essas abordagens foram aplicadas ao projeto de pesquisa "Complexidade Econômica e Desigualdade de Renda: a vista do topo é diferente" (de agora em diante, *Projeto*), trabalho final da disciplina *Economia do Desenvolvimento* cursada no mesmo semestre.

Estruturei o relatório da seguinte forma. Na @sec-projeto, ***Projeto*** **e Bastidores**, faço uma breve apresentação do *Projeto* e das dificuldades aprendizados adquiridos ao longo do processo. Em seguida, a @sec-execucao -- a mais importante -- é dedicada à **Execução**. Ela segue a estrutura do planejamento adotado, dividido nas seguintes seções: "Preparação dos Dados", "Limpeza e Estruturação", "Análise" e "Resultados". No [Apêndice](#sec-appendix) encontram-se informações necessárias para garantir a repetibilidade das análises, incluindo detalhes do ambiente computacional e dos pacotes utilizados.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

# *Projeto* e Bastidores {#sec-projeto}

## Contextualização

@hartmann2017 encontraram uma associação negativa entre *desigualdade de renda* e *complexidade econômica*, medida pelo **Economic Complexity Index (ECI)** do ***Atlas de Complexidade Econômica (Atlas)***. Desde então, diversos estudos testaram essa relação em diferentes contextos, especialmente em análises regionais. No entanto, a desigualdade é frequentemente tratada de forma agregada, via Gini, ignorando sua distribuição entre diferentes estratos. Hartmann et al. usaram bases de dados da *All the Ginis (ALG)* e *Estimated Household Income Inequality Project (EHII)*, baseadas em pesquisas domiciliares que subestimam a renda no topo e são menos sensíveis às caudas da distribuição, e apenas usaram o Gini. 

Novas metodologias integram registros tributários e administrativos a surveys, além de métricas como parcelas apropriadas, como renda total apropriada por parcelas da distribuição (e.g., 10%, 1%, 0.1% mais ricos). Os resultados indicam que não apenas o nível, mas também as dinâmicas da desigualdade têm mudado [ver @DeRosa2024]. Meu objetivo com o *Projeto* foi replicar a análise de Hartmann et al. utilizando dados da ***World Inequality Database (WID)***, que seguem a abordagem das contas nacionais distributivas (DINA). Isso permitiu examinar a desigualdade de forma mais granular e capturar nuances que o Gini não evidencia, como o efeito progressivo da complexidade nos 90% inferiores da distribuição e sua regressividade no topo.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

# Execução {#sec-execucao}

Estruturei o trabalho com os dados do *Projeto* em cinco etapas:

1)  **Preparação**: Exploração das bases de dados (WID e Atlas) e compreensão de suas variáveis.
2)  **Limpeza e Estruturação**: Integração das bases e tratamento dos dados para análise.
3)  **Análises**: Execução de estatísticas descritivas, modelos de regressão e dados em painel.
4)  **Resultados**: Interpretação dos achados e comparação com a literatura existente.
5)  **Repetibilidade e Transparência**: Documentação do processo e organização do código.

Todas as etapas foram conduzidas com *R Statistical Software* [@Rlanguage]. Os detalhes para a repetibilidade dos resultados podem ser encontrados no [Apêndice](#sec-appendix).

::: {style="height: 1px;"}
:::

## Preparação dos Dados

A primeira etapa envolveu explorar as bases de dados para entender suas características, variáveis e estruturas. Com o objetivo de garantir a compatibilidade entre os dados de diferentes fontes, foram feitas inspeções iniciais e verificações de qualidade.

Para estudar a relação entre Complexidade Econômica e Desigualdade de Renda, utilizei duas grandes fontes de dados:
    
- **World Inequality Database (WID)**: Reúne estatísticas de desigualdade de renda e riqueza para mais de 200 países e regiões, cobrindo diferentes períodos. Aplicando a metodologia das Contas Nacionais Distributivas [@blanchet2024distributional],  a WID fornece dados para todos os percentis da distribuição, permitindo o cálculo de métricas como Gini, razão de Palma e parcela apropriada por fatias da distribuição (e.g., os 10%, 1% e 0,1% mais ricos). Além disso, inclui variáveis associadas a diferentes conceitos de renda  (por exemplo, *pretax income*, *post-tax income*, *factor income*). Para uma comparação entre a WID e outras bases de desigualdade, como ALG e EHII, utilizadas por @hartmann2017, comparadas com a WID, clique <a href="table_db_md.png" target="_blank">aqui</a>.

- **Atlas of Economic Complexity (SITC, Rev. 2)**: A segunda base corresponde a dados de comércio exterior e complexidade econômica no formato SITC, revision 2. Ela fornece a *Economic Complexity Index (ECI)*, *Product Complexity Index (PCI)* e valores de importação/exportação por produto e por país. Cada arquivo da SITC, normalmente, vem segmentado por produto ou por bloco de anos. É necessário agrupar esses dados para obter, por exemplo, o valor total de exportações de cada país em cada ano e, a partir daí, calcular ou recuperar o valor médio de ECI para aquele país-ano.

::: {style="height: 1px;"}
:::

## Limpeza e Estruturação dos Dados

Todo o processo de limpeza e integração dos dados encontra-se documentado nos scripts `wid-SITC-cleaning.R`. Entre todas as fases do projeto, a limpeza e estruturação dos dados foi a mais trabalhosa, sobretudo pela escala dos dados da WID, que somam mais de 50 milhões de observações. Resumidamente, segui os seguinets passos:
    
1. **Carregamento e Combinação da WID**  
- A WID estava segmentada em centenas de arquivos CSV, cada um abrangendo subconjuntos de países, anos e variáveis.  
- Utilizei processamento paralelo (`future_map_dfr()` em R) para ler todos os arquivos e concatená-los em um único *data frame* (`wid_full`).
- A seguir, selecionei apenas as variáveis de interesse (Gini, Palma, *share* do topo, GDP, população, etc.). Isso reduziu bastante o tamanho e a complexidade do *dataset* final.

- A Tabela 1 apresenta a estrutura dos códigos da WID e a Tabela 2 as variáveis  utilizados no Projeto -- incluindo Gini, razão de Palma, fração de renda do topo (shares) e seus respectivos percentis. Observa-se que ‘equal-split’ implica uma distribuição homogênea da renda dentro dos domicílios, o que amplia o número de observações disponíveis. Também selecionei as variáveis de **GDP per capita** e *população*, também disponibilizadas na WID, para criar métricas comparáveis entre países e ao longo do tempo.


<div class="small-table">

| **categoria**        | **código** | **significado**                 |
|----------------------|-----------|---------------------------------|
| tipo                | `s`         | fração de renda                |
| tipo                | `g`         | coeficiente de Gini            |
| tipo                | `r`         | Razão de Palma                 |
| conceito de renda   | `ptinc`     | nacional pré-tributação        |
| conceito de renda   | `diinc`    | nacional pós-tributação        |
| conceito de renda   | `fiinc`     | nacional fiscal                |
| unidade            | `j`         | "equal-split"                  |
| idade              | `992`       | acima de 20 anos               |
| percentil de renda | `p99p100`   | 1% no topo                     |

: Tabela 1: Estrutura dos Códigos da WID

</div>

&nbsp;

<div class="small-table">

| **código**   | **tipo**   | **renda**         | **percentis** |
|-------------|-----------|------------------|------------------------------------------------------|
| `sptincj992`  | fração    | pré-tributação   | `p050`, `p50p90`, `p90p100`, `p99p100`, `p99.9p100`, `99.99p100`, `99.999p100` |
| `sdiincj992`  | fração    | pós-tributação   | `p050`, `p50p90`, `p90p100`, `p99p100`, `99.99p100`, `99.999p100` |
| `gptincj992`  | Gini      | pré-tributação   | `p0100` |
| `gdiincj992`  | Gini      | pós-tributação   | `p0100` |
| `rptincj992`  | Palma     | pré-tributação   | `p0p40`, `p90p100` |

: Tabela 2:  Variáveis da WID selecionadas</span>

</div>

&nbsp;

- Várias colunas referentes a percentis (ex.: `p99p100`, `p90p100`) foram reorganizadas para serem interpretadas corretamente (mantendo-as como *string* para evitar erros de arredondamento).
- Concluída a filtragem, gerei *data frames* intermediários e salvei-os em formato `.csv` e `.rds` para que cada estágio do pipeline fosse repetível.

2. **Carregamento e Agregação do SITC (Atlas)**  

- Na SITC (Atlas) fiz uso das seguintes variáveis. **ECI** (`avg_eci`): indicador de complexidade econômica médio do país,
- **Exportações e Importações** (para filtrar países que tivessem volume significativo de comércio exterior),
- **Demais índices** (PCI, COI), embora o foco principal seja o ECI.
- Os dados do Atlas de Complexidade Econômica (SITC) vinham em diversos arquivos `.dta` (Stata) com granularidade produto-ano-país.  
- Para cada arquivo, extraí apenas as colunas de interesse: `country_id`, `year`, `export_value`, `import_value` e `eci`, entre outras. Em seguida, agreguei por país-ano, somando as exportações totais e calculando a média do ECI daquele país no ano.  
- Devido ao volume de dados, também usei blocos de processamento e funções em paralelo para evitar estouro de memória e agilizar a execução.  
- Resultando disso, obtive um *data frame* com uma linha por (país, ano), contendo *eci* médio e exportações totais (entre outras medidas).

3. **Integração com Informações de População (World Bank)**  
    - Para filtrar países com volume demográfico adequado ou para calcular métricas *per capita*, incorporei dados de população do World Bank (arquivo `.csv` com série histórica).  
- Converti o formato largo para longo (`pivot_longer()`), mantive apenas anos de interesse e apliquei `countrycode` para uniformizar códigos de país.

4. **Conversão de Códigos de País**  
    - A padronização de códigos de país foi fundamental. A WID, em muitos casos, utiliza códigos ISO2 (BR, US, FR, etc.), enquanto o SITC estava em ISO numérico (76, 840, 250, etc.).  
- Apliquei a função `countrycode()` em R para converter tudo para ISO3 (BRA, USA, FRA, etc.). Isso viabilizou a junção (`left_join` ou `inner_join`) pelo `country_id`.  
- Também removi códigos associados a regiões agregadas ou subdivisões de países que não interessavam na análise (p.ex. `US-AL`, `DE-BY`, `ARB`, `EUU`, etc.).

5. **Merge Final (SITC + WID)**  
    - Feita a padronização, o *merge* final uniu as variáveis de desigualdade (WID) e as variáveis de complexidade/fluxos comerciais (SITC) por (país, ano).  
- Nesse estágio, verifiquei a existência de *NAs* (valores ausentes) e outliers, decidindo por remoção ou imputação dependendo do caso.  
- Alguns filtros adicionais foram aplicados para manter apenas países e anos relevantes (por exemplo, 1962–2008 para a parte de ECI, já que essa é a abrangência principal do SITC Rev. 2).

6. **Checagem de Consistência e Salvando a Versão Final**  
    - Conferi o número de observações e a distribuição temporal (quantos dados por década e por país).
    
    - Na Figura X, é possível visualizar a distribuição final de observações por década e por variável, evidenciando a concentração dos dados principalmente a partir dos anos 1980.
    
- Gerei estatísticas descritivas (médias, desvios-padrão) para verificar se os valores de ECI, Gini, participação do topo, etc. estavam dentro do esperado.  
- Finalmente, salvei o *dataset* resultante em formatos `.csv` e `.rds`, aptos para a próxima etapa de análise exploratória e regressões.

A leitura e padronização dos arquivos `.csv` da WID foi feita por meio de **programação funcional** e **paralelização** em R, seguindo o que está ilustrado abaixo (Exemplo 1). A listagem de arquivos é obtida com `list.files()`, e cada arquivo passa pela função `process_wid_file()` que lê e transforma colunas específicas. Estes procedimentos empregam **conceitos de orientação a objetos** e **manipulação de estruturas de dados** (principalmente data frames em R), bem como funções do pacote `dplyr` (como `mutate`, `group_by` etc.). Também ilustram as seguintes práticas:
    
- **Programação Funcional e Criação de Funções:** Uso de funções personalizadas (e.g., `process_wid_file()`) para encapsular lógica de leitura e limpeza de cada arquivo, facilitando a manutenção e a reusabilidade de código.
- Relaciona-se ao **Fundamento de Programação em R** visto no curso, incluindo a criação de funções e boas práticas de programação.



::: {.cell}

```{.r .cell-code}
# Uso de programação funcional para ler múltiplos arquivos (funções, loops, paralelismo)

process_wid_file <- function(file_path) {
  # Lê e manipula colunas necessárias
  df <- readr::read_delim(file_path, delim = ";", show_col_types = FALSE) %>%
    dplyr::select(country, variable, percentile, year, value) %>%
    dplyr::mutate(
      lower_percentile = stringr::str_remove(
        stringr::str_extract(percentile, "^p[0-9\\.]+"), "^p"
      ),
      upper_percentile = stringr::str_remove(
        stringr::str_extract(percentile, "p[0-9\\.]+$"), "^p"
      )
    )
  return(df)
}
```
:::



- **Paralelização:** Emprego de `future_map_dfr()` (pacote `**furrr**`) para processamento paralelo, tornando a execução mais eficiente.
- Alinha-se ao tópico **Fundamentos de Programação Avançada**, onde se discutem loops, paralelismo e otimização.



::: {.cell}

```{.r .cell-code}
# Paralelizando a leitura de arquivos com 'future_map_dfr()'
future::plan("multisession", workers = 4)

wid_files <- list.files(path = "data/wid_all_data", pattern = "\\.csv$", full.names = TRUE)
wid_full <- furrr::future_map_dfr(wid_files, process_wid_file)
```
:::



- **Manipulações Avançadas com `dplyr`:** Operações como `select`, `filter`, `mutate`, `group_by` e `summarise` para limpeza e transformação de dados.
- Conecta-se a **Principais Bibliotecas de Ciência de Dados**, demonstrando como organizar e agregar dados de forma idiomática no R.



::: {.cell}

```{.r .cell-code}
# Exemplo 2: uso de verbos do dplyr para limpeza
wid_filtered <- wid_full %>%
  dplyr::filter(variable %in% c("sptincj992", "sdiincj992", "gptincj992", "gfiincj992")) %>%
  dplyr::filter(!is.na(value), lower_percentile %in% c("99", "0")) %>%
  dplyr::mutate(lower_percentile = as.numeric(lower_percentile),
                period_flag = ifelse(year >= 1980, TRUE, FALSE))
```
:::



- **Estrutura e Limpeza de Dados:** Conversão de colunas (p. ex., strings → numérico), criação de variáveis derivadas (e.g., `decade`, `period_flag`), filtragem de *NAs*, seleção de variáveis relevantes e remoção de outliers.
- Está em sintonia com os **Conceitos de Limpeza e Organização de Dados** do curso, ilustrando boas práticas de pré-processamento.



::: {.cell}

```{.r .cell-code}
# Exemplo 3: sumarizar e agrupar — observações por país e ano
wid_summary <- wid_filtered %>%
  dplyr::group_by(country, year) %>%
  dplyr::summarise(
    mean_value = mean(value, na.rm = TRUE),
    n_obs = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(decade = floor(year / 10) * 10)

# Exibindo algumas linhas do resultado
head(wid_summary)
```
:::



- **Visualização e Ferramentas Estatísticas:** Análises com `lm()` e outras técnicas estatísticas, acompanhadas de gráficos gerados via `ggplot2`.
- Relaciona-se à parte de **Visualização e Análise Avançada** e às **Ferramentas Estatísticas** (e.g., regressão linear), usando a sintaxe do `tidyverse` para apresentações claras e reprodutíveis.


Isso ilustra como as técnicas vistas no curso de *Ciência dos Dados* (funções, paralelismo, operadores do `tidyverse`, etc.) foram aplicadas para limpeza e estruturação dos dados.

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

### Cozinha de Pesquisa": limitações e aprendizado

Ao longo do *Projeto*, o principal desafio foi limpar e integrar as bases da WID e do Atlas.  

A estratégia (um tanto óbvia, é verdade) de dividir o processo em três etapas — conhecer a base, limpá-la e estruturá-la, e só depois partir para as análises — funcionou razoavelmente bem. Mas, como era de se esperar, houve muito **vai e volta**. Um exemplo: só depois de já estar rodando as análises percebi que, na minha base estruturada, havia deixado de fora variáveis importantes. Resultado? Tive que voltar para a fase de limpeza e refazer parte do pipeline.

A documentação do Atlas e do WID -- especialmente seu dicionário de variáveis [@wid_codes_dictionary] -- é relativamente clara. Porém, os nomes das variáveis da WID, em particular, podem não parecer intuitivos à primeira vista. Com o tempo, me acostumei à estrutura dos códigos, mas há espaço para melhorias para torná-los mais acessíveis desde o início.

Algumas coisas que aprendi ao longo do caminho. Um problema que consumiu tempo foi lidar com **percentis em casas decimais** (99,9%; 99,99% e 99,999%). Com frequência, esses valores eram lidos como `numeric`, o que fazia com que os decimais fossem truncados. Demorei para perceber essa questão até que finalmente tratei os percentis como `character`, evitando a perda de informação.  

Outro ponto curioso — e até irônico — é que a **cobertura da WID é bastante desigual**. Das mais de 57 milhões de observações, os Estados Unidos sozinhos possuem mais de 800 mil, enquanto o Brasil tem cerca de 250 mil e o Vietnã menos de 650. Além disso, a maior parte dos dados está concentrada nos últimos 40 anos, o que inviabilizou iniciar a análise em 1960, como em Hartman et al. Diante disso, optei por trabalhar com observações de 1980 até o final dos anos 2000, focando em variáveis de renda bruta e líquida, além da unidadade de análise "equal-split".[^wid-stats]

[^wid-stats]: A WID contém um total de 57.208.488 observações distribuídas entre 1.793 variáveis, cobrindo o período de 1807 a 2023. A média dos anos com observações na base é 1999, com um desvio padrão de 20 anos. O primeiro quartil (Q25) corresponde ao ano de 1990, a mediana situa-se em 2002 e o terceiro quartil (Q75) em 2012. O número de anos com dados disponíveis por país também varia consideravelmente: o mínimo é 43 anos, enquanto o máximo chega a 214. Em média, cada país possui informações cobrindo 87 anos, com os seguintes quartis: Q25 = 44 anos, Q50 = 74 anos e Q75 = 95 anos.

Finalmente, não fiz uso de tudo que a WID oferece. As análises foram feitas com faixas de distribuição mais amplas do que as que a WID oferece (50%, 40% e percentis do 10% mais rico). Mas, como a WID disponibiliza os dados em percentis individuais, eu poderia ter gerado recortes mais detalhados. Tentei fazer isso, mas depois de horas rodando (mesmo com paralelização), o processo ainda não tinha terminado. Como saída possível, testei uma abordagem baseada em decis, que confirmou as tendências esperadas, mas acabou ficando de fora do relatório. A demora no processamento certamente poderia ser otimizada com técnicas mais sofisticadas, que, honestamente, eu ainda não domino.  

Outra lição foi sobre **processamento paralelo e repetibilidade**. Para garantir que o código rode de forma consistente em diferentes máquinas, é importante levar em conta limitações de hardware. Descobri que existem alternativas, como `parallel::detectCores()`, que permitem verificar o número de núcleos disponíveis antes de definir o número de *workers*, evitando que o código tente usar mais recursos do que a máquina suporta. Para projetos futuros, isso parece uma boa prática para garantir que os scripts sejam reproduzíveis em ambientes diversos.  


::: {style="height: 1px;"}
:::

------------------------------------------------------------------------


# Agradecimentos {.unnumbered .appendix}

Agradeço a Mateus Leite pela valiosa ajuda na criação dos códigos em R. Mas claro, tanto a falta de elegância quanto eventuais falhas em seu funcionamento ficam por minha conta.

# *Apêndice:* Repetibilidade e Transparência {.unnumbered .appendix #sec-appendix}

Para garantir a **repetibilidade**[^repetibilidade]. ou seja, a possibilidade de se obterem os mesmos resultados quando se utilizam exatamente os mesmos dados, código e condições de análise.

Disponibilizo dois arquivos-chave em **R**: 

- **`wid-SITC-cleaning.R`**: tratamento, integração e estruturação das bases de dados.  
- **`analysis-proper.R`**: modelos estatísticos e análises conduzidas.

Esses scripts podem ser encontrados no arquivo mencionado na Seção X. Além disso, a lista completa de pacotes utilizados, juntamente com suas respectivas versões, também está disponibilizada para consulta.

**Ambiente computacional:**  Todas as análises foram realizadas em R versão **4.4.1 (2024-06-14)**, com os pacotes X Y E Z. Para conferir as especificações do ambiente de trabalho, incluindo o R, o sistema operacional e as configurações locais, acesse

[^repetibilidade]: Seguindo o relatório da *National Academies of Science* [-@rrc2019], é útil distinguir **repetibilidade** de **reprodutibilidade** (ou “computational reproducibility”): a primeira envolve a obtenção de resultados consistentes utilizando exatamente o mesmo conjunto de dados e o mesmo código (focando, portanto, no ambiente computacional original), enquanto a segunda (ou *replicabilidade*, em outra denominação) implica obter resultados consistentes em novos estudos que buscam responder à mesma questão científica, mas com dados coletados de forma independente. Por fim, há ainda o conceito de **generalizabilidade**, que se refere à extensão dos resultados para diferentes contextos ou populações. Neste relatório, nosso principal objetivo foi **garantir a repetibilidade** das análises apresentadas.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

