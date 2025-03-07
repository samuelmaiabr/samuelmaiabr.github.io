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

Neste relatório, apresento como apliquei conceitos e técnicas aprendidas no curso *Ciência de Dados Avançada*, semestre 2024/2, em um projeto de pesquisa. Especificamente, os apliquei para gerar o artigo "Complexidade Econômica e Desigualdade de Renda: a vista do topo é diferente" (de agora em diante, *Projeto*), trabalho final da disciplina *Economia do Desenvolvimento*, cursada no mesmo semestre. O artigo pode ser acessado clicando [aqui](https://drive.google.com/file/d/1xuicRVD2I5QZdf2HObj_-TefAYSYd28O/view?usp=sharing){target="_blank"}.

Executei o ***Projeto*** em duas etapas: a primeira, de exploração, limpeza e estruturação dos dados; a segunda, dedicada à análise e aos resultados. Neste relatório trato apenas da primeira, pois incluir também a fase de análise e resultados o tornaria excessivamente extenso. 

Organizei o relatório da seguinte forma. Na @sec-projeto apresento o tema do artigo, cuja realização envolveu Neste relatório me concentro exclusivamente na primeira etapa, que é o objeto da @sec-execucao, **Execução**. Na @sec-cozinha apresento a **Cozinha de Pesquisa**, com as limitações enfrentados e aprendizado adquirido no processo. O relatório termina na @sec-appendix, com informações sobre a **Repetibilidade** do trabalho.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

# Um *Projeto* sobre complexidade e desigualdade {#sec-projeto}

A **complexidade econômica** de um sistema é determinada por dois fatores principais: *diversidade* e *ubiquidade* de seus produtos [@Hidalgo2007; @atlasEconomicComplexity]. A diversidade representa a variedade de produtos que um país fabrica com vantagem comparativa, enquanto a ubiquidade mede quantos outros países também exportam esses produtos. Os dois fatores estão associados a um maior PIB per capita e ao crescimento econômico, especialmente em países que evoluíram de economias baseadas em recursos naturais para setores mais sofisticados. Para quantificá-los, César Hidalgo e Ricardo Hausmann desenvolveram o **Índice de Complexidade Econômica (ECI)** [@Hidalgo2009].

@Hartmann2017 encontraram uma associação negativa entre *desigualdade de renda* e o ECI, calculado segundo o ***SITC, Rev. 2 do Atlas of Economic Complexity (Atlas)*** [@GrowthLab2019]. Países de economia menos complexa tendem a ser mais desiguais e quanto maior o grau de complexidade de um país, menor sua desigualdade. Desde então, diversos estudos testaram essa relação em diferentes contextos, especialmente regionais. 

No entanto, a desigualdade é frequentemente representada pelo índice de Gini, que não permite conhecermos qual a parcela da renda apropriada tanto pelas faixas mais pobres quanto pelas mais ricas da distribuição. Além disso, a principal fonte de dados são pesquisas domiciliares, que subestimam sistematicamente a concentração de renda no topo [@Alvaredo2023]. Esse é o caso de Hartmann et al., que utilizaram os Ginis calculados pelo ***All the Ginis (ALG)*** e o ***Estimated Household Income Inequality Project (EHII)***, ambas as bases construídas principalmente a partir de surveys. Para conferir a literatura sobre o tema, as medidas de desigualdade e as principais fontes dos dados empregadas, veja a *Tabela 1* clicando <a href="table_gini.png" target="_blank">aqui</a>.

Para superar esses problemas, pesquisadores têm calculado a desigualdade segundo a metodologia das **Contas Nacionais Distributivas** (da sigla em inglês, **DINA**). As DINAs combinam dados de surveys com registros tributários e administrativos e procuram tornar os resultados compatíveis com as estimativas agregadas das contas nacionais [@piketty2018distributional]. Além de diminuir as dificuldades associadas a alcançar a concentração de renda no topo, isso permite calcular parcelas da renda total apropriadas por diferentes faixas da distribuição (e.g., 50% mais pobres, 10%, 1% e 0,1% mais ricos). No geral, os resultados indicam que não apenas o nível foi subestimado, mas mesmo algumas das dinâmicas da desigualdade podem ser diferentes do que resultados anteriores indicavam [este é o caso da América Latina segundo @DeRosa2024]. 

Meu objetivo com o *Projeto* foi replicar a análise de Hartmann et al. utilizando dados da ***World Inequality Database (WID)***, construídas segundo a abordagem da DINA pelo ***World Inequality Lab***. Isso permitiu examinar a relação da complexidade com a desigualdade de forma mais granular. Se no nível agregado os achados reforçaram a relação negativa entre complexidade e desigualdade, também encontrei novos fatos estilizados. Primeiro, o efeito progressivo que acompanha a complexidade se concentra nos 90% inferiores da distribuição. Isso é particularmente forte entre os 50% mais pobres, cuja renda é a que mais cresce conforme a complexidade aumenta. A relação se torna negativa apenas dentro dos 10% mais ricos, mas aqui ela se torna regressiva. Quanto mais nos aproximamos do topo, menor é o efeito de redução da renda associado à mais complexidade. A partir do 1% mais ricos, isso praticamente desaparece. Ou seja, quando vista do topo, a relação entre estrutura produtiva e desigualdade não é homogênea.

Dito isso, agora podemos falar dos conceitos e técnicas de ciência dos dados aplicadas no *Projeto* propriamente.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

# Execução {#sec-execucao}

Estruturei a realização do *Projeto* em cinco etapas:

1)  **Exploração**: Exploração das bases de dados (WID e SITC, Rev. 2) e compreensão de suas variáveis.
2)  **Limpeza e Estruturação**: Integração das bases e tratamento dos dados para análise.
3)  **Análises**: Execução de estatísticas descritivas, modelos de regressão e dados em painel.
4)  **Resultados**: Interpretação dos achados e comparação com a literatura existente.
5)  **Repetibilidade e Transparência**: Documentação do processo e organização do código.

Todas as etapas foram conduzidas em R versão **4.4.1, 2024-06-14** [@Rlanguage]. Como mencionei na introdução, apenas as duas primeiras são objeto deste relatório. De qualquer modo, o script usado na análise e obtenção dos resultados (`analysis-proper.R`) pode ser baixado clicando [aqui](analysis-proper.R). Os detalhes sobre a repetibilidade e transparência dos resultados são tratadas na @sec-appendix.

::: {style="height: 1px;"}
:::

## Exploração dos Dados

Aqui o principal objetivo foi construir uma base de dados que reunisse os dados sobre complexidade do Atlas e aqueles de desigualdade da WID e permitisse conduzir as análises relevantes. Para isso explorei ambas as bases para entender suas características, variáveis e estruturas. Falemos sobre cada uma:

**World Inequality Database (WID)**: A base completa pode ser baixada clicando neste [link](https://wid.world/data/). A WID consiste em centenas de `.csv`, sendo pelo menos dois por país — um com os dados e outro com os metadados. Além disso, há arquivos que agregam informações de regiões inteiras, como a América Latina e o Caribe. Em alguns casos, como China e Estados Unidos, há também arquivos específicos para subdivisões intranacionais. Há estatísticas sobre desigualdade de renda, riqueza, população e agregados macroeconômicos para mais de 200 países e regiões, cobrindo diferentes períodos. 

A base é construída segundo a metodologia da DINA e fornece dados para todos os percentis da distribuição [@blanchet2024distributional]. Além disso, inclui variáveis associadas a diferentes conceitos de renda  (por exemplo, *pretax income*, *post-tax income*, *factor income*). A *Tabela 2* abaixo apresenta a estrutura dos códigos da WID. O dicionário de códigos pode ser acessado [aqui](https://wid.world/codes-dictionary/). Para uma comparação entre a WID e outras bases de desigualdade, como as utilizadas por @Hartmann2017, acesse a *Tabela 3* clicando <a href="table_db_md.png" target="_blank">aqui</a>.

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

: Tabela 3: Estrutura dos Códigos da WID

</div>

&nbsp;

**SITC, Rev. 2 do Atlas of Economic Complexity**: O Atlas oferece bases sobre comércio internacional, crescimento econômico, e produtos e serviços classificados segundo a noção de complexidade econômica [@atlasEconomicComplexity]. Uma apresentação curta sobre ele se encontra [aqui](https://atlas.hks.harvard.edu/about-data#data). Ele contém dados históricos de comércio bilateral para aproximadamente 700 produtos agrupados em 10 setores, cobrindo mais de 250 países e territórios desde 1962. A fonte do Atlas são dados da *United Nations Statistical Division (Comtrade)* e da *IMF Direction of Trade Statistics*.  Os dados são estruturados em dois formatos:

O Atlas disponibiliza os dados estruturados segundo dois sistemas de classificação: *Harmonized System (HS, 1992)* e a *Standard International Trade Classification (SITC, Rev. 2)*. Na SITC, Rev. 2 [@SITC-2], os produtos são classificados em níveis de 1-, 2- ou 4-dígitos, mantendo consistência metodológica desde os anos 1960 – mesmo com o surgimento de novos produtos (ex: eletrônicos). Já a a base HS, 1992 é usada para análises mais recentes. Ela inclui por volta de 5000 produtos (detalhados em 1-, 2-, 4- ou 6-dígitos), mas cobre um período mais curto, a partir de 1995. A *Tabela 4* abaixo apresenta a estrutura dos códigos da SITC, Rev. 2. Em sua análise original, Hartmann et al. optaram pela SITC, Rev. 2, pois ela permite estudos de longo prazo, ainda que com menor granularidade. Pela mesma razão, é ela que empreguei aqui.

<div class="small-table">

| **categoria**       | **código**          | **significado**                                          |
|---------------------|--------------------|----------------------------------------------------------|
| país               | `country_id`        | código do país (M49 - ONU)                              |
| país parceiro      | `partner_country_id`| código do país parceiro (M49 - ONU)                     |
| produto            | `product_id`        | identificador numérico do produto (Growth Lab)         |
| ano                | `year`              | ano do registro                                         |
| valor de exportação | `export_value`      | valor exportado em dólares correntes                   |
| valor de importação | `import_value`      | valor importado em dólares correntes                   |
| complexidade       | `eci`               | Índice de Complexidade Econômica (ECI-SITC)                |
| complexidade       | `coi`               | Complexity Outlook Index (COI-SITC)                        |
| complexidade       | `pci`               | Índice de Complexidade do Produto (PCI)                      |

: Tabela 4: Estrutura dos Códigos da SITC, Rev. 2 [@SITC-2]

</div>


::: {style="height: 1px;"}
:::

## Limpeza e Estruturação dos Dados

O processo de limpeza e integração dos dados da WID e da SITC, Rev. 2 está documentado no script `wid-SITC-cleaning.R` (baixe [aqui](wid-SITC-cleaning.R)). Resumidamente, segui os seguintes passos:
    
1. **Carregamento e Combinação da WID**  

- Todos os arquivos da WID foram lidos e concatenados em um único *data frame* (`wid_full`). Para otimizar esta etapa, utilizei processamento paralelo (`future_map_dfr()`), que distribui a carga de trabalho entre múltiplos núcleos do computador.
- A seguir, selecionei apenas as variáveis de interesse do **Projeto** (veja a *Tabela 4* abaixo): Gini, razão de Palma (razão da renda apropriada pelos 10% mais ricos e os 40% mais pobres), e renda apropriada por diferentes frações (50% mais pobres, 40% intermediários, e 10%, 1%, 0,1%, 0,01%, 0,001% mais ricos).

<div class="small-table">

| **código**   | **tipo**   | **renda**         | **percentis** |
|-------------|-----------|------------------|------------------------------------------------------|
| `sptincj992`  | fração    | pré-tributação   | `p050`, `p50p90`, `p90p100`, `p99p100`, `p99.9p100`, `99.99p100`, `99.999p100` |
| `sdiincj992`  | fração    | pós-tributação   | `p050`, `p50p90`, `p90p100`, `p99p100`, `p99.9p100`, `p99.9p100`, `99.999p100` |
| `gptincj992`  | Gini      | pré-tributação   | `p0100` |
| `gdiincj992`  | Gini      | pós-tributação   | `p0100` |
| `rptincj992`  | Palma     | pré-tributação   | `p0p40`, `p90p100` |
| `rdiincj992`  | Palma     | pré-tributação   | `p0p40`, `p90p100` |

: Tabela 4:  Variáveis da WID selecionadas</span>

</div>

&nbsp;

- Duas coisas devem ser notadas. Além das variáveis distributivas, precisamos definir o conceito de renda. Dentre os diversos disponíveis na WID, optei pela renda pré-tributação (`ptinc`) e pela renda pós-tribução (`diinc`). Isso permitiria avaliar a interação da redistribuição de renda com a complexidade e desigualdade. Segundo, é necessário estabelecer a unidade e a faixa etária das variáveis de desigualdade (veja a **Tabela 3**). Minha escolha foi orientada pela disponibilidade de dados. Como podemos ver na **Figura 1**, a unidade "equal-split", que divide a renda familiar igualmente entre adultos no domicílio (`j`) é significativamente mais abrangente do que a unidade individual, assim como a faixa etária que considera indivíduos com 20 anos ou mais (`992`). A explicação para essas escolhas é a distribuição dos dados da WID. Conforme é possível ver na *Figura 1*.



- Por fim, também selecionei as variáveis de **GDP** e *população*, também disponibilizadas na WID, necessárias para o cálculo do PIB per capita, utilizado como controle na fase de análise.

- Várias colunas referentes a percentis (ex.: `p99p100`, `p90p100`) foram reorganizadas para serem interpretadas corretamente (mantendo-as como *string* para evitar erros de arredondamento).
- Concluída a filtragem, gerei *data frames* intermediários e salvei-os em formato `.csv` e `.rds` para que cada estágio do pipeline fosse repetível.

2. **Carregamento e Agregação do SITC (Atlas)**  

Na SITC (Atlas) fiz uso das seguintes variáveis:

- **ECI** (`avg_eci`): indicador de complexidade econômica médio do país.
- **Exportações e Importações** (para filtrar países que tivessem volume significativo de comércio exterior)
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

[INCLUIR OS PRIMEIROS PLOTS]

- Relaciona-se à parte de **Visualização e Análise Avançada** e às **Ferramentas Estatísticas** (e.g., regressão linear), usando a sintaxe do `tidyverse` para apresentações claras e reprodutíveis.


Isso ilustra como as técnicas vistas no curso de *Ciência dos Dados* (funções, paralelismo, operadores do `tidyverse`, etc.) foram aplicadas para limpeza e estruturação dos dados.


::: {style="height: 1px;"}
:::

# "Cozinha de Pesquisa": limitações e aprendizado {#sec-cozinha}

Ao longo do *Projeto*, o principal desafio foi limpar e integrar as bases da WID e do Atlas.  

A estratégia (um tanto óbvia, é verdade) de dividir o processo em três etapas — conhecer a base, limpá-la e estruturá-la, e só depois partir para as análises — funcionou razoavelmente bem. Mas, como era de se esperar, houve muito **vai e volta**. Um exemplo: só depois de já estar rodando as análises percebi que, na minha base estruturada, havia deixado de fora variáveis importantes. Resultado? Tive que voltar para a fase de limpeza e refazer parte do pipeline.

A documentação do Atlas e do WID -- especialmente seu dicionário de variáveis [@wid_codes_dictionary] -- é relativamente clara. Porém, os nomes das variáveis da WID, em particular, podem não parecer intuitivos à primeira vista. Com o tempo, me acostumei à estrutura dos códigos, mas há espaço para melhorias para torná-los mais acessíveis desde o início.

Algumas coisas que aprendi ao longo do caminho. Um problema que consumiu tempo foi lidar com **percentis em casas decimais** (99,9%; 99,99% e 99,999%). Com frequência, esses valores eram lidos como `numeric`, o que fazia com que os decimais fossem truncados. Demorei para perceber essa questão até que finalmente tratei os percentis como `character`, evitando a perda de informação.  

Outro ponto curioso — e até irônico — é que a **cobertura da WID é bastante desigual**. Das mais de 57 milhões de observações, os Estados Unidos sozinhos possuem mais de 800 mil, enquanto o Brasil tem cerca de 250 mil e o Vietnã menos de 650. Além disso, a maior parte dos dados está concentrada nos últimos 40 anos, o que inviabilizou iniciar a análise em 1960, como em Hartman et al. Diante disso, optei por trabalhar com observações de 1980 até o final dos anos 2000, focando em variáveis de renda bruta e líquida, além da unidadade de análise "equal-split".[^wid-stats]

[^wid-stats]: A WID contém um total de 57.208.488 observações distribuídas entre 1.793 variáveis, cobrindo o período de 1807 a 2023. A média dos anos com observações na base é 1999, com um desvio padrão de 20 anos. O primeiro quartil (Q25) corresponde ao ano de 1990, a mediana situa-se em 2002 e o terceiro quartil (Q75) em 2012. O número de anos com dados disponíveis por país também varia consideravelmente: o mínimo é 43 anos, enquanto o máximo chega a 214. Em média, cada país possui informações cobrindo 87 anos, com os seguintes quartis: Q25 = 44 anos, Q50 = 74 anos e Q75 = 95 anos.

Finalmente, não fiz uso de tudo que a WID oferece. As análises foram feitas com faixas de distribuição mais amplas do que as que a WID oferece (50%, 40% e percentis do 10% mais rico). Mas, como a WID disponibiliza os dados em percentis individuais, eu poderia ter gerado recortes mais detalhados. Tentei fazer isso, mas depois de horas rodando (mesmo com paralelização), o processo ainda não tinha terminado. Como saída possível, testei uma abordagem baseada em decis, que confirmou as tendências esperadas, mas acabou ficando de fora do relatório. A demora no processamento certamente poderia ser otimizada com técnicas mais sofisticadas, que, honestamente, eu ainda não domino.  

Outra lição foi sobre **processamento paralelo e repetibilidade**. Para garantir que o código rode de forma consistente em diferentes máquinas, é importante levar em conta limitações de hardware. Descobri que existem alternativas, como `parallel::detectCores()`, que permitem verificar o número de núcleos disponíveis antes de definir o número de *workers*, evitando que o código tente usar mais recursos do que a máquina suporta. Para projetos futuros, isso parece uma boa prática para garantir que os scripts sejam reproduzíveis em ambientes diversos.  

# Repetibilidade {#sec-appendix}

Segundo o relatório da *National Academies of Science* [-@rrc2019], é útil distinguir **repetibilidade** de **reprodutibilidade**. A primeira envolve a obtenção de resultados consistentes utilizando exatamente o mesmo ambiente computacional. Por isso também podemos chamá-la de *reprodutibilidade computacional*. Já a segunda, também chamada de *replicabilidade*, implica obter resultados consistentes em novos estudos que buscam responder à mesma questão científica, mas com dados coletados de forma independente. 

Para garantir a **repetibilidade** da exploração, limpeza e estruturação dos dados, acesse o **Apêndice** clicando [aqui](repetibilidade.html). Nele você poderá baixar o script `wid-SITC-cleaning.R` além de encontrar informações sobre o ambiente computacional e os pacotes utilizados.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------


# Agradecimentos {.unnumbered .appendix}

Agradeço a Mateus Leite pela valiosa ajuda na criação dos códigos em R. Mas claro, tanto a falta de elegância quanto eventuais falhas em seu funcionamento ficam por minha conta.


[^verrepetibilidade]: Veja a nota <a href="#repetibilidade">2</a>.

::: {style="height: 1px;"}
:::

------------------------------------------------------------------------

