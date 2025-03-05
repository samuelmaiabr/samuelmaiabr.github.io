# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
# ANALYSIS PROPER ####
# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #


# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
## 0. PACOTES ####
# =========== # =========== # =========== # =========== # =========== # =========== #

install.packages(c(
    "tidyverse", "ggplot2", "readr", "dplyr", 
    "countrycode", "lmtest", "data.table", 
    "broom", "fixest", "GGally", "kableExtra", "tikzDevice", "purrr", "xtable", "gridExtra", "plm")
)

library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(countrycode)
library(lmtest)
library(data.table)
library(broom)
library(fixest)
library(GGally)
library(kableExtra)
library(tikzDevice)
library(purrr)
library(xtable)
library(gridExtra)
library(plm)
library(lmtest)
library(sandwich)



# Limpar o ambiente para evitar conflitos
rm(list = ls())
gc()
# =========== # =========== # =========== # =========== # =========== # =========== #


# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
## 1. CARREGAR dados filtrados ####
# =========== # =========== # =========== # =========== # =========== # =========== #

# Definir o caminho
file_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/processed/combined_sitc_wid.rds"
# Carregar dados
combined_data <- readRDS(file_path)

# Verificar a estrutura dos dados carregados
glimpse(combined_data)
# =========== # =========== # =========== # =========== # =========== # =========== #




# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
## 2. ANÁLISES ####

### ~ ####
### 2.1 CORRELAÇÃO ####

#### 2.1.1 Ajustes ####
# Selecionar variáveis

share_variables <- c("sptincj992", "sdiincj992")
gini_variables  <- c("gfiincj992", "gdiincj992")
palma_variables <- c("rptincj992")


top_percentiles <- c(90, 99, 99.9, 99.99, 99.999)
bottom_percentiles <- c(0) 
middle_percentiles <- c(50)

#### 2.1.2 Calcular correlações ####
calculate_correlation <- function(data) {
    correlation_results <- data.frame()
    
    for (decade in unique(data$decade)) {
        decade_data <- data %>% filter(decade == as.character(decade))
        
        if (nrow(decade_data) == 0) next
        
        for (variable in c(share_variables, gini_variables, palma_variables)) {
            var_data <- decade_data %>%
                filter(wid_variable == variable) %>%
                drop_na(avg_eci, wid_value)
            
            if (variable %in% share_variables) {
                # Para as variáveis de share, calcular separadamente para cada percentil do topo
                for (percentile in top_percentiles) {
                    percentile_data <- var_data %>%
                        filter(lower_percentile == percentile)
                    
                    if (nrow(percentile_data) > 5) {
                        cor_value <- cor(percentile_data$avg_eci, percentile_data$wid_value, use = "complete.obs")
                        
                        correlation_results <- rbind(correlation_results, 
                            data.frame(
                                Decade = decade,
                                Variable = variable,
                                Percentile = percentile,
                                Correlation = cor_value
                            )
                        )
                    }
                }
            } else {
                # Para Gini e Palma, calcular a correlação normal
                if (nrow(var_data) > 5) {
                    cor_value <- cor(var_data$avg_eci, var_data$wid_value, use = "complete.obs")
                    
                    correlation_results <- rbind(correlation_results, 
                        data.frame(
                            Decade = decade,
                            Variable = variable,
                            Percentile = NA,  # Não se aplica a Gini e Palma
                            Correlation = cor_value
                        )
                    )
                }
            }
        }
    }
    
    return(correlation_results)
}

# Correlações
correlation_results <- calculate_correlation(combined_data)
# Exibir e salvar os resultados
print(correlation_results)

output_file <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/analyses/correlation/correlation_results.csv"
write.csv(correlation_results, output_file, row.names = FALSE)


#### 2.1.3 Gráficos de share ####

# Defina o caminho de saída
output_path_top <- '/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/writing/eci_top_sptincj992.png'

# Topo
plot_top_shares <- function(data, variable) {
    message(paste("Gerando gráfico para:", variable))
    
    plot_data <- data %>%
        filter(
            wid_variable == variable, 
            lower_percentile %in% c(90, 99, 99.9, 99.99, 99.999),
            decade == "2000"  # Filtrando apenas a década de 2000
        ) %>%
        drop_na(avg_eci, wid_value)
    
    message(paste("Número de linhas após o filtro para", variable, ":", nrow(plot_data)))
    
    if (nrow(plot_data) == 0) {
        message(paste("Nenhum dado disponível para:", variable))
        return(NULL)  # Retorna NULL se não houver dados suficientes
    }
    
    ggplot(plot_data, aes(x = avg_eci, y = wid_value, color = as.factor(lower_percentile))) +
        geom_point(alpha = 0.4) +  
        geom_smooth(method = "lm", se = FALSE, size = 1.75, aes(group = lower_percentile), color = "black") +
        geom_smooth(method = "lm", se = FALSE, size = 1, aes(color = as.factor(lower_percentile))) +  
        labs(
            title = paste("ECI vs. faixas de renda disponível (2000)"),
            x = "Economic Complexity Index",
            y = "Share de Renda",
            color = "Percentil"
        ) +
        theme_minimal(base_family = "lmodern") +  # Fonte LaTeX
        scale_color_manual(values = c("red", "lightsalmon", "green", "blue", "purple"))  
}
plot_top_shares(combined_data, "sptincj992")

plot_top_shares(combined_data, "sdiincj992")


# Base
plot_bottom_middle_shares <- function(data, variable) {
    message(paste("Gerando gráfico para:", variable))
    
    plot_data <- data %>%
        filter(
            wid_variable == variable,
            decade == "2000"  # Filtrando apenas a década de 2000
        ) %>%
        mutate(percentile_group = case_when(
            lower_percentile == 0 & upper_percentile == 50 ~ "Bottom 50%",
            lower_percentile == 50 & upper_percentile == 90 ~ "Middle 40%",
            TRUE ~ NA_character_
        )) %>%
        drop_na(percentile_group, avg_eci, wid_value)
    
    message(paste("Número de linhas após o filtro para", variable, ":", nrow(plot_data)))
    
    if (nrow(plot_data) == 0) {
        message(paste("Nenhum dado disponível para:", variable))
        return(NULL)
    }
    
    ggplot(plot_data, aes(x = avg_eci, y = wid_value, color = percentile_group, shape = percentile_group)) +
        geom_point(alpha = 0.3, size = 2) +  # Reduzi a opacidade e aumentei o tamanho dos pontos
        geom_smooth(method = "lm", se = FALSE, size = 1.75, aes(group = percentile_group), color = "black") +
        geom_smooth(method = "lm", se = FALSE, size = 1, aes(color = percentile_group)) +  
        labs(
            title = paste("ECI vs 50% da base e 40% do meio (post-tax, 2000)"),
            x = "Economic Complexity Index (avg_eci)",
            y = "Share de Renda",
            color = "Grupo Percentil",
            shape = "Grupo Percentil"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("red", "orange")) +  # Apenas duas cores
        scale_shape_manual(values = c(16, 17))  # Apenas duas formas para bottom e middle
}
plot_bottom_middle_shares(combined_data, "sptincj992")
plot_bottom_middle_shares(combined_data, "sdiincj992")



#### 2.1.4 Gráficos de gini ####

plot_correlation_gini <- function(data) {
    plot_data <- data %>%
        filter(
            wid_variable %in% gini_variables,
            decade == "2000"
        ) %>%
        drop_na(avg_eci, wid_value)
    
    ggplot(plot_data, aes(x = avg_eci, y = wid_value, color = wid_variable)) +
        geom_point(alpha = 0.4) +  # Pontos de dispersão
        geom_smooth(method = "lm", se = FALSE, size = 1.5, aes(color = wid_variable)) +  # Linha de regressão colorida
        labs(
            title = "ECI vs Gini (pre e post-tax, 2000",
            x = "Economic Complexity Index (avg_eci)",
            y = "Índice de Gini",
            color = "Gini Variable"
        ) +
        theme_minimal()
}
plot_correlation_gini(combined_data)




#### 2.1.5 Gráfico de Palma ####
plot_correlation_palma <- function(data) {
    plot_data <- data %>%
        filter(
            wid_variable %in% palma_variables,
            decade == "2000"
        ) %>%
        drop_na(avg_eci, wid_value) %>%
        mutate(wid_variable = case_when(
            wid_variable == "rptincj992" ~ "Razão de Palma",  # Nome mais descritivo
            TRUE ~ wid_variable
        ))
    
    ggplot(plot_data, aes(x = avg_eci, y = wid_value, color = wid_variable)) +
        geom_point(alpha = 0.4) +
        geom_smooth(method = "lm", se = FALSE, size = 1.75, aes(group = wid_variable), color = "black") +
        geom_smooth(method = "lm", se = FALSE, size = 1.5, aes(color = wid_variable)) +
        labs(
            title = "ECI vs Razão de Palma bruta (2000)",
            x = "Economic Complexity Index",
            y = "Razão de Palma",
            color = "Indicador"  # Nome da legenda
        ) +
        theme_minimal(base_family = "lmodern")
}
plot_correlation_palma(combined_data)



### ~ ####
## 2.2 REGRESSÃO BIVARIADA ####

### 2.2.1 Variáveis: log e outras ####

# Listar variáveis de interesse
decades <- c("1980", "1990", "2000")
share_variables <- c("sptincj992", "sdiincj992")
gini_variables <- c("gfiincj992", "gdiincj992") # Gini pré e pós-tax
palma_variables <- c("rptincj992") # Razão de Palma
all_variables <- c(share_variables, gini_variables, palma_variables)

# Criar log do PIB
combined_data <- combined_data %>%
    mutate(log_gdp = log(gdppp))

# Filtrar apenas as décadas selecionadas
combined_data <- combined_data %>%
    filter(decade %in% decades, wid_variable %in% all_variables)

### 2.2.2 Definição de Faixas de Renda ####

percentile_groups <- list(
    "Top 10%" = c(90, 100),
    "Top 1%" = c(99, 100),
    "Top 0.1%" = c(99.9, 100),
    "Top 0.01%" = c(99.99, 100),
    "Top 0.001%" = c(99.999, 100),
    "Bottom 50%" = c(0, 50),
    "Middle 40%" = c(50, 90)
)

# Criar grid de combinações entre faixas e décadas
expand_grid_test <- tibble(
    Percentil = names(percentile_groups),
    Lower = map_dbl(percentile_groups, 1),
    Upper = map_dbl(percentile_groups, 2)
) %>%
    distinct() %>% 
    crossing(tibble(Década = decades)) 

print(expand_grid_test, n = 21)

#### Teste de Filtragem ####

test_percentil <- "Top 1%"
test_decada <- "2000"

test_lower <- percentile_groups[[test_percentil]][1]
test_upper <- percentile_groups[[test_percentil]][2]

data_subset_test <- combined_data %>%
    filter(
        lower_percentile >= test_lower, 
        upper_percentile <= test_upper, 
        as.character(decade) == as.character(test_decada)
    )

print(paste("Observações encontradas para", test_percentil, "na década de", test_decada, ":", nrow(data_subset_test)))
print(head(data_subset_test))

### 2.2.3 Regressão ####

# Função para rodar regressões
run_bivariate_regression <- function(df, group_name, lower, upper, decade, variable) {
    data_subset <- df %>%
        filter(
            lower_percentile >= lower, 
            upper_percentile <= upper, 
            as.character(decade) == as.character(decade),
            wid_variable == variable
        )
    
    if (nrow(data_subset) > 10) {
        data_subset <- data_subset %>%
            filter(gdp_ppp > 0) %>%
            mutate(log_gdp = log(gdp_ppp))
        
        # Rodar as regressões
        model_eci <- lm(wid_value ~ avg_eci, data = data_subset)
        summary_eci <- summary(model_eci)
        confint_eci <- confint(model_eci)["avg_eci", ]
        
        return(tibble(
            Década = decade,
            Percentil = group_name,
            Variável = variable,  # <-- Adicionando a variável corretamente
            Num_Observacoes = nrow(data_subset),
            R2_ECI = summary_eci$r.squared,
            Beta_ECI = coef(model_eci)["avg_eci"],
            SE_ECI = summary_eci$coefficients["avg_eci", "Std. Error"],
            p_ECI = summary_eci$coefficients["avg_eci", "Pr(>|t|)"],
            CI_Lower_ECI = confint_eci[1],
            CI_Upper_ECI = confint_eci[2],
            F_Statistic_ECI = summary_eci$fstatistic[1]
        ))
    } else {
        return(NULL)
    }
}


# Aplicar regressões para todas as variáveis, incluindo Gini e Palma
expanded_results <- map2_df(percentile_groups, names(percentile_groups), function(bounds, name) {
    map_df(decades, function(decade) {
        map_df(all_variables, function(variable) {
            run_bivariate_regression(combined_data, name, bounds[1], bounds[2], decade, variable)
        })
    })
})

# Verificar se a coluna "Variável" está presente
colnames(expanded_results)

# Imprimir tabela completa
print(xtable(expanded_results, digits = 4), include.rownames = FALSE)

# Gerar tabelas separadas para Gini e Palma
expanded_results_gini_palma <- expanded_results %>%
    filter(Variável %in% c(gini_variables, palma_variables)) %>%
    select(Década, Percentil, Variável, Num_Observacoes, R2_ECI, Beta_ECI, SE_ECI, p_ECI, CI_Lower_ECI, CI_Upper_ECI, F_Statistic_ECI)

print(xtable(expanded_results_gini_palma, digits = 4), include.rownames = FALSE)


#### 2.2.4 Gráficos ####

##### Topo ####
# Filtros
percentis_desejados <- c(90, 99, 99.9, 99.99, 99.999)

data_2000 <- combined_data %>%
    filter(decade == "2000", wid_variable == "sptincj992", lower_percentile %in% percentis_desejados) %>%
    mutate(Percentil = factor(lower_percentile, levels = percentis_desejados))

# Rodar as regressões para cada percentil
regression_results <- data_2000 %>%
    group_by(Percentil) %>%
    do(tidy(lm(wid_value ~ avg_eci, data = .))) %>%
    filter(term == "avg_eci") %>%  # Pegando apenas os coeficientes de avg_eci
    select(Percentil, estimate, std.error, p.value)

# Gerar os valores ajustados das regressões
fitted_values <- data_2000 %>%
    group_by(Percentil) %>%
    do(augment(lm(wid_value ~ avg_eci, data = .))) %>%
    ungroup()




# Criar o gráfico com as **linhas ajustadas da regressão
plot_top_shares <- function(data, variable) {
    message(paste("Gerando gráfico para:", variable))
    
    plot_data <- data %>%
        filter(
            wid_variable == variable, 
            lower_percentile %in% c(90, 99, 99.9, 99.99, 99.999),
            decade == "2000"
        ) %>%
        drop_na(avg_eci, wid_value)
    
    message(paste("Número de observações após filtragem:", nrow(plot_data)))
    
    if (nrow(plot_data) == 0) {
        message("Nenhum dado disponível após a filtragem. Verifique os valores.")
        return(NULL)
    }
    
    # Rodar regressões para cada percentil
    regressions <- plot_data %>%
        nest_by(lower_percentile) %>%
        mutate(model = list(lm(wid_value ~ avg_eci, data = data))) %>%
        summarise(
            lower_percentile,
            R2 = summary(model)$r.squared,
            p_value = summary(model)$coefficients["avg_eci", "Pr(>|t|)"],
            .groups = "drop"
        )
    
    message("Regressões rodaram com sucesso!")
    
    # Criar nota para o gráfico
    note_text <- paste(
        "Nota:",
        paste0("P", regressions$lower_percentile, ": R² = ", round(regressions$R2, 2), 
            ", p < ", signif(regressions$p_value, 2)),
        collapse = "\n"
    )
    
    p <- ggplot(plot_data, aes(x = avg_eci, y = wid_value * 100, color = as.factor(lower_percentile))) +
        geom_point(alpha = 0.4) +  
        
        # Adiciona um contorno preto nas linhas de regressão
        geom_smooth(method = "lm", se = FALSE, size = 1.5, aes(group = lower_percentile), color = "black") +
        
        # Adiciona as linhas de regressão coloridas sobre o contorno preto
        geom_smooth(method = "lm", se = FALSE, size = 1, aes(color = as.factor(lower_percentile))) +
        
        labs(
            title = paste("ECI vs Share de Renda disponível (2000)"),
            x = "Economic Complexity Index (ECI)",
            y = "Share de Renda (%)",
            color = "Percentil"
        ) +
        theme_minimal(base_family = "lmodern") +
        theme(
            text = element_text(size = 13), 
            plot.title = element_text(size = 17, face = "bold"),
            axis.title = element_text(size = 15),
            legend.title = element_text(size = 13),
            legend.text = element_text(size = 11)
        )
    
    return(p)
}
plot_top_shares(combined_data, "sdiincj992")



##### Gini ####
plot_gini <- function(data, variable) {
    message(paste("Gerando gráfico para:", variable))
    
    plot_data <- data %>%
        filter(
            wid_variable == variable,
            decade == "2000"  # Apenas para o ano 2000
        ) %>%
        drop_na(avg_eci, wid_value)
    
    if (nrow(plot_data) == 0) {
        message(paste("Nenhum dado disponível para:", variable))
        return(NULL)
    }
    
    # Rodar regressão
    model <- lm(wid_value ~ avg_eci, data = plot_data)
    summary_model <- summary(model)
    
    # Obter estatísticas
    r2_value <- round(summary_model$r.squared, 2)
    p_value <- signif(summary_model$coefficients["avg_eci", "Pr(>|t|)"], 2)
    
    ggplot(plot_data, aes(x = avg_eci, y = wid_value)) +
        geom_point(alpha = 0.4, color = "blue") +  
        geom_smooth(method = "lm", se = FALSE, size = 1.75, color = "black") +
        geom_smooth(method = "lm", se = FALSE, size = 1, color = "blue") +  
        scale_y_continuous(breaks = seq(0.1, 0.9, by = 0.1)) +  # Escala do Gini de 0.1 em 0.1
        labs(
            title = paste("ECI vs Gini disponível (2000)"),
            x = "Economic Complexity Index (ECI)",
            y = "Gini",
            caption = paste0("R² = ", r2_value, ", p < ", p_value)  # Nota com R² e p-valor
        ) +
        theme_minimal(base_family = "lmodern") 
}

plot_gini(combined_data, "gptincj992")
plot_gini(combined_data, "gdiincj992")




##### Razão de Palma #####
plot_regression_palma <- function(data) {
    plot_data <- data %>%
        filter(
            wid_variable %in% palma_variables,
            decade == "2000"  # Filtrando apenas a década de 2000
        ) %>%
        drop_na(avg_eci, wid_value)
    
    ggplot(plot_data, aes(x = avg_eci, y = wid_value, color = wid_variable)) +
        geom_point(alpha = 0.4) +  # Pontos de dispersão
        geom_smooth(method = "lm", se = FALSE, size = 1.75, aes(group = wid_variable), color = "black") +  # Contorno preto
        geom_smooth(method = "lm", se = FALSE, size = 1.5, aes(color = wid_variable)) +  # Linha de regressão colorida
        labs(
            title = "ECI vs Razão de Palma bruta (2000)",
            x = "Economic Complexity Index (avg_eci)",
            y = "Razão de Palma",
            color = "Palma Variable"
        ) +
        theme_minimal(base_family = "lmodern")
}
print(plot_regression_palma(combined_data))



##### Bottom 50% e middle 50% ####
plot_bottom_middle_shares <- function(data, variable) {
    message(paste("Gerando gráfico para:", variable))
    
    plot_data <- data %>%
        filter(
            wid_variable == variable,
            decade == "2000"  # Filtrando apenas a década de 2000
        ) %>%
        mutate(percentile_group = case_when(
            lower_percentile == 0 & upper_percentile == 50 ~ "Bottom 50%",
            lower_percentile == 50 & upper_percentile == 90 ~ "Middle 40%",
            TRUE ~ NA_character_
        )) %>%
        drop_na(percentile_group, avg_eci, wid_value)
    
    message(paste("Número de linhas após o filtro para", variable, ":", nrow(plot_data)))
    
    if (nrow(plot_data) == 0) {
        message(paste("Nenhum dado disponível para:", variable))
        return(NULL)
    }
    
    ggplot(plot_data, aes(x = avg_eci, y = wid_value, color = percentile_group, shape = percentile_group)) +
        geom_point(alpha = 0.3, size = 2) +  # Reduzi a opacidade e aumentei o tamanho dos pontos
        geom_smooth(method = "lm", se = FALSE, size = 1.75, aes(group = percentile_group), color = "black") +
        geom_smooth(method = "lm", se = FALSE, size = 1, aes(color = percentile_group)) +  
        labs(
            title = paste("ECI vs 50% e 40% renda disponível (2000)"),
            x = "Economic Complexity Index (avg_eci)",
            y = "Share de Renda",
            color = "Grupo Percentil",
            shape = "Grupo Percentil"
        ) +
        theme_minimal(base_family = "lmodern") +  # Fonte LaTeX
        scale_color_manual(values = c("red", "orange")) +  # Apenas duas cores
        scale_shape_manual(values = c(16, 17))  # Apenas duas formas para bottom e middle
}
plot_bottom_middle_shares(combined_data, "sptincj992")
plot_bottom_middle_shares(combined_data, "sdiincj992")





### ~ ####
## 2.3 MODELO MULTIVARIADO ####


### 2.3.1 Gini ####

# Modelo com Gini Fiscal (gfiincj992) controlando pelas décadas e PIB per capita

gini_model_fiscal <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade),
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  # Controlar pelas décadas
                wid_variable == "gfiincj992"  # Selecionar a variável de Gini Fiscal
        )
)

# Resumo do modelo para Gini Fiscal
summary(gini_model_fiscal)



# Modelo com Gini Disponível (gdiincj992) controlando pelas décadas e PIB per capita
gini_model_disponivel <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade),
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  # Controlar pelas décadas
                wid_variable == "gdiincj992"  # Selecionar a variável de Gini Disponível
        )
)

# Resumo do modelo para Gini Disponível
stargazer(gini_model_fiscal, type = "latex", title = "Regressão do Índice de Gini",
    dep.var.labels = "Índice de Gini",
    covariate.labels = c("Intercepto", "ECI Médio", "PIB per capita", "Década: 1990", "Década: 2000"),
    omit.stat = c("ser", "f", "adj.rsq"),
    label = "tab:gini_regression")




### 2.3.2 Razão de Palma ####
palma_model <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade),
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  # Controlar pelas décadas
                wid_variable == "rptincj992"  # Selecionar a variável de Gini Disponível
        )
)

# Resumo do modelo
stargazer(palma_model, type = "latex", title = "Regressão da Razão de Palma",
    dep.var.labels = "Razão de Palma",
    covariate.labels = c("Intercepto", "ECI Médio", "PIB per capita", "Década: 1990", "Década: 2000"),
    omit.stat = c("ser", "f", "adj.rsq"),
    label = "tab:palma_regression")

### 2.3.3 Faixas superiores ####

#### 90% #### 
# Modelo para Shares Superiores (exemplo usando sptincj992 para o Top 10% de renda)
share_model_top10 <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade),  # A variável dependente é o share de renda
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  # Controlar pelas décadas
                wid_variable == "sptincj992" &  # Selecionar a variável de Share de Renda (Top 10%)
                lower_percentile == 90  # Selecionar o percentil de Top 10%
        )
)

# Resumo do modelo para Share de Renda Top 10%
stargazer(share_model_top10, type = "latex", title = "Regressão para o Top 10% da Renda",
    dep.var.labels = "Participação na Renda do Top 10%",
    covariate.labels = c("Intercepto", "ECI Médio", "PIB per capita", "Década: 1990", "Década: 2000"),
    omit.stat = c("ser", "f", "adj.rsq"),
    label = "tab:share_top10_regression")


#### 99% #### 
share_model_top1 <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade), 
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  
                wid_variable == "sptincj992" &  
                lower_percentile == 99  # Selecionar o percentil de Top 1%
        )
)

stargazer(share_model_top1, type = "latex", title = "Regressão para o Top 1% da Renda",
    dep.var.labels = "Participação na Renda do Top 1%",
    covariate.labels = c("Intercepto", "ECI Médio", "PIB per capita", "Década: 1990", "Década: 2000"),
    omit.stat = c("ser", "f", "adj.rsq"),
    label = "tab:share_top1_regression")


summary(share_model_top1)



#### 99.9% #### 
share_model_top01 <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade), 
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  
                wid_variable == "sptincj992" &  
                lower_percentile == 99.9  # Selecionar o percentil de Top 1%
        )
)

stargazer(share_model_top01, type = "latex", title = "Regressão para o Top 0,1% da Renda",
    dep.var.labels = "Participação na Renda do Top 0,1%",
    covariate.labels = c("Intercepto", "ECI Médio", "PIB per capita", "Década: 1990", "Década: 2000"),
    omit.stat = c("ser", "f", "adj.rsq"),
    label = "tab:share_top01_regression")



#### 99.9% #### 
share_model_top001 <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade), 
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  
                wid_variable == "sptincj992" &  
                lower_percentile == 99.09  # Selecionar o percentil de Top 1%
        )
)

summary(share_model_top001)



#### 99.99% #### 
share_model_top9999 <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade), 
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  
                wid_variable == "sptincj992" &  # Selecionar a variável de Share de Renda
                lower_percentile == 99.99  # Selecionar a faixa de 99.99%
        )
)

# Resumo do modelo para 99.99%
summary(share_model_top9999)


#### 99.999% #### 
share_model_top99999 <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade), 
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  
                wid_variable == "sptincj992" &  # Selecionar a variável de Share de Renda
                lower_percentile == 99.999  # Selecionar a faixa de 99.999%
        )
)

# Resumo do modelo para 99.999%
summary(share_model_top99999)


### 2.3.3 Faixas inferiores ####

# Bottom 50% (controlando por PIB per capita e décadas)
bottom_model <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade), 
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  
                lower_percentile == 0 & 
                upper_percentile == 50  # Faixa Bottom 50%
        )
)

stargazer(bottom_model, type = "latex", title = "Regressão para o Bottom 50% da Renda",
    dep.var.labels = "Participação na Renda do Bottom 50%",
    covariate.labels = c("Intercepto", "ECI Médio", "PIB per capita", "Década: 1990", "Década: 2000"),
    omit.stat = c("ser", "f", "adj.rsq"),
    label = "tab:share_b50_regression")


# Resumo do modelo para Bottom 50%
summary(bottom_model)

# Middle 40% (controlando por PIB per capita e décadas)
middle_model <- lm(
    wid_value ~ avg_eci + gdppp + factor(decade), 
    data = combined_data %>%
        filter(
            decade %in% c("1980", "1990", "2000") &  
                lower_percentile == 50 & 
                upper_percentile == 90  # Faixa Middle 40%
        )
)

# Resumo do modelo para Middle 40%
stargazer(middle_model, type = "latex", title = "Regressão para o Top 10% da Renda",
    dep.var.labels = "Participação na Renda do Middle 40%",
    covariate.labels = c("Intercepto", "ECI Médio", "PIB per capita", "Década: 1990", "Década: 2000"),
    omit.stat = c("ser", "f", "adj.rsq"),
    label = "tab:share_m40_regression")

glimpse(combined_data)


### ~ ####
## 2.4 POOLED REGRESSION ####
run_multivariate_regression <- function(df) {
    return(lm(wid_value ~ avg_eci + log_gdp, data = df))  # Removendo log_population
}

# com dummies de década
run_pooled_regression <- function(df) {
    lm(wid_value ~ avg_eci + log_gdp + factor(decade), data = df)  # Sem log_population
}

### ~ ####
## 2.5 MODELO DE EFEITOS FIXOS ####
run_fixed_effects <- function(df) {
    # Certificar-se de que há pelo menos dois países
    if (length(unique(df$country_id)) < 2) {
        return(NA)  # Retorna NA se houver apenas um país
    }
    
    df <- df %>%
        mutate(country_id = as.factor(country_id))
    
    return(feols(wid_value ~ avg_eci + log_gdp | country_id, data = df))  # Sem log_population
}


### ~ ####
## 2.6 LOOP PARA EXECUTAR ####

unique(combined_data$wid_variable)


all_results <- list()

for (variable in all_variables) {
    cat("\n--- Variável:", variable, "---\n")
    
    variable_results <- list()
    
    for (decade in decades) {
        cat("\n--- Década:", decade, "---\n")
        
        df_decade <- combined_data %>%
            filter(
                decade == as.character(decade),
                wid_variable == variable
            )
        
        # Se a variável tiver `lower_percentile`, aplica o filtro; caso contrário, ignora
        if ("lower_percentile" %in% names(df_decade)) {
            df_decade <- df_decade %>%
                filter(!is.na(lower_percentile))  # Remove NAs de `lower_percentile`
        }
        
        df_decade <- df_decade %>%
            drop_na(avg_eci, wid_value, log_gdp)
        
        if (nrow(df_decade) < 10) {
            cat("Poucos dados disponíveis para rodar o modelo nesta década (n <", nrow(df_decade), ")\n")
            next
        }
        
        # Rodar análises
        bivariate_results <- run_bivariate_regression(df_decade)
        clarke_result <- run_clarke_test(bivariate_results$ECI_Model, bivariate_results$GDP_Model)
        multivariate_result <- run_multivariate_regression(df_decade)
        pooled_result <- run_pooled_regression(df_decade)
        
        # Verificar se há dados suficientes para efeitos fixos
        if (length(unique(df_decade$country_id)) > 1) {
            fixed_effects_result <- run_fixed_effects(df_decade)
        } else {
            fixed_effects_result <- NULL
        }
        
        # Exibir os resultados no console para verificação
        cat("\n **Resultados para", variable, "na década", decade, "**\n")
        print(summary(bivariate_results$ECI_Model))
        print(summary(bivariate_results$GDP_Model))
        print(summary(multivariate_result))
        print(summary(pooled_result))
        
        if (!is.null(fixed_effects_result)) {
            print(summary(fixed_effects_result))
        }
        
        # Criar diretório específico para a variável
        var_output_dir <- file.path(output_dir, variable)
        dir.create(var_output_dir, showWarnings = FALSE)
        
        # Salvar os resultados em arquivos CSV
        write.csv(tidy(bivariate_results$ECI_Model), file.path(var_output_dir, paste0("bivariate_eci_", decade, ".csv")), row.names = FALSE)
        write.csv(tidy(bivariate_results$GDP_Model), file.path(var_output_dir, paste0("bivariate_gdp_", decade, ".csv")), row.names = FALSE)
        write.csv(tidy(multivariate_result), file.path(var_output_dir, paste0("multivariate_", decade, ".csv")), row.names = FALSE)
        write.csv(tidy(pooled_result), file.path(var_output_dir, paste0("pooled_", decade, ".csv")), row.names = FALSE)
        
        if (!is.null(fixed_effects_result)) {
            write.csv(tidy(fixed_effects_result), file.path(var_output_dir, paste0("fixed_effects_", decade, ".csv")), row.names = FALSE)
        }
        
        # Armazenar resultados
        variable_results[[paste("Bivariate", decade, sep = "_")]] <- bivariate_results
        variable_results[[paste("Clarke_Test", decade, sep = "_")]] <- clarke_result
        variable_results[[paste("Multivariate", decade, sep = "_")]] <- summary(multivariate_result)
        variable_results[[paste("Pooled", decade, sep = "_")]] <- summary(pooled_result)
        variable_results[[paste("Fixed_Effects", decade, sep = "_")]] <- if (!is.null(fixed_effects_result)) summary(fixed_effects_result) else NA
    }
    
    all_results[[variable]] <- variable_results
}


### ~ ####
## 2.7 VISUALIZAÇÕES ####
for (variable in all_variables) {
    cat("\n--- Variável:", variable, "---\n")
    
    for (decade in decades) {
        cat("\n--- Década:", decade, "---\n")
        
        df_decade <- combined_data %>%
            filter(decade == as.character(decade), wid_variable == variable) %>%
            drop_na(avg_eci, wid_value, log_gdp)
        
        if (nrow(df_decade) < 10) {
            cat("Poucos dados disponíveis para rodar o modelo nesta década (n <", nrow(df_decade), ")\n")
            next
        }
        
        # Rodar análises
        bivariate_results <- run_bivariate_regression(df_decade)
        clarke_result <- run_clarke_test(bivariate_results$ECI_Model, bivariate_results$GDP_Model)
        multivariate_result <- run_multivariate_regression(df_decade)
        pooled_result <- run_pooled_regression(df_decade)
        
        # Verificar se há dados suficientes para efeitos fixos
        fixed_effects_result <- if (length(unique(df_decade$country_id)) > 1) {
            run_fixed_effects(df_decade)
        } else {
            NULL
        }
        
        # Criar estrutura segura para acessar coeficientes
        extract_coeff <- function(model, variable) {
            if (!inherits(model, "lm") || is.null(coef(model)[variable])) {
                return(NA)  # Retorna NA se o modelo não for válido ou o coeficiente não existir
            } else {
                return(coef(model)[variable])
            }
        }
        
        # Criar lista segura para armazenar os coeficientes
        summary_results[[paste(variable, decade, sep = "_")]] <- data.frame(
            Variable = variable,
            Decade = decade,
            Bivariate_ECI_Coeff = extract_coeff(bivariate_results$ECI_Model, "avg_eci"),
            Bivariate_ECI_pval = if (!is.null(summary(bivariate_results$ECI_Model)$coefficients)) {
                summary(bivariate_results$ECI_Model)$coefficients["avg_eci", 4]
            } else { NA },
            Bivariate_GDP_Coeff = extract_coeff(bivariate_results$GDP_Model, "log_gdp"),
            Bivariate_GDP_pval = if (!is.null(summary(bivariate_results$GDP_Model)$coefficients)) {
                summary(bivariate_results$GDP_Model)$coefficients["log_gdp", 4]
            } else { NA },
            Multivariate_ECI_Coeff = extract_coeff(multivariate_result, "avg_eci"),
            Multivariate_ECI_pval = if (!is.null(summary(multivariate_result)$coefficients)) {
                summary(multivariate_result)$coefficients["avg_eci", 4]
            } else { NA },
            Multivariate_GDP_Coeff = extract_coeff(multivariate_result, "log_gdp"),
            Multivariate_GDP_pval = if (!is.null(summary(multivariate_result)$coefficients)) {
                summary(multivariate_result)$coefficients["log_gdp", 4]
            } else { NA },
            Pooled_ECI_Coeff = extract_coeff(pooled_result, "avg_eci"),
            Pooled_ECI_pval = if (!is.null(summary(pooled_result)$coefficients)) {
                summary(pooled_result)$coefficients["avg_eci", 4]
            } else { NA },
            Fixed_Effects_ECI_Coeff = if (!is.null(fixed_effects_result)) extract_coeff(fixed_effects_result, "avg_eci") else NA,
            Fixed_Effects_ECI_pval = if (!is.null(fixed_effects_result) && !is.null(summary(fixed_effects_result)$coefficients)) {
                summary(fixed_effects_result)$coefficients["avg_eci", 4]
            } else { NA }
        )
    }
}

# Converter lista de dfs em um único df
summary_results_df <- do.call(rbind, summary_results)

# Salvar o resumo consolidado em csv
summary_file <- file.path(output_dir, "summary_results.csv")
write.csv(summary_results_df, summary_file, row.names = FALSE)

print("odos os resultados foram salvos, incluindo um resumo consolidado!")


### ~ ####
## 2.8 CONSOLIDAR RESULTADOS #####

# Caminho para a pasta base
base_path <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/data/analyses/other-analysis"

# Listar todas as subpastas (variáveis)
variables <- list.dirs(base_path, recursive = FALSE)

# Inicializar lista para consolidar os resultados
all_results <- list()

# Loop por cada variável (pasta)
for (variable_path in variables) {
    variable_name <- basename(variable_path)  # Nome da variável
    
    # Listar os arquivos "fixed_effects_*.csv" dentro da pasta
    fixed_files <- list.files(variable_path, pattern = "fixed_effects_\\d{4}.csv", full.names = TRUE)
    
    # Loop por cada arquivo de efeitos fixos
    for (file in fixed_files) {
        # Extrair a década do nome do arquivo
        decade <- str_extract(basename(file), "\\d{4}")
        
        # Ler o arquivo
        data <- read_csv(file)
        
        # Adicionar colunas de variável e década
        data <- data %>%
            mutate(Variable = variable_name, Decade = as.integer(decade))
        
        # Consolidar
        all_results[[length(all_results) + 1]] <- data
    }
}

# Combinar todos os resultados em um único dataframe
final_data <- bind_rows(all_results)

# Salvar em um arquivo CSV consolidado
write_csv(final_data, file.path(base_path, "fixed_effects_consolidated.csv"))
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
### ~ ####
# FIM ####
### ~ ####
# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #








## 3. Subsets para diferentes medidas de desigualdade ####

### 3.1 Subset para Shares de Renda ####
share_data <- combined_data %>%
    filter(startsWith(wid_variable, "s"))  # Filtrar variáveis que começam com "s"

### 3.2 Subset para Gini ####
gini_data <- combined_data %>%
    filter(startsWith(wid_variable, "g"))  # Filtrar variáveis que começam com "g"

### 3.3 Subset para Razão de Palma ####
palma_data <- combined_data %>%
    filter(startsWith(wid_variable, "r"))  # Filtrar variáveis que começam com "r"
# =========== # =========== # =========== # =========== # =========== # =========== #




# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 4. Agregar por décadas ####




### 4.1 Adicionar coluna `decade` ####
combined_data <- combined_data %>%
    mutate(decade = case_when(
        year >= 1960 & year < 1970 ~ "1960",
        year >= 1970 & year < 1980 ~ "1970",
        year >= 1980 & year < 1990 ~ "1980",
        year >= 1990 & year < 2000 ~ "1990",
        year >= 2000 & year < 2010 ~ "2000",
        TRUE ~ "Other"
    )
    )

### 4.2 dataframe para cada década e cada tipo de desigualdade ####

# Lista das décadas
decades <- c("1960", "1970", "1980", "1990", "2000")

# Variáveis de desigualdade: Share, Gini, Palma
inequality_variables <- c("sptincj992", "sdiincj992", "gfiincj992", "gdiincj992", "rptincj992", "rptincj992", "sptinci992", "sdiinci992", "gfiinci992", "gdiinci992", "rptinci992", "rptinci992")


# Loop para realizar a análise para cada década e variável de desigualdade

# Inicializar uma lista para armazenar os resultados
regression_results <- list()

# Diretório para salvar os gráficos
output_dir <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/visualizations"


### 4.3 Share de renda ####

# Lista de variáveis de Share de Renda diretamente definidas
share_variables <- c("sptincj992", "sdiincj992", "sptinci992", "sdiinci992")

# Loop para Share de Renda
for (decade in decades) {
    print(paste("Processando Share de Renda para a década:", decade))
    
    # Filtrar os dados para a década atual e as variáveis corretas de Share de Renda
    share_data <- combined_data %>%
        filter(decade == decade, wid_variable %in% share_variables) %>%
        filter(!is.na(avg_eci) & !is.na(wid_value))  # Remover NAs
    
    for (variable in share_variables) {
        # Filtrar dados para a variável específica
        variable_data <- share_data %>%
            filter(wid_variable == variable)
        
        if (nrow(variable_data) > 0) {
            print(paste("Gerando gráfico para Share de Renda (", variable, ") na década:", decade))
            
            # Criar gráfico
            plot <- ggplot(variable_data, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
                geom_point(alpha = 0.6) +
                geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
                labs(
                    title = paste("ECI vs Share de Renda (", variable, ") (Década:", decade, ")"),
                    x = "Economic Complexity Index (avg_eci)",
                    y = "Share de Renda",
                    color = "Percentil"
                ) +
                theme_minimal()
            
            # Salvar gráfico
            ggsave(
                filename = file.path(output_dir, paste0("share_scatter_", decade, "_", variable, ".png")),
                plot = plot
            )
            
            # Regressão
            lm_model <- lm(wid_value ~ avg_eci, data = variable_data)
            regression_results[[paste("share", decade, variable, sep = "_")]] <- summary(lm_model)
            
            print(paste("Regressão concluída para Share de Renda (", variable, ") na década:", decade))
        } else {
            print(paste("Nenhum dado disponível para Share de Renda (", variable, ") na década:", decade))
        }
    }
}

# Salvar os resultados das regressões em um arquivo de texto
share_results_file <- file.path(output_dir, "share_regression_results.txt")
sink(share_results_file)
for (key in names(regression_results)) {
    cat("Resultado para:", key, "\n")
    print(regression_results[[key]])
    cat("\n----------------------\n")
}
sink()

print(paste("Resultados salvos em:", share_results_file))



### 4.4 Gini ####

# Variáveis do Gini
gini_variables <- c("gfiincj992", "gdiincj992", "gfiinci992", "gdiinci992")

# Diretório para salvar os gráficos
output_dir <- "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/visualizations"

# Inicializar uma lista para armazenar os resultados
gini_results <- list()

# Loop para realizar a análise para cada década e variável do Gini
for (decade in decades) {
    # Filtrar os dados para cada década
    decade_data <- combined_data %>%
        filter(decade == decade) %>%  # Certificar-se de que estamos filtrando corretamente por década
        filter(!is.na(avg_eci))  # Remover NAs em avg_eci para evitar problemas
    
    # Loop para cada variável do Gini
    for (variable in gini_variables) {
        # Filtrar os dados para a variável específica
        gini_data_decade <- decade_data %>%
            filter(wid_variable == variable)  # Filtrar apenas pela variável relevante
        
        if (nrow(gini_data_decade) > 0) {  # Verificar se há dados disponíveis após o filtro
            # Criar gráfico de dispersão
            plot <- ggplot(gini_data_decade, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
                geom_point(alpha = 0.6) +
                geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
                labs(
                    title = paste("ECI vs Gini (", variable, ") - Década:", decade),
                    x = "Economic Complexity Index (avg_eci)",
                    y = "Gini",
                    color = "Percentil"
                ) +
                theme_minimal()
            
            # Salvar o gráfico
            ggsave(
                filename = file.path(output_dir, paste0("gini_scatter_", decade, "_", variable, ".png")),
                plot = plot,
                width = 10, height = 7
            )
            
            # Regressão bivariada para ECI vs Gini
            lm_model_gini <- lm(wid_value ~ avg_eci, data = gini_data_decade)
            
            # Capturando o resumo da regressão (sumário)
            regression_summary <- summary(lm_model_gini)
            
            # Armazenando os resultados da regressão na lista
            gini_results[[paste(decade, variable, sep = "_")]] <- regression_summary
            
            # Exibindo a análise para o usuário
            print(paste("Análise para Década", decade, "- Variável", variable))
            print(regression_summary)
        } else {
            # Mensagem caso não existam dados para a combinação
            print(paste("Nenhum dado disponível para", variable, "na década:", decade))
        }
    }
}

# Salvar os resultados das regressões em um arquivo de texto
gini_results_file <- file.path(output_dir, "gini_regression_results.txt")
sink(gini_results_file)
for (key in names(gini_results)) {
    cat("Resultado para:", key, "\n")
    print(gini_results[[key]])
    cat("\n----------------------\n")
}
sink()

print(paste("Resultados salvos em:", gini_results_file))


### 4.5 Palma Ratio ####

# Lista de variáveis da Razão de Palma diretamente definidas
palma_variables <- c("rptincj992", "rptinci992")

# Loop para Razão de Palma
for (decade in decades) {
    print(paste("Processando Razão de Palma para a década:", decade))
    
    # Filtrar os dados para a década atual e as variáveis corretas da Razão de Palma
    palma_data <- combined_data %>%
        filter(decade == decade, wid_variable %in% palma_variables) %>%
        filter(!is.na(avg_eci) & !is.na(wid_value))  # Remover NAs
    
    for (variable in palma_variables) {
        # Filtrar dados para a variável específica
        variable_data <- palma_data %>%
            filter(wid_variable == variable)
        
        if (nrow(variable_data) > 0) {
            print(paste("Gerando gráfico para Razão de Palma (", variable, ") na década:", decade))
            
            # Criar gráfico
            plot <- ggplot(variable_data, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
                geom_point(alpha = 0.6) +
                geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
                labs(
                    title = paste("ECI vs Razão de Palma (", variable, ") (Década:", decade, ")"),
                    x = "Economic Complexity Index (avg_eci)",
                    y = "Razão de Palma",
                    color = "Percentil"
                ) +
                theme_minimal()
            
            # Salvar gráfico
            ggsave(
                filename = file.path(output_dir, paste0("palma_scatter_", decade, "_", variable, ".png")),
                plot = plot,
                width = 8,
                height = 6
            )
            
            # Regressão
            lm_model <- lm(wid_value ~ avg_eci, data = variable_data)
            regression_results[[paste("palma", decade, variable, sep = "_")]] <- summary(lm_model)
            
            print(paste("Regressão concluída para Razão de Palma (", variable, ") na década:", decade))
        } else {
            print(paste("Nenhum dado disponível para Razão de Palma (", variable, ") na década:", decade))
        }
    }
}

# Salvar os resultados das regressões em um arquivo de texto
palma_results_file <- file.path(output_dir, "palma_regression_results.txt")
sink(palma_results_file)
for (key in names(regression_results)) {
    if (grepl("^palma", key)) {
        cat("Resultado para:", key, "\n")
        print(regression_results[[key]])
        cat("\n----------------------\n")
    }
}
sink()

print(paste("Resultados salvos em:", palma_results_file))








# ANEXOS: falta de dados #### 

## Share: Discrepância entre i e j ####

# Lista de décadas
decades <- c("1960", "1970", "1980", "1990", "2000")

# Lista de variáveis de desigualdade
share_variables <- c("sptincj992", "sdiincj992", "sptinci992", "sdiinci992")

# Inicializar um data frame para armazenar os resultados
observation_counts <- data.frame(
    Decade = character(),
    Variable = character(),
    Observations = numeric(),
    stringsAsFactors = FALSE
)

# Loop para contar observações
for (decade in decades) {
    for (variable in share_variables) {
        # Filtrar os dados por década e variável
        filtered_data <- combined_data %>%
            filter(decade == decade, wid_variable == variable) %>%
            filter(!is.na(wid_value) & !is.na(avg_eci))  # Remover NAs
        
        # Adicionar o número de observações ao data frame
        observation_counts <- rbind(observation_counts, data.frame(
            Decade = decade,
            Variable = variable,
            Observations = nrow(filtered_data)
        ))
    }
}

# Exibir os resultados
print(observation_counts)

# Gráfico para visualizar a quantidade de observações
ggplot(observation_counts, aes(x = Decade, y = Observations, fill = Variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "n de observações por variável e década",
        x = "década",
        y = "n de observações",
        fill = "variável"
    ) +
    theme_minimal()


# Salvar o gráfico em um arquivo PNG
ggsave(
    filename = "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/writing/observations_per_variable_decade.png",
    plot = last_plot(),  # Assume que o gráfico exibido é o correto
    width = 10, height = 7
)


## Gini: discrepância e limites pras décadas 1960 e 1970 ####

# Lista de variáveis de Gini
gini_variables <- c("gfiincj992", "gdiincj992", "gfiinci992", "gdiinci992")

# Adicionar uma coluna indicando se é 'j' ou 'i' para análise
combined_data_gini <- combined_data %>%
    filter(wid_variable %in% gini_variables) %>%
    mutate(population_type = ifelse(grepl("j", wid_variable), "j", "i")) %>%
    filter(!is.na(wid_value))  # Remover NAs relevantes

# Contar número de observações por população (j ou i) e década
gini_counts <- combined_data_gini %>%
    group_by(decade, population_type) %>%
    summarise(num_observations = n(), .groups = "drop")

# Visualizar os dados
print(gini_counts)

# Gerar um gráfico para comparar os números de observações
ggplot(gini_counts, aes(x = decade, y = num_observations, fill = population_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "Número de Observações por População (j ou i) e Década",
        x = "Década",
        y = "Número de Observações",
        fill = "População"
    ) +
    theme_minimal()


## Palma Ratio ####

# Variáveis da Razão de Palma
palma_variables <- c("rptincj992", "rptinci992")

# Filtrar os dados apenas para as variáveis relevantes
palma_data <- combined_data %>%
    filter(wid_variable %in% palma_variables) %>%
    filter(!is.na(avg_eci) & !is.na(wid_value))  # Remover NAs

# Adicionar uma coluna para identificar a população (j ou i) com base no nome da variável
palma_data <- palma_data %>%
    mutate(population = ifelse(grepl("j", wid_variable), "j", "i"))

# Contar o número de observações por década e população (j ou i)
palma_counts <- palma_data %>%
    group_by(decade, population) %>%
    summarise(count = n(), .groups = "drop")

# Gerar o gráfico para visualizar as diferenças
ggplot(palma_counts, aes(x = decade, y = count, fill = population)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "Número de Observações por População (j ou i) e Década - Razão de Palma",
        x = "Década",
        y = "Número de Observações",
        fill = "População"
    ) +
    theme_minimal()

# Salvando
ggsave(
    filename = file.path(output_dir, "/Users/samuelmaia/Desktop/2024-2/economics of underdevelopment/paper/work/project-inequality-eci/writing/palma_population_observations.png"),
    plot = last_plot(),
    width = 8,
    height = 6
)


# Ver os dados resumidos
print(palma_counts)
# =========== # =========== # =========== # =========== # =========== # =========== #



# Lista de variáveis de Razão de Palma
palma_variables <- c("rptincj992", "rptinci992")

# Contagem inicial de observações (sem filtros, exceto variáveis de interesse)
initial_counts <- combined_data %>%
    filter(wid_variable %in% palma_variables) %>%
    group_by(decade, wid_variable) %>%
    summarise(n = n(), .groups = "drop")

# Contagem após os filtros aplicados para análise
filtered_counts <- combined_data %>%
    filter(wid_variable %in% palma_variables) %>%
    filter(!is.na(avg_eci) & !is.na(wid_value)) %>%
    group_by(decade, wid_variable) %>%
    summarise(n = n(), .groups = "drop")

# Unir os resultados para comparação
comparison_counts <- initial_counts %>%
    rename(initial_n = n) %>%
    left_join(filtered_counts, by = c("decade", "wid_variable")) %>%
    rename(filtered_n = n) %>%
    mutate(
        reduction = initial_n - filtered_n,
        reduction_percentage = round((reduction / initial_n) * 100, 2)
    )

# Visualizar o resultado
print(comparison_counts)

# Gráfico para visualizar a diferença
ggplot(comparison_counts, aes(x = decade, y = filtered_n, fill = wid_variable)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = paste0(reduction_percentage, "%")), vjust = -0.5, size = 3.5) +
    labs(
        title = "Comparação do Número de Observações (Antes e Depois dos Filtros)",
        x = "Década",
        y = "Número de Observações (Filtradas)",
        fill = "Variável"
    ) +
    theme_minimal()








# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #

# EXPLORATÓRIO #####
#### 2.1.1. sptinc (share pré-tax) ####

# população "j" (equal-split adults)
sptincj <- share_data %>%
    filter(wid_variable == "sptincj992")

# população "i" (individuals)
sptinci <- share_data %>%
    filter(wid_variable == "sptinci992")


# scatter plot: sptincj
ggplot(sptincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (sptincj992, j)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()


# scatter plot: sptinci
ggplot(sptinci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (sptinci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()



#### 2.1.2. sdiinc ####

# população "j" (equal-split adults)
sdiincj <- share_data %>%
    filter(wid_variable == "sdiincj992")

# população "i" (individuals)
sdiinci <- share_data %>%
    filter(wid_variable == "sdiinci992")


# scatter plot: sdiincj
ggplot(sdiincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (sdiincj992, j)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()


# scatter plot: sdiinci
ggplot(sdiinci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (sdiinci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()


#### 2.1.3. scainc ####
# população "j" (equal-split adults)
scaincj <- share_data %>%
    filter(wid_variable == "scaincj992")

# população "i" (individuals)
scainci <- share_data %>%
    filter(wid_variable == "scainci992")


# scatter plot: scaincj
ggplot(scaincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (scaincj992, j)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()


# scatter plot: scainci
ggplot(scainci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (scainci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()



### 2.2 Subset para Gini ####
gini_data <- combined_data %>%
    filter(startsWith(wid_variable, "g"))  # Filtrar variáveis que começam com "g"

# população "j" (equal-split adults)
gfiincj <- gini_data %>%
    filter(wid_variable == "gfiincj992")

# população "i" (individuals)
gfiinci <- share_data %>%
    filter(wid_variable == "gfiinci992")


#### 2.2.1. gfiinc ####
# scatter plot: gfiinci992 com i
ggplot(gfiinci992, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (gfiinci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Gini",
        color = "Percentil"
    ) +
    theme_minimal()

# scatter plot: gfiinci992 com j
ggplot(gfiincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (gfiincj992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Gini",
        color = "Percentil"
    ) +
    theme_minimal()



#### 2.2.2 gdiinc ####
# população "j" (equal-split adults)
gdiincj <- gini_data %>%
    filter(wid_variable == "gdiincj992")

# população "i" (individuals)
gdiinci <- share_data %>%
    filter(wid_variable == "gdiinci992")


# scatter plot: gdiinci992 com i
ggplot(gdiinci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (gdiinci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Gini",
        color = "Percentil"
    ) +
    theme_minimal()

# scatter plot: gdiincj992 com j
ggplot(gdiincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (gdiincj992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Gini",
        color = "Percentil"
    ) +
    theme_minimal()




### 2.3 Subset para Razão de Palma ####
palma_data <- combined_data %>%
    filter(startsWith(wid_variable, "r"))  # Filtrar variáveis que começam com "r"

# população "j" (equal-split adults)
rptincj <- palma_data %>%
    filter(wid_variable == "rptincj992")

# população "i" (individuals)
rptinci <- palma_data %>%
    filter(wid_variable == "rptinci992")


# scatter plot: rptincj992 com j
ggplot(rptincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (rptincj992, j)",  # Corrigir o título
        x = "Economic Complexity Index (avg_eci)",
        y = "Palma Ratio",
        color = "Percentil"
    ) +
    theme_minimal()

# scatter plot: rptinci992 com i
ggplot(rptinci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (rptinci992, i)",  # Corrigir o título
        x = "Economic Complexity Index (avg_eci)",
        y = "Palma Ratio",
        color = "Percentil"
    ) +
    theme_minimal()




## 3.SEM NAs ####

### 3.1. Shares ####
# população "j" (equal-split adults)
sptincj <- share_data %>%
    filter(wid_variable == "sptincj992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# população "i" (individuals)
sptinci <- share_data %>%
    filter(wid_variable == "sptinci992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# scatter plot: sptincj
ggplot(sptincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (sptincj992, j)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()

# scatter plot: sptinci
ggplot(sptinci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (sptinci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()


#### 3.1.2. sdiinc ####

# população "j" (equal-split adults)
sdiincj <- share_data %>%
    filter(wid_variable == "sdiincj992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# população "i" (individuals)
sdiinci <- share_data %>%
    filter(wid_variable == "sdiinci992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# scatter plot: sdiincj
ggplot(sdiincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (sdiincj992, j)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()

# scatter plot: sdiinci
ggplot(sdiinci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (sdiinci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()


#### 3.1.3. scainc ####
# população "j" (equal-split adults)
scaincj <- share_data %>%
    filter(wid_variable == "scaincj992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# população "i" (individuals)
scainci <- share_data %>%
    filter(wid_variable == "scainci992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# scatter plot: scaincj
ggplot(scaincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (scaincj992, j)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()

# scatter plot: scainci
ggplot(scainci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (scainci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Share de Renda",
        color = "Percentil"
    ) +
    theme_minimal()


### 3.2 Gini ####
# população "j" (equal-split adults)
gfiincj <- gini_data %>%
    filter(wid_variable == "gfiincj992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# população "i" (individuals)
gfiinci <- gini_data %>%
    filter(wid_variable == "gfiinci992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# scatter plot: gfiinci992 com i
ggplot(gfiinci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (gfiinci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Gini",
        color = "Percentil"
    ) +
    theme_minimal()

# scatter plot: gfiinci992 com j
ggplot(gfiincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (gfiincj992, j)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Gini",
        color = "Percentil"
    ) +
    theme_minimal()




# população "j" (equal-split adults)
gdiincj <- gini_data %>%
    filter(wid_variable == "gdiincj992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# população "i" (individuals)
gdiinci <- gini_data %>%
    filter(wid_variable == "gdiinci992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# scatter plot: gdiinci992 com i
ggplot(gdiinci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (gdiinci992, i)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Gini",
        color = "Percentil"
    ) +
    theme_minimal()

# scatter plot: gdiincj992 com j
ggplot(gdiincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (gdiincj992, j)",
        x = "Economic Complexity Index (avg_eci)",
        y = "Gini",
        color = "Percentil"
    ) +
    theme_minimal()




### 3.3 Palma Ratio ####

# população "j" (equal-split adults)
rptincj <- palma_data %>%
    filter(wid_variable == "rptincj992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))

# população "i" (individuals)
rptinci <- palma_data %>%
    filter(wid_variable == "rptinci992") %>%
    filter(!is.na(avg_eci), !is.na(wid_value))


# scatter plot: rptincj992 com j
ggplot(rptincj, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (rptincj992, j)",  # Corrigir o título
        x = "Economic Complexity Index (avg_eci)",
        y = "Palma Ratio",
        color = "Percentil"
    ) +
    theme_minimal()

# scatter plot: rptinci992 com i
ggplot(rptinci, aes(x = avg_eci, y = wid_value, color = factor(lower_percentile))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "ECI vs Share de Renda (rptinci992, i)",  # Corrigir o título
        x = "Economic Complexity Index (avg_eci)",
        y = "Palma Ratio",
        color = "Percentil"
    ) +
    theme_minimal()

## FIM DO EXPLORATÓRIO ####








# Criar médias por década (como no paper)
sitc_decadal <- sitc_filtered %>%
    mutate(decade = floor(year / 10) * 10) %>%
    group_by(country_id, decade) %>%
    summarise(
        avg_eci = mean(avg_eci, na.rm = TRUE),
        avg_gini = mean(population, na.rm = TRUE),  # Substituir com a variável GINI real
        .groups = "drop"
    )

# Verificar os dados agregados por década
glimpse(sitc_decadal)
# =========== # =========== # =========== # =========== # =========== # =========== #




# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 3. ANÁLISE BIVARIADA: GRÁFICOS DE DISPERSÃO ####
# =========== # =========== # =========== # =========== # =========== # =========== #

# Gráfico da relação entre ECI e GINI
ggplot(sitc_decadal, aes(x = avg_eci, y = avg_gini)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
        title = "Relação entre Complexidade Econômica e Desigualdade de Renda",
        x = "Índice de Complexidade Econômica (ECI)",
        y = "Coeficiente de GINI (Médio por Década)"
    ) +
    theme_minimal()

# Gráfico de dispersão por década
ggplot(sitc_decadal, aes(x = avg_eci, y = avg_gini, color = as.factor(decade))) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "Evolução da Relação entre Complexidade Econômica e Desigualdade",
        x = "Índice de Complexidade Econômica (ECI)",
        y = "GINI Médio por Década",
        color = "Década"
    ) +
    theme_minimal()
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 4. MATRIZ DE CORRELAÇÕES ####
# =========== # =========== # =========== # =========== # =========== # =========== #

# Criar matriz de correlações entre variáveis principais
GGally::ggpairs(sitc_decadal, columns = c("avg_eci", "avg_gini"))

# Comparar a correlação ECI-GINI com ECI-PIB per capita (quando disponível)
correlations <- sitc_decadal %>%
    summarise(
        cor_eci_gini = cor(avg_eci, avg_gini, use = "complete.obs")
    )

print(correlations)
# =========== # =========== # =========== # =========== # =========== # =========== #




# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 5. REGRESSÕES SIMPLES ####
# =========== # =========== # =========== # =========== # =========== # =========== #

# Rodar regressão básica (OLS) para testar a relação entre ECI e GINI
modelo_eci_gini <- lm(avg_gini ~ avg_eci, data = sitc_decadal)

# Exibir os resultados da regressão
summary(modelo_eci_gini)

# Formatar os resultados da regressão usando `broom`
tidy(modelo_eci_gini)
glance(modelo_eci_gini)
# =========== # =========== # =========== # =========== # =========== # =========== #



# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 6. MODELO DE PAINEL COM EFEITOS FIXOS ####
# =========== # =========== # =========== # =========== # =========== # =========== #

# Regressão com efeitos fixos por país
modelo_painel <- feols(avg_gini ~ avg_eci | country_id, data = sitc_decadal)

# Exibir resultados do modelo de painel
summary(modelo_painel)
# =========== # =========== # =========== # =========== # =========== # =========== #




# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
## 7. SALVAR RESULTADOS ####
# =========== # =========== # =========== # =========== # =========== # =========== #

# Salvar modelo de regressão
saveRDS(modelo_painel, "/caminho/para/resultados/modelo_painel.rds")

# Salvar a tabela de dados processados
write_csv(sitc_decadal, "/caminho/para/resultados/sitc_decadal.csv")


# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #


# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #
# FIM #####
# =========== # =========== # =========== # =========== # =========== # =========== #
# =========== # =========== # =========== # =========== # =========== # =========== #




