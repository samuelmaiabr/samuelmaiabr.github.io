# Relatório de Ciência de Dados: Projeto “Complexidade e Desigualdade”

**Autor**: Samuel Maia  
**Afiliação**: Cedeplar (UFMG)

Este repositório contém os arquivos do relatório de ciência de dados e outros materiais do projeto **“Complexidade Econômica e Desigualdade de Renda: a vista do topo é diferente”** (de agora em diante, **Projeto**) (Maia, 2025). O projeto foi desenvolvido como parte dos requisitos da disciplina **Economia do Desenvolvimento** (2024/2) e integra também técnicas aprendidas no curso **Ciência de Dados Avançada** do mesmo semestre.

---

## 1. Visão Geral

Neste relatório, abordo a etapa de **exploração, limpeza e estruturação** dos dados utilizados no projeto. A fase de análise final e resultados, embora seja parte essencial do artigo submetido como trabalho, não está incluída integralmente aqui para evitar excesso de extensão.  

- **Seção 2**: Introduz o tema do artigo sobre complexidade econômica e desigualdade.  
- **Seção 3**: Detalha a execução (exploração e limpeza dos dados).  
- **Seção 4**: Discute as limitações encontradas e os aprendizados (“Cozinha de Pesquisa”).  
- **Seção 5**: Traz informações sobre repetibilidade e possíveis caminhos de replicação.

O artigo completo (com a análise estatística avançada e resultados) pode ser acessado [neste link](./samuel-maia-2025-CORRIGIDO-paper-economia-desenvolvimento.pdf).

---

## 2. Estrutura Principais do Repositório

A pasta possui vários arquivos, mas estes são os principais para quem quer entender ou reproduzir o relatório:

- **`index.html` / `maia-report-qmd-2025.html`**  
  Arquivos HTML que apresentam o relatório renderizado. Abra em seu navegador para ler a versão final do relatório.

- **`maia-report-qmd-2025.qmd` / `maia-report-2025.Rmd`**  
  Arquivos-fonte do relatório (Quarto ou R Markdown).

- **`analysis-proper.R`**  
  Script R contendo as análises e manipulações de dados mostradas no **Projeto**.

- **`repetibilidade.qmd` / `repetibilidade.html`**  
  Apêndice do relatório que discute como repetir computacionalmente o trabalho (incluindo ambiente, pacotes e passos de instalação).

- **`report_data/`**  
  Pasta contendo arquivos de referência, logs de sessão e metadados sobre pacotes (por exemplo, `installed_packagess_cleaning.csv`, `session_infos_cleaning.txt`).

- **Imagens e Gráficos**  
  Vários arquivos `.png`, `.pdf` e `.tex` que contêm visualizações geradas durante a fase de exploração. Exemplos: `plot_foice.png`, `plot_topshare.png`, `10exporters.png`.

- **`deploy-report-data-science.sh`**  
  (Opcional) Script que auxilia na publicação ou atualização do relatório, se você estiver usando GitHub Pages ou outro método de deploy automatizado.

- **`samuel-maia-2025-CORRIGIDO-paper-economia-desenvolvimento.pdf`**  
  Versão final do artigo submetido como trabalho na disciplina **Economia do Desenvolvimento**.

Para ver a lista completa de arquivos, consulte a árvore de diretórios (`tree`) fornecida no repositório.

---

## 3. Como Executar e Reproduzir

1. **Clonar o repositório**  
   ```bash
   git clone https://github.com/seu-usuario/report-data-science.git
   cd report-data-science
