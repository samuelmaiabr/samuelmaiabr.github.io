#!/bin/bash

# Renderiza o Quarto `.qmd`
quarto render maia-report-qmd-2025.qmd

# Move os arquivos gerados para o repositório do GitHub Pages
mv maia-report-qmd-2025.html samuelmaiabr.github.io/index.html
mv -r maia-report-qmd-2025_files samuelmaiabr.github.io/

# Commit e push automático
cd samuelmaiabr.github.io
git add .
git commit -m "Atualizando site com novo relatório"
git push origin main

echo "✅ Site atualizado com sucesso!"
