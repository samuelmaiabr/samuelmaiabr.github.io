#!/bin/bash

# Removendo arquivos temporários do LaTeX
echo "Limpando arquivos temporários..."
git clean -f -X

# Exibir status do repositório antes do commit
echo "Status do repositório:"
git status

# Pedir a mensagem do commit
echo "Digite a mensagem do commit:"
read commit_message

# Adicionar todas as mudanças
git add .

# Criar commit
git commit -m "$commit_message"

# Fazer push para o repositório remoto
echo "Enviando pro GitHub..."
git push origin main  # Altere 'main' para 'master' se necessário

echo "Commit realizado e enviado com sucesso"
