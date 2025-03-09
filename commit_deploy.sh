#!/bin/bash

# Definição da branch principal
BRANCH_MAIN="main"

echo "Limpando arquivos temporários..."
git clean -f -X

echo "Adicionando mudanças ao commit..."
git add -A

echo "Digite a mensagem do commit:"
read commit_message

git commit -m "$commit_message"
git push origin $BRANCH_MAIN

echo "Deploy concluído com sucesso!"
