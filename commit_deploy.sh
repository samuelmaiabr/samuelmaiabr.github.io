#!/bin/bash

# Configurações básicas
BRANCH_DEPLOY="gh-pages"
BRANCH_MAIN="main"

echo "Limpando arquivos temporários..."
git clean -f -X

echo "Adicionando mudanças ao commit..."
git add -A

echo "Digite a mensagem do commit:"
read commit_message

git commit -m "$commit_message"
git push origin $BRANCH_MAIN

echo "Iniciando deploy para GitHub Pages..."

# Verifica se a branch de deploy existe, se não, cria
if ! git show-ref --verify --quiet refs/heads/$BRANCH_DEPLOY; then
    echo "Branch $BRANCH_DEPLOY não existe. Criando..."
    git branch $BRANCH_DEPLOY
    git push origin $BRANCH_DEPLOY
fi

# Salva mudanças temporariamente caso haja algo pendente
git stash

# Muda para a branch de deploy
git checkout $BRANCH_DEPLOY

# Atualiza a branch com a versão da main
git reset --hard $BRANCH_MAIN
git push origin $BRANCH_DEPLOY --force

# Volta para a branch principal e recupera mudanças pendentes
git checkout $BRANCH_MAIN
git stash pop || echo "Nenhuma mudança pendente para restaurar."

echo "Deploy concluído com sucesso!"
