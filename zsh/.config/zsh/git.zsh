# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    git.zsh                                            :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2017/01/26 19:23:22 by mplanell          #+#    #+#              #
#    Updated: 2017/01/26 19:33:10 by mplanell         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# Git
alias g='git'
alias upl='git add -all && git commit && git push'

# Branches
alias gb='git branch'
alias gbc='git checkout -b'

# Commit
alias gc='git commit --verbose'
alias gca='git commit --verbose --all'
alias gcm='git commit --message'
alias gco='git checkout'
alias gcf='git commit --ammend --reuse-message HEAD'
alias gcr='git revert'
alias gcR='git reset "HEAD^"'
alias gcs='git show'

# Data
alias gd='git ls-files'
alias gdc='git ls-files --cached'
alias gdx='git ls-files --deleted'
alias gdm='git ls-files --modified'

# Fetch
alias gf='git fetch'
alias gfc='git clone'
alias gfm='git pull'
alias gfr='git pull --rebase'

# Grep
alias gg='git grep'
alias ggi='git grep --ignore-case'

# Merge
alias gm='git merge'
alias gmC='git merge --no-commit'

# Push
alias gp='git push'
alias gpf='git push --force'
alias gpa='git push --all'
alias gpc='git push --set-upstream origin "$(git-branch-current 2> /dev/null)"'

# Rebase
alias gr='git rebase'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias gri='git rebase --interactive'
alias grs='git rebase --skip'

# Remote
alias gR='git remote'
alias gRl='git remote --verbose'
alias gRa='git remote add'
alias gRx='git remote rm'
alias gRm='git remote rename'
alias gRu='git remote update'

# Stash
alias gs='git stash'

# Submodule
alias gS='git submodule'
alias gSa='git submodule add'
alias gSf='git submodule foreach'
alias gSi='git submodule init'
alias gSI='git submodule update --init --recursive'
alias gSl='git submodule status'
alias gSm='git-submodule-move'
alias gSs='git submodule sync'
alias gSu='git submodule foreach git pull origin master'
alias gSx='git-submodule-remove'

# Working Copy
alias gws='git status --short'
alias gwS='git status'
alias gwd='git diff --no-ext-diff'
alias gwD='git diff --no-ext-diff --word-diff'
alias gwr='git reset --soft'
alias gwR='git reset --hard'
alias gwc='git clean -n'
alias gwC='git clean -df'
alias gwx='git rm -r'
alias gwX='git rm -rf'
