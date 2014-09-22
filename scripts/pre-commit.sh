#!/bin/bash

# https://gist.github.com/alexbevi/3436040
#
# Git pre-commit hook to check all staged Ruby (*.rb/haml/coffee) files 
# for Pry binding references
#
# Installation
#
#   ln -s /path/to/pre-commit.sh /path/to/project/.git/hooks/pre-commit
#
# Based on 
#
#   http://codeinthehole.com/writing/tips-for-using-a-git-pre-commit-hook/
#   http://mark-story.com/posts/view/using-git-commit-hooks-to-prevent-stupid-mistakes
#   https://gist.github.com/3266940
#
FILES_PATTERN='\.(rb|haml|coffee)(\..+)?$'
FORBIDDEN='binding.pry'

git diff --cached --name-only | \
    grep -E $FILES_PATTERN | \
    GREP_COLOR='4;5;37;41' xargs grep --color --with-filename -n $FORBIDDEN && \
    echo 'COMMIT REJECTED' && \
    exit 1

exit 0
