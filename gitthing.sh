#!/bin/sh

git filter-branch --env-filter '
OLD_EMAIL="aidawork@Aidas-MacBook-Pro.local"
CORRECT_NAME="Aida El Kouri"
CORRECT_EMAIL="aelkouri@email.sc.edu"
if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_COMMITTER_NAME="$CORRECT_NAME"
    export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
fi
if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_AUTHOR_NAME="$CORRECT_NAME"
    export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
fi
' --tag-name-filter cat -- --branches --tags
