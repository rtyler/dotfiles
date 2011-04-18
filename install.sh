#!/bin/zsh


for f in $(cat files); do
    echo "Creating symbolic link for $f"
    ln -s `pwd`/$f ~/$f
done

for f in bin/*; do
    echo "Creating symbolic link for $f"
    ln -s `pwd`/$f ~/$f
done
