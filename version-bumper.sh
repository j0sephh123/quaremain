#!/bin/sh

# Bump and sync versions for Quaremain software
# Example: ./version-bumpber.sh 5.4.3

CURRENT_VERSION=$1
README_DIST=./dist-data/README
README_ROOT=./README.md
README_REP="s/^[0-9]\.[0-9]\.[0-9]$/${CURRENT_VERSION}/"
ASDF_REP="s/[0-9]\.[0-9]\.[0-9]\"$/${CURRENT_VERSION}\"/"
ASDF_ROOT=./quaremain.asd
ABOUT_REP="s/[0-9]\.[0-9]\.[0-9]/${CURRENT_VERSION}/"
ABOUT_ROOT=./templates/about.html

sed -i $README_REP $README_DIST
sed -i $README_REP $README_ROOT
sed -i $ASDF_REP $ASDF_ROOT
sed $ABOUT_REP $ABOUT_ROOT
