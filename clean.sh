#!/bin/sh

# ./clean.sh firefox, ./clean.sh chrome
rm -r $1/_locales/
rm -r $1/img/
rm $1/popup.css
rm $1/popup.js
echo Done!