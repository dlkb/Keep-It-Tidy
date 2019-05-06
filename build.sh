#!/bin/sh

# ./build.sh firefox, ./build.sh chrome
cp -r shared/_locales $1/
cp -r shared/img $1/
cp shared/popup.css $1/
cp shared/popup.html $1/
cd elm
elm make src/Main.elm --output=popup.js --optimize
cd ..
mv elm/popup.js $1/
echo Done!