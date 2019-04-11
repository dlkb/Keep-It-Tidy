#!/bin/sh

# ./build.sh firefox, ./build.sh chrome
cp -r _locales/ $1/_locales/
cp -r img $1/img/
cp popup.css $1/popup.css
cd elm
elm make src/Main.elm --output=popup.js --optimize
cd ..
mv elm/popup.js $1/
echo Done!