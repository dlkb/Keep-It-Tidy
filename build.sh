#!/bin/sh

# ./build.sh firefox, ./build.sh chrome
cp -r _locales $1/
cp -r img $1/
cp popup.css $1/
cd elm
elm make src/Main.elm --output=popup.js --optimize
cd ..
mv elm/popup.js $1/
echo Done!