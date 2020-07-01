#!/bin/sh

# ./build.sh firefox && ./build.sh chrome
rm -f -r $1/_locales/
rm -f -r $1/img/
rm -f $1/popup.css
rm -f $1/popup.html
rm -f $1/popup.js

cp -r shared/ $1/

cd elm
elm make src/Main.elm --output=popup.js --optimize
cd ..
mv elm/popup.js $1/

echo Done!