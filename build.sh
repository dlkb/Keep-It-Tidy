#!/bin/sh

# ./build.sh firefox && ./build.sh chrome
rm -f -r $1/_locales/
rm -f -r $1/img/
rm -f $1/popup.css
rm -f $1/popup.html
rm -f $1/background.js
rm -f $1/glue.js
rm -f $1/popup.js

cp -r shared/ $1/

# Replace "browser" with "chrome"
if [ $1 == "chrome" ]; then
    sed -i "" -e "s/browser/chrome/g" $1/background.js
    sed -i "" -e "s/browser/chrome/g" $1/glue.js
fi

cd elm
elm make src/Main.elm --output=popup.js --optimize
cd ..
mv elm/popup.js $1/

echo Done!