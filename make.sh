#!/bin/sh

rm -r ./dist
mkdir ./dist

evm use 0.17.1
elm-make src/Main.elm --output=dist/app.js
cp src/main.html dist/index.html
cp src/SoundPlayer.js dist/SoundPlayer.js
cp src/web-audio-api-shim.js dist/web-audio-api-shim.js
