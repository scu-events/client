Production Build
```
elm make src/Main.elm --output build/elm.js --optimize
uglifyjs build/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=build/elm.min.js
scp -r ./build SERVING_LOCATION
```

Development
```
elm make src/Main.elm --output public/elm.js --debug
open public/index.html
```
