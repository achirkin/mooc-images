#/bin/sh

stack clean
stack build mooc-images --ghc-options=-O2 --no-library-profiling --no-executable-profiling --no-haddock
cp `stack path --dist-dir`/build/mooc-images/mooc-images .

#cabal clean
#cabal configure --enable-optimization=2 --disable-profiling --disable-library-profiling
#cabal build
#cp dist/build/mooc-images/mooc-images .

tar czpf mooc-images.keter mooc-images static config/keter.yaml
rm mooc-images
scp mooc-images.keter achirkin@mooc.ia.arch.ethz.ch:/opt/keter/incoming
