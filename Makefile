install-prof:
	cabal-dev --extra-config-file=cabal-dev.config install --force-reinstalls
build-prof:
	cabal-dev build --ghc-options="-osuf p_o -prof -auto-all -caf-all"
	echo "run program with +RTS -h"
