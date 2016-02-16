##  Makefile

IDRIS := idris
EXE   := frigg
OPTS  :=

.PHONY: clean build clobber check doc exe

exe:
	${IDRIS} ${OPTS} --build ${EXE}.ipkg

clean:
	${IDRIS} --clean ${EXE}.ipkg
	find . -name "*~" -delete

clobber: clean
	find . -name "*.ibc" -delete

check: clobber
	${IDRIS} --checkpkg ${EXE}.ipkg

doc:
	${IDRIS} --mkdoc ${EXE}.ipkg
