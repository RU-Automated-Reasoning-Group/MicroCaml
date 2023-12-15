SOURCES = microCamlTypes.ml tokenTypes.ml lexer.mli lexer.ml utils.ml parser.mli parser.ml infer.ml main.ml

all: frontend

clean:
	rm -f frontend
	for X in .; do \
      for Y in cmo cmi output; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

frontend: $(SOURCES)
	ocamlc -o frontend -g -I +str str.cma $(SOURCES)