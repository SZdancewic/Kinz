OCBFLAGS := -use-ocamlfind -pkg imagelib
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top profile

all: main.native
debug: all main.cma

%.cma: .FORCE
	$(OCB) $@

%.cmxa: .FORCE
	$(OCB) $@

%.native: .FORCE
	$(OCB) $@

%.byte: .FORCE
	$(OCB) $@

.FORCE:

clean:
	$(OCB) -clean

top: main.cma
	utop

profile: main.p.native
	instruments -t "/Applications/Xcode.app/Contents/Applications/Instruments.app/Contents/Resources/templates/Time Profiler.tracetemplate" main.p.native
