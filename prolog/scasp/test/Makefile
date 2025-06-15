# SCASP defines the command to run s(CASP) using SWI-Prolog with the main
# library loaded.
SCASP ?= swipl -g "[library(scasp/main)]" --

help: ; $(SCASP) --help
.PHONY: help

%.json: %.pl ; $(SCASP) -s0 --tree --json=$@ $<

swipl-win: ; swipl-win --no-packs -g "attach_packs(../../../..)" &
.PHONY: swipl-win

PUML = $(wildcard *.puml)
DOT = $(wildcard *.dot)

dot: $(DOT:.dot=.svg) $(DOT:.dot=.eps) $(DOT:.dot=.png)

%.svg: %.dot ; dot -Tsvg $< -o $@

%.eps: %.dot ; dot -Teps $< -o $@

%.png: %.dot ; dot -Tpng $< -o $@
