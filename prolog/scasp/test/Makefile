# SCASP defines the command to run s(CASP) using SWI-Prolog with the main
# library loaded.
SCASP ?= swipl -g "[library(scasp/main)]" --

help: ; $(SCASP) --help
.PHONY: help

%.json: %.pl ; $(SCASP) -s0 --tree --json=$@ $<

%.dot: %.json ; swipl -s ../just_dot_main.pl -- $<

swipl-win: ; swipl-win --no-packs -g "attach_packs(../../../..)" &
.PHONY: swipl-win

ifeq ($OS,Windows_NT)
EXE := .exe
else
EXE :=
endif

scasp-just-dot$(EXE): ../just_dot_main.pl
	swipl -o scasp-just-dot -c ../just_dot_main.pl

PUML = $(wildcard *.puml)
DOT = $(wildcard *.dot)

dot: $(DOT:.dot=.svg) $(DOT:.dot=.eps) $(DOT:.dot=.png)

%.svg: %.dot ; dot -Tsvg $< -o $@

%.eps: %.dot ; dot -Teps $< -o $@

%.png: %.dot ; dot -Tpng $< -o $@
