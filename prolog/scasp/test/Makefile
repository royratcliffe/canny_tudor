# SCASP defines the command to run s(CASP) using SWI-Prolog with the main
# library loaded.
SCASP ?= swipl -g "[library(scasp/main)]" --

help: ; $(SCASP) --help
.PHONY: help

%.json: %.pl ; $(SCASP) -s0 --tree --json=$@ $<
