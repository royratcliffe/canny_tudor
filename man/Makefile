.PHONY: doc.tex
doc.tex:
	swipl --no-packs -g "attach_packs(../..)" -s doc.pl -t halt

canny_tudor.tex: doc.tex

# quarto pandoc --list-extensions=gfm
# +alerts
# -ascii_identifiers
# -attributes
# +autolink_bare_uris
# -bracketed_spans
# -definition_lists
# -east_asian_line_breaks
# +emoji
# -fancy_lists
# -fenced_divs
# +footnotes
# +gfm_auto_identifiers
# -hard_line_breaks
# -implicit_figures
# -implicit_header_references
# +pipe_tables
# -raw_attribute
# +raw_html
# -rebase_relative_paths
# -smart
# -sourcepos
# +strikeout
# -subscript
# -superscript
# +task_lists
# +tex_math_dollars
# +tex_math_gfm
# -wikilinks_title_after_pipe
# -wikilinks_title_before_pipe
# +yaml_metadata_block
../README.md: canny_tudor.tex
	quarto pandoc --from latex --to gfm canny_tudor.tex --output $@

update-tinytex:
	quarto tools update tinytex
