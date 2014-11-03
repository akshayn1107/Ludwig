# ludwig_lang_spec.pdf:
# 	pdflatex --jobname="ludwig_lang_spec" '\input{language_spec.tex}'
# 	pdflatex --jobname="ludwig_lang_spec" '\input{language_spec.tex}'

all:
	pdflatex --jobname="ludwig_lang_spec" '\input{language_spec.tex}'
	pdflatex --jobname="ludwig_lang_spec" '\input{language_spec.tex}'
	open ludwig_lang_spec.pdf

clearlog:
	rm -f *.aux *.log *.out *.toc

clearpdf:
	rm -f *.pdf

single:
	pdflatex --jobname="ludwig_single" '\newcommand{\compilesec}{${SECT}}\input{single_section.tex}'
	pdflatex --jobname="ludwig_single" '\newcommand{\compilesec}{${SECT}}\input{single_section.tex}'

clean: clearlog clearpdf
