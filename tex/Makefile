#----------------------------------------------------
# DOCUMENT OPTIONS
#----------------------------------------------------
MASTER=fortran-guidelines
USE_BIBTEX=0
USE_MAKEINDEX=0
USE_MAKEGLOSS=0
USE_PSTRICKS=0
WINDOWS=0
#----------------------------------------------------
# PATHS
#----------------------------------------------------
FIGSDIR=figs
FIGSDUMPDIR=figsdump
SVGDIR=svg
SVGTEXDIR=svgtex
TKZDIR=figstkz
TEXOUTDIR=figs
BIBDIR=bib
SRCDIR=.
OUTDIR=out
#----------------------------------------------------
# BINARIES
#----------------------------------------------------
LATEX	 = latex
BIBTEX	 = bibtex
DVIPS	 =dvips
PDFLATEX =pdflatex
MAKEINDEX= makeindex
LATEXHTML= latex2html
PS4PDF= ps4pdf


CONVERT=convert
LN=ln
RM=rm -f
CP=cp
PDFTOEPS=pdftops -eps
EPSTOPDF=epstopdf
SVGTOLATEX=svg2latex.py   # OLD METHOD
SVGTOLATEX=inkscape -z -D 
SVGTOPDF=inkscape -D -A
#SVGTEXTOPDF=inkscape -z -D --file=A.svg --export-pdf=A.pdf --export-latex
SVGTOEPS=inkscape -D -E
SLASH="/"
ifeq ($(WINDOWS),1)
	CONVERT=C:/bin/ImageMagick/convert.exe
	LN=copy
	RM=del
	CP=copy
	PDFTOEPS=pdftops -eps
	EPSTOPDF=epstopdf
	SVGTOLATEX=svg2latex.py
	SVGTOLATEX=C:/bin/InkscapePortable/App/Inkscape/inkscape.exe -z -D 
	SVGTOPDF=C:/bin/InkscapePortable/App/Inkscape/inkscape.exe -D -A
	SVGTOEPS=C:/bin/InkscapePortable/App/Inkscape/inkscape.exe -D -E
	SLASH="/"
endif

#----------------------------------------------------
# FLAGS
#----------------------------------------------------
TEX_FLAGS	=  --file-line-error --interaction=nonstopmode #--output-directory out --jobname NewReport
BIB_FLAGS	=
HTML_FLAGS	=
PDFLATEX_FLAGS	= -synctex=1 --file-line-error-style --shell-escape --interaction=nonstopmode #--output-directory out --jobname NewReport
PS2PDF_FLAGS    = -dMaxSubsetPct=100 -dCompatibilityLevel=1.2 -dSubsetFonts=true -dEmbedAllFonts=true
MAKEINDEX_FLAGS = -s pre/index_style.ist
MAKEGLOSS_FLAGS = -s pre/glossaire_style.ist
DVIPS_FLAGS	= -Ppdf -G0 

#----------------------------------------------------
# SRC FILES
#----------------------------------------------------
FIGS=$(notdir $(wildcard $(FIGSDUMPDIR)/*))
FIGSRC=$(patsubst %,$(FIGSDUMPDIR)/%,$(FIGS))

TEXSRC	=  $(wildcard $(SRCDIR)/pre/*.tex) \
	  $(wildcard $(SRCDIR)/tex/*.tex) 

BIBSRC	= $(wildcard $(BIBDIR)/*.bib)
# Using Bibtex
ifneq ($(strip $(BIBSRC)),)
BBLSRC	= $(MASTER).bbl
endif


#----------------------------------------------------
# Display Codes (this is so we can track passes)
#----------------------------------------------------
SETCOLOR_BLACK	= @echo "\\033[0;30m"
SETCOLOR_RED 	= @echo "\\033[0;31m"
SETCOLOR_GREEN 	= @echo "\\033[0;32m"
SETCOLOR_BLUE	= @echo "\\033[0;34m"





#----------------------------------------------------
# PNG
#-----------------------------------------------------
PNGS=$(notdir $(wildcard $(FIGSDUMPDIR)/*.png))
PNGS2PNGS=$(patsubst %,$(FIGSDIR)/%,$(PNGS:.png=.png)) # a choice...
PNGS2EPSS=$(patsubst %,$(FIGSDIR)/%,$(PNGS:.png=.eps))
PNGS2PDFS=$(patsubst %,$(FIGSDIR)/%,$(PNGS:.png=.pdf))
#-----------------------------------------------------
# JPG
#-----------------------------------------------------
JPGS=$(notdir $(wildcard $(FIGSDUMPDIR)/*.jpg))
JPGS2EPSS=$(patsubst %,$(FIGSDIR)/%,$(JPGS:.jpg=.eps))
JPGS2PDFS=$(patsubst %,$(FIGSDIR)/%,$(JPGS:.jpg=.pdf))
#-----------------------------------------------------
# EPS
#-----------------------------------------------------
EPSS=$(notdir $(wildcard $(FIGSDUMPDIR)/*.eps))
EPSS2EPSS=$(patsubst %,$(FIGSDIR)/%,$(EPSS:.eps=.eps))
EPSS2PDFS=$(patsubst %,$(FIGSDIR)/%,$(EPSS:.eps=.pdf))
#-----------------------------------------------------
# PDF
#-----------------------------------------------------
PDFS=$(notdir $(wildcard $(FIGSDUMPDIR)/*.pdf))
PDFS2EPSS=$(patsubst %,$(FIGSDIR)/%,$(PDFS:.pdf=.eps))
PDFS2PDFS=$(patsubst %,$(FIGSDIR)/%,$(PDFS:.pdf=.pdf))
#-----------------------------------------------------
# SVGTEX
#-----------------------------------------------------
SVGTEXS=$(notdir $(wildcard $(SVGTEXDIR)/*.svg))
SVGTEXS2PDFS=$(patsubst %,$(TEXOUTDIR)/%,$(SVGTEXS:.svg=.pdf))
SVGTEX2SEPSS=$(patsubst %,$(TEXOUTDIR)/%,$(SVGTEXS:.svg=.eps))
#-----------------------------------------------------
# SVG
#-----------------------------------------------------
SVGS=$(notdir $(wildcard $(SVGDIR)/*.svg))
SVGS2PDFS=$(patsubst %,$(FIGSDIR)/%,$(SVGS:.svg=.pdf))
SVGS2SEPSS=$(patsubst %,$(FIGSDIR)/%,$(SVGS:.svg=.eps))
#-----------------------------------------------------

# TKZ
#-----------------------------------------------------
TKZS=$(notdir $(wildcard $(TKZDIR)/*.tkz))
TKZ2PDFS=$(patsubst %,$(FIGSDIR)/%, $(TKZS:.tkz=.pdf))
TKZ2SEPSS=$(patsubst %,$(FIGSDIR)/%,$(TKZS:.tkz=.eps))
#-----------------------------------------------------






#-----------------------------------------------------
# If chosen to keep pngs..
#-----------------------------------------------------
$(FIGSDIR)/%.png : $(FIGSDUMPDIR)/%.png
	$(LN) "$<" "$@"
#-----------------------------------------------------
# CONVERSION TO EPS
#-----------------------------------------------------
$(FIGSDIR)/%.eps : $(FIGSDUMPDIR)/%.png
	$(CONVERT) "$<"  EPS3:"$@"

$(FIGSDIR)/%.eps : $(FIGSDUMPDIR)/%.jpg
	$(CONVERT) "$<"  EPS3:"$@"

$(FIGSDIR)/%.eps : $(FIGSDUMPDIR)/%.pdf
	$(PDFTOEPS) "$<" "$@"

$(FIGSDIR)/%.eps : $(FIGSDUMPDIR)/%.eps
	$(LN) "$<" "$@"
	
$(FIGSDIR)/%.eps: $(SVGTEXDIR)/%.svg
	$(SVGTOLATEX) -f "$<" -l "$(FIGSDIR)/$*.tex" -e -o

$(FIGSDIR)/%.eps: $(SVGDIR)/%.svg
	$(SVGTOEPS) "$@" "$<"
	
#-----------------------------------------------------
# CONVERSION TO PDF
#-----------------------------------------------------

$(FIGSDIR)/%.pdf : $(FIGSDUMPDIR)/%.png
	$(CONVERT) "$<" "$@"

$(FIGSDIR)/%.pdf : $(FIGSDUMPDIR)/%.jpg
	$(CONVERT) "$<" "$@"

$(FIGSDIR)/%.pdf : $(FIGSDUMPDIR)/%.eps
	$(EPSTOPDF)  "$<" --outfile="$@"

$(FIGSDIR)/%.pdf : $(FIGSDUMPDIR)/%.pdf
	$(LN) "$<" "$@"
	
$(TEXOUTDIR)/%.pdf: $(SVGTEXDIR)/%.svg
#SVGTEXTOPDF=inkscape -z -D --file=A.svg --export-pdf=A.pdf --export-latex
	$(SVGTOLATEX) --file="$<" --export-pdf="$(TEXOUTDIR)/$*.pdf" --export-latex
	mv "$(TEXOUTDIR)/$*.pdf_tex" "$(TEXOUTDIR)/$*.tex"
	#OLD METHOD $(SVGTOLATEX) -f "$<" -l "$(FIGSDIR)/$*.tex" -o

$(FIGSDIR)/%.pdf: $(SVGDIR)/%.svg
	$(SVGTOPDF) "$@" "$<"

$(FIGSDIR)/%.pdf: $(TKZDIR)/%.tkz
	$(SETCOLOR_GREEN)
	@echo " ----------------- TIKZ -----------------------"
	$(SETCOLOR_BLACK)
	MakeStandaloneTikz $(MASTER) "$<"
	$(PDFLATEX)  --file-line-error-style --shell-escape --interaction=nonstopmode tmp_tikz.tex >/dev/null
	@mv tmp_tikz.pdf "$@" 
	@rm tmp_tikz*


#-----------------------------------------------------
# VERSIONS 
#-----------------------------------------------------
#echo %date:~10,4%%date:~4,2%%date:~7,2%
#DATE=echo %date:~10,4%
DATE=$(shell echo _%date:~10,4%-%date:~4,2%-%date:~7,2%)

FVN=$(notdir $(wildcard $(OUTDIR)/*.pdf))
FVN2=$(patsubst $(MASTER)_v%,%,$(FVN))
FVN3=$(FVN2:.pdf=)
FVN4=$(subst  ' ',\n,$(FVN3))
FVN5=$(filter _v%_,$(FVN2))
FVN5=$(shell FOR /F %i in ($(FVN3)) DO echo %i)
#FVN2=$(FVN:Tip%=Caca%)



#-----------------------------------------------------
# MAIN COMMANDS
#-----------------------------------------------------
all:allpdf
# 
# diff:
# 	git ldiff

diff:
	latexdiff $(MASTER)-sub.tex $(MASTER).tex > $(MASTER)-diff.tex
	cp $(MASTER).aux $(MASTER)-diff.aux
	cp $(MASTER).bbl $(MASTER)-diff.bbl
	cp $(MASTER).blg $(MASTER)-diff.blg
	pdflatex -interaction batchmode $(MASTER)-diff.tex
public:

tkz: $(TKZ2PDFS)


version: allpdf dateit

allpdf:  figspdf pdfall


dateit:
	@echo $(DATE)
	@echo $(FVN)
	@echo $(FVN2)
	@echo $(FVN3)
	@echo $(FVN4)
	@echo $(FVN5)
#	$(CP) $(MASTER).pdf $(MASTER)$(DATE).pdf


# $(MASTER).pdf
	
#$(MASTER).pdf $(TEXSRC) $(FIGSRC) $(BIBSRC)


clean :
	@$(RM) *.aux *.bbl *.blg *.log *.dvi *.idx *.ilg *.ind *.toc *.lot *.thm *.cb *.cb2 *.gls *.mtc0\
	 *.lof *~ *.bak *.blg *.exa *.adx *.bmt *.mtc *.out *.som *.glo *.glx *.tns *.tpt *.maf *.brf *.gz
	
cleansvgs:
	$(RM) $(SVGTEXS2PDFS) $(SVGS2PDFS)

figssvg: $(SVGTEXS2PDFS) $(SVGS2PDFS)
	

cleanfigs:
	$(RM) $(FIGSDIR)/*
	$(RM) $(TEXOUTDIR)/*


figspdf: $(JPGS2PDFS) $(PNGS2PDFS) $(SVGTEXS2PDFS) $(SVGS2PDFS) $(EPSS2PDFS) $(PDFS2PDFS) $(TKZ2PDFS)

figseps: $(JPGS2EPSS) $(PNGS2EPSS) $(SVGTEXS2EPSS) $(SVGS2EPSS) $(EPSS2EPSS) $(PDFS2EPSS)

fipspng:

figslower:
	for i in figsdump/* ;do ext=`echo $${i#*.}|tr '[:upper:]' '[:lower:]'`; mv "$$i" "$${i%.*}.$$ext";  done

vimcrash:
	$(RM) *.swp /A H
	$(RM) bib\*.swp /A H
	$(RM) tex\*.swp /A H
	$(RM) anx\*.swp /A H
	$(RM) pre\*.swp /A H






#-----------------------------------------------------
# SEQUENCES
#-----------------------------------------------------
pdf: figspdf
	$(PDFLATEX) $(PDFLATEX_FLAGS) $(MASTER)


pdfall: figspdf
ifeq ($(USE_PSTRICKS),1)
	@echo "=======================================  PSTRICKS   ================================================"
	$(PS4PDF) $(MASTER).tex
endif
	@echo "=======================================  PDFLATEX1  ================================================"
	$(PDFLATEX) $(PDFLATEX_FLAGS) $(MASTER)
ifeq ($(USE_BIBTEX),1)
	@echo "=======================================    BIBTEX   ================================================"
	$(BIBTEX) $(BIB_FLAGS) $(MASTER)
endif
ifeq ($(USE_MAKEINDEX),1)
	@echo "=======================================  MAKEINDEX  ================================================"
	$(MAKEINDEX) $(MAKEINDEX_FLAGS) $(MASTER)
endif
ifeq ($(USE_MAKEGLOSS),1)
	@echo "=======================================  MAKEGLOSS  ================================================"
	$(MAKEINDEX) $(MAKEGLOSS_FLAGS) -o $(MASTER).gls $(MASTER).glo
endif
	@echo "=======================================  PDFLATEX2  ================================================"
	$(PDFLATEX) $(PDFLATEX_FLAGS) $(MASTER)
	@echo "=======================================  PDFLATEX3  ================================================"
	$(PDFLATEX) $(PDFLATEX_FLAGS) $(MASTER)

   #~ $(LATEX) $(TEX_FLAGS) $(MASTER)
   #~ $(DVIPS) $(DVIPS_FLAGS) -o $(FIGSDIR)/PSTRICKSFIGURES.ps $(MASTER).dvi
   #~ $(PS2PDF) -dAutoRotatePages=/None               pst-pdf-example1-pics.pdf
   #~ $(PDFLATEX) $(PDFLATEX_FLAGS) $(MASTER)


latexquick:
	latex $(MASTER)	

latex:
	$(LATEX) $(TEX_FLAGS) $(MASTER)
	$(BIBTEX) $(BIB_FLAGS) $(MASTER)
	$(LATEX) $(TEX_FLAGS) $(MASTER)
	$(LATEX) $(TEX_FLAGS) $(MASTER)





	
#-----------------------------------------------------
# LATEX
#-----------------------------------------------------
# To accomplish 2 compilation we chain from tex->aux->dvi

# To generate a .aux file from a .tex file


# To generate a .dvi file from a .tex file
$(MASTER).dvi :	$(MASTER).aux
	$(LATEX) $(TEX_FLAGS) $<  >/dev/null

# To generate a .ps file from a .dvi file
$(MASTER).ps :$(MASTER).dvi
	$(DVIPS) $(DVIPS_FLAGS) -o $@ $< 
	

#-----------------------------------------------------
# INDEX
#-----------------------------------------------------
index:	$(MASTER).aux $(MASTER).idx $(MASTER).ilg $(MASTER).ind


#-----------------------------------------------------
# INDEX
#-----------------------------------------------------
bibtex:	
	$(BIBTEX) $(BIB_FLAGS) $(MASTER)




# To generate a .idx file from a .tex file
%.ilg :	%.idx
	$(MAKEINDEX) $(MAKEINDEX_FLAGS) $(*F)


bibindex : bibtex index
	#makeindex -s bibidx/manuel.ist $(MASTER)
	#makeindex -s bibidx/glossaire.ist $(MASTER).glo -o $(MASTER).glx
	#bibtex $(MASTER)



#-----------------------------------------------------
# HTML Output
#-----------------------------------------------------
html :       $(MASTER).html fipspng

#-----------------------------------------------------
# PDFLATEX
#-----------------------------------------------------
$(MASTER).pdf : $(MASTER).tex
	$(PDFLATEX) $(PDFLATEX_FLAGS) $< 

%.aux :	%.tex
	$(PDFLATEX) $(PDFLATEX_FLAGS) $< 

#-----------------------------------------------------
# BIBTEX
#-----------------------------------------------------
%.bbl : %.tex
ifneq ($(strip $(BIBSRC)),)
	echo "Here"
	$(BIBTEX) $(BIB_FLAGS) $(*F)
endif






# Dependencies
#$(MASTER).tex : $(TEXSRC) $(FIGSRC) 
#$(MASTER).aux : $(TEXSRC) $(FIGSRC) $(BBLSRC)
#$(MASTER).bbl : $(BIBSRC) $(MASTER).aux
#$(MASTER).pdf : $(MASTER).aux $(TEXSRC) $(FIGSRC) $(BBLSRC) $(BIBSRC)
#$(MASTER).dvi : $(TEXSRC) $(FIGSRC)  $(BBLSRC)
