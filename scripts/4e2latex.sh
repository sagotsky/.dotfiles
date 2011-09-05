#!/bin/sh

# styles 4e documents with latex.
# input is file(s) to put on page.

# suggested usage: ./4e2latex.sh file1.4e file2.4e > encounter.tex && pdflatex encounter.tex

tex_head() {
	# <<- redirection is refered to as a "here document"
	cat <<-endHead 
		%\documentclass{article}
		\documentclass[9pt]{extarticle}
		\usepackage{color}
		\usepackage{fullpage}
		\usepackage{multicol}

		\addtolength{\oddsidemargin}{-.25in}
		\addtolength{\evensidemargin}{-.25in}
		\addtolength{\textwidth}{0.5in}
		\addtolength{\topmargin}{-.5in}
		\addtolength{\textheight}{0.5in}

		\parindent0em
		\setlength{\parindent}{0pt} 

		\pagestyle{empty} % hides page number

		\definecolor{keywords}{RGB}{100,50,00}
		\definecolor{atwill}{RGB}{000,100,050}
		\definecolor{recharge}{RGB}{125,000,125}
		\definecolor{encounter}{RGB}{150,000,000}
		\definecolor{immediate}{RGB}{050,050,150}
		\definecolor{nonaction}{RGB}{200,200,200}
		\definecolor{daily}{RGB}{255,255,255}


		\begin{document}
		\begin{multicols*}{2}


	endHead
}

tex_foot() {
	cat <<-endFoot
		\end{multicols*}
		\end{document}
	endFoot
}

block_head() {
	cat <<-endBlockHead
		\parbox{0.48\textwidth}{
	endBlockHead
}

block_foot() {
	cat <<-endBlockFoot
		}
	endBlockFoot
}

texify() {
	TMP=$(mktemp)
	cat $1 > $TMP

	_MINIPAGE='\\\begin{minipage}[t]{0.48\\textwidth}'

	KEYWORDS="\(Speed\|Level\|XP\|Initiative\|HP\|AC\|Speed\|Fortitude\|Reflex\|Will\|Reach\|dominated\|stunned\|weakened\|dazed\|slowed\|Senses\|darkvision\|Resist\|Saving\|Throws\|fly\|teleport\|Action\|Points\|prone\|Close\|burst\|blast\|Petrified\|Marked\|Unconscious\|Surprised\|Immobilized\|Helpless\|Deafened\|Blinded\|Restrained\) "
	KEYWORDS_HI="\\\textcolor\{keywords\}\{\1}"

	ACTION="\(standard\|move\|minor\|free\)"
	ACTION_HI="\\\underline\{\1\}"

	ENCOUNTER="\(^.*(.*$ACTION.*\?encounter.*$\)" # should only appear in parens.  not sure if that's true.
	ENCOUNTER_HI="{\\\bf \\\fcolorbox\{white\}\{encounter\}{$_MINIPAGE\\\textcolor\{white\}\{\1\}\\\end{minipage}}}"

	DAILY="\(^.*($ACTION.*\?daily.*).*$\)" # should only appear in parens.  not sure if that's true.
	DAILY_HI="{\\\bf \\\fcolorbox\{black\}\{daily\}{$_MINIPAGE\\\textcolor\{black\}\{\1\}\\\end{minipage}}}"

	ATWILL="\(^.*($ACTION.*at-will.*).*$\)"
	ATWILL_HI="{\\\bf \\\fcolorbox\{white\}\{atwill\}{$_MINIPAGE\\\textcolor\{white\}\{\1\}\\\end{minipage}}}"

	RECHARGE="\(^.*($ACTION.*recharge\?.*).*$\)"
	RECHARGE_HI="{\\\bf \\\fcolorbox{white}\{recharge\}{$_MINIPAGE\\\textcolor\{white\}\{\1\}\\\end{minipage}}}"

	#IMMEDIATE="\(^.*(immediate.*[^\(encounter\|at-will\|daily\|recharge\)]).*$\)"
	IMMEDIATE="\(^.*(.*immediate\?.*).*$\)"
	IMMEDIATE_HI="{\\\bf \\\fcolorbox{white}\{immediate\}{$_MINIPAGE\\\textcolor\{white\}\{\1\}\\\end{minipage}}}"

        SECONDARY="^Secondary Attack$"
        SECONDARY_HI="\\\underline\{\1\}"

    NONACTION="\(^[[:alpha:][:blank:]]\+$\)"
    NONACTION_HI="{ \\\fcolorbox{white}{nonaction}{$_MINIPAGE\\\textcolor\{black\}\{\1\}\\\end{minipage}}}"
    #NONACTION_HI="\\\textcolor\{red\}\{\1\}"

	TAILFLUFF="^\(Alignment\|Skills\|Str\|Con\|Published\|Equipment\).*$"
	TAILFLUFF_HI=""

	XP_UNDERLINE="\(.*XP.*\)$"
	XP_UNDERLINE_HI="\\\underline\{\1\} "

	FIRSTLINE="^.*$" # 1s/THIS/THAT replaces on first line
	FIRSTLINE_HI="\\\paragraph\{\1\}\n"

	sed -i "s/$/\n/"  $TMP
	sed -i "s/'//g" $TMP
	sed -i "s/\($ENCOUNTER\)/$ENCOUNTER_HI/g" $TMP
	sed -i "s/\($DAILY\)/$DAILY_HI/g" $TMP
	sed -i "s/\($RECHARGE\)/$RECHARGE_HI/g" $TMP
	sed -i "s/\($ATWILL\)/$ATWILL_HI/g" $TMP
	sed -i "s/\($IMMEDIATE\)/$IMMEDIATE_HI/g" $TMP
	sed -i "s/\($ACTION\)/$ACTION_HI/g" $TMP
	sed -i "s/\($SECONDARY\)/$SECONDARY_HI/g" $TMP
	sed -i "4,$ s/\($NONACTION\)/$NONACTION_HI/g" $TMP
	sed -i "s/\($ACTION_NEWLINE\)/$ACTION_NEWLINE_HI/g" $TMP
	sed -i "s/\($XP_UNDERLINE\)/$XP_UNDERLINE_HI/g" $TMP
	sed -i "1s/\($FIRSTLINE\)/$FIRSTLINE_HI\\\\/" $TMP
	sed -i "s/\($KEYWORDS\)/$KEYWORDS_HI/g" $TMP
	sed -i "s/\($TAILFLUFF\)/$TAILFLUFF_HI/m" $TMP 

	block_head
	cat $TMP
	block_foot

	rm $TMP
}

tex_head

while [ "$#" -gt 0 ]
do
	texify $1
	shift
done

tex_foot

#   this script should take multiple enemies as args and put them on a single page.

