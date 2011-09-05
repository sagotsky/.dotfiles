;; docbook-xml-mode.el version 1.1
;;
;; Author: Francis Litterio <franl@world.std.com>
;; Sourceforge Project Page: http://sourceforge.net/projects/docbookxml/
;;
;; This Emacs-Lisp package provodes DocbookXML mode, a major mode that makes
;; editing DocBook XML documents easier.
;;
;; To install this package, do the following:
;;
;; 1. Copy this file to your personal Emacs-Lisp directory.
;;
;; 2. If you have not already done so, add the pathname of that directory to
;;    your load-path variable in your ~/.emacs file.  One way to do that is like
;;    this:
;;
;;	(setq load-path (append '("~/my-elisp-dir") load-path))
;;
;; 3. Restart Emacs.
;;
;; 4. Emacs will activate it's own built-in XML mode when you find a DocBook XML
;;    file.  You can manually activate DocBook XML mode by typing:
;;
;;	M-x docbook-xml-mode ENTER
;;
;;    Alternatively, you can configure Emacs to automatically turn on DocBook
;;    XML mode when you find your DocBook XML files.  To do this, put this
;;    comment anywhere on the first non-blank line in your file:
;;
;;	<!-- -*- Docbook XML -*- -->
;;
;;    or put these comments at the bottom of your file:
;;
;;	<!-- Local Variables: -->
;;	<!-- mode: docbook-xml -->
;;	<!-- End: -->
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Things to do:
;;
;; o Make docbook-xml-indent-line aware of when it's being called from indent-region
;;   (for performance reasons).
;;
;; o Don't set the buffer-modified flag if indentation didn't change.
;;
;; o Make my-xml-electric-open-tag handle end tags too.
;;
;; o Write docbook-xml-goto-prev-level and docbook-xml-goto-prev-element.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-configurable variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar docbook-xml-indent-level 2
  "Number of columns to indent new element blocks beyond the enclosing block.")

(defvar docbook-xml-prompt-for-tag-names nil
  "If non-nil, pressing '<' prompts you to enter a tag name in the minibuffer,
otherwise it simply inserts a '<'.")

(defvar docbook-xml-mode-hook nil
  "A hook that is run after docbook-xml-mode is activated in a buffer.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ATTENTION!
;;
;; There are no user configurable variables below this point!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar docbook-xml-mode-map nil "Keymap for docbook-xml-mode.")

(progn
  (setq docbook-xml-mode-map (make-sparse-keymap))

  (define-key docbook-xml-mode-map [return]	'docbook-xml-newline)
  (define-key docbook-xml-mode-map "\C-j"	'docbook-xml-newline)
  ;;(define-key docbook-xml-mode-map "<"	'docbook-xml-electric-open-tag)
  (define-key docbook-xml-mode-map ">"		'docbook-xml-electric-close-tag)
  (define-key docbook-xml-mode-map " "		'docbook-xml-electric-space)
  (define-key docbook-xml-mode-map "\M-r"	'docbook-xml-render)

  ;; Insertion keybindings:

  (define-key docbook-xml-mode-map (kbd "C-!")	'docbook-xml-insert-comment)
  (define-key docbook-xml-mode-map "\C-ci1"	'docbook-xml-insert-sect1)
  (define-key docbook-xml-mode-map "\C-ci2"	'docbook-xml-insert-sect2)
  (define-key docbook-xml-mode-map "\C-ci3"	'docbook-xml-insert-sect3)
  (define-key docbook-xml-mode-map "\C-ci4"	'docbook-xml-insert-sect4)
  (define-key docbook-xml-mode-map "\C-cic"	'docbook-xml-insert-computeroutput)
  (define-key docbook-xml-mode-map "\C-cie"	'docbook-xml-insert-example)
  (define-key docbook-xml-mode-map "\C-ciE"	'docbook-xml-insert-emphasis)
  (define-key docbook-xml-mode-map "\C-cil"	'docbook-xml-insert-link)
  (define-key docbook-xml-mode-map "\C-cip"	'docbook-xml-insert-para)
  (define-key docbook-xml-mode-map "\C-cit"	'docbook-xml-insert-title)
  (define-key docbook-xml-mode-map "\C-cu"	'docbook-xml-insert-under-construction)
  (define-key docbook-xml-mode-map "\C-civ"	'docbook-xml-insert-varname)
  (define-key docbook-xml-mode-map "\C-cix"	'docbook-xml-insert-xref))

(defvar docbook-xml-insert-element-history nil
  "Minibuffer history variable used by docbook-xml-insert-element.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun docbook-xml-mode ()
  "Major mode used for editing XML documents.

Special commands:

\\{docbook-xml-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'indent-line-function)
  (use-local-map docbook-xml-mode-map)
  (setq mode-name			"DocBook-XML"
	major-mode			'docbook-xml-mode
	indent-line-function		#'docbook-xml-indent-current-line
	fill-paragraph-function		#'docbook-xml-fill-paragraph
	;; NOTE: '.' doesn't match a newline in a regexp!
	font-lock-defaults		'((("<!--\\(.\\|[\n\r]\\)*-->" . font-lock-comment-face)
					   ("<!\\(DOCTYPE\\)\\(.*\\)"
					    (1 font-lock-keyword-face)
					    (2 font-lock-type-face))
					   ("UNDER CONSTRUCTION" 0 font-lock-warning-face t)
					   ("TODO:.*" 0 font-lock-warning-face t)
					   ("<!ENTITY[^>]*>" 0 font-lock-builtin-face t)
					   ("</?sect[0-9][^>]*>" . font-lock-function-name-face)
					   ("<[^!][^>]*>" . font-lock-keyword-face)
					   ("&[^;]+;" 0 font-lock-constant-face t)
					   )
					  keywords-only
					  case-fold))
  (turn-on-font-lock)
  (run-hooks 'docbook-xml-mode-hook))
			      

(defun docbook-xml-indent-current-line ()
  "Indents the current line appropriately for docbook-xml-mode."
  (interactive)

  (catch 'return
    (let ((origin (point))
	  (origin-marker (set-marker (make-marker) (point)))
	  (origin-bol (progn (beginning-of-line) (point)))
	  (level 0)
	  taginfo tagtype tagname tagstart tagend
	  last-start-tagname last-start-tagtype)
      (goto-char (point-min))

      ;; First, count the number of start and end tags between the start of the
      ;; document and the beginning of the current line.  The number of unclosed
      ;; start tags dictates the indentation level for the current line.  NOTE:
      ;; This doesn't detect when tags are incorrectly nested!  It assumes you
      ;; have correct nesting.

      (catch 'donesearching
	(while (setq taginfo (docbook-xml-find-next-tag))
	  (setq tagtype (car taginfo)
		tagname (nth 1 taginfo)
		tagstart (nth 2 taginfo)
		tagend (nth 3 taginfo))

	  (when (and (eq tagtype 'comment)
		   (> origin tagstart)
		   (< origin tagend))
	    (goto-char origin-marker)
	    (throw 'return nil))

	  (if (>= tagstart origin-bol)
	      (throw 'donesearching nil))

	  (if (eq tagtype 'start)
	      (progn
		(setq last-start-tagname tagname
		      last-start-tagtype tagtype
		      level (1+ level)))
	    (if (eq tagtype 'end)
		(setq last-start-tagname nil
		      last-start-tagtype nil
		      level (1- level))))
	  (goto-char (nth 3 taginfo))))

      (goto-char origin-bol)

      ;; Special case the indentation based on the most recent unclosed start
      ;; tag.

      (when (and (eq last-start-tagtype 'start)
		 (member-ignore-case last-start-tagname '("screen" "programlisting"))
		 (not (looking-at "[ \t]*</")))
	(goto-char origin)
	(throw 'return nil))

      ;; If the first tag on the current line is an end tag, assume it closes
      ;; the last unclosed start tag.

      (if (looking-at "[^<]*</")
	  (setq level (1- level)))

      ;; Do the re-indentation.

      (delete-horizontal-space)
      (indent-to-column (* level docbook-xml-indent-level))

      ;; Position the cursor.

      (goto-char origin-marker)
      (if (looking-at "[ \t]*$") ;; Don't use "\\s-*</" because it matches newlines.
	  (end-of-line)
	(if (< (current-column) (current-indentation))
	    (back-to-indentation))))))


(defun docbook-xml-newline ()
  "Inserts a newline and indents the new line appropriately."
  (interactive)
  (newline)
  (docbook-xml-indent-current-line))


(defun docbook-xml-electric-close-tag ()
  "Inserts '>', re-indents the current line, and bounces cursor to the matching
'<' if it's nearby."
  (interactive)
  (insert ">")
  (docbook-xml-indent-current-line)
  (save-excursion
    (backward-char 2)
    (while (not (looking-at "[<>]"))
      (backward-char 1))
    (if (looking-at "<")
	(sit-for 0.7))))


(defun docbook-xml-electric-space ()
  "Inserts a space, and, if the current column is greater than fill-column, fills
the line."
  (interactive)
  (catch 'return
    (let ((origin (set-marker (make-marker) (point))))
      (when (and (integerp fill-column)
		 (> (current-column) fill-column))
	(move-to-column (+ fill-column 5))
	(while (not (looking-at " "))
	  (backward-char))
	(delete-char 1)
	;; Handle the case where the line ends with spaces that span the
	;; fill-column.
	(if (looking-at "\\s-+")
	    (progn
	      (docbook-xml-newline)
	      (throw 'return nil))
	  (docbook-xml-newline)
	  (goto-char origin))))
    (insert " ")))


(defun docbook-xml-electric-open-tag ()
  "Prompts the user for a tag name, then inserts an element using that tag."
  (interactive)
  (insert "<")
  (if docbook-xml-prompt-for-tag-names
      (call-interactively #'docbook-xml-insert-element)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insertion commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun docbook-xml-insert-comment ()
  "Inserts a new comment element.  Leaves point inside the element."
  (interactive)
  (insert "<!--  -->")
  (backward-char 4))


(defun docbook-xml-insert-computeroutput ()
  "Inserts a new computerouput element.  Leaves point inside the element."
  (interactive)
  (let ((element "<computeroutput></computeroutput>"))
    (if (save-excursion
	  (backward-char 16)
	  (looking-at element))
	(progn
	  (insert "<command></command>")
	  (backward-char 10))
      (insert element)
      (backward-char 17))))


(defun docbook-xml-insert-under-construction ()
  "Inserts an UNDER CONSTRUCTION paragraph."
  (interactive)
  (beginning-of-line)
  (let ((bol (point))
	(line-is-empty (looking-at "[ \t]*$")))
    (if line-is-empty
	(delete-horizontal-space))
    (insert "<para>*** UNDER CONSTRUCTION ***</para>")
    (if (not line-is-empty)
	(insert "\n"))
    (goto-char bol))
  (docbook-xml-indent-current-line))


(defun docbook-xml-insert-emphasis ()
  "Inserts a new emphasis element.  Leaves point inside the element."
  (interactive)
  (insert "<emphasis></emphasis>")
  (backward-char 11))


(defun docbook-xml-insert-example ()
  "Inserts a new example, made out of <para>, <blockquote>, and <example> elements."
  (interactive)
  (let ((lines '("<para>"
		 "  <blockquote>"
		 "    <example id=\"\">"
		 "      <title>...</title>"
		 "      <programlisting>"
		 "      ..."
		 "      </programlisting>"
		 "    </example>"
		 "  </blockquote>"
		 "</para>"))
	(indent (current-indentation))
	line)
    (while (setq line (car lines))
      (setq lines (cdr lines))
      (insert (concat line (if lines "\n")))
      (indent-to-column indent))
    (forward-line -7)
    (end-of-line)
    (backward-char 2)))


(defun docbook-xml-insert-link ()
  "Inserts a new LINK element."
  (interactive)
  (insert "<link linkend=\"\"/>")
  (backward-char 3))


(defun docbook-xml-insert-para ()
  "Inserts a new PARA element.  Leaves point inside the element."
  (interactive)
  (insert "<para>")
  (docbook-xml-indent-current-line)
  (let ((indentation (current-indentation))
	endpoint)
    (newline)
    (setq endpoint (point))
    (newline)
    (indent-to-column indentation)
    (insert "</para>")
    (goto-char endpoint)
    (indent-to-column (+ indentation docbook-xml-indent-level))))


(defun docbook-xml-insert-section (level)
  "Inserts a SECT<N> element, where <N> is the value of argument LEVEL."
  (save-excursion
    (let ((indentation (current-indentation)))
      (insert (format "<sect%d id=\"\">\n" level))
      (indent-to-column indentation)
      (insert (format "</sect%d>" level))))
  (forward-char 11))


(defun docbook-xml-insert-sect1 ()
  "Inserts a SECT1 element."
  (interactive)
  (docbook-xml-insert-section 1))


(defun docbook-xml-insert-sect2 ()
  "Inserts a SECT1 element."
  (interactive)
  (docbook-xml-insert-section 2))


(defun docbook-xml-insert-sect3 ()
  "Inserts a SECT1 element."
  (interactive)
  (docbook-xml-insert-section 3))


(defun docbook-xml-insert-sect4 ()
  "Inserts a SECT1 element."
  (interactive)
  (docbook-xml-insert-section 4))


(defun docbook-xml-insert-title ()
  "Inserts a TITLE element."
  (interactive)
  (insert "<title></title>")
  (backward-char 8))


(defun docbook-xml-insert-varname ()
  "Inserts a VARNAME element."
  (interactive)
  (insert "<varname></varname>")
  (backward-char 10))


(defun docbook-xml-insert-xref ()
  "Inserts a new XREF element.  Leaves point inside the linkend attribute."
  (interactive)
  (insert "<xref linkend=\"\"/>")
  (backward-char 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun docbook-xml-render ()
  "Renders the current XML document into HTML."
  (interactive)
  (if (buffer-modified-p)
      (error "Buffer has been modified.  Save your changes first!"))
  (message "Rendering document into HTML ...")
  (my-background-shell-command (format "db2html %s" (buffer-file-name))))


(defun docbook-xml-fill-paragraph (arg)
  "Fills the current paragraph in docbook-xml-mode."
  (catch 'return
    (let ((origin (point))
	  (origin-marker (set-marker (make-marker) (point)))
	  (fill-prefix fill-prefix)
	  (case-fold-search t)
	  para-start
	  para-end)
      ;; If inside a tag, do nothing.
      ;; What about text in comments?
      (if (save-excursion (and (search-forward-regexp "[<>]" nil t)
			       (not (backward-char)) ;; Always t.  Need side-effect.
			       (looking-at ">")))
	  (throw 'return nil))

      ;; Narrow the buffer to the paragraph to be filled.  The paragraph is bounded
      ;; above by one of:
      ;;
      ;;   o A <para> tag.
      ;;   o A line containing exactly one start tag (e.g., <listitem>).
      ;;   o A blank line.
      ;;   o A line containing only whitespace.
      ;;   o The beginning of the buffer.
      ;;
      ;; The paragraph is bounded below by one of:
      ;;
      ;;   o A </para> tag.
      ;;   o A line containing exactly one start tag (e.g., <listitem>).
      ;;   o A blank line.
      ;;   o A line containing only whitespace.
      ;;   o The end of the buffer.

      (if (not (search-backward-regexp "\\(<para\\|^\\s-*<[^/>]+>\\s-*$\\|^\\s-*$\\)"
				       nil t))
	  (setq para-start (point-min))
	(if (looking-at "<para[^>]+")
	    (forward-char (length (match-string 0)))
	  (forward-line))
	(setq para-start (point)))

      (goto-char origin)
      (if (not (search-forward-regexp "\\(</para\\|^\\s-*<[^/>]+>\\s-*$\\|^\\s-*$\\)"
				      nil t))
	  (setq para-end (point-max))
	(beginning-of-line)
	(if (looking-at "\\(.*\\)</para")
	    (forward-char (length (match-string 1)))
	  (forward-line))
	(setq para-end (point)))
	  
      ;; Compute the fill-prefix.  This has to be done before we narrow the buffer
      ;; below.
      (when (null fill-prefix)
	(goto-char para-start)
	(beginning-of-line)
	(if (looking-at "\\(\\s-+\\)")
	    (setq fill-prefix (match-string 1))))

      ;; Now fill the paragraph.
      (let ((fill-paragraph-function nil)
	    added-newline)
	(goto-char origin)
	(save-restriction
	  (narrow-to-region para-start para-end)
	  (save-excursion
	    (goto-char (point-max))
	    (when (not (bolp))
	      (insert "\n")
	      (setq added-newline t)))
	  ;;(debug)
	  (fill-paragraph nil)
	  (when added-newline
	    (goto-char (point-max))
	    (backward-delete-char 1))))

      ;; Leave point somewhere sane.
      (goto-char origin-marker))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun docbook-xml-find-next-tag ()
  "Searches forward from point in the current buffer for an XML tag.  This
function returns nil if a complete tag cannot be found, otherwise it returns a
list of the form

	TAGTYPE TAGNAME START END STARTCOL

TAGTYPE is one of the symbols: comment, empty, start, end.  TAGNAME is a string:
\"\" for comments and some other tags.  START and END are integers denoting
buffer positions.  STARTCOL is an integer denoting the column in which the tag's
'<' appears."
  (catch 'return
    (let ((tagname "")
	  (case-fold-search t)
	  tagtype start end startcol)
      (when (search-forward "<" nil t)
	(setq start (1- (point))
	      startcol (1- (current-column)))
    
	;; Get the tag type.
	(if (looking-at "\\?\\|!\\(doctype\\|entity\\)")
	    (setq tagtype 'empty)
	  (if (looking-at "!--")
	      (setq tagtype 'comment)
	    (if (not (looking-at "/"))
		;; This might change to 'empty below.
		(setq tagtype 'start)
	      (setq tagtype 'end)
	      (forward-char))))

	;; Get the tag name.
	(let ((namestart (point)))
	  (skip-chars-forward "a-zA-Z0-9:_-")
	  (setq tagname (buffer-substring namestart (point))))

	;; Find the end of the tag.
	(if (eq tagtype 'comment)
	    (progn
	      (if (null (docbook-xml-find-end-of-comment))
		  (throw 'return nil))
	      (setq end (point)))
	  (if (null (search-forward ">"))
	      (throw 'return nil)
	    (setq end (point))
	    (backward-char 2)
	    (when (looking-at "/>")
	      (setq tagtype 'empty))
	    (goto-char end)))
	
	;; Compute result.
	(if (and tagtype start end)
	    (list tagtype tagname start end startcol))))))


(defun docbook-xml-find-end-of-comment ()
  "Moves point forward in current buffer to just after the closing '>' in a
comment, taking into consideration that comments can contain nested tags.
Returns nil if end of the comment cannot be found, non-nil if it is found."
  ;; Assume we are already past the "<" which starts the comment.  Start out
  ;; with nesting-level set to 1.  Increment it for each "<" we find.  Decrement
  ;; for each ">" we find.  When it reaches 0, we're at the end of the comment.
  (let ((nesting-level 1)
	(origin (point))
	(comment-start-line (count-lines (point-min) (point))))
    (while (and (> nesting-level 0)
		(search-forward-regexp "[<>]" nil t))
	(if (string= "<" (buffer-substring (1- (point)) (point)))
	    (setq nesting-level (1+ nesting-level))
	  (setq nesting-level (1- nesting-level))))
    (= 0 nesting-level)))


(defun docbook-xml-insert-element (tagname)
  "Inserts a new XML element.  Leaves point inside the element."
  (interactive (list (completing-read "Element: " docbook-xml-tags nil nil nil
				'docbook-xml-insert-element-history "para" nil)))
  (if (= ?/ (aref tagname 0))
      (insert (concat tagname ">"))

    (let ((taginfo (cdr (assoc (downcase tagname) docbook-xml-tags)))
	  (tagname (downcase tagname))
	  (indent (current-indentation)))
      (if (memq 'multiline taginfo)
	  (progn
	    (insert tagname ">\n")
	    (indent-to-column (+ indent (if (string= tagname "para")
					    ;;0
					    docbook-xml-indent-level
					  docbook-xml-indent-level)))
	    (save-excursion
	      (insert "\n")
	      (indent-to-column indent)
	      (insert "</" tagname ">")))
	(if (memq 'empty taginfo)
	    (progn
	      (insert tagname "/>")
	      (when (string= tagname "xref")
		(backward-char 2)
		(insert " linkend=\"\"")
		(backward-char 1)))
	  (insert tagname ">")
	  (save-excursion (insert "</" tagname ">")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frob auto-mode-alist.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((entry (assoc "\\.xml\\'" auto-mode-alist)))
  (if entry
      (setcdr entry 'docbook-xml-mode)
    (add-to-list 'auto-mode-alist '("\\.xml\\'" . docbook-xml-mode))))

;; This defvar should be at the top of the file, but it's so huge that it just
;; gets in the way up there.

(makunbound 'docbook-xml-tags)
(defvar docbook-xml-tags
  '(("abbrev") ("abstract") ("accel") ("ackno") ("acronym") ("action")
    ("address") ("affiliation") ("alt") ("anchor") ("answer") ("appendix")
    ("appendixinfo") ("application") ("area") ("areaset") ("areaspec")
    ("arg") ("article" multiline) ("articleinfo") ("artpagenums") ("attribution")
    ("audiodata") ("audioobject") ("author" multiline) ("authorblurb") ("authorgroup")
    ("authorinitials") ("beginpage") ("bibliocoverage") ("bibliodiv")
    ("biblioentry") ("bibliography") ("bibliographyinfo") ("biblioid")
    ("bibliomisc") ("bibliomixed") ("bibliomset") ("bibliorelation")
    ("biblioset") ("bibliosource") ("blockinfo") ("blockquote") ("book")
    ("bookinfo") ("bridgehead") ("callout") ("calloutlist") ("caption")
    ("caution") ("chapter") ("chapterinfo") ("citation") ("citebiblioid")
    ("citerefentry") ("citetitle") ("city") ("classname") ("classsynopsis")
    ("classsynopsisinfo") ("cmdsynopsis") ("co") ("collab") ("collabname")
    ("colophon") ("colspec") ("command") ("computeroutput") ("confdates")
    ("confgroup") ("confnum") ("confsponsor") ("conftitle") ("constant")
    ("constraint") ("constraintdef") ("constructorsynopsis") ("contractnum")
    ("contractsponsor") ("contrib") ("copyright") ("coref") ("corpauthor")
    ("corpname") ("country") ("database") ("date") ("dedication") ("destructorsynopsis")
    ("edition") ("editor") ("email") ("emphasis") ("entry") ("entrytbl")
    ("envar") ("epigraph") ("equation") ("errorcode") ("errorname") ("errortext")
    ("errortype") ("example" multiline) ("exceptionname") ("fax") ("fieldsynopsis")
    ("figure") ("filename") ("firstname") ("firstterm") ("footnote")
    ("footnoteref") ("foreignphrase") ("formalpara") ("funcdef") ("funcparams")
    ("funcprototype") ("funcsynopsis") ("funcsynopsisinfo") ("function")
    ("glossary") ("glossaryinfo") ("glossdef") ("glossdiv") ("glossentry")
    ("glosslist") ("glosssee") ("glossseealso") ("glossterm") ("graphic")
    ("graphicco") ("group") ("guibutton") ("guiicon") ("guilabel") ("guimenu")
    ("guimenuitem") ("guisubmenu") ("hardware") ("highlights") ("holder")
    ("honorific") ("html:form") ("imagedata") ("imageobject") ("imageobjectco")
    ("important") ("index") ("indexdiv") ("indexentry") ("indexinfo")
    ("indexterm") ("informalequation") ("informalexample") ("informalfigure")
    ("informaltable") ("initializer") ("inlineequation") ("inlinegraphic")
    ("inlinemediaobject") ("interface") ("interfacename") ("invpartnumber")
    ("isbn") ("issn") ("issuenum") ("itemizedlist") ("itermset") ("jobtitle")
    ("keycap") ("keycode") ("keycombo") ("keysym") ("keyword") ("keywordset")
    ("label") ("legalnotice") ("lhs") ("lineage") ("lineannotation")
    ("link") ("listitem" multiline) ("literal") ("literallayout") ("lot") ("lotentry")
    ("manvolnum") ("markup") ("medialabel") ("mediaobject") ("mediaobjectco")
    ("member") ("menuchoice") ("methodname") ("methodparam") ("methodsynopsis")
    ("mml:math") ("modespec") ("modifier") ("mousebutton") ("msg") ("msgaud")
    ("msgentry") ("msgexplan") ("msginfo") ("msglevel") ("msgmain") ("msgorig")
    ("msgrel") ("msgset") ("msgsub") ("msgtext") ("nonterminal") ("note" multiline)
    ("objectinfo") ("olink") ("ooclass") ("ooexception") ("oointerface")
    ("option") ("optional") ("orderedlist") ("orgdiv") ("orgname") ("otheraddr")
    ("othercredit") ("othername") ("pagenums") ("para" multiline) ("paramdef")
    ("parameter") ("part") ("partinfo") ("partintro") ("personblurb")
    ("personname") ("phone") ("phrase") ("pob") ("postcode") ("preface")
    ("prefaceinfo") ("primary") ("primaryie") ("printhistory") ("procedure")
    ("production") ("productionrecap") ("productionset") ("productname")
    ("productnumber") ("programlisting" multiline) ("programlistingco") ("prompt")
    ("property") ("pubdate") ("publisher") ("publishername") ("pubsnumber")
    ("qandadiv") ("qandaentry") ("qandaset") ("question") ("quote") ("refclass")
    ("refdescriptor") ("refentry") ("refentryinfo") ("refentrytitle")
    ("reference") ("referenceinfo") ("refmeta") ("refmiscinfo") ("refname")
    ("refnamediv") ("refpurpose") ("refsect1") ("refsect1info") ("refsect2")
    ("refsect2info") ("refsect3") ("refsect3info") ("refsection") ("refsectioninfo")
    ("refsynopsisdiv") ("refsynopsisdivinfo") ("releaseinfo") ("remark")
    ("replaceable") ("returnvalue") ("revdescription") ("revhistory")
    ("revision") ("revnumber") ("revremark") ("rhs") ("row") ("sbr")
    ("screen") ("screenco") ("screeninfo") ("screenshot") ("secondary")
    ("secondaryie") ("sect1" multiline) ("sect1info") ("sect2" multiline)
    ("sect2info") ("sect3" multiline) ("sect3info") ("sect4" multiline)
    ("sect4info") ("sect5" multiline) ("sect5info") ("section")
    ("sectioninfo") ("see") ("seealso") ("seealsoie") ("seeie") ("seg")
    ("seglistitem") ("segmentedlist") ("segtitle") ("seriesvolnums")
    ("set") ("setindex") ("setindexinfo") ("setinfo") ("sgmltag") ("shortaffil")
    ("shortcut") ("sidebar") ("sidebarinfo") ("simpara") ("simplelist")
    ("simplemsgentry") ("simplesect") ("spanspec") ("state") ("step")
    ("street") ("structfield") ("structname") ("subject") ("subjectset")
    ("subjectterm") ("subscript") ("substeps") ("subtitle") ("superscript")
    ("surname") ("svg:svg") ("symbol") ("synopfragment") ("synopfragmentref")
    ("synopsis") ("systemitem") ("table") ("tbody") ("term") ("tertiary")
    ("tertiaryie") ("textdata") ("textobject") ("tfoot") ("tgroup") ("thead")
    ("tip") ("title") ("titleabbrev") ("toc") ("tocback") ("tocchap")
    ("tocentry") ("tocfront") ("toclevel1") ("toclevel2") ("toclevel3")
    ("toclevel4") ("toclevel5") ("tocpart") ("token") ("trademark") ("type")
    ("ulink") ("userinput") ("varargs") ("variablelist") ("varlistentry")
    ("varname") ("videodata") ("videoobject") ("void") ("volumenum")
    ("warning" multiline) ("wordasword") ("xref" empty) ("year"))
  "DocBook tags and info about each one.")

(provide 'docbook-xml-mode)
