" Vim syntax file
" Language: D&D 4e stat block
"
" Keywords to highlight:

syn keyword 4eLabel	Level XP Initiative HP AC Speed Fortitude Reflex Will Reach dominated stunned weakened dazed slowed Senses darkvision Resist Saving Throws fly teleport Action Points prone Close burst blast Bloodied Petrified Marked Unconscious Surprised Immobilized Helpless Deafened Blinded Restrained
syn match   4eSave "(save ends\( both\)\?)"
syn keyword 4eActionD standard move minor free		contained
syn keyword 4eActionW standard move minor free		contained
syn keyword 4eActionE standard move minor free		contained
syn keyword 4eActionR standard move minor free		contained
syn keyword 4eActionI immmediate contained

syn match 4eDaily			"^.*daily).*$" 	contains=4eActionD
syn match 4eWill			"^.*at-will).*$" 	contains=4eActionW
syn match 4eEncounter 		"^.*encounter).*$" 	contains=4eActionE
syn match 4eRecharge		"^.*recharges\? .*).*" 	contains=4eActionR
syn match 4eInterrupt		"^.*interrupt.*).*" 	contains=4eActionI
syn match 4eSecondary           "^Secondary Attack$"    
syn match 4eNonAction       "^\D\+[^\.]$"   contains=ALLBUT,x4eDaily,x4eWill,x4eEncounter,x4eRecharge,x4eInterrupt,x4eSecondary
"syn match free

" new block section highlighters
syn match 4eWill			"^.* At-Will" 	contains=4eActionW
syn match 4eDaily			"^.* Daily$" 	contains=4eActionD
syn match 4eEncounter 		"^.* Encounter$" 	contains=4eActionE
syn match 4eRecharge		"^.* Recharge \(\d\+\|when\).*" 	contains=4eActionR
syn match 4eInterrupt		"Trigger.*:.*$" 	contains=4eActionI
syn match 4eStandardSection           "^Standard Actions$"
syn match 4eMoveSection           "^Move Actions$"
syn match 4eMinorSection           "^Minor Actions$"
syn match 4eTriggeredSection           "^Triggered Actions$"

syn match 4eNumber 			/+\?-\?\d\+d\?/ " match numbers, include + or - sign
syn region 4eTitle start="^.*\n.*\nLevel.*\n\?XP.*" end="XP.*$"

hi 4eDaily 			ctermbg=white 	ctermfg=black
hi 4eWill  			ctermbg=22		ctermfg=white
hi 4eEncounter 		ctermbg=88	 	ctermfg=white
hi 4eRecharge		ctermbg=55
hi 4eInterrupt		ctermbg=blue
hi 4eLabel 			cterm=bold 		ctermfg=228
hi 4ePowerText 		ctermbg=red
hi 4eTitle 			ctermbg=18 		cterm=bold 	gui=bold guifg=DeepSkyBlue2
hi 4eNumber 		ctermfg=208		guifg=goldenrod3
hi 4eSave			ctermfg=159 	cterm=bold
hi 4eActionD		ctermbg=white ctermfg=black cterm=underline
hi 4eActionW		ctermbg=22  cterm=underline
hi 4eActionE		ctermbg=88 cterm=underline
hi 4eActionR		ctermbg=55 cterm=underline
hi 4eActionI		ctermbg=blue cterm=underline
hi 4eNonAction      ctermbg=234
hi 4eSecondary      cterm=underline 
"hi 4eAction			cterm=underline

hi 4eStandardSection     cterm=underline
hi 4eMoveSection         cterm=underline
hi 4eMinorSection         cterm=underline
hi 4eTriggeredSection         cterm=underline

hi CursorLine 		ctermbg=none cterm=bold
set nonumber
set wrap

" level up and down functions
" if no level mod attribute, add it
" subtract out current level mod
" set new level mod
" modify levels:
"   +n saves ac.  easy
"   +attacks are always vs a save.  damage comes after semicolon
"   +n to attacks, n/2 to damage.
"   +hp.  8n for skim, soldier, cont.  10n for brute.  6n for lurk, art
"

" gets the number following a string
" string must match exactly.
function! GetValue( s )
	if ( search(a:s.' \w\+', "w") > 0 )
		let line = getline(".")
		let value = substitute(line,'.*'.a:s.'\s\+\(\w\+\).*', '\1', "g") " value is a string here.
		return value
	else
		echo "No match for value ".a:s
		return 0
	endif
endfunction

" changes a D&D value.  assumes only one instane of that value.
function! SetValue( s, v )
	if ( search(a:s.' \w\+', "w") > 0 )
		let line = getline(".")
		let current = GetValue( a:s )
		let new = substitute(line, a:s." ".current, a:s." ".a:v, "g")
		call setline(".", new)
	else 
		echo "Couldn't find value ".a:s
		return 0
	endif
endfunction

" adjusts current values instead of setting them.
function! ModValue( s, mod )
	call SetValue( a:s, GetValue(a:s) + a:mod )
endfunction

function! GetLevelMod()
	call search("Level")
	let line = getline(".")
	if ( match(line, '\(+\|-\)') > 0 )
		return substitute( line, '^.*\(+\|-\)\(.\).*$', '\1\2', "" )
	else
		return 0
	endif
endfunction


function! Levelup()
	" first, level back to +0

	let mod = GetLevelMod()

	let mod += 1
	let role = "Brute"

	let role_hp = {"Minion": 0, "Skirmisher": 8, "Soldier": 8, "Controller": 8, "Brute": 10, "Lurker": 6, "Artillery": 6}

	call ModValue( "AC" , 			mod)
	call ModValue( "Fortitude" , 	mod)
	call ModValue( "Reflex" ,  		mod)
	call ModValue( "Will" , 		mod)
	call ModValue( "HP",			mod * role_hp[role])

	" attacks and damages.


endfunction


