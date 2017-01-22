#!/usr/bin/perl

# adds latex around 4e documents
# sagotsky@gmail.com

use strict;
use warnings;

# boilerplate for starting latex document
sub latex_head {
    # single quotes around name of here doc ensure \ doesn't count as escape character
    print <<'endHead'; 
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

my $_MINIPAGE = '\begin{minipage}[t]{\textwidth}';

# hash of all replacements.  key => [match, repl]
my %replacements = ();

$replacements{actions2} = [
    '(Standard Actions|Move Actions|Triggered Actions|Minor Actions|Traits)',
    '\underline{$1}'
];

$replacements{actions} = [  
    '(standard|move[^s]|minor|free)' ,
    '\underline{$1}'
];

$replacements{keywords} = [
    ' (Speed|Aura|Ranged|Initiative|HP|AC|Speed|Fortitude|Reflex|Will|Reach|dominated|stunned|weakened|dazed|slowed|Senses|darkvision|Resist|Saving Throws|fly|teleport|Action|Points|prone|Close|burst|blast|Petrified|Marked|Unconscious|Surprised|Immobilized|Helpless|Deafened|Blinded|Restrained) ',
    ' \textcolor{keywords}{$1} '
];
    
$replacements{encounter} = [
    '(.*' . $replacements{actions}[0] . '.*encounter[^;].*)',
    '\fcolorbox{white}{encounter}{$_MINIPAGE \textcolor{white}{$1} \end{minipage}}'
];

$replacements{encounter2} = [
    '(.*Encounter$)',
    '\fcolorbox{white}{encounter}{$_MINIPAGE \textcolor{white}{$1} \end{minipage}}'
];

$replacements{daily} = [
    '(.*' . $replacements{actions}[0] . '.*daily[^;].*)',
    '\fcolorbox{black}{daily}{$_MINIPAGE \textcolor{black}{$1} \end{minipage}}'
];

$replacements{atwill} = [
    '(.*' . $replacements{actions}[0] . '.*at-will[^;].*)',
    '\fcolorbox{white}{atwill}{$_MINIPAGE \textcolor{white}{$1} \end{minipage}}'
];

$replacements{atwill2} = [
    '(.*At-Will$)',
    '\fcolorbox{white}{atwill}{$_MINIPAGE \textcolor{white}{$1} \end{minipage}}'
];
$replacements{recharge} = [
    '(.*' . $replacements{actions}[0] . '.*recharge[^;].*)',
    '\fcolorbox{white}{recharge}{$_MINIPAGE \textcolor{white}{$1} \end{minipage}}'
];
$replacements{recharge2} = [
    '(.*Recharge \d+$)',
    '\fcolorbox{white}{recharge}{$_MINIPAGE \textcolor{white}{$1} \end{minipage}}'
];
$replacements{immediate} = [
    '(.*\(.*immediate[^;].*\).*)',
    '\fcolorbox{white}{immediate}{$_MINIPAGE \textcolor{white}{$1} \end{minipage}}'
];
$replacements{immediate2} = [
    '(Trigger.*:.*)$',
    '\fcolorbox{white}{immediate}{$_MINIPAGE \textcolor{white}{$1} \end{minipage}}'
];
$replacements{secondary} = [
    '(Secondary Attack|Aftereffect)', # is the $ getting screwed?
    '\underline{$1}'
];

$replacements{skills} = [
    '^Skills (.*)$',
    '{\it $1}'
    ];

$replacements{firstline} = [
'^(.*)\n',
    '{\bf $1 }'
];

# fixes heading.  originally matched $ but that seems to be broken
$replacements{newline} = [
'^(.*)$',
' $1'. " \n"
];

$replacements{XP} = [
'(.*XP.*)',
'\underline{$1}'
];

#may not be correct
$replacements{fluff} = [
    '((Revision|Update|Replace|Alignment|Str|Con|Equipment:|Published|Description:) .*)',
    ''
    ];

$replacements{nonaction} = [
    '^((?!(Secondary Attack|Aftereffect|Move Actions|Standard Actions|Minor Actions|Trigger))(\w+\s?)+)$',
    '\fcolorbox{white}{nonaction}{$_MINIPAGE \textcolor{black}{$1} \end{minipage}}'
];




sub latex_foot {
    print <<'endFoot';
\end{multicols*}
\end{document}

endFoot
    
}

#adds latex to 4e txt doc
sub texify4e {
    my ($txt, $linenr) = @_;

    # run each sub in %replacements
    while (my ($replacement, @regex) = each(%replacements)) {
        ### account for special cases
        # header matches passive ability
        next if ($replacement eq "nonaction" && $linenr < 12) ;
        
        # bold first line
        next if ($replacement eq "firstline" && $linenr > 0);

        # more newlines needed in head of doc
        next if ($replacement eq "newline" && $linenr > 10);
        
        # xp updates and revisions cause unended underline.  just do xp in header
        next if ($replacement eq "XP" && $linenr > 10);

        my $match = $replacements{$replacement}[0] ;
        my $repl  = $replacements{$replacement}[1] ;
        $repl =~ s/\\/\\\\/g; # you like escapes so we escaped escapes in your escapes

        $txt =~ s/$match/eval("qq($repl)")/eg; #eval forces str interpolation

    }
    return $txt;
}


sub usage {
    print STDERR <<'endUsage';
Usage:

4E2latex.pl /path/to/4e/text.files > /tmp/4eEncounter.tex
You must specify at least one text file.

endUsage
}

# help and quit if no args
if ($#ARGV < 0) {
    usage();
    exit(1);
}

latex_head();

# print each file
foreach(@ARGV) {
    open FILE, "<".$_ or die $!;
    print '\begin{minipage}{0.48\textwidth}' . "\n%# $_\n";

    my $count = 0;
    while (my $line = <FILE>) {   
        print texify4e( $line, $count );
        $count += 1;
    }

    close FILE;
    print '\end{minipage}\newline'."\n".'\newline'."\n".'\newline'."\n";
}

latex_foot();
