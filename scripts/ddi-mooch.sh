#!/bin/sh

# Downloads entire DDI compendium as text files.

CONCURRENT_DOWNLOADS=25
CATEGORIES="Background Class Companion Monster Deity Disease EpicDestiny Feat Glossary Item ParagonPath Poison Power Race Ritual Terrain Trap"

function usage() {
cat <<-EOF

Usage: ddi-yoink.sh /path/to/cookies.txt
Where cookies.txt is a cookies file from a browser that has logged into DDI.
Firefox has an addon for exporting this file, Cookie Exporter.
https://addons.mozilla.org/en-US/firefox/addon/cookie-exporter/

Open ddi-yoink.sh and edit CONCURRENT_DOWNLOADS variable to adjust how many
files are downloaded simultaneously.

EOF

exit
}

function getIDs() {
    CAT=$1
    URL="http://www.wizards.com/dndinsider/compendium/CompendiumSearch.asmx/ViewAll?tab="
    curl -s "$URL$CAT" | grep "<ID>" | sed -e 's/[^0-9]//g'
}

function download() {
    COOKIES=$1
    URL=$2
    CATEGORY=$(echo $URL | sed -e 's/.*\///' | sed -e 's/.aspx.*//')
    ID=$(echo $URL | sed -e 's/.*=//')

    HTML=$(curl -s -b $COOKIES $URL)
    TITLE=$(\
        echo $HTML |\
        w3m -T text/html -dump -cols 20000 |\
        head -n 1 |\
        tr " " "_" |\
        tr "/" "_" |\
        tr -d "'" |\
        tr -d "’"
    )
    LEVEL=$(\
        echo $HTML |\
        w3m -T text/html -dump -cols 20000 |\
        head -n 3 |\
        grep Level |\
        awk '{print $2}'
    )

    [[ $( echo $LEVEL | wc -c ) -gt 1 ]] && LEVEL="[$LEVEL]"
    FILE="DDI/$CATEGORY/$TITLE$LEVEL"

    if [[ $TITLE == "Server_Error_in_/_Application." ]] || [[ $TITLE == "Object_moved_to_here." ]] ; then
        echo "-Skipping $CATEGORY #$ID"
    else
        echo "+Downloading $CATEGORY #$ID: $TITLE"
        [[ -z $FILE ]] && FILE="$FILE_"
        echo $HTML |\
            w3m -T text/html -dump -cols 20000 |\
            sed -e 's/\[x\] //g' |\
            sed -e 's/\[...\?\] //g' |\
            sed -e 's/\[\([1-6]\).\]/\1/g' |\
            sed -e 's/recharge 6$/recharge 6\)/' \
            > "$FILE.4e.txt"
    fi
}

export -f download

[[ $# -eq 1 ]] || usage

mkdir 'DDI'
for cat in $CATEGORIES ; do
    for id in $(getIDs $cat) ; do
        cat=$(echo $cat |  tr '[:upper:]' '[:lower:]')
        [[ -d "DDI/$cat" ]] || mkdir "DDI/$cat"
        echo "http://www.wizards.com/dndinsider/compendium/$cat.aspx?id=$id"
    done
done | xargs -n 1 -P $CONCURRENT_DOWNLOADS -I {} bash -c download\ $1\ \{\}

