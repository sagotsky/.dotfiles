# _fd borrowed from /etc/bash_completion's _cd
# _fd_filedir borrowed from /etc/bash_completion's_filedir, but
# uses find instead of compgen to get several directories deep
_fd()
{
    local cur IFS=$'\t\n' i j k
    _get_comp_words_by_ref cur

    # try to allow variable completion
    if [[ "$cur" == ?(\\)\$* ]]; then
        COMPREPLY=( $( compgen -v -P '$' -- "${cur#?(\\)$}" ) )
        return 0
    fi

    # Enable -o filenames option, see Debian bug #272660
    compgen -f /non-existing-dir/ >/dev/null

    # Use standard dir completion if no CDPATH or parameter starts with /,
    # ./ or ../
    if [[ -z "${CDPATH:-}" || "$cur" == ?(.)?(.)/* ]]; then
        _fd_filedir -d
        return 0
    fi

    local -r mark_dirs=$(_rl_enabled mark-directories && echo y)
    local -r mark_symdirs=$(_rl_enabled mark-symlinked-directories && echo y)

    # we have a CDPATH, so loop on its contents
    for i in ${CDPATH//:/$'\t'}; do
        # create an array of matched subdirs
        k="${#COMPREPLY[@]}"
        for j in $( compgen -d $i/$cur ); do
            if [[ ( $mark_symdirs && -h $j || $mark_dirs && ! -h $j ) && ! -d ${j#$i/} ]]; then
                j="${j}/"
            fi
            COMPREPLY[k++]=${j#$i/}
        done
    done

    _fd_filedir -d

    if [[ ${#COMPREPLY[@]} -eq 1 ]]; then
        i=${COMPREPLY[0]}
        if [[ "$i" == "$cur" && $i != "*/" ]]; then
            COMPREPLY[0]="${i}/"
        fi
    fi

    return 0
}


_fd_filedir()
{
    local i IFS=$'\t\n' xspec

    __expand_tilde_by_ref cur

    local -a toks
    local quoted tmp

    _quote_readline_by_ref "$cur" quoted
  
    FIND_LIMIT=150
    dirs=$(find ./ -name "${cur}*" -type d -printf '%f\n' -nowarn 2>/dev/null | head -n $FIND_LIMIT) 
    echo -e "\ndirs: $dirs\n"  >> /tmp/comp.log

    toks=( ${toks[@]-} $(
        #compgen -d -- "$quoted" | {
        echo "$dirs" | {
            while read -r tmp; do
                printf '%s\n' $tmp
            done
        }
    ))

    # On bash-3, special characters need to be escaped extra.  This is
    # unless the first character is a single quote (').  If the single
    # quote appears further down the string, bash default completion also
    # fails, e.g.:
    #
    #     $ ls 'a&b/'
    #     f
    #     $ foo 'a&b/<TAB>  # Becomes: foo 'a&b/f'
    #     $ foo a'&b/<TAB>  # Nothing happens
    #
    if [[ "$1" != -d ]]; then
        xspec=${1:+"!*.$1"}
        if [[ ${cur:0:1} == "'" && ${BASH_VERSINFO[0]} -ge 4 ]]; then
            toks=( ${toks[@]-} $(
                eval compgen -f -X \"\$xspec\" -- $quoted
            ) )
        else
            toks=( ${toks[@]-} $(
                compgen -f -X "$xspec" -- $quoted
            ) )
        fi
        if [ ${#toks[@]} -ne 0 ]; then
            # If `compopt' is available, set `-o filenames'
            compopt &>/dev/null && compopt -o filenames ||
            # No, `compopt' isn't available;
            # Is `-o filenames' set?
            [[ (
                ${COMP_WORDS[0]} && 
                "$(complete -p ${COMP_WORDS[0]})" == *"-o filenames"*
            ) ]] || {
                # No, `-o filenames' isn't set;
                # Emulate `-o filenames'
                # NOTE: A side-effect of emulating `-o filenames' is that
                #       backslash escape characters are visible within the list
                #       of presented completions, e.g.  the completions look
                #       like:
                #
                #           $ foo a<TAB>
                #           a\ b/  a\$b/
                #
                #       whereas with `-o filenames' active the completions look
                #       like:
                #
                #           $ ls a<TAB>
                #           a b/  a$b/
                #
                for ((i=0; i < ${#toks[@]}; i++)); do
                    # If directory exists, append slash (/)
                    if [[ ${cur:0:1} != "'" ]]; then
                        [[ -d ${toks[i]} ]] && toks[i]="${toks[i]}"/
                        if [[ ${cur:0:1} == '"' ]]; then
                            toks[i]=${toks[i]//\\/\\\\}
                            toks[i]=${toks[i]//\"/\\\"}
                            toks[i]=${toks[i]//\$/\\\$}
                        else
                            toks[i]=$(printf %q ${toks[i]})
                        fi
                    fi
                done
            }
        fi
    fi

    COMPREPLY=( "${COMPREPLY[@]}" "${toks[@]}" )
} # _filedir()

complete -F _fd fd

