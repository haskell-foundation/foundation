#!/bin/bash

EDITDIFFTOOL=vimdiff
DIFFTOOL=diff

PREF=basement/Basement/Alg/Native

case $1 in
editdiff)
    if [ ! -f $PREF/$2 ]; then echo "$2 doesn't exist"; exit 1; fi
    $EDITDIFFTOOL $PREF/$2 ${PREF/Native/Foreign}/$2 
    ;;
editdiff)
    if [ ! -f $PREF/$2 ]; then echo "$2 doesn't exist"; exit 1; fi
    $DIFFTOOL $PREF/$2 ${PREF/Native/Foreign}/$2 
    ;;
gen)
    for file in $PREF/*.hs
    do
        dest=${file/Native/Foreign}
        if [ "$file" != "$PREF/Prim.hs" ]; then
            echo "GENERATING $dest from $file"
            sed -n '
/Basement.Alg.Native./ !{
    p
}
/Basement.Alg.Native./ {
    /NO SUBST/ {
        p
    }
    /NO SUBST/ !{
        s;Basement.Alg.Native.;Basement.Alg.Foreign.;
        p
    }
}' $file > $dest
        fi
    done
    ;;
*)
    echo "unknown mode: $1"
    echo "expecting editdiff, diff, gen"
    exit 1
    ;;
esac

