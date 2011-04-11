#!/usr/bin/env bash
# =============================================================================
# @doc (very) Simple (continous) Files Monitor
# @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
# @since 2011-04-11 
# =============================================================================

# get arguments
while [ "$1" != "" ] ; do
    case "$1" in
        --command) var=command ;;
        --args) var=args ;;
        --files) var=files ;;
        *) eval x=\$$var
           eval "$var=\"$1 $x\""
    esac
    shift
done

# specifu the test arguments
if [ "$eunit" != "" ] ; then
    args="eunit=$eunit"
fi
if [ "$ct" != "" ] ; then
    args="$args ct=$ct"
fi
echo "sbt> running $command $args iff some change happens in/to $files"

# get files & directories to watch
to_watch=
for f in $files ; do
    if [ -f $f -o -d $f ]; then to_watch="$to_watch $f"; fi
done

while true ; do
    echo '---------------------- test run ----------------------'
    $command $args
    echo
    echo
    echo '----------------- waiting for change -----------------'
    EVENT=$(inotifywait -r -e close_write --format '%e' $to_watch)
done