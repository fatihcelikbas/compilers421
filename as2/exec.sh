#!/bin/sh
/usr/local/smlnj/bin/sml <<EOF 2> /dev/null | egrep -v '\- |val it = |val use = |Standard|\[(linking|library|loading|scanning)'
    CM.make "sources.cm";
    Parse.parse "$1";
EOF
