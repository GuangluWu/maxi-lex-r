#!/bin/bash
for f in `ls response | grep -v "\..*\.txt$"`
do
    thisfile=response/$f
    ./parsefile.sh $thisfile
done
R --no-save < preprocess.R
R --no-save < lex_responses.R
R --no-save < lex_activation_over_time.R
R --no-save < bysession.R
R --no-save < lex_accuracy_rts.R
R --no-save < lex_session_info.R
gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -sOutputFile=bysess.pdf bysession/*.pdf
