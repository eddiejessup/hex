for inF in test/input/*.tex; do
    echo $inF

    outF=${inF/input/output}
    outF=${outF/.tex/.dvi}
    echo $outF

    stack exec hex -- $inF --output=$outF

    outPDF=${outF/.dvi/.pdf}
    echo $outPDF
    dvipdf $outF $outPDF
done
