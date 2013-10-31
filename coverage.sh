minimumTopLevel=100
minimumExpressions=95
minimumAlternatives=95

excludes=`eval find test -name \*.hs | \
   sed -e 's/test\///g' | \
   sed -e 's/\//./g' | \
   sed -e 's/.hs//g' | \
   sed -e 's/\(.*\)/--exclude=\1/g' | \
   xargs`

markup=`eval echo $excludes | \
   sed -e 's/\(.*\)/hpc markup \1 --destdir=code-coverage Spec.tix/g'`
$markup

report=`eval echo $excludes | \
   sed -e 's/\(.*\)/hpc report \1 Spec.tix/g'`

topLevelDeclarations=`eval $report | \
   grep 'top-level' | \
   sed -r 's/ ([0-9]+)%.*/\1/g'`

expressions=`eval $report | \
   grep 'expressions' | \
   sed -r 's/ ([0-9]+)%.*/\1/g'`

alternatives=`eval $report | \
   grep 'alternatives' | \
   sed -r 's/ ([0-9]+)%.*/\1/g'`

exitCode=0
if [ $topLevelDeclarations -lt $minimumTopLevel ]
   then
      echo ERROR: Top Level coverage too low:    $topLevelDeclarations%
      exitCode=$(($exitCode + 1))
fi
if [ $expressions -lt $minimumExpressions ]
   then
      echo ERROR: Expressions coverage too low:  $expressions%
      exitCode=$(($exitCode + 10))
fi
if [ $alternatives -lt $minimumAlternatives ]
   then
      echo ERROR: Alternatives coverage too low: $alternatives%
      exitCode=$(($exitCode + 100))
fi
exit $exitCode
