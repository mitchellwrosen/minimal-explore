#!/bin/bash
minimumTopLevel=100
minimumExpressions=95
minimumAlternatives=95

function excludeDir {
   excludes=`eval find $1/$2 -name \*.hs | \
      sed -e "s/$1\///g" | \
      sed -e 's/\//./g' | \
      sed -e 's/.hs//g' | \
      sed -e 's/\(.*\)/--exclude=\1/g' | \
      xargs`
   echo $excludes
}
levelExcludes=`eval excludeDir "src" "Levels"`
testExcludes=`eval excludeDir "test"`
excludes=`eval echo $levelExcludes $testExcludes`

markup=`eval echo $excludes | \
   sed -e 's/\(.*\)/hpc markup \1 --destdir=code-coverage Spec.tix/g'`
$markup

report=`eval echo $excludes | \
   sed -e 's/\(.*\)/hpc report \1 Spec.tix/g'`

function coverage {
   val=`eval $report | \
      grep "$1" | \
      sed -r 's/ ([0-9]+)%.*/\1/g'`
   echo $val
}

topLevelDeclarations=`eval coverage "top-level"`
expressions=`eval coverage "expressions"`
alternatives=`eval coverage "alternatives"`

exitCode=0
if [ $topLevelDeclarations -lt $minimumTopLevel ]
   then
      echo Top Level coverage:    $topLevelDeclarations%
      exitCode=$(($exitCode + 1))
fi
if [ $expressions -lt $minimumExpressions ]
   then
      echo Expressions coverage:  $expressions%
      exitCode=$(($exitCode + 10))
fi
if [ $alternatives -lt $minimumAlternatives ]
   then
      echo Alternatives coverage: $alternatives%
      exitCode=$(($exitCode + 100))
fi
xdg-open code-coverage/hpc_index.html
exit $exitCode
