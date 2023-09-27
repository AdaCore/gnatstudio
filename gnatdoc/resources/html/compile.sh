#!/bin/sh


files="gnatdoc.js"

# gjslint is provided, in macports, in the package 'closure-linter'
# see also  https://developers.google.com/closure/utilities/docs/linter_howto?hl=fr
gjslint "$files"

if [ ! -f compiler.jar ]; then

   # This version requires java 7
   #    rm -f compiler-latest.zip
   #    wget http://dl.google.com/closure-compiler/compiler-latest.zip
   #    unzip compiler-latest.zip compiler.jar
   #    rm -f compiler-latest.zip

   # This version works with java 6
   rm -f compiler-20131014.zip
   wget --no-check-certificate https://closure-compiler.googlecode.com/files/compiler-20131014.zip
   unzip compiler-20131014.zip compiler.jar
   rm -f compiler-20131014.zip
fi


# See documentation for the annotations at
#   https://developers.google.com/closure/compiler/docs/js-for-compiler
# and type descriptions at
#   https://developers.google.com/closure/compiler/docs/js-for-compiler#types

java -jar compiler.jar --jscomp_error=accessControls  --jscomp_error=ambiguousFunctionDecl --jscomp_error=checkRegExp --jscomp_error=checkTypes --jscomp_error=checkVars --jscomp_error=constantProperty --jscomp_error=deprecated --jscomp_error=es5Strict --jscomp_error=externsValidation --jscomp_error=fileoverviewTags --jscomp_error=globalThis --jscomp_error=internetExplorerChecks --jscomp_error=invalidCasts --jscomp_error=missingProperties --jscomp_error=nonStandardJsDocs --jscomp_error=strictModuleDepCheck --jscomp_error=typeInvalidation --jscomp_error=undefinedVars --jscomp_error=unknownDefines --jscomp_error=uselessCode --jscomp_error=visibility --warning_level=VERBOSE --js_output_file=compiled.js --process_closure_primitives "$files"
