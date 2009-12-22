#
# usage
#
# python <path to gnat sources>
#
# generates five files:
#
# fixed.out -> errors messages known as fixed (marked --  CODEFIX in gnat
#    sources)
# to_be_fixed.out -> errors messages marked as potentially being automatically
#    fixed (marked --  CODEFIX ??? in gnat sources)
# known_unfixable.out -> erors messages that have been marked as not to be
#    fixed, patterns extracted from the file known_unfixable.txt, installed
#    with this script
# unknown.out -> messages that have not been yet analyzed
# junk.out -> lines that looked like errors messages but couldn't be retreive
#    as such by advanced analysis - may be interresting things there

import sys, os, re, StringIO

nonFixableMessages = []

fixedOut = open ("fixed.out", "w")
toBeFixedOut = open ("to_be_fixed.out", "w+")
knownUnfixableOut = open ("known_unfixable.out", "w+")
unknownOut = open ("unknown.out", "w+")
junkOut = open ("junk.out", "w+")

###############
# analyseCall #
###############

def analyzeCall (call, fileName, line):

   global nonFixableMessages
   global fixedOut
   global toBeFixedOut
   global knownUnfixableOut
   global unknownOut
   global junkOut

   basename = re.search ("[\w-]+\.adb", fileName).group (0)

   param = re.sub ("(\"[\s]*&[\s]*\")", "", call)
   param = re.sub ("\n", "", param)
   param = re.sub ("^[^\(]*\(", "", param)
   param = re.sub ("\)[^\)]*$", "", param)
   param = re.sub (",[^\"]*$", "", param)

   if re.match ("^\".*\"$", param) == None:
      junkOut.write (basename + ":" + str (line) + ": " + param + "\n")
      return

   message = re.sub ("^\"", "", param)
   message = re.sub ("\"$", "", message)

   codefixed = re.search ("CODEFIX", call) != None
   codefixable = not codefixed and re.search ("CODEFIX ?\?", call) != None

   if param != None:
      if codefixed:
         fixedOut.write (basename + ":" + str (line) + ": " + message + "\n")
      elif codefixable:
         toBeFixedOut (basename + ":" + str (line) + ": " + message + "\n")
      else:
         found = False

         for pattern in nonFixableMessages:
            if pattern.search (message) != None:
               found = True

         if found:
            knownUnfixableOut.write (basename + ":" + str (line) + ": " + message + "\n")
         else:
            unknownOut.write (basename + ":" + str (line) + ": " + message + "\n")

###############
# analyseFile #
###############

def analyzeFile (fileName):
   matchingName = "Error_Msg"
   matchingIndex = 0
   matchStr = ""
   currentLine = 1

   inString = False
   prevQuote = False

   MODE_MATCH = 0
   MODE_RETREIVE = 1
   mode = MODE_MATCH

   file = open (fileName);

   contentStr = file.read ()

   for c in contentStr:
      if c == '\n':
         currentLine = currentLine + 1

      if mode == MODE_MATCH:
         if matchingName [matchingIndex] == c:
            matchingIndex = matchingIndex + 1
         else:
            matchingIndex = 0

         if matchingIndex == len (matchingName):
            matchingIndex = 0
            mode = MODE_RETREIVE
            prevQuote = False
            inString = False

      elif mode == MODE_RETREIVE:
         matchStr = matchStr + c
         if c == "\"":
            if inString:
               inString = False
            elif prevQuote:
               inString = True
               prevQuote = False
            else:
               inString = not inString
               prevQuote = True

         if c == ";" and not inString:
            analyzeCall (matchStr, fileName, currentLine)
            matchStr = ""
            mode = MODE_MATCH


   file.close ()

########
# MAIN #
########

nonFixbableMessagesFile = open ("known_unfixable.txt")

for pattern in nonFixbableMessagesFile.readlines ():
   cleanPattern = re.sub ("\r|\n", "", pattern)
   try:
      nonFixableMessages.append (re.compile (cleanPattern))
   except:
      print "can't compile \"" + cleanPattern + "\""

dirName = sys.argv [1]

for fileName in os.listdir (dirName):
   if re.match ("[\w-]+\.adb", fileName):
      analyzeFile (dirName + "/" + fileName)

fixedOut.close ()
toBeFixedOut.close ()
knownUnfixableOut.close ()
unknownOut.close ()
junkOut.close ()
