BEGIN {
    st = 0
    warnings = 0
    styles = 0
    print "switches_comments={"
}

# This script handles a state machine represented by 'st'
# st == 0) analysis not started yet
# st == 1) first -gnatw switch seen
# st == 2) analysis of the short description
# st == 3) analysis of the full description
# st == 4) end of analysis

# the order the analysis is done make the switch goes to the following states:
# 0 => 1 => 2 => 3 => 2 => 3 => 2 ... => 3 => 4

# start the analysis when we encounter "@item -gnatwa"
st == 0 && /^@item -gnatwa/ {
    warnings = 1
    st = 1
}

# upon a new item, we first insert the trailer of the preceding item
# we don't do this for the first item (i.e. st == 1)
warnings == 1 && st > 1 && st < 4 && /^@item -gnatw/ {
    printf("\"\"\"],\n")
}

# new item detected, start a new python dictionary item.
warnings == 1 && st > 0 && st < 4 && /^@item -gnatw/ {
    gsub (/@item /, "")
    printf("   '%s': [\n",$0)
    st = 2
}

# new item detected, but not a -gnatw one. Stop analysis here (st = 4)
warnings == 1 && st > 0 && st < 4 && /^@item [\^]?-.*/ && $1!~"@item -gnatw" {
    print "\"\"\"],"
    warnings = 0
    st = 0
}

# print the short comment. Do this after all previous handling of st == 3, as
# we don't want them to analyse the current string.
warnings == 1 && st == 2 && /^@emph{/ {
    sub (/@emph{/, "         \"\"\"",$0)
    sub (/}$/, "\"\"\", \n         \"\"\"",$0)
    printf ("%s", $0)
    prev=""
    itemize=0
    $0=""
    st = 3
}

styles == 1 && /^@item -/ {
    print "\"\"\"]}\n"
    styles = 0
    st = 0
}

st == 0 && /^@node Style Checking/ {
    st = 1
    styles = 1
}

styles == 1 && st > 1 && st < 4 && /^@item / {
    printf("\"\"\"],\n")
}

styles == 1 && /\^[^^]*\^[^^]*\^/ {
    sub (/[\^]/,"",$0)
    sub (/\^[^^]*\^/,"", $0)
}

styles == 1 && /@item / {
    sub (/@item /,"", $0)
    sub (/nnn/,"",$0)
    sub (/0-9/,"",$0)
    printf("   '-gnaty%s': [\n",$0)
    st = 2
}

styles == 1 && st == 2 && /^@emph{/ {
    sub (/@emph{/, "         \"\"\"",$0)
    sub (/}$/, "\"\"\", \n         \"\"\"",$0)
    printf ("%s", $0)
    prev=""
    itemize=0
    $0=""
    st = 3
}

# if we encounter a fully empty string, then this means a forced \n
st == 3 && /^ *$/ {
    prev="\n"
}

# replace the 'itemize' texi functions with a text list.
st == 3 && /^@itemize/ {
    $0=""
    prev="\n"
    itemize=1
}

st == 3 && itemize == 1 {
    sub (/@item/,"\n * ",$0)
}

st == 3 && itemize == 1 && /^@end itemize/ {
    $0=""
    print ("\n")
    itemize=0
}

st == 3 && /^@smallexample/ {
    $0=""
    prev=""
    printf ("\n")
    example=1
}

st == 3 && /^@end smallexample/ {
    $0=""
    prev=""
    example=0
}

# perform some filtering
st == 3 {
    # remove lines with @cindex foo bar
    gsub (/ *@cindex .*/, "", $0)
    # replace <, > and " characters so that they can be added later to xml
    gsub (/</, "\\&lt;", $0)
    gsub (/>/, "\\&gt;", $0)
    gsub (/\"/, "'", $0)
    gsub (/@dots{}/,"..",$0)
    # format "This warning blah blah" and "The default blah blah" with
    # leading \n\n
    gsub (/This warning/, "\n\n&", $0)
    gsub (/The default/, "\n\n&", $0)
    # remove all @foo{}
    gsub (/@[^ {]*{/,"",$0)
    gsub (/}/,"",$0)
    gsub (/@ifclear [^ ]*/,"",$0)
    gsub (/@end [^ ]*/,"",$0)
    gsub (/@[^ ]*/,"",$0)
    prev=""
}

# if smallexample, print raw text
st == 3 && example == 1 {
    print
    prev=""
}

# actually print the full comment
st == 3 && example == 0 && $0!~"^ *$"{
    # handle space duplications
    sub (/^ */," ",$0)
    sub (/ *$/,"",$0)
    printf ("%s%s", prev, $0)
    prev=""
}
