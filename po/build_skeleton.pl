#!/usr/bin/env perl

use strict;

## If set to 1, all translations are set as empty. Otherwise, the translation
## is the same as the message itself
my ($empty_translation) = 1;

my (%strings);
my (@modules);
my ($msg);

# Find the list of modules
opendir (DIR, "..");
foreach (readdir (DIR)) {
  if (-d "../$_/src") {
     push (@modules, "../$_/src");
  }
}
close (DIR);

# Parse each of their source files

sub process_modules() {
  local (*DIR, *FILE);
  my ($module, $file, $contents, @matches, $str);

  foreach $module (@modules) {
     opendir (DIR, $module);
     while (($file = readdir(DIR))) {
        if ($file =~ /\.ad[bs]$/) {
           open (FILE, "$module/$file");
           $contents = join ("", <FILE>);

           # Single-line strings
           @matches = ($contents =~ /-"([^"\n]+)"/gso);
           foreach $str (@matches) {
             ${$strings{$str}}{$file}++; #  .= "$file ";
           }

           # Multi-line strings: we need to concatenate
           @matches = ($contents =~ /-\("([^)]+)/gso);
           foreach $str (@matches) {
              $str =~ s/(ASCII\.)?LF/"\\n\"\n  \""/g;
              $str =~ s/"\s*&\s*"//g;
              $str =~ s/"\s*$//g;
              ${$strings{$str}}{$file}++; #  .= "$file ";
           } 
           close (FILE); 
        }
     }

     closedir (DIR);
  }
}


&process_modules;

my ($date) = `date +'%Y-%m-%d'`;
chomp ($date);

print <<EOF
# Translation file for the GNAT Programming Studio
# Copyright (C) 2005 AdaCore
#
msgid ""
msgstr ""
"Project-Id-Version: GPS 3.1.0\\n"
"Report-Msgid-Bugs-To: report\@adacore.com\\n"
"POT-Creation-Date: $date\\n"
"PO-Revision-Date: \\n"
"Last-Translator: AdaCore\\n"
"Language-Team: \\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=ISO-8859-1\\n"
"Content-Transfer-Encoding: 8bit\\n"

EOF
  ;

foreach $msg (sort {uc($a) cmp uc($b)} keys %strings) {
   print "#: ", join (" ", keys %{$strings{$msg}}), "\n";
   print "msgid \"$msg\"\n";
   if ($empty_translation) {
      print "msgstr \"\"\n\n";
   } else {
      print "msgstr \"$msg\"\n\n";
   }
}
