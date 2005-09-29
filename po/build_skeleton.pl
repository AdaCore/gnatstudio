#!/usr/bin/env perl

use strict;

my (%strings);
my (@modules);

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
              $str =~ s/(ASCII\.)?LF/"\\n"/g;
              $str =~ s/"\s*&\s*"//g;
              $str =~ s/"\s*$//g;
              ${$strings{$str}}{$file}++; #  .= "$file ";
              # $strings{$str} .= "$file ";
           } 
           close (FILE); 
        }
     }

     closedir (DIR);
  }
}


&process_modules;

my ($msg);
foreach $msg (sort {uc($a) cmp uc($b)} keys %strings) {
   print "# From ", join ("", keys %{$strings{$msg}}), "\n";
   print "msgid  \"$msg\"\nmsgstr \"$msg\"\n\n";
}
