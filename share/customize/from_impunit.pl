#!/usr/bin/env perl


$output="runtime.xml";
$impunit="impunit.adb";

open (OUT, '>' . $output);
open (IMPUNIT, $impunit);

print OUT "<?xml version=\"1.0\" ?>\n<doc>\n";

foreach $line (<IMPUNIT>) {
   chomp ($line);
   if ($line =~ /"([^"]+)",\s*-- (.*)/) {
       $filename=$1;
       $unit=$2;
       $unit =~ s/_/__/g;
       ($hierarchy) = ($unit =~ /^([^.]+)\./);

       print OUT "<documentation_file>
   <shell>Editor.edit \"$filename.ads\"</shell>
   <descr>$unit</descr>
   <menu>/Help/GNAT Runtime/$hierarchy/$unit</menu>
   <category>GNAT Runtime</category>
</documentation_file>\n";
   }

}

print OUT "</doc>";

close (IMPUNIT);
close (OUT);
