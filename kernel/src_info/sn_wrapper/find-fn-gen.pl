#!/usr/bin/perl -w
use strict;

#
# Description file format:
#
# <file suffix>  <description (quoted string)> <record format as-is in SN docs>
#

sub read_file ($);
sub parse ($);
sub generate_find_fns (*$$);

my %data_types = (
   'position'		=> 'Point',
   'start_position'	=> 'Point',
   'end_position'	=> 'Point',
   'sym-type'		=> 'Symbol_Type',
   'default'		=> 'String'
);

my %default_values = (
   'Point'		=> 'Invalid_Point',
   'String'     	=> 'Invalid_String',
   'Symbol_Type'	=> 'Undef'
);

##
## MAIN
##

if ( -1 == $#ARGV ) {
    print "Usage: perl find-fn-gen.pl <input-table-description.file>\n\n";
    exit (0);
}

my @tables = parse (read_file $ARGV[0]);
open (ADB_FILE, ">sn-find_fns.adb") or die "Failed to open sn-find-fns.adb";
print ADB_FILE <<EOS;
with SN.DB_Structures,
     Ada.Unchecked_Deallocation,
     DB_API;
use  SN.DB_Structures,
     DB_API;

package body SN.Find_Fns is
      procedure Delete is
         new Ada.Unchecked_Deallocation (String, String_Access);
EOS

foreach my $table (@tables) {
   generate_find_fns (\*ADB_FILE, $table, 0);
}

   print ADB_FILE <<EOS;
      procedure To_String (P : in Point; Str : in String_Access;
            Where : in out Integer) is
         Line_Img : String := Integer'Image (P.Line);
         Col_Img  : String := Integer'Image (P.Column);
      begin
         Str (Where .. Where + 9) := "000000.000";
         Str (Where + 5 - Line_Img'Length + 2 .. Where + 5)
            := Line_Img (2 .. Line_Img'Length);
         Where := Where + 7;
         Str (Where + 2 - Col_Img'Length + 2 .. Where + 2)
            := Col_Img (2 .. Col_Img'Length);
         Where := Where + 3;
      end To_String;

      procedure To_String (Sym_Type : in Symbol_Type; Str : in String_Access;
          Where : in out Integer) is
      begin
         case Sym_Type is
            when CL    => Str (Where .. Where + 1) := "cl";
                        Where := Where + 2;
            when COM   => Str (Where .. Where + 2) := "com";
                        Where := Where + 3;
            when COV   => Str (Where .. Where + 2) := "cov";
                        Where := Where + 3;
            when CON   => Str (Where .. Where + 2) := "con";
                        Where := Where + 3;
            when E     => Str (Where .. Where) := "e";
                        Where := Where + 1;
            when EC    => Str (Where .. Where + 1) := "ec";
                        Where := Where + 2;
            when FD    => Str (Where .. Where + 1) := "fd";
                        Where := Where + 2;
            when FR    => Str (Where .. Where + 1) := "fr";
                        Where := Where + 2;
            when FU    => Str (Where .. Where + 1) := "fu";
                        Where := Where + 2;
            when GV    => Str (Where .. Where + 1) := "gv";
                        Where := Where + 1;
            when IV    => Str (Where .. Where + 1) := "iv";
                        Where := Where + 2;
            when LV    => Str (Where .. Where + 1) := "lv";
                        Where := Where + 2;
            when MA    => Str (Where .. Where + 1) := "ma";
                        Where := Where + 2;
            when MD    => Str (Where .. Where + 1) := "md";
                        Where := Where + 2;
            when MI    => Str (Where .. Where + 1) := "mi";
                        Where := Where + 2;
            when SU    => Str (Where .. Where + 1) := "su";
                        Where := Where + 2;
            when T     => Str (Where .. Where) := "t";
                        Where := Where + 1;
            when UN    => Str (Where .. Where + 1) := "un";
                        Where := Where + 2;
            when IU    => Str (Where .. Where + 1) := "iu";
                        Where := Where + 2;
            when others => raise Invalid_Symbol_Type;
         end case;
      end To_String;
end SN.Find_Fns;

EOS
close (ADB_FILE);

open (ADS_FILE, ">sn-find_fns.ads") or die "Failed to open sn-find-fns.ads";
print ADS_FILE <<EOS;
with SN.DB_Structures,
     DB_API;
use  SN.DB_Structures,
     DB_API;

package SN.Find_Fns is
      Not_Found           : exception;
      --  raised by the find function when key does
      --  not correspond to a variable

      Invalid_Symbol_Type : exception;
      --  raised when a bad symbol passed to To_String function

      procedure To_String (Sym_Type : in Symbol_Type; Str : in String_Access;
          Where : in out Integer);
      --  converts symbol type into string

      procedure To_String (P : in Point; Str : in String_Access;
                           Where : in out Integer);
      --  converts Point to 000000.000 string

EOS

foreach my $table (@tables) {
   generate_find_fns (\*ADS_FILE, $table, 1);
}

   print ADS_FILE <<EOS;
end SN.Find_Fns;

EOS
close (ADS_FILE);

exit (0);

##
## End MAIN
##


#
# Reads whole file into memory
#
# INPUT:   filename
# OUTPUT:  file content as a string
#
sub read_file ($) {
  local *FILE;
  my ($fname) = @_;
  open (FILE, $fname) or die "Failed to open file $fname";
  seek (FILE, 0, 2);
  my $length = tell (FILE);
  seek (FILE, 0, 0);
  my $data;
  read (FILE, $data, $length);
  close (FILE);
  return $data;
}

#
# Parses table descriptions
#
# INPUT:   descriptions as a string
# OUTPUT:  parsed tree of data
#
sub parse ($) {
  my ($str) = @_;
  my @raw_lines = split (/\n/, $str);
  my @lines = ();
  my $concat = 0;

  # join extended lines
  foreach my $line (@raw_lines) {
      if ( $line =~ /^(.*)\\\s*$/ ) {
          push (@lines, $concat ? (pop @lines).$1 : $1);
          $concat = 1;
      } else {
          $line =~ /^\s*(.*)$/;
          push (@lines, $concat ? (pop @lines).$1 : $1);
          $concat = 0;
      }
  }

  my @tables = ();
  foreach my $line (@lines) {
     next if ( $line =~ /^\s*#/ || $line =~ /^\s*$/ );
     if ( $line =~ /^\s*(\w+)\s+["']([^"']+)["']\s+(.*)$/ ) {
         my $table = { 'suffix' => $1, 'description' => $2 };
         my ($key, $data) = split (/\s*;\s*/, $3);
         my @key_fields   = split (/\s*\?\s*/, $key);
         my @data_fields  = split (/\s*\?\s*/, $data);
         $table->{'keys'} = [];
         $table->{'data'} = [];
         push (@{$table->{'keys'}}, @key_fields);
         push (@{$table->{'data'}}, @data_fields);
         push (@tables, $table);
     } else {
         die "Extra characters in line:\n$line";
     }
  }
  return @tables;
}

#
# Generates find functions for a single table
#
#  INPUT:   file handle, table structure, specification flag
#  OUTPUT:  none
#
sub generate_find_fns (*$$) {
  my ($file, $table, $spec) = @_;
  print $file "\n", ' 'x6, "--  Find functions for $table->{'description'} table\n";
  # form argument list
  print $file "      function Find (DB : DB_File;\n";
  my @args = ();
  foreach my $arg (@{$table->{'keys'}}) {
      my $type_str = exists $data_types {$arg} ? $data_types {$arg} 
                                               : $data_types {'default'};
      my $sarg = $arg;
      my $def_val = exists $default_values {$type_str} ? $default_values {$type_str}
                              : die "No default value for type: $type_str";
      $sarg =~ s/-/ /g;
      $sarg =~ s/\b(.)/\u$1/g;
      $sarg =~ s/ /_/g;
      print $file ' 'x12, "$sarg : $type_str := $def_val";
      print $file ";\n" if ( $arg ne ${$table->{'keys'}}[$#{$table->{'keys'}}] );
      push (@args, { 'name' => $sarg, 'type' => $type_str, 'def_value' => $def_val });
  }
  if ( $spec ) {
     print $file ")\n      return \U$table->{'suffix'}\E_Table;\n";
  } else {
     print $file ")\n      return \U$table->{'suffix'}\E_Table is\n";
     print $file <<"EOS";
         P    : Pair_Ptr;
         Tab  : \U$table->{'suffix'}\E_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
EOS
       # count pass
       for ( my $j = 0; $j <= $#args; ++$j ) {
           print $file <<"EOS";
         if not Fall then
            if $args[$j]{'name'} = $args[$j]{'def_value'} then
               Fall := True;
            else
EOS
           if ( $args[$j]{'type'} eq 'String' ) {
              if ( $j != $#args ) {
                  print $file ' 'x15, "Len := Len + $args[$j]{'name'}'Length + 1;\n";
              } else {
                  print $file ' 'x15, "Len := Len + $args[$j]{'name'}'Length;\n";
              }
           } elsif ( $args[$j]{'type'} eq 'Point' ) {
              if ( $j != $#args ) {
                 print $file ' 'x15, "Len := Len + 11;\n";
              } else {
                 print $file ' 'x15, "Len := Len + 10;\n";
              }
           } elsif ( $args[$j]{'type'} eq 'Symbol_Type') {
              if ( $j != $#args ) {
                  print $file ' 'x15, "--  Symbol type is 3 letters at most\n";
                  print $file ' 'x15, "Len := Len + 3 + 1;\n";
              } else {
                  print $file ' 'x15, "--  Symbol type is 3 letters at most\n";
                  print $file ' 'x15, "Len := Len + 3;\n";
              }
           } else {
              die "Unknown type: $args[$j]{'type'}, can't find length of its string representation";
           }
           print $file ' 'x12, "end if;\n", ' 'x9, "end if;\n";
       }
       print $file "\n", ' 'x9, "Key := new String (1 .. Len);\n", ' 'x9, "Fall := False;\n\n";
       for ( my $j = 0; $j <= $#args; ++$j ) {
           print $file <<"EOS";
         if not Fall then
            if $args[$j]{'name'} = $args[$j]{'def_value'} then
               Fall := True;
            else
EOS
           if ( $args[$j]{'type'} eq 'String' ) {
              print $file <<"EOS";
               Key (Pos .. Pos + $args[$j]{'name'}'Length - 1)
                  := $args[$j]{'name'};
               Pos := Pos + $args[$j]{'name'}'Length;
EOS
           } else {
              print $file ' 'x15, "To_String ($args[$j]{'name'}, Key, Pos);\n";
           }

           if ( $j != $#args ) {
               print $file ' 'x15, "Key (Pos) := Field_Sep;\n";
               print $file ' 'x15, "Pos := Pos + 1;\n";
           }
           print $file ' 'x12, "end if;\n", ' 'x9, "end if;\n";
       }
       print $file <<"EOS";
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         Release_Cursor (DB);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;


EOS
    }
}

