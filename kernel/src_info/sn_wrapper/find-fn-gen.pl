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

my @tables = parse (read_file "tables.dat");
open (ADB_FILE, ">-") or die "$!";

foreach my $table (@tables) {
   generate_find_fns (\*ADB_FILE, $table, 0);
}
close (ADB_FILE);

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
  print $file "   -----------\n   -- Find --\n   ----------";
  print $file "\n", ' 'x3, "--  Find functions for $table->{'description'} table\n\n";
  # form argument list
  print $file ' 'x3, "function Find\n",' 'x5,"(DB : DB_File;\n";
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
      $sarg =~ s/_(.)/_\u$1/g;
      print $file ' 'x6, "$sarg : $type_str := $def_val";
      print $file ";\n" if ( $arg ne ${$table->{'keys'}}[$#{$table->{'keys'}}] );
      push (@args, { 'name' => $sarg, 'type' => $type_str, 'def_value' => $def_val });
  }
  if ( $spec ) {
     print $file ") return \U$table->{'suffix'}\E_Table;\n";
  } else {
     print $file ") return \U$table->{'suffix'}\E_Table\n",' 'x3,"is\n";
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
                  print $file ' 'x12, "Len := Len + $args[$j]{'name'}'Length + 1;\n";
              } else {
                  print $file ' 'x12, "Len := Len + $args[$j]{'name'}'Length;\n";
              }
           } elsif ( $args[$j]{'type'} eq 'Point' ) {
              if ( $j != $#args ) {
                 print $file ' 'x12, "Len := Len + 11;\n";
              } else {
                 print $file ' 'x12, "Len := Len + 10;\n";
              }
           } elsif ( $args[$j]{'type'} eq 'Symbol_Type') {
              if ( $j != $#args ) {
                  print $file ' 'x12, "--  Symbol type is 3 letters at most\n";
                  print $file ' 'x12, "Len := Len + 3 + 1;\n";
              } else {
                  print $file ' 'x12, "--  Symbol type is 3 letters at most\n";
                  print $file ' 'x12, "Len := Len + 3;\n";
              }
           } else {
              die "Unknown type: $args[$j]{'type'}, can't find length of its string representation";
           }
           print $file ' 'x9, "end if;\n", ' 'x6, "end if;\n";
       }
       print $file "\n", ' 'x6, "Key := new String (1 .. Len);\n", ' 'x6, "Fall := False;\n\n";
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
              print $file ' 'x12, "To_String ($args[$j]{'name'}, Key.all, Pos);\n";
           }

           if ( $j != $#args ) {
               print $file ' 'x12, "Key (Pos) := Field_Sep;\n";
               print $file ' 'x12, "Pos := Pos + 1;\n";
           }
           print $file ' 'x9, "end if;\n", ' 'x6, "end if;\n";
       }
       print $file <<"EOS";
      Set_Cursor (DB, By_Key, Key.all, False);
      Free (Key);
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

