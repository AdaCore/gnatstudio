-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Regpat;

package body GNATStack.CI_Utilities is

   use type GNAT.Regpat.Match_Location;

   Node_Regexp : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile
       ("^node: { title: ""([^""]+)"" label: ""([^""]+)"" }");

   Location_Regexp : constant String :=
     "(<no location>)|" &  -- Encoding for no location
     "(<built-in>.*|<internal>.*)|" &  -- Location for built-in functions
     "(((\\\\|[a-zA-Z]:[\\/])?[\w\\/ \.~\-\+]+)" &  -- Normal locations
     ":(\d+)(:(\d+))?)";  -- line and possibly column number
   --  Accepted locations are Unix-style (directories separated by slashes),
   --  Windows UNC-style (drive_letter:\path or \\server\path), and
   --  cygwin-style (drive_letter:/path or /path). There is also .extension
   --  for the file, and file location (:line[:column]). Note that sometimes,
   --  on cygwin, locations have a pattern with mixed / and \, such as
   --  /path\file.ext.

   Stack_Regexp : constant String :=
     "((\d+) bytes( \(static\)| \(dynamic\)| \(dynamic,bounded\))?)|"
       & "(XXX bytes)";
   --  At least the size in bytes

   Node_Label_Regexp : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile
       ("^((&|\*|\*\*|\+|\-|/|/=|<|<=|>|>=|=|" &  -- Unary operators
        "[\w \(\):,\*\&<>\[\]=~\+\-\|!\{\}\$\?/\^]*)\\n)?" &  -- Demangled name
        "((" & Location_Regexp & ",?)*(\\n)?)?" &
        "(" & Stack_Regexp & ")?$");
   --  Extra information about a node

   type String_Access is access all String;

   type Operator_Pair is record
      Source : String_Access;
      Target : String_Access;
   end record;
   --  Type that contains pairs corresponding to encoded operator names (in
   --  the field Source) and their Ada counterpart (in the field Target).

   Conversion_Table : constant array (Positive range <>) of Operator_Pair :=
     (Operator_Pair'(new String'("Oabs"),      new String'("""abs""")),
      Operator_Pair'(new String'("Oand"),      new String'("""and""")),
      Operator_Pair'(new String'("Omod"),      new String'("""mod""")),
      Operator_Pair'(new String'("Onot"),      new String'("""not""")),
      Operator_Pair'(new String'("Oor"),       new String'("""or""")),
      Operator_Pair'(new String'("Orem"),      new String'("""rem""")),
      Operator_Pair'(new String'("Oxor"),      new String'("""xor""")),
      Operator_Pair'(new String'("Oeq"),       new String'("""=""")),
      Operator_Pair'(new String'("One"),       new String'("""/=""")),
      Operator_Pair'(new String'("Olt"),       new String'("""<""")),
      Operator_Pair'(new String'("Ole"),       new String'("""<=""")),
      Operator_Pair'(new String'("Ogt"),       new String'(""">""")),
      Operator_Pair'(new String'("Oge"),       new String'(""">=""")),
      Operator_Pair'(new String'("Oadd"),      new String'("""+""")),
      Operator_Pair'(new String'("Osubtract"), new String'("""-""")),
      Operator_Pair'(new String'("Oconcat"),   new String'("""&""")),
      Operator_Pair'(new String'("Omultiply"), new String'("""*""")),
      Operator_Pair'(new String'("Odivide"),   new String'("""/""")),
      Operator_Pair'(new String'("Oexpon"),    new String'("""**""")));
   --  Table used for translating encoded operator names

   function Ada_Decode (Symbol_Name : String) return String;
   --  Return the Ada name from the encoded form. It transforms a symbol name
   --  (such as pck__proc) into its Ada counterpart (pck.proc). It also
   --  handles library level subprogram, overloaded subprograms, task bodies,
   --  and operators.

   ----------------
   -- Ada_Decode --
   ----------------

   function Ada_Decode (Symbol_Name : String) return String is
      Length          : Integer := Symbol_Name'Length;
      Buffer          : String (1 .. Length * 2);
      Matches         : GNAT.Regpat.Match_Array (0 .. 1);
      Match_Lead_File : GNAT.Regpat.Match_Array (0 .. 1);
      Match_Operator  : GNAT.Regpat.Match_Array (0 .. 2);

   begin
      Buffer (1 .. Length) := Symbol_Name;

      --  Remove the leading file information when is there. It may happen
      --  in the case of local (static) subprograms. The file name is
      --  separated by ':' such as "file.ext:symbol_name".

      GNAT.Regpat.Match ("^.*:(.*)$", Buffer (1 .. Length), Match_Lead_File);

      if Match_Lead_File (0) /= GNAT.Regpat.No_Match then
         --  Keep only the local symbol name

         Length := Match_Lead_File (1).Last - Match_Lead_File (1).First + 1;
         Buffer (1 .. Length) :=
           Buffer (Match_Lead_File (1).First .. Match_Lead_File (1).Last);
      end if;

      --  Check for library level subprogram

      if GNAT.Regpat.Match ("^_ada_", Buffer (1 .. Length)) then
         Buffer (1 .. Length - 5) := Buffer (6 .. Length);

         Length := Length - 5;
      end if;

      --  Remove sequence number for symbols generated by the compiler .nn

      GNAT.Regpat.Match ("(\.[0-9]+)$", Buffer (1 .. Length), Matches);

      if Matches (0) /= GNAT.Regpat.No_Match then
         Length := Matches (1).First - 1;
      end if;

      --  Check for task body

      if GNAT.Regpat.Match ("TKB$", Buffer (1 .. Length)) then
         Length := Length - 3;
      elsif GNAT.Regpat.Match ("B$", Buffer (1 .. Length)) then
         Length := Length - 1;
      end if;

      --  Check for body-nested entity: X[bn]

      if GNAT.Regpat.Match ("X$", Buffer (1 .. Length)) then
         Length := Length - 1;
      elsif GNAT.Regpat.Match ("X(b|n)$", Buffer (1 .. Length)) then
         Length := Length - 2;
      end if;

      --  Change instance of TK__ (object declared inside a task) to __

      loop
         GNAT.Regpat.Match ("(TK__)", Buffer (1 .. Length), Matches);
         exit when Matches (0) = GNAT.Regpat.No_Match;

         Buffer (1 .. Length - 2) :=
           Buffer (1 .. Matches (1).First - 1) &
         "__" &
         Buffer (Matches (1).Last + 1 .. Length);

         Length := Length - 2;
      end loop;

      --  Check for overloading: name terminated by $nn or __nn.

      GNAT.Regpat.Match ("(\$[0-9]+)", Buffer (1 .. Length), Matches);

      if Matches (0) /= GNAT.Regpat.No_Match then
         Length := Matches (1).First - 1;
      end if;

      GNAT.Regpat.Match
        ("(__[0-9]+)(_[0-9]+)*$", Buffer (1 .. Length), Matches);

      if Matches (0) /= GNAT.Regpat.No_Match then
         Length := Matches (1).First - 1;
      end if;

      --  Change all "__" to ".". Do not change "__" if it at the beginning
      --  of the symbol name.

      loop
         GNAT.Regpat.Match ("[^\^](__)", Buffer (1 .. Length), Matches);
         exit when Matches (0) = GNAT.Regpat.No_Match;

         Buffer (1 .. Length - 1) :=
           Buffer (1 .. Matches (1).First - 1) &
         "." &
         Buffer (Matches (1).Last + 1 .. Length);

         Length := Length - 1;
      end loop;

      --   Checks for operator names. They appear always after a ".", and they
      --   may be either the last part of the String, such as X.Oconcat (note
      --   that overload markers have already been removed), or before block
      --   information, such as X.Oconcat.B71b._clean.2759.

      for Index in Conversion_Table'Range loop
         GNAT.Regpat.Match
           ("\.(" & Conversion_Table (Index).Source.all & ")($|\.)",
            Buffer (1 .. Length),
            Match_Operator);

         if Match_Operator (0) /= GNAT.Regpat.No_Match then
            declare
               Old_Length : constant Integer := Length;
            begin
               Length := Length -
                 (Conversion_Table (Index).Source.all'Length -
                    Conversion_Table (Index).Target.all'Length);

               Buffer (1 .. Length) :=
                 Buffer (1 .. Match_Operator (1).First - 1) &
               Conversion_Table (Index).Target.all &
               Buffer (Match_Operator (1).Last + 1 .. Old_Length);
            end;

            exit;
         end if;
      end loop;

      --  Return the decoded string

      return Buffer (1 .. Length);
   end Ada_Decode;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Data      : in out GNATStack.Data_Model.Analysis_Information;
      File_Name : String)
   is
      File        : Ada.Text_IO.File_Type;
      Line        : String (1 .. 1024);
      Last        : Natural;
      Matches     : GNAT.Regpat.Match_Array (0 .. 18);
      Identifier  : GNATStack.Data_Model.Subprogram_Identifier;
      Location    : GNATStack.Data_Model.Subprogram_Location;
      pragma Unreferenced (Location);
      CI_Data     : constant GNATStack.Data_Model.CI_Information_Access :=
                      new GNATStack.Data_Model.CI_Information'
                        (Ada.Strings.Unbounded.To_Unbounded_String (File_Name),
                         others => <>);
      Subprogram  : GNATStack.Data_Model.Subprogram_Information_Access;

   begin
      Data.CIs.Append (CI_Data);

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);

      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Line (File, Line, Last);

         GNAT.Regpat.Match (Node_Regexp, Line (1 .. Last), Matches);

         if Matches (0) /= GNAT.Regpat.No_Match then
            Identifier.Linker_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (Line (Matches (1).First .. Matches (1).Last));
            Identifier.Prefix_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (Ada_Decode
                   (Ada.Strings.Unbounded.To_String (Identifier.Linker_Name)));
            Identifier.Locations :=
              GNATStack.Data_Model.Subprogram_Location_Sets.Empty_Set;

            GNAT.Regpat.Match
              (Node_Label_Regexp,
               Line (Matches (2).First .. Matches (2).Last),
               Matches);

            if Matches (0) /= GNAT.Regpat.No_Match then
               Location :=
                 (Name   => Ada.Strings.Unbounded.Null_Unbounded_String,
                  File   => Ada.Strings.Unbounded.Null_Unbounded_String,
                  Line   => 1,
                  Column => 1,
                  Mark   => null,
                  Lines  => 0);

               if Matches (2) /= GNAT.Regpat.No_Match
                 and then Matches (8) /= GNAT.Regpat.No_Match
                 and then Matches (10) /= GNAT.Regpat.No_Match
                 and then Matches (12) /= GNAT.Regpat.No_Match
               then
                  Identifier.Locations.Insert
                    ((Name =>
                        Ada.Strings.Unbounded.To_Unbounded_String
                          (Line (Matches (2).First .. Matches (2).Last)),
                      File =>
                        Ada.Strings.Unbounded.To_Unbounded_String
                          (Line (Matches (8).First .. Matches (8).Last)),
                      Line =>
                        Integer'Value
                          (Line (Matches (10).First .. Matches (10).Last)),
                      Column =>
                        Integer'Value
                          (Line (Matches (12).First .. Matches (12).Last)),
                      Mark => null,
                      Lines => 0));
               end if;

               if Data.Subprogram_Map.Contains (Identifier) then
                  Subprogram := Data.Subprogram_Map.Element (Identifier);

                  Subprogram.Local_Usage.Size :=
                    Integer'Value
                      (Line (Matches (16).First .. Matches (16).Last));

                  CI_Data.Subprograms.Insert (Subprogram);

                  if Data.External_Set.Contains (Subprogram) then
                     Data.External_Set.Delete (Subprogram);
                  end if;
               end if;
            end if;
         end if;
      end loop;

      Ada.Text_IO.Close (File);
   end Merge;

   -----------
   -- Write --
   -----------

   procedure Write
     (File_Name : String;
      Data      :
        GNATStack.Data_Model.Subprogram_Information_Ordered_Sets.Set)
   is
      use GNATStack.Data_Model.Subprogram_Information_Ordered_Sets;

      File : Ada.Text_IO.File_Type;

      procedure Write (Position : Cursor);
      --  Writes stack usage information into the file.

      -----------
      -- Write --
      -----------

      procedure Write (Position : Cursor) is
         use Ada.Strings;
         use Ada.Strings.Fixed;
         use Ada.Strings.Unbounded;

         Subprogram : constant Data_Model.Subprogram_Information_Access :=
                        Element (Position);
         Identifier : constant Data_Model.Subprogram_Identifier :=
                        Subprogram.Identifier;
         Location   : Data_Model.Subprogram_Location;

      begin
         Ada.Text_IO.Put
           (File,
            "node: { title: """
            & To_String (Identifier.Linker_Name)
            & """ label: """);

         if not Identifier.Locations.Is_Empty then
            Location :=
              Data_Model.Subprogram_Location_Sets.Element
                (Identifier.Locations.First);
            Ada.Text_IO.Put
              (File,
               To_String (Location.Name)
               & "\n"
               & To_String (Location.File)
               & ":"
               & Trim (Integer'Image (Location.Line), Both)
               & ":"
               & Trim (Integer'Image (Location.Column), Both)
               & "\n");
         end if;

         Ada.Text_IO.Put_Line
           (File,
            Trim (Integer'Image (Subprogram.Local_Usage.Size), Both)
            & " bytes"" }");
      end Write;

   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, File_Name);
      Data.Iterate (Write'Access);
      Ada.Text_IO.Close (File);
   end Write;

end GNATStack.CI_Utilities;
