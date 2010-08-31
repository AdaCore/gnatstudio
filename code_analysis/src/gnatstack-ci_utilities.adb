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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C;
with System;
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
      CI_Data     : constant GNATStack.Data_Model.CI_Information_Access :=
                      new GNATStack.Data_Model.CI_Information'
                        (Ada.Strings.Unbounded.To_Unbounded_String (File_Name),
                         others => <>);
      Subprogram  : GNATStack.Data_Model.Subprogram_Information_Access;

      procedure GNAT_Decode
        (Coded   : System.Address;
         Decoded : System.Address;
         Verbose : Integer := 0);
      pragma Import (C, GNAT_Decode, "__gnat_decode");

   begin
      Data.CIs.Append (CI_Data);

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);

      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Line (File, Line, Last);

         GNAT.Regpat.Match (Node_Regexp, Line (1 .. Last), Matches);

         if Matches (0) /= GNAT.Regpat.No_Match then
            declare
               use Interfaces.C;

               Linker_Name   : constant String :=
                                 Line (Matches (1).First .. Matches (1).Last);
               C_Linker_Name : constant char_array := To_C (Linker_Name);
               Decoded_Name  : char_array (0 .. Linker_Name'Length * 2 + 60);
               --  See adadecode.c for description of safe size of the buffer
               --  for decoded name.

            begin
               Identifier.Linker_Name := To_Unbounded_String (Linker_Name);
               GNAT_Decode (C_Linker_Name'Address, Decoded_Name'Address);
               Identifier.Prefix_Name :=
                 To_Unbounded_String (To_Ada (Decoded_Name));
            end;

            Identifier.Locations :=
              GNATStack.Data_Model.Subprogram_Location_Sets.Empty_Set;

            GNAT.Regpat.Match
              (Node_Label_Regexp,
               Line (Matches (2).First .. Matches (2).Last),
               Matches);

            if Matches (0) /= GNAT.Regpat.No_Match then
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
