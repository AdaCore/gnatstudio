------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Basic_Types;                  use Basic_Types;
with Casing_Exceptions;
with Case_Handling;                use Case_Handling;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with Language;                     use Language;
with Refactoring.Services;
with VSS.Characters.Latin;
with VSS.Strings;                  use VSS.Strings;
with VSS.Strings.Conversions;      use VSS.Strings.Conversions;
with Commands;                     use Commands;

package body Language_Formatter is

   -------------------------
   -- On_Range_Formatting --
   -------------------------

   overriding
   function On_Range_Formatting
     (Self        : in out Language_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural;
      Cursor_Move : in out Integer) return Boolean
   is
      Lang          : Language.Language_Access;
      Indent_Params : Indent_Parameters;
      Indent_Style  : Indentation_Kind;
      Buffer        : constant Editor_Buffer'Class := From.Buffer;
      File          : constant Virtual_File := Buffer.File;
      Local_Buffer  : Virtual_String :=
        GPS.Editors.Get_Text
          (This                 => Buffer,
           --  The language formatter needs all the previous text
           --  so use (0, 0) in place of From
           From                 => Buffer.New_Location (0, 0),
           To                   => To,
           Include_Hidden_Chars => True);
      --  Create an undo group at this level to undo the whole formatting at
      --  once.
      G             : constant Group_Block := Buffer.New_Undo_Group;
      Result        : Boolean := True;

      procedure Replace_Text
        (Line : Natural; First : Natural; Last : Natural; Replace : String);
      --  Callback for Format_Buffer

      ------------------
      -- Replace_Text --
      ------------------

      procedure Replace_Text
        (Line : Natural; First : Natural; Last : Natural; Replace : String)
      is
         use type Basic_Types.Visible_Column_Type;
         --  According to the documentation of Replace_Text_Callback:
         --  "First and Last are byte offsets from the start of the line"
         Line_Offset   : constant Natural :=
           Buffer.New_Location (Line => Line, Column => 0).Offset;
         Replace_From  : constant Editor_Location'Class :=
           Buffer.New_Location (Offset => Line_Offset + First - 1);
         --  Last offset is not included
         Replace_To : constant Editor_Location'Class :=
           Buffer.New_Location (Offset => Line_Offset + Last - 1);

         Previous_Text : constant String :=
           VSS.Strings.Conversions.To_UTF_8_String
             (GPS.Editors.Get_Text
                (This                 => Buffer,
                 From                 => Replace_From,
                 To                   => Replace_To,
                 Include_Hidden_Chars => True));
      begin
         --  Only replace if needed
         if Previous_Text (Previous_Text'First .. Previous_Text'Last - 1)
           /= Replace
         then
            Result :=
              Result
              and then Refactoring.Services.Insert_Text
                         (Context     => Self.Kernel.Refactoring_Context,
                          In_File     => File,
                          From_Line   => Line,
                          From_Column => Replace_From.Column,
                          To_Line     => Line,
                          To_Column   => Replace_To.Column,
                          Text        => Replace);
            if Line = Cursor_Line then
               --  Add the new characters
               Cursor_Move := Cursor_Move + Replace'Length;
               --  Remove the deleted characters
               Cursor_Move :=
                 Cursor_Move -
                   Integer (Replace_To.Column - Replace_From.Column);
            end if;
         end if;
      end Replace_Text;

   begin
      Lang := Self.Kernel.Get_Language_Handler.Get_Language_From_File (File);

      Get_Indentation_Parameters
        (Lang => Lang, Params => Indent_Params, Indent_Style => Indent_Style);

      --  Set proper casing policy, we want to disable the auto-casing here if
      --  we are using the on-the-fly casing policy
      if Indent_Params.Casing_Policy in End_Of_Word .. On_The_Fly
      then
         Indent_Params.Casing_Policy := Disabled;
      end if;

      if Buffer.End_Of_Buffer = To then
         --  The old engine expects an EOL at EOF to format the last line.
         Local_Buffer.Append (VSS.Characters.Latin.Line_Feed);
      end if;

      if Indent_Style = Language.Simple then
         Language.Format_Buffer
           (Lang            => Language_Root (Lang.all)'Access,
            Buffer          => To_UTF_8_String (Local_Buffer),
            Replace         => Replace_Text'Unrestricted_Access,
            From            => From.Line,
            To              => To.Line,
            Indent_Params   => Indent_Params,
            Case_Exceptions => Casing_Exceptions.Get_Case_Exceptions);
      else
         Language.Format_Buffer
           (Lang            => Lang,
            Buffer          => To_UTF_8_String (Local_Buffer),
            Replace         => Replace_Text'Unrestricted_Access,
            From            => From.Line,
            To              => To.Line,
            Indent_Params   => Indent_Params,
            Case_Exceptions => Casing_Exceptions.Get_Case_Exceptions);
      end if;

      return Result;
   end On_Range_Formatting;

   ------------------------
   -- On_Type_Formatting --
   ------------------------

   overriding
   function On_Type_Formatting
     (Self        : in out Language_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural) return Boolean
   is
      Cursor_Offset : Integer := 1;
   begin
      return Self.On_Range_Formatting (From, To, Cursor_Line, Cursor_Offset);
   end On_Type_Formatting;

   --------------
   -- Get_Name --
   --------------

   overriding
   function Get_Name
     (Self : Language_Formatting_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Construct";
   end Get_Name;

end Language_Formatter;
