-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2001-2008, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with Language_Handlers;       use Language_Handlers;
with Traces;                  use Traces;

package body Docgen.Backend is

   --------------------
   -- Get_Last_Index --
   --------------------

   function Get_Last_Index (B : Backend'Class) return Natural is
   begin
      return B.Last_Index;
   end Get_Last_Index;

   -------------------
   -- Get_Last_Line --
   -------------------

   function Get_Last_Line (B : Backend'Class) return Natural is
   begin
      return B.Last_Line;
   end Get_Last_Line;

   ----------------
   -- Get_Indent --
   ----------------

   function Get_Indent (B : Backend'Class) return Natural is
   begin
      return B.Indent;
   end Get_Indent;

   --------------------
   -- Set_Last_Index --
   --------------------

   procedure Set_Last_Index (B : in out Backend'Class; Value : Natural) is
   begin
      B.Last_Index := Value;
   end Set_Last_Index;

   -------------------
   -- Set_Last_Line --
   -------------------

   procedure Set_Last_Line (B : in out Backend'Class; Value : Natural) is
   begin
      B.Last_Line := Value;
   end Set_Last_Line;

   -----------------
   -- Format_Code --
   -----------------

   procedure Format_Code
     (B                : access Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Text             : String;
      File_Name        : GNATCOLL.VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Is_Body          : Boolean;
      Options          : All_Options;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural;
      Indent           : Natural)
   is
      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Looks the value of type Language_Entity returned by
      --  Parse_Entities and calls the appropriate subprograms to format
      --  the current entity.

      --------------
      -- Callback --
      --------------

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);
      begin
         case Entity is
            when Comment_Text =>
               Format_Comment
                 (B, Kernel, Result,
                  Text,
                  Sloc_Start,
                  Sloc_End,
                  Entity_Line);

            when Keyword_Text =>
               Format_Keyword
                 (B, Kernel, Result,
                  Text,
                  Sloc_Start,
                  Sloc_End,
                  Entity_Line);

            when String_Text =>
               Format_String
                 (B, Kernel, Result,
                  Text,
                  Sloc_Start,
                  Sloc_End,
                  Entity_Line);

            when Character_Text =>
               Format_Character
                 (B, Kernel, Result,
                  Text,
                  Sloc_Start,
                  Sloc_End,
                  Entity_Line);

            when Identifier_Text =>
               --  Parse_Entities return ";" as an identifier.
               if Text (Sloc_Start.Index .. Sloc_End.Index) /= ";" then
                  Format_Identifier
                    (B, Kernel, Result,
                     List_Ref_In_File,
                     Text,
                     Sloc_Start,
                     Sloc_End,
                     File_Name,
                     Entity_Line,
                     Line_In_Body,
                     Source_File_List,
                     Options.Link_All,
                     Is_Body,
                     Options.Process_Body_Files,
                     Level,
                     Indent);
               end if;

            when others =>
               null;
         end case;

         return False;
      end Callback;

   begin
      Initialize (B, Text);

      Parse_Entities
        (Get_Language_From_File (Get_Language_Handler (Kernel), File_Name),
         Text, Callback'Unrestricted_Access);

      Finish (B, Kernel, Result, Text, Entity_Line);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Format_Code;

   ---------------------
   -- Format_All_Link --
   ---------------------

   procedure Format_All_Link
     (B                : access Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Text             : String;
      Sloc_Start       : Source_Location;
      Sloc_End         : Source_Location;
      File_Name        : GNATCOLL.VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_Table.HTable;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Level            : Natural;
      Indent           : Natural)
   is
      use List_Reference_In_File;
      use Type_Entity_List;

      Case_Sensitive     : constant Boolean :=
                             Get_Language_Context
                               (Get_Language_From_File
                                  (Get_Language_Handler (Kernel),
                                   File_Name)).Case_Sensitive;

      Loc_End            : Natural;
      Loc_Start          : Natural;
      Point_In_Column    : Natural := 0;
      Entity_Info        : Entity_Information;
      Ref_List_Info      : List_Reference_In_File.List_Node;
      Ref_List_Info_Prec : List_Reference_In_File.List_Node;
      Found              : Boolean := False;
      Entity_Abstract    : Boolean;

      procedure Get_Declaration
        (Text            : String;
         E_I             : in out Entity_Information;
         Line            : Natural;
         Column          : Natural;
         E_L_I           : List_Reference_In_File.List_Node;
         Result          : out Boolean;
         Entity_Abstract : in out Boolean);
      --  Looks if the reference E_L_I is the same as (Text+Line+Column)
      --  If yes, the declaration of E_L_I is returned and Result is True

      ---------------------
      -- Get_Declaration --
      ---------------------

      procedure Get_Declaration
        (Text            : String;
         E_I             : in out Entity_Information;
         Line            : Natural;
         Column          : Natural;
         E_L_I           : List_Reference_In_File.List_Node;
         Result          : out Boolean;
         Entity_Abstract : in out Boolean)
      is
         pragma Unreferenced (Column);

         Ref : List_Reference_In_File.Data_Access;
      begin
         Result := False;

         Ref := List_Reference_In_File.Data_Ref (E_L_I);

         if Ref.Line = Line
           and then Equal (Text, Get_Name (Ref.Entity).all, Case_Sensitive)
         --  The column should be tested as well but cannot be with the
         --  current implementation since the text has been reformated.
         then
            Result := True;
            E_I := Ref.Entity;

            if Get_Kind (E_I).Is_Abstract then
               Entity_Abstract := True;
            end if;
         end if;
      end Get_Declaration;

   begin
      Loc_Start := Sloc_Start.Index;

      --  Take apart parsed entites with any "."'s in the middle
      for J in 1 ..
        1 + Count_Points (Text (Sloc_Start.Index .. Sloc_End.Index))
      loop
         Point_In_Column := Index (Text (Loc_Start .. Sloc_End.Index), ".");

         if Point_In_Column > 0 then
            Loc_End := Point_In_Column - 1;
         else
            Loc_End := Sloc_End.Index;
         end if;

         --  We search the declaration of the entity
         --  (which is an identifier)

         Entity_Abstract    := False;
         Ref_List_Info      := List_Reference_In_File.First (List_Ref_In_File);
         Ref_List_Info_Prec := List_Reference_In_File.Null_Node;

         --  Text (Loc_Start .. Loc_End) is a reference.
         --  We search it in the list we have made before in order to
         --  find its declaration.

         while Ref_List_Info /= List_Reference_In_File.Null_Node loop
            Get_Declaration
              (Text (Loc_Start .. Loc_End),
               Entity_Info,
               Sloc_Start.Line + Entity_Line - 1,
               Sloc_Start.Column + Loc_Start - Sloc_Start.Index +
                 Level * Indent,
               Ref_List_Info,
               Found,
               Entity_Abstract);

            if Found then
               List_Reference_In_File.Remove_Nodes
                 (List_Ref_In_File, Ref_List_Info_Prec,
                  Ref_List_Info);
            end if;

            exit when Found;

            Ref_List_Info_Prec := Ref_List_Info;
            Ref_List_Info      := List_Reference_In_File.Next (Ref_List_Info);
         end loop;

         --  We create a link on the declaration for this entity

         if Found then
            Format_Link
              (B, Kernel, Result,
               Text,
               Sloc_Start, Sloc_End,
               File_Name, Entity_Line,
               Line_In_Body, Source_File_List, Link_All, Is_Body,
               Process_Body, Loc_End, Loc_Start, Entity_Info,
               Entity_Abstract);
         end if;

         if Point_In_Column > 0 then
            Loc_Start := Point_In_Column + 1;
         end if;
      end loop;
   end Format_All_Link;

   -------------------
   -- Get_Extension --
   -------------------

   function Get_Extension (B : access Backend'Class) return String is
   begin
      return '.' & B.Output_Description.Extension.all;
   end Get_Extension;

end Docgen.Backend;
