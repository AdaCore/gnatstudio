-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                            ACT-Europe                             --
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

with Traces;                  use Traces;
with Basic_Types;             use Basic_Types;
with Language;                use Language;
with Language_Handlers;       use Language_Handlers;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

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
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Is_Body          : Boolean;
      Options          : All_Options;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural;
      Indent           : Natural)
   is
      --  ???
      --       Is_Spec : constant Boolean := Is_Spec_File (Kernel, File_Name);

      --         function Is_Operator (Op : String) return Boolean;
      --  Indicates if op is a subprogram which overloads an operator

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Looks the value of type Language_Entity returned by
      --  Parse_Entities and calls the good subprograms to format
      --  the current entity

      -------------------
      --  Is_Operator  --
      -------------------

      --  ???
--        function Is_Operator (Op : String) return Boolean is
--           use List_Reference_In_File;
--           Ref_List_Info : List_Reference_In_File.List_Node;
--           Ref           : List_Reference_In_File.Data_Access;
--
--        begin
--           --  Can this function be simplified ???
--
--           Ref_List_Info := List_Reference_In_File.First (List_Ref_In_File);
--
--           while Ref_List_Info /= List_Reference_In_File.Null_Node loop
--              Ref := List_Reference_In_File.Data_Ref (Ref_List_Info);
--
--              if Get_Name (Ref.Entity) = Op then
--                 case Get_Kind (Ref.Entity).Kind is
--                    when Function_Or_Operator | Procedure_Kind =>
--                       --  The entity of this reference overloads an operator
--                       return True;
--                    when others  =>
--                       return False;
--                 end case;
--              end if;
--
--              Ref_List_Info := List_Reference_In_File.Next (Ref_List_Info);
--           end loop;
--
--           return False;
--        end Is_Operator;

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
                  Sloc_Start.Index,
                  Sloc_Start.Line,
                  Sloc_End.Index - 1,
                  Sloc_End.Line,
                  Entity_Line);

            when Keyword_Text =>
               Format_Keyword
                 (B, Kernel, Result,
                  Text,
                  Sloc_Start.Index,
                  Sloc_Start.Line,
                  Sloc_End.Index,
                  Sloc_End.Line,
                  Entity_Line);

            when String_Text =>
               --  ???
               --  In this context, we must detect overriden operators
--                 if Text (Sloc_Start.Index) = '"'
--                   and then
--                     Text (Sloc_End.Index) = '"'
--                   and then
--                     Sloc_Start.Index + 1 <= Sloc_End.Index - 1
--                 then
--                    if (not Is_Spec
--                        and then
--                        --  For a body file, we must search the word in the
--                        --  list of reference that has been made before
--                        --  because the body is formated in one piece and we
--                        --  don't have any information about this word
--                          Is_Operator
--                            (Text (Sloc_Start.Index + 1 ..
--                                     Sloc_End.Index - 1)))
--                      or else
--                        (Is_Spec
--                         --  For a spec file, we know immediatly the nature
--                         --  of the word
--                         and then Info in Doc_Info_Subprogram'Class
--                         and then
--                          Text (Sloc_Start.Index + 1 .. Sloc_End.Index - 1) =
--                           Get_Name (Doc_Info_Subprogram
--                                       (Info).Subprogram_Entity.Entity))
--                    then
--                       --  Function which overrides an operator
--                       Format_Identifier
--                         (B,
--                          List_Ref_In_File,
--                          Sloc_Start.Index + 1,
--                          Sloc_Start.Line,
--                          Sloc_Start.Column,
--                          Sloc_End.Index - 1,
--                          Sloc_End.Line,
--                          Kernel,
--                          File,
--                          Text,
--                          File_Name,
--                          Entity_Line,
--                          Line_In_Body,
--                          Source_File_List,
--                          Options.Link_All,
--                          Is_Body,
--                          Options.Process_Body_Files,
--                          Level,
--                          Indent);
--                    else
--                       Simple String
               Format_String
                 (B, Kernel, Result,
                  Text,
                  Sloc_Start.Index,
                  Sloc_Start.Line,
                  Sloc_End.Index,
                  Sloc_End.Line,
                  Entity_Line);
               --                       end if;
               --                    else
               --                       --  Simple string
               --                       Format_String
               --                         (B,
               --                          File,
               --                          Text,
               --                          Sloc_Start.Index,
               --                          Sloc_Start.Line,
               --                          Sloc_End.Index,
               --                          Sloc_End.Line,
               --                          Entity_Line);
               --                    end if;

            when Character_Text =>
               Format_Character
                 (B, Kernel, Result,
                  Text,
                  Sloc_Start.Index,
                  Sloc_Start.Line,
                  Sloc_End.Index,
                  Sloc_End.Line,
                  Entity_Line);

            when Identifier_Text =>
               if Text (Sloc_Start.Index .. Sloc_End.Index) /= ";" then
                  --  ???  This test is necessary because Parse_Entity
                  --  consider the last ";" of the text as an identifier.
                  --  What is surprising is it occurs only for the text of
                  --  spec file and not for the text of body file. Perhaps,
                  --  the reason is that in the body the ";" ended the
                  --  file.
                  Format_Identifier
                    (B, Kernel, Result,
                     List_Ref_In_File,
                     Sloc_Start.Index,
                     Sloc_Start.Line,
                     Sloc_Start.Column,
                     Sloc_End.Index,
                     Sloc_End.Line,
                     Text,
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Format_Code;

   ---------------------
   -- Format_All_Link --
   ---------------------

   procedure Format_All_Link
     (B                : access Backend'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Start_Index      : Natural;
      Start_Line       : Natural;
      Start_Column     : Natural;
      End_Index        : Natural;
      Text             : String;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : in out Natural;
      Source_File_List : Type_Source_File_Table.HTable;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Level            : Natural;
      Indent           : Natural)
   is
      use type Basic_Types.String_Access;
      use List_Reference_In_File;
      use Type_Entity_List;
      Loc_End            : Natural;
      Loc_Start          : Natural;
      Point_In_Column    : Natural := 0;
      Entity_Info        : Entity_Information;
      Ref_List_Info      : List_Reference_In_File.List_Node;
      Ref_List_Info_Prec : List_Reference_In_File.List_Node;
      Found              : Boolean;
      Entity_Abstract    : Boolean;
      Indentation        : Natural;
      --  This last parameter is used to add levels of indentation
      --  for spec files

      procedure Get_Declaration
        (Text            : String;
         E_I             : in out Entity_Information;
         Line            : Natural;
         Column          : Natural;
         E_L_I           : in List_Reference_In_File.List_Node;
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
         E_L_I           : in List_Reference_In_File.List_Node;
         Result          : out Boolean;
         Entity_Abstract : in out Boolean)
      is
         Ref : List_Reference_In_File.Data_Access;
      begin
         Result := False;

         Ref := List_Reference_In_File.Data_Ref (E_L_I);

         if Ref.Line = Line
           and then To_Lower (Text) = Get_Name (Ref.Entity).all
           and then Ref.Column = Column
         then
            Result := True;
            E_I := Ref.Entity;

            if Get_Kind (E_I).Is_Abstract then
               Entity_Abstract := True;
            end if;
         end if;
      end Get_Declaration;

   begin
      if Is_Body then
         Indentation := 0;
      else
         Indentation := Level * Indent;
         --  For spec files, we must add levels of indentation otherwise
         --  a text and its associated reference in list won't match.
         --  In fact, the string which is given to Format_File is obtained by
         --  Get_Whole_Header + Remove_Indent. Get_Whole_Header remove the
         --  indentation of the first line and Remove_Indent remove the
         --  indentation of the other lines
      end if;
      Loc_Start := Start_Index;

      --  Take apart parsed entites with any "."'s in the middle
      for J in 1 ..
        1 + Count_Points (Text (Start_Index .. End_Index))
      loop
         Point_In_Column :=
           Index (Text (Loc_Start .. End_Index), ".");
         if Point_In_Column > 0 then
            Loc_End := Point_In_Column - 1;
         else
            Loc_End := End_Index;
         end if;

         --  We search the declaration of the entity
         --  (which is an identifier)

         Entity_Abstract := False;
         Ref_List_Info      := List_Reference_In_File.First (List_Ref_In_File);
         Ref_List_Info_Prec := List_Reference_In_File.Null_Node;

         --  Text(Loc_Start .. Loc_End) is a reference.
         --  We search it in the list we have made before in order to
         --  find its declaration.
         while Ref_List_Info /= List_Reference_In_File.Null_Node loop
            Get_Declaration
              (Text (Loc_Start .. Loc_End),
               Entity_Info,
               Start_Line + Entity_Line - 1,
               Start_Column + Loc_Start - Start_Index + Indentation,
               Ref_List_Info,
               Found,
               Entity_Abstract);

            if Found then
               List_Reference_In_File.Remove_Nodes
                 (List_Ref_In_File, Ref_List_Info_Prec,
                  Ref_List_Info);
            end if;

            exit when Is_Body or else Found;

            --  For body files, no loop because references in the list
            --  are sorted. So the first element met in list is the right
            --  one (for this elements are removed after being met).

            Ref_List_Info_Prec := Ref_List_Info;
            Ref_List_Info := List_Reference_In_File.Next (Ref_List_Info);
         end loop;

         --  We create a link on the declaration for this entity

         if Found then
            Format_Link
              (B, Kernel, Result,
               Start_Index, Start_Line, Start_Column, End_Index,
               Text, File_Name, Entity_Line,
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
