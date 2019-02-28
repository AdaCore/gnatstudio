------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

with GNAT.Regpat;       use GNAT.Regpat;
with GNATCOLL.Utils;    use GNATCOLL.Utils;
with GNATCOLL.Xref;

with Gtk.Text_Mark;     use Gtk.Text_Mark;

with GUI_Utils;         use GUI_Utils;
with Src_Editor_Box;    use Src_Editor_Box;
with String_Utils;      use String_Utils;
with Language;          use Language;
with Src_Editor_Module; use Src_Editor_Module;
with Xref;              use Xref;

package body Src_Editor_Buffer.Hyper_Mode is

   use type GNATCOLL.Xref.Visible_Column;

   -----------------------------
   -- Hyper_Mode_Highlight_On --
   -----------------------------

   procedure Hyper_Mode_Highlight_On
     (Buffer  : Source_Buffer;
      Iter    : Gtk_Text_Iter)
   is
      Entity_Start, Entity_End : Gtk_Text_Iter;
      Line_Start, Line_End     : Gtk_Text_Iter;
      use List_Of_Highlighters;

      Found_Highlighter  : Boolean := False;
      Result             : Boolean;
      In_Comment         : Boolean := False;
      L                  : List_Of_Highlighters.List;
      Maybe_File         : Boolean := False;

   begin
      --  Remove the previous highlight

      Remove_Highlight (Buffer);

      L := Get_Highlighters;

      --  If we have registered custom highlighters, try to apply them in
      --  priority.

      if not L.Is_Empty then
         --  Get the line

         Copy (Iter, Line_Start);
         Copy (Iter, Line_End);

         Set_Line_Offset (Line_Start, 0);
         Forward_To_Line_End (Line_End, Result);

         if Result then
            declare
               C           : List_Of_Highlighters.Cursor;
               Highlighter : Highlighter_Record;
               Line        : constant String := Get_Slice
                 (Line_Start, Line_End);
               First       : Integer := Line'First;
               Iter_Is_At  : constant Integer := Integer
                 (Get_Offset (Iter) - Get_Offset (Line_Start)) + First;
               Index       : Natural := Line'First;
               Entity      : Language_Entity;
               Ignore      : Natural;
               pragma Unreferenced (Ignore);

            begin
               Maybe_File := Has_Include_Directive (Line);

               C := L.First;

               while Has_Element (C)
                 and then not Found_Highlighter
               loop
                  Highlighter := List_Of_Highlighters.Element (C);

                  Subloop :
                  while First < Line'Last loop
                     --  Attempt to match this highlighter against the line
                     declare
                        Matches : Match_Array (0 .. Highlighter.Paren_Count);
                     begin
                        Match
                          (Highlighter.Pattern.all,
                           Line,
                           Matches,
                           First);

                        exit Subloop when Matches (0) = No_Match;

                        First := Matches (0).Last;

                        if Matches (Highlighter.Index).First <= Iter_Is_At
                          and then
                            Matches (Highlighter.Index).Last >= Iter_Is_At
                        then
                           --  We have a match, and the iter is within the
                           --  proper parenthesis: we have found a valid match!

                           Buffer.Hyper_Mode_Current_Action :=
                             Highlighter.Action;
                           Buffer.Hyper_Mode_Current_Alternate :=
                             Highlighter.Alternate;
                           Found_Highlighter := True;

                           Copy (Iter, Entity_Start);
                           Copy (Iter, Entity_End);

                           Set_Offset
                             (Entity_Start,
                              Get_Offset (Iter) -
                                Gint (Iter_Is_At -
                                    Matches (Highlighter.Index).First));

                           Set_Offset
                             (Entity_End,
                              Get_Offset (Iter) +
                                Gint (Matches (Highlighter.Index).Last -
                                    Iter_Is_At + 1));
                        end if;
                     end;
                  end loop Subloop;

                  C := Next (C);
               end loop;

               if not Found_Highlighter then
                  Skip_Blanks (Line, Index);

                  --  Reset Index to the last Line character when no blanks
                  --  have been found.
                  if Index > Line'Last then
                     Index := Line'Last;
                  end if;

                  Looking_At (Lang      => Buffer.Lang,
                              Buffer    => Line,
                              First     => Index,
                              Entity    => Entity,
                              Next_Char => Ignore);
                  In_Comment := Entity in Comment_Text | Aspect_Comment_Text;
               end if;
            end;
         end if;
      end if;

      --  If we did not find a highlighter in the custom highlighters, display
      --  hyperlinks on entities.

      if not Found_Highlighter then
         --  Get the current word / entity

         Copy (Iter, Entity_Start);

         Search_Entity_Bounds (Entity_Start, Entity_End, Maybe_File);

         declare
            Slice : constant String := Get_Slice (Entity_Start, Entity_End);

            function Callback
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Auxiliary parsing function

            function Is_Simple_Word (S : String) return Boolean;
            --  Return True is S only contains lower case letters.

            Highlight : Boolean := False;

            --------------
            -- Callback --
            --------------

            function Callback
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Sloc_Start, Sloc_End, Partial_Entity);
            begin
               case Entity is
                  when Normal_Text
                     | Identifier_Text
                     | Block_Text
                     | Type_Text
                     | Partial_Identifier_Text
                     | Operator_Text
                     => Highlight := True;
                  when Keyword_Text
                     | Annotated_Keyword_Text
                     | Aspect_Keyword_Text
                     | Aspect_Comment_Text
                     | Comment_Text
                     | Annotated_Comment_Text
                     | Aspect_Text
                     | Character_Text
                     | String_Text
                     | Number_Text
                     => Highlight := False;
               end case;
               return True;
            end Callback;

            --------------------
            -- Is_Simple_Word --
            --------------------

            function Is_Simple_Word (S : String) return Boolean is
            begin
               for J in S'Range loop
                  if S (J) not in 'a' .. 'z' then
                     return False;
                  end if;
               end loop;

               return True;
            end Is_Simple_Word;

         begin
            --  Assume that an all lower case word in a comment is not an
            --  interesting target for source navigation.

            if In_Comment and then Is_Simple_Word (Slice) then
               return;
            end if;

            Parse_Entities (Lang     => Buffer.Lang,
                            Buffer   => Slice,
                            Callback => Callback'Unrestricted_Access);

            if not Highlight then
               return;
            end if;
         end;
      end if;

      --  At this point, Entity_Start and Entity_End should be set to the
      --  bounds of the region to highlight

      if Buffer.Hyper_Mode_Highlight_Begin = null then
         Buffer.Hyper_Mode_Highlight_Begin :=
           Create_Mark (Buffer, Where => Entity_Start);
      else
         Move_Mark (Buffer, Buffer.Hyper_Mode_Highlight_Begin, Entity_Start);
      end if;

      if Buffer.Hyper_Mode_Highlight_End = null then
         Buffer.Hyper_Mode_Highlight_End :=
           Create_Mark (Buffer, Where => Entity_End);
      else
         Move_Mark (Buffer, Buffer.Hyper_Mode_Highlight_End, Entity_End);
      end if;

      Buffer.Hyper_Mode_Has_Highlight := True;

      --  Highlight the section that we want to highlight
      Apply_Tag (Buffer, Buffer.Hyper_Mode_Tag, Entity_Start, Entity_End);
   end Hyper_Mode_Highlight_On;

   -------------------------
   -- Hyper_Mode_Click_On --
   -------------------------

   procedure Hyper_Mode_Click_On
     (Buffer    : Source_Buffer;
      Project   : Project_Type;
      Alternate : Boolean := False)
   is
      Entity_Start : Gtk_Text_Iter;
      Entity_End   : Gtk_Text_Iter;
      Line         : Editable_Line_Type;
      Column       : Visible_Column_Type;

      Closest      : Root_Entity_Reference_Ref;

      Location     : General_Location;
      Decl         : General_Entity_Declaration;
      Current      : General_Location;

      use type GNATCOLL.VFS.Virtual_File;

   begin
      if not Buffer.Hyper_Mode_Has_Highlight then
         --  If we are not highlighting anything, clicking should do nothing
         return;
      end if;

      Get_Iter_At_Mark
        (Buffer, Entity_Start, Buffer.Hyper_Mode_Highlight_Begin);
      Get_Iter_At_Mark
        (Buffer, Entity_End, Buffer.Hyper_Mode_Highlight_End);

      --  First, check whether we are reacting to a custom highlighter
      if Buffer.Hyper_Mode_Current_Action /= null then
         declare
            The_Action : Subprogram_Type;
            Text       : constant String := Get_Slice
              (Entity_Start, Entity_End);
         begin
            if Alternate then
               The_Action := Buffer.Hyper_Mode_Current_Alternate;
            else
               The_Action := Buffer.Hyper_Mode_Current_Action;
            end if;

            if The_Action = null then
               return;
            end if;

            declare
               C   : Callback_Data'Class :=
                       Create (Get_Script (The_Action.all),
                               Arguments_Count => 1);
               Dummy : Boolean;
               pragma Unreferenced (Dummy);
            begin
               Set_Nth_Arg (C, 1, Text);
               Dummy := Execute (The_Action, C);
               Free (C);
            end;
         end;

         return;
      end if;

      --  Fallback on jumping to entity spec/implementation

      Get_Iter_Position (Buffer, Entity_Start, Line, Column);

      --  Move the cursor, so that "goto previous location" takes us back to
      --  the entity location after we jump to its declaration

      Buffer.Set_Cursor_Position
        (Line             => Line,
         Column           => Character_Offset_Type (Column),
         Internal         => False,
         Extend_Selection => False);

      declare
         Entity : constant Root_Entity'Class :=
           Buffer.Kernel.Databases.Get_Entity
             (Loc         => (File   => Buffer.Filename,
                              Project_Path => Project.Project_Path,
                              Line   => Integer (Line),
                              Column => Column),
              Name => Get_Slice (Buffer, Entity_Start, Entity_End),
              Closest_Ref       => Closest);
      begin
         if Entity = No_Root_Entity then
            return;
         end if;

         Decl := Get_Declaration (Entity);
         Location := Decl.Loc;

         --  Do not check the column: it is unlikely to have both spec and
         --  body on the same line, and this works around an issue in the
         --  constructs where going to the body for a child package
         --  declaration goes in fact to the name of the parent
         --  package on the child package declaration line.

         if Alternate
           or else
             (Location.Line = Natural (Line)
              and then Location.File = Buffer.Filename)
         then
            --  We asked for the alternate behavior, or we are already on
            --  the spec: in this case, go to the body
            Current :=
              (File   => Buffer.Filename,
               Project_Path => Project.Project_Path,
               Line   => Integer (Line),
               Column => Column);
            Location := Get_Body (Entity, After => Current);
            if Location = No_Location then
               Location := Decl.Loc;
            end if;
         end if;

         Go_To_Closest_Match
           (Buffer.Kernel,
            Location.File,
            Get_Project (Location),
            Editable_Line_Type (Location.Line),
            Location.Column, Entity);
      end;
   end Hyper_Mode_Click_On;

   ----------------------
   -- Hyper_Mode_Enter --
   ----------------------

   procedure Hyper_Mode_Enter (Buffer : Source_Buffer) is
   begin
      if Buffer.Hyper_Mode then
         return;
      end if;

      Buffer.Hyper_Mode := True;
   end Hyper_Mode_Enter;

   ----------------------
   -- Remove_Highlight --
   ----------------------

   procedure Remove_Highlight (Buffer : Source_Buffer) is
      Entity_Start, Entity_End : Gtk_Text_Iter;
   begin
      if not Buffer.Hyper_Mode_Has_Highlight then
         return;
      end if;

      if Buffer.Hyper_Mode_Highlight_Begin /= null then
         Get_Iter_At_Mark
           (Buffer, Entity_Start, Buffer.Hyper_Mode_Highlight_Begin);
         Get_Iter_At_Mark
           (Buffer, Entity_End, Buffer.Hyper_Mode_Highlight_End);
         Remove_Tag (Buffer, Buffer.Hyper_Mode_Tag, Entity_Start, Entity_End);
      end if;

      Buffer.Hyper_Mode_Has_Highlight := False;
      Buffer.Hyper_Mode_Current_Action := null;
      Buffer.Hyper_Mode_Current_Alternate := null;
   end Remove_Highlight;

   ----------------------
   -- Hyper_Mode_Leave --
   ----------------------

   procedure Hyper_Mode_Leave (Buffer : Source_Buffer) is
   begin
      if not Buffer.Hyper_Mode then
         return;
      end if;

      Buffer.Hyper_Mode := False;

      --  Clean up the tag
      Remove_Highlight (Buffer);
   end Hyper_Mode_Leave;

end Src_Editor_Buffer.Hyper_Mode;
