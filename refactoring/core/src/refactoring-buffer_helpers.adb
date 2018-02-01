------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

with GPS.Editors;            use GPS.Editors;
with Language.Tree.Database; use Language.Tree.Database;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GNATCOLL.Utils;         use GNATCOLL.Utils;
with GNATCOLL.Xref;

package body Refactoring.Buffer_Helpers is
   use type GNATCOLL.Xref.Visible_Column;

   Me : constant Trace_Handle := Create ("GPS.REFACTORING.REFACTORING");

   EOL_Str : constant String := (1 => ASCII.LF);
   --  String used to insert an end of line.

   -----------------
   -- To_Location --
   -----------------

   function To_Location
     (Context   : not null access Factory_Context_Record'Class;
      Location  : access Universal_Location)
      return GPS.Editors.Editor_Location'Class
   is
      Editor : constant Editor_Buffer'Class :=
        Context.Buffer_Factory.Get (Get_File_Path (Get_File (Location)));
   begin
      return Editor.New_Location
        (Get_Line (Location), Get_Column (Location));
   end To_Location;

   function To_Location
     (Context   : not null access Factory_Context_Record'Class;
      Location  : GPS.Editors.Editor_Location'Class)
      return Universal_Location
   is
      S_File : constant Structured_File_Access :=
        Get_Or_Create (Context.Db.Constructs, Location.Buffer.File);
   begin
      return To_Location (S_File, Location.Line, Location.Column);
   end To_Location;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Context   : not null access Factory_Context_Record'Class;
      Location  : access Universal_Location;
      Start_Col : Visible_Column_Type := 0) return String
   is
      Editor : constant Editor_Buffer'Class :=
        Context.Buffer_Factory.Get (Get_File_Path (Get_File (Location)));
      Loc_Start : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Get_Line (Location));
      Loc_End   : constant Editor_Location'Class := Loc_Start.End_Of_Line;

      Line : constant String := Editor.Get_Chars (Loc_Start, Loc_End);

      Char_Ind : String_Index_Type;
      Last_Ind : Integer := Line'Last;
   begin
      if Start_Col = 0 then
         Char_Ind := Get_Index_In_Line (Location);
      else
         Char_Ind := To_Line_String_Index
           (Get_File (Location), Get_Line (Location), Start_Col);
      end if;

      while Last_Ind >= Line'First and then Line (Last_Ind) = ASCII.LF loop
         Last_Ind := Last_Ind - 1;
      end loop;

      return Line (Natural (Char_Ind) .. Last_Ind);
   end Get_Line;

   ---------
   -- Get --
   ---------

   function Get
     (Context     : not null access Factory_Context_Record'Class;
      Start, Stop : access Universal_Location) return String
   is
      Editor : constant Editor_Buffer'Class :=
        Context.Buffer_Factory.Get (Get_File_Path (Get_File (Start)));
      Loc_Start : constant Editor_Location'CLass :=
        Editor.New_Location (Get_Line (Start), Get_Column (Start));
      Loc_End   : constant Editor_Location'CLass :=
        Editor.New_Location (Get_Line (Stop), Get_Column (Stop));
   begin
      return Editor.Get_Chars (Loc_Start, Loc_End);
   end Get;

   -----------------
   -- Remove_Code --
   -----------------

   procedure Remove_Code
     (Context : not null access Factory_Context_Record'Class;
      Start, Stop : access Universal_Location)
   is
      Lock : Update_Lock := Lock_Updates (Get_File (Start));
   begin
      Replace (Context, Start, Stop, "");

      if Is_Blank_Line (Get_Line (Context, Start, 1)) then
         Remove_Line (Context, Start);
      end if;

      Update_Contents (Get_File (Start));
      Unlock (Lock);
   end Remove_Code;

   ------------------
   -- Comment_Code --
   ------------------

   procedure Comment_Code
     (Context     : not null access Factory_Context_Record'Class;
      Start, Stop : access Universal_Location)
   is
      Lock : Update_Lock := Lock_Updates (Get_File (Start));
      Line_Cursor      : aliased Universal_Location;
      Start_Char_Index : String_Index_Type;
      Stop_Char_Index  : String_Index_Type;
      Tmp_Cursor       : aliased Universal_Location;
   begin
      Trace
        (Me,
         "COMMENT CODE: "
         & Get_Line (Start)'Img & Get_Column (Start)'Img
         & " TO " & Get_Line (Stop)'Img & Get_Column (Stop)'Img);
      Line_Cursor :=
        To_Location (Get_File (Start), Get_Line (Start), 1);

      if Get_Line (Start) = Get_Line (Stop) then
         --  Add a new line if there is some text after the entity that has
         --  not to be commented

         declare
            Current_String : constant String := Get_Line (Context, Start, 1);
         begin
            Start_Char_Index := Get_Index_In_Line (Start);
            Stop_Char_Index := Get_Index_In_Line (Stop);

            declare
               Back : constant String := Current_String
                 (Integer (Stop_Char_Index) + 1 .. Current_String'Last);
            begin
               if not Is_Blank_Line (Back) then
                  Trace
                    (Me,
                     "ADD LINE @ "
                     & Get_Line (Stop)'Img & Get_Column (Stop)'Img);
                  Add_Line (Context, Stop, "", True);
                  Tmp_Cursor := Stop.all;
                  Set_Column
                    (Tmp_Cursor'Access, Get_Column (Tmp_Cursor'Access) + 1);
               end if;
            end;

            --  But proper comment at the begining of the entity

            Trace (Me, "CURRENT STRING IS [" & Current_String & "]");
            Trace (Me, "STARTS ON [" & Current_String'First'Img & "]");
            Trace (Me, "START CHAR INDEX IS " & Start_Char_Index'Img);

            if Is_Blank_Line (Current_String
              (Current_String'First .. Natural (Start_Char_Index) - 1))
            then
               Trace (Me, "CASE A");
               Set_Column (Line_Cursor'Access, 1);
               Replace (Context, Line_Cursor'Access, 0, "--  ");
            else
               Trace (Me, "CASE B");
               Set_Column (Line_Cursor'Access, Get_Column (Start));
               Trace (Me, "COL = " & Get_Column (Line_Cursor'Access)'Img);
               Replace (Context, Line_Cursor'Access, 0, "--  ");
            end if;
         end;
      else
         --  But proper comment at the begining of the entity

         Trace (Me, "STOP = " & Get_Line (Stop)'Img & Get_Column (Stop)'Img);

         declare
            Current_String : constant String := Get_Line (Context, Start, 1);
         begin
            Start_Char_Index := Get_Index_In_Line (Start);

            if Is_Blank_Line
              (Current_String
                 (Current_String'First  .. Natural (Start_Char_Index) - 1))
            then
               Replace (Context, Line_Cursor'Access, 0, "--  ");
            else
               Replace (Context, Start, 0, "--  ");
            end if;

            for J in Get_Line (Start) + 1 .. Get_Line (Stop) - 1 loop
               Set_Line_Column (Tmp_Cursor'Access, J, 1);
               Replace (Context, Line_Cursor'Access, 0, "--  ");
            end loop;
         end;

         Line_Cursor := Stop.all;
         Set_Column (Line_Cursor'Access, 1);

         --  Add a new line if there is some text after the entity that has
         --  not to be commented

         declare
            Current_String : constant String := Get_Line (Context, Stop);
            Back           : constant String := Current_String
              (Current_String'First + 1 .. Current_String'Last);
         begin
            Trace (Me, "CURRENT STRING: [" & Current_String & "]");
            Trace (Me, "BACK: [" & Back & "]");

            if not Is_Blank_Line (Back) then
               Add_Line (Context, Stop, "", False);

               Replace (Context, Line_Cursor'Access, 0, "--  ");

               --  The indentation must be done after the comment, otherwise
               --  the file may not be semantically correct while indenting.

               Tmp_Cursor := To_Location
                 (Get_File (Stop), Get_Line (Stop) + 1, 1);
               Indent_Line (Context, Tmp_Cursor'Access);
            else
               Replace (Context, Line_Cursor'Access, 0, "--  ");
            end if;
         end;
      end if;

      Update_Contents (Get_File (Start));
      Unlock (Lock);
   end Comment_Code;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (Context  : not null access Factory_Context_Record'Class;
      Location : access Universal_Location;
      New_Line : String;
      Indent   : Boolean := False)
   is
      Lock : Update_Lock := Lock_Updates (Get_File (Location));
      Insert_Position : aliased Universal_Location := Location.all;
   begin
      if Get_Line (Location) = 0 then
         Replace (Context, Insert_Position'Access, 0, New_Line & EOL_Str);
      else
         Set_Column
           (Insert_Position'Access,
            Get_Column (Insert_Position'Access) + 1);

         Trace
           (Me,
            "ADD LINE ON "
            & Get_Line (Insert_Position'Access)'Img
            & Get_Column (Insert_Position'Access)'Img);

         Replace (Context, Insert_Position'Access, 0, EOL_Str & New_Line);
      end if;

      if Indent then
         declare
            Line_Cursor : aliased Universal_Location := Location.all;
         begin
            Set_Line_Column
              (Line_Cursor'Access,
               Get_Line (Line_Cursor'Access) + 1,
               1);

            Indent_Line (Context, Line_Cursor'Access);
         end;
      end if;

      Update_Contents (Get_File (Location));
      Unlock (Lock);
   end Add_Line;

   -----------------
   -- Indent_Line --
   -----------------

   procedure Indent_Line
     (Context  : not null access Factory_Context_Record'Class;
      Location : access Universal_Location)
   is
      Lock : Update_Lock := Lock_Updates (Get_File (Location));

      Editor : constant Editor_Buffer'Class :=
        Context.Buffer_Factory.Get (Get_File_Path (Get_File (Location)));
      Loc : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Get_Line (Location));
   begin
      Editor.Indent (Loc, Loc);

      Update_Contents (Get_File (Location));
      Unlock (Lock);
   end Indent_Line;

   -----------------
   -- Remove_Line --
   -----------------

   procedure Remove_Line
     (Context  : not null access Factory_Context_Record'Class;
      Location : access Universal_Location)
   is
      Lock : Update_Lock := Lock_Updates (Get_File (Location));

      Editor : constant Editor_Buffer'Class :=
        Context.Buffer_Factory.Get (Get_File_Path (Get_File (Location)));
      Loc_Start : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Get_Line (Location));
      Loc_End : constant Editor_Location'Class := Loc_Start.End_Of_Line;
   begin
      Editor.Delete (Loc_Start, Loc_End);

      Update_Contents (Get_File (Location));
      Unlock (Lock);
   end Remove_Line;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Context     : not null access Factory_Context_Record'Class;
      Start, Stop : access Universal_Location;
      New_Value   : String)
   is
      Lock : Update_Lock := Lock_Updates (Get_File (Start));

      Editor : constant Editor_Buffer'Class :=
        Context.Buffer_Factory.Get (Get_File_Path (Get_File (Start)));
      Loc_Start : constant Editor_Location'Class :=
        Editor.New_Location
          (Get_Line (Start), Get_Column (Start));
      Loc_End : constant Editor_Location'Class :=
        Editor.New_Location
          (Get_Line (Stop), Get_Column (Stop));
   begin
      pragma Assert (Get_File (Start) = Get_File (Stop));

      if not
        (Loc_Start.Line > Loc_End.Line
         or else
           (Loc_Start.Line = Loc_End.Line
            and then Loc_Start.Column > Loc_End.Column))
      then
         --  Loc start must be after Loc end, we don't delete null ranges.

         Editor.Delete (Loc_Start, Loc_End);
      end if;

      Editor.Insert (Loc_Start, New_Value);

      Update_Contents (Get_File (Start));
      Unlock (Lock);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Context   : not null access Factory_Context_Record'Class;
      Location  : access Universal_Location;
      Len       : Natural;
      New_Value : String)
   is
      Lock : Update_Lock := Lock_Updates (Get_File (Location));

      Editor : constant Editor_Buffer'Class :=
        Context.Buffer_Factory.Get (Get_File_Path (Get_File (Location)));
   begin
      declare
         Loc_Start : constant Editor_Location'Class :=
           Editor.New_Location
             (Get_Line (Location), Get_Column (Location));
      begin
         if Len /= 0 then
            declare
               Loc_End : constant Editor_Location'Class :=
                 Loc_Start.Forward_Char (Len - 1);
            begin
               Editor.Delete (Loc_Start, Loc_End);
            end;
         end if;

         Editor.Insert (Loc_Start, New_Value);
      end;

      Update_Contents (Get_File (Location));
      Unlock (Lock);
   end Replace;

end Refactoring.Buffer_Helpers;
