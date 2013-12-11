------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Glib.Properties;

package body Src_Editor_Buffer.Multi_Cursors is

   Mc_Selection_Tag : constant String := "mc_selection";

   procedure Update_MC_Selection (B : Source_Buffer) is
      Start_Loc, End_Loc : Gtk_Text_Iter;
      T : constant Gtk_Text_Tag := B.Get_Tag_Table.Lookup (Mc_Selection_Tag);
      Line : Editable_Line_Type;
      Col  : Character_Offset_Type;
   begin
      if T = null then
         return;
      end if;
      B.Get_Start_Iter (Start_Loc);
      B.Get_End_Iter (End_Loc);
      B.Remove_Tag (T, Start_Loc, End_Loc);

      for C of B.Multi_Cursors_List loop
         B.Get_Iter_At_Mark (Start_Loc, C.Sel_Mark);
         B.Get_Iter_At_Mark (End_Loc, C.Mark);

         Get_Iter_Position (B, End_Loc, Line, Col);

         B.Apply_Tag (T, Start_Loc, End_Loc);
      end loop;
   end Update_MC_Selection;

   function Get_Mark (C : Cursor) return Gtk_Text_Mark
   is (C.Mark);

   function Get_Sel_Mark (C : Cursor) return Gtk_Text_Mark
   is (C.Sel_Mark);

   function Get_Column_Memory (C : Cursor) return Gint
   is (C.Column_Memory);

   procedure Set_Column_Memory (C : Cursor; Offset : Gint) is
   begin
      C.Column_Memory := Offset;
   end Set_Column_Memory;

   procedure Add_Multi_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter) is
      function Next_Multi_Cursor_Name return String is
        ("multi_cursor_" & Buffer.Multi_Cursors_Next_Id'Img);

      Cursor_Name : constant String := Next_Multi_Cursor_Name;
      Cursor_Mark : constant Gtk_Text_Mark := Gtk_Text_Mark_New
        (Cursor_Name, False);
      Sel_Mark : constant Gtk_Text_Mark := Gtk_Text_Mark_New
        (Get_Sel_Mark_Name (Cursor_Name), False);
      T : Gtk_Text_Tag := Buffer.Get_Tag_Table.Lookup (Mc_Selection_Tag);
   begin
      if T = null then
         T := Buffer.Create_Tag (Mc_Selection_Tag);
         Glib.Properties.Set_Property (T, Background_Property, "green");
      end if;

      Buffer.Multi_Cursors_List.Append
        ((Mark            => Cursor_Mark,
          Sel_Mark        => Sel_Mark,
          Current_Command => null,
          Column_Memory   => Get_Offset (Location),
          Clipboard       => <>));

      Buffer.Add_Mark (Cursor_Mark, Location);
      Buffer.Add_Mark (Sel_Mark, Location);
      Buffer.Multi_Cursors_Next_Id := Buffer.Multi_Cursors_Next_Id + 1;
      Cursor_Mark.Set_Visible (True);
   end Add_Multi_Cursor;

   function Add_Multi_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter) return Cursor
   is
   begin
      Add_Multi_Cursor (Buffer, Location);
      declare
         Last_El : constant Multi_Cursors_Lists.Cursor :=
           Buffer.Multi_Cursors_List.Last;
      begin
         return Buffer.Multi_Cursors_List.Reference (Last_El).Element;
      end;
   end Add_Multi_Cursor;

   procedure Remove_All_Multi_Cursors (Buffer : Source_Buffer) is
   begin
      for Cursor of Buffer.Multi_Cursors_List loop
         null;
         Buffer.Delete_Mark (Cursor.Mark);
         Buffer.Delete_Mark (Cursor.Sel_Mark);
      end loop;

      Buffer.Has_MC_Clipboard := False;
      Buffer.Multi_Cursors_List.Clear;
   end Remove_All_Multi_Cursors;

   procedure Set_Multi_Cursors_Manual_Sync (Buffer : Source_Buffer)
   is
   begin
      Buffer.Multi_Cursors_Sync := (Mode => Manual_Master);
   end Set_Multi_Cursors_Manual_Sync;

   procedure Set_Multi_Cursors_Manual_Sync
     (Buffer  : Source_Buffer;
      MC_Mark : Gtk_Text_Mark)
   is
      MC : Multi_Cursor;
   begin
      for Multi of Buffer.Multi_Cursors_List loop
         if MC_Mark = Multi.Mark then
            MC := Multi;
         end if;
      end loop;
      Buffer.Multi_Cursors_Sync :=
        (Manual_Slave, To_Unbounded_String (MC_Mark.Get_Name), MC);
   end Set_Multi_Cursors_Manual_Sync;

   procedure Set_Multi_Cursors_Manual_Sync
     (Buffer : Source_Buffer;
      MC     : Multi_Cursor)
   is
   begin
      Buffer.Multi_Cursors_Sync :=
        (Manual_Slave, To_Unbounded_String (MC.Mark.Get_Name), MC);
   end Set_Multi_Cursors_Manual_Sync;

   procedure Set_Multi_Cursors_Auto_Sync (Buffer : Source_Buffer)
   is
   begin
      Buffer.Multi_Cursors_Sync := (Mode => Auto);
   end Set_Multi_Cursors_Auto_Sync;

   function Get_Multi_Cursors
     (Buffer : Source_Buffer) return Cursors_Lists.List
   is
      package L renames Multi_Cursors_Lists;
      C : L.Cursor;
   begin
      return List : Cursors_Lists.List do
         C := Buffer.Multi_Cursors_List.First;
         while L.Has_Element (C) loop
            List.Append (Buffer.Multi_Cursors_List.Reference (C).Element);
            C := L.Next (C);
         end loop;
      end return;
   end Get_Multi_Cursors;

   function Get_Multi_Cursors_Sync
     (Buffer : Source_Buffer) return Multi_Cursors_Sync_Type
   is (Buffer.Multi_Cursors_Sync);

   procedure Set_Multi_Cursors_Sync
     (Buffer : Source_Buffer; Sync : Multi_Cursors_Sync_Type)
   is
   begin
      Buffer.Multi_Cursors_Sync := Sync;
   end Set_Multi_Cursors_Sync;

end Src_Editor_Buffer.Multi_Cursors;
