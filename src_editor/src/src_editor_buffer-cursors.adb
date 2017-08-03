------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Gtk.Text_Tag; use Gtk.Text_Tag;
with Glib.Properties;
with Default_Preferences; use Default_Preferences;

package body Src_Editor_Buffer.Cursors is

   Mc_Selection_Tag : constant String := "mc_selection";
   Selection_Pref_Name : constant String :=
     "Plugins/multi_cursors/multicursor_selection_color";

   procedure Check_Mc_Selection_Tag (Buffer : Source_Buffer);

   procedure Remove_Slave_Cursor
     (Buffer : Source_Buffer;
      Cursor : Slave_Cursor);

   function Exists
     (Buffer : Source_Buffer;
      Id     : Integer)
      return Boolean;
   --  Check whether the slave cursor with given id is still exist

   ----------------------------
   -- Check_Mc_Selection_Tag --
   ----------------------------

   procedure Check_Mc_Selection_Tag (Buffer : Source_Buffer) is
      T : Gtk_Text_Tag := Buffer.Get_Tag_Table.Lookup (Mc_Selection_Tag);
      P : constant Preference :=
        Buffer.Kernel.Get_Preferences.Get_Pref_From_Name
          (Selection_Pref_Name, True);
      Color_Name : constant String := P.Get_Pref;
   begin
      if T = null then
         T := Buffer.Create_Tag (Mc_Selection_Tag);
      end if;

      if Color_Name /= "" then
         Glib.Properties.Set_Property (T, Background_Property, Color_Name);
      end if;
   end Check_Mc_Selection_Tag;

   ------------
   -- Create --
   ------------

   function Create
     (C : Slave_Cursor_Access; Buffer : Source_Buffer) return Cursor
   is
     ((Is_Main_Cursor => False,
       Cursor         => C,
       Cursor_Id      => C.Id,
       Buffer         => Buffer));

   --------------
   -- Is_Alive --
   --------------

   function Is_Alive (C : Cursor) return Boolean is
     (C.Is_Main_Cursor
      or else Exists (C.Buffer, C.Cursor_Id));

   ------------
   -- Exists --
   ------------

   function Exists
     (Buffer : Source_Buffer;
      Id     : Integer)
      return Boolean is
   begin
      for Cursor of Buffer.Slave_Cursors_List loop
         if Cursor.Id = Id then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   -------------------------
   -- Update_MC_Selection --
   -------------------------

   procedure Update_MC_Selection (B : Source_Buffer) is
      Start_Loc, End_Loc : Gtk_Text_Iter;
      T : Gtk_Text_Tag;
      Line : Editable_Line_Type;
      Col  : Character_Offset_Type;
   begin
      Check_Mc_Selection_Tag (B);
      T := B.Get_Tag_Table.Lookup (Mc_Selection_Tag);
      B.Get_Start_Iter (Start_Loc);
      B.Get_End_Iter (End_Loc);
      B.Remove_Tag (T, Start_Loc, End_Loc);

      for C of B.Slave_Cursors_List loop
         B.Get_Iter_At_Mark (Start_Loc, C.Sel_Mark);
         B.Get_Iter_At_Mark (End_Loc, C.Mark);

         Get_Iter_Position (B, End_Loc, Line, Col);

         B.Apply_Tag (T, Start_Loc, End_Loc);
      end loop;
   end Update_MC_Selection;

   --------------
   -- Get_Mark --
   --------------

   function Get_Mark (C : Cursor) return Gtk_Text_Mark
   is (if C.Is_Main_Cursor then C.Buffer.Get_Insert
       else C.Cursor.Mark);

   -----------------------
   -- Get_Sel_Mark_Name --
   -----------------------

   function Get_Sel_Mark_Name (Cursor_Mark_Name : String) return String
   is
     (Cursor_Mark_Name & "_sel");

   ------------------
   -- Get_Sel_Mark --
   ------------------

   function Get_Sel_Mark (C : Cursor) return Gtk_Text_Mark
   is (if C.Is_Main_Cursor then C.Buffer.Get_Mark ("selection_bound")
       else C.Cursor.Sel_Mark);

   -----------------------
   -- Get_Column_Memory --
   -----------------------

   function Get_Column_Memory (C : Cursor) return Gint
   is (if C.Is_Main_Cursor then C.Buffer.Cursor_Column_Memory
       else C.Cursor.Column_Memory);

   -----------------------
   -- Set_Column_Memory --
   -----------------------

   procedure Set_Column_Memory (C : Cursor; Offset : Gint) is
   begin
      if C.Is_Main_Cursor then
         C.Buffer.Cursor_Column_Memory := Offset;
      else
         C.Cursor.Column_Memory := Offset;
      end if;
   end Set_Column_Memory;

   ----------------
   -- Add_Cursor --
   ----------------

   procedure Add_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter)
   is
      Cursor_Name : constant String :=
        "slave_cursor_" & Buffer.Slave_Cursors_Next_Id'Img;
      Cursor_Mark : constant Gtk_Text_Mark := Gtk_Text_Mark_New
        (Cursor_Name, False);
      Sel_Mark    : constant Gtk_Text_Mark := Gtk_Text_Mark_New
        (Get_Sel_Mark_Name (Cursor_Name), False);
   begin
      Check_Mc_Selection_Tag (Buffer);

      Buffer.Slave_Cursors_List.Append
        ((Id              => Buffer.Slave_Cursors_Next_Id,
          Mark            => Cursor_Mark,
          Sel_Mark        => Sel_Mark,
          Current_Command => null,
          Column_Memory   => Get_Offset (Location),
          Clipboard       => <>));

      Buffer.Add_Mark (Cursor_Mark, Location);
      Buffer.Add_Mark (Sel_Mark, Location);
      Buffer.Slave_Cursors_Next_Id := Buffer.Slave_Cursors_Next_Id + 1;
      Cursor_Mark.Set_Visible (True);
   end Add_Cursor;

   ----------------
   -- Add_Cursor --
   ----------------

   function Add_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter) return Cursor
   is
   begin
      Add_Cursor (Buffer, Location);
      declare
         Last_El : constant Slave_Cursors_Lists.Cursor :=
           Buffer.Slave_Cursors_List.Last;
      begin
         return Create
           (Buffer.Slave_Cursors_List.Reference (Last_El).Element, Buffer);
      end;
   end Add_Cursor;

   -------------------
   -- Delete_Cursor --
   -------------------

   procedure Delete_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter)
   is
      use Slave_Cursors_Lists;
      C    : Slave_Cursors_Lists.Cursor := Buffer.Slave_Cursors_List.First;
      Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      while Has_Element (C) loop
         Buffer.Get_Iter_At_Mark (Iter, Element (C).Mark);
         if Equal (Iter, Location) then
            Remove_Slave_Cursor (Buffer, Element (C));
            Buffer.Slave_Cursors_List.Delete (C);
            return;
         end if;
         Next (C);
      end loop;
   end Delete_Cursor;

   -------------------------
   -- Remove_Slave_Cursor --
   -------------------------

   procedure Remove_Slave_Cursor
     (Buffer : Source_Buffer;
      Cursor : Slave_Cursor) is
   begin
      Buffer.Delete_Mark (Cursor.Mark);
      Buffer.Delete_Mark (Cursor.Sel_Mark);

      if Buffer.Slave_Cursors_List.Is_Empty then
         Buffer.Has_MC_Clipboard := False;
      end if;
   end Remove_Slave_Cursor;

   ------------------------------
   -- Remove_All_Slave_Cursors --
   ------------------------------

   procedure Remove_All_Slave_Cursors (Buffer : Source_Buffer) is
   begin
      for Cursor of Buffer.Slave_Cursors_List loop
         Remove_Slave_Cursor (Buffer, Cursor);
      end loop;

      Buffer.Slave_Cursors_List.Clear;
   end Remove_All_Slave_Cursors;

   ---------------------
   -- Set_Manual_Sync --
   ---------------------

   procedure Set_Manual_Sync
     (C : Cursor)
   is
   begin
      C.Buffer.Cursors_Sync := (if C.Is_Main_Cursor
                                then (Mode => Manual_Master)
                                else (Manual_Slave, C.Cursor));
   end Set_Manual_Sync;

   ---------------------------
   -- Set_Cursors_Auto_Sync --
   ---------------------------

   procedure Set_Cursors_Auto_Sync (Buffer : Source_Buffer)
   is
   begin
      Buffer.Cursors_Sync := (Mode => Auto);
   end Set_Cursors_Auto_Sync;

   -----------------------
   -- Has_Slave_Cursors --
   -----------------------

   function Has_Slave_Cursors
     (Buffer : Source_Buffer) return Boolean
   is (not Buffer.Slave_Cursors_List.Is_Empty);

   ----------
   -- Move --
   ----------

   procedure Move
     (C : Cursor; Loc : Gtk_Text_Iter; Extend_Selection : Boolean)
   is
   begin
      if C.Is_Main_Cursor and then not Extend_Selection then
         C.Buffer.Place_Cursor (Loc);
      else
         C.Buffer.Move_Mark (Get_Mark (C), Loc);
         if not Extend_Selection then
            C.Buffer.Move_Mark (Get_Sel_Mark (C), Loc);
         end if;
      end if;
   end Move;

   -----------------
   -- Get_Cursors --
   -----------------

   function Get_Cursors
     (Buffer : Source_Buffer) return Cursors_Lists.List
   is
      package L renames Slave_Cursors_Lists;
      C : L.Cursor;
   begin
      return List : Cursors_Lists.List do
         C := Buffer.Slave_Cursors_List.First;
         List.Append (Get_Main_Cursor (Buffer));
         while L.Has_Element (C) loop
            List.Append
              (Create
                 (Buffer.Slave_Cursors_List.Reference (C).Element, Buffer));
            C := L.Next (C);
         end loop;
      end return;
   end Get_Cursors;

   ----------------------
   -- Get_Cursors_Sync --
   ----------------------

   function Get_Cursors_Sync
     (Buffer : Source_Buffer) return Cursors_Sync_Type
   is (Buffer.Cursors_Sync);

   ----------------------
   -- Set_Cursors_Sync --
   ----------------------

   procedure Set_Cursors_Sync
     (Buffer : Source_Buffer; Sync : Cursors_Sync_Type)
   is
   begin
      Buffer.Cursors_Sync := Sync;
   end Set_Cursors_Sync;

end Src_Editor_Buffer.Cursors;
