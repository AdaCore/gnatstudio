with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Src_Editor_Buffer.Multi_Cursors is

   function Get_Mark (C : Cursor) return Gtk_Text_Mark
   is (C.Mark);

   function Get_Column_Memory (C : Cursor) return Gint
   is (C.Column_Memory);

   procedure Set_Column_Memory (C : Cursor; Offset : Gint) is
   begin
      C.Column_Memory := Offset;
   end Set_Column_Memory;

   procedure Add_Multi_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter)
   is
      function Next_Multi_Cursor_Name return String is
        ("multi_cursor_" & Buffer.Multi_Cursors_Next_Id'Img);

      Cursor_Mark : constant Gtk_Text_Mark := Gtk_Text_Mark_New
        (Next_Multi_Cursor_Name, False);
   begin
      Buffer.Multi_Cursors_List.Append
        ((Mark => Cursor_Mark,
          Current_Command => null,
          Column_Memory => Get_Offset (Location)));
      Buffer.Add_Mark (Cursor_Mark, Location);
      Buffer.Multi_Cursors_Next_Id := Buffer.Multi_Cursors_Next_Id + 1;
      Cursor_Mark.Set_Visible (True);
   end Add_Multi_Cursor;

   procedure Remove_All_Multi_Cursors (Buffer : Source_Buffer) is
   begin
      for Cursor of Buffer.Multi_Cursors_List loop
         Buffer.Delete_Mark (Cursor.Mark);
         Cursor.Mark.Deallocate;
      end loop;
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
