with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Src_Editor_Buffer.Multi_Cursors is

   procedure Add_Multi_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter)
   is
      function Next_Multi_Cursor_Name return String is
        ("multi_cursor_" & Buffer.Multi_Cursors_Next_Id'Img);

      Cursor_Mark : constant Gtk_Text_Mark := Gtk_Text_Mark_New
        (Next_Multi_Cursor_Name, False);
   begin
      Buffer.Multi_Cursors_List.Append ((Mark => Cursor_Mark,
                                         Current_Command => null));
      Buffer.Add_Mark (Cursor_Mark, Location);
      Buffer.Multi_Cursors_Next_Id := Buffer.Multi_Cursors_Next_Id + 1;
      Cursor_Mark.Set_Visible (True);
   end Add_Multi_Cursor;

   procedure Remove_All_Multi_Cursors (Buffer : Source_Buffer)
   is
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
      Buffer.Multi_Cursors_Current_Cursor_Name := To_Unbounded_String ("");
      Buffer.Multi_Cursors_Sync_Mode := Manual_Master;
   end Set_Multi_Cursors_Manual_Sync;

   procedure Set_Multi_Cursors_Manual_Sync
     (Buffer  : Source_Buffer;
      MC_Mark : Gtk_Text_Mark) is
   begin
      Buffer.Multi_Cursors_Sync_Mode := Manual_Slave;
      Buffer.Multi_Cursors_Current_Cursor_Name
        := To_Unbounded_String (MC_Mark.Get_Name);
   end Set_Multi_Cursors_Manual_Sync;

   procedure Set_Multi_Cursors_Auto_Sync (Buffer : Source_Buffer)
   is
   begin
      Buffer.Multi_Cursors_Current_Cursor_Name := To_Unbounded_String ("");
      Buffer.Multi_Cursors_Sync_Mode := Auto;
   end Set_Multi_Cursors_Auto_Sync;

   function Get_Multi_Cursors_Marks
     (Buffer : Source_Buffer) return Marks_Lists.List is
   begin
      return List : Marks_Lists.List do
         for Cursor of Buffer.Multi_Cursors_List loop
            List.Append (Cursor.Mark);
         end loop;
      end return;
   end Get_Multi_Cursors_Marks;

   function Get_Multi_Cursors_Sync_Mode
     (Buffer : Source_Buffer) return Multi_Cursors_Sync_Mode_Type
   is (Buffer.Multi_Cursors_Sync_Mode);

end Src_Editor_Buffer.Multi_Cursors;
