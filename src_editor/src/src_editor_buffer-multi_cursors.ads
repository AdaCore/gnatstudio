with Gtk.Text_Iter; use Gtk.Text_Iter;

package Src_Editor_Buffer.Multi_Cursors is

   procedure Add_Multi_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter);
   --  Add a new multi cursor at the specified location
   --  in the specified buffer

   procedure Remove_All_Multi_Cursors (Buffer : Source_Buffer);
   --  Remove all multi cursors from the current buffer

   procedure Set_Multi_Cursors_Manual_Sync (Buffer : Source_Buffer);
   --  This sets the buffer in "main manual mode" regarding multi cursor
   --  insertion. It should be called before the main cursor action is done
   --  This basically means that in this mode, if any action is performed :
   --  - It wont impact any multi cursor
   --  - The main cursor will move accordingly to the action

   procedure Set_Multi_Cursors_Manual_Sync
     (Buffer  : Source_Buffer;
      MC_Mark : Gtk_Text_Mark);
   --  This sets the buffer in "slave manual mode" regarding multi cursor
   --  insertion, with the corresponding text mark as the multi-cursors mark.
   --  This should be called before the corresponding multi cursor's action is
   --  done. This basically means that in this mode, if any action is
   --  performed :
   --  - It wont impact any multi cursor
   --  - The main cursor will not move
   --  The action will be recorded as part of the same group as the main
   --  cursor's action regarding undo/redo groups.

   procedure Set_Multi_Cursors_Auto_Sync (Buffer : Source_Buffer);
   --  This sets the buffer in auto mode regarding multi cursor insertion.
   --  This means that every insert/delete will impact every active cursors
   --  in the buffer. Do not forget to set that back after a manual multi
   --  cursor operation !

   function Get_Multi_Cursors_Marks
     (Buffer : Source_Buffer) return Marks_Lists.List;
   --  Return a full list of all multi cursor's marks.

   function Get_Multi_Cursors_Sync
     (Buffer : Source_Buffer) return Multi_Cursors_Sync_Type;

end Src_Editor_Buffer.Multi_Cursors;
