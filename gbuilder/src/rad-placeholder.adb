with RAD.Editor; use RAD.Editor;

package body RAD.Placeholder is

   Placeholder_Width  : constant := 16;
   --  Minimal width of a placeholder

   Placeholder_Height : constant := 16;
   --  Minimal height of a placeholder

   procedure Gtk_New (Placeholder : out RAD_Placeholder) is
   begin
      Placeholder := new RAD_Placeholder_Record;
   end Gtk_New;

   procedure Initialize (Placeholder : access RAD_Placeholder_Record'Class) is
   begin
      Drawing_Area.Initialize (Placeholder);
      Set_Usize (Placeholder, Placeholder_Width, Placeholder_Height);
      Add_Draw_Signals (Placeholder);
      Add_Mouse_Signals (Placeholder);
      Set_Is_Placeholder (Placeholder);
      --  Connect to destroy signal ???
   end Initialize;

end RAD.Placeholder;
