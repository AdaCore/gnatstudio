package body VCS is

   -----------------------------
   -- Register_Error_Function --
   -----------------------------

   procedure Register_Error_Function
     (Rep  : access VCS_Record;
      Func : Error_Function;
      Data : Gtk.Widget.Gtk_Widget)
   is
   begin
      Rep.User_Data := Data;
      Rep.Local_Error_Function := Func;
   end Register_Error_Function;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (Rep     : access VCS_Record;
      Message : String)
   is
   begin
      if Rep.Local_Error_Function /= null then
         Rep.Local_Error_Function (Message, Rep.User_Data);
      end if;
   end Set_Error;

end VCS;
