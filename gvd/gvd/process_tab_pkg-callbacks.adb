with System; use System;
with Glib; use Glib;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Handlers; use Gtk.Handlers;
with Unchecked_Conversion;

package body Process_Tab_Pkg.Callbacks is

   use Gtk.Arguments;

   ----------------------------------
   -- On_Debugger_Text_Insert_Text --
   ----------------------------------

   procedure On_Debugger_Text_Insert_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : String := To_String (Params, 1);
      --  Arg2 : Gint := To_Gint (Params, 2);
      Arg3 : Address := To_Address (Params, 3);

      Top  : Process_Tab_Access := Process_Tab_Access (Object);

      type Guint_Ptr is access all Guint;
      function To_Guint_Ptr is new Unchecked_Conversion (Address, Guint_Ptr);
      use Odd_Tools.Process;

   begin
      if To_Guint_Ptr (Arg3).all < Top.Edit_Pos then
         Emit_Stop_By_Name (Top.Debugger_Text, "insert_text");
      else
         if Arg1 (Arg1'First) = ASCII.LF then
            Send_Command
              (Top.Debugger,
               Get_Chars (Top.Debugger_Text, Gint (Top.Edit_Pos)));
         end if;
      end if;
   end On_Debugger_Text_Insert_Text;

   ----------------------------------
   -- On_Debugger_Text_Delete_Text --
   ----------------------------------

   procedure On_Debugger_Text_Delete_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      --  Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);

      Top  : Process_Tab_Access := Process_Tab_Access (Object);

   begin
      if Arg2 <= Gint (Top.Edit_Pos) then
         Emit_Stop_By_Name (Top.Debugger_Text, "delete_text");
      end if;
   end On_Debugger_Text_Delete_Text;

end Process_Tab_Pkg.Callbacks;
