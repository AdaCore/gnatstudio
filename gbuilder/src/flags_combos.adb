with Gdk.Cursor;               use Gdk.Cursor;
with Gdk.Event;                use Gdk.Event;
with Gdk.Main;                 use Gdk.Main;
with Gdk.Types;                use Gdk.Types;
with Gdk.Window;               use Gdk.Window;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib;                     use Glib;
with Gtk.Arrow;                use Gtk.Arrow;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Object;               use Gtk.Object;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;

with Interfaces.C.Strings;     use Interfaces.C.Strings;

package body Flags_Combos is

   procedure Popup_Window (Combo : access Gtk_Widget_Record'Class);
   --  Display the popup dialog associated with the combo box.

   function Hide_Dialog (Combo : access Gtk_Widget_Record'Class)
      return Boolean;
   --  Hides the dialog, and recomputes the value of the combo

   Flags_Class_Record        : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;

   Signals : constant chars_ptr_array :=
     (1 => New_String ("changed"));

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Combo : out Flags_Combo;
      Klass : Glib.Properties.Creation.Flags_Class) is
   begin
      Combo := new Flags_Combo_Record;
      Flags_Combos.Initialize (Combo, Klass);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Combo : access Flags_Combo_Record'Class;
      Klass : Glib.Properties.Creation.Flags_Class)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_None));
      Arrow : Gtk_Arrow;
   begin
      Initialize_Hbox (Combo, Homogeneous => False);
      Initialize_Class_Record
        (Combo, Signals, Flags_Class_Record,
         "FlagsCombo", Signal_Parameters);
      Combo.Klass := Klass;

      Gtk_New (Combo.Ent, 1024);
      Pack_Start (Combo, Combo.Ent, Expand => True, Fill => True);
      Set_Editable (Combo.Ent, False);

      Gtk_New (Combo.Toggle);
      Gtk_New (Arrow, Arrow_Down, Shadow_None);
      Add (Combo.Toggle, Arrow);
      Pack_Start (Combo, Combo.Toggle, Expand => False, Fill => False);

      Widget_Callback.Object_Connect
        (Combo.Toggle, "toggled",
         Widget_Callback.To_Marshaller (Popup_Window'Access), Combo);
   end Initialize;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Combo : access Flags_Combo_Record;
      Val   : Flags_Int_Value)
   is
      K     : Guint := 0;
      F_Val : Flags_Value;
   begin
      Combo.Value := Val;

      Set_Text (Combo.Ent, "");
      loop
         F_Val := Nth_Value (Combo.Klass, K);
         exit when F_Val = null;

         if (Val and Value (F_Val)) = Value (F_Val) then
            Prepend_Text (Combo.Ent, "1");
         else
            Prepend_Text (Combo.Ent, "0");
         end if;
         K := K + 1;
      end loop;

      Widget_Callback.Emit_By_Name (Combo, "changed");
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Combo : access Flags_Combo_Record) return
     Flags_Int_Value is
   begin
      return Combo.Value;
   end Get_Value;

   -----------------
   -- Hide_Dialog --
   -----------------

   function Hide_Dialog (Combo : access Gtk_Widget_Record'Class)
      return Boolean
   is
      use Widget_List;
      C : Flags_Combo := Flags_Combo (Combo);
      Frame : Gtk_Frame := Gtk_Frame (Get_Child (Gtk_Window (C.Win)));
      List : Glist := Children (Gtk_Box (Get_Child (Frame)));
      Tmp : Glist := First (List);
      Index : Guint := 0;
      Val : Flags_Int_Value := 0;
   begin
      while Tmp /= Null_List loop
         if Get_Active (Gtk_Check_Button (Get_Data (Tmp))) then
            Val := Val or Value (Nth_Value (C.Klass, Index));
         end if;
         Index := Index + 1;
         Tmp := Next (Tmp);
      end loop;

      Free (List);

      Set_Value (C, Val);

      Grab_Remove (C.Win);
      Pointer_Ungrab (Time => 0);
      Set_Active (C.Toggle, False);
      Destroy (C.Win);
      C.Win := null;
      return True;
   end Hide_Dialog;

   ------------------
   -- Popup_Window --
   ------------------

   procedure Popup_Window (Combo : access Gtk_Widget_Record'Class) is
      C : Flags_Combo := Flags_Combo (Combo);
      Frame : Gtk_Frame;
      Box : Gtk_Box;
      Toggle : Gtk_Check_Button;
      F_Val   : Flags_Value;
      K : Guint := 0;
      X, Y : Gint;
      Mask        : Gdk_Modifier_Type;
      Window      : Gdk_Window;
      Cursor : Gdk_Cursor;
      Tmp : Boolean;
      Req : Gtk_Requisition;
      X_Root, Y_Root, W_Root, H_Root, Depth : Gint;
   begin
      if not Get_Active (C.Toggle) then
         return;
      end if;

      Gtk_New (C.Win, Window_Popup);
      Add_Events (C.Win, Key_Press_Mask);

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_Out);
      Add (C.Win, Frame);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      loop
         F_Val := Nth_Value (C.Klass, K);
         exit when F_Val = null;
         Gtk_New (Toggle, Nick (F_Val));
         Pack_Start (Box, Toggle, Expand => False, Fill => False);
         Set_Active
           (Toggle,
            (C.Value and Value (F_Val)) = Value (F_Val));
         K := K + 1;
      end loop;

      Realize (C.Win);
      Get_Pointer (null, X, Y, Mask, Window);
      Get_Geometry (null, X_Root, Y_Root, W_Root, H_Root, Depth);

      --  We have to temporarily show the window, so as to get its
      --  proper requested size. However, we try to avoid flicker by
      --  displaying it on an invisible part of the screen
      Move (Get_Window (C.Win), W_Root + 1, H_Root + 1);
      Show_All (C.Win);
      Size_Request (C.Win, Req);

      if Y + 10 + Req.Height > H_Root then
         Y := Gint'Max (0, H_Root - 10 - Req.Height);
      end if;

      --  ??? Would be nicer to popdown the dialog so that it is aligned
      --  with the left of the Gtk_Entry.

      if X + 10 + Req.Width > W_Root then
         X := Gint'Max (0, W_Root - 10 - Req.Width);
      end if;

      Move (Get_Window (C.Win), X + 10, Y + 10);

      Return_Callback.Object_Connect
        (C.Win, "button_press_event",
         Return_Callback.To_Marshaller (Hide_Dialog'Access), C);

      Grab_Add (C.Win);
      Gdk_New (Cursor, Top_Left_Arrow);
      Tmp := Pointer_Grab
        (Get_Window (C.Win),
         True,
         Button_Press_Mask or Button_Release_Mask,
         Cursor => Cursor,
         Time => 0);
      Destroy (Cursor);
   end Popup_Window;

end Flags_Combos;

