------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with Glib;               use Glib;
with Gtk;                use Gtk;
with Gtk.Box;            use Gtk.Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Check_Button;   use Gtk.Check_Button;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Label;          use Gtk.Label;
with Gtk.Stock;          use Gtk.Stock;
with Gtk.Text_Buffer;    use Gtk.Text_Buffer;
with Gtk.Text_Iter;      use Gtk.Text_Iter;
with Gtk.Text_View;      use Gtk.Text_View;
with Gtk.Widget;         use Gtk.Widget;

package body Std_Dialogs is

   ------------------------
   -- Text Input Dialogs --
   ------------------------

   type Text_Input_Dialog_Record is abstract
   new Gtk_Dialog_Record and Text_Input_Dialog_Interface with record
      Main_View    : Gtk_Vbox;
      --  The text input dialog's main view

      Check        : Gtk_Check_Button;
      --  First optional check box

      Check2       : Gtk_Check_Button;
      --  Second optional check box
   end record;
   type Text_Input_Dialog is access
     all Text_Input_Dialog_Record'Class;
   --  Base type for all the text input dialogs

   procedure Initialize
     (Dialog     : access Text_Input_Dialog_Record'Class;
      Parent     : access Gtk.Window.Gtk_Window_Record'Class;
      Extra_Box  : Gtk_Check_Button := null;
      Extra_Box2 : Gtk_Check_Button := null;
      Title      : String;
      Message    : String;
      Position   : Gtk_Window_Position := Win_Pos_Center_On_Parent;
      History    : Histories.History;
      Key        : History_Key := "");
   --  Initialize the common attributes of text input dialogs

   function Run_And_Get_Input
     (Self    : not null access Text_Input_Dialog_Record'Class;
      History : Histories.History;
      Key     : History_Key) return String;
   --  Run the text input dialog and return the input typed by the user

   ------------------------------------
   -- Single Line Text Input Dialogs --
   ------------------------------------

   type Single_Line_Text_Input_Dialog_Record is new Text_Input_Dialog_Record
   with record
      Combo_Entry : Gtk_Combo_Box_Text;
   end record;
   --  Type used to represent single line text input dialogs, which use
   --  a combobox with an entry to retrieve the user's input.

   overriding procedure Initialize
     (Self    : not null access Single_Line_Text_Input_Dialog_Record;
      Message : String;
      History : Histories.History;
      Key     : Histories.History_Key);
   overriding function Get_Input_Text
     (Self : not null access Single_Line_Text_Input_Dialog_Record)
      return String;

   -----------------------------------
   -- Multi Line Text Input Dialogs --
   -----------------------------------

   type Multi_Line_Text_Input_Dialog_Record is new Text_Input_Dialog_Record
   with record
      Text_View : Gtk_Text_View;
   end record;
   --  Type used to represent multi-line text input dialogs, which use a
   --  text view to retrieve the user's input.

   overriding procedure Initialize
     (Self    : not null access Multi_Line_Text_Input_Dialog_Record;
      Message : String;
      History : Histories.History;
      Key     : Histories.History_Key);
   overriding function Get_Input_Text
     (Self : not null access Multi_Line_Text_Input_Dialog_Record)
      return String;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self    : not null access Single_Line_Text_Input_Dialog_Record;
      Message : String;
      History : Histories.History;
      Key     : Histories.History_Key)
   is
      Label : Gtk_Label;
      Hbox  : Gtk_Hbox;
   begin
      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Self.Main_View.Pack_Start (Hbox, Expand => False);

      Gtk_New (Label, Message);
      Label.Set_Halign (Align_Start);
      Hbox.Pack_Start (Label, Expand => False, Padding => 5);

      Gtk_New_With_Entry (Self.Combo_Entry);
      Hbox.Pack_Start (Self.Combo_Entry);

      if Key /= "" and then History /= null then
         Get_History (History.all, Key, Self.Combo_Entry);
      end if;
   end Initialize;

   --------------------
   -- Get_Input_Text --
   --------------------

   overriding function Get_Input_Text
     (Self : not null access Single_Line_Text_Input_Dialog_Record)
      return String
   is
      (Self.Combo_Entry.Get_Active_Text);

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self    : not null access Multi_Line_Text_Input_Dialog_Record;
      Message : String;
      History : Histories.History;
      Key     : Histories.History_Key)
   is
      Label_Widget : Gtk_Label;
   begin
      --  Set a default size so that users can see that it is a multi-line
      --  text view.
      Self.Set_Default_Size (400, 200);

      Gtk_New (Label_Widget, Message);
      Label_Widget.Set_Halign (Align_Start);
      Self.Main_View.Pack_Start (Label_Widget, Expand => False, Padding => 5);

      Gtk_New (Self.Text_View);
      Self.Text_View.Set_Wrap_Mode (Wrap_Char);

      Self.Main_View.Pack_Start (Self.Text_View);

      if Key /= "" and then History /= null then
         Self.Text_View.Get_Buffer.Set_Text (Most_Recent (History, Key));
      end if;
   end Initialize;

   --------------------
   -- Get_Input_Text --
   --------------------

   overriding function Get_Input_Text
     (Self : not null access Multi_Line_Text_Input_Dialog_Record)
      return String
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Buffer               : constant Gtk_Text_Buffer :=
                               Self.Text_View.Get_Buffer;
   begin
      Buffer.Get_Start_Iter (Start_Iter);
      Buffer.Get_End_Iter (End_Iter);

      return Buffer.Get_Text (Start_Iter, End_Iter);
   end Get_Input_Text;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Dialog          : access Text_Input_Dialog_Record'Class;
      Parent          : access Gtk_Window_Record'Class;
      Extra_Box       : Gtk_Check_Button := null;
      Extra_Box2      : Gtk_Check_Button := null;
      Title           : String;
      Message         : String;
      Position        : Gtk_Window_Position := Win_Pos_Center_On_Parent;
      History         : Histories.History;
      Key             : History_Key := "")
   is
      Button : Gtk_Widget;
      pragma Unreferenced (Button);
   begin
      Gtk.Dialog.Initialize
        (Dialog,
         Title  => Title,
         Parent => Gtk_Window (Parent),
         Flags  => Use_Header_Bar_From_Settings (Parent));
      Dialog.Set_Modal;
      Dialog.Set_Position (Position);

      Gtk_New_Vbox (Dialog.Main_View, Homogeneous => False);
      Dialog.Get_Content_Area.Pack_Start (Dialog.Main_View);

      Initialize
        (Dialog,
         Message => Message,
         History => History,
         Key     => Key);

      if Extra_Box /= null then
         Dialog.Main_View.Pack_Start (Extra_Box, Expand => False);
      end if;

      if Extra_Box2 /= null then
         Dialog.Main_View.Pack_Start (Extra_Box2, Expand => False);
      end if;

      Button := Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
      Button := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Dialog.Set_Default_Response (Gtk_Response_OK);
   end Initialize;

   -----------------------
   -- Run_And_Get_Input --
   -----------------------

   function Run_And_Get_Input
     (Self    : not null access Text_Input_Dialog_Record'Class;
      History : Histories.History;
      Key     : History_Key) return String is
   begin
      Self.Show_All;

      if Self.Run = Gtk_Response_OK then
         declare
            S : constant String := Self.Get_Input_Text;
         begin
            if History /= null then
               Add_To_History (History.all, Key, S);
            end if;

            return S;
         end;
      end if;

      return (1 => ASCII.NUL);
   end Run_And_Get_Input;

   -------------------------------
   -- Display_Text_Input_Dialog --
   -------------------------------

   function Display_Text_Input_Dialog
     (Parent         : access Gtk_Window_Record'Class;
      Title          : String;
      Message        : String;
      Position       : Gtk_Window_Position := Win_Pos_Center_On_Parent;
      Multi_Line     : Boolean := False;
      History        : Histories.History := null;
      Key            : History_Key := "";
      Check_Msg      : String := "";
      Button_Active  : Boolean_Access := null;
      Key_Check      : Histories.History_Key := "";
      Check_Msg2     : String := "";
      Button2_Active : Boolean_Access := null;
      Key_Check2     : Histories.History_Key := "") return String
   is
      Dialog : constant Text_Input_Dialog :=
                 (if Multi_Line then
                     new Multi_Line_Text_Input_Dialog_Record
                  else
                     new Single_Line_Text_Input_Dialog_Record);
   begin
      if Check_Msg /= "" then
         Gtk_New (Dialog.Check, Check_Msg);
         Associate (History.all, Key_Check, Dialog.Check);
      end if;

      if Check_Msg2 /= "" then
         Gtk_New (Dialog.Check2, Check_Msg2);
         Associate (History.all, Key_Check2, Dialog.Check2);
      end if;

      Initialize
        (Dialog,
         Parent    => Parent,
         Extra_Box  => Dialog.Check,
         Extra_Box2 => Dialog.Check2,
         Title      => Title,
         Message    => Message,
         Position   => Position,
         History    => History,
         Key        => Key);

      declare
         Input : constant String := Dialog.Run_And_Get_Input (History, Key);
      begin
         if Dialog.Check /= null then
            Button_Active.all := Get_Active (Dialog.Check);
         end if;

         if Dialog.Check2 /= null then
            Button2_Active.all := Get_Active (Dialog.Check2);
         end if;

         Destroy (Dialog);

         return Input;
      end;
   end Display_Text_Input_Dialog;

end Std_Dialogs;
