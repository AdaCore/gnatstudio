-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2007                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                  use Glib;
with Gtk;                   use Gtk;
with Gtk.Box;               use Gtk.Box;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Label;             use Gtk.Label;
with Gtk.Combo;             use Gtk.Combo;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Stock;             use Gtk.Stock;

package body Std_Dialogs is

   type Simple_Entry_Dialog_Record is new Gtk_Dialog_Record with record
      Entry_Field : Gtk_Combo;
      Label       : Gtk_Label;
   end record;
   type Simple_Entry_Dialog_Access is access
     all Simple_Entry_Dialog_Record'Class;

   type Display_Dialog_Record is new Simple_Entry_Dialog_Record with record
      Check  : Gtk_Check_Button;
      Check2 : Gtk_Check_Button;
   end record;
   type Display_Dialog_Access is access all Display_Dialog_Record'Class;

   function Internal_Simple_Entry_Dialog
     (Dialog     : access Simple_Entry_Dialog_Record'Class;
      Parent     : access Gtk.Window.Gtk_Window_Record'Class;
      Extra_Box  : Gtk_Check_Button := null;
      Extra_Box2 : Gtk_Check_Button := null;
      Title      : String;
      Message    : String;
      Position   : Gtk_Window_Position := Win_Pos_Mouse;
      History    : Histories.History;
      Key        : History_Key := "") return String;
   --  Internal version of Simple_Entry_Dialog, where Dialog is already
   --  created.
   --  Dialog is not destroyed on exit, it is your responsability to do so.

   procedure Ok_Simple_Entry
     (Simple_Dialog : access Gtk_Widget_Record'Class);
   --  "Ok" was pressed in a simple entry dialog

   ----------------------------------
   -- Internal_Simple_Entry_Dialog --
   ----------------------------------

   function Internal_Simple_Entry_Dialog
     (Dialog          : access Simple_Entry_Dialog_Record'Class;
      Parent          : access Gtk_Window_Record'Class;
      Extra_Box       : Gtk_Check_Button := null;
      Extra_Box2      : Gtk_Check_Button := null;
      Title           : String;
      Message         : String;
      Position        : Gtk_Window_Position := Win_Pos_Mouse;
      History         : Histories.History;
      Key             : History_Key := "") return String
   is
      use Widget_List;
      Button : Gtk_Widget;
      pragma Unreferenced (Button);

      Box    : Gtk_Box;
      Vbox   : Gtk_Box;

   begin
      Set_Transient_For (Dialog, Parent);
      Set_Modal (Dialog);
      Set_Position (Dialog, Position);

      Gtk_New_Vbox (Vbox);
      Pack_Start (Get_Vbox (Dialog), Vbox, False);

      Gtk_New_Hbox (Box);
      Pack_Start (Vbox, Box, False, Padding => 10);

      Gtk_New (Dialog.Label, Message);
      Set_Alignment (Dialog.Label, 0.0, 0.5);
      Pack_Start (Box, Dialog.Label, False, Padding => 10);

      Gtk_New (Dialog.Entry_Field);
      Set_Case_Sensitive (Dialog.Entry_Field);
      Pack_Start (Box, Dialog.Entry_Field, Padding => 10);
      Disable_Activate (Dialog.Entry_Field);
      Widget_Callback.Object_Connect
        (Get_Entry (Dialog.Entry_Field), Signal_Activate,
         Widget_Callback.To_Marshaller (Ok_Simple_Entry'Access),
         Dialog);

      if Key /= "" and then History /= null then
         Get_History (History.all, Key, Dialog.Entry_Field);
      end if;

      if Extra_Box /= null then
         Gtk_New_Hbox (Box);
         Pack_Start (Vbox, Box);
         Pack_Start (Box, Extra_Box, Padding => 10);
      end if;

      if Extra_Box2 /= null then
         Gtk_New_Hbox (Box);
         Pack_Start (Vbox, Box);
         Pack_Start (Box, Extra_Box2, Padding => 10);
      end if;

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Set_Title (Dialog, Title);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            S : constant String := Get_Text (Get_Entry (Dialog.Entry_Field));
         begin
            if History /= null then
               Add_To_History (History.all, Key, S);
            end if;

            return S;
         end;
      end if;

      return (1 => ASCII.NUL);
   end Internal_Simple_Entry_Dialog;

   -------------------------
   -- Simple_Entry_Dialog --
   -------------------------

   function Simple_Entry_Dialog
     (Parent   : access Gtk_Window_Record'Class;
      Title    : String;
      Message  : String;
      Position : Gtk_Window_Position := Win_Pos_Mouse;
      History  : Histories.History := null;
      Key      : History_Key := "") return String
   is
      Dialog          : Simple_Entry_Dialog_Access;
   begin
      Dialog := new Simple_Entry_Dialog_Record;
      Initialize (Dialog);

      declare
         S : constant String := Internal_Simple_Entry_Dialog
           (Dialog, Parent, null, null, Title,
            Message, Position, History, Key);
      begin
         Destroy (Dialog);
         return S;
      end;
   end Simple_Entry_Dialog;

   --------------------------
   -- Display_Entry_Dialog --
   --------------------------

   function Display_Entry_Dialog
     (Parent         : access Gtk_Window_Record'Class;
      Title          : String;
      Message        : String;
      Position       : Gtk_Window_Position := Win_Pos_Mouse;
      Check_Msg      : String := "";
      History        : Histories.History;
      Key            : History_Key := "";
      Button_Active  : Boolean_Access := null;
      Key_Check      : Histories.History_Key := "";
      Check_Msg2     : String := "";
      Button2_Active : Boolean_Access := null;
      Key_Check2     : Histories.History_Key := "") return String
   is
      Dialog : Display_Dialog_Access;
   begin
      Dialog := new Display_Dialog_Record;
      Initialize (Dialog);

      if Check_Msg /= "" then
         Gtk_New (Dialog.Check, Check_Msg);
         Associate (History.all, Key_Check, Dialog.Check);
      end if;

      if Check_Msg2 /= "" then
         Gtk_New (Dialog.Check2, Check_Msg2);
         Associate (History.all, Key_Check2, Dialog.Check2);
      end if;

      declare
         S : constant String := Internal_Simple_Entry_Dialog
           (Dialog, Parent, Dialog.Check, Dialog.Check2,
            Title, Message, Position, History, Key);
      begin
         if Dialog.Check /= null then
            Button_Active.all := Get_Active (Dialog.Check);
         end if;

         if Dialog.Check2 /= null then
            Button2_Active.all := Get_Active (Dialog.Check2);
         end if;

         Destroy (Dialog);
         return S;
      end;
   end Display_Entry_Dialog;

   ---------------------
   -- Ok_Simple_Entry --
   ---------------------

   procedure Ok_Simple_Entry
     (Simple_Dialog : access Gtk_Widget_Record'Class) is
   begin
      Response (Gtk_Dialog (Simple_Dialog), Gtk_Response_OK);
   end Ok_Simple_Entry;

end Std_Dialogs;
