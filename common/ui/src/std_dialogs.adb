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

with Glib;            use Glib;
with Gtk;             use Gtk;
with Gtk.GEntry;
with Gtk.Label;       use Gtk.Label;
with Gtk.Stock;       use Gtk.Stock;
with Gtk.Widget;      use Gtk.Widget;
with Gtkada.Handlers; use Gtkada.Handlers;

package body Std_Dialogs is

   procedure On_Enter_Key_Press
     (Dialog : access Gtk_Widget_Record'Class);
   --  Called when the user presses the 'Enter' key.
   --  Validate the text input dialog.

   ------------------------
   -- On_Enter_Key_Press --
   ------------------------

   procedure On_Enter_Key_Press
     (Dialog : access Gtk_Widget_Record'Class) is
   begin
      Gtk_Dialog (Dialog).Response (Gtk_Response_OK);
   end On_Enter_Key_Press;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : not null access Text_Input_Dialog_Record'Class;
      Parent     : access Gtk.Window.Gtk_Window_Record'Class;
      Title      : String;
      Message    : String;
      Position   : Gtk_Window_Position := Win_Pos_Center_On_Parent;
      History    : Histories.History;
      Key        : History_Key := "";
      Check_Msg  : String := "";
      Key_Check  : Histories.History_Key := "";
      Check_Msg2 : String := "";
      Key_Check2 : Histories.History_Key := "")
   is
      Button : Gtk_Widget;
      pragma Unreferenced (Button);

      procedure Create_Text_Input_Widget;
      --  Create the text input field area, with its associated label

      procedure Create_Extra_Check_Boxes_If_Needed;
      --  Create extra checkboxes if Check_Msg and or Check_Msg2 are non-empty.
      ---
      --  If non-empty keys have been provided, associate them respectively
      --  with Key_Check and Key_Check2 in History.

      ------------------------------
      -- Create_Text_Input_Widget --
      ------------------------------

      procedure Create_Text_Input_Widget is
         Label : Gtk_Label;
         Hbox  : Gtk_Hbox;
      begin
         Gtk_New_Hbox (Hbox, Homogeneous => False);
         Self.Main_View.Pack_Start (Hbox, Expand => False);

         Gtk_New (Label, Message);
         Label.Set_Halign (Align_Start);
         Hbox.Pack_Start (Label, Expand => False, Padding => 5);

         Gtk_New_With_Entry (Self.Combo_Entry);

         Widget_Callback.Object_Connect
           (Widget      => Self.Combo_Entry.Get_Child,
            Name        => Gtk.GEntry.Signal_Activate,
            Cb          => On_Enter_Key_Press'Access,
            Slot_Object => Self);
         Hbox.Pack_Start (Self.Combo_Entry);

         if Key /= "" and then History /= null then
            Get_History (History.all, Key, Self.Combo_Entry);
         end if;
      end Create_Text_Input_Widget;

      ----------------------------------------
      -- Create_Extra_Check_Boxes_If_Needed --
      ----------------------------------------

      procedure Create_Extra_Check_Boxes_If_Needed is
      begin
         if Check_Msg /= "" then
            Gtk_New (Self.Check, Check_Msg);
            Self.Main_View.Pack_Start (Self.Check, Expand => False);

            if History /= null then
               Associate (History.all, Key_Check, Self.Check);
            end if;
         end if;

         if Check_Msg2 /= "" then
            Gtk_New (Self.Check2, Check_Msg2);
            Self.Main_View.Pack_Start (Self.Check2, Expand => False);

            if History /= null then
               Associate (History.all, Key_Check2, Self.Check2);
            end if;
         end if;
      end Create_Extra_Check_Boxes_If_Needed;

   begin
      Gtk.Dialog.Initialize
        (Self,
         Title  => Title,
         Parent => Gtk_Window (Parent),
         Flags  => Use_Header_Bar_From_Settings (Parent));
      Self.Set_Modal;
      Self.Set_Position (Position);

      Gtk_New_Vbox (Self.Main_View, Homogeneous => False);
      Self.Get_Content_Area.Pack_Start (Self.Main_View);

      Create_Text_Input_Widget;

      Create_Extra_Check_Boxes_If_Needed;

      Button := Self.Add_Button (Stock_Ok, Gtk_Response_OK);
      Button := Self.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Self.Set_Default_Response (Gtk_Response_OK);
   end Initialize;

   -----------------------
   -- Run_And_Get_Input --
   -----------------------

   function Run_And_Get_Input
     (Self           : not null access Text_Input_Dialog_Record'Class;
      History        : Histories.History;
      Key            : History_Key;
      Button_Active  : access Boolean := null;
      Button_Active2 : access Boolean := null) return String is
   begin
      Self.Show_All;

      if Self.Run = Gtk_Response_OK then
         declare
            S : constant String := Self.Combo_Entry.Get_Active_Text;
         begin
            if History /= null then
               Add_To_History (History.all, Key, S);
            end if;

            if Self.Check /= null then
               Button_Active.all := Self.Check.Get_Active;
            end if;

            if Self.Check2 /= null then
               Button_Active2.all := Self.Check2.Get_Active;
            end if;

            return S;
         end;
      end if;

      return (1 => ASCII.NUL);
   end Run_And_Get_Input;

end Std_Dialogs;
