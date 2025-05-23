------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2025, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with VSS.Strings.Conversions;

with GNATCOLL.VFS;          use GNATCOLL.VFS;
with Gdk.Event;             use Gdk.Event;
with Glib.Object;           use Glib.Object;
with Gtk;                   use Gtk;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Label;             use Gtk.Label;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;
with Gtkada.Stock_Labels;   use Gtkada.Stock_Labels;

with GPS.Kernel.MDI;        use GPS.Kernel.MDI;
with GPS.Main_Window;       use GPS.Main_Window;

package body GPS.Dialogs is

   procedure On_Enter_Key_Press (Dialog : access GObject_Record'Class);
   --  Called when the user presses the 'Enter' key.
   --  Validate the text input dialog.

   procedure On_Destroy_Combo (Self : access Gtk_Widget_Record'Class);
   --  Called when Self is destroyed

   function On_GPS_Dialog_Focus_In
     (Dialog : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Focus) return Boolean;
   --  Called when a dialog gets the focus. This updates the kernel context.

   type Browse_Button_Record is new Gtk_Button_Record with record
      Kernel            : Kernel_Handle;
      Ent               : Gtk_Entry;
      Use_Native_Dialog : Boolean;
      Key               : Unbounded_String;
      File_Pattern      : Unbounded_String;
      Pattern_Name      : Unbounded_String;
      Default_Name      : Unbounded_String;
      Kind              : File_Selector_Kind := Unspecified;
   end record;
   type Browse_Button is access all Browse_Button_Record'Class;

   procedure On_Browse_Button_Clicked
     (Self : access Gtk_Button_Record'Class);

   ------------------------------
   -- On_Browse_Button_Clicked --
   ------------------------------

   procedure On_Browse_Button_Clicked
     (Self : access Gtk_Button_Record'Class)
   is
      Button : constant Browse_Button := Browse_Button (Self);
      Name   : constant Virtual_File :=
        Select_File
          (Title             => "Select file",
           Parent            => Get_Main_Window (Button.Kernel),
           File_Pattern      => +(To_String (Button.File_Pattern)),
           Pattern_Name      => To_String (Button.Pattern_Name),
           Default_Name      => +(To_String (Button.Default_Name)),
           Use_Native_Dialog => Button.Use_Native_Dialog,
           Kind              => Save_File,
           Remote_Browsing   => True,
           History           => Get_History (Button.Kernel));
   begin
      if Name /= GNATCOLL.VFS.No_File then
         Button.Ent.Set_Text (Name.Display_Full_Name);
         Add_To_History
           (Handle    => Button.Kernel,
            Key       => Histories.History_Key (To_String (Button.Key)),
            New_Entry => Name.Display_Full_Name);
      end if;
   end On_Browse_Button_Clicked;

   ----------------------------
   -- On_GPS_Dialog_Focus_In --
   ----------------------------

   function On_GPS_Dialog_Focus_In
     (Dialog : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Focus) return Boolean
   is
      Self : constant GPS_Dialog := GPS_Dialog (Dialog);
      pragma Unreferenced (Event);
   begin
      Self.Kernel.Context_Changed (No_Context);
      return False;   --  propagate the event
   end On_GPS_Dialog_Focus_In;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self           : out GPS_Dialog;
      Title          : Glib.UTF8_String;
      Kernel         : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Flags          : Gtk_Dialog_Flags := Destroy_With_Parent;
      Typ            : Glib.GType := Gtk.Dialog.Get_Type;
      Default_Width  : Glib.Gint := -1;
      Default_Length : Glib.Gint := -1) is
   begin
      Self := new GPS_Dialog_Record;
      Initialize
        (Self, Title, Kernel, Flags, Typ, Default_Width, Default_Length);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : not null access GPS_Dialog_Record'Class;
      Title          : Glib.UTF8_String;
      Kernel         : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Flags          : Gtk_Dialog_Flags := Destroy_With_Parent;
      Typ            : Glib.GType := Gtk.Dialog.Get_Type;
      Default_Width  : Glib.Gint := -1;
      Default_Length : Glib.Gint := -1)
   is
      Win : constant Gtk_Window := Get_Current_Window (Kernel);
      F   : constant Gtk_Dialog_Flags := Flags
        or Destroy_With_Parent
        or Use_Header_Bar_From_Settings (Win);
   begin
      Self.Kernel := Kernel;

      G_New_Dialog (Self, Flags => F, Typ => Typ);

      Self.Set_Title (Title);
      Self.Set_Transient_For (Win);

      Gtk_New (Self.Label_Size_Group);

      if (F and Gtk.Dialog.Modal) /= 0 then
         Self.Set_Modal (True);
      end if;

      if (F and Gtk.Dialog.Destroy_With_Parent) /= 0 then
         Self.Set_Destroy_With_Parent (True);
      end if;

      Self.Set_Position (Win_Pos_Center_On_Parent);

      Self.On_Focus_In_Event (On_GPS_Dialog_Focus_In'Access);

      Set_Default_Size_From_History
        (Win    => Self,
         Name   => Title,
         Kernel => Kernel,
         Width  => Default_Width,
         Height => Default_Length);
   end Initialize;

   ----------------------
   -- Add_Check_Button --
   ----------------------

   function Add_Check_Button
     (Self    : not null access GPS_Dialog_Record'Class;
      Message : String;
      Key     : Histories.History_Key) return Gtk_Check_Button
   is
      Check : Gtk_Check_Button;
   begin
      Gtk_New (Check, Message);
      Self.Get_Content_Area.Pack_Start (Check, Expand => False);
      Associate (Self.Kernel.Get_History.all, Key, Check);
      return Check;
   end Add_Check_Button;

   ------------------------------
   -- Add_File_Selection_Entry --
   ------------------------------

   function Add_File_Selection_Entry
     (Self              : not null access GPS_Dialog_Record'Class;
      Message           : String;
      Key               : Histories.History_Key;
      File_Pattern      : String := "";
      Pattern_Name      : String := "";
      Default_Name      : String := "";
      Use_Native_Dialog : Boolean := False;
      Kind              : File_Selector_Kind := Unspecified)
      return Gtk.GEntry.Gtk_Entry
   is
      Box                  : Gtk_Box;
      LabelW               : Gtk_Label;
      Ent                  : Gtk_Entry;
      Button               : Browse_Button;
      Default_History_Path : constant VSS.Strings.Virtual_String :=
        Most_Recent (Self.Kernel.Get_History, Key);

   begin
      Gtk_New_Hbox (Box, Homogeneous => False);
      Self.Get_Content_Area.Pack_Start (Box, Expand => False);

      Gtk_New (LabelW, Message);
      LabelW.Set_Halign (Align_Start);
      LabelW.Set_Alignment (0.0, 0.5);
      Box.Pack_Start (LabelW, Expand => False, Padding => 5);
      Self.Label_Size_Group.Add_Widget (LabelW);

      Gtk_New (Ent);
      Box.Pack_Start (Ent, Expand => True, Fill => True);

      if not Default_History_Path.Is_Empty then
         Ent.Set_Text
           (Text =>
              VSS.Strings.Conversions.To_UTF_8_String (Default_History_Path));

      else
         Ent.Set_Text
           (Create_From_Base
              (Base_Name => +Default_Name).Display_Full_Name);
      end if;

      Button := new Browse_Button_Record;
      Initialize (Button, "Browse");
      Button.Kernel := Self.Kernel;
      Button.Ent := Ent;
      Button.Use_Native_Dialog := Use_Native_Dialog;
      Button.Key := To_Unbounded_String (String (Key));
      Button.File_Pattern := To_Unbounded_String (File_Pattern);
      Button.Pattern_Name := To_Unbounded_String (Pattern_Name);
      Button.Default_Name := To_Unbounded_String (Default_Name);
      Button.Kind := Kind;
      Box.Pack_Start (Button, Expand => False);

      Button.On_Clicked (On_Browse_Button_Clicked'Access);

      return Ent;
   end Add_File_Selection_Entry;

   -------------------------------
   -- Display_Text_Input_Dialog --
   -------------------------------

   function Display_Text_Input_Dialog
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Title          : String;
      Message        : String;
      Key            : History_Key := "";
      Check_Msg      : String := "";
      Button_Active  : access Boolean := null;
      Key_Check      : Histories.History_Key := "";
      Check_Msg2     : String := "";
      Button2_Active : access Boolean := null;
      Key_Check2     : Histories.History_Key := "") return String
   is
      Dialog  : GPS_Dialog;
      Check, Check2 : Gtk_Check_Button;
      Combo : Combo_Box;
   begin
      Gtk_New
        (Dialog,
         Title  => Title,
         Kernel => Kernel,
         Flags  => Destroy_With_Parent or Modal);

      Dialog.Add_OK_Cancel;
      Combo := Dialog.Add_Combo (Message, Key);

      if Check_Msg /= "" then
         Check := Dialog.Add_Check_Button (Check_Msg, Key_Check);
      end if;

      if Check_Msg2 /= "" then
         Check2 := Dialog.Add_Check_Button (Check_Msg2, Key_Check2);
      end if;

      Dialog.Show_All;

      if Dialog.Run = Gtk_Response_OK then
         if Button_Active /= null then
            Button_Active.all := Check.Get_Active;
         end if;

         if Button2_Active /= null then
            Button2_Active.all := Check2.Get_Active;
         end if;

         return S : constant String := Combo.Get_Text do
            Dialog.Destroy;
         end return;
      end if;

      Dialog.Destroy;
      return (1 => ASCII.NUL);
   end Display_Text_Input_Dialog;

   ----------------------------
   --  Display_Select_Dialog --
   ----------------------------

   function Display_Select_Dialog
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Title   : VSS.Strings.Virtual_String;
      Message : VSS.Strings.Virtual_String;
      Value   : in out Enumerated_Type) return Boolean
   is
      Dialog  : GPS_Dialog;
      Combo   : Gtk_Combo_Box_Text;
      Box     : Gtk_Box;
      Label   : Gtk_Label;

   begin
      Gtk_New
        (Dialog,
         Title  => VSS.Strings.Conversions.To_UTF_8_String (Title),
         Kernel => Kernel,
         Flags  => Destroy_With_Parent or Modal);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Dialog.Get_Content_Area.Pack_Start (Box, Expand => False);

      Gtk_New (Label, VSS.Strings.Conversions.To_UTF_8_String (Message));
      Label.Set_Halign (Align_Start);
      Box.Pack_Start (Label, Expand => False, Padding => 5);

      Gtk_New (Combo);
      for Item in Enumerated_Type'Range loop
         Combo.Append_Text (Item'Image);
      end loop;
      Combo.Set_Active (Enumerated_Type'Pos (Value));

      Box.Pack_Start (Combo);

      Dialog.Add_OK_Cancel;

      Dialog.Show_All;

      if Dialog.Run = Gtk_Response_OK then
         Value := Enumerated_Type'Val (Combo.Get_Active);
         Dialog.Destroy;
         return True;
      end if;

      Dialog.Destroy;
      return False;
   end Display_Select_Dialog;

   ----------------------
   -- On_Destroy_Combo --
   ----------------------

   procedure On_Destroy_Combo (Self : access Gtk_Widget_Record'Class) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (History_Key, History_Key_Access);
      C : constant Combo_Box := Combo_Box (Self);
   begin
      Unchecked_Free (C.Key);
   end On_Destroy_Combo;

   -------------------
   -- Add_OK_Cancel --
   -------------------

   procedure Add_OK_Cancel (Self : not null access GPS_Dialog_Record'Class) is
      Dummy  : Gtk_Widget;
   begin
      Self.Add_Button (Stock_Ok, Gtk_Response_OK).Grab_Default;
      Dummy := Self.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Self.Set_Default_Response (Gtk_Response_OK);
   end Add_OK_Cancel;

   ----------------
   -- Add_Button --
   ----------------

   procedure Add_Button
     (Self       : not null access GPS_Dialog_Record'Class;
      Text       : String;
      Response   : Gtk_Response_Type;
      Is_Default : Boolean := False)
   is
      Dummy : Gtk_Widget;
   begin
      Dummy := Self.Add_Button (Text, Response);
      if Is_Default then
         Self.Set_Default_Response (Response);
      end if;
   end Add_Button;

   ---------------
   -- Add_Label --
   ---------------

   procedure Add_Label
     (Self    : not null access GPS_Dialog_Record'Class;
      Message : String)
   is
      Label : Gtk_Label;
   begin
      Gtk_New (Label, Message);
      Self.Label_Size_Group.Add_Widget (Label);
      Label.Set_Halign (Align_Start);
      Self.Get_Content_Area.Pack_Start (Label, Expand => False);
   end Add_Label;

   ---------------
   -- Add_Combo --
   ---------------

   function Add_Combo
     (Self    : not null access GPS_Dialog_Record'Class;
      Message : String;
      Key     : Histories.History_Key;
      Tooltip : String := "") return Combo_Box
   is
      Result : Combo_Box;
      Box    : Gtk_Box;
      LabelW : Gtk_Label;
   begin
      Gtk_New_Hbox (Box, Homogeneous => False);
      Self.Get_Content_Area.Pack_Start (Box, Expand => False);

      Gtk_New (LabelW, Message);
      LabelW.Set_Halign (Align_Start);
      LabelW.Set_Alignment (0.0, 0.5);
      Box.Pack_Start (LabelW, Expand => False, Padding => 5);
      Self.Label_Size_Group.Add_Widget (LabelW);

      Result := new Combo_Box_Record;
      Result.Key := new History_Key'(Key);
      Result.Kernel := Self.Kernel;
      Initialize_With_Entry (Result);
      Box.Pack_Start (Result);
      Gtk_Entry (Result.Get_Child).On_Activate
        (On_Enter_Key_Press'Access, Self);
      Result.On_Destroy (On_Destroy_Combo'Access);

      if Tooltip /= "" then
         Result.Set_Tooltip_Text (Tooltip);
      end if;

      Allow_Duplicates
        (Hist  => Self.Kernel.Get_History.all,
         Key   => Key,
         Allow => False);
      Get_History (Self.Kernel.Get_History.all, Key, Result);

      return Result;
   end Add_Combo;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Self : not null access Combo_Box_Record) return String
   is
      S : constant String := Self.Get_Active_Text;
   begin
      Add_To_History
        (Self.Kernel.Get_History.all,
         Self.Key.all,
         VSS.Strings.Conversions.To_Virtual_String (S));

      return S;
   end Get_Text;

   ----------------
   -- Add_Choice --
   ----------------

   procedure Add_Choice
     (Self : not null access Combo_Box_Record;
      Choice : String) is
   begin
      Self.Append_Text (Choice);
   end Add_Choice;

   ------------------------
   -- On_Enter_Key_Press --
   ------------------------

   procedure On_Enter_Key_Press (Dialog : access GObject_Record'Class) is
   begin
      Gtk_Dialog (Dialog).Response (Gtk_Response_OK);
   end On_Enter_Key_Press;

end GPS.Dialogs;
