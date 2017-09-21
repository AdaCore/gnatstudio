------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
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

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Gtk.Adjustment;         use Gtk.Adjustment;
with Gtk.Box;                use Gtk.Box;
with Gtk.Button;             use Gtk.Button;
with Gtk.Check_Button;       use Gtk.Check_Button;
with Gtkada.Check_Button;    use Gtkada.Check_Button;
with Gtk.Container;          use Gtk.Container;
with Gtk.Combo_Box;
with Gtk.Combo_Box_Text;     use Gtk.Combo_Box_Text;
with Gtk.Dialog;             use Gtk.Dialog;
with Gtk.Editable;           use Gtk.Editable;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Frame;              use Gtk.Frame;
with Gtk.Handlers;           use Gtk.Handlers;
with Gtk.Label;              use Gtk.Label;
with Gtk.Radio_Button;       use Gtk.Radio_Button;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Size_Group;         use Gtk.Size_Group;
with Gtk.Spin_Button;        use Gtk.Spin_Button;
with Gtk.Style_Context;      use Gtk.Style_Context;
with Gtk.Table;              use Gtk.Table;
with Gtk.Toggle_Button;      use Gtk.Toggle_Button;
with Gtk.Widget;             use Gtk.Widget;
with Gtk.Window;             use Gtk.Window;
with Gtkada.File_Selector;   use Gtkada.File_Selector;
with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtkada.Intl;            use Gtkada.Intl;
with Gtk.Text_View;          use Gtk.Text_View;
with Gtk.Text_Buffer;        use Gtk.Text_Buffer;

with GUI_Utils;              use GUI_Utils;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

package body Switches_Chooser.Gtkada is

   use Switch_Description_Vectors, Combo_Switch_Vectors;
   use Frame_Description_Vectors;

   type Switch_Data is record
      Editor : Switches_Editor;
      Switch : Switch_Description_Vectors.Cursor;
   end record;
   package User_Widget_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Switch_Data);

   type Popup_Button_Record is new Gtk_Button_Record with record
      Switch : Switch_Description_Vectors.Extended_Index;
   end record;
   type Popup_Button is access all Popup_Button_Record'Class;

   procedure On_Toggle_Check
     (Toggle : access Gtk_Widget_Record'Class;
      Data   : Switch_Data);
   procedure On_Toggle_Radio
     (Toggle : access Gtk_Widget_Record'Class;
      Data   : Switch_Data);
   procedure On_Field_Changed
     (Field  : access Gtk_Widget_Record'Class;
      Data   : Switch_Data);
   procedure On_Combo_Changed
     (Combo  : access Gtk_Widget_Record'Class;
      Data   : Switch_Data);
   procedure On_Spin_Changed
     (Spin   : access Gtk_Widget_Record'Class;
      Data   : Switch_Data);
   procedure On_Popup_Button_Clicked
     (Pop    : access Gtk_Widget_Record'Class;
      Data   : Switch_Data);
   procedure On_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Switch_Data);
   procedure On_Command_Line_Changed
     (Editor : access Gtk_Widget_Record'Class);
   --  Called when some of the widgets change

   procedure Destroy_Dialog (Dialog : access Gtk_Widget_Record'Class);
   --  Called to destroy a popup dialog

   procedure On_Dialog_Destroy (Pop : access Gtk_Widget_Record'Class);
   --  Called when a popup dialog is destroyed

   procedure Browse_Directory
     (Field  : access Gtk_Widget_Record'Class;
      Data   : Switch_Data);
   procedure Browse_File
     (Field : access Gtk_Widget_Record'Class;
      Data   : Switch_Data);
   --  Open a dialog to select a directory or a file

   procedure Create_Box_For_Popup
     (Editor         : access Switches_Editor_Record'Class;
      Popup          : Popup_Index;
      Table          : access Gtk_Table_Record'Class;
      Lines, Columns : Positive);
   --  Create, inside Table, the frames that contain the switches associated
   --  with the given popup (or main window).

   procedure Create_Widget
     (Editor    : access Switches_Editor_Record'Class;
      Switch    : Switch_Description_Vectors.Cursor;
      Size      : Gtk_Size_Group;
      Box       : Gtk_Box);
   --  Create and register the widget matching S

   procedure Set_Tooltip
     (Editor   : access Switches_Editor_Record'Class;
      W        : access Gtk_Widget_Record'Class;
      Switch   : Switch_Description_Vectors.Cursor;
      S        : Switch_Description);
   --  Set the tooltip on W

   --------------------------------
   -- Set_Graphical_Command_Line --
   --------------------------------

   overriding procedure Set_Graphical_Command_Line
     (Editor    : in out Switches_Editor_Record;
      Cmd_Line  : String) is
   begin
      Set_Text (Editor.Ent, Cmd_Line);
   end Set_Graphical_Command_Line;

   ---------------------
   -- On_Toggle_Check --
   ---------------------

   procedure On_Toggle_Check
     (Toggle : access Gtk_Widget_Record'Class;
      Data   : Switch_Data)
   is
      State : constant State_Type := Get_State (Gtkada_Check_Button (Toggle));
   begin
      case State is
         when State_Checked =>
            Change_Switch
              (Data.Editor.all, Toggle,
               Parameter => "Checked");
         when State_Unchecked =>
            Change_Switch
              (Data.Editor.all, Toggle,
               Parameter => "Unchecked");
         when State_Checked_Default =>
            Change_Switch
              (Data.Editor.all, Toggle,
               Parameter => "Checked_Default");
      end case;
   end On_Toggle_Check;

   ---------------------
   -- On_Toggle_Radio --
   ---------------------

   procedure On_Toggle_Radio
     (Toggle : access Gtk_Widget_Record'Class;
      Data   : Switch_Data)
   is
   begin
      Change_Switch
        (Data.Editor.all, Toggle,
         Parameter => Boolean'Image (Get_Active (Gtk_Check_Button (Toggle))));
   end On_Toggle_Radio;

   --------------------
   -- Destroy_Dialog --
   --------------------

   procedure Destroy_Dialog (Dialog : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Dialog);
   end Destroy_Dialog;

   -----------------------
   -- On_Dialog_Destroy --
   -----------------------

   procedure On_Dialog_Destroy
     (Pop    : access Gtk_Widget_Record'Class)
   is
   begin
      Set_Sensitive (Pop, True);
   end On_Dialog_Destroy;

   -----------------------------
   -- On_Popup_Button_Clicked --
   -----------------------------

   procedure On_Popup_Button_Clicked
     (Pop    : access Gtk_Widget_Record'Class;
      Data   : Switch_Data)
   is
      Dialog   : Gtk_Dialog;
      Scrolled : Gtk_Scrolled_Window;
      Table    : Gtk_Table;
      Config   : constant Switches_Editor_Config := Get_Config (Data.Editor);
      S        : constant Switch_Description :=
        Element (Config.Switches, Popup_Button (Pop).Switch);
      Tmp      : Gtk_Widget;
      Flags    : Gtk_Dialog_Flags := 0;
   begin
      --  If the parent window is modal, we need to make the popup modal as
      --  well, since otherwise the user will not be able to click on any of
      --  its children.
      if Get_Modal (Gtk_Window (Get_Toplevel (Data.Editor))) then
         Flags := Modal;
      end if;

      Gtk_New (Dialog,
               Title  => To_String (S.Label),
               Parent => Gtk_Window (Get_Toplevel (Data.Editor)),
               Flags  => Flags);
      Set_Sensitive (Pop, False);
      Dialog.Set_Default_Size (600, 400);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Never, Policy_Automatic);
      Pack_Start (Get_Content_Area (Dialog), Scrolled);

      Gtk_New
        (Table,
         Rows        => Guint (S.Lines),
         Columns     => Guint (S.Columns),
         Homogeneous => False);
      Scrolled.Add (Table);
      Create_Box_For_Popup
        (Editor    => Data.Editor,
         Popup     => S.To_Popup,
         Table     => Table,
         Lines     => S.Lines,
         Columns   => S.Columns);
      Gtk_Switches_Editors.On_Command_Line_Changed (Data.Editor.all);

      Tmp := Add_Button (Dialog, "OK", Gtk_Response_OK);
      Show_All (Dialog);

      Widget_Callback.Object_Connect
        (Tmp, Gtk.Button.Signal_Clicked,
         Destroy_Dialog'Access, Dialog);
      Widget_Callback.Object_Connect
        (Dialog, Gtk.Widget.Signal_Destroy,
         On_Dialog_Destroy'Access, Pop);
   end On_Popup_Button_Clicked;

   ----------------------
   -- On_Field_Changed --
   ----------------------

   procedure On_Field_Changed
     (Field  : access Gtk_Widget_Record'Class;
      Data   : Switch_Data) is
   begin
      Change_Switch (Data.Editor.all, Field, Get_Text (Gtk_Entry (Field)));
   end On_Field_Changed;

   ----------------------
   -- On_Combo_Changed --
   ----------------------

   procedure On_Combo_Changed
     (Combo  : access Gtk_Widget_Record'Class;
      Data   : Switch_Data) is
   begin
      Change_Switch
        (Data.Editor.all, Combo, Get_Active_Text (Gtk_Combo_Box_Text (Combo)));
   end On_Combo_Changed;

   ---------------------
   -- On_Spin_Changed --
   ---------------------

   procedure On_Spin_Changed
     (Spin   : access Gtk_Widget_Record'Class;
      Data   : Switch_Data)
   is
      V : constant String :=
        Gint'Image (Get_Value_As_Int (Gtk_Spin_Button (Spin)));
   begin
      if V (V'First) = ' ' then
         Change_Switch (Data.Editor.all, Spin, V (V'First + 1 .. V'Last));
      else
         Change_Switch (Data.Editor.all, Spin, V);
      end if;
   end On_Spin_Changed;

   -----------------------------
   -- On_Command_Line_Changed --
   -----------------------------

   procedure On_Command_Line_Changed
     (Editor : access Gtk_Widget_Record'Class)
   is
   begin
      On_Command_Line_Changed
        (Switches_Editor (Editor).all,
         Get_Text (Switches_Editor (Editor).Ent));
   end On_Command_Line_Changed;

   --------------------------
   -- Set_Graphical_Widget --
   --------------------------

   overriding procedure Set_Graphical_Widget
     (Editor     : in out Switches_Editor_Record;
      Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Switch     : Switch_Type;
      Parameter  : String;
      Is_Default : Boolean := False)
   is
      pragma Unreferenced (Editor);
   begin
      case Switch is
         when Switch_Check =>
            if Is_Default then
               Set_Default
                 (Gtkada_Check_Button (Widget),
                  Boolean'Value (Parameter));
            else
               Set_Active
                 (Gtkada_Check_Button (Widget),
                  Boolean'Value (Parameter));
            end if;

         when Switch_Radio =>
            Set_Active (Gtk_Check_Button (Widget), Boolean'Value (Parameter));

         when Switch_Field =>
            Set_Text (Gtk_Entry (Widget), Parameter);

         when Switch_Spin =>
            Set_Value (Gtk_Spin_Button (Widget), Gdouble'Value (Parameter));

         when Switch_Combo =>
            Set_Active_Text (Gtk_Combo_Box_Text (Widget), Parameter);

         when Switch_Popup =>
            null;
      end case;

   exception
      when others =>
         null;
   end Set_Graphical_Widget;

   ----------------------
   -- Browse_Directory --
   ----------------------

   procedure Browse_Directory
     (Field  : access Gtk_Widget_Record'Class;
      Data   : Switch_Data)
   is
      F   : constant Gtk_Entry := Gtk_Entry (Field);
      Dir : constant Virtual_File := Select_Directory
        (Base_Directory    => Create_From_UTF8 (Get_Text (F)),
         Parent            => Gtk_Window (Get_Toplevel (F)),
         Use_Native_Dialog => Data.Editor.Native_Dialogs);
   begin
      if Dir /= GNATCOLL.VFS.No_File then
         Set_Text (F, Display_Full_Name (Dir));
      end if;
   end Browse_Directory;

   -----------------
   -- Browse_File --
   -----------------

   procedure Browse_File
     (Field  : access Gtk_Widget_Record'Class;
      Data   : Switch_Data)
   is
      F    : constant Gtk_Entry := Gtk_Entry (Field);
      VF   : constant Virtual_File := Create_From_UTF8 (Get_Text (F));
      File : constant Virtual_File := Select_File
        (Base_Directory    => Dir (VF),
         Default_Name      => Base_Name (VF),
         Parent            => Gtk_Window (Get_Toplevel (F)),
         Kind              => Open_File,
         File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
         Pattern_Name      => -"All files;Ada files;C/C++ files",
         Use_Native_Dialog => Data.Editor.Native_Dialogs);
   begin
      if File /= GNATCOLL.VFS.No_File then
         Set_Text (F, Display_Full_Name (File));
      end if;
   end Browse_File;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Switch_Data)
   is
      pragma Unreferenced (Widget);
   begin
      Set_Widget (Data.Editor.all, To_Index (Data.Switch), null);
   end On_Destroy;

   -----------------
   -- Set_Tooltip --
   -----------------

   procedure Set_Tooltip
     (Editor   : access Switches_Editor_Record'Class;
      W        : access Gtk_Widget_Record'Class;
      Switch   : Switch_Description_Vectors.Cursor;
      S        : Switch_Description)
   is
   begin
      Set_Widget (Editor.all, To_Index (Switch), Gtk_Widget (W));
      User_Widget_Callback.Connect
        (W, Gtk.Widget.Signal_Destroy, On_Destroy'Access,
         (Switches_Editor (Editor), Switch));
      if S.Tip /= "" then
         Set_Tooltip_Text
           (W,
            '(' & To_String (S.Switch) & ") " & ASCII.LF
            & To_String (S.Tip));
      else
         Set_Tooltip_Text (W, '(' & To_String (S.Switch) & ") ");
      end if;
   end Set_Tooltip;

   -------------------
   -- Create_Widget --
   -------------------

   procedure Create_Widget
     (Editor    : access Switches_Editor_Record'Class;
      Switch    : Switch_Description_Vectors.Cursor;
      Size      : Gtk_Size_Group;
      Box       : Gtk_Box)
   is
      S : constant Switch_Description := Element (Switch);
      Check    : Gtkada_Check_Button;
      Field    : Gtk_Entry;
      Label    : Gtk_Label;
      Spin     : Gtk_Spin_Button;
      Adj      : Gtk_Adjustment;
      Radio    : Gtk_Radio_Button;
      Hbox     : Gtk_Box;
      Button   : Gtk_Button;
      Combo    : Gtk_Combo_Box_Text;
      Combo_Iter : Combo_Switch_Vectors.Cursor;
      Switch2  : Switch_Description_Vectors.Cursor;
      Pop        : Popup_Button;
      Frame      : Gtk_Frame;

   begin
      if S.Typ /= Switch_Check
        and then S.Typ /= Switch_Radio
        and then S.Typ /= Switch_Popup
      then
         Gtk_New_Hbox  (Hbox, False, Spacing => 3);
         Pack_Start    (Box, Hbox, Expand => False, Padding => 0);

         if S.Label /= "" then
            Gtk_New       (Label, To_String (S.Label));
            Pack_Start    (Hbox, Label, Expand => False, Padding => 0);
            Set_Alignment (Label, 0.0, 0.5);
            Add_Widget    (Size, Label);
         end if;
      end if;

      case S.Typ is
         when Switch_Check =>
            Gtk_New    (Check, To_String (S.Label), S.Default_State);
            Check.Set_Sensitive (not Editor.Read_Only and then S.Active);
            Pack_Start (Box, Check, Expand => False, Padding => 0);
            Set_Tooltip (Editor, Check, Switch, S);
            User_Widget_Callback.Connect
              (Check, Gtk.Toggle_Button.Signal_Toggled,
               On_Toggle_Check'Access,
               (Switches_Editor (Editor), Switch));

         when Switch_Field =>
            Gtk_New (Field);
            Field.Set_Sensitive (not Editor.Read_Only and then S.Active);
            Set_Tooltip (Editor, Field, Switch, S);
            Pack_Start (Hbox, Field, True, True, 0);
            User_Widget_Callback.Connect
              (Field, Gtk.Editable.Signal_Changed,
               On_Field_Changed'Access,
               (Switches_Editor (Editor), Switch));

            if S.As_File then
               Gtk_New (Button, -"Browse");
               Button.Set_Sensitive (not Editor.Read_Only and then S.Active);
               Pack_Start (Hbox, Button, Expand => False, Padding => 0);
               User_Widget_Callback.Object_Connect
                 (Button, Signal_Clicked, Browse_File'Access,
                  Slot_Object => Field, User_Data =>
                    (Switches_Editor (Editor), Switch));

            elsif S.As_Directory then
               Gtk_New (Button, -"Browse");
               Button.Set_Sensitive (not Editor.Read_Only and then S.Active);
               Pack_Start (Hbox, Button, Expand => False, Padding => 0);
               User_Widget_Callback.Object_Connect
                 (Button, Signal_Clicked,
                  Browse_Directory'Access,
                  Slot_Object => Field, User_Data =>
                    (Switches_Editor (Editor), Switch));
            end if;

         when Switch_Spin =>
            Gtk_New (Adj, Gdouble (S.Default),
                     Gdouble (S.Min), Gdouble (S.Max),
                     1.0, 10.0);
            Gtk_New (Spin, Adj, 1.0, 0);
            Spin.Set_Sensitive (not Editor.Read_Only and then S.Active);
            Set_Tooltip (Editor, Spin, Switch, S);
            Pack_Start (Hbox, Spin, True, True, 0);

            User_Widget_Callback.Connect
              (Spin, Gtk.Spin_Button.Signal_Value_Changed,
               On_Spin_Changed'Access,
               (Switches_Editor (Editor), Switch));

         when Switch_Radio =>
            if not S.Is_Entry then
               --  Create the frame containing all the radio entries for this
               --  radio group.
               Gtk_New (Frame);
               Box.Pack_Start (Frame, Expand => False, Padding => 5);

               Gtk_New_Hbox (Hbox, Homogeneous => False);
               Hbox.Set_Spacing (12);
               Frame.Add (Hbox);

               if S.Label /= Null_Unbounded_String then
                  Frame.Set_Label (To_String (S.Label));
               end if;

               if S.Tip /= Null_Unbounded_String then
                  Frame.Set_Tooltip_Text (To_String (S.Tip));
               end if;

               --  Find all buttons in that group and add them in the same row
               Switch2 := Next (Switch);
               while Has_Element (Switch2) loop
                  declare
                     S2 : constant Switch_Description := Element (Switch2);
                  begin
                     if S2.Typ = Switch_Radio
                       and then S2.Group = S.Group
                     then
                        Gtk_New
                          (Radio, Group => Radio,
                           Label => To_String (S2.Label));
                        Radio.Set_Sensitive
                          (not Editor.Read_Only and then S.Active);
                        Pack_Start (Hbox, Radio, Expand => False);
                        Set_Tooltip (Editor, Radio, Switch2, S2);
                        User_Widget_Callback.Connect
                          (Radio, Gtk.Toggle_Button.Signal_Toggled,
                           On_Toggle_Radio'Access,
                           (Switches_Editor (Editor), Switch));
                     end if;
                  end;

                  Next (Switch2);
               end loop;
            end if;

         when Switch_Combo =>
            Gtk_New (Combo);
            Combo.Set_Sensitive (not Editor.Read_Only and then S.Active);
            Set_Tooltip (Editor, Combo, Switch, S);
            Pack_Start (Hbox, Combo, True, True, Padding => 0);

            Combo_Iter := First (S.Entries);
            while Has_Element (Combo_Iter) loop
               Combo.Append_Text (To_String (Element (Combo_Iter).Label));
               Next (Combo_Iter);
            end loop;

            User_Widget_Callback.Object_Connect
              (Combo, Gtk.Combo_Box.Signal_Changed,
               On_Combo_Changed'Access, Combo,
               (Switches_Editor (Editor), Switch));

         when Switch_Popup =>
            Pop := new Popup_Button_Record'
              (Gtk_Button_Record with
               Switch => To_Index (Switch));

            Gtk_New_Hbox  (Hbox, False, Spacing => 3);
            Gtk_New       (Label, To_String (S.Label) & ": ");
            Pack_Start    (Hbox, Label,
                           Expand => True, Fill => True, Padding => 0);
            Set_Alignment (Label, 0.0, 0.5);

            Gtk_New       (Label, "...");
            Set_Alignment (Label, 1.0, 0.5);
            Pack_End      (Hbox, Label, Expand => True, Fill => True);

            Gtk.Button.Initialize (Pop, "");
            Add (Pop, Hbox);
            Pack_Start (Box, Pop, False, True, 0);
            User_Widget_Callback.Connect
              (Pop, Gtk.Button.Signal_Clicked,
               On_Popup_Button_Clicked'Access,
               (Switches_Editor (Editor), Switch));
      end case;
   end Create_Widget;

   --------------------------
   -- Create_Box_For_Popup --
   --------------------------

   procedure Create_Box_For_Popup
     (Editor             : access Switches_Editor_Record'Class;
      Popup              : Popup_Index;
      Table              : access Gtk_Table_Record'Class;
      Lines, Columns     : Positive)
   is
      Config   : constant Switches_Editor_Config := Get_Config (Editor);
      Sizes    : array (1 .. Lines, 1 .. Columns) of Gtk_Size_Group;
      F        : Gtk_Frame;
      Scrolled : Gtk_Scrolled_Window;
      Switch   : Switch_Description_Vectors.Cursor;
      Boxes    : array (1 .. Lines, 1 .. Columns) of Gtk_Box;
      Frame_C  : Frame_Description_Vectors.Cursor;
      Frame    : Frame_Description;
      Subtable : Gtk_Table;
      Col_Span, Line_Span : Positive;
      Col, Line           : Positive;
      Label    : Gtk_Label;

      procedure Add_To_Frame (Widget : access Gtk_Container_Record'Class);
      --  Add Widget to F, with some padding

      ------------------
      -- Add_To_Frame --
      ------------------

      procedure Add_To_Frame (Widget : access Gtk_Container_Record'Class) is
         HBox : Gtk_Box;
      begin
         Gtk_New_Hbox (HBox);
         Set_Border_Width (Widget, 3);
         Pack_Start (HBox, Widget, True, True, Padding => 7);
         Add (F, HBox);
      end Add_To_Frame;

   begin
      for L in 1 .. Lines loop
         for C in 1 .. Columns loop
            Switch := First (Config.Switches);

            while Has_Element (Switch) loop
               declare
                  S : constant Switch_Description := Element (Switch);
               begin
                  --  Radio buttons are made of radio entries, which should not
                  --  be displayed explicitely (they will be displayed as part
                  --  of the radio button itself)
                  if S.Popup = Popup
                    and then S.Line = L
                    and then S.Column = C
                  then
                     if Boxes (L, C) = null then
                        Gtk_New (F);
                        Set_Border_Width (F, 3);
                        Set_Shadow_Type (F, Shadow_None);
                        Col_Span := 1;
                        Line_Span := 1;
                        Col := C;
                        Line := L;

                        Frame_C := First (Config.Frames);
                        while Has_Element (Frame_C) loop
                           Frame := Element (Frame_C);
                           if Frame.Popup = Popup
                             and then Frame.Line <= L
                             and then Frame.Line + Frame.Line_Span - 1 >= L
                             and then Frame.Column <= C
                             and then Frame.Column + Frame.Col_Span - 1 >= C
                           then
                              Gtk_New (Label);
                              Set_Use_Markup (Label, True);
                              Set_Markup
                                (Label,
                                 "<b>" & To_String (Frame.Title)
                                 & "</b>");
                              Set_Label_Widget (F, Label);
                              Set_Label_Align (F, 0.0, 0.0);
                              Col_Span  := Frame.Col_Span;
                              Col       := Frame.Column;
                              Line_Span := Frame.Line_Span;
                              Line      := Frame.Line;
                              exit;
                           end if;
                           Next (Frame_C);
                        end loop;

                        Attach
                          (Table, F,
                           Guint (Col - 1),
                           Guint (Col - 1 + Col_Span),
                           Guint (Line - 1),
                           Guint (Line - 1 + Line_Span),
                           Yoptions => Expand or Fill);

                        if Config.Scrolled_Window then
                           Gtk_New (Scrolled);
                           Set_Policy
                             (Scrolled, Policy_Automatic, Policy_Automatic);
                           Set_Shadow_Type (Scrolled, Shadow_None);
                           Add_To_Frame (Scrolled);
                        end if;

                        if Col_Span > 1 or else Line_Span > 1 then
                           Gtk.Table.Gtk_New
                             (Subtable,
                              Guint (Line_Span),
                              Guint (Col_Span),
                              False);

                           for Sub_Col in 1 .. Col_Span loop
                              for Sub_Line in 1 .. Line_Span loop
                                 Gtk_New_Vbox
                                   (Boxes
                                      (Line + Sub_Line - 1,
                                       Col + Sub_Col - 1), False, 0);
                                 Gtk_New
                                   (Sizes
                                      (Line + Sub_Line - 1,
                                       Col + Sub_Col - 1));
                                 Attach
                                   (Subtable,
                                    Boxes
                                      (Line + Sub_Line - 1,
                                       Col + Sub_Col - 1),
                                    Guint (Sub_Col - 1),
                                    Guint (Sub_Col),
                                    Guint (Sub_Line - 1),
                                    Guint (Sub_Line));
                              end loop;
                           end loop;

                           if Scrolled /= null then
                              Add_With_Viewport (Scrolled, Subtable);
                           else
                              Add_To_Frame (Subtable);
                           end if;

                        else
                           Gtk_New_Vbox (Boxes (L, C), False, 0);
                           Gtk_New (Sizes (L, C));
                           if Scrolled /= null then
                              Add_With_Viewport (Scrolled, Boxes (L, C));
                           else
                              Add_To_Frame (Boxes (L, C));
                           end if;

                        end if;
                     end if;

                     Create_Widget
                       (Editor   => Editor,
                        Switch   => Switch,
                        Size     => Sizes (L, C),
                        Box      => Boxes (L, C));
                  end if;
               end;
               Next (Switch);
            end loop;
         end loop;
      end loop;
   end Create_Box_For_Popup;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor             : out Switches_Editor;
      Config             : Switches_Editor_Config;
      Use_Native_Dialogs : Boolean;
      Read_Only          : Boolean;
      History            : Histories.History;
      Key                : History_Key;
      Cmd_Line_Tooltip   : String;
      Help_Msg           : String := "";
      Fixed_Font         : Pango_Font_Description := null) is
   begin
      Editor := new Switches_Editor_Record;
      Initialize
        (Editor             => Editor,
         Config             => Config,
         Use_Native_Dialogs => Use_Native_Dialogs,
         Read_Only          => Read_Only,
         History            => History,
         Key                => Key,
         Cmd_Line_Tooltip   => Cmd_Line_Tooltip,
         Help_Msg           => Help_Msg,
         Fixed_Font         => Fixed_Font);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor             : access Switches_Editor_Record'Class;
      Config             : Switches_Editor_Config;
      Use_Native_Dialogs : Boolean;
      Read_Only          : Boolean;
      History            : Histories.History;
      Key                : History_Key;
      Cmd_Line_Tooltip   : String;
      Help_Msg           : String := "";
      Fixed_Font         : Pango_Font_Description := null)
   is
      Combo                   : Gtk_Combo_Box_Text;
      Widget_For_Command_Line : Gtk_Widget;
      Scroll                  : Gtk_Scrolled_Window;
      Table                   : Gtk_Table;
      Help_Scroll             : Gtk_Scrolled_Window;
      Help_View               : Gtk_Text_View;
   begin
      Editor.Native_Dialogs := Use_Native_Dialogs;
      Editor.Read_Only      := Read_Only;

      Initialize (Editor.all, Config);
      Gtk.Box.Initialize_Vbox (Editor);

      if not Config.Switches.Is_Empty then
         Gtk_New (Scroll);
         Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);

         Gtk_New
           (Table,
            Rows        => Guint (Config.Lines) + 1,
            Columns     => Guint (Config.Columns),
            Homogeneous => False);

         Scroll.Set_Size_Request (-1, 120);
         Scroll.Add (Table);

         Pack_Start (Editor, Scroll, True, True, 2);

         Create_Box_For_Popup
           (Editor    => Editor,
            Popup     => Main_Window,
            Table     => Table,
            Lines     => Config.Lines,
            Columns   => Config.Columns);
      end if;

      --  Show the help if available
      if Help_Msg /= "" then
         Gtk_New (Help_Scroll);
         Gtk_New (Help_View);
         Help_View.Get_Buffer.Set_Text (Help_Msg);
         if Fixed_Font /= null then
            Modify_Font (Help_View, Fixed_Font);
         end if;
         Help_Scroll.Add (Help_View);
         Pack_Start (Editor, Help_Scroll, True, True, 3);
      end if;

      if History = null then
         Gtk_New (Editor.Ent);
         Widget_For_Command_Line := Gtk_Widget (Editor.Ent);
      else
         Gtk_New_With_Entry (Combo);
         Editor.Ent := Gtk_Entry (Get_Child (Combo));
         Widget_For_Command_Line := Gtk_Widget (Combo);
         Get_History (History.all, Key, Combo, False, False);
      end if;

      Editor.Ent.Set_Sensitive (not Read_Only);

      if Config.Show_Command_Line then
         declare
            Hbox : Gtk_Hbox;
         begin
            Gtk_New_Hbox (Hbox);
            Pack_Start (Hbox, Widget_For_Command_Line, True, True, 0);
            Pack_Start (Editor, Hbox, False, False, 2);
         end;
         Set_Tooltip_Text
           (Editor.Ent,
            -Cmd_Line_Tooltip);
         Widget_Callback.Object_Connect
           (Editor.Ent, Gtk.Editable.Signal_Changed,
            Widget_Callback.To_Marshaller (On_Command_Line_Changed'Access),
            Editor);
      end if;

      On_Command_Line_Changed (Editor.all, "");

      Get_Style_Context (Editor).Add_Class ("gps-switches-editor");
   end Initialize;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
     (Editor : access Switches_Editor_Record'Class) return Gtk_Entry is
   begin
      return Editor.Ent;
   end Get_Entry;

end Switches_Chooser.Gtkada;
