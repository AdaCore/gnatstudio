-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2004                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                      use Glib;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.List_Item;             use Gtk.List_Item;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Widget;                use Gtk.Widget;

with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Intl;                use Glide_Intl;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Custom_Module;             use Custom_Module;
with GUI_Utils;                 use GUI_Utils;

with Gtk.List;                  use Gtk.List;
with System;                    use System;
with Ada.Unchecked_Conversion;

package body Custom_Combos is

   Id_Cst            : aliased constant String := "id";
   Entry_Cst         : aliased constant String := "entry";
   Label_Cst         : aliased constant String := "label";
   Choice_Cst        : aliased constant String := "choice";
   On_Select         : aliased constant String := "on_selected";
   On_Changed        : aliased constant String := "on_changed";
   Add_Args : constant Cst_Argument_List :=
     (Choice_Cst'Access, On_Select'Access);
   Create_Args : constant Cst_Argument_List :=
     (Id_Cst'Access, On_Changed'Access);
   Remove_Args : constant Cst_Argument_List :=
     (1 => Choice_Cst'Access);
   Set_Text_Args : constant Cst_Argument_List :=
     (1 => Choice_Cst'Access);
   Simple_Args : constant Cst_Argument_List := (1 => Id_Cst'Access);
   Append_Args : constant Cst_Argument_List :=
     (Entry_Cst'Access, Label_Cst'Access);

   type Custom_Combo_Record is new Gtk_Combo_Record with record
      Id                   : String_Access;
      On_Change_Subprogram : Glide_Kernel.Scripts.Subprogram_Type;
   end record;
   type Custom_Combo is access all Custom_Combo_Record'Class;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Custom_Combo);
   function Convert is new Ada.Unchecked_Conversion
     (Custom_Combo, System.Address);

   -----------------------
   -- Local subprograms --
   -----------------------

   type Item_Callback is record
      Combo       : Custom_Combo;
      On_Selected : Glide_Kernel.Scripts.Subprogram_Type;
   end record;

   package Item_Handlers is new Gtk.Handlers.User_Return_Callback
     (Widget_Type => Gtk_List_Item_Record,
      Return_Type => Boolean,
      User_Type   => Item_Callback);

   function Create_Combo
     (Kernel     : access Kernel_Handle_Record'Class;
      Id         : String;
      On_Changed : Subprogram_Type) return Custom_Combo;
   --  Create a new combo

   procedure Add_Combo_Entry
     (Combo       : access Custom_Combo_Record'Class;
      Label       : String;
      On_Selected : Glide_Kernel.Scripts.Subprogram_Type);
   --  Add a combo entry and a command that should be
   --  executed whenever this entry is selected.
   --  On_Selected is also executed when the entry is selected

   procedure Custom_Entry_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands dealing with ToolbarEntry class

   procedure Custom_Toolbar_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands dealing with Toolbar class

   function Lookup_Custom_Combo
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String) return Custom_Combo;
   --  Return a registered combo with label S. If no combo was found,
   --  return null.

   function Item_Selected
     (Item  : access Gtk_List_Item_Record'Class;
      Data  : Item_Callback) return Boolean;
   --  Called when an item is selected

   procedure Combo_Changed
     (Combo_Box : access Gtk_Widget_Record'Class);
   --  Called whenever the text in GPS_Combo is changed.

   function Get_Combo_Text
     (Combo  : access Custom_Combo_Record'Class) return String;
   --  Return the text currently being displayed in the
   --  combo entry identified by Id.

   procedure Set_Combo_Text
     (Combo  : access Custom_Combo_Record'Class;
      Text   : String);
   --  Set the text in combo identified by Id to Text.

   procedure Clear_Combo
     (Combo  : access Custom_Combo_Record'Class);
   --  Clear the combo from all its items.

   procedure Remove_Combo_Item
     (Combo  : access Custom_Combo_Record'Class;
      Label  : String);
   --  Remove choice identified by Label from combo identified by Id.

   -----------------------
   -- Remove_Combo_Item --
   -----------------------

   procedure Remove_Combo_Item
     (Combo  : access Custom_Combo_Record'Class;
      Label  : String)
   is
      use Gtk.Widget.Widget_List;
      Entries : constant Glist := Get_Children (Get_List (Combo));
      Tmp     : Glist := Entries;
   begin
      while Tmp /= Null_List loop
         if Get_Text (Gtk_Label (Get_Data (Tmp))) = Label then
            Remove (Get_List (Combo), Get_Data (Tmp));
            exit;
         end if;

         Tmp := Next (Tmp);
      end loop;
   end Remove_Combo_Item;

   -----------------
   -- Clear_Combo --
   -----------------

   procedure Clear_Combo (Combo  : access Custom_Combo_Record'Class) is
   begin
      Clear_Items (Get_List (Combo), 0, -1);
   end Clear_Combo;

   -------------------------
   -- Lookup_Custom_Combo --
   -------------------------

   function Lookup_Custom_Combo
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String) return Custom_Combo
   is
      use Gtk.Widget.Widget_List;
      Toolbar  : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Children : Glist := Get_Children (Toolbar);
      Tmp      : Glist := Children;
      Child    : Gtk_Widget;
   begin
      while Tmp /= Null_List loop
         Child := Get_Data (Tmp);
         if Child.all in Custom_Combo_Record'Class
           and then Custom_Combo (Child).Id.all = Id
         then
            Free (Children);
            return Custom_Combo (Child);
         end if;

         Tmp := Next (Tmp);
      end loop;

      Free (Children);
      return null;
   end Lookup_Custom_Combo;

   -------------------
   -- Item_Selected --
   -------------------

   function Item_Selected
     (Item  : access Gtk_List_Item_Record'Class;
      Data  : Item_Callback) return Boolean
   is
      Text : constant String := Get_Text (Gtk_Label (Get_Child (Item)));
   begin
      if Data.On_Selected /= null then
         declare
            D : Callback_Data'Class := Create
              (Get_Script (Data.On_Selected.all),
               Arguments_Count => 2);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Set_Nth_Arg (D, 1, Data.Combo.Id.all);
            Set_Nth_Arg (D, 2, Text);
            Tmp := Execute (Data.On_Selected, D);
            Free (D);
         end;
      end if;

      return False;
   end Item_Selected;

   -------------------
   -- Combo_Changed --
   -------------------

   procedure Combo_Changed
     (Combo_Box : access Gtk_Widget_Record'Class)
   is
      Combo       : constant Custom_Combo := Custom_Combo (Combo_Box);
      Combo_Text  : constant String := Get_Text (Get_Entry (Combo));
   begin
      --  Do not react on empty string.

      if Combo_Text = "" then
         return;
      end if;

      --  Execute the actions to call whenever changing, if any.

      if Combo.On_Change_Subprogram /= null then
         declare
            D : Callback_Data'Class := Create
              (Get_Script (Combo.On_Change_Subprogram.all),
               Arguments_Count => 2);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Set_Nth_Arg (D, 1, Combo.Id.all);
            Set_Nth_Arg (D, 2, Combo_Text);
            Tmp := Execute (Combo.On_Change_Subprogram, D);
            Free (D);
         end;
      end if;
   end Combo_Changed;

   ------------------
   -- Create_Combo --
   ------------------

   function Create_Combo
     (Kernel     : access Kernel_Handle_Record'Class;
      Id         : String;
      On_Changed : Subprogram_Type) return Custom_Combo
   is
      Combo   : Custom_Combo := Lookup_Custom_Combo (Kernel, Id);
   begin
      if Combo /= null then
         Insert
           (Kernel, -"Entry already registered: " & Id,
            Mode => Error);
         return Combo;
      end if;

      Combo := new Custom_Combo_Record;
      Gtk.Combo.Initialize (Combo);
      Set_Editable (Get_Entry (Combo), False);

      Widget_Callback.Object_Connect
        (Get_Entry (Combo), "changed",
         Widget_Callback.To_Marshaller (Combo_Changed'Access),
         Slot_Object => Combo,
         After       => True);

      Combo.Id := new String'(Id);
      Combo.On_Change_Subprogram := On_Changed;
      return Combo;
   end Create_Combo;

   ---------------------
   -- Add_Combo_Entry --
   ---------------------

   procedure Add_Combo_Entry
     (Combo       : access Custom_Combo_Record'Class;
      Label       : String;
      On_Selected : Glide_Kernel.Scripts.Subprogram_Type)
   is
      Item  : constant Gtk_List_Item := Add_Unique_Combo_Entry (Combo, Label);
   begin
      if On_Selected /= null then
         Item_Handlers.Connect
           (Item, "button_release_event",
            Item_Handlers.To_Marshaller (Item_Selected'Access),
            User_Data   => (Combo       => Custom_Combo (Combo),
                            On_Selected => On_Selected),
            After => True);
      end if;
   end Add_Combo_Entry;

   --------------------
   -- Get_Combo_Text --
   --------------------

   function Get_Combo_Text
     (Combo  : access Custom_Combo_Record'Class) return String is
   begin
      return Get_Text (Get_Entry (Combo));
   end Get_Combo_Text;

   --------------------
   -- Set_Combo_Text --
   --------------------

   procedure Set_Combo_Text
     (Combo  : access Custom_Combo_Record'Class;
      Text   : String) is
   begin
      Set_Text (Get_Entry (Combo), Text);
   end Set_Combo_Text;

   ----------------------------
   -- Custom_Toolbar_Handler --
   ----------------------------

   procedure Custom_Toolbar_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel    : constant Kernel_Handle := Custom_Module_ID.Kernel;
      Class     : constant Class_Type := New_Class (Kernel, "Toolbar");
      EntClass  : constant Class_Type := New_Class (Kernel, "ToolbarEntry");
      Inst      : constant Class_Instance := Nth_Arg (Data, 1, Class);
      EntInst   : Class_Instance;
      Combo     : Custom_Combo;
   begin
      if Command = Constructor_Method then
         null;

      elsif Command = "entry" then
         Name_Parameters (Data, Simple_Args);
         Combo := Lookup_Custom_Combo (Kernel, Nth_Arg (Data, 2));
         if Combo = null then
            Set_Error_Msg (Data, -"Entry not found: " & Nth_Arg (Data, 2));
         else
            EntInst := New_Instance (Get_Script (Data), EntClass);
            Set_Data (EntInst, Convert (Combo));
            Set_Return_Value (Data, EntInst);
         end if;

      elsif Command = "append" then
         Name_Parameters (Data, Append_Args);

         declare
            Title : constant String := Nth_Arg (Data, 3, "");
            Label : Gtk_Label;
         begin
            if Title /= "" then
               Gtk_New (Label, Title & " ");
               Append_Widget (Get_Toolbar (Kernel), Label, "", "");
               Show_All (Label);
            end if;

            EntInst := Nth_Arg (Data, 2, EntClass);
            Combo   := Convert (Get_Data (EntInst));
            Append_Widget (Get_Toolbar (Kernel), Combo, Tooltip_Text => Title);
            Free (EntInst);
            Show_All (Combo);
         end;
      end if;

      Free (Inst);
   end Custom_Toolbar_Handler;

   --------------------------
   -- Custom_Entry_Handler --
   --------------------------

   procedure Custom_Entry_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel    : constant Kernel_Handle := Custom_Module_ID.Kernel;
      Class     : constant Class_Type := New_Class (Kernel, "ToolbarEntry");
      Inst      : constant Class_Instance := Nth_Arg (Data, 1, Class);
      Combo     : Custom_Combo;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Create_Args);
         Combo := Create_Combo
           (Kernel, Id => Nth_Arg (Data, 2),
            On_Changed => Nth_Arg (Data, 3, null));
         Set_Data (Inst, Convert (Combo));

      elsif Command = "add" then
         Name_Parameters (Data, Add_Args);
         Add_Combo_Entry
           (Combo       => Convert (Get_Data (Inst)),
            Label       => Nth_Arg (Data, 2),
            On_Selected => Nth_Arg (Data, 3, null));

      elsif Command = "remove" then
         Name_Parameters (Data, Remove_Args);
         Remove_Combo_Item
           (Combo => Convert (Get_Data (Inst)),
            Label => Nth_Arg (Data, 2));

      elsif Command = "clear" then
         Name_Parameters (Data, Simple_Args);
         Clear_Combo (Combo => Convert (Get_Data (Inst)));

      elsif Command = "get_text" then
         Name_Parameters (Data, Simple_Args);
         Set_Return_Value
           (Data, Get_Combo_Text (Convert (Get_Data (Inst))));

      elsif Command = "set_text" then
         Name_Parameters (Data, Set_Text_Args);
         Set_Combo_Text (Convert (Get_Data (Inst)), Nth_Arg (Data, 2));
      end if;

      Free (Inst);
   end Custom_Entry_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Toolbar_Class : constant Class_Type := New_Class (Kernel, "Toolbar");
      Toolbar_Entry_Class : constant Class_Type :=
        New_Class (Kernel, "ToolbarEntry");
   begin
      Register_Command
        (Kernel, "entry",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Toolbar_Class,
         Handler       => Custom_Toolbar_Handler'Access);
      Register_Command
        (Kernel, Constructor_Method,
         Class         => Toolbar_Class,
         Handler       => Custom_Toolbar_Handler'Access);
      Register_Command
        (Kernel, "append",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Toolbar_Class,
         Handler       => Custom_Toolbar_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Toolbar_Entry_Class,
         Handler       => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "remove",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Toolbar_Entry_Class,
         Handler       => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "add",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Toolbar_Entry_Class,
         Handler       => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "clear",
         Class         => Toolbar_Entry_Class,
         Handler       => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "get_text",
         Class         => Toolbar_Entry_Class,
         Handler       => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "set_text",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Toolbar_Entry_Class,
         Handler       => Custom_Entry_Handler'Access);
   end Register_Commands;

end Custom_Combos;
