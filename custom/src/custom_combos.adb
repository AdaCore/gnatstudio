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
with Glib.Values;               use Glib.Values;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Label;                 use Gtk.Label;

with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with Glide_Intl;                use Glide_Intl;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Commands.Interactive;      use Commands.Interactive;
with Custom_Module;             use Custom_Module;
with Commands;                  use Commands;
with GUI_Utils;                 use GUI_Utils;

with Gtk.List;                  use Gtk.List;

package body Custom_Combos is

   Entry_Cst         : aliased constant String := "id";
   Label_Cst         : aliased constant String := "label";
   Choice_Cst        : aliased constant String := "choice";
   On_Select         : aliased constant String := "on_selected";
   On_Changed        : aliased constant String := "on_changed";
   Add_Args : constant Cst_Argument_List :=
     (Entry_Cst'Access, Choice_Cst'Access, On_Select'Access);
   Create_Args : constant Cst_Argument_List :=
     (Entry_Cst'Access, Label_Cst'Access, On_Changed'Access);
   Remove_Args : constant Cst_Argument_List :=
     (Entry_Cst'Access, Choice_Cst'Access);
   Set_Text_Args : constant Cst_Argument_List :=
     (Entry_Cst'Access, Choice_Cst'Access);
   Simple_Args : constant Cst_Argument_List := (1 => Entry_Cst'Access);

   -----------------------
   -- Local subprograms --
   -----------------------

   package GPS_Combo_Handler is new User_Callback
     (Gtk_Entry_Record, GPS_Combo_Access);

   function Lookup_GPS_Combo (S : String) return GPS_Combo_Access;
   --  Return a registered combo with label S. If no combo was found,
   --  return null.

   function Lookup_Entry
     (Combo : GPS_Combo_Access;
      Label : String) return Entry_Action_Record;
   --  Return the label in Combo, if it exists. Otherwise return (null, null).

   procedure Combo_Changed
     (Ent       : access Gtk_Entry_Record'Class;
      Values    : GValues;
      GPS_Combo : GPS_Combo_Access);
   --  Called whenever the text in GPS_Combo is changed.

   procedure Execute_Combo_Action
     (Command : Action_Record_Access;
      Title   : String;
      Text    : String);
   --  Execute a command with the parameters Title and Text.

   function Get_Combo_Text
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String) return String;
   --  Return the text currently being displayed in the
   --  combo entry identified by Id.

   procedure Set_Combo_Text
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String;
      Text   : String);
   --  Set the text in combo identified by Id to Text.

   procedure Clear_Combo
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String);
   --  Clear the combo from all its items.

   procedure Remove_Combo_Item
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String;
      Label  : String);
   --  Remove choice identified by Label from combo identified by Id.

   procedure Set_Combo_Changed_Action
     (Kernel  : access Kernel_Handle_Record'Class;
      Id      : String;
      Command : Subprogram_Type);
   --  Set the callback to call when the content of the combo is changed

   -----------------------
   -- Remove_Combo_Item --
   -----------------------

   procedure Remove_Combo_Item
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String;
      Label  : String)
   is
      use Entry_List;

      Found     : Boolean := False;
      Node      : List_Node;
      Prev      : List_Node := Null_Node;
      GPS_Combo : constant GPS_Combo_Access := Lookup_GPS_Combo (Id);
   begin
      if GPS_Combo = null then
         Insert
           (Kernel, -"Entry not registered: " & Id,
            Mode => Error);
         return;
      end if;

      --  Find the node and remove it.

      Node := First (GPS_Combo.Entries);

      while Node /= Null_Node loop
         if Data (Node).Label.all = Label then
            Remove_Nodes (GPS_Combo.Entries, Prev, Node);
            Found := True;
            exit;
         end if;

         Prev := Node;
         Node := Next (Node);
      end loop;

      --  If a node was actually removed, regenerate the list
      --  of entries.

      if Found then
         declare
            Selected : constant String :=
              Get_Text (Get_Entry (GPS_Combo.Combo));
         begin
            Clear_Items (Get_List (GPS_Combo.Combo), 0, -1);

            Node := First (GPS_Combo.Entries);

            while Node /= Null_Node loop
               Add_Unique_Combo_Entry
                 (GPS_Combo.Combo, Data (Node).Label.all);
               Node := Next (Node);
            end loop;

            if Selected /= Label then
               Set_Text (Get_Entry (GPS_Combo.Combo), Selected);
            else
               Set_Text (Get_Entry (GPS_Combo.Combo), "");
            end if;
         end;
      else
         Insert
           (Kernel, -"Entry choice not found: " & Label,
            Mode => Error);
      end if;
   end Remove_Combo_Item;

   -----------------
   -- Clear_Combo --
   -----------------

   procedure Clear_Combo
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String)
   is
      use Entry_List;
      GPS_Combo : constant GPS_Combo_Access := Lookup_GPS_Combo (Id);
   begin
      if GPS_Combo = null then
         Insert
           (Kernel, -"Entry not registered: " & Id,
            Mode => Error);
         return;
      end if;

      Free (GPS_Combo.Entries);
      Clear_Items (Get_List (GPS_Combo.Combo), 0, -1);
   end Clear_Combo;

   ------------------
   -- Lookup_Entry --
   ------------------

   function Lookup_Entry
     (Combo : GPS_Combo_Access;
      Label : String) return Entry_Action_Record
   is
      use Entry_List;

      Node : List_Node;
   begin
      Node := First (Combo.Entries);

      while Node /= Null_Node loop
         if Data (Node).Label.all = Label then
            return Data (Node);
         end if;

         Node := Next (Node);
      end loop;

      return (null, null, null);
   end Lookup_Entry;

   --------------------------
   -- Execute_Combo_Action --
   --------------------------

   procedure Execute_Combo_Action
     (Command : Action_Record_Access;
      Title   : String;
      Text    : String)
   is
      Custom : Command_Access;
      Args   : GNAT.OS_Lib.String_List_Access;
   begin
      Args := new GNAT.OS_Lib.String_List (1 .. 2);
      Args (1) := new String'(Title);
      Args (2) := new String'(Text);

      Custom := Create_Proxy
        (Command.Command,
         (null,
          null,
          null,
          Args,
          new String'(Title)));

      Launch_Background_Command
        (Custom_Module_ID.Kernel,
         Custom,
         True,
         True,
         "",
         True);
   end Execute_Combo_Action;

   ----------------------
   -- Lookup_GPS_Combo --
   ----------------------

   function Lookup_GPS_Combo (S : String) return GPS_Combo_Access is
      Node : Combo_List.List_Node;

      use Combo_List;
   begin
      Node := First (Custom_Module_ID.Combos);

      while Node /= Null_Node loop
         if Data (Node).Label.all = S then
            return Data (Node);
         end if;

         Node := Next (Node);
      end loop;

      return null;
   end Lookup_GPS_Combo;

   -------------------
   -- Combo_Changed --
   -------------------

   procedure Combo_Changed
     (Ent       : access Gtk_Entry_Record'Class;
      Values    : GValues;
      GPS_Combo : GPS_Combo_Access)
   is
      pragma Unreferenced (Ent, Values);

      Combo_Text : constant String := Get_Text
        (Get_Entry (GPS_Combo.Combo));

      Combo_Entry : Entry_Action_Record;
   begin
      --  Do not react on empty string.

      if Combo_Text = "" then
         return;
      end if;

      --  Execute the action to call whenever changing, if any.
      if GPS_Combo.On_Change_Action /= null then
         Execute_Combo_Action
           (GPS_Combo.On_Change_Action,
            GPS_Combo.Label.all,
            Combo_Text);
      end if;

      if GPS_Combo.On_Change_Subprogram /= null then
         declare
            D : Callback_Data'Class := Create
              (Get_Script (GPS_Combo.On_Change_Subprogram.all),
               Arguments_Count => 2);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Set_Nth_Arg (D, 1, GPS_Combo.Label.all);
            Set_Nth_Arg (D, 2, Combo_Text);
            Tmp := Execute (GPS_Combo.On_Change_Subprogram, D);
            Free (D);
         end;
      end if;

      --  Attempt to find an item in the registered items.
      --  If such an item is found, execute the associated action.

      Combo_Entry := Lookup_Entry (GPS_Combo, Combo_Text);

      if Combo_Entry.Label /= null
        and then Combo_Entry.Command /= null
      then
         Execute_Combo_Action
           (Combo_Entry.Command,
            GPS_Combo.Label.all,
            Combo_Text);
      end if;

      if Combo_Entry.On_Selected /= null then
         declare
            D : Callback_Data'Class := Create
              (Get_Script (Combo_Entry.On_Selected.all),
               Arguments_Count => 2);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Set_Nth_Arg (D, 1, GPS_Combo.Label.all);
            Set_Nth_Arg (D, 2, Combo_Text);
            Tmp := Execute (Combo_Entry.On_Selected, D);
            Free (D);
         end;
      end if;
   end Combo_Changed;

   --------------------
   -- Register_Combo --
   --------------------

   procedure Register_Combo
     (Kernel : access Kernel_Handle_Record'Class;
      Title  : String;
      Id     : String)
   is
      GPS_Combo : GPS_Combo_Access;
      Label     : Gtk_Label;

      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);
   begin
      if Lookup_GPS_Combo (Title) /= null then
         Insert
           (Kernel, -"Entry already registered: " & Title,
            Mode => Error);
         return;
      end if;

      if Title /= "" then
         Gtk_New (Label, Title & " ");
         Append_Widget (Toolbar, Label, "", "");
         Show_All (Label);
      end if;

      GPS_Combo := new GPS_Combo_Record;
      GPS_Combo.Label := new String'(Id);

      Gtk_New (GPS_Combo.Combo);
      Set_Editable (Get_Entry (GPS_Combo.Combo), False);

      GPS_Combo_Handler.Connect
        (Get_Entry (GPS_Combo.Combo), "changed",
         Combo_Changed'Access,
         GPS_Combo,
         After => True);

      Append_Widget (Toolbar, GPS_Combo.Combo, Title, Title);
      Show_All (GPS_Combo.Combo);

      Combo_List.Append (Custom_Module_ID.Combos, GPS_Combo);
   end Register_Combo;

   ------------------------------
   -- Set_Combo_Changed_Action --
   ------------------------------

   procedure Set_Combo_Changed_Action
     (Kernel  : access Kernel_Handle_Record'Class;
      Id      : String;
      Command : Subprogram_Type)
   is
      GPS_Combo : constant GPS_Combo_Access := Lookup_GPS_Combo (Id);
   begin
      if GPS_Combo = null then
         Insert
           (Kernel, -"Entry not registered: " & Id,
            Mode => Error);
         return;
      end if;

      GPS_Combo.On_Change_Subprogram := Command;
   end Set_Combo_Changed_Action;

   ------------------------------
   -- Set_Combo_Changed_Action --
   ------------------------------

   procedure Set_Combo_Changed_Action
     (Kernel  : access Kernel_Handle_Record'Class;
      Id      : String;
      Command : Action_Record_Access)
   is
      GPS_Combo : constant GPS_Combo_Access := Lookup_GPS_Combo (Id);
   begin
      if GPS_Combo = null then
         Insert
           (Kernel, -"Entry not registered: " & Id,
            Mode => Error);
         return;
      end if;

      GPS_Combo.On_Change_Action := Command;
   end Set_Combo_Changed_Action;

   ---------------------
   -- Add_Combo_Entry --
   ---------------------

   procedure Add_Combo_Entry
     (Kernel  : access Kernel_Handle_Record'Class;
      Id      : String;
      Label   : String;
      Command : Action_Record_Access;
      On_Selected : Glide_Kernel.Scripts.Subprogram_Type)
   is
      GPS_Combo   : constant GPS_Combo_Access := Lookup_GPS_Combo (Id);
      Combo_Entry : Entry_Action_Record;
   begin
      if GPS_Combo = null then
         Insert
           (Kernel, -"Entry not registered: " & Id,
            Mode => Error);
         return;
      end if;

      Combo_Entry := Lookup_Entry (GPS_Combo, Label);

      if Combo_Entry.Label /= null then
         Insert
           (Kernel, -"Entry choice already registered: " & Label,
            Mode => Error);
         return;
      end if;

      Combo_Entry.Label       := new String'(Label);
      Combo_Entry.Command     := Command;
      Combo_Entry.On_Selected := On_Selected;

      Entry_List.Append
        (GPS_Combo.Entries,
         Combo_Entry);

      Add_Unique_Combo_Entry (GPS_Combo.Combo, Label);
   end Add_Combo_Entry;

   --------------------
   -- Get_Combo_Text --
   --------------------

   function Get_Combo_Text
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String)
      return String
   is
      GPS_Combo   : constant GPS_Combo_Access := Lookup_GPS_Combo (Id);
   begin
      if GPS_Combo = null then
         Insert
           (Kernel, -"Entry not registered: " & Id,
            Mode => Error);
         return "";
      end if;

      return Get_Text (Get_Entry (GPS_Combo.Combo));
   end Get_Combo_Text;

   --------------------
   -- Set_Combo_Text --
   --------------------

   procedure Set_Combo_Text
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String;
      Text   : String)
   is
      GPS_Combo   : constant GPS_Combo_Access := Lookup_GPS_Combo (Id);
   begin
      if GPS_Combo = null then
         Insert
           (Kernel, -"Entry not registered: " & Id,
            Mode => Error);
         return;
      end if;

      Set_Text (Get_Entry (GPS_Combo.Combo), Text);
   end Set_Combo_Text;

   --------------------------
   -- Custom_Entry_Handler --
   --------------------------

   procedure Custom_Entry_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel    : constant Kernel_Handle := Custom_Module_ID.Kernel;
      Callback  : Subprogram_Type;
   begin
      if Command = "entry_add" then
         Name_Parameters (Data, Add_Args);
         Add_Combo_Entry
           (Kernel,
            Nth_Arg (Data, 2),
            Nth_Arg (Data, 3),
            Command     => null,
            On_Selected => Nth_Arg (Data, 4, null));

      elsif Command = "entry_create" then
         Name_Parameters (Data, Create_Args);
         declare
            Id : constant String := Nth_Arg (Data, 2);
         begin
            Register_Combo
              (Kernel,
               Title => Nth_Arg (Data, 3, ""),
               Id    => Id);
            Callback := Nth_Arg (Data, 4, null);
            if Callback /= null then
               Set_Combo_Changed_Action
                 (Kernel, Id => Id, Command => Callback);
            end if;
         end;

      elsif Command = "entry_remove" then
         Name_Parameters (Data, Remove_Args);
         Remove_Combo_Item (Kernel, Nth_Arg (Data, 2), Nth_Arg (Data, 3));

      elsif Command = "entry_clear" then
         Name_Parameters (Data, Simple_Args);
         Clear_Combo (Kernel, Nth_Arg (Data, 2));

      elsif Command = "entry_get_text" then
         Name_Parameters (Data, Simple_Args);
         Set_Return_Value (Data, Get_Combo_Text (Kernel, Nth_Arg (Data, 2)));

      elsif Command = "entry_set_text" then
         Name_Parameters (Data, Set_Text_Args);
         Set_Combo_Text (Kernel, Nth_Arg (Data, 2), Nth_Arg (Data, 3));
      end if;
   end Custom_Entry_Handler;

end Custom_Combos;
