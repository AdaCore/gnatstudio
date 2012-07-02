------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with System;                  use System;

with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with Glib.Main;               use Glib.Main;
with Glib.Object;             use Glib, Glib.Object;

with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;

with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Gtk.Accel_Map;           use Gtk.Accel_Map;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Cell_Renderer_Text;  use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;        use Gtk.Check_Button;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Event_Box;           use Gtk.Event_Box;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main;                use Gtk.Main;
with Gtk.Paned;               use Gtk.Paned;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Separator;           use Gtk.Separator;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtk.Text_View;           use Gtk.Text_View;
with Gtk.Toggle_Button;       use Gtk.Toggle_Button;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;   use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort;     use Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection;      use Gtk.Tree_Selection;
with Gtk.Tree_Store;          use Gtk.Tree_Store;
with Gtk.Tree_View;           use Gtk.Tree_View;
with Gtk.Tree_View_Column;    use Gtk.Tree_View_Column;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Pango.Enums;             use Pango.Enums;

with Commands.Interactive;    use Commands.Interactive;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;   use GPS.Kernel.Modules.UI;
with GPS.Intl;                use GPS.Intl;
with GUI_Utils;               use GUI_Utils;
with Traces;                  use Traces;

package body KeyManager_Module.GUI is
   use Key_Htable;

   Action_Column     : constant := 0;
   Key_Column        : constant := 1;

   type Keys_Editor_Record is new Gtk_Dialog_Record with record
      Kernel             : Kernel_Handle;
      Bindings           : HTable_Access;
      View               : Gtk_Tree_View;
      Model              : Gtk_Tree_Store;
      Filter             : Gtk_Tree_Model_Filter;
      Sort               : Gtk_Tree_Model_Sort;
      Help               : Gtk_Text_Buffer;
      Action_Name        : Gtk_Label;
      With_Shortcut_Only : Gtk_Check_Button;
      Flat_List          : Gtk_Check_Button;
      Remove_Button      : Gtk_Button;
      Grab_Button        : Gtk_Toggle_Button;
      Grab_Label         : Gtk_Label;

      Disable_Filtering  : Boolean := False;
   end record;
   type Keys_Editor is access all Keys_Editor_Record'Class;

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class);
   --  Fill the contents of the editor

   procedure On_Edit_Keys
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open a GUI to edit the key bindings

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class);
   procedure On_Remove_Key (Editor : access Gtk_Widget_Record'Class);
   --  Handle the "Grab", "Remove" and "Add" buttons

   function Grab_Multiple_Key
     (Kernel         : access Kernel_Handle_Record'Class;
      Widget         : access Gtk_Widget_Record'Class;
      Allow_Multiple : Boolean) return String;
   --  Grab a key binding, with support for multiple keymaps. Returns the
   --  empty string if no key could be grabbed.

   function Cancel_Grab return Boolean;
   --  Exit the current nest main loop, if any

   procedure Refresh_Editor (Editor : access Keys_Editor_Record'Class);
   --  Refresh the list of key bindings in editor. Better use this one than
   --  Fill_Editor when possible, since this will preserve expanded/closed
   --  nodes.

   procedure Save_Editor (Editor : access Keys_Editor_Record'Class);
   --  Save the contents of the editor

   procedure On_Toggle_Flat_List (Editor : access Gtk_Widget_Record'Class);
   --  Called when the user toggles the "View Flat List" filter button

   procedure On_Toggle_Shortcuts_Only
     (Editor : access Gtk_Widget_Record'Class);
   --  Called when the user toggles "View only actions with shortcuts"

   package Keys_Editor_Visible_Funcs is new Gtk.Tree_Model_Filter.Visible_Funcs
     (Keys_Editor);
   function Action_Is_Visible
     (Model : access Gtk_Tree_Model_Record'Class;
      Iter  : Gtk_Tree_Iter;
      Data  : Keys_Editor) return Boolean;
   --  Selects whether a given row should be visible in the key shortcuts
   --  editor.

   procedure Add_Selection_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the selection has changed

   function Find_Parent
     (Model  : Gtk_Tree_Store;
      Action : Action_Record_Access) return Gtk_Tree_Iter;
   --  Find the parent node for Action.
   --  Create the parent node if needed

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String;
      Key    : String := "") return Gtk_Tree_Iter;
   --  Add a new line into the model

   ---------
   -- Set --
   ---------

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String;
      Key    : String := "") return Gtk_Tree_Iter
   is
      procedure Set
        (Tree, Iter : System.Address;
         Col1       : Gint; Value1 : String;
         Col2       : Gint; Value2 : String);
      pragma Import (C, Set, "ada_gtk_tree_store_set_ptr_ptr");

      Iter : Gtk_Tree_Iter;

   begin
      Append (Model, Iter, Parent);
      Set
        (Get_Object (Model), Iter'Address,
         Col1 => Action_Column,     Value1 => Descr & ASCII.NUL,
         Col2 => Key_Column,        Value2 => Key & ASCII.NUL);
      return Iter;
   end Set;

   -----------------
   -- Find_Parent --
   -----------------

   function Find_Parent
     (Model  : Gtk_Tree_Store;
      Action : Action_Record_Access) return Gtk_Tree_Iter
   is
      Parent : Gtk_Tree_Iter;
   begin
      if Action = null or else Action.Category = null then
         return Null_Iter;

      else
         Parent := Find_Node (Model, Action.Category.all, Action_Column);
         if Parent = Null_Iter then
            Parent := Set (Model, Null_Iter,
                           Descr => Action.Category.all);
         end if;
      end if;

      return Parent;
   end Find_Parent;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class) is
      Flat_List : constant Boolean := Get_Active (Editor.Flat_List);

      Parent       : Gtk_Tree_Iter;
      Action       : Action_Record_Access;
      Action_Iter  : Action_Iterator := Start (Editor.Kernel);
      User_Changed : aliased Boolean;
   begin
      --  Disable tree filtering while refreshing the contents of the tree.
      --  This works around a bug in gtk+.
      Editor.Disable_Filtering := True;

      Clear (Editor.Model);

      --  Add all known actions in the table.
      loop
         Action := Get (Action_Iter);
         exit when Action = null;

         if not Flat_List then
            Parent := Find_Parent (Editor.Model, Action);
         else
            Parent := Null_Iter;
         end if;

         if Action.Category /= null
           and then (Flat_List or else Parent /= Null_Iter)
         then
            declare
               Name : constant String := Get (Action_Iter).Name.all;
            begin
               Parent := Set
                 (Model   => Editor.Model,
                  Parent  => Parent,
                  Descr   => Name,
                  Key     => Lookup_Key_From_Action
                    (Editor.Bindings,
                     Name,
                     Is_User_Changed => User_Changed'Unchecked_Access,
                     Default => -Disabled_String));
            end;
         end if;

         Next (Editor.Kernel, Action_Iter);
      end loop;

      Editor.Disable_Filtering := False;

      Refilter (Editor.Filter);
   end Fill_Editor;

   -----------------
   -- Save_Editor --
   -----------------

   procedure Save_Editor (Editor : access Keys_Editor_Record'Class) is

      procedure Process_Menu_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean);
      --  Called for each known accel path

      --------------------------
      -- Process_Menu_Binding --
      --------------------------

      procedure Process_Menu_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean)
      is
         First   : Natural := Accel_Path'First + 1;
         Iter    : Key_Htable.Cursor;
         Binding : Key_Description_List;
         Found   : Boolean := False;
         Ignore  : Boolean;
         pragma Unreferenced (Data, Changed, Accel_Key, Accel_Mods, Ignore);

      begin
         while First <= Accel_Path'Last
           and then Accel_Path (First - 1) /= '>'
         loop
            First := First + 1;
         end loop;

         --  If the menu is associated with at least one short key binding (ie
         --  from the toplevel keymap), we change it so that it shows up in the
         --  menu as well).
         Get_First (Get_Shortcuts (Editor.Kernel).all, Iter);
         Foreach_Binding :
         loop
            Binding := Get_Element (Iter);
            exit Foreach_Binding when Binding = No_Key;

            if Get_Key (Iter).Key = 0 then
               --  An invalid key, here just to indicate the key should be
               --  disabled during the next startup.
               Binding := null;
            end if;

            while Binding /= null loop
               if Binding.Action /= null
                 and then Equal
                   (Binding.Action.all,
                    Accel_Path (First .. Accel_Path'Last),
                    Case_Sensitive => False)
               then
                  Found := True;

                  --  The following call will fail in general, since the
                  --  shortcut is already associated with the same Accel_Path.
                  --  Unfortunately, gtk+ doesn't detect that we are just
                  --  trying to set the same binding again, and will always
                  --  report a failure. We should not therefore fallback on
                  --  clearing the binding in case of failure. F721-013
                  Ignore := Change_Entry
                    (Accel_Path => Accel_Path,
                     Accel_Key  => Get_Key (Iter).Key,
                     Accel_Mods => Get_Key (Iter).Modifier,
                     Replace    => True);
                  exit Foreach_Binding;
               end if;

               Binding := Binding.Next;
            end loop;

            Get_Next (Get_Shortcuts (Editor.Kernel).all, Iter);
         end loop Foreach_Binding;

         if not Found then
            Ignore := Change_Entry
              (Accel_Path => Accel_Path,
               Accel_Key  => 0,
               Accel_Mods => 0,
               Replace    => True);
         end if;
      end Process_Menu_Binding;

   begin
      Clone (From => Editor.Bindings.all,
             To   => Get_Shortcuts (Editor.Kernel).all);

      --  Update the gtk+ accelerators for the menus to reflect the keybindings
      Gtk.Accel_Map.Foreach_Unfiltered
        (System.Null_Address, Process_Menu_Binding'Unrestricted_Access);

      Save_Custom_Keys (Editor.Kernel);
   end Save_Editor;

   ---------------------------
   -- Add_Selection_Changed --
   ---------------------------

   procedure Add_Selection_Changed (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Keys_Editor := Keys_Editor (Editor);
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Text_Iter : Gtk_Text_Iter;
      Action    : Action_Record_Access;
      Comp_Iter : Component_Iterator;
      Bold      : Gtk_Text_Tag;

      procedure Insert_Details
        (Comp_Iter : Component_Iterator; Prefix : String);
      --  Insert the detail for the components of the action

      --------------------
      -- Insert_Details --
      --------------------

      procedure Insert_Details
        (Comp_Iter : Component_Iterator; Prefix : String)
      is
         Comp    : Command_Component;
         Failure : Component_Iterator;
      begin
         loop
            Comp := Get (Comp_Iter);
            exit when Comp = null;

            Insert
              (Ed.Help, Text_Iter, Prefix & Get_Name (Comp) & ASCII.LF);

            Failure := On_Failure (Comp_Iter);
            if Failure /= null then
               Insert (Ed.Help, Text_Iter, Prefix & "on-failure:" & ASCII.LF);
               Insert_Details (Failure, Prefix & "   ");
            end if;

            Next (Comp_Iter);
         end loop;
      end Insert_Details;

      User_Changed : aliased Boolean;
   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)
      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Set_Sensitive (Ed.Remove_Button, True);
         Set_Sensitive (Ed.Grab_Button, True);

         Action := Lookup_Action (Ed.Kernel, Get_String (Model, Iter, 0));

         if Action /= null and then Action.Description /= null then
            Set_Text (Ed.Help, Action.Description.all);
         else
            Set_Text (Ed.Help, "");
         end if;

         --  Action could be null if we chose to display only lines with
         --  shortcuts and the user clicks on a line for a category
         if Action /= null then
            Get_End_Iter (Ed.Help, Text_Iter);

            Bold := Create_Tag (Ed.Help);
            Set_Property (Bold, Gtk.Text_Tag.Weight_Property,
                          Pango_Weight_Bold);

            Insert_With_Tags
              (Ed.Help, Text_Iter,
               ASCII.LF & ASCII.LF & (-"Current key shortcut: "),
               Bold);
            Insert
              (Ed.Help, Text_Iter,
               Lookup_Key_From_Action
                 (Ed.Bindings,
                  Action            => Get_String (Model, Iter, Action_Column),
                  Default           => -"none",
                  Is_User_Changed => User_Changed'Unchecked_Access,
                  Use_Markup        => False));

            Insert_With_Tags
              (Ed.Help, Text_Iter, ASCII.LF & (-"Declared in: "),
               Bold);
            if Action.Defined_In /= GNATCOLL.VFS.No_File then
               Insert
                 (Ed.Help, Text_Iter, Display_Full_Name (Action.Defined_In));

               Comp_Iter := Start (Action.Command);
               if Get (Comp_Iter) /= null then
                  Insert_With_Tags
                    (Ed.Help, Text_Iter,
                     ASCII.LF & (-"Implementation details:") & ASCII.LF,
                     Bold);
                  Insert_Details (Comp_Iter, "  ");
               end if;

            else
               Insert (Ed.Help, Text_Iter, -"built-in");
            end if;

            Set_Text (Ed.Action_Name, Get_String (Model, Iter, 0));
         end if;
      else
         Set_Sensitive (Ed.Remove_Button, False);
         Set_Sensitive (Ed.Grab_Button, False);
         Set_Text (Ed.Help, "");
         Set_Text (Ed.Action_Name, "");
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Selection_Changed;

   -------------------------
   -- On_Toggle_Flat_List --
   -------------------------

   procedure On_Toggle_Flat_List (Editor : access Gtk_Widget_Record'Class) is
   begin
      Fill_Editor (Keys_Editor (Editor));
   end On_Toggle_Flat_List;

   ------------------------------
   -- On_Toggle_Shortcuts_Only --
   ------------------------------

   procedure On_Toggle_Shortcuts_Only
     (Editor : access Gtk_Widget_Record'Class) is
   begin
      Refilter (Keys_Editor (Editor).Filter);
   end On_Toggle_Shortcuts_Only;

   -----------------------
   -- Action_Is_Visible --
   -----------------------

   function Action_Is_Visible
     (Model : access Gtk_Tree_Model_Record'Class;
      Iter  : Gtk_Tree_Iter;
      Data  : Keys_Editor) return Boolean
   is
   begin
      return Data.Disable_Filtering
        or else not Get_Active (Data.With_Shortcut_Only)
        or else Get_String (Model, Iter, 1) /= ""
        or else N_Children (Model, Iter) > 0;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return True;
   end Action_Is_Visible;

   --------------------
   -- Refresh_Editor --
   --------------------

   procedure Refresh_Editor (Editor : access Keys_Editor_Record'Class) is

      procedure Refresh_Iter (Iter : Gtk_Tree_Iter);
      --  Refresh for Iter and its sibling

      ------------------
      -- Refresh_Iter --
      ------------------

      procedure Refresh_Iter (Iter : Gtk_Tree_Iter) is
         It           : Gtk_Tree_Iter;
         User_Changed : aliased Boolean;
      begin
         Iter_Copy (Source => Iter, Dest => It);
         while It /= Null_Iter loop
            if Children (Editor.Model, It) /= Null_Iter then
               Refresh_Iter (Children (Editor.Model, It));
            else
               Set
                 (Editor.Model, It, Key_Column,
                  Lookup_Key_From_Action
                    (Editor.Bindings,
                     Action => Get_String (Editor.Model, It, Action_Column),
                     Default => "",
                     Is_User_Changed => User_Changed'Unchecked_Access));
            end if;

            Next (Editor.Model, It);
         end loop;
      end Refresh_Iter;

   begin
      Refresh_Iter (Get_Iter_First (Editor.Model));
      Add_Selection_Changed (Editor);
   end Refresh_Editor;

   -----------------
   -- Cancel_Grab --
   -----------------

   function Cancel_Grab return Boolean is
   begin
      --  If there is a grab pending

      if Main_Level > 1 then
         Main_Quit;
      end if;

      return False;
   end Cancel_Grab;

   -----------------------
   -- Grab_Multiple_Key --
   -----------------------

   function Grab_Multiple_Key
     (Kernel         : access Kernel_Handle_Record'Class;
      Widget         : access Gtk_Widget_Record'Class;
      Allow_Multiple : Boolean) return String
   is
      Grabbed, Tmp : String_Access;
      Key          : Gdk_Key_Type;
      Modif        : Gdk_Modifier_Type;
      Id           : Glib.Main.G_Source_Id;

   begin
      Block_Key_Shortcuts (Kernel);

      Key_Grab (Widget, Key, Modif);

      if Key /= GDK_Escape or else Modif /= 0 then
         Grabbed := new String'(Image (Key, Modif));
      else
         return "";
      end if;

      --  Are we grabbing multiple keymaps ?

      if Allow_Multiple then
         loop
            Id := Glib.Main.Timeout_Add (500, Cancel_Grab'Access);
            Key_Grab (Widget, Key, Modif);
            Glib.Main.Remove (Id);

            exit when Key = 0 and then Modif = 0;

            if Key = GDK_Escape and then Modif = 0 then
               Free (Grabbed);
               return "";
            end if;

            Tmp := Grabbed;
            Grabbed := new String'(Grabbed.all & ' ' & Image (Key, Modif));
            Free (Tmp);
         end loop;
      end if;

      Unblock_Key_Shortcuts (Kernel);

      declare
         K : constant String := Grabbed.all;
      begin
         Free (Grabbed);
         return K;
      end;

   exception
      when others =>
         Unblock_Key_Shortcuts (Kernel);
         raise;
   end Grab_Multiple_Key;

   -----------------
   -- On_Grab_Key --
   -----------------

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class) is
      Ed         : constant Keys_Editor := Keys_Editor (Editor);
      Selection  : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Sort_Model : Gtk_Tree_Model;
      Sort_Iter, Filter_Iter, Iter : Gtk_Tree_Iter;
   begin
      if Get_Active (Ed.Grab_Button) then
         Get_Selected (Selection, Sort_Model, Sort_Iter);
         if Sort_Iter /= Null_Iter then
            Convert_Iter_To_Child_Iter (Ed.Sort, Filter_Iter, Sort_Iter);
            Convert_Iter_To_Child_Iter (Ed.Filter, Iter, Filter_Iter);
         else
            Iter := Null_Iter;
         end if;

         --  Only edit for leaf nodes (otherwise these are contexts)

         if Iter /= Null_Iter
           and then Children (Ed.Model, Iter) = Null_Iter
         then
            Show (Ed.Grab_Label);

            declare
               Key          : constant String :=
                                Grab_Multiple_Key
                                  (Ed.Kernel, Ed, Allow_Multiple => True);
               Old_Action   : constant String :=
                                Lookup_Action_From_Key (Key, Ed.Bindings);
               Old_Prefix   : constant String :=
                                Actions_With_Key_Prefix (Key, Ed.Bindings);
               User_Changed : aliased Boolean := False;
               Count_Prefix : constant Natural :=
                                Count
                                  (Old_Prefix,
                                   To_Set (String'(1 => ASCII.LF)));
               New_Action   : constant String :=
                                Get_String (Ed.Model, Iter, Action_Column);
               Do_Nothing   : Boolean := False;
            begin
               if Key /= "" and then Key /= "Escape" then
                  --  Do we already have an action with such a binding ?

                  if Old_Action /= ""
                    and then
                      Equal (Old_Action, New_Action, Case_Sensitive => False)
                    --  And there is a single action for this key
                    and then Count_Prefix = 1
                    --  We also check for the key binding, this is the tricky
                    --  case where the action is mapped to ctrl-x for example
                    --  and we want to map it to ctrl-x+b. So we do nothing
                    --  only if the if keys are fully equivelent.
                    and then Key = Lookup_Key_From_Action
                      (Ed.Bindings,
                       Old_Action,
                       Is_User_Changed => User_Changed'Access)
                  then
                     --  key already bound to Old_Action and no clash for the
                     --  prefix, nothing to do.
                     Do_Nothing := True;

                  elsif Count_Prefix > 1
                    or else (Count_Prefix = 1
                             and then Index (Old_Prefix, New_Action) = 0)
                  then
                     if Active (Testsuite_Handle) then
                        --  When running the testsuite, we cannot display the
                        --  dialog, since there is apparently no way to control
                        --  is from python otherwise (probably because it is
                        --  running in its own gtk+ loop).

                        Do_Nothing := False;  --  Perform the replacement
                        Trace
                          (Testsuite_Handle,
                           "Dialog for already assigned key would have"
                           & " been displayed");

                     elsif Message_Dialog
                       (Msg =>
                          Key
                        & (-" (or prefix) is already used for: ")
                        & ASCII.LF & ASCII.LF
                        & Old_Prefix & ASCII.LF
                        & (-"Do you want to assign ") & Key & (-" to """)
                        & New_Action
                        & (-""" and disable all actions above ?"),
                        Dialog_Type => Confirmation,
                        Buttons => Button_OK or Button_Cancel,
                        Title   => -"Key shortcut already exists",
                        Parent  => Get_Main_Window (Ed.Kernel)) /= Button_OK
                     then
                        Do_Nothing := True;
                     end if;
                  end if;

                  if Do_Nothing then
                     Hide (Ed.Grab_Label);
                     Set_Active (Ed.Grab_Button, False);
                     return;
                  end if;

                  Bind_Default_Key_Internal
                    (Kernel           => Ed.Kernel,
                     Table            => Ed.Bindings.all,
                     Action           => New_Action,
                     Key              => Key,
                     Save_In_Keys_XML => True,
                     Remove_Existing_Actions_For_Shortcut => True,
                     Remove_Existing_Shortcuts_For_Action => True,
                     Update_Menus     => True);
                  Refresh_Editor (Ed);
               end if;
            end;

            Hide (Ed.Grab_Label);
         end if;

         Set_Active (Ed.Grab_Button, False);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Grab_Key;

   -------------------
   -- On_Remove_Key --
   -------------------

   procedure On_Remove_Key (Editor : access Gtk_Widget_Record'Class) is
      Ed         : constant Keys_Editor := Keys_Editor (Editor);
      Selection  : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Sort_Model : Gtk_Tree_Model;
      Iter, Filter_Iter, Sort_Iter  : Gtk_Tree_Iter;
   begin
      Get_Selected (Selection, Sort_Model, Sort_Iter);
      Convert_Iter_To_Child_Iter (Ed.Sort, Filter_Iter, Sort_Iter);
      Convert_Iter_To_Child_Iter (Ed.Filter, Iter, Filter_Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)

      if Iter /= Null_Iter
        and then Children (Ed.Model, Iter) = Null_Iter
      then
         Bind_Default_Key_Internal
           (Table             => Ed.Bindings.all,
            Kernel            => Ed.Kernel,
            Action            => Get_String (Ed.Model, Iter, Action_Column),
            Key               => "",
            Save_In_Keys_XML  => True,
            Remove_Existing_Shortcuts_For_Action => True,
            Remove_Existing_Actions_For_Shortcut => True,
            Update_Menus      => False);

         Refresh_Editor (Ed);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Remove_Key;

   ------------------
   -- On_Edit_Keys --
   ------------------

   procedure On_Edit_Keys
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Editor    : Keys_Editor;
      Scrolled  : Gtk_Scrolled_Window;
      Bbox      : Gtk_Box;
      Hbox, Vbox, Filter_Box : Gtk_Box;
--        Button    : Gtk_Button;
      Col       : Gtk_Tree_View_Column;
      Render    : Gtk_Cell_Renderer_Text;
      Ignore    : Gint;
      Frame     : Gtk_Frame;
      Pane      : Gtk_Paned;
      Sep       : Gtk_Separator;
      Event     : Gtk_Event_Box;
      Text      : Gtk_Text_View;
      Ignore_Action : Gtk_Widget;
      pragma Unreferenced (Widget, Ignore, Ignore_Action);

   begin
      Editor := new Keys_Editor_Record;
      Editor.Bindings := new Key_Htable.Instance;

      Initialize
        (Editor,
         Title  => -"Key shortcuts",
         Parent => Get_Current_Window (Kernel),
         Flags  => Destroy_With_Parent or Modal);
      Set_Name (Editor, "Key shortcuts");  --  for testsuite
      Set_Default_Size (Editor, 900, 700);
      Editor.Kernel  := Kernel;

      Clone
        (From => Get_Shortcuts (Kernel).all,
         To   => Editor.Bindings.all);

      Gtk_New_Vbox (Vbox, Homogeneous => False);
      Pack_Start (Get_Vbox (Editor), Vbox, Expand => True, Fill => True);

      Gtk_New_Hbox (Filter_Box, Homogeneous => False);
      Pack_Start (Vbox, Filter_Box, Expand => False);

      Gtk_New (Editor.With_Shortcut_Only, -"Shortcuts only");
      Set_Tooltip_Text
        (Editor.With_Shortcut_Only,
         -("Show only actions that are associated with a key shortcut"));
      Set_Active (Editor.With_Shortcut_Only, False);
      Pack_Start (Filter_Box, Editor.With_Shortcut_Only, Expand => False);
      Widget_Callback.Object_Connect
        (Editor.With_Shortcut_Only,
         Gtk.Toggle_Button.Signal_Toggled, On_Toggle_Shortcuts_Only'Access,
         Editor);

      Gtk_New (Editor.Flat_List, -"Flat list");
      Set_Tooltip_Text
        (Editor.Flat_List,
         -("If selected, actions are not grouped into categories, but"
           & " displayed as a single long list. This might help to find some"
           & " specific actions"));
      Set_Active (Editor.Flat_List, False);
      Pack_Start (Filter_Box, Editor.Flat_List, Expand => False);
      Widget_Callback.Object_Connect
        (Editor.Flat_List,
         Signal_Toggled, On_Toggle_Flat_List'Access, Editor);

      --  ??? Will be implemented shortly
--        Gtk_New_From_Stock (Button, Stock_Find);
--        Pack_Start (Filter_Box, Button, Expand => False);

      Gtk_New_Vpaned (Pane);
      Pack_Start (Vbox, Pane, Expand => True, Fill => True);

      --  List of macros

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack1 (Pane, Scrolled, True, True);

      --  The model we will modify
      Gtk_New
        (Editor.Model,
         (Action_Column     => GType_String,
          Key_Column        => GType_String));

      --  A filter model on top of it, so that we can filter out some rows
      Gtk_New (Editor.Filter, Editor.Model);
      Keys_Editor_Visible_Funcs.Set_Visible_Func
        (Editor.Filter, Action_Is_Visible'Access, Editor);

      --  A sort model on top of the filter, so that rows can be sorted
      Gtk_New_With_Model (Editor.Sort, Editor.Filter);

      Gtk_New (Editor.View, Editor.Sort);
      Set_Name (Editor.View, "Key shortcuts tree"); --  for testsuite
      Add (Scrolled, Editor.View);

      --  Bottom area
      Gtk_New (Frame);
      Pack2 (Pane, Frame, False, True);
      Set_Size_Request (Frame, -1, 200);

      Gtk_New_Vbox (Hbox, Homogeneous => False);
      Add (Frame, Hbox);

      --  Name of current action

      Create_Blue_Label (Editor.Action_Name, Event);
      Pack_Start (Hbox,  Event, Expand => False);

      Gtk_New_Hbox (Bbox, Homogeneous => False);
      Pack_Start (Hbox, Bbox, Expand => False);

      Gtk_New_From_Stock (Editor.Remove_Button, Stock_Remove);
      Set_Sensitive (Editor.Remove_Button, False);
      Pack_Start (Bbox, Editor.Remove_Button, Expand => False);
      Widget_Callback.Object_Connect
        (Editor.Remove_Button,
         Gtk.Button.Signal_Clicked, On_Remove_Key'Access, Editor);

      Gtk_New (Editor.Grab_Button, -"Change Shortcut");
      Set_Sensitive (Editor.Grab_Button, False);
      Pack_Start (Bbox, Editor.Grab_Button, Expand => False);
      Widget_Callback.Object_Connect
        (Editor.Grab_Button,
         Gtk.Toggle_Button.Signal_Toggled, On_Grab_Key'Access, Editor);

      Gtk_New
        (Editor.Grab_Label,
         -"<i>Press the key(s) to use a shortcut / ESC to cancel</i>");
      Set_Use_Markup (Editor.Grab_Label, True);
      Set_Alignment (Editor.Grab_Label, 0.0, 0.5);
      Set_Padding (Editor.Grab_Label, 10, 0);
      Pack_Start (Bbox, Editor.Grab_Label, Expand => True, Fill => True);

      Widget_Callback.Object_Connect
        (Get_Selection (Editor.View), Gtk.Tree_Selection.Signal_Changed,
         Add_Selection_Changed'Access, Editor);

      Gtk_New_Hseparator (Sep);
      Pack_Start (Hbox, Sep, Expand => False);

      --  Help on current action

      Gtk_New (Editor.Help);
      Gtk_New (Scrolled);
      Pack_Start (Hbox, Scrolled, Expand => True, Fill => True);

      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Gtk_New (Text, Editor.Help);
      Set_Wrap_Mode (Text, Wrap_Word);
      Set_Editable (Text, False);
      Add (Scrolled, Text);

      --  The tree

      Gtk_New (Render);

      Gtk_New (Col);
      Ignore := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Action");
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", Action_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Action_Column);

      Clicked (Col);

      Gtk_New (Col);
      Ignore := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Shortcut");
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "markup", Key_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Key_Column);

      Fill_Editor (Editor);

      Ignore_Action := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Ignore_Action := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Editor);
      Hide (Editor.Grab_Label);

      Set_GUI_Running (True);

      if Run (Editor) = Gtk_Response_OK then
         Save_Editor (Editor);
      end if;

      Set_GUI_Running (False);

      Reset (Editor.Bindings.all);
      Unchecked_Free (Editor.Bindings);
      Destroy (Editor);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Edit_Keys;

   -----------------------
   -- Register_Key_Menu --
   -----------------------

   procedure Register_Key_Menu
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Register_Menu
        (Kernel, '/' & (-"Edit"),
         -"_Key Shortcuts",
         Callback => On_Edit_Keys'Access,
         Ref_Item => -"Aliases",
         Add_Before => False);
   end Register_Key_Menu;

end KeyManager_Module.GUI;
