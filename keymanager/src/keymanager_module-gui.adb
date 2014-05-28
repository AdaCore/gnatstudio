------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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
with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Button_Box;          use Gtk.Button_Box;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;  use Gtk.Cell_Renderer_Text;
with Gtk.Check_Menu_Item;     use Gtk.Check_Menu_Item;
with Gtk.Combo_Box_Text;      use Gtk.Combo_Box_Text;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main;                use Gtk.Main;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Paned;               use Gtk.Paned;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Separator;           use Gtk.Separator;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtk.Text_View;           use Gtk.Text_View;
with Gtk.Toggle_Button;       use Gtk.Toggle_Button;
with Gtk.Toolbar;             use Gtk.Toolbar;
with Gtk.Tool_Button;         use Gtk.Tool_Button;
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
with Gtkada.MDI;              use Gtkada.MDI;
with Pango.Enums;             use Pango.Enums;

with Commands.Interactive;    use Commands, Commands.Interactive;
with Generic_Views;           use Generic_Views;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Intl;                use GPS.Intl;
with GPS.Search;              use GPS.Search;
with GPS.Stock_Icons;         use GPS.Stock_Icons;
with GUI_Utils;               use GUI_Utils;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with Histories;               use Histories;

package body KeyManager_Module.GUI is
   Me : constant Trace_Handle := Create ("KEYMGR");
   use Key_Htable;

   Action_Column     : constant := 0;
   Key_Column        : constant := 1;
   Weight_Column     : constant := 2;
   Icon_Column       : constant := 3;

   Hist_Show_All_Menus : constant History_Key := "shortcuts-show-all-menus";
   Hist_Shortcuts_Only : constant History_Key := "shortcuts-only";
   Hist_Categories     : constant History_Key := "shorcuts-categories";

   type Keys_Editor_Record is new Generic_Views.View_Record with record
      View               : Gtk_Tree_View;
      Model              : Gtk_Tree_Store;
      Filter             : Gtk_Tree_Model_Filter;
      Sort               : Gtk_Tree_Model_Sort;
      Help               : Gtk_Text_Buffer;
      Remove_Button      : Gtk_Button;
      Grab_Button        : Gtk_Toggle_Button;
      Disable_Filtering  : Boolean := False;
      Themes             : Gtk_Combo_Box_Text;

      Filter_Pattern     : Search_Pattern_Access;
      --  ??? Should be freed when the view is destroyed
   end record;
   function Initialize
     (Editor : access Keys_Editor_Record'Class) return Gtk_Widget;
   overriding procedure Create_Toolbar
     (View    : not null access Keys_Editor_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access Keys_Editor_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Keys_Editor_Record;
      Pattern : in out Search_Pattern_Access);

   package Keys_Editor_Views is new Simple_Views
     (Module_Name        => "Keyshortcuts_editor",
      View_Name          => "Key Shortcuts",
      Formal_View_Record => Keys_Editor_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Group              => Group_Default,
      Areas              => Gtkada.MDI.Both,
      Default_Width      => 700,
      Default_Height     => 700,
      Commands_Category  => -"Views",
      Add_Close_Button_On_Float => True,
      MDI_Flags          =>
         All_Buttons or Float_As_Dialog or Always_Destroy_Float,
      Position           => Position_Float,
      Initialize         => Initialize);
   use Keys_Editor_Views;
   subtype Keys_Editor is Keys_Editor_Views.View_Access;

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class);
   procedure Refill_Editor (View : access GObject_Record'Class);
   --  Fill the contents of the editor
   --  The second version is suitable for gtk+ callbacks.

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class);
   procedure On_Remove_Key (Editor : access Gtk_Widget_Record'Class);
   procedure On_Reset (Editor : access Gtk_Widget_Record'Class);
   procedure On_Create (Editor : access Gtk_Widget_Record'Class);
   --  Handle the "Grab", "Remove", "Reset" and "Create" buttons

   procedure On_Grab_For_Filter (View : access GObject_Record'Class);
   --  Called when the user wants to grab a key for the filter

   function Grab_Multiple_Key
     (View : not null access Keys_Editor_Record'Class;
      For_Display : Boolean := False) return String;
   --  Grab a key binding, with support for multiple keymaps. Returns the
   --  empty string if no key could be grabbed.
   --  If For_Display is true, the returned string is suitable for displaying
   --  the shortcut to the user, but not to parse it into its components.

   function Cancel_Grab return Boolean;
   --  Exit the current nest main loop, if any

   procedure On_Load_Key_Theme (Editor : access GObject_Record'Class);
   --  Called when the user selects an alternative key theme to load

   procedure Refresh_Editor (Editor : access Keys_Editor_Record'Class);
   --  Refresh the list of key bindings in editor. Better use this one than
   --  Fill_Editor when possible, since this will preserve expanded/closed
   --  nodes.

   package Keys_Editor_Visible_Funcs is new
     Gtk.Tree_Model_Filter.Set_Visible_Func_User_Data (Keys_Editor);
   function Action_Is_Visible
     (Model : Gtk_Tree_Model;
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
      Icon   : String := "";
      Key    : String := "";
      Weight : Pango.Enums.Weight := Pango_Weight_Normal)
      return Gtk_Tree_Iter;
   --  Add a new line into the model

   type Expand_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Expand_All_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Expand all files within the current category

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Expand_All_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      K : constant Kernel_Handle := Get_Kernel (Context.Context);
      V : constant Keys_Editor := Keys_Editor_Views.Retrieve_View (K);
      Path : Gtk_Tree_Path;
   begin
      if V /= null then
         Path := Gtk_Tree_Path_New_First;
         if V.View.Row_Expanded (Path) then
            V.View.Collapse_All;
         else
            V.View.Expand_All;
         end if;
         Path_Free (Path);
      end if;
      return Commands.Success;
   end Execute;

   ---------
   -- Set --
   ---------

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String;
      Icon   : String := "";
      Key    : String := "";
      Weight : Pango.Enums.Weight := Pango_Weight_Normal)
      return Gtk_Tree_Iter
   is
      procedure Set
        (Tree, Iter : System.Address;
         Col1       : Gint; Value1 : String;
         Col2       : Gint; Value2 : String;
         Col3       : Gint; Value3 : String;
         Col4       : Gint; Value4 : Pango.Enums.Weight);
      pragma Import (C, Set, "ada_gtk_tree_store_set_ptr_ptr_ptr_weight");

      Iter : Gtk_Tree_Iter;

   begin
      Append (Model, Iter, Parent);
      Set
        (Get_Object (Model), Iter'Address,
         Col1 => Action_Column,     Value1 => Descr & ASCII.NUL,
         Col2 => Key_Column,        Value2 => Key & ASCII.NUL,
         Col3 => Icon_Column,       Value3 => Icon & ASCII.NUL,
         Col4 => Weight_Column,     Value4 => Weight);
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
      Show_All_Menus : constant Boolean :=
        Get_History (Get_History (Editor.Kernel).all, Hist_Show_All_Menus);
      Show_Categories : constant Boolean :=
        Get_History (Get_History (Editor.Kernel).all, Hist_Categories);
      Shortcuts_Only : constant Boolean :=
        Get_History (Get_History (Editor.Kernel).all, Hist_Shortcuts_Only);

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

         if Action.Category /= null then
            declare
               Name : constant String := Get (Action_Iter).Name.all;
               Key  : constant String := Lookup_Key_From_Action
                 (Get_Shortcuts (Editor.Kernel),
                  Name,
                  Use_Markup => False,
                  Is_User_Changed => User_Changed'Unchecked_Access,
                  Default => -Disabled_String);
               Show : Boolean;
            begin
               if Name (Name'First) /= '/' then
                  Show := not Shortcuts_Only or else Key /= "";
               else
                  Show := Key /= ""
                    or else (not Shortcuts_Only and then Show_All_Menus);
               end if;

               if Show then
                  if Show_Categories then
                     --  Create category node only when needed, which ensures
                     --  we do not show empty categories
                     Parent := Find_Parent (Editor.Model, Action);
                  else
                     Parent := Null_Iter;
                  end if;

                  Parent := Set
                    (Model   => Editor.Model,
                     Parent  => Parent,
                     Descr   => Name,
                     Icon    => (if Action.Stock_Id /= null then
                                      Action.Stock_Id.all
                                 else ""),
                     Key     => Key
                     & (if User_Changed then " (modified)" else ""),
                     Weight  => (if User_Changed then Pango_Weight_Bold
                                 else Pango_Weight_Normal));
               end if;
            end;
         end if;

         Next (Editor.Kernel, Action_Iter);
      end loop;

      Editor.Disable_Filtering := False;

      Refilter (Editor.Filter);
   end Fill_Editor;

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
                 (Get_Shortcuts (Ed.Kernel),
                  Action            => Get_String (Model, Iter, Action_Column),
                  Default           => -"none",
                  Is_User_Changed => User_Changed'Unchecked_Access,
                  Use_Markup        => False));

            Insert_With_Tags
              (Ed.Help, Text_Iter, ASCII.LF & (-"Menus: ") & ASCII.LF,
               Bold);
            if Action.Menus = null then
               Insert (Ed.Help, Text_Iter, -"none");
            else
               for M in Action.Menus'Range loop
                  Insert (Ed.Help, Text_Iter, Action.Menus (M).all & ASCII.LF);
               end loop;
            end if;
         end if;
      else
         Set_Sensitive (Ed.Remove_Button, False);
         Set_Sensitive (Ed.Grab_Button, False);
         Set_Text (Ed.Help, "");
      end if;

   exception
      when E : others => Trace (Me, E);
   end Add_Selection_Changed;

   -----------------------
   -- Action_Is_Visible --
   -----------------------

   function Action_Is_Visible
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : Keys_Editor) return Boolean
   is
      Row_Visible : Boolean := True;
      Child       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Action      : Action_Record_Access;
   begin
      if Data.Disable_Filtering then
         return True;
      end if;

      --  Compute the row itself should be visible (not withstanding its
      --  children.

      if Data.Filter_Pattern /= null then
         Row_Visible :=
           Data.Filter_Pattern.Start (Get_String (Model, Iter, 0)) /= No_Match
           or else
           Data.Filter_Pattern.Start (Get_String (Model, Iter, 1)) /= No_Match;

         if not Row_Visible then
            Action := Lookup_Action (Data.Kernel, Get_String (Model, Iter, 0));
            if Action /= null and then Action.Description /= null then
               Row_Visible :=
                 Data.Filter_Pattern.Start (Action.Description.all)
                 /= No_Match;
            end if;

            if Action /= null
              and then not Row_Visible
              and then Action.Menus /= null
            then
               for M in Action.Menus'Range loop
                  if Data.Filter_Pattern.Start (Action.Menus (M).all)
                    /= No_Match
                  then
                     Row_Visible := True;
                     exit;
                  end if;
               end loop;
            end if;
         end if;
      end if;

      --  If the row should be invisible, but any of its children is visible,
      --  we display it anyway.

      if not Row_Visible then
         Child := Children (Model, Iter);
         while Child /= Null_Iter loop
            if Action_Is_Visible (Model, Child, Data) then
               return True;
            end if;
            Next (Model, Child);
         end loop;
      end if;

      return Row_Visible;

   exception
      when E : others =>
         Trace (Me, E);
         return True;
   end Action_Is_Visible;

   -------------------
   -- Refill_Editor --
   -------------------

   procedure Refill_Editor (View : access GObject_Record'Class) is
   begin
      Fill_Editor (Keys_Editor (View));
   end Refill_Editor;

   --------------------
   -- Refresh_Editor --
   --------------------

   procedure Refresh_Editor (Editor : access Keys_Editor_Record'Class) is

      procedure Set
        (Tree, Iter : System.Address; Col1 : Gint; Val1 : Pango.Enums.Weight);
      pragma Import (C, Set, "ada_gtk_tree_store_set_weight");

      procedure Refresh_Iter (Iter : Gtk_Tree_Iter);
      --  Refresh for Iter and its sibling

      ------------------
      -- Refresh_Iter --
      ------------------

      procedure Refresh_Iter (Iter : Gtk_Tree_Iter) is
         It           : Gtk_Tree_Iter;
      begin
         It := Iter;
         while It /= Null_Iter loop
            if Children (Editor.Model, It) /= Null_Iter then
               Refresh_Iter (Children (Editor.Model, It));
            else
               declare
                  User_Changed : aliased Boolean;
                  Key : constant String :=
                    Lookup_Key_From_Action
                      (Get_Shortcuts (Editor.Kernel),
                       Action => Get_String (Editor.Model, It, Action_Column),
                       Default => "",
                       Use_Markup => False,
                       Is_User_Changed => User_Changed'Unchecked_Access);
                  W : Weight;
               begin
                  Set
                    (Editor.Model, It, Key_Column,
                     Key & (if User_Changed then " (modified)" else ""));

                  if User_Changed then
                     W := Pango_Weight_Bold;
                  else
                     W := Pango_Weight_Normal;
                  end if;

                  Set
                    (Get_Object (Editor.Model), It'Address, Weight_Column, W);
               end;
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

      if Main_Level > 0 then
         Main_Quit;
      end if;

      return True;  --  so that we can remove it later without an error
   end Cancel_Grab;

   -----------------------
   -- Grab_Multiple_Key --
   -----------------------

   function Grab_Multiple_Key
     (View : not null access Keys_Editor_Record'Class;
      For_Display : Boolean := False) return String
   is
      Grabbed, Tmp : String_Access;
      Key          : Gdk_Key_Type;
      Modif        : Gdk_Modifier_Type;
      Id           : Glib.Main.G_Source_Id;
   begin
      Block_Key_Shortcuts (View.Kernel);

      Key_Grab (View.Get_Toplevel, Key, Modif);

      if Key /= GDK_Escape or else Modif /= 0 then
         if For_Display then
            Grabbed := new String'
              (Gtk.Accel_Group.Accelerator_Get_Label (Key, Modif));
         else
            Grabbed := new String'(Image (Key, Modif));
         end if;
      else
         return "";
      end if;

      --  Are we grabbing multiple keymaps ?

      loop
         Id := Glib.Main.Timeout_Add (500, Cancel_Grab'Access);
         Key_Grab (View.Get_Toplevel, Key, Modif);
         Glib.Main.Remove (Id);

         exit when Key = 0 and then Modif = 0;

         if Key = GDK_Escape and then Modif = 0 then
            Free (Grabbed);
            return "";
         end if;

         Tmp := Grabbed;
         if For_Display then
            Grabbed := new String'
              (Grabbed.all & ' '
               & Gtk.Accel_Group.Accelerator_Get_Label (Key, Modif));
         else
            Grabbed := new String'(Grabbed.all & ' ' & Image (Key, Modif));
         end if;

         Free (Tmp);
      end loop;

      Unblock_Key_Shortcuts (View.Kernel);

      return K : constant String := Grabbed.all do
         Free (Grabbed);
      end return;

   exception
      when others =>
         Unblock_Key_Shortcuts (View.Kernel);
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
            declare
               Key          : constant String := Grab_Multiple_Key (Ed);
               Old_Action   : constant String := Lookup_Action_From_Key
                 (Key, Get_Shortcuts (Ed.Kernel));
               Old_Prefix   : constant String := Actions_With_Key_Prefix
                 (Key, Get_Shortcuts (Ed.Kernel));
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
                      (Get_Shortcuts (Ed.Kernel),
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
                     Set_Active (Ed.Grab_Button, False);
                     return;
                  end if;

                  Bind_Default_Key_Internal
                    (Kernel           => Ed.Kernel,
                     Table            => Get_Shortcuts (Ed.Kernel).all,
                     Action           => New_Action,
                     Key              => Key,
                     Save_In_Keys_XML => True,
                     Remove_Existing_Actions_For_Shortcut => True,
                     Remove_Existing_Shortcuts_For_Action => True,
                     Update_Menus     => True);
                  Save_Custom_Keys (Ed.Kernel);
                  Refresh_Editor (Ed);
               end if;
            end;
         end if;

         Set_Active (Ed.Grab_Button, False);
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Grab_Key;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Editor : access Gtk_Widget_Record'Class) is
      Self   : constant Keys_Editor := Keys_Editor (Editor);
      Dialog : Gtk_Dialog;
      Label  : Gtk_Label;
      Ent    : Gtk_Entry;
      W      : Gtk_Widget;
      pragma Unreferenced (W);
   begin
      Gtk_New (Dialog,
               Title  => -"Select key theme name",
               Parent => Get_Main_Window (Self.Kernel),
               Flags  => Destroy_With_Parent or Modal);

      Gtk_New (Label, -"Enter theme name:");
      Label.Set_Alignment (0.0, 0.5);
      Dialog.Get_Content_Area.Pack_Start (Label, Expand => False);

      Gtk_New (Ent);
      Dialog.Get_Content_Area.Pack_Start (Ent, Expand => False);

      W := Dialog.Add_Button (-"OK", Gtk_Response_OK);
      W := Dialog.Add_Button (-"Cancel", Gtk_Response_Cancel);
      Dialog.Set_Default_Response (Gtk_Response_Cancel);

      Dialog.Show_All;

      if Dialog.Run = Gtk_Response_OK then
         declare
            Name : constant String := Ent.Get_Text;
         begin
            Save_Keys
              (Self.Kernel, Save_All => True,
               Filename => Create_From_Dir
                 (User_Key_Theme_Directory (Self.Kernel), +Name & ".xml"));

            --  Discard all user-specific shortcuts
            Remove_Shortcuts (Self.Kernel, User_Shortcuts);
            Save_Custom_Keys (Self.Kernel);

            Self.Themes.Insert_Text (0, Text => Name);
            Self.Themes.Set_Active (0);
         end;
      end if;

      Dialog.Destroy;
   end On_Create;

   --------------
   -- On_Reset --
   --------------

   procedure On_Reset (Editor : access Gtk_Widget_Record'Class) is
      Self : constant Keys_Editor := Keys_Editor (Editor);
   begin
      if Message_Dialog
        (Dialog_Type    => Confirmation,
         Buttons        => Button_Yes or Button_No,
         Default_Button => Button_Yes,
         Title          => -"Reset custom shortcuts",
         Msg            =>
           -("This operation will remove all the custom shortcuts you have"
             & ASCII.LF
             & "added (set the filter to 'modified' to see them)."
             & ASCII.LF & ASCII.LF
             & "Remove all custom shortcuts?"))
        = Button_Yes
      then
         Remove_Shortcuts (Self.Kernel, User_Shortcuts);
         Save_Custom_Keys (Self.Kernel);
         Refresh_Editor (Self);
      end if;
   end On_Reset;

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
           (Table             => Get_Shortcuts (Ed.Kernel).all,
            Kernel            => Ed.Kernel,
            Action            => Get_String (Ed.Model, Iter, Action_Column),
            Key               => "",
            Save_In_Keys_XML  => True,
            Remove_Existing_Shortcuts_For_Action => True,
            Remove_Existing_Actions_For_Shortcut => True,
            Update_Menus      => False);
         Save_Custom_Keys (Ed.Kernel);
         Refresh_Editor (Ed);
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Remove_Key;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Keys_Editor_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Check : Gtk_Check_Menu_Item;
   begin
      Gtk_New (Check, Label => -"Shortcuts only");
      Check.Set_Tooltip_Text
        (-("If enabled, only actions with a shortcut are displayed"));
      Associate (Get_History (View.Kernel).all, Hist_Shortcuts_Only, Check);
      Menu.Append (Check);
      Check.On_Toggled (Refill_Editor'Access, View);

      Gtk_New (Check, Label => -"Show categories");
      Check.Set_Tooltip_Text (-"Whether to group actions by categories");
      Associate (Get_History (View.Kernel).all, Hist_Categories, Check);
      Menu.Append (Check);
      Check.On_Toggled (Refill_Editor'Access, View);

      Gtk_New (Check, Label => -"Show all menus");
      Check.Set_Tooltip_Text
        (-("Whether to show all menus, or only those with a shortcut"
         & ASCII.LF
         & "Historically, shortcuts used to be associated directly to menus,"
         & " but it is in fact better to associate them with the corresponding"
         & " action, which you can see by looking at the tooltip on the menu."
         & ASCII.LF
         & "This ensures the shortcut remains available even when the menu is"
         & " not visible."));
      Associate (Get_History (View.Kernel).all, Hist_Show_All_Menus, Check);
      Menu.Append (Check);
      Check.On_Toggled (Refill_Editor'Access, View);

   end Create_Menu;

   ------------------------
   -- On_Grab_For_Filter --
   ------------------------

   procedure On_Grab_For_Filter (View : access GObject_Record'Class) is
      V : constant Keys_Editor := Keys_Editor (View);
      Key : constant String := Grab_Multiple_Key (V, For_Display => True);
   begin
      V.Set_Filter (Key);
   end On_Grab_For_Filter;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Keys_Editor_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
      B : Gtk_Tool_Button;
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "keyshortcuts",
         Tooltip     => -"Filter the contents of the shortcuts list",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy
         or Has_Approximate);

      Gtk_New (B, Label => -"Grab");
      B.Set_Tooltip_Text (-"Grab a key sequence to search for");
      B.On_Clicked (On_Grab_For_Filter'Access, View);
      View.Append_Toolbar
        (Toolbar, B, Is_Filter => True, Homogeneous => False);
   end Create_Toolbar;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Keys_Editor_Record;
      Pattern : in out Search_Pattern_Access)
   is
   begin
      Free (Self.Filter_Pattern);
      Self.Filter_Pattern := Pattern;
      Self.Filter.Refilter;

      if Pattern /= null then
         Self.View.Expand_All;  --  show all results more conveniently
      end if;
   end Filter_Changed;

   -----------------------
   -- On_Load_Key_Theme --
   -----------------------

   procedure On_Load_Key_Theme (Editor : access GObject_Record'Class) is
      Self : constant Keys_Editor := Keys_Editor (Editor);
   begin
      Remove_Shortcuts (Self.Kernel, Mode => Standard_Shortcuts);
      Load_Key_Theme (Self.Kernel, Self.Themes.Get_Active_Text);
      Fill_Editor (Self);
      Set_Key_Theme (Self.Kernel, Self.Themes.Get_Active_Text);
   end On_Load_Key_Theme;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Editor : access Keys_Editor_Record'Class) return Gtk_Widget
   is
      Scrolled  : Gtk_Scrolled_Window;
      Hbox      : Gtk_Box;
      Bbox      : Gtk_Button_Box;
      Button    : Gtk_Button;
      Col       : Gtk_Tree_View_Column;
      Render    : Gtk_Cell_Renderer_Text;
      Pixbuf    : Gtk_Cell_Renderer_Pixbuf;
      Frame     : Gtk_Frame;
      Pane      : Gtk_Paned;
      Text      : Gtk_Text_View;
      Ignore    : Gint;
      Sep       : Gtk_Separator;
      Selected  : Gint := 0;
      Key_Themes : String_List_Access := List_Key_Themes (Editor.Kernel);
      Theme_Name : constant String := Get_Key_Theme (Editor.Kernel);

   begin
      Initialize_Vbox (Editor);
      Editor.Set_Name ("Key shortcuts");  --  for testsuite

      --  The model we will modify, wrapped in a filter and sort model

      Gtk_New
        (Editor.Model,
         (Action_Column     => GType_String,
          Key_Column        => GType_String,
          Weight_Column     => GType_Int,
          Icon_Column       => GType_String));

      Gtk_New (Editor.Filter, +Editor.Model);
      Keys_Editor_Visible_Funcs.Set_Visible_Func
        (Editor.Filter, Action_Is_Visible'Access, Editor);

      Gtk_New_With_Model (Editor.Sort, +Editor.Filter);

      --  A hbox: on the left, the list of actions and help, on the left some
      --  buttons to modify key shortcuts

      Gtk_New_Hbox (Hbox);
      Editor.Pack_Start (Hbox, Expand => True, Fill => True);

      Gtk_New_Vpaned (Pane);
      Hbox.Pack_Start (Pane, Expand => True, Fill => True);

      --  List of actions

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Pane.Pack1 (Scrolled, Resize => True, Shrink => True);
      Pane.Set_Position (500);

      Gtk_New (Editor.View, Editor.Sort);
      Editor.View.Set_Name ("Key shortcuts tree"); --  for testsuite
      Scrolled.Add (Editor.View);
      Widget_Callback.Object_Connect
        (Get_Selection (Editor.View), Gtk.Tree_Selection.Signal_Changed,
         Add_Selection_Changed'Access, Editor);

      --  Action buttons

      Gtk_New (Bbox, Orientation_Vertical);
      Bbox.Set_Layout (Buttonbox_Start);
      Bbox.Set_Spacing (5);
      Hbox.Pack_Start (Bbox, Expand => False, Fill => True);

      Gtk_New (Editor.Themes);
      Bbox.Add (Editor.Themes);
      Editor.Themes.Set_Tooltip_Text
        (-("Select an alternate list of shortcuts. User-overridden shortcuts"
           & " are preserved, but all others are reset and reloaded from the"
           & " new theme"));
      for K in Key_Themes'Range loop
         Editor.Themes.Append_Text (Key_Themes (K).all);
         if Key_Themes (K).all = Theme_Name then
            Selected := Gint (K - Key_Themes'First);
         end if;
      end loop;
      Editor.Themes.Set_Active (Selected);

      --  Set the callback after setting the active item.
      Editor.Themes.On_Changed (On_Load_Key_Theme'Access, Editor);
      Free (Key_Themes);

      Gtk_New (Button, -"Reset");
      Button.Set_Tooltip_Text
        (-"Remove all custom key bindings, and revert to the theme's default");
      Bbox.Add (Button);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, On_Reset'Access, Editor);

      Gtk_New (Button, -"Create");
      Button.Set_Tooltip_Text
        (-("Create a new key theme which includes the base theme plus the user"
         & " any change you did. This resets all custom changes."));
      Bbox.Add (Button);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, On_Create'Access, Editor);

      Gtk_New (Sep, Orientation_Horizontal);
      Bbox.Add (Sep);

      Gtk_New (Editor.Remove_Button, -"Remove");
      Editor.Remove_Button.Set_Tooltip_Text (-"Remove selected key binding");
      Editor.Remove_Button.Set_Sensitive (False);
      Bbox.Add (Editor.Remove_Button);
      Widget_Callback.Object_Connect
        (Editor.Remove_Button,
         Gtk.Button.Signal_Clicked, On_Remove_Key'Access, Editor);

      Gtk_New (Editor.Grab_Button, -"Modify");
      Editor.Grab_Button.Set_Tooltip_Text (-"Modify selected key binding");
      Editor.Grab_Button.Set_Sensitive (False);
      Bbox.Add (Editor.Grab_Button);
      Widget_Callback.Object_Connect
        (Editor.Grab_Button,
         Gtk.Toggle_Button.Signal_Toggled, On_Grab_Key'Access, Editor);

      --  Help on selected action

      Gtk_New (Frame);
      Pane.Pack2 (Frame, Resize => True, Shrink => True);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scrolled);

      Gtk_New (Editor.Help);
      Gtk_New (Text, Editor.Help);
      Text.Set_Wrap_Mode (Wrap_Word);
      Text.Set_Editable (False);
      Scrolled.Add (Text);

      --  The tree

      Gtk_New (Render);
      Gtk_New (Pixbuf);
      Pixbuf.Set_Alignment (Xalign => 0.0, Yalign => 0.5);

      Gtk_New (Col);
      Ignore := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Action");
      Pack_Start (Col, Pixbuf, False);
      Add_Attribute (Col, Pixbuf, "stock-id", Icon_Column);
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", Action_Column);
      Add_Attribute (Col, Render, "weight", Weight_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Action_Column);

      Clicked (Col);

      Gtk_New (Col);
      Ignore := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Shortcut");
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "text", Key_Column);
      Add_Attribute (Col, Render, "weight", Weight_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Key_Column);

      Fill_Editor (Editor);

      return Gtk_Widget (Editor.View);
   end Initialize;

   -----------------------
   -- Register_Key_Menu --
   -----------------------

   procedure Register_Key_Menu
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Keys_Editor_Views.Register_Module (Kernel);

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Show_All_Menus, False);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Shortcuts_Only, False);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Categories, True);

      Register_Action
        (Kernel, "key shortcuts expand all",
         new Expand_All_Command,
         -"Expand or collapse all nodes in the shortcuts editor",
         Stock_Id => GPS_Expand_All,
         Category => -"Key Shortcuts");
   end Register_Key_Menu;

end KeyManager_Module.GUI;
