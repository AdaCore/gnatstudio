------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                   Copyright (C) 2023-2024, AdaCore                       --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;

with GNAT.Decode_UTF8_String;

with GNATCOLL.JSON;               use GNATCOLL.JSON;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.Utils;              use GNATCOLL.Utils;
with GNATCOLL.VFS;

with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with Glib_Values_Utils;           use Glib_Values_Utils;

with Gdk.Drag_Contexts;           use Gdk.Drag_Contexts;
with Gdk.RGBA;                    use Gdk.RGBA;

with Gtk.Box;                     use Gtk.Box;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Cell_Renderer_Pixbuf;    use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Dnd;                     use Gtk.Dnd;
with Gtk.Tree_Store;              use Gtk.Tree_Store;
with Gtk.Tree_Selection;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;

with Gtkada.File_Selector;

with VSS.Regular_Expressions;     use VSS.Regular_Expressions;
with VSS.String_Vectors;
with VSS.Strings.Conversions;
with VSS.Transformers.Casing;     use VSS.Transformers.Casing;

with GPS.Dialogs;                 use GPS.Dialogs;
with GPS.Kernel.Actions;          use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.Modules.UI;       use GPS.Kernel.Modules.UI;
with GPS.Kernel.Properties;       use GPS.Kernel.Properties;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Properties;              use GPS.Properties;

with Commands.Interactive;        use Commands, Commands.Interactive;
with Default_Preferences;         use Default_Preferences;
with Filter_Panels;               use Filter_Panels;
with Language;                    use Language;
with Language.Icons;              use Language.Icons;
with GUI_Utils;                   use GUI_Utils;
with XML_Utils;                   use XML_Utils;

with GPS.Debuggers;

with DAP.Module;
with DAP.Contexts;                use DAP.Contexts;
with DAP.Modules.Preferences;
with DAP.Tools;                   use DAP.Tools;
with DAP.Utils;                   use DAP.Utils;

package body DAP.Views.Variables is

   Me : constant Trace_Handle := Create ("GPS.DAP.Variables", On);

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);

   type On_Command is new Debugger_String_Hooks_Function with null record;
   overriding function Execute
     (Self    : On_Command;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Process : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Command : String) return String;
   --  Parse and process a "tree print" or "tree display" commands

   type Tree_Display_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Display_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Tree_Undisplay_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Undisplay_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Tree_Clear_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Tree_Expression_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Expression_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Value_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Export_Variables_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Export_Variables_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Saves the contents of the view in a file

   type Set_Format_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Set_Format_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Variables_Collapse_Or_Expand_Command
     (Command : Expansion_Command_Type) is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Variables_Collapse_Or_Expand_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Print_Variable_Command is new Interactive_Command with record
      Dereference : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Display_Arguments_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Display_Arguments_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Display_Locals_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Display_Locals_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Access_Variable_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Is_Variable_Editable_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Variable_Editable_Filter;
      Context : Selection_Context) return Boolean;

   type Is_Variables_View_Focused_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Variables_View_Focused_Filter;
      Context : Selection_Context) return Boolean;

   type Variable_Single_Selection is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Variable_Single_Selection;
      Context : Selection_Context) return Boolean;
   --  True if only one row is selected.

   procedure On_Drag_Data_Received
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  DnD handler called when users drag some text in the Variables view.
   --  If this text corresponds to an entity, display it in the view.

   function Display_Value_Select_Dialog is
     new Display_Select_Dialog (Value_Format);

   -- Variables_Property_Record --

   type Variables_Property_Record is new Property_Record with record
      Items : Item_Info_Vectors.Vector;
   end record;
   overriding procedure Save
     (Self  : access Variables_Property_Record;
      Value : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Self  : in out Variables_Property_Record;
      Value : GNATCOLL.JSON.JSON_Value);
   --  Saving and loading which variables are displayed for a given executable

   function Get_Or_Create_View
     (Kernel : not null access Kernel_Handle_Record'Class;
      Client : not null DAP.Clients.DAP_Client_Access)
      return DAP_Variables_View;
   --  Get or create the variables view

   Show_Types : Boolean_Preference;

   DAP_Variables_Contextual_Group : constant Integer := 1;

   Tree_Cmd_Format : constant Regular_Expression :=
     To_Regular_Expression
       ("(tree|graph)\s+"
        & "((?:un)?display)\s+"    --  paren 1: type of command
        & "(?:"
        &   "`([^`]+)`"            --  paren 2: `command`
        &   "\s*(split)?"          --  paren 3: whether to split
        &   "|"
        &   "(\S+)"                --  paren 4: varname
        & ")",
        (Case_Insensitive => True, others => False));
   Tree_Cmd_Display : constant := 2;
   Tree_Cmd_Command : constant := 3;
   Tree_Cmd_Split   : constant := 4;
   Tree_Cmd_Varname : constant := 5;

   Set_Command : constant Regular_Expression :=
     To_Regular_Expression
       ("set\s+variable\s+(\S+)\s+:=\s+(\S+)",
        (Case_Insensitive => True, others => False));

   ------------
   -- Update --
   ------------

   procedure Update (Client : not null access DAP.Clients.DAP_Client'Class)
   is
      View : constant DAP_Variables_View := DAP_Variables_View
        (Variables_MDI_Views.Retrieve_View (Client.Kernel));
   begin
      if View /= null
        and then Get_Client (View) = Client
      then
         View.Update;
      end if;
   end Update;

   ---------------------------
   -- On_Variable_Not_Found --
   ---------------------------

   procedure On_Variable_Not_Found
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Params : Request_Parameters)
   is
      View : constant DAP_Variables_View := DAP_Variables_View
        (Variables_MDI_Views.Retrieve_View (Client.Kernel));
   begin
      if View /= null
        and then View.Get_Client = Client
      then
         if Params.Path = Null_Gtk_Tree_Path
           and then Params.Item.Info.Id /= Unknown_Id
         then
            --  we did not found the root variable, just add a row that it
            --  can be deleted from GUI
            View.Tree.Add_Row
              (Item   => Params.Item.Info.all,
               Cursor => Variables_References_Trees.No_Element,
               Parent => Null_Iter);
         end if;
      end if;
   end On_Variable_Not_Found;

   ------------------------
   -- On_Variable_Loaded --
   ------------------------

   procedure On_Variable_Loaded
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Params : Request_Parameters;
      C      : Variables_References_Trees.Cursor)
   is
      View   : constant DAP_Variables_View := DAP_Variables_View
        (Variables_MDI_Views.Retrieve_View (Client.Kernel));
      Parent : Gtk_Tree_Iter := Null_Iter;
   begin
      if View /= null
        and then Get_Client (View) = Client
      then
         if (Params.Path = Null_Gtk_Tree_Path
             and then Params.Item.Info.Id /= Unknown_Id)
           or else (Params.Path /= Null_Gtk_Tree_Path
                    and then Params.Item.Info.Id = Unknown_Id)
         --  do not add child items when we are updating the view
         then
            if Params.Path /= Null_Gtk_Tree_Path then
               Parent := View.Tree.Model.Get_Iter (Params.Path);
            end if;

            --  add it to the table
            View.Tree.Add_Row (Params.Item.Info.all, C, Parent);
         end if;

         if Params.Position /= 0 then
            --  Position /= 0 when we updating the whole view,
            --  continue updating.
            View.Update (Params.Position);

         else
            View.Restore_Expansion;
         end if;
      end if;
   end On_Variable_Loaded;

   ------------------------
   -- On_Children_Loaded --
   ------------------------

   procedure On_Children_Loaded
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Params : Request_Parameters;
      C      : Variables_References_Trees.Cursor)
   is
      View    : constant DAP_Variables_View := DAP_Variables_View
        (Variables_MDI_Views.Retrieve_View (Client.Kernel));
      Current : Variables_References_Trees.Cursor := C;
      Parent  : Gtk_Tree_Iter := Null_Iter;
      Dummy   : Boolean;
   begin
      if View /= null
        and then Get_Client (View) = Client
      then
         if Params.Path /= Null_Gtk_Tree_Path then
            Parent := View.Tree.Model.Get_Iter (Params.Path);
         end if;

         if Params.Item.Info.Id /= Unknown_Id
         --  add the item when it is root item
           or else Parent /= Null_Iter
         --  or we are adding as a nested element
         then
            if Parent /= Null_Iter then
               View.Tree.Remove_Dummy_Child (Parent);
            end if;

            Current := First_Child (Current);
            while Current /= Variables_References_Trees.No_Element loop
               View.Tree.Add_Row (Params.Item.Info.all, Current, Parent);
               Next_Sibling (Current);
            end loop;
            Dummy := View.Tree.Expand_Row (Params.Path, False);
         end if;

         if Params.Position /= 0 then
            --  Position /= 0 when we updating the whole view,
            --  continue updating.
            View.Update (Params.Position);

         else
            View.Restore_Expansion;
         end if;
      end if;
   end On_Children_Loaded;

   -----------------------
   -- Restore_Expansion --
   -----------------------

   procedure Restore_Expansion
     (Self : access DAP_Variables_View_Record'Class)
   is
      use type Expansions.Expansion_Status;
      Collapse : Boolean := Self.Collapse_All_First;
   begin
      if Self.Expansion = Expansions.No_Expansion then
         --  Nothing to restore
         return;
      end if;

      --  Set ot false to prevent collapsing when calling next time
      Self.Collapse_All_First := False;

      --  Inc calling counter that will be 1 for the first call or when we get
      --  a DAP response and more than 1 for recursion when a variable
      --  is already loaded and we do not need a DAP request to get it.
      Self.Expansion_Restoring_Count := Self.Expansion_Restoring_Count + 1;

      if Self.Expansion_Restoring_Count = 1 then
         --  Enter only when called at first, to prevent recursion that may
         --  occur when a variable that we expand is already loaded.

         while Self.Expansion /= Expansions.No_Expansion
           and then Self.Expansion_Restoring_Count > 0
         loop
            Trace (Me, "Restore_Expansion");
            --  Call expansion restoring until we have status to restore
            --  and so many times as recursion counted.
            Expansions.Set_Expansion_Status_Stop_On_Dummy
              (Self.Tree, Self.Expansion, Collapse);

            Collapse := False;
            Self.Expansion_Restoring_Count :=
              Self.Expansion_Restoring_Count - 1;
         end loop;

         --  if we stop loop by Self.Expansion = Expansions.No_Expansion
         Self.Expansion_Restoring_Count := 0;
      end if;
   end Restore_Expansion;

   ---------------------
   -- On_Variable_Set --
   ---------------------

   procedure On_Variable_Set
     (Client   : not null access DAP.Clients.DAP_Client'Class;
      Params   : Request_Parameters;
      Variable : DAP.Tools.Variable)
   is
      View    : constant DAP_Variables_View := DAP_Variables_View
        (Variables_MDI_Views.Retrieve_View (Client.Kernel));
      Iter    : Gtk_Tree_Iter;

   begin
      if View /= null
        and then Get_Client (View) = Client
      then
         if Params.Set_Path = Null_Gtk_Tree_Path then
            Iter := GUI_Utils.Find_Node
              (Model     => View.Tree.Model,
               Name      => Ada.Characters.Handling.To_Lower
                 (VSS.Strings.Conversions.To_UTF_8_String
                      (Params.Name)),
               Column    => Column_Full_Name,
               Recursive => False);

         else
            --  The user has edited the row with the given path: use
            --  directly the path instead of searching for the
            --  variable's row

            Iter := View.Tree.Model.Get_Iter (Params.Set_Path);
         end if;

         if Iter /= Null_Iter then
            Set_And_Clear
              (View.Tree.Model,
               Iter    => Iter,
               Columns => (Column_Value, Column_Value_Fg),
               Values  =>
                 (1 => As_String (To_UTF8 (Variable.value)),
                  2 => As_String (To_String (Numbers_Style.Get_Pref_Fg))));
         end if;
      end if;
   end On_Variable_Set;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (Self : not null access DAP_Variables_View_Record) is
   begin
      Self.Clear;
      Self.Old_Scopes.Clear;
   end On_Process_Terminated;

   -----------------------
   -- On_Status_Changed --
   -----------------------

   overriding procedure On_Status_Changed
     (Self   : not null access DAP_Variables_View_Record;
      Status : GPS.Debuggers.Debugger_State)
   is
      use GPS.Debuggers;
      use type Expansions.Expansion_Status;
   begin
      if Status = Debug_Busy then
         Self.Old_Scopes := Get_Client (Self).Get_Variables.Get_Scopes;

         if not Self.Tree.Items.Is_Empty
           and then Self.Expansion = Expansions.No_Expansion
         then
            --  Store expansion if not done yet
            Trace (Me, "Store expansion");
            Expansions.Get_Expansion_Status (Self.Tree, Self.Expansion);
            Self.Collapse_All_First := True;
         end if;

      elsif Status = Debug_None then
         Self.Expansion := Expansions.No_Expansion;
         Self.Old_Scopes.Clear;
      end if;
   end On_Status_Changed;

   ---------------------------
   -- On_Drag_Data_Received --
   ---------------------------

   procedure On_Drag_Data_Received
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      Dnd_Context : constant Drag_Context :=
        Drag_Context (Get_Object (Nth (Args, 1)));

   begin
      --  Do nothing when the DnD comes from the Variables view

      if Get_Source_Widget (Dnd_Context) = Object then
         return;
      end if;

      --  Try to execute the "debug tree display variable" action with the
      --  current context: the text being dragged is already present in the
      --  current context since the user needs to select it in order to drag
      --  it.

      declare
         Success : Boolean;
      begin
         Success := Execute_Action
           (Kernel,
            Action  => "debug tree display variable");
         Gtk.Dnd.Finish
              (Dnd_Context,
               Success => Success,
               Del     => False);
      end;
   end On_Drag_Data_Received;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if GPS.Kernel.Contexts.Has_Entity_Name_Information (Context) then
         return True;
      else
         return False;
      end if;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Variable_Single_Selection;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Get_Kernel (Context),
           Visible_Only => True);

      Res  : Boolean := False;
   begin
      if View /= null then
         declare
            Selection : constant Gtk.Tree_Selection.Gtk_Tree_Selection :=
              Get_Selection (View.Tree);
         begin
            Res := Selection.Count_Selected_Rows = 1;
         end;
      end if;

      return Res;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Variables_View_Focused_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      use Gtkada.MDI;

      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Get_Kernel (Context),
           Visible_Only => False);
   begin
      if View /= null then
         declare
            Focus_Child : constant MDI_Child :=
              Get_Focus_Child (Get_MDI (View.Kernel));

            View_Child  : constant MDI_Child :=
              Variables_MDI_Views.Child_From_View (View);
         begin
            return Focus_Child = View_Child;
         end;
      end if;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Variable_Editable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);

      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Get_Kernel (Context),
           Visible_Only => True);
   begin
      if View /= null
        and then GPS.Kernel.Contexts.Has_Debugging_Variable (Context)
        and then not Get_Variable (Context).Is_Command
      then
         declare
            Name : constant String :=
              Get_Variable_Name (Context, Dereference => False);
         begin
            if Name /= "" then
               return not Get_Item_Info (View.Tree, Name).Is_No_Item;
            end if;
         end;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ----------------
   -- Is_Changed --
   ----------------

   function Is_Changed
     (Self   : access DAP_Variables_View_Record'Class;
      Cursor : Variables_References_Trees.Cursor)
      return Boolean
   is
      C     : Variables_References_Trees.Cursor := Self.Old_Scopes.Root;
      Found : Boolean;
   begin
      Find_Name_Or_Parent (Full_Name (Cursor), C, Found);

      return Found
        and then Element (C).Data.value /= Element (Cursor).Data.value;
   end Is_Changed;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self   : not null access Variables_Tree_View_Record;
      Iter   : Gtk_Tree_Iter) return Boolean is
   begin
      return Self.Pattern = null
        or else Self.Pattern.Start
          (Self.Model.Get_String
             (Iter, Column_Name)) /= GPS.Search.No_Match
          or else Self.Pattern.Start
            (Self.Model.Get_String
               (Iter, Column_Value)) /= GPS.Search.No_Match
            or else Self.Pattern.Start
              (Self.Model.Get_String
                 (Iter, Column_Type)) /= GPS.Search.No_Match;
   end Is_Visible;

   ------------------
   -- Add_Children --
   ------------------

   overriding procedure Add_Children
     (Self       : not null access Variables_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter)
   is
      Full_Name : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String
          (Get_String (Self.Model, Store_Iter, Column_Full_Name));
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      C         : Item_Info_Vectors.Cursor;
      Found     : Boolean;
   begin
      --  Check whether we already have this item in the list.
      --  We expect this list to be relatively short (less than 1000 elements
      --  in general), so no need for a more elaborate data structure.

      Path := Self.Model.Get_Path (Store_Iter);
      for Item of Self.Items loop
         if Item.Get_Full_Name = Full_Name then
            DAP_Variables_View (Self.View).Update (Item, 0, Path, True);
            return;
         end if;
      end loop;

      Self.Find_Best_Info (Full_Name, C, Found);

      declare
         Item : constant DAP.Modules.Variables.Items.Item_Info'Class :=
           DAP.Modules.Variables.Items.Create
             (Variable => Full_Name,
              Format   =>
                (if Found
                 then Element (C).Format
                 else Default_Format));
      begin
         Self.Items.Append (Item);
         Trace (Me, "Add_Children->Update");
         DAP_Variables_View (Self.View).Update (Item, 0, Path, True);
      end;
   end Add_Children;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row
     (Self   : not null access Variables_Tree_View_Record'Class;
      Item   : Item_Info'Class;
      Cursor : Variables_References_Trees.Cursor;
      Parent : Gtk_Tree_Iter)
   is
      Var    : Variable;
      Row    : Gtk_Tree_Iter;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      C      : Variables_References_Trees.Cursor;
      Dummy  : Boolean;
      Fg     : constant String := To_String (Default_Style.Get_Pref_Fg);
      Var_Id : Integer := -1;

      Item_Full_Name : constant VSS.Strings.Virtual_String :=
        To_Lowercase.Transform (Full_Name (Cursor));

      function Display_Type_Name return String with Inline;
      function Display_Name return String with Inline;
      function Display_Value return String with Inline;
      --  Return the display name or type name or value

      function Validate_UTF_8 (S : String) return String;
      --  This function cuts S up to it's last valid UTF8 symbol

      ------------------
      -- Display_Name --
      ------------------

      function Display_Name return String
      is
         function Wrap (Name : String) return String;
         function Wrap (Name : String) return String is
         begin
            if Name = "" then
               return "";
            else
               return "<b>" & XML_Utils.Protect (Name) & "</b>"
                 & Item.Get_Format;
            end if;
         end Wrap;

      begin
         if Cursor = Variables_References_Trees.No_Element
           or else Cursor.Is_Root
         then
            return Wrap (Item.Get_Name);

         else
            return Wrap (To_UTF8 (Var.name));
         end if;
      end Display_Name;

      -----------------------
      -- Display_Type_Name --
      -----------------------

      function Display_Type_Name return String is
      begin
         if Cursor = Variables_References_Trees.No_Element
           or else Cursor.Is_Root
         then
            return "";
         else
            return XML_Utils.Protect (To_UTF8 (Var.a_type));
         end if;
      end Display_Type_Name;

      --------------------
      -- Validate_UTF_8 --
      --------------------

      function Validate_UTF_8 (S : String) return String
      is
         Ptr : Natural := S'First;
      begin
         begin
            while Ptr <= S'Last loop
               GNAT.Decode_UTF8_String.Next_Wide_Wide_Character (S, Ptr);
            end loop;

         exception
            when Constraint_Error =>
               null;
         end;

         return S (S'First .. Ptr - 1);
      end Validate_UTF_8;

      -------------------
      -- Display_Value --
      -------------------

      function Display_Value return String is
      begin
         if Cursor = Variables_References_Trees.No_Element
           or else Cursor.Is_Root
         then
            return "";

         else
            return XML_Utils.Protect
              (Validate_UTF_8 (To_UTF8 (Var.value)));
         end if;
      end Display_Value;

   begin
      if Cursor /= Variables_References_Trees.No_Element
        and then not Is_Root (Cursor)
      then
         Var    := Element (Cursor).Data;
         Var_Id := Var.variablesReference;
      end if;

      Trace
        (Me, "Add row:" & To_UTF8
           ((if Var.name.Is_Empty
            then Item.Get_Full_Name
            else Var.name)) &
           Var_Id'Img &
           " " & Item.Get_Special_Kind'Img);

      if Parent /= Null_Iter then
         Self.Remove_Dummy_Child (Parent);

      elsif Var_Id /= -1 then
         if To_Lowercase.Transform (Item.Get_Name) /= Item_Full_Name then
            Self.Set_Item_Full_Name (Item, Item_Full_Name);
         end if;
      end if;

      Self.Model.Append (Iter => Row, Parent => Parent);
      Set_And_Clear
        (Self.Model,
         Iter   => Row,
         Values =>
           (Column_Name         => As_String (Display_Name),
            Column_Value        => As_String (Display_Value),
            Column_Type         => As_String (Display_Type_Name),
            Column_Icon         => As_String
              (if Parent = Null_Iter
               then Stock_From_Category
                 (Is_Declaration => False,
                  Visibility     => Language.Visibility_Public,
                  Category       => Language.Cat_Function)
               else ""),
            Column_Id           => As_Int (Gint (Item.Id)),
            Column_Name_Fg      => As_String (Fg),
            Column_Value_Fg     => As_String
              (if DAP_Variables_View (Self.View).Is_Changed (Cursor)
               then To_String (Numbers_Style.Get_Pref_Fg)
               else Fg),
            Column_Type_Fg      => As_String
              (To_String (Types_Style.Get_Pref_Fg)),
            Column_Full_Name    => As_String
              (VSS.Strings.Conversions.To_UTF_8_String (Item_Full_Name))));

      if Cursor /= Variables_References_Trees.No_Element then
         if Item.Get_Special_Kind /= Non_Specified then
            pragma Assert (Cursor.Is_Root);

            --  Fill `special` (arguments/locals) node
            C := Cursor.First_Child;
            while C.Has_Element loop
               if Element (C).Kind = Item.Get_Special_Kind then
                  Self.Add_Row
                    (Item => No_Item, Cursor => C, Parent => Row);
               end if;
               C.Next_Sibling;
            end loop;

            --  Expand `arguments` node
            Path  := Self.Get_Sortable_Path_For_Store_Iter (Row);
            Dummy := Self.Expand_Row (Path, False);
            Path_Free (Path);

         elsif Var.variablesReference > 0 then
            Self.Set_Might_Have_Children (Row);
         end if;

      else
         Trace (Me, "Entity is empty");
      end if;
   end Add_Row;

   --------------------------
   -- Item_From_Store_Iter --
   --------------------------

   function Item_From_Store_Iter
     (Self       : not null access Variables_Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter)
      return Item_Info'Class
   is
      Id       : Item_ID;
      Parent   : Gtk_Tree_Iter;
      Cur_Iter : Gtk_Tree_Iter := Store_Iter;
   begin
      while Cur_Iter /= Null_Iter loop
         Parent := Self.Model.Parent (Cur_Iter);
         if Parent = Null_Iter then
            Id := Item_ID (Self.Model.Get_Int (Cur_Iter, Column_Id));

            return Self.Find_Info (Id);
         end if;

         Cur_Iter := Parent;
      end loop;

      return No_Item;
   end Item_From_Store_Iter;

   ---------------------------
   -- Item_From_Filter_Iter --
   ---------------------------

   function Item_From_Filter_Iter
     (Self        : not null access Variables_Tree_View_Record'Class;
      Filter_Iter : in out Gtk_Tree_Iter)
      return Item_Info'Class
   is
      Store_Iter : Gtk_Tree_Iter;
      M          : Gtk_Tree_Model;
   begin
      if Filter_Iter = Null_Iter then
         Self.Get_First_Selected (M, Filter_Iter);
         Store_Iter := Self.Convert_To_Store_Iter (Filter_Iter);
      else
         Store_Iter := Self.Convert_To_Store_Iter (Filter_Iter);
      end if;

      return Item_From_Store_Iter (Self, Store_Iter);
   end Item_From_Filter_Iter;

   ---------------
   -- Find_Info --
   ---------------

   function Find_Info
     (Self : not null access Variables_Tree_View_Record'Class;
      Id   : Item_ID)
      return Item_Info'Class is
   begin
      for Item of Self.Items loop
         if Item.Id = Id then
            return Item;
         end if;
      end loop;

      return No_Item;
   end Find_Info;

   ------------------------
   -- Set_Item_Full_Name --
   ------------------------

   procedure Set_Item_Full_Name
     (Self : not null access Variables_Tree_View_Record'Class;
      Item : Item_Info'Class;
      Name : VSS.Strings.Virtual_String) is
   begin
      Trace (Me, "Set_Item_Full_Name " &
               VSS.Strings.Conversions.To_UTF_8_String (Item.Get_Name)
             & " " & VSS.Strings.Conversions.To_UTF_8_String (Name));

      for Old of Self.Items loop
         if Virtual_String'(Old.Get_Name) = Virtual_String'(Item.Get_Name) then
            Old.Set_Full_Name (Name);
            exit;
         end if;
      end loop;
   end Set_Item_Full_Name;

   ---------------
   -- On_Edited --
   ---------------

   overriding procedure On_Edited
     (Self        : not null access Variables_Tree_View_Record;
      Store_Iter  : Gtk_Tree_Iter;
      View_Column : Edited_Column_Id;
      Text        : String)
   is
      use type DAP.Clients.DAP_Client_Access;
   begin
      if Store_Iter /= Null_Iter
        and then View_Column = Column_Value
      then
         declare
            It : constant Item_Info'Class :=
              Item_From_Store_Iter (Self, Store_Iter);
         begin
            if Self.View /= null
              and then Get_Client (Self.View) /= null
            then
               if It.Get_Full_Name /= Empty_Virtual_String then
                  DAP_Variables_View (Self.View).Set_Variable_Value
                    (Full_Name => Get_Full_Name (It),
                     Value     => Text,
                     Path      => Self.Model.Get_Path (Store_Iter));
               end if;
            end if;
         end;
      end if;
   end On_Edited;

   ------------------------
   -- Set_Variable_Value --
   ------------------------

   procedure Set_Variable_Value
     (Self      : access DAP_Variables_View_Record'Class;
      Full_Name : VSS.Strings.Virtual_String;
      Value     : String;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      use type DAP.Clients.DAP_Client_Access;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
      Cursor : Item_Info_Vectors.Cursor;
      Found  : Boolean;
   begin
      if Client = null then
         Path_Free (Path);
         return;
      end if;

      Self.Tree.Find_Best_Info (Full_Name, Cursor, Found);

      if Found then
         declare
            Holder : DAP.Modules.Variables.Items.Item_Holder;
         begin
            Set (Holder, Element (Cursor));

            Client.Get_Variables.Set_Variable
              ((Kind     => Set_Variable,
                Item     => Holder,
                Children => False,
                Name     => Full_Name,
                Value    => VSS.Strings.Conversions.To_Virtual_String (Value),
                Set_Path => Path));
         end;
      else
         Path_Free (Path);
      end if;
   end Set_Variable_Value;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self   : access DAP_Variables_View_Record'Class) return Gtk_Widget
   is
      Scrolled : Gtk_Scrolled_Window;
      Col      : Gtk_Tree_View_Column;
      Dummy    : Gint;
      Text     : Gtk_Cell_Renderer_Text;
      Pixbuf   : Gtk_Cell_Renderer_Pixbuf;
      Pref     : Preferences_Hooks_Function_Access;

   begin
      Gtk.Box.Initialize_Vbox (Self);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scrolled, Expand => True, Fill => True);

      Self.Tree := new Variables_Tree_View_Record;
      Self.Tree.View := Views.View_Access (Self);
      Initialize
        (Self.Tree,
         Column_Types     => (Column_Name      => GType_String,
                              Column_Value     => GType_String,
                              Column_Type      => GType_String,
                              Column_Icon      => GType_String,
                              Column_Id        => GType_Int,
                              Column_Name_Fg   => GType_String,
                              Column_Value_Fg  => GType_String,
                              Column_Type_Fg   => GType_String,
                              Column_Full_Name => GType_String),
         Capability_Type  => Filtered,
         Set_Visible_Func => True);
      Set_Name (Self.Tree, "Variables Tree");  --  For testsuite
      Self.Tree.Set_Search_Column (Column_Full_Name);
      Self.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Set_Font_And_Colors (Self.Tree, Fixed_Font => True);

      Scrolled.Add (Self.Tree);

      Self.Tree.Set_Headers_Visible (True);
      Self.Tree.Set_Enable_Search (True);
      Self.Tree.Set_Grid_Lines (Grid_Lines_Vertical);
      Setup_Contextual_Menu (Self.Kernel, Self.Tree);

      --  Create the column that displays the variables' icons

      Gtk_New (Text);
      Gtk_New (Col);
      Col.Set_Resizable (True);
      Col.Set_Reorderable (True);

      Dummy := Self.Tree.Append_Column (Col);

      Gtk_New (Pixbuf);
      Col.Pack_Start (Pixbuf, False);
      Col.Add_Attribute (Pixbuf, "icon-name", Column_Icon);

      Col.Pack_Start (Text, Expand => True);
      Col.Add_Attribute (Text, "markup", Column_Name);
      Col.Add_Attribute (Text, "foreground", Column_Name_Fg);
      Col.Set_Title ("Name");

      --  Create the column that displays the variables' values

      Gtk_New (Col);
      Col.Set_Resizable (True);
      Col.Set_Reorderable (True);

      Dummy := Self.Tree.Append_Column (Col);

      Gtk_New (Text);
      Self.Tree.Text := Text;
      Col.Pack_Start (Text, Expand => True);
      Col.Add_Attribute (Text, "markup", Column_Value);
      Col.Add_Attribute (Text, "foreground", Column_Value_Fg);
      Col.Set_Title ("Value");

      --  Create the column that displays the variables' type

      Gtk_New (Self.Tree.Types_Column);
      Self.Tree.Types_Column.Set_Resizable (True);
      Self.Tree.Types_Column.Set_Reorderable (True);
      Dummy := Self.Tree.Append_Column (Self.Tree.Types_Column);

      Gtk_New (Text);
      Self.Tree.Types_Column.Pack_Start (Text, Expand => False);
      Self.Tree.Types_Column.Add_Attribute (Text, "markup", Column_Type);
      Self.Tree.Types_Column.Add_Attribute
        (Text, "foreground", Column_Type_Fg);
      Self.Tree.Types_Column.Set_Title ("Type");

      Pref := new On_Pref_Changed;
      Pref.Execute (Self.Kernel, null);
      Preferences_Changed_Hook.Add (Pref, Watch => Self);

      Dest_Set
        (Widget  => Self.Tree,
         Flags   => Dest_Default_All,
         Actions => Action_Copy);
      Self.Tree.Drag_Dest_Add_Text_Targets;
      Kernel_Callback.Connect
        (Self.Tree,
         Signal_Drag_Data_Received,
         On_Drag_Data_Received'Access,
         Self.Kernel);

      Self.Show_All;

      return Gtk_Widget (Self.Tree);
   end Initialize;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Variables_MDI_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context
   is
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.View_From_Child (Self);

      Context     : Selection_Context;
      Filter_Iter : Gtk_Tree_Iter;
   begin
      Context := GPS_MDI_Child_Record (Self.all).Build_Context (Event);

      if Event /= null then
         Filter_Iter := Find_Iter_For_Event (View.Tree, Event);
      else
         Filter_Iter := Null_Iter;
      end if;

      declare
         It : constant Item_Info'Class := Item_From_Filter_Iter
           (View.Tree, Filter_Iter => Filter_Iter);
      begin
         if Event /= null then
            View.Tree.Get_Selection.Unselect_All;
            View.Tree.Get_Selection.Select_Iter (Filter_Iter);
         end if;

         if It.Get_Full_Name /= "" then
               Store_Variable (Context, It.Get_Full_Name, It);
         end if;
      end;

      return Context;
   end Build_Context;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : not null access DAP_Variables_View_Record'Class) is
   begin
      Self.Tree.Items.Clear;
      Self.Tree.Model.Clear;
   end Clear;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access DAP_Variables_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu (Menu, View.Kernel, Show_Types);
   end Create_Menu;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (Self    : not null access DAP_Variables_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      Self.Build_Filter
        (Toolbar,
         Hist_Prefix => "debugger-variables",
         Tooltip     => "Filter the contents of the Variables view",
         Placeholder => "filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy);
   end Create_Toolbar;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access DAP_Variables_View_Record;
      Pattern : in out Search_Pattern_Access) is
   begin
      GPS.Search.Free (Self.Tree.Pattern);
      Self.Tree.Pattern := Pattern;
      Self.Tree.Refilter;  --  Recompute visibility of rows
   end Filter_Changed;

   -------------------------
   -- On_Location_Changed --
   -------------------------

   overriding procedure On_Location_Changed
     (Self : not null access DAP_Variables_View_Record) is
   begin
      Self.Update;
   end On_Location_Changed;

   ---------------
   -- On_Attach --
   ---------------

   overriding procedure On_Attach
     (Self   : not null access DAP_Variables_View_Record;
      Client : not null access DAP.Clients.DAP_Client'Class)
   is
      Found    : Boolean;
      Property : Variables_Property_Record;
   begin
      if Client /= null
        and then DAP.Modules.Preferences.Preserve_State_On_Exit.Get_Pref
      then
         Get_Property
           (Property,
            Client.Get_Executable,
            Name  => "dap_debugger_variables",
            Found => Found);

         if Found then
            Self.Tree.Ids   := DAP.Modules.Variables.Items.Unknown_Id;
            Self.Tree.Items := Property.Items;

            for It of Self.Tree.Items loop
               Self.Tree.Ids := Self.Tree.Ids + 1;
               It.Id := Self.Tree.Ids;
            end loop;
         end if;
      end if;
   end On_Attach;

   ---------------
   -- On_Detach --
   ---------------

   overriding procedure On_Detach
     (Self   : not null access DAP_Variables_View_Record;
      Client : not null access DAP.Clients.DAP_Client'Class)
   is
      Store : Item_Info_Vectors.Vector;
   begin
      if Client /= null
        and then DAP.Modules.Preferences.Preserve_State_On_Exit.Get_Pref
      then
         for Item of Self.Tree.Items loop
            if Item.Id /= Unknown_Id then
               Store.Append (Item);
            end if;
         end loop;

         Set_Property
           (Kernel     => Self.Kernel,
            File       => Client.Get_Executable,
            Name       => "dap_debugger_variables",
            Property   => new Variables_Property_Record'(Items => Store),
            Persistent => True);
      end if;

      Self.Clear;
   end On_Detach;

   -------------
   -- Display --
   -------------

   procedure Display
     (Self : access DAP_Variables_View_Record'Class;
      Name : String)
   is
      Item : DAP.Modules.Variables.Items.Item_Info'Class :=
        DAP.Modules.Variables.Items.Create
          (Variable => VSS.Strings.Conversions.To_Virtual_String (Name));
   begin
      Trace (Me, "Display:" & Name);
      Self.Display (Item);
   end Display;

   -------------
   -- Display --
   -------------

   procedure Display
     (Self : access DAP_Variables_View_Record'Class;
      Item : in out Item_Info'Class) is
   begin
      Self.Tree.Ids := Self.Tree.Ids + 1;
      Item.Id       := Self.Tree.Ids;

      Self.Tree.Items.Append (Item);
      Self.Update (Item, 0, Null_Gtk_Tree_Path);
   end Display;

   ---------------
   -- Undisplay --
   ---------------

   procedure Undisplay
     (Self : access DAP_Variables_View_Record'Class;
      Item : Item_Info'Class) is
   begin
      if Item.Get_Special_Kind /= Non_Specified then
         declare
            Curs : Item_Info_Vectors.Cursor;
         begin
            Curs := Self.Tree.Items.First;
            while Item_Info_Vectors.Has_Element (Curs) loop
               if Item_Info_Vectors.Element (Curs).Get_Special_Kind =
                 Item.Get_Special_Kind
               then
                  Self.Tree.Items.Delete (Curs);
                  exit;
               end if;
               Item_Info_Vectors.Next (Curs);
            end loop;

            Self.Update;
         end;
      else
         Self.Undisplay (Get_Full_Name (Item));
      end if;
   end Undisplay;

   ---------------
   -- Undisplay --
   ---------------

   procedure Undisplay
     (Self : access DAP_Variables_View_Record'Class;
      Name : Virtual_String)
   is
      Curs : Item_Info_Vectors.Cursor;
   begin
      if Name.Is_Empty then
         return;
      end if;

      Curs := Self.Tree.Items.First;
      while Item_Info_Vectors.Has_Element (Curs) loop
         if Is_Same_Name (Item_Info_Vectors.Element (Curs), Name) then
            Self.Tree.Items.Delete (Curs);
            exit;
         end if;
         Item_Info_Vectors.Next (Curs);
      end loop;

      Self.Update;
   end Undisplay;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Self : not null access DAP_Variables_View_Record)
   is
      use type DAP.Clients.DAP_Client_Access;
      use type Expansions.Expansion_Status;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);

   begin
      Trace (Me, "Update view");

      if Client /= null
        and then Client.Get_Variables /= null
      then
         if not Self.Tree.Items.Is_Empty
           and then Self.Expansion = Expansions.No_Expansion
         then
            --  Store expansion if not done yet
            Trace (Me, "Store expansion");
            Expansions.Get_Expansion_Status (Self.Tree, Self.Expansion);
            Self.Collapse_All_First := True;
         end if;

         Client.Get_Variables.Clear;
      end if;

      if Client = null
        or else not Client.Is_Stopped
      then
         Self.Tree.Model.Clear;
         for Item of Self.Tree.Items loop
            if Item.Id /= Unknown_Id then
               Self.Tree.Add_Row
                 (Item, Variables_References_Trees.No_Element, Null_Iter);
            end if;
         end loop;

      elsif not Self.Tree.Items.Is_Empty then
         Self.Tree.Types_Column.Set_Visible (Show_Types.Get_Pref);
         Self.Tree.Model.Clear;

         Self.Update (0);

      else
         Self.Tree.Model.Clear;
      end if;
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self     : access DAP_Variables_View_Record'Class;
      Position : Natural) is
   begin
      --  we done with the current variable but we updateing the view,
      --  so start with the next one

      for Pos in Position + 1 .. Self.Tree.Items.Last_Index loop
         if Self.Tree.Items (Pos).Auto_Refresh then
            Self.Update
              (Self.Tree.Items (Pos),
               Pos,
               Null_Gtk_Tree_Path,
               Self.Tree.Items (Pos).Id = Unknown_Id);
            return;
         end if;
      end loop;

      Self.Restore_Expansion;
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self     : access DAP_Variables_View_Record'Class;
      Item     : Item_Info'Class;
      Position : Natural;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Children : Boolean := False)
   is
      use type DAP.Clients.DAP_Client_Access;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
   begin
      if Client = null then
         if Path /= Null_Gtk_Tree_Path then
            Path_Free (Path);
         end if;

         Self.Tree.Model.Clear;
         return;
      end if;

      if Client.Is_Stopped then
         declare
            Holder : DAP.Modules.Variables.Items.Item_Holder;
         begin
            Set (Holder, Item);
            declare
               Params : Request_Parameters :=
                 (Kind     => View,
                  Item     => Holder,
                  Children => Children,
                  Position => Position,
                  Path     => Path);
            begin
               Get_Client (Self).Get_Variables.Get_Variable (Params);
               --  Callback (Variable_Loaded, Children_Loaded) will be called
               --  when the variable is loaded.
            end;
         end;

      else
         Self.Tree.Add_Row
           (Item, Variables_References_Trees.No_Element,
            Self.Tree.Model.Get_Iter (Path));

         if Path /= Null_Gtk_Tree_Path then
            Path_Free (Path);
         end if;
      end if;
   end Update;

   ------------------------
   -- Get_Or_Create_View --
   ------------------------

   function Get_Or_Create_View
     (Kernel : not null access Kernel_Handle_Record'Class;
      Client : not null DAP.Clients.DAP_Client_Access)
      return DAP_Variables_View
   is
      View : DAP_Variables_View;
   begin
      View := DAP_Variables_View
        (Variables_MDI_Views.Retrieve_View (Kernel));

      if View = null then
         Variables_Views.Attach_To_View
           (Client, Kernel, Create_If_Necessary => True);

         View := DAP_Variables_View
           (Variables_MDI_Views.Retrieve_View (Kernel));
      end if;

      return View;
   end Get_Or_Create_View;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Kernel,
           Visible_Only => True);
   begin
      if View /= null then
         Set_Font_And_Colors (View.Tree, Fixed_Font => True, Pref => Pref);
         if Pref = null
           or else Pref = Preference (Show_Types)
         then
            View.Update;
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : On_Command;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Process : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Command : String) return String
   is
      pragma Unreferenced (Self);
      Cmd   : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String (Command);
      Match : VSS.Regular_Expressions.Regular_Expression_Match;
      View  : DAP_Variables_View;

      Arguments   : Boolean := False;
      Varname     : VSS.Strings.Virtual_String;
      Comname     : VSS.Strings.Virtual_String;
      Split_Lines : Boolean := False;

   begin
      if Process = null or else
        (not Starts_With (Command, "tree ")
         and then not Starts_With (Command, "graph ")
         and then not Starts_With (Command, "set "))
      then
         return Command;
      end if;

      Match := Tree_Cmd_Format.Match (Cmd);

      if Match.Has_Match then
         View := Get_Or_Create_View
           (Kernel, DAP.Clients.DAP_Visual_Debugger_Access (Process).Client);
         if View = null then
            return "";
         end if;

         if Match.Has_Capture (Tree_Cmd_Command) then
            if Match.Captured (Tree_Cmd_Command) = "arguments" then
               --  for backward compatibility to show arguments for
               --  "tree display `arguments`" command
               Arguments := True;
            else
               Comname     := Match.Captured (Tree_Cmd_Command);
               Split_Lines := Match.Has_Capture (Tree_Cmd_Split);
            end if;

         elsif Match.Has_Capture (Tree_Cmd_Varname) then
            Varname := Match.Captured (Tree_Cmd_Varname);
         else
            return Command;  --  Should not happen
         end if;

         declare
            Cmd : constant Virtual_String := Match.Captured (Tree_Cmd_Display);
         begin
            if Cmd = "display" then
               --  Do not send debugger quit command
               if DAP.Clients.DAP_Visual_Debugger_Access
                 (Process).Client.Is_Quit_Command (Comname)
               then
                  return "";
               end if;

               declare
                  Item : Item_Info'Class := DAP.Modules.Variables.Items.Create
                    (Varname, Comname, Split_Lines, Arguments);
               begin
                  View.Display (Item);
               end;

            elsif Cmd = "undisplay" then
               View.Undisplay
                 (DAP.Modules.Variables.Items.Create
                    (Varname, Comname, Split_Lines, Arguments));

            else
               Trace (Me, "Unsupported command:" & Command);
            end if;
         end;

         return "";  --  command was processed

      else
         --  Is set command?
         Match := Set_Command.Match (Cmd);

         if Match.Has_Match then
            View := Get_Or_Create_View
              (Kernel,
               DAP.Clients.DAP_Visual_Debugger_Access (Process).Client);

            if View = null then
               return "";
            end if;

            View.Set_Variable_Value
              (Full_Name => Match.Captured (1),
               Value     => VSS.Strings.Conversions.To_UTF_8_String
                 (Match.Captured (2)),
               Path      => Null_Gtk_Tree_Path);
            return "";
         else
            return Command;
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         return Command;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Display_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      Name   : constant String := Get_Variable_Name
        (Context.Context, Dereference => False);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : constant DAP_Variables_View :=
        Get_Or_Create_View (Kernel, Client);
   begin
      if View /= null
        and then Name /= ""
      then
         View.Display (Name);
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      Name   : constant Virtual_String :=
        To_Virtual_String
          (Get_Variable_Name (Context.Context, Command.Dereference));
   begin
      if GPS.Kernel.Contexts.Has_Debugging_Variable (Context.Context) then
         declare
            Info : constant Item_Info'Class := Get_Variable (Context.Context);
         begin
            if Info.Is_Command then
               Client.Process_User_Command
                 (Cmd               => Info.Get_Name,
                  Output_Command    => True,
                  Result_In_Console => True);

               return Commands.Success;
            end if;
         end;
      end if;

      if Name /= "" then
         Client.Process_User_Command
           (Cmd               => "print " & Name,
            Output_Command    => True,
            Result_In_Console => True);
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Display_Arguments_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      use type DAP.Clients.DAP_Client_Access;

      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      View   : DAP_Variables_View;
   begin
      if Client /= null then
         View := Get_Or_Create_View (Client.Kernel, Client);
         if View /= null then
            declare
               It : Item_Info'Class := DAP.Modules.Variables.Items.Create
                 (Arguments => True);
            begin
               View.Display (It);
            end;
         end if;
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Display_Locals_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      use type DAP.Clients.DAP_Client_Access;

      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      View   : DAP_Variables_View;
   begin
      if Client /= null then
         View := Get_Or_Create_View (Client.Kernel, Client);
         if View /= null then
            declare
               It : Item_Info'Class := DAP.Modules.Variables.Items.Create
                 (Locals => True);
            begin
               View.Display (It);
            end;
         end if;
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Undisplay_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View       : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));
      List       : Gtk_Tree_Path_List.Glist;
      G_Iter     : Gtk_Tree_Path_List.Glist;
      Path       : Gtk_Tree_Path;
      Model      : Gtk_Tree_Model;
      Store_Iter : Gtk_Tree_Iter;

      use Gtk_Tree_Path_List;
   begin
      if View /= null then
         View.Tree.Get_Selection.Get_Selected_Rows (Model, List);

         if Model /= Null_Gtk_Tree_Model and then List /= Null_List then
            --  The children must be modified before there fathers
            G_Iter := Gtk_Tree_Path_List.Last (List);

            while G_Iter /= Gtk_Tree_Path_List.Null_List loop
               Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));

               if Path /= Null_Gtk_Tree_Path then
                  Store_Iter :=
                    View.Tree.Convert_To_Store_Iter (Get_Iter (Model, Path));

                  View.Undisplay
                    (Item_From_Store_Iter (View.Tree, Store_Iter));
               end if;

               G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
            end loop;
         end if;

         Free_Path_List (List);
         View.Tree.Get_Selection.Unselect_All;
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Variables_Collapse_Or_Expand_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));
   begin
      if View /= null then
         Expand_Or_Collapse_Selected_Rows
           (Tree    => View.Tree,
            Command => Command.Command);
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));
   begin
      if View /= null then
         View.Clear;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Expression_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      Kernel     : constant Kernel_Handle := Get_Kernel (Context.Context);
      View       : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View (Kernel);
      Is_Var     : aliased Boolean;
      Expression : constant String := Display_Text_Input_Dialog
        (Kernel        => Kernel,
         Title         => "Display the value of an expression",
         Message       => "Enter an expression to display:",
         Key           => "gvd_display_expression_dialog",
         Check_Msg     => "Uncheck to evaluate as a command",
         Key_Check     => "expression_subprogram_debugger",
         Button_Active => Is_Var'Unchecked_Access);
   begin
      if Expression /= "" & ASCII.NUL then
         if Is_Var then
            View.Display (Expression);
         else
            declare
               Item : Item_Info'Class := DAP.Modules.Variables.Items.Create
                 (Command =>
                    VSS.Strings.Conversions.To_Virtual_String (Expression));
            begin
               View.Display (Item);
            end;
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));
   begin
      if View /= null then
         View.Tree.Start_Editing
           (Render      => View.Tree.Text,
            View_Column => Column_Value);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Export_Variables_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      use GNATCOLL.VFS;
      use Gtkada.File_Selector;

      pragma Unreferenced (Self);

      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));

      File   : constant Virtual_File :=
        Select_File
          (Title             => "Save variables as",
           Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
           Kind              => Save_File,
           Parent            => Get_Current_Window
             (Get_Kernel (Context.Context)),
           History           => Get_History
             (Get_Kernel (Context.Context)));

      Names      : VSS.String_Vectors.Virtual_String_Vector;
      Values     : VSS.String_Vectors.Virtual_String_Vector;
      Types      : VSS.String_Vectors.Virtual_String_Vector;

      Max_Names  : Integer := 0;
      Max_Values : Integer := 0;
      Max_Types  : Integer := 0;

   begin
      if File = GNATCOLL.VFS.No_File then
         return Commands.Success;
      end if;

      declare
         WF : Writable_File;

         procedure Process
           (Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
            Prefix : String);

         -------------
         -- Process --
         -------------

         procedure Process
           (Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
            Prefix : String)
         is
            I     : Gtk.Tree_Model.Gtk_Tree_Iter := Iter;
            Value : Glib.Values.GValue;
         begin
            while I /= Null_Iter loop
               --  Name
               View.Tree.Model.Get_Value (I, Column_Name, Value);
               declare
                  Name : constant String := Get_String (Value);
               begin
                  Names.Append
                    (To_Virtual_String
                       (Prefix &
                        (if Type_Of (Value) = GType_String
                           then Name (Name'First + 3 .. Name'Last - 4)
                           else "")));
               end;

               --  Value
               View.Tree.Model.Get_Value (I, Column_Value, Value);
               Values.Append
                 (To_Virtual_String
                    (Prefix &
                     (if Type_Of (Value) = GType_String
                        then XML_Utils.Translate (Get_String (Value))
                        else "")));

               --  Type
               if Show_Types.Get_Pref then
                  View.Tree.Model.Get_Value (I, Column_Type, Value);
                  Types.Append
                    (To_Virtual_String
                       (Prefix &
                        (if Type_Of (Value) = GType_String
                           then XML_Utils.Translate (Get_String (Value))
                           else "")));
               end if;

               for Idx in 1 .. View.Tree.Model.N_Children (I) loop
                  Process
                    (View.Tree.Model.Nth_Child (I, Idx - 1), Prefix & "  ");
               end loop;

               View.Tree.Model.Next (I);
            end loop;
         end Process;

      begin
         Process (View.Tree.Model.Get_Iter_First, "");

         for Index in 1 .. Names.Length loop
            Max_Names := Natural'Max
              (Max_Names, Natural (Names.Element (Index).Character_Length));

            if Show_Types.Get_Pref then
               Max_Values := Natural'Max
                 (Max_Values,
                  Natural (Values.Element (Index).Character_Length));

               Max_Types := Natural'Max
                 (Max_Types, Natural (Types.Element (Index).Character_Length));
            end if;
         end loop;

         WF := File.Write_File;
         for Index in 1 .. Names.Length loop
            GNATCOLL.VFS.Write
              (WF, To_UTF_8_String (Names.Element (Index)) &
               ((Max_Names - Natural
                  (Names.Element (Index).Character_Length) + 1) * ' ') &
                 " | ");

            GNATCOLL.VFS.Write
              (WF, To_UTF_8_String (Values.Element (Index)) &
               (if Show_Types.Get_Pref
                  then ((Max_Values - Natural
                    (Values.Element (Index).Character_Length) + 1) * ' ')
                  else ""));

            if Show_Types.Get_Pref then
               Write (WF, " | " & To_UTF_8_String (Types.Element (Index)));
            end if;

            Write (WF, GPS.Kernel.Preferences.Get_Line_Terminator);
         end loop;

         Close (WF);
      end;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Set_Format_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));
      Info : constant Item_Info'Class := Get_Variable (Context.Context);
      Name : constant Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String
          (Get_Variable_Name (Context.Context, Dereference => False));

      Format : Value_Format;
   begin
      if View /= null
        and then not Info.Is_Command
        and then Name /= ""
      then
         for Item of View.Tree.Items loop
            if Item.Get_Full_Name = Name then
               Format := Convert (Item.Format);
               if Display_Value_Select_Dialog
                 (Get_Kernel (Context.Context),
                  "Set format",
                  "Format for " & Name,
                  Format)
               then
                  Item.Format := Convert (Format);
                  View.Update;
               end if;
               exit;
            end if;
         end loop;
      end if;

      return Commands.Success;
   end Execute;

   --------------------
   -- Find_Best_Info --
   --------------------

   procedure Find_Best_Info
     (Self   : access Variables_Tree_View_Record'Class;
      Name   : Virtual_String;
      Cursor : out Item_Info_Vectors.Cursor;
      Found  : out Boolean)
   is
      Result     : Item_Info_Vectors.Cursor := Item_Info_Vectors.No_Element;
      Count      : Character_Count := 0;
      Lower_Name : constant Virtual_String := To_Lowercase.Transform (Name);
   begin
      Found  := False;
      Cursor := Self.Items.First;

      while Has_Element (Cursor) loop
         declare
            N : constant Virtual_String := Get_Full_Name (Element (Cursor));
         begin
            if Lower_Name = N then
               Found := True;
               return;

            elsif Lower_Name.Starts_With (N)
              and then N.Character_Length > Count
            then
               Result := Cursor;
               Count  := N.Character_Length;
            end if;
         end;

         Next (Cursor);
      end loop;

      Cursor := Result;
   end Find_Best_Info;

   -------------------
   -- Get_Item_Info --
   -------------------

   function Get_Item_Info
     (Self : not null access Variables_Tree_View_Record'Class;
      Name : String)
      return Item_Info'Class
   is
      N : constant Virtual_String := To_Lowercase.Transform
        (VSS.Strings.Conversions.To_Virtual_String (Name));
   begin
      for It of Self.Items loop
         if It.Get_Full_Name = N then
            return It;
         end if;
      end loop;

      return No_Item;
   end Get_Item_Info;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
     (Self : not null access Variables_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Virtual_String
   is
      Val      : GValue;
      Variable : Virtual_String;
   begin
      Get_Value (+Self.Model, Iter, Column_Full_Name, Val);
      Variable := VSS.Strings.Conversions.To_Virtual_String (Get_String (Val));
      Unset (Val);
      return Variable;
   end Get_Id;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Self  : access Variables_Property_Record;
      Value : in out GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;
   begin
      Trace (Me, "Saving variable view to JSON, has items ?"
             & Self.Items.Length'Img);

      for Item of Self.Items loop
         declare
            Value : JSON_Value := Create_Object;
         begin
            Item.Store (Value);
            Append (Values, Value);
         end;
      end loop;
      Value.Set_Field ("value", Values);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Self  : in out Variables_Property_Record;
      Value : GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : constant JSON_Array := Value.Get ("value");
   begin
      Trace (Me, "Loading variable view from JSON, has items ?"
             &  Boolean'Image (Length (Values) > 0));

      for Index in 1 .. Length (Values) loop
         Self.Items.Append (Restore (Get (Values, Index)));
      end loop;
   end Load;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Printable_Var_Filter     : Action_Filter;
      Debugger_Stopped_Filter  : Action_Filter;
      Not_Command_Filter       : Action_Filter;
      Access_Filter            : Action_Filter;
      Is_Editable_Filter       : Action_Filter;
      Selection_Filter         : Action_Filter;
      No_Or_Initialized_Filter : Action_Filter;
      View_Focused_Filter      : Action_Filter;
      Command                  : Interactive_Command_Access;

   begin
      Variables_Views.Register_Module (Kernel);
      Variables_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open debugger variables window",
         Description => "Open the Variables view for the debugger");

      Debugger_Command_Action_Hook.Add (new On_Command);

      Debugger_Stopped_Filter := Kernel.Lookup_Filter ("Debugger stopped");
      Printable_Var_Filter    := Kernel.Lookup_Filter
        ("Debugger printable variable");
      Not_Command_Filter := Kernel.Lookup_Filter
        ("Debugger not command variable");
      No_Or_Initialized_Filter := Kernel.Lookup_Filter
        ("No debugger or initialized");

      Access_Filter := new Access_Variable_Filter;
      Register_Filter
        (Kernel, Access_Filter, "Debugger variable is access");

      Register_Action
        (Kernel, "debug tree display variable",
         Command => new Tree_Display_Command,
         Description =>
           "Display the value of the variable in the Variables view",
         Filter      => Debugger_Stopped_Filter and Not_Command_Filter and
           Printable_Var_Filter,
         Category    => "Debug");
      Register_Contextual_Menu
        (Kernel,
         Label       => "Debug/Display %S in Variables view",
         Action      => "debug tree display variable",
         Group       => DAP_Variables_Contextual_Group);

      Register_Action
        (Kernel, "debug tree display expression",
         Command     => new Tree_Expression_Command,
         Description =>
           "Display the value of any expression in the Variables view",
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-add-symbolic",
         Category    => "Debug");

      Register_Action
        (Kernel, "debug tree undisplay",
         Command     => new Tree_Undisplay_Command,
         Description =>
           "Remove the display of the selected variables"
           & " in the Variables view",
         Filter      => No_Or_Initialized_Filter,
         Icon_Name   => "gps-remove-symbolic",
         Category    => "Debug");

      Register_Action
        (Kernel, "debug tree clear",
         Command     => new Tree_Clear_Command,
         Description =>
           "Remove the display of all variables in the Variables view",
         Filter      => No_Or_Initialized_Filter,
         Icon_Name   => "gps-clear-symbolic",
         Category    => "Debug");

      Register_Action
        (Kernel, "debug tree set value",
         Command     => new Set_Value_Command,
         Description => "Set a new value for the selected variable.",
         Icon_Name   => "gps-rename-symbolic",
         Category    => "Debug");

      Register_Action
        (Kernel, "variables view collapse selected",
         Command     => new Variables_Collapse_Or_Expand_Command
           (Collapse_Rows),
      Description => "Collapse the selected nodes in the variables tree",
         Icon_Name   => "gps-collapse-all-symbolic",
         Category    => "Debug");

      Register_Action
        (Kernel, "variables view expand selected",
         Command     => new Variables_Collapse_Or_Expand_Command
           (Expand_Rows),
         Description => "Expand the selected nodes in the variables tree",
         Icon_Name   => "gps-expand-all-symbolic",
         Category    => "Debug");

      Selection_Filter := new Variable_Single_Selection;
      Register_Action
        (Kernel, "variables view expand next layer",
         Command     => new Variables_Collapse_Or_Expand_Command
           (Expand_All_Rows),
         Description =>
           "Expand all the children of the selected node",
         Icon_Name   => "gps-expand-all-symbolic",
         Category    => "Debug",
         Filter      => Selection_Filter);

      Register_Action
        (Kernel, "debug export variables",
         Command     => new Export_Variables_Command,
         Description => "Save variables to a file",
         Icon_Name   => "gps-save-symbolic",
         Category    => "Debug",
         Filter      => Debugger_Stopped_Filter);

      View_Focused_Filter := new Is_Variables_View_Focused_Filter;
      Register_Action
        (Kernel, "debug tree remove selected variables",
         Command     => new Tree_Undisplay_Command,
         Description =>
           "Remove the display of the selected variables"
           & " in the Variables view, when focused",
         Filter      => View_Focused_Filter and No_Or_Initialized_Filter,
         Icon_Name   => "gps-remove-symbolic",
         Category    => "Debug");

      Is_Editable_Filter := new Is_Variable_Editable_Filter;
      Register_Filter
        (Kernel, Is_Editable_Filter, "Debugger is variable editable");

      Register_Action
        (Kernel, "debug set variable format",
         Command => new Set_Format_Command,
         Description =>
           "Set format for the variable in the Variables view",
         Filter      => No_Or_Initialized_Filter and Is_Editable_Filter,
         Category    => "Debug");
      Register_Contextual_Menu
        (Kernel,
         Label       => "Debug/Set format for %S",
         Action      => "debug set variable format",
         Group       => DAP_Variables_Contextual_Group);

      Show_Types := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debugger-variables-show-types", True, Label => "Show types");

      Register_Action
        (Kernel, "debug print variable",
         Command     => new Print_Variable_Command,
         Description =>
           "Print the value of the variable in the debugger console",
         Filter      => Debugger_Stopped_Filter and Printable_Var_Filter,
         Category    => "Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => "Debug/Print %S",
         Action => "debug print variable",
         Group  => DAP_Variables_Contextual_Group);

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Dereference := True;
      Register_Action
        (Kernel, "debug print dereferenced variable",
         Command     => Command,
         Description =>
           "Print the value pointed to by the variable in the debugger"
           & " console",
         Filter    => Debugger_Stopped_Filter and Access_Filter and
           Printable_Var_Filter,
         Category  => "Debug");

      Register_Action
        (Kernel, "debug tree display arguments",
         Command     => new Display_Arguments_Command,
         Description =>
           "Display the arguments of the current subprogram in the" &
           "  Variables view",
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-debugger-arguments-symbolic",
         Category    => "Debug");

      Register_Action
        (Kernel, "debug tree display local variables",
         Command => new Display_Locals_Command,
         Description =>
           "Display the local variables in the Variables view",
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-debugger-local-vars-symbolic",
         Category    => "Debug");

   end Register_Module;

end DAP.Views.Variables;
