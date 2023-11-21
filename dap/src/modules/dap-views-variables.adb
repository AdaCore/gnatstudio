------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with Ada.Containers.Vectors;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

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

with VSS.Characters;
with VSS.Regular_Expressions;     use VSS.Regular_Expressions;
with VSS.Strings.Conversions;
with VSS.Strings.Cursors.Iterators.Characters;

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
with DAP.Modules.Contexts;        use DAP.Modules.Contexts;
with DAP.Modules.Preferences;
with DAP.Utils;                   use DAP.Utils;

with DAP.Views.Variables.Evaluate_Requests;
with DAP.Views.Variables.Scopes_Requests;
with DAP.Views.Variables.Variables_Requests;
with DAP.Views.Variables.Set_Expression_Requests;
with DAP.Views.Variables.Set_Variable_Requests;

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

   type Access_Variable_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Is_Variables_View_Focused_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Variables_View_Focused_Filter;
      Context : Selection_Context) return Boolean;

   type Is_Variable_Editable_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Variable_Editable_Filter;
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

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (Self : not null access DAP_Variables_View_Record) is
   begin
      Self.Locals_Id := 0;
      Self.Clear;
      Self.Locals.Clear;
      Self.Old.Clear;
   end On_Process_Terminated;

   -----------------------
   -- On_Status_Changed --
   -----------------------

   overriding procedure On_Status_Changed
     (Self   : not null access DAP_Variables_View_Record;
      Status : GPS.Debuggers.Debugger_State)
   is
      use GPS.Debuggers;
   begin
      if Status = Debug_Busy
        or else Status = Debug_None
      then
         Self.Locals_Id := 0;
         Self.Old := Self.Locals;
         Self.Locals.Clear;
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
        and then Get_Variable (Context).Cmd = ""
      then
         declare
            Name : constant String :=
              Get_Variable_Name (Context, Dereference => False);
         begin
            if Name /= "" then
               return Get_Item_Info (View.Tree, Name) /= No_Item_Info;
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
      C     : Variables_References_Trees.Cursor := Self.Old.Root;
      Found : Boolean;
   begin
      Find_Best_Ref (Full_Name (Cursor), C, Found);
      return Found and then Element (C).value /= Element (Cursor).value;
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
      It        : Item_Info;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      C         : Item_Info_Vectors.Cursor;
      Found     : Boolean;
   begin
      --  Check whether we already have this item in the list.
      --  We expect this list to be relatively short (less than 1000 elements
      --  in general), so no need for a more elaborate data structure.

      Path := Self.Model.Get_Path (Store_Iter);
      for It2 of Self.Items loop
         if It2.Varname.To_Lowercase = Full_Name then
            DAP_Variables_View (Self.View).Update (It2, 0, Path, True);
            return;
         end if;
      end loop;

      Self.Find_Best_Info (Full_Name, C, Found);

      It :=
        (Id      => Unknown_Id,
         Varname => Full_Name,
         Format  =>
           (if C /= Item_Info_Vectors.No_Element
            then Element (C).Format
            else Default_Format),
         others  => <>);
      Self.Items.Append (It);
      DAP_Variables_View (Self.View).Update (It, 0, Path, True);
   end Add_Children;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row
     (Self   : not null access Variables_Tree_View_Record'Class;
      Item   : Item_Info;
      Cursor : Variables_References_Trees.Cursor;
      Parent : Gtk_Tree_Iter)
   is
      Var : Variable;
      Row : Gtk_Tree_Iter;
      Fg  : constant String := To_String (Default_Style.Get_Pref_Fg);

      function Display_Type_Name return String with Inline;
      function Display_Name return String with Inline;
      --  Return the display name or type name

      function Validate_UTF_8 (S : String) return String;
      --  This function cuts S up to it's last valid UTF8 symbol

      function Display_Name return String is
         function Wrap (Name : String) return String;
         function Wrap (Name : String) return String is
         begin
            if Name = "" then
               return "";
            else
               return "<b>" & XML_Utils.Protect (Name) & "</b>"
                 & (if Item = No_Item_Info
                    then ""
                    else Image (Item.Format));
            end if;
         end Wrap;

      begin
         if Cursor = Variables_References_Trees.No_Element then
            return Wrap (Get_Name (Item));
         else
            return Wrap (UTF8 (Element (Cursor).name));
         end if;
      end Display_Name;

      function Display_Type_Name return String is
      begin
         if Cursor = Variables_References_Trees.No_Element then
            return "";
         else
            return XML_Utils.Protect (UTF8 (Element (Cursor).a_type));
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

      Var_Name  : constant String := Display_Name;
      Value     : constant String :=
                    (if Cursor = Variables_References_Trees.No_Element
                     then ""
                     else XML_Utils.Protect
                       (Validate_UTF_8 (UTF8 (Element (Cursor).value))));
      Type_Name : constant String := Display_Type_Name;
   begin
      if Cursor /= Variables_References_Trees.No_Element then
         Var := Element (Cursor);
      end if;

      Trace
        (Me, "Add row:" & UTF8
           ((if Cursor /= Variables_References_Trees.No_Element
            then Var.name
            else Item.Varname)));

      Self.Model.Append (Iter => Row, Parent => Parent);
      Set_And_Clear
        (Self.Model,
         Iter   => Row,
         Values =>
           (Column_Name         => As_String (Var_Name),
            Column_Value        => As_String (Value),
            Column_Type         => As_String (Type_Name),
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
            Column_Full_Name    => As_String (Full_Name (Cursor))));

      if Cursor /= Variables_References_Trees.No_Element then
         if Var.variablesReference > 0 then
            Self.Set_Might_Have_Children (Row);
         end if;

      else
         Trace (Me, "Entity is empty");
      end if;
   end Add_Row;

   --------------------
   -- Item_From_Iter --
   --------------------

   function Item_From_Iter
     (Self       : not null access Variables_Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter)
      return Item_Info
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

      return No_Item_Info;
   end Item_From_Iter;

   --------------------
   -- Item_From_Iter --
   --------------------

   procedure Item_From_Iter
     (Self        : not null access Variables_Tree_View_Record'Class;
      Filter_Iter : in out Gtk_Tree_Iter;
      It          : out Item_Info)
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

      It := Item_From_Iter (Self, Store_Iter);
   end Item_From_Iter;

   ---------------
   -- Find_Info --
   ---------------

   function Find_Info
     (Self : not null access Variables_Tree_View_Record'Class;
      Id   : Item_ID)
      return Item_Info is
   begin
      for Item of Self.Items loop
         if Item.Id = Id then
            return Item;
         end if;
      end loop;

      return No_Item_Info;
   end Find_Info;

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

      It : Item_Info;
   begin
      if Store_Iter /= Null_Iter
        and then View_Column = Column_Value
      then
         It := Item_From_Iter (Self, Store_Iter);

         if Self.View /= null
           and then Get_Client (Self.View) /= null
           and then It /= No_Item_Info
         then
            if It.Cmd /= Empty_Virtual_String then
               DAP_Variables_View (Self.View).Set_Variable
                 (Name  => Get_Name (It),
                  Value => Text,
                  Path  => Self.Model.Get_Path (Store_Iter));

            elsif It.Varname /= Empty_Virtual_String then
               DAP_Variables_View (Self.View).Set_Variable
                 (Name  => Self.Model.Get_String
                    (Store_Iter, Column_Full_Name),
                  Value => Text,
                  Path  => Self.Model.Get_Path (Store_Iter));
            end if;
         end if;
      end if;
   end On_Edited;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable
     (Self  : access DAP_Variables_View_Record'Class;
      Name  : String;
      Value : String;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      use type DAP.Clients.DAP_Client_Access;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
      N      : constant Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String (Name);
      Inf    : Boolean;
      Ref    : Boolean;
      C      : Variables_References_Trees.Cursor := Self.Locals.Root;
      Cursor : Item_Info_Vectors.Cursor;
   begin
      if Client = null then
         return;
      end if;

      Self.Tree.Find_Best_Info (N, Cursor, Inf);

      if Client.Get_Capabilities.Is_Set
        and then Client.Get_Capabilities.Value.supportsSetExpression
      then
         declare
            Req : DAP.Views.Variables.Set_Expression_Requests.
              Set_Expression_Request_Access :=
                new DAP.Views.Variables.Set_Expression_Requests.
                  Set_Expression_Request (Self.Kernel);
         begin
            Req.Name := N;
            Req.Path := Copy (Path);
            Req.Parameters.arguments.expression := N;
            Req.Parameters.arguments.value :=
              VSS.Strings.Conversions.To_Virtual_String (Value);
            Req.Parameters.arguments.frameId :=
              (Is_Set => True, Value => Client.Get_Selected_Frame);
            if Inf
              and then Element (Cursor).Format /= Default_Format
            then
               Req.Parameters.arguments.format :=
                 (Is_Set => True, Value => Element (Cursor).Format);
            end if;

            Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
         end;

      elsif Client.Get_Capabilities.Is_Set
        and then Client.Get_Capabilities.Value.supportsSetVariable
      then
         Find_Best_Ref (N, C, Ref);

         if Ref then
            declare
               Req    : DAP.Views.Variables.Set_Variable_Requests.
                 Set_Variable_Request_Access :=
                   new DAP.Views.Variables.Set_Variable_Requests.
                     Set_Variable_Request (Self.Kernel);
               Parent : constant Variables_References_Trees.Cursor :=
                 Variables_References_Trees.Parent (C);
            begin
               Req.Name := N;
               Req.Path := Copy (Path);
               Req.Parameters.arguments.variablesReference :=
                 Element (Parent).variablesReference;
               Req.Parameters.arguments.name  := Element (C).name;
               Req.Parameters.arguments.value :=
                 VSS.Strings.Conversions.To_Virtual_String (Value);
               if Inf
                 and then Element (Cursor).Format /= Default_Format
               then
                  Req.Parameters.arguments.format :=
                    (Is_Set => True, Value => Element (Cursor).Format);
               end if;
               Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
            end;
         end if;

      else
         Self.Kernel.Get_Messages_Window.Insert_Text
           ("Editing is not supported");
      end if;

      Path_Free (Path);
   end Set_Variable;

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
      It          : Item_Info;
      Filter_Iter : Gtk_Tree_Iter;
   begin
      Context := GPS_MDI_Child_Record (Self.all).Build_Context (Event);

      if Event /= null then
         Filter_Iter := Find_Iter_For_Event (View.Tree, Event);
         Item_From_Iter (View.Tree, Filter_Iter => Filter_Iter, It => It);
         View.Tree.Get_Selection.Unselect_All;
         View.Tree.Get_Selection.Select_Iter (Filter_Iter);
      else
         Filter_Iter := Null_Iter;
         Item_From_Iter (View.Tree, Filter_Iter => Filter_Iter, It => It);
      end if;

      if It /= No_Item_Info then
         if not It.Cmd.Is_Empty then
            Set_Variable (Context, Get_Name (It), It);

         elsif not It.Varname.Is_Empty then
            Set_Variable
              (Context,
               View.Tree.Filter.Get_String (Filter_Iter, Column_Full_Name),
               It);
         end if;
      end if;

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
            Self.Tree.Items := Property.Items;

            for It of Self.Tree.Items loop
               Self.Tree.Ids := Self.Tree.Ids + 1;
               It.Id := Self.Tree.Ids;
            end loop;

            Self.Update;
         end if;
      end if;
   end On_Attach;

   ---------------
   -- On_Detach --
   ---------------

   overriding procedure On_Detach
     (Self   : not null access DAP_Variables_View_Record;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      if Client /= null
        and then DAP.Modules.Preferences.Preserve_State_On_Exit.Get_Pref
      then
         Set_Property
           (Kernel     => Self.Kernel,
            File       => Client.Get_Executable,
            Name       => "dap_debugger_variables",
            Property   =>
               new Variables_Property_Record'
                 (Items => Self.Tree.Items),
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
      Item : Item_Info;
   begin
      Item.Varname := VSS.Strings.Conversions.To_Virtual_String (Name);
      Self.Display (Item);
   end Display;

   -------------
   -- Display --
   -------------

   procedure Display
     (Self : access DAP_Variables_View_Record'Class;
      Item : in out Item_Info) is
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
      Item : Item_Info) is
   begin
      if not Item.Varname.Is_Empty then
         Self.Undisplay (Item.Varname);
      elsif not Item.Cmd.Is_Empty then
         Self.Undisplay (Item.Cmd);
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
      Curs := Self.Tree.Items.First;
      while Item_Info_Vectors.Has_Element (Curs) loop
         if Is_Same (Item_Info_Vectors.Element (Curs), Name) then
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
      use type DAP.Requests.DAP_Request_Access;

      Client  : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
      Request : DAP.Requests.DAP_Request_Access;
   begin
      Self.Locals_Id := 0;
      Self.Locals.Clear;

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
         Expansions.Get_Expansion_Status (Self.Tree, Self.Expansion);
         Self.Tree.Model.Clear;

         Self.Continue_Update (0, Request);
         if Request /= null then
            Get_Client (Self).Enqueue (Request);
         end if;

      else
         Self.Tree.Model.Clear;
      end if;
   end Update;

   ---------------------
   -- Continue_Update --
   ---------------------

   procedure Continue_Update
     (Self     : not null access DAP_Variables_View_Record'Class;
      Position : Natural;
      Request  : out DAP.Requests.DAP_Request_Access)
   is
      use type DAP.Requests.DAP_Request_Access;
      Num : Natural;
   begin
      --  we done with the current variable but we updateing the view,
      --  so start with the next one

      Request := null;
      for Pos in Position + 1 .. Self.Tree.Items.Last_Index loop
         if Self.Tree.Items (Pos).Auto_Refresh then
            Request := Self.Update
              (Self.Tree.Items (Pos),
               Pos,
               Null_Gtk_Tree_Path,
               Self.Tree.Items (Pos).Id = Unknown_Id);

            if Request /= null then
               Num := Pos;
               exit;
            end if;
         end if;
      end loop;

      if Request = null then
         --  we processed all variables, restore expansions
         Expansions.Set_Expansion_Status (Self.Tree, Self.Expansion);

      else
         Trace (Me, "Update:" & Get_Name (Self.Tree.Items (Num)));
      end if;
   end Continue_Update;

   ------------
   -- Update --
   ------------

   procedure Update (Client : not null access DAP.Clients.DAP_Client'Class) is
      View : constant DAP_Variables_View := Get_View (Client);
   begin
      if View /= null then
         View.Update;
      end if;
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self     : access DAP_Variables_View_Record'Class;
      Item     : Item_Info;
      Position : Natural;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Childs   : Boolean := False)
   is
      use type DAP.Requests.DAP_Request_Access;
      Request : DAP.Requests.DAP_Request_Access;
   begin
      Request := Self.Update (Item, Position, Path, Childs);
      if Request /= null then
         Get_Client (Self).Enqueue (Request);
      end if;
   end Update;

   ------------
   -- Update --
   ------------

   function Update
     (Self     : access DAP_Variables_View_Record'Class;
      Item     : Item_Info;
      Position : Natural;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Childs   : Boolean := False)
      return DAP.Requests.DAP_Request_Access
   is
      use type DAP.Clients.DAP_Client_Access;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
      Req    : DAP.Views.Variables.Scopes_Requests.Scopes_Request_Access;
      Result : DAP.Requests.DAP_Request_Access;
   begin
      if Client = null then
         if Path /= Null_Gtk_Tree_Path then
            Path_Free (Path);
         end if;

         Self.Tree.Model.Clear;
         return null;
      end if;

      if Client.Is_Stopped then
         if Self.Locals_Id = 0
           and then Item.Cmd.Is_Empty
         --  don't have local's id and not a command, get it
         then
            Req := new DAP.Views.Variables.Scopes_Requests.
              Scopes_Request (Self.Kernel);

            Req.Client   := Client;
            Req.Item     := Item;
            Req.Position := Position;
            Req.Childs   := Childs;
            if Path /= Null_Gtk_Tree_Path then
               Req.Path := Copy (Path);
            end if;
            Req.Parameters.arguments.frameId := Client.Get_Selected_Frame;
            Result := DAP.Requests.DAP_Request_Access (Req);

         else
            Self.Publish_Or_Request (Item, Position, Childs, Path, Result);
         end if;

      else
         Self.Tree.Add_Row
           (Item, Variables_References_Trees.No_Element,
            Self.Tree.Model.Get_Iter (Path));
      end if;

      if Path /= Null_Gtk_Tree_Path then
         Path_Free (Path);
      end if;

      return Result;
   end Update;

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
           Visible_Only => False);
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
      It    : Item_Info;
      View  : DAP_Variables_View;

   begin
      if Process = null or else
        (not Starts_With (Command, "tree ")
         and then not Starts_With (Command, "graph "))
      then
         return Command;
      end if;

      Match := Tree_Cmd_Format.Match (Cmd);

      if not Match.Has_Match then
         return Command;
      end if;

      Variables_Views.Attach_To_View
        (DAP.Clients.DAP_Visual_Debugger_Access (Process).Client,
         Kernel, Create_If_Necessary => True);
      View := DAP_Variables_View
        (DAP.Clients.DAP_Visual_Debugger_Access
           (Process).Client.Get_Variables_View);

      if Match.Has_Capture (Tree_Cmd_Command) then
         It.Cmd := Match.Captured (Tree_Cmd_Command);
         It.Split_Lines := Match.Has_Capture (Tree_Cmd_Split);

      elsif Match.Has_Capture (Tree_Cmd_Varname) then
         It.Varname := Match.Captured (Tree_Cmd_Varname);
      else
         return Command;  --  Should not happen
      end if;

      declare
         Cmd : constant Virtual_String := Match.Captured (Tree_Cmd_Display);
      begin
         if Cmd = "display" then
            --  Do not send debugger quit command
            if DAP.Clients.DAP_Visual_Debugger_Access
              (Process).Client.Is_Quit_Command (It.Cmd)
            then
               return "";
            end if;

            View.Display (It);

         elsif Cmd = "undisplay" then
            View.Undisplay (It);

         else
            Trace (Me, "Unsupported command:" & Command);
         end if;
      end;

      return "";  --  command was processed

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

      View : DAP_Variables_View;
   begin
      Variables_Views.Attach_To_View
        (Client, Get_Kernel (Context.Context), Create_If_Necessary => True);
      View := Get_View (Client);

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
      Name   : constant String :=
        Get_Variable_Name (Context.Context, Command.Dereference);
   begin
      if GPS.Kernel.Contexts.Has_Debugging_Variable (Context.Context) then
         declare
            Info : constant Item_Info := Get_Variable (Context.Context);
         begin
            if Info.Cmd /= "" then
               Client.Process_User_Command
                 (Cmd               => UTF8 (Info.Cmd),
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
     (Command : access Tree_Undisplay_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View       : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));
      It         : Item_Info;
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
                  It := Item_From_Iter (View.Tree, Store_Iter);

                  if It.Cmd /= Empty_Virtual_String then
                     View.Undisplay (It.Cmd);
                  elsif It.Varname /= Empty_Virtual_String then
                     View.Undisplay (It.Varname);
                  end if;
               end if;

               G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
            end loop;
         end if;

         Free_Path_List (List);
      end if;
      View.Tree.Get_Selection.Unselect_All;

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
               Item : Item_Info;
            begin
               Item.Cmd :=
                 VSS.Strings.Conversions.To_Virtual_String (Expression);
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

      package Unbounded_String_Vectors is new Ada.Containers.Vectors
        (Positive, Ada.Strings.Unbounded.Unbounded_String);

      Names      : Unbounded_String_Vectors.Vector;
      Values     : Unbounded_String_Vectors.Vector;
      Types      : Unbounded_String_Vectors.Vector;

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
                    (To_Unbounded_String
                       (Prefix &
                        (if Type_Of (Value) = GType_String
                           then Name (Name'First + 3 .. Name'Last - 4)
                           else "")));
               end;

               --  Value
               View.Tree.Model.Get_Value (I, Column_Value, Value);
               Values.Append
                 (To_Unbounded_String
                    (Prefix &
                     (if Type_Of (Value) = GType_String
                        then XML_Utils.Translate (Get_String (Value))
                        else "")));

               --  Type
               if Show_Types.Get_Pref then
                  View.Tree.Model.Get_Value (I, Column_Type, Value);
                  Types.Append
                    (To_Unbounded_String
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

         for Index in 1 .. Natural (Names.Length) loop
            Max_Names := Natural'Max
              (Max_Names, Length (Names.Element (Index)));

            if Show_Types.Get_Pref then
               Max_Values := Natural'Max
                 (Max_Values, Length (Values.Element (Index)));

               Max_Types := Natural'Max
                 (Max_Types, Length (Types.Element (Index)));
            end if;
         end loop;

         WF := File.Write_File;
         for Index in 1 .. Natural (Names.Length) loop
            GNATCOLL.VFS.Write
              (WF, To_String (Names.Element (Index)) &
               ((Max_Names - Length (Names.Element (Index)) + 1) * ' ') &
                 " | ");

            GNATCOLL.VFS.Write
              (WF, To_String (Values.Element (Index)) &
               (if Show_Types.Get_Pref
                  then ((Max_Values - Length
                    (Values.Element (Index)) + 1) * ' ')
                  else ""));

            if Show_Types.Get_Pref then
               Write (WF, " | " & To_String (Types.Element (Index)));
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
      Info : constant Item_Info := Get_Variable (Context.Context);
      Name : constant Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String
          (Get_Variable_Name (Context.Context, Dereference => False));

      Format : Value_Format;
   begin
      if View /= null
        and then Info.Cmd = ""
        and then Name /= ""
      then
         for Item of View.Tree.Items loop
            if Item.Varname = Name then
               Format := Convert (Item.Format);
               if Display_Value_Select_Dialog
                 (Get_Kernel (Context.Context),
                  "Set format",
                  "Format for " & UTF8 (Name),
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

   -------------------
   -- Find_Best_Ref --
   -------------------

   procedure Find_Best_Ref
     (Name   : Virtual_String;
      Cursor : in out Variables_References_Trees.Cursor;
      Found  : out Boolean)
   is
      use type Ada.Containers.Count_Type;
      use VSS.Strings.Cursors.Iterators.Characters;
      use VSS.Characters;

      procedure Find (N : Virtual_String);
      procedure Find (N : Virtual_String) is
         Pos   : VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
           N.At_First_Character;
         Part  : Virtual_String;
         Dummy : Boolean;
      begin
         if Child_Count (Cursor) = 0 then
            return;
         end if;

         while Has_Element (Pos)
           and then Element (Pos) /= '.'
         loop
            Part.Append (Element (Pos));
            Dummy := Forward (Pos);
         end loop;

         Cursor := First_Child (Cursor);
         Dummy  := False;
         while Cursor /= Variables_References_Trees.No_Element loop
            if Element (Cursor).name.To_Lowercase = Part then
               exit;
            end if;
            Next_Sibling (Cursor);
         end loop;

         if Cursor /= Variables_References_Trees.No_Element then
            if Has_Element (Pos) then
               --  the part of the name is found
               Dummy := Forward (Pos); --  skip '.'
               Find (N.Slice (Pos, N.At_Last_Character));
            else
               --  the last name part is found
               Found := True;
            end if;
         end if;
      end Find;

   begin
      Found := False;

      if Child_Count (Cursor) = 0 then
         Cursor := Variables_References_Trees.No_Element;
         return;
      end if;

      Find (Name.To_Lowercase);
   end Find_Best_Ref;

   ------------------
   -- Find_Cmd_Ref --
   ------------------

   procedure Find_Cmd_Ref
     (Name   : Virtual_String;
      Cursor : in out Variables_References_Trees.Cursor;
      Found  : out Boolean)
   is
      use type Ada.Containers.Count_Type;
      N : constant Virtual_String := Name.To_Lowercase;

   begin
      Found := False;

      if Child_Count (Cursor) = 0 then
         return;
      end if;

      Cursor := First_Child (Cursor);
      while Cursor /= Variables_References_Trees.No_Element loop
         if Element (Cursor).name.To_Lowercase = N then
            Found := True;
            return;
         end if;
         Next_Sibling (Cursor);
      end loop;
   end Find_Cmd_Ref;

   --------------------
   -- Find_Best_Info --
   --------------------

   procedure Find_Best_Info
     (Self   : access Variables_Tree_View_Record'Class;
      Name   : Virtual_String;
      Cursor : out Item_Info_Vectors.Cursor;
      Found  : out Boolean)
   is
      Result : Item_Info_Vectors.Cursor := Item_Info_Vectors.No_Element;
      Count  : Character_Count := 0;
   begin
      Found  := False;
      Cursor := Self.Items.First;

      while Has_Element (Cursor) loop
         declare
            N : constant Virtual_String := Get_Name (Element (Cursor));
         begin
            if Name = N then
               Found := True;
               return;

            elsif Name.Starts_With (N)
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
      return Item_Info
   is
      N : constant Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String (Name);
   begin
      for It of Self.Items loop
         if It.Varname = N then
            return It;
         end if;
      end loop;

      return No_Item_Info;
   end Get_Item_Info;

   ------------------------
   -- Publish_Or_Request --
   ------------------------

   procedure Publish_Or_Request
     (Self     : access DAP_Variables_View_Record'Class;
      Item     : Item_Info;
      Position : Natural;
      Childs   : Boolean;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Request  : out DAP.Requests.DAP_Request_Access)
   is
      use type Ada.Containers.Count_Type;
      Found  : Boolean;
      C      : Variables_References_Trees.Cursor;
      Parent : Gtk_Tree_Iter := Null_Iter;

      procedure Create_Request;
      procedure Create_Ev_Request;

      -- Create_Request --
      procedure Create_Request
      is
         Req : constant DAP.Views.Variables.Variables_Requests.
           Variables_Request_Access :=
             new DAP.Views.Variables.Variables_Requests.
               Variables_Request (Self.Kernel);
      begin
         Req.Client   := Get_Client (Self);
         Req.Item     := Item;
         Req.Position := Position;
         Req.Childs   := Childs;
         if Path /= Null_Gtk_Tree_Path then
            Req.Path := Copy (Path);
         end if;
         Req.Ref := Element (C).variablesReference;
         Req.Parameters.arguments.variablesReference := Req.Ref;
         if Item.Format /= Default_Format then
            Req.Parameters.arguments.format :=
              (Is_Set => True, Value => Item.Format);
         end if;

         Request := DAP.Requests.DAP_Request_Access (Req);
      end Create_Request;

      -- Create_Ev_Request --
      procedure Create_Ev_Request
      is
         Req : constant DAP.Views.Variables.Evaluate_Requests.
           Evaluate_Request_Access :=
             new DAP.Views.Variables.Evaluate_Requests.
               Evaluate_Request (Self.Kernel);
      begin
         Req.Client   := Get_Client (Self);
         Req.Item     := Item;
         Req.Position := Position;
         if Path /= Null_Gtk_Tree_Path then
            Req.Path := Copy (Path);
         end if;

         Req.Parameters.arguments.expression := Item.Cmd;
         Req.Parameters.arguments.frameId :=
           (Is_Set => True, Value => Req.Client.Get_Selected_Frame);
         Req.Parameters.arguments.context :=
           (Is_Set => True, Value => DAP.Tools.Enum.repl);

         Request := DAP.Requests.DAP_Request_Access (Req);
      end Create_Ev_Request;

   begin
      Request := null;
      C := Self.Locals.Root;
      if Item.Varname.Is_Empty then
         Find_Cmd_Ref (Item.Cmd, C, Found);

      else
         Find_Best_Ref (Item.Varname, C, Found);
      end if;

      if Found then
         --  we found the variable

         if Path /= Null_Gtk_Tree_Path then
            Parent := Self.Tree.Model.Get_Iter (Path);
         end if;

         if not Childs then
            --  we need the variable itself, add it to the table

            if (Path = Null_Gtk_Tree_Path
                and then Item.Id /= Unknown_Id)
              or else (Path /= Null_Gtk_Tree_Path
                       and then Item.Id = Unknown_Id)
            --  do not add child items when we in the view update
            then
               Self.Tree.Add_Row (Item, C, Parent);
            end if;

         elsif Element (C).variablesReference > 0 then
            --  we need the variable's childs and they can be fetched

            if Child_Count (C) /= 0 then
               --  we already have childs, add them to the table

               if Item.Id /= Unknown_Id
                 --  add the item when it is root item
                 or else Parent /= Null_Iter
                 --  or we are adding as a nested element
               then
                  C := First_Child (C);
                  while C /= Variables_References_Trees.No_Element loop
                     Self.Tree.Add_Row (Item, C, Parent);
                     Next_Sibling (C);
                  end loop;
                  Found := Self.Tree.Expand_Row (Path, False);
               end if;

            else
               --  we need to get them, prepare a request
               Create_Request;
            end if;
         end if;

      elsif C /= Variables_References_Trees.No_Element then
         --  we found a variable that contains one we are looking for.
         --  Send new request to "expand" it and get childs
         if not Item.Cmd.Is_Empty then
            Create_Ev_Request;
         else
            Create_Request;
         end if;

      else
         if not Item.Cmd.Is_Empty then
            Create_Ev_Request;

         elsif Path = Null_Gtk_Tree_Path
           and then Item.Id /= Unknown_Id
         then
            --  we did not found the root variable, just add a row that it
            --  can be deleted from GUI
            Self.Tree.Add_Row
              (Item, Variables_References_Trees.No_Element, Null_Iter);
         end if;
      end if;
   end Publish_Or_Request;

   --------------
   -- Find_Ref --
   --------------

   function Find_Ref
     (Self : access DAP_Variables_View_Record'Class;
      Id   : Integer)
      return Variables_References_Trees.Cursor
   is
      function Find
        (C : Variables_References_Trees.Cursor)
         return Variables_References_Trees.Cursor;
      function Find
        (C : Variables_References_Trees.Cursor)
         return Variables_References_Trees.Cursor
      is
         use type Ada.Containers.Count_Type;
         Current : Variables_References_Trees.Cursor := C;
         R       : Variables_References_Trees.Cursor;
      begin
         while Current /= Variables_References_Trees.No_Element loop
            if Element (Current).variablesReference = Id then
               return Current;
            end if;
            Next_Sibling (Current);
         end loop;

         Current := C;
         while Current /= Variables_References_Trees.No_Element loop
            if Child_Count (Current) /= 0 then
               R := Find (First_Child (Current));
               if R /= Variables_References_Trees.No_Element then
                  return R;
               end if;
            end if;
            Next_Sibling (Current);
         end loop;

         return Variables_References_Trees.No_Element;
      end Find;

   begin
      if Id = Self.Locals_Id then
         return Self.Locals.Root;
      else
         return Find (First_Child (Self.Locals.Root));
      end if;
   end Find_Ref;

   -------------
   -- Full_Name --
   ---------------

   function Full_Name
     (Cursor : Variables_References_Trees.Cursor)
      return String
   is
      Result : Ada.Strings.Unbounded.Unbounded_String;
      C      : Variables_References_Trees.Cursor := Cursor;
   begin
      while C /= Variables_References_Trees.No_Element
        and then not Is_Root (C)
      loop
         if Result /= "" then
            Result := VSS.Strings.Conversions.To_Unbounded_UTF_8_String
                 (Element (C).name) & "." & Result;
         else
            Append (Result,
                    VSS.Strings.Conversions.To_Unbounded_UTF_8_String
                      (Element (C).name));
         end if;
         C := Parent (C);
      end loop;
      return To_String (Result);
   end Full_Name;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (Cursor : Variables_References_Trees.Cursor)
      return Virtual_String is
   begin
      return VSS.Strings.Conversions.To_Virtual_String (Full_Name (Cursor));
   end Full_Name;

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

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access DAP_Variables_View_Record'Class is
   begin
      return DAP_Variables_View (Client.Get_Variables_View);
   end Get_View;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access DAP_Variables_View_Record'Class := null)
   is
      use type Generic_Views.Abstract_View_Access;
   begin
      if Client.Get_Variables_View /= null then
         DAP_Variables_View (Client.Get_Variables_View).On_Process_Terminated;
      end if;

      Client.Set_Variables_View (Generic_Views.Abstract_View_Access (View));
   end Set_View;

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
         if Item.Id /= Unknown_Id then
            declare
               Value : constant JSON_Value := Create_Object;
            begin
               if Item.Cmd /= "" then
                  Value.Set_Field ("tag", "cmd");
                  Value.Set_Field ("value", UTF8 (Item.Cmd));
                  Value.Set_Field ("split", Item.Split_Lines);

               else
                  Value.Set_Field ("tag", "variable");
                  Value.Set_Field ("value", UTF8 (Item.Varname));
                  Value.Set_Field
                    ("format", Value_Format'Image (Convert (Item.Format)));
               end if;
               Append (Values, Value);
            end;
         end if;
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

      It     : Item_Info;
      Values : constant JSON_Array := Value.Get ("value");
   begin
      Trace (Me, "Loading variable view from JSON, has items ?"
             &  Boolean'Image (Length (Values) > 0));

      for Index in 1 .. Length (Values) loop
         declare
            V : constant JSON_Value := Get (Values, Index);
         begin
            if String'(V.Get ("tag")) = "cmd" then
               It :=
                 (Cmd         => VSS.Strings.Conversions.To_Virtual_String
                    (String'(V.Get ("value"))),
                  Split_Lines => V.Get ("split"),
                  Id          => Unknown_Id,
                  others      => <>);
               Self.Items.Append (It);

            elsif String'(V.Get ("tag")) = "variable" then
               It :=
                 (Varname => VSS.Strings.Conversions.To_Virtual_String
                    (String'(V.Get ("value"))),
                  Format  => Convert (Value_Format'Value (V.Get ("format"))),
                  Id      => Unknown_Id,
                  others  => <>);
               Self.Items.Append (It);
            end if;
         end;
      end loop;
   end Load;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Printable_Var_Filter    : Action_Filter;
      Debugger_Stopped_Filter : Action_Filter;
      Not_Connamd_Filter      : Action_Filter;
      Access_Filter           : Action_Filter;
      View_Focused_Filter     : Action_Filter;
      Is_Editable_Filter      : Action_Filter;
      Selection_Filter        : Action_Filter;
      Command                 : Interactive_Command_Access;

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
      Not_Connamd_Filter := Kernel.Lookup_Filter
        ("Debugger not command variable");

      Access_Filter := new Access_Variable_Filter;
      Register_Filter
        (Kernel, Access_Filter, "Debugger variable is access");

      Register_Action
        (Kernel, "debug tree display variable",
         Command => new Tree_Display_Command,
         Description =>
           "Display the value of the variable in the Variables view",
         Filter      => Debugger_Stopped_Filter and Not_Connamd_Filter and
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
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-remove-symbolic",
         Category    => "Debug");

      Register_Action
        (Kernel, "debug tree clear",
         Command     => new Tree_Clear_Command,
         Description =>
           "Remove the display of all variables in the Variables view",
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-clear-symbolic",
         Category    => "Debug");

      Command := new Set_Value_Command;
      Register_Action
        (Kernel, "debug tree set value",
         Command     => Command,
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
         Filter      => View_Focused_Filter and Debugger_Stopped_Filter,
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
         Filter      => Kernel.Lookup_Filter
           ("Debugger inactive or stopped") and Is_Editable_Filter,
         Category    => "Debug");
      Register_Contextual_Menu
        (Kernel,
         Label       => "Debug/Set format for %S",
         Action      => "debug set variable format",
         Group       => DAP_Variables_Contextual_Group);

      Show_Types := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debugger-variables-show-types", True, Label => "Show types");

      Command := new Print_Variable_Command;
      Register_Action
        (Kernel, "debug print variable",
         Command     => Command,
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
   end Register_Module;

end DAP.Views.Variables;
