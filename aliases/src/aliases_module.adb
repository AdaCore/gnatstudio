------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Calendar;             use Ada.Calendar;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Hashed_Maps;

with GNAT.Calendar.Time_IO;    use GNAT.Calendar.Time_IO;
with GNAT.Case_Util;           use GNAT.Case_Util;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GNATCOLL.Arg_Lists;       use GNATCOLL.Arg_Lists;
with GNATCOLL.Templates;       use GNATCOLL.Templates;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with System.Assertions;

with Gdk.Event;                use Gdk.Event;
with Gdk.RGBA;                 use Gdk.RGBA;

with Glib.Object;              use Glib.Object;
with Glib.Types;               use Glib.Types;
with Glib.Unicode;             use Glib.Unicode;
with Glib.Values;              use Glib.Values;
with Glib_Values_Utils;        use Glib_Values_Utils;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;             use Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Separator;            use Gtk.Separator;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Mark;            use Gtk.Text_Mark;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;

with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.Handlers;          use Gtkada.Handlers;

with Commands.Interactive;     use Commands, Commands.Interactive;
with Dialog_Utils;             use Dialog_Utils;
with GPS.Customizable_Modules; use GPS.Customizable_Modules;
with GPS.Dialogs;              use GPS.Dialogs;
with GPS.Intl;                 use GPS.Intl;
with GPS.Environments;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;       use GPS.Kernel.Scripts;
with GPS.Main_Window;          use GPS.Main_Window;
with GUI_Utils;                use GUI_Utils;
with Histories;                use Histories;
with String_Hash;
with String_Utils;             use String_Utils;
with XML_Utils;                use XML_Utils;
with XML_Parsers;
with Glib_String_Utils;        use Glib_String_Utils;

with Aliases_Module.Scripts;

package body Aliases_Module is

   Aliases_Column_Types : constant GType_Array :=
     (0 => GType_String,
      1 => GType_Boolean,
      2 => GType_String);

   Variables_Column_Types : constant GType_Array :=
     (0 => GType_String,
      1 => GType_String,
      2 => GType_Boolean,
      3 => GType_Boolean,
      4 => GType_Boolean,
      5 => GType_Boolean);

   function To_UStr (a : String)
                     return SU.Unbounded_String
                     renames SU.To_Unbounded_String;
   function To_Str (a : SU.Unbounded_String)
                     return String
                     renames SU.To_String;

   Me : constant Trace_Handle := Create ("GPS.OTHERS.ALIASES");

   Special : constant Character := '%';

   Highlight_Color : constant String := "#DD0000";
   --  Color used to highlight special entities in the expansion

   package Implements_Editable is new Glib.Types.Implements
     (Gtk.Editable.Gtk_Editable, GObject_Record, GObject);
   function "+"
     (Widget : access GObject_Record'Class)
      return Gtk.Editable.Gtk_Editable
      renames Implements_Editable.To_Interface;

   type Interactive_Alias_Expansion_Command is new Interactive_Command with
      null record;
   overriding function Execute
     (Command : access Interactive_Alias_Expansion_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;

   package Aliases_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => SU.Unbounded_String,
      Element_Type    => Alias_Type,
      Hash            => SU.Hash_Case_Insensitive,
      Equivalent_Keys => SU.Equal_Case_Insensitive);
   use Aliases_Map;

   type Expansion_Function_Record;
   type Expansion_Function_List is access Expansion_Function_Record;
   type Expansion_Function_Record (Length : Natural) is record
      Func : Alias_Expansion_Function;
      Next : Expansion_Function_List;
      Descr : String (1 .. Length);
   end record;

   type Expansion_Function_Array is array (Character) of
     Expansion_Function_List;

   procedure Do_Nothing (Var : in out Boolean);
   package Boolean_Hash is new String_Hash (Boolean, Do_Nothing, False);

   type Aliases_Module_Id_Record is new Module_ID_Record with record
      Aliases      : Aliases_Map.Map;
      Module_Funcs : Expansion_Function_Array;
      Expanded     : Boolean_Hash.String_Hash_Table.Instance;
   end record;
   type Aliases_Module_Id_Access is access all Aliases_Module_Id_Record'Class;

   overriding procedure Destroy (Module : in out Aliases_Module_Id_Record);
   overriding procedure Customize
     (Module : access Aliases_Module_Id_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   Aliases_Module_Id : Aliases_Module_Id_Access;

   type Alias_Editor_Record is new GPS_Dialog_Record with record
      Local_Aliases         : Aliases_Map.Map;
      Aliases               : Gtk_Tree_View;
      Aliases_Model         : Gtk_Tree_Store;
      Variables             : Gtk_Tree_View;
      Variables_Model       : Gtk_Tree_Store;
      Expansion             : Gtk_Text_View;
      Alias_Col             : Gtk_Tree_View_Column;
      Show_Read_Only        : Gtk_Check_Button;
      Must_Reindent         : Gtk_Check_Button;
      Is_New_Interactive    : Boolean := False;

      Current_Var           : String_Access;
      Highlight_Tag         : Gtk_Text_Tag;

      Expansion_Inserted_Cb : Gtk.Handlers.Handler_Id;
      Expansion_Deleted_Cb  : Gtk.Handlers.Handler_Id;
   end record;
   type Alias_Editor is access all Alias_Editor_Record'Class;

   procedure Gtk_New
     (Editor : out Alias_Editor; Kernel : access Kernel_Handle_Record'Class);
   --  Initialize the aliases editor

   type String_Menu_Item_Record (Length : Natural) is new Gtk_Menu_Item_Record
     with record
        View    : Gtk_Text_View;
        Special : String (1 .. Length);
     end record;
   type String_Menu_Item is access all String_Menu_Item_Record'Class;

   procedure Gtk_New
     (Item           : out String_Menu_Item;
      View           : access Gtk_Text_View_Record'Class;
      Label, Special : String);
   --  Create a new String_Menu_Item

   procedure Update_Contents (Editor : access Alias_Editor_Record'Class);
   --  Update the contents of the editor, based on the list of currently
   --  defined aliases

   type Alias_Option_Check_Record is new Gtk_Check_Button_Record with record
      Option : access Alias_Option_Type;
   end record;
   type Alias_Option_Check is access all Alias_Option_Check_Record'Class;

   procedure On_Toggled (Self : access Gtk_Toggle_Button_Record'Class);

   procedure Find_Current_Entity
     (Text        : String;
      Current_Pos : Integer;
      First, Last : out Integer);
   --  Set First .. Last to the beginning and end of the current alias name.
   --  If no alias name was found, set First > Last.
   --  ??? Should accept unicode

   procedure Parse_File
     (Kernel    : access Kernel_Handle_Record'Class;
      Filename  : Virtual_File;
      Read_Only : Boolean);
   --  Load a filename, and make the resulting aliases Read_Only

   procedure Save_Aliases (Kernel : access Kernel_Handle_Record'Class);
   --  Save the aliases in filename

   type Edit_Aliases_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Edit_Aliases_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the aliases edition

   procedure Alias_Selection_Changed
     (Editor : access Gtk_Widget_Record'Class);
   --  Called when a new alias was selected in the editor

   procedure Save_Current_Var (Editor : access Alias_Editor_Record'Class);
   --  Save the value of the currently edited variable in the local aliases
   --  table.

   procedure Update_Aliases (Editor : access Alias_Editor_Record'Class);
   --  Merge the local aliases into the global table

   procedure Alias_Renamed (Editor : access Gtk_Widget_Record'Class);
   --  Called when an alias was renamed

   procedure Alias_Deleted (Editor : access Gtk_Widget_Record'Class);
   --  Deletes an alias

   procedure Alias_Created (Editor : access Gtk_Widget_Record'Class);
   --  Creates a new alias

   function Get_Value
     (Editor : access Alias_Editor_Record'Class;
      Name   : String) return Aliases_Map.Cursor;
   --  Get the current value of the variable (includes checking in the local
   --  aliases).

   procedure Add_New_Alias
     (Editor    : access Alias_Editor_Record'Class;
      Name      : String;
      Selected  : Boolean := False;
      Read_Only : Boolean := False;
      Is_New_Interactive : Boolean := False);
   --  Add a new entry in the aliases list of the editor

   procedure Param_Env_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when a variable is changed from environment variable to standard
   --  variable.

   type Param_Substitution is record
      Param : Alias_Param_Type;
      Ent   : Gtk_Entry;
   end record;
   --  Widget used to store the current value for parameters: Ent contains the
   --  widget used by the user to edit the value.
   package Params_Subst_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Param_Substitution);

   function Substitute_Params
     (Text                 : String;
      Params_Substitutions : Alias_Parameter_Substitution_Map.Map)
      return String;
   --  Compute the replacement string for Text, given the current parameter
   --  values.

   procedure Expansion_Inserted
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when some new text has been inserted in Editor.Expansion

   procedure Expansion_Deleted
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when some text has been removed from Editor.Expansion

   procedure Highlight_Expansion_Range
     (Editor : access Alias_Editor_Record'Class; First, Last : Gtk_Text_Iter);
   --  Highlight part of Editor.Expansion

   procedure Set_Variable
     (Editor   : access Alias_Editor_Record'Class;
      Name     : String;
      Default  : String;
      From_Env : Boolean;
      Editable : Boolean := True);
   --  Add a new variable in the variable editor

   procedure Update_Vars (Editor : access Alias_Editor_Record'Class);
   --  Parse the expansion of the alias, and update the list of parameters

   procedure Find_Next_Parameter
     (Editor       : access Alias_Editor_Record'Class;
      Start        : out Gtk_Text_Iter;
      Current      : in out Gtk_Text_Iter;
      Last         : Gtk_Text_Iter;
      All_Specials : Boolean);
   --  Starting from Current, find the occurrence of the next parameter
   --  reference in Editor.Expansion. On exit, Start .. Current points to
   --  "$(..)", or Is_End (Start) is true if there are no more references.
   --  Last is the last character we want to check, instead of stopping at the
   --  end of the buffer.
   --  If All_Specials is true, then all special entities are returned, not
   --  only the references to parameters.

   function Find_And_Replace_Cursor
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Alias         : Alias_Type;
      Str           : String;
      Cursor        : out Integer;
      Offset_Column : Gint := 0) return String;
   --  Find the position of the cursor in Str, and returns the new Str when
   --  the special entity has been removed

   function Contextual_Factory
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Gtk_Menu;
   --  Create the contextual menu for the expansion edition area

   procedure Insert_Special (Item : access Gtk_Widget_Record'Class);
   --  Insert the special entity associated with Item

   function Special_Entities
     (Kernel    : access Kernel_Handle_Record'Class;
      Expansion : String;
      Special   : Character)
      return String;
   --  Provide expansion for some of the special entities

   procedure Show_Read_Only_Toggled (Editor : access Gtk_Widget_Record'Class);
   --  Called when the "show read-only" toggle is changed

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Node_Ptr;
      Level  : Customization_Level;
      Read_Only : Boolean);
   --  Called when a new customization in parsed

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Alias : Alias_Type) return String
   is
     (To_Str (Alias.Name));

   -------------------
   -- Get_Expansion --
   -------------------

   function Get_Expansion (Alias : Alias_Type) return String
   is
     (To_Str (Alias.Expansion));

   ------------------
   -- Expand_Macro --
   ------------------

   function Expand_Macro (Alias : Alias_Type) return String is
      Cursor : Integer;
      Result : constant String := Find_And_Replace_Cursor
        (Kernel        => Aliases_Module_Id.Get_Kernel,
         Alias         => Alias,
         Str           => To_Str (Alias.Expansion),
         Cursor        => Cursor);
   begin
      if Cursor = Result'Length then
         return Result;
      else
         return Result (1 .. Cursor) & "%_"
           & Result (Cursor + 1 .. Result'Last);
      end if;
   end Expand_Macro;

   -------------------------
   -- Has_Same_Parameters --
   -------------------------

   function Has_Same_Parameters
     (Left, Right : Alias_Type) return Boolean
   is
      use Ada.Containers;
      use Params_List;
      use SU;

      Left_C, Right_C : Params_List.Cursor;
   begin
      if Left.Params.Length /= Right.Params.Length then
         return False;
      end if;

      Left_C := Left.Params.First;
      Right_C := Right.Params.First;

      while Left_C /= Params_List.No_Element loop
         if Element (Left_C).Name /= Element (Right_C).Name then
            return False;
         end if;

         Next (Left_C);
         Next (Right_C);
      end loop;

      return True;
   end Has_Same_Parameters;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Param : Alias_Param_Type) return String
   is
     (To_Str (Param.Name));

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description (Param : Alias_Param_Type) return String
   is
      (To_Str (Param.Description));

   -----------------------
   -- Get_Default_Value --
   -----------------------

   function Get_Default_Value
     (Alias : Alias_Type; Name : String) return String
   is
      use type SU.Unbounded_String;
      Env : constant GPS.Environments.Environment :=
        Aliases_Module_Id.Get_Kernel.Get_Environment;
   begin
      for Param of Alias.Params loop
         if Param.Name = Name then
            if Param.From_Env and then Env.Has_Element (Name) then
               return Env.Value (Name);
            else
               return To_Str (Param.Initial);
            end if;
         end if;
      end loop;

      return "";
   end Get_Default_Value;

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (Var : in out Boolean) is
      pragma Unreferenced (Var);
   begin
      null;
   end Do_Nothing;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable
     (Editor   : access Alias_Editor_Record'Class;
      Name     : String;
      Default  : String;
      From_Env : Boolean;
      Editable : Boolean := True)
   is
      procedure Set
        (Tree, Iter : System.Address;
         Col1  : Gint := 0; Name : String;
         Col2  : Gint := 1; Initial : String;
         Col3  : Gint := 2; Editable : Gboolean := 1);
      pragma Import (C, Set, "ada_gtk_tree_store_set_ptr_ptr_int");

      procedure Set2
        (Tree, Iter : System.Address;
         Col4  : Gint := 3; From_Env : Gboolean := 0;
         Col5  : Gint := 4; Seen : Gboolean := 1;
         Col6  : Gint := 5; Activable : Gboolean := 1);
      pragma Import (C, Set2, "ada_gtk_tree_store_set_int_int_int");

      Iter : Gtk_Tree_Iter;

   begin
      Append (Editor.Variables_Model, Iter, Null_Iter);
      Set
        (Get_Object (Editor.Variables_Model), Iter'Address,
         Name      => Name & ASCII.NUL,
         Initial   => Default & ASCII.NUL,
         Editable  => Boolean'Pos (Editable and then not From_Env));
      Set2
        (Get_Object (Editor.Variables_Model), Iter'Address,
         From_Env  => Boolean'Pos (From_Env),
         Activable => Boolean'Pos (Editable));
   end Set_Variable;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Aliases_Module_Id_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Expansion_Function_Record, Expansion_Function_List);

      Tmp : Expansion_Function_List;

   begin
      Module.Aliases.Clear;

      for C in Module.Module_Funcs'Range loop
         while Module.Module_Funcs (C) /= null loop
            Tmp := Module.Module_Funcs (C).Next;
            Unchecked_Free (Module.Module_Funcs (C));
            Module.Module_Funcs (C) := Tmp;
         end loop;
      end loop;
   end Destroy;

   ------------------
   -- Save_Aliases --
   ------------------

   procedure Save_Aliases (Kernel : access Kernel_Handle_Record'Class) is
      Filename          : constant Virtual_File :=
                            Create_From_Dir (Get_Home_Dir (Kernel), "aliases");
      File, XML_Key, Child  : Node_Ptr;
      Iter              : Aliases_Map.Cursor :=
        Aliases_Module_Id.Aliases.First;
      Value             : Alias_Type;
      Success           : Boolean;

      use Aliases_Map;
   begin
      File := new Node;
      File.Tag := new String'("Aliases");

      while Iter /= No_Element loop

         Value := Element (Iter);

         --  We only save user-defined aliases, not the ones that are defined
         --  in the standard system files.

         if not Value.Read_Only then
            XML_Key := new Node;
            XML_Key.Tag := new String'("alias");
            Set_Attribute (XML_Key, "name", SU.To_String (Key (Iter)));

            if Value.Must_Reindent then
               Set_Attribute (XML_Key, "indent", "true");
            end if;

            Child := new Node;
            Child.Tag := new String'("text");
            Child.Value := new String'(Strip_CR (To_Str (Value.Expansion)));
            Add_Child (XML_Key, Child);

            for P of Value.Params loop
               Child := new Node;
               Child.Tag := new String'("param");
               Set_Attribute (Child, "name", To_Str (P.Name));

               if P.From_Env then
                  Set_Attribute (Child, "environment", "true");
               end if;

               Child.Value := new String'(To_Str (P.Initial));
               Add_Child (XML_Key, Child);
            end loop;

            Add_Child (File, XML_Key);
         end if;

         Next (Iter);
      end loop;

      Print (File, Filename, Success);
      Free (File);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      end if;
   end Save_Aliases;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File
     (Kernel    : access Kernel_Handle_Record'Class;
      Filename  : Virtual_File;
      Read_Only : Boolean)
   is
      File : Node_Ptr;
      Err : String_Access;
   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename.Display_Full_Name);
         XML_Parsers.Parse (Filename, File, Err);
         if File /= null then
            Customize (Kernel, File.Child, User_Specific, Read_Only);
            Free (File);
         else
            Insert (Kernel, Err.all, Mode => Error);
            Free (Err);
         end if;
      else
         Trace (Me, "No such file: " & Filename.Display_Full_Name);
      end if;

   exception
      when System.Assertions.Assert_Failure =>
         Insert (Kernel, "Invalid format for " & Filename.Display_Full_Name,
                 Mode => Error);
         Free (File);

      when Status_Error | Name_Error =>
         Trace (Me, "No aliases file " & Filename.Display_Full_Name);

      when E : others =>
         Trace (Me, E);
         Free (File);
   end Parse_File;

   -------------------------
   -- Find_Current_Entity --
   -------------------------

   procedure Find_Current_Entity
     (Text        : String;
      Current_Pos : Integer;
      First, Last : out Integer) is
   begin
      if Current_Pos < Text'First then
         First := Current_Pos + 1;
         Last  := Current_Pos;
         return;
      end if;

      Last := Current_Pos;

      while Last <= Text'Last
        and then Is_Entity_Letter (UTF8_Get_Char (Text (Last .. Text'Last)))
      loop
         Last := UTF8_Find_Next_Char (Text, Last);
      end loop;

      First := Current_Pos;

      loop
         while First >= Text'First
           and then Is_Entity_Letter
             (UTF8_Get_Char (Text (First .. Text'Last)))
         loop
            First := UTF8_Find_Prev_Char (Text, First);
         end loop;

         if First < Text'First then
            First := Text'First;
            return;
         end if;

         exit when Aliases_Module_Id.Aliases.Contains
           (To_UStr (Text (UTF8_Find_Next_Char (Text, First) .. Last - 1)));

         while First >= Text'First
           and then not Is_Entity_Letter
             (UTF8_Get_Char (Text (First .. Text'Last)))
         loop
            First := UTF8_Find_Prev_Char (Text, First);
         end loop;
      end loop;

      First := UTF8_Find_Next_Char (Text, First);
   end Find_Current_Entity;

   -----------------------
   -- Substitute_Params --
   -----------------------

   function Substitute_Params
     (Text                 : String;
      Params_Substitutions : Alias_Parameter_Substitution_Map.Map)
      return String
   is
      use Alias_Parameter_Substitution_Map;
      Count      : Integer;
      Length     : constant Integer :=
                     Integer (Params_Substitutions.Length) + 1;
      Substrings : Substitution_Array (1 .. Length);
      Param_C    : Alias_Parameter_Substitution_Map.Cursor :=
                     Params_Substitutions.First;
   begin
      Count := Substrings'First + 1;

      --  Preserve %%, since Find_And_Replace_Cursor does a second expansion
      --  phase

      Substrings (Substrings'First) :=
        (Name  => new String'("" & Special),
         Value => new String'("" & Special & Special));

      while Param_C /= Alias_Parameter_Substitution_Map.No_Element loop
         Substrings (Count) :=
           (Name  =>
               new String'(Key (Param_C)),
            Value =>
               new String'(Element (Param_C)));
         Count := Count + 1;
         Next (Param_C);
      end loop;

      declare
         Val : constant String := Substitute
           (Str        => Text,
            Delimiter  => Special,
            Substrings => Substrings);
      begin
         Free (Substrings);
         return Val;
      end;
   end Substitute_Params;

   -----------------------------
   -- Find_And_Replace_Cursor --
   -----------------------------

   function Find_And_Replace_Cursor
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Alias         : Alias_Type;
      Str           : String;
      Cursor        : out Integer;
      Offset_Column : Gint := 0) return String is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Name   : constant String := Get_Name (Alias);
      Tmp    : Expansion_Function_List;
      Result : Ada.Strings.Unbounded.Unbounded_String;
      First  : Natural := Str'First;
      S      : Natural := Str'First;
      Found  : Boolean;

   begin
      --  Prevent recursion in alias expansion

      if Boolean_Hash.String_Hash_Table.Get
        (Aliases_Module_Id.Expanded, Name)
      then
         return Str;
      end if;

      Boolean_Hash.String_Hash_Table.Set
        (Aliases_Module_Id.Expanded, Name, True);

      Cursor := 0;

      while S <= Str'Last - 1 loop
         if Str (S) = Special then
            Result := Result & Str (First .. S - 1);
            First := S + 2;

            if Str (S + 1) = '_' then
               Cursor := Ada.Strings.Unbounded.Length (Result);

            elsif Str (S + 1) = Special then
               Result := Result & Special;

            else
               Tmp := Aliases_Module_Id.Module_Funcs (Str (S + 1));
               Found := False;

               while Tmp /= null loop
                  declare
                     Replace : constant String := Tmp.Func
                       (Kernel,
                        Ada.Strings.Unbounded.To_String (Result),
                        Str (S + 1));
                  begin
                     if Replace /= Invalid_Expansion then
                        Result := To_UStr (Replace);
                        Found := True;
                        exit;
                     end if;
                  end;

                  Tmp := Tmp.Next;
               end loop;

               if not Found then
                  Result := Result & Special & Str (S + 1);
               end if;
            end if;

            S := S + 2;

            --  Preserve the indentation as set in the expansion of aliases

         elsif Str (S) = ASCII.LF then
            Result := Result & Str (First .. S - 1)
              & ASCII.LF & (1 .. Integer (Offset_Column) => ' ');
            S := S + 1;
            First := S;

         else
            S := S + 1;
         end if;
      end loop;

      if First <= Str'Last then
         Result := Result & Str (First .. Str'Last);
      end if;

      Boolean_Hash.String_Hash_Table.Set
        (Aliases_Module_Id.Expanded, Name, False);

      if Cursor = 0 then
         Cursor := Ada.Strings.Unbounded.Length (Result);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Find_And_Replace_Cursor;

   ------------------
   -- Expand_Alias --
   ------------------

   function Expand_Alias
     (Alias                : Alias_Type;
      Kernel               : not null access Kernel_Handle_Record'Class;
      Cursor               : out Integer;
      Must_Reindent        : out Boolean;
      Params_Substitutions : out Alias_Parameter_Substitution_Map.Map;
      Offset_Column        : Gint := 0;
      Dialog_Title         : String := "Alias Parameters Selection";
      Option               : access Alias_Option_Type := null)
      return String
   is
      Values       : Params_Subst_List.List;
      Dialog       : GPS_Dialog;
      Main_View    : Dialog_View;
      Group_Widget : Dialog_Group_Widget;
      Button       : Gtk_Widget;
      Val          : String_Access;
      Ent          : Gtk_Entry;
   begin
      Must_Reindent := False;

      if Alias = No_Alias then
         return "";
      else
         Must_Reindent := Alias.Must_Reindent;
         if Alias.Params.Is_Empty then
            return Find_And_Replace_Cursor
              (Kernel,
               Alias         => Alias,
               Str           => To_Str (Alias.Expansion),
               Cursor        => Cursor,
               Offset_Column => Offset_Column);
         else
            for P of Alias.Params loop
               if Dialog = null then
                  Gtk_New (Dialog,
                           Title  => Dialog_Title,
                           Kernel => Kernel,
                           Flags  => Destroy_With_Parent);
                  Set_Default_Size_From_History
                    (Win    => Dialog,
                     Name   => "alias_dialog",
                     Kernel => Kernel,
                     Width  => 400,
                     Height => 200);

                  Button := Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
                  Grab_Default (Button);
                  Button :=
                    Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);

                  Main_View := new Dialog_View_Record;
                  Dialog_Utils.Initialize (Main_View);
                  Dialog.Get_Content_Area.Pack_Start (Main_View);

                  Group_Widget := new Dialog_Group_Widget_Record;
                  Initialize
                    (Group_Widget,
                     Parent_View         => Main_View,
                     Group_Name          => "Parameters",
                     Allow_Multi_Columns => True);
               end if;

               Gtk_New (Ent);
               Ent.Set_Activates_Default (True);

               declare
                  Label : String := P.Get_Name;
               begin
                  To_Mixed (Label);
                  Group_Widget.Create_Child
                    (Widget => Ent,
                     Label  => Label,
                     Doc    => P.Get_Description);
               end;

               if P.From_Env then
                  Val := Getenv (To_Str (P.Name));
                  Ent.Set_Text (Val.all);
                  Free (Val);
               else
                  Ent.Set_Text (To_Str (P.Initial));
               end if;

               Values.Append ((Param => P, Ent => Ent));
            end loop;

            if Dialog /= null then

               if Option /= null and then Option.all /= No_Option then
                  declare
                     Option_Check : Alias_Option_Check;
                  begin
                     Group_Widget := new Dialog_Group_Widget_Record;
                     Initialize
                       (Group_Widget,
                        Parent_View         => Main_View,
                        Allow_Multi_Columns => False);

                     Option_Check := new Alias_Option_Check_Record;
                     Gtk.Check_Button.Initialize
                       (Option_Check,
                        Label => SU.To_String (Option.Label));
                     Option_Check.Set_Active (Option.Enabled);
                     Option_Check.Option := Option;
                     Option_Check.On_Toggled (On_Toggled'Access);

                     Group_Widget.Create_Child
                       (Option_Check,
                        Doc => SU.To_String (Option.Doc));
                  end;
               end if;

               Dialog.Show_All;

               --  Give the focus to the first entry, if any
               if not Values.Is_Empty then
                  Values.First_Element.Ent.Grab_Focus;
               end if;

               if Dialog.Run /= Gtk_Response_OK then
                  Destroy (Dialog);
                  return "";
               end if;
            end if;

            for Value of Values loop
               Params_Substitutions.Include
                 (To_Str (Value.Param.Name),
                  Value.Ent.Get_Text);
            end loop;

            declare
               Val                  : constant String := Substitute_Params
                 (Text                 => To_Str (Alias.Expansion),
                  Params_Substitutions => Params_Substitutions);
            begin
               if Dialog /= null then
                  Dialog.Destroy;
               end if;

               return Find_And_Replace_Cursor
                 (Kernel        => Kernel,
                  Alias         => Alias,
                  Str           => Val,
                  Cursor        => Cursor,
                  Offset_Column => Offset_Column);
            end;
         end if;
      end if;
   end Expand_Alias;

   ------------------
   -- Expand_Alias --
   ------------------

   function Expand_Alias
     (Alias         : Alias_Type;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Cursor        : out Integer;
      Must_Reindent : out Boolean;
      Offset_Column : Gint := 0;
      Dialog_Title  : String := "Alias Parameters Selection")
      return String
   is
      Params_Substitutions : Alias_Parameter_Substitution_Map.Map;
   begin
      return Expanded_Text : constant String :=
        Expand_Alias
          (Alias                => Alias,
           Kernel               => Kernel,
           Cursor               => Cursor,
           Must_Reindent        => Must_Reindent,
           Params_Substitutions => Params_Substitutions,
           Offset_Column        => Offset_Column,
           Dialog_Title         => Dialog_Title)
      do
         Params_Substitutions.Clear;
      end return;
   end Expand_Alias;

   ------------------------------
   -- Expand_Alias_With_Values --
   ------------------------------

   function Expand_Alias_With_Values
     (Alias                : Alias_Type;
      Kernel               : not null access Kernel_Handle_Record'Class;
      Params_Substitutions : Alias_Parameter_Substitution_Map.Map;
      Cursor               : out Integer;
      Offset_Column        : Gint := 0)
      return String
   is
      Str : constant String := Substitute_Params
        (Text                 => SU.To_String (Alias.Expansion),
         Params_Substitutions => Params_Substitutions);
   begin
         return Find_And_Replace_Cursor
           (Kernel        => Kernel,
            Alias         => Alias,
            Str           => Str,
            Cursor        => Cursor,
            Offset_Column => Offset_Column);
   end Expand_Alias_With_Values;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Interactive_Alias_Expansion_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      W         : constant Gtk_Widget := Get_Current_Focus_Widget (Kernel);
      Had_Focus : Boolean;

   begin
      Boolean_Hash.String_Hash_Table.Reset (Aliases_Module_Id.Expanded);

      if W /= null
        and then Is_A (W.Get_Type, Gtk.Editable.Get_Type)
      then
         if Get_Editable (+W) then
            declare
               Text        : constant String := Get_Chars (+W, 0);
               First, Last : Integer;
            begin
               Find_Current_Entity
                 (Text, Integer (Get_Position (+W)),
                  First, Last);

               if First > Last then
                  return Failure;
               end if;

               declare
                  Cursor        : Integer;
                  Must_Reindent : Boolean;
                  Replace : constant String := Expand_Alias
                    (Alias         => Get_Alias (Text (First .. Last - 1)),
                     Kernel        => Kernel,
                     Cursor        => Cursor,
                     Must_Reindent => Must_Reindent);
                  F       : Gint := Gint (First - Text'First);
                  Back    : constant Glong := Glib.Unicode.UTF8_Strlen
                    (Replace (Cursor + 1 .. Replace'Last));
               begin
                  if Replace /= "" then
                     Delete_Text
                       (+W,
                        F,
                        Gint (Last - Text'First));
                     Insert_Text (+W, Replace, F);
                     Set_Position (+W, F - Gint (Back));
                  end if;
               end;

               return Commands.Success;
            end;
         end if;

      elsif W /= null and then W.all in Gtk_Text_View_Record'Class then
         if Get_Editable (Gtk_Text_View (W)) then
            declare
               Buffer     : constant Gtk_Text_Buffer :=
                              Get_Buffer (Gtk_Text_View (W));
               First_Iter : Gtk_Text_Iter;
               Last_Iter  : Gtk_Text_Iter;
               Line_Start : Gtk_Text_Iter;
               Success    : Boolean := True;

            begin
               Get_Iter_At_Mark (Buffer, First_Iter, Get_Insert (Buffer));
               Search_Entity_Bounds (First_Iter, Last_Iter);

               Copy (Source => First_Iter, Dest => Line_Start);
               Set_Line_Index (Line_Start, 0);

               while Success
                 and then Compare (Line_Start, First_Iter) <= 0
                 and then not Aliases_Module_Id.Aliases.Contains
                   (To_UStr (Get_Slice (Buffer, First_Iter, Last_Iter)))
               loop
                  Backward_Word_Start (First_Iter, Success);
               end loop;

               declare
                  Cursor        : Integer;
                  Must_Reindent : Boolean;
                  Column        : constant Gint :=
                                    Get_Line_Offset (First_Iter);
                  Replace       : constant String :=
                                    Expand_Alias
                                      (Alias         =>
                                         Get_Alias (Get_Slice
                                         (Buffer, First_Iter, Last_Iter)),
                                       Kernel        => Kernel,
                                       Cursor        => Cursor,
                                       Must_Reindent => Must_Reindent,
                                       Offset_Column => Column);
                  Back          : constant Glong := Glib.Unicode.UTF8_Strlen
                                    (Replace (Cursor + 1 .. Replace'Last));

                  Mark          : Gtk_Text_Mark;
                  Result        : Boolean;
                  Event         : Gdk_Event;
                  Count         : Natural := 0;
                  Index         : Natural := Replace'First;
                  Start_Line    : constant Integer :=
                                    Integer (Get_Line (First_Iter));
               begin
                  if Replace /= "" then
                     Had_Focus := W.Has_Focus;

                     --  Simulate a focus_in/focus_out event, needed for the
                     --  GPS source editor, which saves and restores the
                     --  cursor position when the focus changes (for the
                     --  handling of multiple views).

                     if not Had_Focus then
                        Gdk_New (Event, Enter_Notify);
                        Event.Crossing.Window := Get_Window (W);
                        Result := Return_Callback.Emit_By_Name
                          (W, Signal_Focus_In_Event, Event);

                        --  Avoid unreferencing window
                        Event.Crossing.Window := null;

                        Free (Event);
                     end if;

                     Delete (Buffer, First_Iter, Last_Iter);
                     Insert (Buffer, First_Iter, Replace);
                     Backward_Chars (First_Iter,
                                     Gint (Back),
                                     Result);
                     Place_Cursor (Buffer, First_Iter);

                     --  Reindent the current editor. Since we have given the
                     --  focus to the widget, the call to the shell command
                     --  will have no effect unless this is really an editor.

                     if Must_Reindent then
                        Mark := Create_Mark (Buffer, Where => First_Iter);

                        while Index <= Replace'Last loop
                           Index := Next_Line (Replace, Index) + 1;
                           Count := Count + 1;
                        end loop;

                        declare
                           CL : Arg_List;
                        begin
                           CL := Create ("Editor.select_text");
                           Append_Argument
                             (CL, Image (Start_Line + 1), One_Arg);
                           Append_Argument
                             (CL, Image (Start_Line + Count), One_Arg);
                           Execute_GPS_Shell_Command (Kernel, CL);
                        end;

                        Execute_GPS_Shell_Command
                          (Kernel, CL => Create ("Editor.indent"));

                        Get_Iter_At_Mark (Buffer, First_Iter, Mark);
                        Place_Cursor (Buffer, First_Iter);
                        Delete_Mark (Buffer, Mark);
                     end if;

                     if not Had_Focus then
                        Gdk_New (Event, Leave_Notify);
                        Event.Crossing.Window := Get_Window (W);
                        Result := Return_Callback.Emit_By_Name
                          (W, Signal_Focus_Out_Event, Event);

                        --  Avoid unreferencing window
                        Event.Crossing.Window := null;

                        Free (Event);
                     end if;
                  end if;
               end;

               return Commands.Success;
            end;
         end if;
      end if;

      return Failure;

   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Execute;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Editor : access Alias_Editor_Record'Class;
      Name   : String) return Aliases_Map.Cursor
   is
      U_Name : constant SU.Unbounded_String := To_UStr (Name);
      Cursor : constant Aliases_Map.Cursor
        := Editor.Local_Aliases.Find (U_Name);
   begin
      return (if Cursor = No_Element
              then Aliases_Module_Id.Aliases.Find (U_Name)
              else Cursor);
   end Get_Value;

   ----------------------
   -- Save_Current_Var --
   ----------------------

   procedure Save_Current_Var (Editor : access Alias_Editor_Record'Class) is
      Buffer      : constant Gtk_Text_Buffer := Get_Buffer (Editor.Expansion);
      Start, Last : Gtk_Text_Iter;
      Params      : Params_List.List;
      Iter        : Gtk_Tree_Iter;
   begin
      if Editor.Current_Var /= null then
         declare
            Alias_Name : constant SU.Unbounded_String :=
                           To_UStr (Editor.Current_Var.all);
            Alias      : constant Alias_Type :=
                           Get_Alias (Editor.Current_Var.all);
            Read_Only  : constant Boolean :=
                           (Alias /= No_Alias and then Alias.Read_Only);
         begin
            --  Don't save anything from the GUI if the selected alias is
            --  read-only.
            if Read_Only then
               return;
            end if;

            Iter := Get_Iter_First (Editor.Variables_Model);

            while Iter /= Null_Iter loop
               declare
                  Name     : constant String :=
                               Get_String (Editor.Variables_Model, Iter, 0);
                  Initial  : constant String :=
                               Get_String (Editor.Variables_Model, Iter, 1);
                  From_Env : constant Boolean :=
                               Get_Boolean (Editor.Variables_Model, Iter, 3);

               begin
                  Params.Append
                    ((Name        => To_UStr (Name),
                      Description => SU.Null_Unbounded_String,
                      Initial     => To_UStr (Initial),
                      From_Env    => From_Env));
               end;

               Next (Editor.Variables_Model, Iter);
            end loop;

            Get_Start_Iter (Buffer, Start);
            Get_End_Iter   (Buffer, Last);

            Editor.Local_Aliases.Include
              (Alias_Name,
               (Name          => Alias_Name,
                Expansion     => To_UStr (Buffer.Get_Text (Start, Last)),
                Params        => Params,
                Read_Only     => False,
                Must_Reindent => Get_Active (Editor.Must_Reindent)));
         end;
      end if;
   end Save_Current_Var;

   -----------------------------
   -- Alias_Selection_Changed --
   -----------------------------

   procedure Alias_Selection_Changed
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed               : constant Alias_Editor := Alias_Editor (Editor);
      Model            : Gtk_Tree_Model;
      Iter             : Gtk_Tree_Iter;
      Expansion_Buffer : constant Gtk_Text_Buffer := Ed.Expansion.Get_Buffer;
      Cursor           : Aliases_Map.Cursor := No_Element;
      Start, Last      : Gtk_Text_Iter;

   begin
      Save_Current_Var (Ed);

      Free (Ed.Current_Var);

      Get_Selected (Get_Selection (Ed.Aliases), Model, Iter);

      if Iter /= Null_Iter then
         declare
            Name : constant String := Get_String (Ed.Aliases_Model, Iter, 0);
         begin
            Cursor := Get_Value (Ed, Name);
            Ed.Current_Var := new String'(Name);
         end;
      end if;

      --  Block temporarly the handlers on the expansion text view buffer since
      --  we retrieve the alias parameters from the element that was found and
      --  not from the text itself.
      Gtk.Handlers.Handler_Block (Expansion_Buffer, Ed.Expansion_Inserted_Cb);
      Gtk.Handlers.Handler_Block (Expansion_Buffer, Ed.Expansion_Deleted_Cb);

      --  Clear the variable tree model before inserting the selected alias
      --  parameters in it.
      Ed.Variables_Model.Clear;

      if Cursor = No_Element then
         Expansion_Buffer.Set_Text ("");
         Ed.Must_Reindent.Set_Active (False);
         Ed.Expansion.Set_Editable (Ed.Is_New_Interactive);
      else
         declare
            Alias : constant Alias_Type := Element (Cursor);
         begin
            Expansion_Buffer.Set_Text (To_Str (Alias.Expansion));
            Ed.Must_Reindent.Set_Active (Alias.Must_Reindent);
            Ed.Expansion.Set_Editable (not Alias.Read_Only);

            for P of Alias.Params loop
               Ed.Set_Variable
                 (Name     => To_Str (P.Name),
                  Default  => To_Str (P.Initial),
                  From_Env => P.From_Env,
                  Editable => not Alias.Read_Only);
            end loop;
         end;
      end if;

      Expansion_Buffer.Get_Bounds (Start, Last);
      Ed.Highlight_Expansion_Range (Start, Last);

      --  Unblock the handlers on the expansion text view buffer
      Gtk.Handlers.Handler_Unblock
        (Expansion_Buffer, Ed.Expansion_Inserted_Cb);
      Gtk.Handlers.Handler_Unblock
        (Expansion_Buffer, Ed.Expansion_Deleted_Cb);

   exception
      when E : others => Trace (Me, E);
   end Alias_Selection_Changed;

   -------------------
   -- Alias_Renamed --
   -------------------

   procedure Alias_Renamed (Editor : access Gtk_Widget_Record'Class) is
      Ed      : constant Alias_Editor := Alias_Editor (Editor);
      Model   : Gtk_Tree_Model;
      Iter    : Gtk_Tree_Iter;
      Message : Message_Dialog_Buttons;
      pragma Unreferenced (Message);

   begin
      if Ed.Current_Var /= null then
         Get_Selected (Get_Selection (Ed.Aliases), Model, Iter);

         if Iter /= Null_Iter then
            declare
               Old   : constant String := Ed.Current_Var.all;
               Name  : constant String :=
                 Ed.Aliases_Model.Get_String (Iter, 0);
               Cursor : constant Aliases_Map.Cursor := Get_Value (Ed, Old);
            begin
               if Name'Length = 0
                 or else not Is_Entity_Letter
                   (UTF8_Get_Char (Name (Name'First .. Name'Last)))
               then
                  Ed.Aliases_Model.Set (Iter, 0, Old);
                  Message := GPS_Message_Dialog
                    (Msg => -"Error: invalid name for alias: " & Name
                     & ASCII.LF
                     & (-"Alias names must start with a letter"),
                     Dialog_Type => Error,
                     Buttons => Button_OK,
                     Title => -"Invalid alias name",
                     Parent => Gtk_Window (Editor));
                  return;
               end if;

               if Name /= Old then
                  if Cursor /= No_Element then
                     Ed.Local_Aliases.Include
                       (To_UStr ('_' & Old), Element (Cursor));
                     Ed.Local_Aliases.Include
                       (To_UStr (Name), Element (Cursor));
                  end if;

                  Ed.Local_Aliases.Exclude (To_UStr (Old));

                  Free (Ed.Current_Var);
                  Ed.Current_Var := new String'(Name);
               end if;
            end;
         end if;
      end if;

   exception
      when E : others => Trace (Me, E);
   end Alias_Renamed;

   -------------------
   -- Alias_Deleted --
   -------------------

   procedure Alias_Deleted (Editor : access Gtk_Widget_Record'Class) is
      Ed     : constant Alias_Editor := Alias_Editor (Editor);
      Cursor : Aliases_Map.Cursor;
      Model  : Gtk_Tree_Model;
      Iter   : Gtk_Tree_Iter;

   begin
      if Ed.Current_Var /= null then
         Cursor := Get_Value (Ed, Ed.Current_Var.all);

         if Cursor /= No_Element then
            Ed.Local_Aliases.Include
              (To_UStr ('_' & Ed.Current_Var.all),
               Element (Cursor));
            Ed.Local_Aliases.Exclude
              (To_UStr (Ed.Current_Var.all));
         end if;

         Free (Ed.Current_Var);

         Get_Selected (Get_Selection (Ed.Aliases), Model, Iter);
         Remove (Ed.Aliases_Model, Iter);
      end if;

   exception
      when E : others => Trace (Me, E);
   end Alias_Deleted;

   -------------------
   -- Alias_Created --
   -------------------

   procedure Alias_Created (Editor : access Gtk_Widget_Record'Class) is
   begin
      Add_New_Alias (Alias_Editor (Editor), "_new_",
                     Selected => True, Is_New_Interactive => True);

   exception
      when E : others => Trace (Me, E);
   end Alias_Created;

   -----------------------
   -- Param_Env_Changed --
   -----------------------

   procedure Param_Env_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Ed          : constant Alias_Editor := Alias_Editor (Editor);
      Path_String : constant String := Get_String (Nth (Params, 1));
      Iter        : constant Gtk_Tree_Iter :=
        Get_Iter_From_String (Ed.Variables_Model, Path_String);

      Value : constant Boolean := Get_Boolean (Ed.Variables_Model, Iter, 3);
   begin
      --  Set the default value as no longer editable
      Set_And_Clear
        (Ed.Variables_Model, Iter,
         (2 => As_Boolean (Value), 3 => As_Boolean (not Value)));

   exception
      when E : others => Trace (Me, E);
   end Param_Env_Changed;

   -------------------------
   -- Find_Next_Parameter --
   -------------------------

   procedure Find_Next_Parameter
     (Editor       : access Alias_Editor_Record'Class;
      Start        : out Gtk_Text_Iter;
      Current      : in out Gtk_Text_Iter;
      Last         : Gtk_Text_Iter;
      All_Specials : Boolean)
   is
      Last_Offset : constant Gint := Get_Offset (Last);
      Result      : Boolean;
   begin
      Copy (Source => Current, Dest => Start);

      while not Is_End (Start)
        and then Get_Offset (Start) <= Last_Offset
      loop
         --  ??? Should use unicode

         if Get_Char (Start) = Special then
            Copy (Source => Start, Dest => Current);
            Forward_Char (Current, Result);

            if Get_Char (Current) = '(' then
               loop
                  Forward_Char (Current, Result);
                  exit when not Result
                    or else Ends_Line (Current);

                  if Get_Char (Current) = ')' then
                     Forward_Char (Current, Result);
                     return;
                  end if;
               end loop;

               Copy (Source => Current, Dest => Start);

            --  Handling of $$ => No highlighting to perform
            elsif Get_Char (Current) = Special then
               Forward_Char (Current, Result);
               Copy (Source => Current, Dest => Start);

            elsif All_Specials
              and then
                (Get_Char (Current) = '_'
                 or else Aliases_Module_Id.Module_Funcs (Get_Char (Current))
                   /= null)
            then
               Forward_Char (Current, Result);
               return;

            else
               Forward_Char (Start, Result);
            end if;
         else
            Forward_Char (Start, Result);
         end if;

         exit when not Result;
      end loop;

      Get_End_Iter (Get_Buffer (Editor.Expansion), Start);
   end Find_Next_Parameter;

   -----------------
   -- Update_Vars --
   -----------------

   procedure Update_Vars (Editor : access Alias_Editor_Record'Class) is
      Iter    : Gtk_Tree_Iter := Get_Iter_First (Editor.Variables_Model);
      Start   : Gtk_Text_Iter;
      Current : Gtk_Text_Iter;
      Last    : Gtk_Text_Iter;
      Found   : Boolean;

   begin
      while Iter /= Null_Iter loop
         Editor.Variables_Model.Set (Iter, 4, False);
         Next (Editor.Variables_Model, Iter);
      end loop;

      Get_Start_Iter (Get_Buffer (Editor.Expansion), Current);
      Get_End_Iter (Get_Buffer (Editor.Expansion), Last);

      loop
         Find_Next_Parameter (Editor, Start, Current, Last, False);

         exit when Is_End (Start);

         declare
            Full_Name : constant String := Get_Text
              (Get_Buffer (Editor.Expansion), Start, Current);
            Name      : constant String :=
              Full_Name (Full_Name'First + 2 .. Full_Name'Last - 1);

         begin
            Found := False;
            Iter := Get_Iter_First (Editor.Variables_Model);

            while Iter /= Null_Iter loop
               if Get_String (Editor.Variables_Model, Iter, 0) = Name then
                  Editor.Variables_Model.Set (Iter, 4, True);
                  Found := True;
                  exit;
               end if;

               Next (Editor.Variables_Model, Iter);
            end loop;

            if not Found then
               Set_Variable
                 (Editor,
                  Name     => Name,
                  Default  => "",
                  From_Env => False);
            end if;
         end;
      end loop;

      Iter := Get_Iter_First (Editor.Variables_Model);
      while Iter /= Null_Iter loop
         if not Get_Boolean (Editor.Variables_Model, Iter, 4) then
            Remove (Editor.Variables_Model, Iter);
         else
            Next (Editor.Variables_Model, Iter);
         end if;
      end loop;
   end Update_Vars;

   ------------------------
   -- Expansion_Inserted --
   ------------------------

   procedure Expansion_Inserted
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Length   : constant Gint := Get_Int (Nth (Params, 3));
      Pos, Eol : Gtk_Text_Iter;
      Result   : Boolean;

   begin
      Get_Text_Iter (Nth (Params, 1), Pos);
      Copy (Source => Pos, Dest => Eol);

      Set_Line_Index (Pos, 0);
      Forward_Chars (Eol, Length, Result);
      Forward_To_Line_End (Eol, Result);

      Highlight_Expansion_Range (Alias_Editor (Editor), Pos, Eol);
      Update_Vars (Alias_Editor (Editor));

   exception
      when E : others => Trace (Me, E);
   end Expansion_Inserted;

   -----------------------
   -- Expansion_Deleted --
   -----------------------

   procedure Expansion_Deleted
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Start, Last : Gtk_Text_Iter;
      Result      : Boolean;
   begin
      Get_Text_Iter (Nth (Params, 1), Start);
      Get_Text_Iter (Nth (Params, 2), Last);

      Set_Line_Index (Start, 0);
      Forward_To_Line_End (Last, Result);

      Highlight_Expansion_Range (Alias_Editor (Editor), Start, Last);
      Update_Vars (Alias_Editor (Editor));

   exception
      when E : others => Trace (Me, E);
   end Expansion_Deleted;

   -------------------------------
   -- Highlight_Expansion_Range --
   -------------------------------

   procedure Highlight_Expansion_Range
     (Editor : access Alias_Editor_Record'Class; First, Last : Gtk_Text_Iter)
   is
      Current, Current2 : Gtk_Text_Iter;
   begin
      Remove_All_Tags (Get_Buffer (Editor.Expansion), First, Last);
      Copy (Source => First, Dest => Current);

      loop
         Find_Next_Parameter (Editor, Current2, Current, Last, True);

         exit when Is_End (Current2);

         Apply_Tag (Get_Buffer (Editor.Expansion),
                    Editor.Highlight_Tag,
                    Current2, Current);
      end loop;

      --  Force a refresh of the text area, otherwise the new tags are
      --  sometimes not shown properly, which is probably a bug in gtk+
      Grab_Focus (Editor.Expansion);
   end Highlight_Expansion_Range;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item           : out String_Menu_Item;
      View           : access Gtk_Text_View_Record'Class;
      Label, Special : String) is
   begin
      Item := new String_Menu_Item_Record (Length => Special'Length);
      Item.View    := Gtk_Text_View (View);
      Item.Special := Special;
      Gtk.Menu_Item.Initialize (Item, Label);
   end Gtk_New;

   --------------------
   -- Insert_Special --
   --------------------

   procedure Insert_Special (Item : access Gtk_Widget_Record'Class) is
      It     : constant String_Menu_Item := String_Menu_Item (Item);
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (It.View);
      Iter   : Gtk_Text_Iter;

   begin
      Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
      Insert (Buffer, Iter, It.Special);

   exception
      when E : others => Trace (Me, E);
   end Insert_Special;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Gtk_Menu
   is
      pragma Unreferenced (Event);
      View : constant Gtk_Text_View := Gtk_Text_View (Widget);
      Menu : Gtk_Menu;
      Item : String_Menu_Item;
      Tmp  : Expansion_Function_List;

   begin
      Gtk_New (Menu);

      Gtk_New (Item, View, -"Insert Parameter Reference", Special & "()");
      Add (Menu, Item);
      Widget_Callback.Connect
        (Item, Gtk.Menu_Item.Signal_Activate, Insert_Special'Access);

      Gtk_New (Item, View, -"Insert Cursor Position", Special & "_");
      Add (Menu, Item);
      Widget_Callback.Connect
        (Item, Gtk.Menu_Item.Signal_Activate, Insert_Special'Access);

      Gtk_New (Item, View, -"Insert Percent Sign", Special & Special);
      Add (Menu, Item);
      Widget_Callback.Connect
        (Item, Gtk.Menu_Item.Signal_Activate, Insert_Special'Access);

      for C in Aliases_Module_Id.Module_Funcs'Range loop
         Tmp := Aliases_Module_Id.Module_Funcs (C);

         while Tmp /= null loop
            Gtk_New (Item, View, Tmp.Descr, Special & C);
            Add (Menu, Item);
            Widget_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate, Insert_Special'Access);
            Tmp := Tmp.Next;
         end loop;
      end loop;

      return Menu;

   exception
      when E : others => Trace (Me, E);
         return Menu;
   end Contextual_Factory;

   ----------------------------
   -- Show_Read_Only_Toggled --
   ----------------------------

   procedure Show_Read_Only_Toggled
     (Editor : access Gtk_Widget_Record'Class) is
   begin
      Update_Contents (Alias_Editor (Editor));
   end Show_Read_Only_Toggled;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Alias_Editor; Kernel : access Kernel_Handle_Record'Class)
   is
      Main_View        : Dialog_View_With_Button_Box;
      Box              : Gtk_Box;
      Pane             : Gtk_Paned;
      Render           : Gtk_Cell_Renderer_Text;
      Toggle_Render    : Gtk_Cell_Renderer_Toggle;
      Col              : Gtk_Tree_View_Column;
      Number           : Gint;
      C                : Gdk_RGBA;
      Expansion_Buffer : Gtk_Text_Buffer;
      Scrolled         : Gtk_Scrolled_Window;
      Button           : Gtk_Button;
      Sep              : Gtk_Separator;
      W                : Gtk_Widget;
      Success          : Boolean;
      pragma Unreferenced (Number, W);

      Current_Win      : constant Gtk_Window := Get_Current_Window (Kernel);

   begin
      Editor := new Alias_Editor_Record;
      Initialize (Editor,
                  Title  => -"Aliases edition",
                  Parent => Current_Win,
                  Flags  => Destroy_With_Parent
                    or Use_Header_Bar_From_Settings (Current_Win));
      Set_Default_Size_From_History (Editor, "aliases", Kernel, 640, 400);

      Editor.Local_Aliases.Clear;

      Main_View := new Dialog_View_With_Button_Box_Record;
      Main_View.Initialize (Position => Pos_Left);
      Pack_Start
        (Get_Content_Area (Editor), Main_View, Expand => True, Fill => True);

      Gtk_New_Hpaned (Pane);
      Main_View.Append (Pane);

      --  List of aliases

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack1 (Pane, Box);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Pack_Start (Box, Scrolled, Expand => True, Fill => True);

      Gtk_New (Editor.Aliases_Model, Aliases_Column_Types);
      Gtk_New (Editor.Aliases, Editor.Aliases_Model);
      Add (Scrolled, Editor.Aliases);
      Set_Mode (Get_Selection (Editor.Aliases), Selection_Single);

      Gtk_New (Render);
      Gtk_New (Editor.Alias_Col);
      Set_Clickable (Editor.Alias_Col, True);
      Set_Sort_Column_Id (Editor.Alias_Col, 0);
      Number := Append_Column (Editor.Aliases, Editor.Alias_Col);
      Set_Title (Editor.Alias_Col, -"Aliases");
      Pack_Start (Editor.Alias_Col, Render, False);
      Add_Attribute (Editor.Alias_Col, Render, "text", 0);
      Add_Attribute (Editor.Alias_Col, Render, "editable", 1);
      Set_Editable_And_Callback (Editor.Aliases_Model, Render, 0);

      Widget_Callback.Object_Connect
        (Get_Selection (Editor.Aliases), Gtk.Tree_Selection.Signal_Changed,
         Alias_Selection_Changed'Access, Editor);
      Widget_Callback.Object_Connect
        (Render, Signal_Edited, Alias_Renamed'Access, Editor);

      Gtk_New (Render);
      Pack_Start (Editor.Alias_Col, Render, False);
      Add_Attribute (Editor.Alias_Col, Render, "text", 2);

      Clicked (Editor.Alias_Col);

      Gtk_New (Editor.Show_Read_Only, -"Show read-only");
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "aliases-show-read-only", True);
      Associate (Get_History (Kernel).all,
                 "aliases-show-read-only",
                 Editor.Show_Read_Only);
      Pack_Start (Box, Editor.Show_Read_Only, Expand => False);
      Widget_Callback.Object_Connect
        (Editor.Show_Read_Only, Gtk.Toggle_Button.Signal_Toggled,
         Show_Read_Only_Toggled'Access, Editor);

      --  Right part

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack2 (Pane, Box);

      --  Parameters list

      Gtk_New (Editor.Variables_Model, Variables_Column_Types);
      Gtk_New (Editor.Variables, Editor.Variables_Model);
      Pack_Start (Box, Editor.Variables, Expand => False);

      Gtk_New (Render);
      Gtk_New (Col);
      Number := Append_Column (Editor.Variables, Col);
      Set_Title (Col, -"Parameter");
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "text", 0);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, 0);

      Clicked (Col);

      Gtk_New (Toggle_Render);
      Gtk_New (Col);
      Number := Append_Column (Editor.Variables, Col);
      Set_Title (Col, -"Environment");
      Set_Resizable (Col, True);
      Pack_Start (Col, Toggle_Render, False);
      Add_Attribute (Col, Toggle_Render, "active", 3);
      Add_Attribute (Col, Toggle_Render, "activatable", 5);

      Widget_Callback.Object_Connect
        (Toggle_Render, Gtk.Toggle_Button.Signal_Toggled,
         Param_Env_Changed'Access, Editor);

      Gtk_New (Render);
      Gtk_New (Col);
      Number := Append_Column (Editor.Variables, Col);
      Set_Title (Col, -"Default Value");
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "text", 1);
      Add_Attribute (Col, Render, "editable", 2);
      Add_Attribute (Col, Render, "strikethrough", 3);
      Set_Editable_And_Callback (Editor.Variables_Model, Render, 1);
      Set_Resizable (Col, True);

      Gtk_New_Hseparator (Sep);
      Pack_Start (Box, Sep, Expand => False, Padding => 2);

      --  Expansion

      Gtk_New (Scrolled);
      Pack_Start (Box, Scrolled, Expand => True, Fill => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Gtk_New (Expansion_Buffer);
      Gtk_New (Editor.Expansion, Expansion_Buffer);
      Add (Scrolled, Editor.Expansion);
      Set_Wrap_Mode (Editor.Expansion, Wrap_None);

      Realize (Editor.Expansion);
      Modify_Font (Editor.Expansion, Default_Style.Get_Pref_Font);

      Register_Contextual_Menu
        (Widget       => Editor.Expansion,
         Menu_Create  => Contextual_Factory'Access);

      Editor.Expansion_Inserted_Cb := Widget_Callback.Object_Connect
        (Expansion_Buffer, Gtk.Text_Buffer.Signal_Insert_Text,
         Expansion_Inserted'Access, Editor,
         After => True);
      Editor.Expansion_Deleted_Cb := Widget_Callback.Object_Connect
        (Expansion_Buffer, Gtk.Text_Buffer.Signal_Delete_Range,
         Expansion_Deleted'Access, Editor,
         After => True);

      --  Filters

      Gtk_New (Editor.Must_Reindent, -"Indent source editor after expansion");
      Pack_Start (Box, Editor.Must_Reindent, Expand => False, Fill => True);

      --  Buttons

      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-add-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Main_View.Append_Button (Button);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, Alias_Created'Access, Editor);

      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-remove-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Main_View.Append_Button (Button);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, Alias_Deleted'Access, Editor);

      Gtk_New_Vseparator (Sep);
      Pack_Start (Get_Action_Area (Editor), Sep, Expand => False);

      W := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      W := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);

      Gtk_New (Editor.Highlight_Tag);
      Parse (C, Highlight_Color, Success);
      if Success then
         Set_Property
           (Editor.Highlight_Tag, Gtk.Text_Tag.Foreground_Rgba_Property, C);
         Gtk.Text_Tag_Table.Add
           (Get_Tag_Table (Get_Buffer (Editor.Expansion)),
            Editor.Highlight_Tag);
      end if;
   end Gtk_New;

   -------------------
   -- Add_New_Alias --
   -------------------

   procedure Add_New_Alias
     (Editor    : access Alias_Editor_Record'Class;
      Name      : String;
      Selected  : Boolean := False;
      Read_Only : Boolean := False;
      Is_New_Interactive : Boolean := False)
   is
      procedure Set_Alias
        (Tree, Iter : System.Address;
         Col1  : Gint; Name : String;
         Col2  : Gint := 1; Editable : Gboolean := 1;
         Col3  : Gint := 2; Read_Only_Text : String := "" & ASCII.NUL);
      pragma Import (C, Set_Alias, "ada_gtk_tree_store_set_ptr_int_ptr");

      Alias_Iter : Gtk_Tree_Iter;
      Path       : Gtk_Tree_Path;

   begin
      Editor.Is_New_Interactive := Is_New_Interactive;
      Append (Editor.Aliases_Model, Alias_Iter, Null_Iter);

      if Read_Only then
         Set_Alias
           (Get_Object (Editor.Aliases_Model), Alias_Iter'Address,
            0, Name & ASCII.NUL, Editable => 0,
            Read_Only_Text => -" (read-only)" & ASCII.NUL);
      else
         Set_Alias
           (Get_Object (Editor.Aliases_Model), Alias_Iter'Address,
            0, Name & ASCII.NUL);
      end if;

      if Selected then
         Path := Get_Path (Editor.Aliases_Model, Alias_Iter);
         Scroll_To_Cell
           (Tree_View => Editor.Aliases,
            Path      => Path,
            Column    => null,
            Use_Align => False,
            Row_Align => 0.0,
            Col_Align => 0.0);
         Set_Cursor
           (Editor.Aliases, Path,
            Get_Column (Editor.Aliases, 0), Start_Editing => True);
         Path_Free (Path);
      end if;
      Editor.Is_New_Interactive := False;
   end Add_New_Alias;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents (Editor : access Alias_Editor_Record'Class) is
      Cursor  : Aliases_Map.Cursor := Aliases_Module_Id.Aliases.First;
      Value : Alias_Type;
      It    : Gtk_Tree_Iter;
      use Aliases_Map;
   begin
      Clear (Editor.Aliases_Model);

      while Cursor /= No_Element loop
         Value := Element (Cursor);

         if not Value.Read_Only
           or else Get_Active (Editor.Show_Read_Only)
         then
            Add_New_Alias
              (Editor,
               SU.To_String (Key (Cursor)),
               Read_Only => Value.Read_Only);
         end if;
         Next (Cursor);
      end loop;

      It := Get_Iter_First (Editor.Aliases_Model);
      if It /= Null_Iter then
         Select_Iter (Get_Selection (Editor.Aliases), It);
      end if;
   end Update_Contents;

   --------------------
   -- Update_Aliases --
   --------------------

   procedure Update_Aliases (Editor : access Alias_Editor_Record'Class) is
      Cursor : Aliases_Map.Cursor := Editor.Local_Aliases.First;
      Value  : Alias_Type;
   begin

      while Cursor /= No_Element loop
         Value := Element (Cursor);

         declare
            Name : constant SU.Unbounded_String := Key (Cursor);
         begin
            if SU.Element (Name, 1) = '_' then
               Aliases_Module_Id.Aliases.Delete
                 (SU.Unbounded_Slice (Name, 2, SU.Length (Name)));
            else
               Aliases_Module_Id.Aliases.Include (Name, Value);
            end if;
         end;

         Next (Cursor);
      end loop;

      Editor.Local_Aliases.Clear;
   end Update_Aliases;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_Aliases_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Editor : Alias_Editor;
   begin
      Gtk_New (Editor, Kernel);
      Update_Contents (Editor);
      Show_All (Editor);

      case Run (Editor) is
         when Gtk_Response_OK =>
            Save_Current_Var (Editor);
            Update_Aliases (Editor);
            Save_Aliases (Kernel);

         when Gtk_Response_Cancel =>
            null;

         when others =>
            null;
      end case;

      Editor.Local_Aliases.Clear;
      Free (Editor.Current_Var);
      Unref (Editor.Highlight_Tag);

      Destroy (Editor);
      return Commands.Success;
   end Execute;

   -----------------------------------
   -- Register_Special_Alias_Entity --
   -----------------------------------

   procedure Register_Special_Alias_Entity
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Description : String;
      Entity      : Character;
      Func        : Alias_Expansion_Function)
   is
      pragma Unreferenced (Kernel);
   begin
      if Aliases_Module_Id /= null then
         Aliases_Module_Id.Module_Funcs (Entity) :=
           new Expansion_Function_Record'
             (Length => Description'Length,
              Descr  => Description,
              Func   => Func,
              Next   => Aliases_Module_Id.Module_Funcs (Entity));
      end if;
   end Register_Special_Alias_Entity;

   ----------------------
   -- Special_Entities --
   ----------------------

   function Special_Entities
     (Kernel    : access Kernel_Handle_Record'Class;
      Expansion : String;
      Special   : Character)
      return String
   is
      First, Last : Integer;
   begin
      case Special is
         when 'D'   => return Expansion & Image (Ada.Calendar.Clock, ISO_Date);
         when 'H'   => return Expansion & Image (Ada.Calendar.Clock, "%T");
         when 'O'    =>
            Find_Current_Entity (Expansion, Expansion'Last, First, Last);

            if First > Last then
               return Invalid_Expansion;
            end if;

            declare
               Cursor        : Integer;
               Must_Reindent : Boolean;
               Replace       : constant String := Expand_Alias
                 (Alias         => Get_Alias (Expansion (First .. Last - 1)),
                  Kernel        => Kernel,
                  Cursor        => Cursor,
                  Must_Reindent => Must_Reindent,
                  Offset_Column => 0);
            begin
               if Replace /= "" then
                  return Expansion (Expansion'First .. First - 1) & Replace;
               else
                  return Expansion;
               end if;
            end;

         when 'l' => return Expansion & "line";
         when 'c' => return Expansion & "column";
         when 'f' => return Expansion & "file";
         when 'd' => return Expansion & "directory";
         when 'p' => return Expansion & "project";
         when 'P' => return Expansion & "Project";
         when others => return Invalid_Expansion;
      end case;

   exception
      when E : others =>
         Trace (Me, E);
         return Invalid_Expansion;
   end Special_Entities;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Aliases_Module_Id_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File);
   begin
      Customize (Get_Kernel (Module.all), Node, Level, Read_Only => True);
   end Customize;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Kernel    : access Kernel_Handle_Record'Class;
      Node      : Node_Ptr;
      Level     : Customization_Level;
      Read_Only : Boolean)
   is
      pragma Unreferenced (Level);
      use Aliases_Map;
      Child      : Node_Ptr;
      Expand     : XML_Utils.String_Ptr;
      Params     : Params_List.List;
      Old_Cursor : Aliases_Map.Cursor;
      N          : Node_Ptr := Node;
   begin
      while N /= null loop
         if N.Tag.all = "alias" then
            declare
               Name          : constant String :=
                                 Get_Attribute (N, "name");
               Must_Reindent : constant Boolean :=
                 Get_Attribute (N, "indent", "false") = "true";
            begin
               Expand := null;

               if Name = "" then
                  Insert
                    (Kernel, -"Invalid alias format for " & Name,
                     Mode => Error);
               end if;

               Child := N.Child;
               while Child /= null loop
                  if Child.Tag.all = "text" then
                     Expand := Child.Value;

                  elsif Child.Tag.all = "param" then
                     Params.Append
                       ((Name        =>
                            To_UStr (Get_Attribute (Child, "name")),
                         Description =>
                            To_UStr (Get_Attribute (Child, "description")),
                         Initial     =>
                            To_UStr (Child.Value.all),
                         From_Env    =>
                            Get_Attribute (Child, "environment") = "true"));

                  else
                     Insert (Kernel,
                             -"Unknown XML tag in alias definition for "
                             & Name,
                             Mode => Error);
                  end if;

                  Child := Child.Next;
               end loop;

               --  Do not override a read-write alias: they have been parsed
               --  before all others, but should in fact have higher priority

               Old_Cursor := Aliases_Module_Id.Aliases.Find (To_UStr (Name));

               if Old_Cursor = No_Element
                 or else Element (Old_Cursor).Read_Only
               then
                  Aliases_Module_Id.Aliases.Include
                    (To_UStr (Name),
                     (Name          => To_UStr (Name),
                      Expansion     => To_UStr
                        (if Expand = null then "" else Expand.all),
                      Params        => Params,
                      Read_Only     => Read_Only,
                      Must_Reindent => Must_Reindent));
               end if;
            end;
         end if;

         N := N.Next;
         Params.Clear;
      end loop;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Aliases_Module_Id := new Aliases_Module_Id_Record;
      Register_Module
        (Module      => Module_ID (Aliases_Module_Id),
         Kernel      => Kernel,
         Module_Name => "Aliases",
         Priority    => Default_Priority);

      Register_Action
         (Kernel, "aliases edit", new Edit_Aliases_Command,
          -"Open the aliases editor",
          Category => -"Aliases");

      Parse_File
        (Kernel,
         Create_From_Dir (Get_Home_Dir (Kernel), "aliases"),
         Read_Only => False);

      Register_Action
        (Kernel, "Expand alias", new Interactive_Alias_Expansion_Command,
         Category    => -"Editor",
         Description => -"Expand the alias found just before the cursor",
         Filter      => Lookup_Filter (Kernel, "Source editor"));

      Register_Special_Alias_Entity
        (Kernel, "Expand previous alias", 'O', Special_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, "Current Date", 'D', Special_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, "Current Hour", 'H', Special_Entities'Access);

      Aliases_Module.Scripts.Register_Commands
        (Kernel_Handle (Kernel));

      --  Others are registered in src_editor_module
   end Register_Module;

   ----------------------
   -- Get_Aliases_List --
   ----------------------

   function Get_Aliases_List return Alias_List is
      use Aliases_Map;
      Nb_Aliases : constant Integer :=
        Integer (Aliases_Module_Id.Aliases.Length);
      J : Integer := 1;
   begin
      return Aliases : Alias_List (1 .. Nb_Aliases) do
         for Alias of Aliases_Module_Id.Aliases loop
            Aliases (J) := Alias;
            J := J + 1;
         end loop;
      end return;
   end Get_Aliases_List;

   ---------------
   -- Get_Alias --
   ---------------

   function Get_Alias (Name : String) return Alias_Type is
      U_Name : constant SU.Unbounded_String := To_UStr (Name);
   begin
      if Aliases_Module_Id.Aliases.Contains (U_Name) then
         return Aliases_Module_Id.Aliases.Element (U_Name);
      else
         return No_Alias;
      end if;
   end Get_Alias;

   ----------------
   -- On_Toggled --
   ----------------

   procedure On_Toggled (Self : access Gtk_Toggle_Button_Record'Class) is
      Option_Check : constant Alias_Option_Check := Alias_Option_Check (Self);
   begin
      Option_Check.Option.Enabled := Option_Check.Get_Active;
   end On_Toggled;

   ------------
   -- Create --
   ------------

   function Create
     (Label         : String;
      Default_Value : Boolean := False;
      Doc           : String := "") return Alias_Option_Type is
   begin
      return Alias_Option_Type'
        (Label   => SU.To_Unbounded_String (Label),
         Doc     => SU.To_Unbounded_String (Doc),
         Enabled => Default_Value);
   end Create;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Option : Alias_Option_Type) return Boolean is
   begin
      return Option.Enabled;
   end Is_Enabled;

end Aliases_Module;
