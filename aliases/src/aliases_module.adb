-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Calendar;             use Ada.Calendar;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Calendar.Time_IO;    use GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with System.Assertions;

with Gdk.Color;                use Gdk.Color;
with Gdk.Event;                use Gdk.Event;
with Gdk.Types;                use Gdk.Types;
with Gdk.Types.Keysyms;        use Gdk.Types.Keysyms;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Unicode;             use Glib.Unicode;
with Glib.Values;              use Glib.Values;
with Glib.Xml_Int;             use Glib.Xml_Int;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;             use Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Separator;            use Gtk.Separator;
with Gtk.Size_Group;           use Gtk.Size_Group;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Style;                use Gtk.Style;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;
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

with Basic_Types;
with Commands.Interactive;     use Commands, Commands.Interactive;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Console;       use GPS.Kernel.Console;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;       use GPS.Kernel.Scripts;
with GPS.Kernel;               use GPS.Kernel;
with GUI_Utils;                use GUI_Utils;
with Histories;                use Histories;
with String_Hash;
with String_Utils;             use String_Utils;
with Traces;                   use Traces;
with VFS;                      use VFS;
with XML_Parsers;

package body Aliases_Module is

   Me : constant Debug_Handle := Create ("Aliases");

   Special : constant Character := '%';

   Highlight_Color : constant String := "#DD0000";
   --  Color used to highlight special entities in the expansion

   type Interactive_Alias_Expansion_Command is new Interactive_Command with
   record
      Kernel : Kernel_Handle;
   end record;
   type Interactive_Alias_Expansion_Command_Access is access all
     Interactive_Alias_Expansion_Command'Class;
   function Execute
     (Command : access Interactive_Alias_Expansion_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;

   type Param_Record;
   type Param_Access is access Param_Record;
   type Param_Record is record
      Name     : String_Access;
      Initial  : String_Access;
      From_Env : Boolean;
      Next     : Param_Access;
   end record;

   procedure Free (P : in out Param_Access);
   --  Free the memory occupied by P

   type Alias_Record is record
      Expansion : String_Access;
      Params    : Param_Access;
      Read_Only : Boolean;

      Must_Reindent : Boolean;
      --  Whether the editor should be reindent after insertion of the macro
   end record;

   procedure Free (Alias : in out Alias_Record);

   function Clone (Alias : Alias_Record) return Alias_Record;
   --  Return a deep copy of Alias

   No_Alias : constant Alias_Record :=
     (Expansion => null, Params => null, Read_Only => False,
      Must_Reindent => False);

   package Aliases_Hash is new String_Hash
     (Alias_Record, Free, No_Alias, Case_Sensitive => False);
   use Aliases_Hash.String_Hash_Table;

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
      Aliases      : Aliases_Hash.String_Hash_Table.HTable;
      Module_Funcs : Expansion_Function_Array;
      Expanded     : Boolean_Hash.String_Hash_Table.HTable;
   end record;
   type Aliases_Module_Id_Access is access all Aliases_Module_Id_Record'Class;

   procedure Destroy (Module : in out Aliases_Module_Id_Record);
   procedure Customize
     (Module : access Aliases_Module_Id_Record;
      File   : VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   Aliases_Module_Id : Aliases_Module_Id_Access;

   type Alias_Editor_Record is new Gtk_Dialog_Record with record
      Local_Aliases   : Aliases_Hash.String_Hash_Table.HTable;
      Aliases         : Gtk_Tree_View;
      Aliases_Model   : Gtk_Tree_Store;
      Current_Alias   : Gtk_Label;
      Variables       : Gtk_Tree_View;
      Variables_Model : Gtk_Tree_Store;
      Expansion       : Gtk_Text_View;
      Alias_Col       : Gtk_Tree_View_Column;
      Show_Read_Only  : Gtk_Check_Button;
      Must_Reindent   : Gtk_Check_Button;

      Current_Var     : String_Access;
      Highlight_Tag   : Gtk_Text_Tag;
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
   --  Create a new String_Menu_Item.

   procedure Update_Contents (Editor : access Alias_Editor_Record'Class);
   --  Update the contents of the editor, based on the list of currently
   --  defined aliases

   procedure Find_Current_Entity
     (Text        : String;
      Current_Pos : Integer;
      First, Last : out Integer);
   --  Set First .. Last to the beginning and end of the current alias name.
   --  If no alias name was found, set First > Last.
   --  ??? Should accept unicode

   function Expand_Alias
     (Kernel        : access Kernel_Handle_Record'Class;
      Name          : String;
      Cursor        : access Integer;
      Must_Reindent : access Boolean;
      Offset_Column : Gint)
      return String;
   --  Return the expanded version of Name.
   --  Cursor is the index in the returned string for the cursor position.
   --  The empty string is returned if there is no such alias.
   --  Must_Reindent is set to True if the editor should be reindented after
   --  insertion.

   procedure Parse_File
     (Kernel    : access Kernel_Handle_Record'Class;
      Filename  : String;
      Read_Only : Boolean);
   --  Load a filename, and make the resulting aliases Read_Only

   procedure Save_Aliases (Kernel : access Kernel_Handle_Record'Class);
   --  Save the aliases in filename.

   procedure On_Edit_Aliases
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for the aliases edition.

   procedure Alias_Selection_Changed
     (Editor : access Gtk_Widget_Record'Class);
   --  Called when a new alias was selected in the editor.

   procedure Save_Current_Var (Editor : access Alias_Editor_Record'Class);
   --  Save the value of the currently edited variable in the local aliases
   --  table.

   procedure Update_Aliases (Editor : access Alias_Editor_Record'Class);
   --  Merge the local aliases into the global table.

   procedure Alias_Renamed (Editor : access Gtk_Widget_Record'Class);
   --  Called when an alias was renamed.

   procedure Alias_Deleted (Editor : access Gtk_Widget_Record'Class);
   --  Deletes an alias

   procedure Alias_Created (Editor : access Gtk_Widget_Record'Class);
   --  Creates a new alias

   function Get_Value
     (Editor : access Alias_Editor_Record'Class;
      Name   : String) return Alias_Record;
   --  Get the current value of the variable (includes checking in the local
   --  aliases).

   procedure Add_New_Alias
     (Editor    : access Alias_Editor_Record'Class;
      Name      : String;
      Selected  : Boolean := False;
      Read_Only : Boolean := False);
   --  Add a new entry in the aliases list of the editor

   procedure Param_Env_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when a variable is changed from environment variable to standard
   --  variable.

   type Param_Substitution;
   type Param_Substitution_Access is access Param_Substitution;
   type Param_Substitution is record
      Param   : Param_Access;
      Edition : Gtk_Entry;
      Next    : Param_Substitution_Access;
   end record;
   --  Widget used to store the current value for parameters: Edition contains
   --  the widget used by the user to edit the value.

   procedure Free (Param : in out Param_Substitution_Access);
   --  Free the list of params

   function Substitute_Params
     (Text : String; Values : Param_Substitution_Access) return String;
   --  Compute the replacement string for Text, given the currnet parameter
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

   function Contextual_Factory
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Gtk_Menu;
   --  Create the contextual menu for the expansion edition area

   procedure Contextual_Destroy
     (Widget : access Gtk_Widget_Record'Class; Menu : Gtk_Menu);
   --  Destroy the contextual menu area for the expansion edition zone.

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

   ----------
   -- Free --
   ----------

   procedure Free (Param : in out Param_Substitution_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Param_Substitution, Param_Substitution_Access);

      Tmp : Param_Substitution_Access;

   begin
      while Param /= null loop
         Tmp := Param;
         Param := Param.Next;
         Unchecked_Free (Tmp);
      end loop;
   end Free;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Aliases_Module_Id_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Expansion_Function_Record, Expansion_Function_List);

      Tmp : Expansion_Function_List;

   begin
      Reset (Module.Aliases);

      for C in Module.Module_Funcs'Range loop
         while Module.Module_Funcs (C) /= null loop
            Tmp := Module.Module_Funcs (C).Next;
            Unchecked_Free (Module.Module_Funcs (C));
            Module.Module_Funcs (C) := Tmp;
         end loop;
      end loop;
   end Destroy;

   -----------
   -- Clone --
   -----------

   function Clone (Alias : Alias_Record) return Alias_Record is
      A : Alias_Record;
      P : Param_Access;
   begin
      if Alias = No_Alias then
         return No_Alias;
      else
         A := (Expansion => new String'(Alias.Expansion.all),
               Params    => null,
               Read_Only => Alias.Read_Only,
               Must_Reindent => Alias.Must_Reindent);

         P := Alias.Params;
         while P /= null loop
            A.Params := new Param_Record'
              (Name     => new String'(P.Name.all),
               Initial  => new String'(P.Initial.all),
               From_Env => P.From_Env,
               Next     => A.Params);
            P := P.Next;
         end loop;

         return A;
      end if;
   end Clone;

   ----------
   -- Free --
   ----------

   procedure Free (P : in out Param_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Param_Record, Param_Access);
   begin
      if P /= null then
         Free (P.Name);
         Free (P.Initial);
         Unchecked_Free (P);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Alias : in out Alias_Record) is
      P : Param_Access;
   begin
      Free (Alias.Expansion);

      while Alias.Params /= null loop
         P := Alias.Params;
         Alias.Params := P.Next;
         Free (P);
      end loop;
   end Free;

   ------------------
   -- Save_Aliases --
   ------------------

   procedure Save_Aliases (Kernel : access Kernel_Handle_Record'Class) is
      Filename          : constant String := Get_Home_Dir (Kernel) & "aliases";
      File, Key, Child  : Node_Ptr;
      Iter              : Iterator;
      Value             : Alias_Record;
      P                 : Param_Access;
      Success           : Boolean;

   begin
      File := new Node;
      File.Tag := new String'("Aliases");

      Get_First (Aliases_Module_Id.Aliases, Iter);

      loop
         Value := Get_Element (Iter);

         exit when Value = No_Alias;

         --  We only save user-defined aliases, not the ones that are defined
         --  in the standard system files.

         if not Value.Read_Only then
            Key := new Node;
            Key.Tag := new String'("alias");
            Set_Attribute (Key, "name", Get_Key (Iter));

            if Value.Must_Reindent then
               Set_Attribute (Key, "indent", "true");
            end if;

            Child := new Node;
            Child.Tag := new String'("text");
            Child.Value := new String'(Strip_CR (Value.Expansion.all));
            Add_Child (Key, Child);

            P := Value.Params;

            while P /= null loop
               Child := new Node;
               Child.Tag := new String'("param");
               Set_Attribute (Child, "name", P.Name.all);

               if P.From_Env then
                  Set_Attribute (Child, "environment", "true");
               end if;

               Child.Value := new String'(P.Initial.all);
               Add_Child (Key, Child);
               P := P.Next;
            end loop;

            Add_Child (File, Key);
         end if;

         Get_Next (Aliases_Module_Id.Aliases, Iter);
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
      Filename  : String;
      Read_Only : Boolean)
   is
      File : Node_Ptr;
      Err : String_Access;
   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);
         XML_Parsers.Parse (Filename, File, Err);
         if File /= null then
            Customize (Kernel, File.Child, User_Specific, Read_Only);
            Free (File);
         else
            Insert (Kernel, Err.all, Mode => Error);
            Free (Err);
         end if;
      else
         Trace (Me, "No such file: " & Filename);
      end if;

   exception
      when System.Assertions.Assert_Failure =>
         Insert (Kernel, "Invalid format for " & Filename, Mode => Error);
         Free (File);

      when Status_Error | Name_Error =>
         Trace (Me, "No aliases file " & Filename);

      when E : others =>
         Trace (Exception_Handle, E);
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

         exit when Get
           (Aliases_Module_Id.Aliases,
            Text (UTF8_Find_Next_Char (Text, First) .. Last - 1)) /= No_Alias;

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
     (Text : String; Values : Param_Substitution_Access) return String
   is
      P     : Param_Substitution_Access := Values;
      Count : Natural := 0;
   begin
      while P /= null loop
         Count := Count + 1;
         P := P.Next;
      end loop;

      declare
         Substrings : Substitution_Array (1 .. Count + 1);
      begin
         P := Values;
         Count := Substrings'First + 1;

         --  Preserve %%, since Find_And_Replace_Cursor does a second expansion
         --  phase

         Substrings (Substrings'First) :=
           (Name  => new String'("" & Special),
            Value => new String'("" & Special & Special));

         while P /= null loop
            Substrings (Count) :=
              (Name  => new String'("(" & P.Param.Name.all & ')'),
               Value => new String'(Get_Text (P.Edition)));
            Count := Count + 1;

            P := P.Next;
         end loop;

         declare
            Val : constant String := Substitute (Text, Special, Substrings);
         begin
            Free (Substrings);
            return Val;
         end;
      end;
   end Substitute_Params;

   ------------------
   -- Expand_Alias --
   ------------------

   function Expand_Alias
     (Kernel        : access Kernel_Handle_Record'Class;
      Name          : String;
      Cursor        : access Integer;
      Must_Reindent : access Boolean;
      Offset_Column : Gint) return String
   is
      function Find_And_Replace_Cursor (Str : String) return String;
      --  Find the position of the cursor in Str, and returns the new Str when
      --  the special entity has been removed

      -----------------------------
      -- Find_And_Replace_Cursor --
      -----------------------------

      function Find_And_Replace_Cursor (Str : String) return String is
         use type Ada.Strings.Unbounded.Unbounded_String;

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

         Cursor.all := Str'Length;

         while S <= Str'Last - 1 loop
            if Str (S) = Special then
               Result := Result & Str (First .. S - 1);
               First := S + 2;

               if Str (S + 1) = '_' then
                  Cursor.all := Ada.Strings.Unbounded.Length (Result);

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
                           Result := Ada.Strings.Unbounded.To_Unbounded_String
                             (Replace);
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

         return Ada.Strings.Unbounded.To_String (Result);
      end Find_And_Replace_Cursor;

      Alias  : constant Alias_Record :=
                 Get (Aliases_Module_Id.Aliases, Name);
      Values : Param_Substitution_Access;
      Dialog : Gtk_Dialog;
      Box    : Gtk_Box;
      S      : Gtk_Size_Group;
      P      : Param_Access;
      Label  : Gtk_Label;
      W      : Gtk_Widget;
      Val    : String_Access;

   begin
      Must_Reindent.all := Alias.Must_Reindent;

      if Alias = No_Alias then
         return "";

      elsif Alias.Params = null then
         return Find_And_Replace_Cursor (Alias.Expansion.all);

      else
         P := Alias.Params;

         while P /= null loop
            Values := new Param_Substitution'
              (Param   => P,
               Edition => null,
               Next    => Values);

            if Dialog = null then
               Gtk_New (Dialog,
                        Title  => -"Alias Parameter Selection",
                        Parent => Get_Current_Window (Kernel),
                        Flags  => Destroy_With_Parent);
               Gtk_New (S);

               W := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
               Grab_Default (W);
               W := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

               Gtk_New_Hbox (Box, Homogeneous => False);
               Pack_Start (Get_Vbox (Dialog), Box, Expand => False);

               Gtk_New (Label, -"Alias name: ");
               Add_Widget (S, Label);
               Set_Alignment (Label, 0.0, 0.5);
               Pack_Start (Box, Label, Expand => False);

               Gtk_New (Label, Name);
               Add_Widget (S, Label);
               Set_Alignment (Label, 0.0, 0.5);
               Pack_Start (Box, Label, Expand => False);
            end if;

            Gtk_New_Hbox (Box, Homogeneous => False);
            Pack_Start (Get_Vbox (Dialog), Box, Expand => False);

            Gtk_New (Label, P.Name.all & ":   ");
            Pack_Start (Box, Label, Expand => False, Fill => True);
            Set_Alignment (Label, 0.0, 0.5);
            Add_Widget (S, Label);

            Gtk_New (Values.Edition);
            Set_Activates_Default (Values.Edition, True);
            Pack_Start (Box, Values.Edition, Expand => True, Fill => True);

            if P.From_Env then
               Val := Getenv (P.Name.all);
               Set_Text (Values.Edition, Val.all);
               Free (Val);
            else
               Set_Text (Values.Edition, P.Initial.all);
            end if;

            P := P.Next;
         end loop;

         if Dialog /= null then
            Show_All (Dialog);
            if Run (Dialog) /= Gtk_Response_OK then
               Destroy (Dialog);
               return "";
            end if;
         end if;

         declare
            Val : constant String := Substitute_Params
              (Alias.Expansion.all, Values);
         begin
            if Dialog /= null then
               Destroy (Dialog);
            end if;

            Free (Values);
            return Find_And_Replace_Cursor (Val);
         end;
      end if;
   end Expand_Alias;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Interactive_Alias_Expansion_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

      W         : constant Gtk_Widget :=
                    Get_Current_Focus_Widget (Command.Kernel);
      Had_Focus : Boolean;

   begin
      Boolean_Hash.String_Hash_Table.Reset (Aliases_Module_Id.Expanded);

      if W /= null and then W.all in Gtk_Editable_Record'Class then
         if Get_Editable (Gtk_Editable (W)) then
            declare
               Text        : constant String := Get_Chars (Gtk_Editable (W));
               First, Last : Integer;
            begin
               Find_Current_Entity
                 (Text, Integer (Get_Position (Gtk_Editable (W))),
                  First, Last);

               if First > Last then
                  return Failure;
               end if;

               declare
                  Cursor  : aliased Integer;
                  Must_Reindent : aliased Boolean;
                  Replace : constant String := Expand_Alias
                    (Command.Kernel, Text (First .. Last - 1),
                     Cursor'Unchecked_Access, Must_Reindent'Unchecked_Access,
                     0);
                  F       : Gint := Gint (First - Text'First);

               begin
                  if Replace /= "" then
                     Delete_Text
                       (Gtk_Editable (W),
                        F,
                        Gint (Last - Text'First));
                     Insert_Text (Gtk_Editable (W), Replace, F);
                     Set_Position
                       (Gtk_Editable (W),
                        F + Gint (Cursor - Replace'Length));
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
                 and then Get
                   (Aliases_Module_Id.Aliases,
                    Get_Slice (Buffer, First_Iter, Last_Iter)) = No_Alias
               loop
                  Backward_Word_Start (First_Iter, Success);
               end loop;

               declare
                  Cursor        : aliased Integer;
                  Must_Reindent : aliased Boolean;
                  Column        : constant Gint :=
                                    Get_Line_Offset (First_Iter);
                  Replace       : constant String :=
                                    Expand_Alias
                                      (Command.Kernel,
                                       Get_Slice
                                         (Buffer, First_Iter, Last_Iter),
                                       Cursor'Unchecked_Access,
                                       Must_Reindent'Unchecked_Access,
                                       Column);
                  Result        : Boolean;
                  Event         : Gdk_Event;
                  Args          : Argument_List (1 .. 2);
                  Count         : Natural := 0;
                  Index         : Natural := Replace'First;
                  Start_Line    : constant Integer :=
                                    Integer (Get_Line (First_Iter));
               begin
                  if Replace /= "" then
                     Had_Focus := Has_Focus_Is_Set (W);

                     --  Simulate a focus_in/focus_out event, needed for the
                     --  GPS source editor, which saves and restores the
                     --  cursor position when the focus changes (for the
                     --  handling of multiple views).

                     if not Had_Focus then
                        Allocate (Event, Enter_Notify, Get_Window (W));
                        Result := Return_Callback.Emit_By_Name
                          (W, Signal_Focus_In_Event, Event);
                        Free (Event);
                     end if;

                     Delete (Buffer, First_Iter, Last_Iter);
                     Insert (Buffer, First_Iter, Replace);
                     Backward_Chars (First_Iter,
                                     Gint (Replace'Length - Cursor),
                                     Result);
                     Place_Cursor (Buffer, First_Iter);

                     --  Reindent the current editor. Since we have given the
                     --  focus to the widget, the call to the shell command
                     --  will have no effect unless this is really an editor.

                     if Must_Reindent then
                        while Index <= Replace'Last loop
                           Index := Next_Line (Replace, Index) + 1;
                           Count := Count + 1;
                        end loop;
                        Args (1) := new String'(Image (Start_Line + 1));
                        Args (2) := new String'(Image (Start_Line + Count));

                        Execute_GPS_Shell_Command
                          (Command.Kernel,
                           Command => "Editor.select_text",
                           Args    => Args);
                        Basic_Types.Free (Args);

                        Execute_GPS_Shell_Command
                          (Command.Kernel, Command => "Editor.indent");
                     end if;

                     if not Had_Focus then
                        Allocate (Event, Leave_Notify, Get_Window (W));
                        Result := Return_Callback.Emit_By_Name
                          (W, Signal_Focus_Out_Event, Event);
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
         Trace (Exception_Handle, E);
         return Failure;
   end Execute;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Editor : access Alias_Editor_Record'Class;
      Name   : String) return Alias_Record
   is
      Alias : Alias_Record := Get (Editor.Local_Aliases, Name);
   begin
      if Alias = No_Alias then
         Alias := Get (Aliases_Module_Id.Aliases, Name);
      end if;

      return Alias;
   end Get_Value;

   ----------------------
   -- Save_Current_Var --
   ----------------------

   procedure Save_Current_Var (Editor : access Alias_Editor_Record'Class) is
      Buffer      : constant Gtk_Text_Buffer := Get_Buffer (Editor.Expansion);
      Start, Last : Gtk_Text_Iter;
      P           : Param_Access;
      Iter        : Gtk_Tree_Iter;

   begin
      if Editor.Current_Var /= null then
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
               P := new Param_Record'
                 (Name     => new String'(Name),
                  Initial  => new String'(Initial),
                  From_Env => From_Env,
                  Next     => P);
            end;

            Next (Editor.Variables_Model, Iter);
         end loop;

         Get_Start_Iter (Buffer, Start);
         Get_End_Iter   (Buffer, Last);

         Set (Editor.Local_Aliases, Editor.Current_Var.all,
              (Expansion => new String'(Get_Text (Buffer, Start, Last)),
               Params    => P,
               Read_Only => False,
               Must_Reindent => Get_Active (Editor.Must_Reindent)));
      end if;
   end Save_Current_Var;

   -----------------------------
   -- Alias_Selection_Changed --
   -----------------------------

   procedure Alias_Selection_Changed
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed          : constant Alias_Editor := Alias_Editor (Editor);
      Model       : Gtk_Tree_Model;
      Iter        : Gtk_Tree_Iter;
      Alias       : Alias_Record := No_Alias;
      P           : Param_Access;
      Start, Last : Gtk_Text_Iter;

   begin
      Save_Current_Var (Ed);

      Free (Ed.Current_Var);

      Get_Selected (Get_Selection (Ed.Aliases), Model, Iter);

      if Iter /= Null_Iter then
         declare
            Name : constant String := Get_String (Ed.Aliases_Model, Iter, 0);
         begin
            Alias := Get_Value (Ed, Name);
            Set_Text (Ed.Current_Alias, Name);
            Ed.Current_Var := new String'(Name);
         end;
      end if;

      if Alias = No_Alias then
         Set_Text (Get_Buffer (Ed.Expansion), "");
         Clear (Ed.Variables_Model);
      else
         Set_Text (Get_Buffer (Ed.Expansion), Alias.Expansion.all);
         Clear (Ed.Variables_Model);

         P := Alias.Params;

         while P /= null loop
            Set_Variable
              (Ed,
               Name     => P.Name.all,
               Default  => P.Initial.all,
               From_Env => P.From_Env,
               Editable => not Alias.Read_Only);
            P := P.Next;
         end loop;
      end if;

      Set_Active (Ed.Must_Reindent, Alias.Must_Reindent);

      Get_Bounds (Get_Buffer (Ed.Expansion), Start, Last);
      Highlight_Expansion_Range (Ed, Start, Last);

      Set_Editable (Ed.Expansion, not Alias.Read_Only);

   exception
      when E : others => Trace (Exception_Handle, E);
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
                 Get_String (Ed.Aliases_Model, Iter, 0);
               Alias : constant Alias_Record := Get_Value (Ed, Old);
            begin
               if Name'Length = 0
                 or else not Is_Entity_Letter
                   (UTF8_Get_Char (Name (Name'First .. Name'Last)))
               then
                  Set (Ed.Aliases_Model, Iter, 0, Old);
                  Message := Message_Dialog
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
                  if Alias /= No_Alias then
                     Set (Ed.Local_Aliases, '_' & Old, Clone (Alias));
                     Set (Ed.Local_Aliases, Name, Clone (Alias));
                  end if;

                  Remove (Ed.Local_Aliases, Old);

                  Free (Ed.Current_Var);
                  Ed.Current_Var := new String'(Name);
                  Set_Text (Ed.Current_Alias, Name);
               end if;
            end;
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Alias_Renamed;

   -------------------
   -- Alias_Deleted --
   -------------------

   procedure Alias_Deleted (Editor : access Gtk_Widget_Record'Class) is
      Ed    : constant Alias_Editor := Alias_Editor (Editor);
      Value : Alias_Record;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;

   begin
      if Ed.Current_Var /= null then
         Value := Get_Value (Ed, Ed.Current_Var.all);

         Set (Ed.Local_Aliases, '_' & Ed.Current_Var.all, Clone (Value));
         Remove (Ed.Local_Aliases, Ed.Current_Var.all);

         Free (Ed.Current_Var);

         Get_Selected (Get_Selection (Ed.Aliases), Model, Iter);
         Remove (Ed.Aliases_Model, Iter);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Alias_Deleted;

   -------------------
   -- Alias_Created --
   -------------------

   procedure Alias_Created (Editor : access Gtk_Widget_Record'Class) is
   begin
      Add_New_Alias (Alias_Editor (Editor), "_new_", Selected => True);

   exception
      when E : others => Trace (Exception_Handle, E);
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

   begin
      --  Set the default value as no longer editable

      Set (Ed.Variables_Model, Iter, 2,
           Get_Boolean (Ed.Variables_Model, Iter, 3));
      Set (Ed.Variables_Model, Iter, 3,
           not Get_Boolean (Ed.Variables_Model, Iter, 3));

   exception
      when E : others => Trace (Exception_Handle, E);
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
         Set (Editor.Variables_Model, Iter, 4, False);
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
                  Set (Editor.Variables_Model, Iter, 4, True);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
         return Menu;
   end Contextual_Factory;

   ------------------------
   -- Contextual_Destroy --
   ------------------------

   procedure Contextual_Destroy
     (Widget : access Gtk_Widget_Record'Class; Menu : Gtk_Menu)
   is
      pragma Unreferenced (Widget);
   begin
      Destroy (Menu);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Contextual_Destroy;

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
      Box              : Gtk_Box;
      Pane             : Gtk_Paned;
      Render           : Gtk_Cell_Renderer_Text;
      Toggle_Render    : Gtk_Cell_Renderer_Toggle;
      Col              : Gtk_Tree_View_Column;
      Number           : Gint;
      Event            : Gtk_Event_Box;
      Frame            : Gtk_Frame;
      Color            : Gdk_Color;
      Expansion_Buffer : Gtk_Text_Buffer;
      Scrolled         : Gtk_Scrolled_Window;
      Button           : Gtk_Button;
      Sep              : Gtk_Separator;
      W                : Gtk_Widget;
      pragma Unreferenced (Number, W);

   begin
      Editor := new Alias_Editor_Record;
      Initialize (Editor,
                  Title  => -"Aliases edition",
                  Parent => Get_Current_Window (Kernel),
                  Flags  => Destroy_With_Parent);
      Set_Default_Size (Editor, 640, 400);

      Reset (Editor.Local_Aliases);

      Gtk_New_Hpaned (Pane);
      Pack_Start (Get_Vbox (Editor), Pane, Expand => True, Fill => True);

      --  List of aliases

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack1 (Pane, Box);

      Gtk_New (Frame);
      Pack_Start (Box, Frame, Expand => True, Fill => True);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Add (Frame, Scrolled);

      Gtk_New (Editor.Aliases_Model,
               (0 => GType_String, 1 => GType_Boolean, 2 => GType_String));
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
        (Editor.Show_Read_Only, Signal_Toggled,
         Show_Read_Only_Toggled'Access, Editor);

      --  Right part

      Gtk_New (Frame);
      Pack2 (Pane, Frame);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      --  Name of current alias

      Gtk_New (Event);
      Pack_Start (Box, Event, Expand => False);
      Color := Parse ("#0e79bd");
      --  ??? Should be shared with the preferences dialog and wizard
      Alloc (Get_Default_Colormap, Color);
      Set_Style (Event, Copy (Get_Style (Event)));
      Set_Background (Get_Style (Event), State_Normal, Color);

      Gtk_New (Editor.Current_Alias, "Current alias");
      Set_Alignment (Editor.Current_Alias, 0.1, 0.5);
      Add (Event, Editor.Current_Alias);

      Gtk_New_Hseparator (Sep);
      Pack_Start (Box, Sep, Expand => False);

      --  Parameters list

      Gtk_New (Editor.Variables_Model,
               (0 => GType_String, 1 => GType_String, 2 => GType_Boolean,
                3 => GType_Boolean, 4 => GType_Boolean, 5 => GType_Boolean));
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
        (Toggle_Render, Signal_Toggled, Param_Env_Changed'Access, Editor);

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
      Modify_Font (Editor.Expansion, Get_Pref_Font (Default_Style));

      Register_Contextual_Menu
        (Widget       => Editor.Expansion,
         Menu_Create  => Contextual_Factory'Access,
         Menu_Destroy => Contextual_Destroy'Access);

      Widget_Callback.Object_Connect
        (Expansion_Buffer, Gtk.Text_Buffer.Signal_Insert_Text,
         Expansion_Inserted'Access, Editor,
         After => True);
      Widget_Callback.Object_Connect
        (Expansion_Buffer, Signal_Delete_Range,
         Expansion_Deleted'Access, Editor,
         After => True);

      --  Filters

      Gtk_New (Editor.Must_Reindent, -"Indent source editor after expansion");
      Pack_Start (Box, Editor.Must_Reindent, Expand => False, Fill => True);

      --  Buttons

      Gtk_New_From_Stock (Button, Stock_New);
      Pack_Start (Get_Action_Area (Editor), Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, Alias_Created'Access, Editor);

      Gtk_New_From_Stock (Button, Stock_Delete);
      Pack_Start (Get_Action_Area (Editor), Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, Alias_Deleted'Access, Editor);

      Gtk_New_Vseparator (Sep);
      Pack_Start (Get_Action_Area (Editor), Sep, Expand => False);

      W := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      W := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);

      Color := Parse (Highlight_Color);
      Alloc (Get_Default_Colormap, Color);
      Gtk_New (Editor.Highlight_Tag);
      Set_Property (Editor.Highlight_Tag, Foreground_Gdk_Property, Color);
      Gtk.Text_Tag_Table.Add
        (Get_Tag_Table (Get_Buffer (Editor.Expansion)),
         Editor.Highlight_Tag);
   end Gtk_New;

   -------------------
   -- Add_New_Alias --
   -------------------

   procedure Add_New_Alias
     (Editor    : access Alias_Editor_Record'Class;
      Name      : String;
      Selected  : Boolean := False;
      Read_Only : Boolean := False)
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
   end Add_New_Alias;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents (Editor : access Alias_Editor_Record'Class) is
      Iter  : Iterator;
      Value : Alias_Record;
      It    : Gtk_Tree_Iter;

   begin
      Clear (Editor.Aliases_Model);
      Get_First (Aliases_Module_Id.Aliases, Iter);

      loop
         Value := Get_Element (Iter);

         exit when Value = No_Alias;

         if not Value.Read_Only
           or else Get_Active (Editor.Show_Read_Only)
         then
            Add_New_Alias
              (Editor, Get_Key (Iter), Read_Only => Value.Read_Only);
         end if;
         Get_Next (Aliases_Module_Id.Aliases, Iter);
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
      Iter  : Iterator;
      Value : Alias_Record;
   begin
      Get_First (Editor.Local_Aliases, Iter);

      loop
         Value := Get_Element (Iter);
         exit when Value = No_Alias;

         declare
            Name : constant String := Get_Key (Iter);
         begin
            if Name (Name'First) = '_' then
               Remove (Aliases_Module_Id.Aliases,
                       Name (Name'First + 1 .. Name'Last));
            else
               Set (Aliases_Module_Id.Aliases, Name, Clone (Value));
            end if;
         end;

         Get_Next (Editor.Local_Aliases, Iter);
      end loop;

      Reset (Editor.Local_Aliases);
   end Update_Aliases;

   ---------------------
   -- On_Edit_Aliases --
   ---------------------

   procedure On_Edit_Aliases
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
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

      Reset (Editor.Local_Aliases);
      Free (Editor.Current_Var);
      Unref (Editor.Highlight_Tag);

      Destroy (Editor);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Edit_Aliases;

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
               Cursor : aliased Integer;
               Must_Reindent : aliased Boolean;
               Replace : constant String := Expand_Alias
                 (Kernel, Expansion (First .. Last - 1),
                  Cursor'Unchecked_Access,
                  Must_Reindent'Unchecked_Access, 0);
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
         Trace (Exception_Handle, E);
         return Invalid_Expansion;
   end Special_Entities;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Aliases_Module_Id_Record;
      File   : VFS.Virtual_File;
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
      Child  : Node_Ptr;
      Expand : String_Ptr;
      P      : Param_Access;
      Old    : Alias_Record;
      N      : Node_Ptr := Node;
   begin
      while N /= null loop
         if N.Tag.all = "alias" then
            declare
               Name : constant String := Get_Attribute (N, "name");
               Must_Reindent : constant Boolean :=
                 Get_Attribute (N, "indent", "false") = "true";
            begin
               Expand := null;
               P := null;

               if N.Tag.all /= "alias"
                 or else Name = ""
               then
                  Insert
                    (Kernel, -"Invalid alias format for " & Name,
                     Mode => Error);
               end if;

               Child := N.Child;
               while Child /= null loop
                  if Child.Tag.all = "text" then
                     Expand := Child.Value;

                  elsif Child.Tag.all = "param" then
                     P := new Param_Record'
                       (Name     => new String'(Get_Attribute (Child, "name")),
                        Initial  => new String'(Child.Value.all),
                        From_Env =>
                          Get_Attribute (Child, "environment") = "true",
                        Next     => P);

                  else
                     Insert (Kernel,
                             -"Unknown XML tag in alias definition for "
                             & Name,
                             Mode => Error);
                  end if;

                  Child := Child.Next;
               end loop;

               --  Do not override a read-only alias: they have been parsed
               --  before all others, but should in fact have higher priority

               Old := Get (Aliases_Module_Id.Aliases, Name);

               if Old = No_Alias or else Old.Read_Only then
                  if Expand /= null then
                     Set (Aliases_Module_Id.Aliases,
                          Name,
                          (Expansion => new String'(Expand.all),
                           Params    => P,
                           Read_Only => Read_Only,
                           Must_Reindent => Must_Reindent));
                  else
                     Set (Aliases_Module_Id.Aliases,
                          Name,
                          (Expansion => new String'(""),
                           Params    => P,
                           Read_Only => Read_Only,
                           Must_Reindent => Must_Reindent));
                  end if;
               else
                  Free (P);
               end if;
            end;
         end if;

         N := N.Next;
      end loop;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Edit : constant String := "/" & (-"Edit");
      Command : Interactive_Alias_Expansion_Command_Access;
      Action  : Action_Record_Access;
   begin
      Aliases_Module_Id := new Aliases_Module_Id_Record;
      Register_Module
        (Module                  => Module_ID (Aliases_Module_Id),
         Kernel                  => Kernel,
         Module_Name             => "Aliases",
         Priority                => Default_Priority);

      Register_Menu
        (Kernel, Edit, -"_Aliases",
         Ref_Item   => -"Preferences",
         Add_Before => True,
         Callback   => On_Edit_Aliases'Access);

      Parse_File
        (Kernel, Get_Home_Dir (Kernel) & "aliases", Read_Only => False);

      Command := new Interactive_Alias_Expansion_Command;
      Command.Kernel := Kernel_Handle (Kernel);
      Action := Register_Action
        (Kernel,
         Name        => "Expand alias",
         Command     => Command,
         Category    => "Editor",
         Description => -"Expand the alias found just before the cursor");
      Register_Menu
        (Kernel, Edit, -"Expand alias",
         Ref_Item   => -"Aliases",
         Add_Before => True,
         Accel_Key  => GDK_LC_o,
         Accel_Mods => Control_Mask,
         Callback   => null,
         Action     => Action,
         Filter     => Lookup_Filter (Kernel, "Source editor"));

      Register_Special_Alias_Entity
        (Kernel, "Expand previous alias", 'O', Special_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, "Current Date", 'D', Special_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, "Current Hour", 'H', Special_Entities'Access);
      --  Others are registered in src_editor_module
   end Register_Module;

end Aliases_Module;
