-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
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
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Calendar.Time_IO;    use GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GUI_Utils;                use GUI_Utils;
with Gdk.Color;                use Gdk.Color;
with Gdk.Event;                use Gdk.Event;
with Gdk.Types.Keysyms;        use Gdk.Types.Keysyms;
with Gdk.Types;                use Gdk.Types;
with Glib.Values;              use Glib.Values;
with Glib.Xml_Int;             use Glib.Xml_Int;
with Glib;                     use Glib;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel;             use Glide_Kernel;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;             use Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Label;                use Gtk.Label;
with Gtk.GEntry;               use Gtk.GEntry;
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
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.Handlers;          use Gtkada.Handlers;
with String_Hash;
with String_Utils;             use String_Utils;
with Traces;                   use Traces;
with System.Assertions;
with Glib.Object;              use Glib.Object;
with Default_Preferences;      use Default_Preferences;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Intl;               use Glide_Intl;
with GUI_Utils;                use GUI_Utils;

package body Aliases_Module is

   Me : constant Debug_Handle := Create ("Aliases");

   Alias_Key : Param_Spec_Key;

   Special : constant Character := '%';

   Highlight_Color : constant String := "#DD0000";
   --  Color used to highlight special entities in the expansion

   type Param_Record;
   type Param_Access is access Param_Record;
   type Param_Record is record
      Name     : String_Access;
      Initial  : String_Access;
      From_Env : Boolean;
      Next     : Param_Access;
   end record;

   type Alias_Record is record
      Expansion : String_Access;
      Params    : Param_Access;
   end record;

   procedure Free (Alias : in out Alias_Record);

   function Clone (Alias : Alias_Record) return Alias_Record;
   --  Return a deep copy of Alias

   No_Alias : constant Alias_Record :=
     (Expansion => null, Params => null);

   package Aliases_Hash is new String_Hash
     (Alias_Record, Free, No_Alias);
   use Aliases_Hash.String_Hash_Table;

   type Expansion_Function_Record;
   type Expansion_Function_List is access Expansion_Function_Record;
   type Expansion_Function_Record (Length : Natural) is record
      Descr : String (1 .. Length);
      Func : Alias_Expansion_Function;
      Next : Expansion_Function_List;
   end record;

   type Expansion_Function_Array is array (Character) of
     Expansion_Function_List;

   type Aliases_Module_Id_Record is new Module_ID_Record with record
      Aliases : Aliases_Hash.String_Hash_Table.HTable;
      Module_Funcs : Expansion_Function_Array;

      Key      : Gdk_Key_Type;
      Modifier : Gdk_Modifier_Type;
      --  Key used to activate the aliases (cached for efficiency)
   end record;
   type Aliases_Module_Id_Access is access all Aliases_Module_Id_Record'Class;

   procedure Destroy (Module : in out Aliases_Module_Id_Record);
   --  Free the memory occupied by the module

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

      Current_Var     : String_Access;
      Highlight_Tag   : Gtk_Text_Tag;
   end record;
   type Alias_Editor is access all Alias_Editor_Record'Class;

   procedure Gtk_New
     (Editor : out Alias_Editor; Kernel : access Kernel_Handle_Record'Class);
   --  Initialize the aliases editor

   type String_Menu_Item_Record (Length : Natural) is new Gtk_Menu_Item_Record
     with record
        Special : String (1 .. Length);
        View    : Gtk_Text_View;
     end record;
   type String_Menu_Item is access all String_Menu_Item_Record'Class;

   procedure Gtk_New
     (Item : out String_Menu_Item;
      View : access Gtk_Text_View_Record'Class;
      Label, Special : String);
   --  Create a new String_Menu_Item.

   procedure Update_Contents (Editor : access Alias_Editor_Record'Class);
   --  Update the contents of the editor, based on the list of currently
   --  defined aliases

   function Key_Handler (Data : Event_Data) return Boolean;
   --  Handler for key events in the GPS main window

   procedure Find_Current_Entity
     (Text : String;
      Current_Pos : Integer;
      First, Last : out Integer);
   --  Set First .. Last to the beginning and end of the current alias name.
   --  ??? Should accept unicode

   function Expand_Alias
     (Data : Event_Data; Name : String; Cursor : access Integer)
      return String;
   --  Return the expanded version of Name.
   --  Cursor is the index in the returnef string for the cursor position.
   --  The empty string is returned if there is no such alias.

   procedure On_Preferences_Changed
     (Kernel : access GObject_Record'Class; K : Kernel_Handle);
   --  Called when preferences'values change

   procedure Load_Aliases
     (Kernel : access Kernel_Handle_Record'Class; Filename : String);
   --  Load aliases from filename.

   procedure Save_Aliases (Filename : String);
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
     (Editor   : access Alias_Editor_Record'Class;
      Name     : String;
      Selected : Boolean := False);
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
      From_Env : Boolean);
   --  Add a new variable in the variable editor

   procedure Update_Vars (Editor : access Alias_Editor_Record'Class);
   --  Parse the expansion of the alias, and update the list of parameters

   procedure Find_Next_Parameter
     (Editor  : access Alias_Editor_Record'Class;
      Start   : out Gtk_Text_Iter;
      Current : in out Gtk_Text_Iter;
      Last    : Gtk_Text_Iter;
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

   function Special_Entities (Data : Event_Data; Special : Character)
      return String;
   --  Provide expansion for some of the special entities

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable
     (Editor   : access Alias_Editor_Record'Class;
      Name     : String;
      Default  : String;
      From_Env : Boolean)
   is
      procedure Internal
        (Tree, Iter : System.Address;
         Col1  : Gint := 0; Name : String;
         Col2  : Gint := 1; Initial : String;
         Col3  : Gint := 2; Editable : Gboolean := 1;
         Col4  : Gint := 3; From_Env : Gboolean := 0;
         Col5  : Gint := 4; Seen : Gboolean := 1;
         Final : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

      Iter : Gtk_Tree_Iter;
   begin
      Append (Editor.Variables_Model, Iter, Null_Iter);
      Internal
        (Get_Object (Editor.Variables_Model), Iter'Address,
         Name => Name & ASCII.NUL, Initial => Default & ASCII.NUL,
         Editable => Boolean'Pos (not From_Env),
         From_Env => Boolean'Pos (From_Env));
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
               Params    => null);

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

   procedure Free (Alias : in out Alias_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Param_Record, Param_Access);

      P : Param_Access;
   begin
      Free (Alias.Expansion);

      while Alias.Params /= null loop
         P := Alias.Params;
         Alias.Params := P.Next;

         Free (P.Name);
         Free (P.Initial);
         Unchecked_Free (P);
      end loop;
   end Free;

   ------------------
   -- Save_Aliases --
   ------------------

   procedure Save_Aliases (Filename : String) is
      File, Key, Child  : Node_Ptr;
      Iter  : Iterator;
      Value : Alias_Record;
      P     : Param_Access;
   begin
      File := new Node;
      File.Tag := new String'("Aliases");

      Get_First (Aliases_Module_Id.Aliases, Iter);
      loop
         Value := Get_Element (Iter);
         exit when Value = No_Alias;

         Key := new Node;
         Key.Tag := new String'("alias");
         Set_Attribute (Key, "name", Get_Key (Iter));

         Child := new Node;
         Child.Tag := new String'("text");
         Child.Value := new String'(Value.Expansion.all);
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
         Get_Next (Aliases_Module_Id.Aliases, Iter);
      end loop;

      Print (File, Filename);
      Free (File);
   end Save_Aliases;

   ------------------
   -- Load_Aliases --
   ------------------

   procedure Load_Aliases
     (Kernel : access Kernel_Handle_Record'Class; Filename : String)
   is
      Alias       : Node_Ptr;
      File, Child : Node_Ptr;
      Expand      : String_Ptr;
      P           : Param_Access;
   begin
      File  := Parse (Filename);
      Alias := File.Child;

      while Alias /= null loop
         declare
            Name : constant String := Get_Attribute (Alias, "name");
         begin
            Expand := null;
            P := null;

            if Alias.Tag.all /= "alias"
              or else Name = ""
            then
               Insert
                 (Kernel, "Invalid format for " & Filename, Mode => Error);
            end if;

            Child := Alias.Child;
            while Child /= null loop
               if Child.Tag.all = "text" then
                  Expand := Child.Value;

               elsif Child.Tag.all = "param" then
                  P := new Param_Record'
                    (Name     => new String'(Get_Attribute (Child, "name")),
                     Initial  => new String'(Child.Value.all),
                     From_Env => Get_Attribute (Child, "environment") = "true",
                     Next     => P);

               else
                  Insert (Kernel, "Unknown XML tag in " & Filename,
                          Mode => Error);
               end if;

               Child := Child.Next;
            end loop;

            if Expand /= null then
               Set (Aliases_Module_Id.Aliases,
                    Get_Attribute (Alias, "name"),
                    (Expansion => new String'(Expand.all),
                     Params    => P));
            else
               Set (Aliases_Module_Id.Aliases,
                    Get_Attribute (Alias, "name"),
                    (Expansion => new String'(""),
                     Params    => P));
            end if;
         end;

         Alias := Alias.Next;
      end loop;

      Free (File);

   exception
      when System.Assertions.Assert_Failure =>
         Insert (Kernel, "Invalid format for " & Filename, Mode => Error);
         Free (File);

      when Status_Error | Name_Error =>
         Trace (Me, "No aliases file " & Filename);

      when E : others =>
         Trace (Me, "Load_Aliases: unexcepted exception "
                & Exception_Information (E));
         Free (File);
   end Load_Aliases;

   -------------------------
   -- Find_Current_Entity --
   -------------------------

   procedure Find_Current_Entity
     (Text : String;
      Current_Pos : Integer;
      First, Last : out Integer) is
   begin
      Last := Current_Pos;
      while Last <= Text'Last
        and then Is_Entity_Letter (Text (Last))
      loop
         Last := Last + 1;
      end loop;

      First := Current_Pos;
      loop
         while First >= Text'First
           and then Is_Entity_Letter (Text (First))
         loop
            First := First - 1;
         end loop;

         exit when Get (Aliases_Module_Id.Aliases,
                        Text (First + 1 .. Last - 1)) /= No_Alias;

         while First >= Text'First
           and then not Is_Entity_Letter (Text (First))
         loop
            First := First - 1;
         end loop;
      end loop;
   end Find_Current_Entity;

   -----------------------
   -- Substitute_Params --
   -----------------------

   function Substitute_Params
     (Text : String; Values : Param_Substitution_Access) return String
   is
      P : Param_Substitution_Access := Values;
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

         Substrings (Substrings'First) :=
           (Name  => new String'("" & Special),
            Value => new String'("" & Special));

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
     (Data : Event_Data; Name : String; Cursor : access Integer)
      return String
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
      begin
         Cursor.all := Str'Length;

         while S <= Str'Last - 1 loop
            if Str (S) = Special then
               Result := Result & Str (First .. S - 1);
               First := S + 2;

               if Str (S + 1) = '_' then
                  Cursor.all := Ada.Strings.Unbounded.Length (Result);

               else
                  Tmp := Aliases_Module_Id.Module_Funcs (Str (S + 1));

                  while Tmp /= null loop
                     declare
                        Replace : constant String :=
                          Tmp.Func (Data, Str (S + 1));
                     begin
                        if Replace /= Invalid_Expansion then
                           Result := Result & Replace;
                           exit;
                        end if;
                     end;

                     Tmp := Tmp.Next;
                  end loop;
               end if;

               S := S + 2;

            else
               S := S + 1;
            end if;
         end loop;

         if First <= Str'Last then
            Result := Result & Str (First .. Str'Last);
         end if;

         return Ada.Strings.Unbounded.To_String (Result);
      end Find_And_Replace_Cursor;

      Alias : constant Alias_Record := Get
        (Aliases_Module_Id.Aliases, Name);
      Values : Param_Substitution_Access;
      Dialog : Gtk_Dialog;
      Box : Gtk_Box;
      S : Gtk_Size_Group;
      P : Param_Access;
      Label : Gtk_Label;
      W : Gtk_Widget;
      Val : String_Access;
   begin
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
                        Parent => Get_Main_Window (Get_Kernel (Data)),
                        Flags  => Destroy_With_Parent);
               Gtk_New (S);

               W := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
               Grab_Default (W);
               W := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
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

   -----------------
   -- Key_Handler --
   -----------------

   function Key_Handler (Data : Event_Data) return Boolean is
      Event : constant Gdk_Event := Get_Event (Data);
      W : Gtk_Widget;
   begin
      if Get_Event_Type (Event) = Key_Press
        and then Get_State (Event) = Aliases_Module_Id.Modifier
        and then Get_Key_Val (Event) = Aliases_Module_Id.Key
      then
         W := Get_Widget (Data);

         if W.all in Gtk_Editable_Record'Class then
            if Get_Editable (Gtk_Editable (W)) then
               declare
                  Text : constant String := Get_Chars (Gtk_Editable (W));
                  First, Last : Integer;
               begin
                  Find_Current_Entity
                    (Text, Integer (Get_Position (Gtk_Editable (W))),
                     First, Last);

                  declare
                     Cursor : aliased Integer;
                     Replace : constant String := Expand_Alias
                       (Data,
                        Text (First + 1 .. Last - 1),
                        Cursor'Unchecked_Access);
                  begin
                     if Replace /= "" then
                        Delete_Text
                          (Gtk_Editable (W), Gint (First), Gint (Last - 1));
                        Insert_Text (Gtk_Editable (W), Replace, Gint (First));
                        Set_Position (Gtk_Editable (W),
                                      Gint (First + Cursor - Replace'Length));
                     end if;
                  end;
                  return True;
               end;
            end if;

         elsif W.all in Gtk_Text_View_Record'Class then
            if Get_Editable (Gtk_Text_View (W)) then
               declare
                  Buffer : constant Gtk_Text_Buffer := Get_Buffer
                    (Gtk_Text_View (W));
                  First_Iter, Last_Iter : Gtk_Text_Iter;
                  Success : Boolean := True;
               begin
                  Get_Iter_At_Mark (Buffer, First_Iter, Get_Insert (Buffer));
                  Search_Entity_Bounds (First_Iter, Last_Iter);

                  while Success
                    and then Get
                    (Aliases_Module_Id.Aliases,
                     Get_Slice (Buffer, First_Iter, Last_Iter)) = No_Alias
                  loop
                     Backward_Word_Start (First_Iter, Success);
                  end loop;

                  declare
                     Cursor : aliased Integer;
                     Replace : constant String := Expand_Alias
                       (Data,
                        Get_Slice (Buffer, First_Iter, Last_Iter),
                        Cursor'Unchecked_Access);
                     Result : Boolean;
                     Event : Gdk_Event;
                  begin
                     if Replace /= "" then
                        --  Simulate a focus_in/focus_out event, needed for the
                        --  GPS source editor, which saves and restores the
                        --  cursor position when the focus changes (for the
                        --  handling of multiple views).
                        Allocate (Event, Enter_Notify, Get_Window (W));
                        Result := Return_Callback.Emit_By_Name
                          (W, "focus_in_event", Event);
                        Free (Event);

                        Delete (Buffer, First_Iter, Last_Iter);
                        Insert (Buffer, First_Iter, Replace);
                        Backward_Chars (First_Iter,
                                        Gint (Replace'Length - Cursor),
                                        Result);
                        Place_Cursor (Buffer, First_Iter);

                        Allocate (Event, Leave_Notify, Get_Window (W));
                        Result := Return_Callback.Emit_By_Name
                          (W, "focus_out_event", Event);
                        Free (Event);
                     end if;
                  end;
                  return True;
               end;
            end if;
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexception exception: " & Exception_Information (E));
         return False;
   end Key_Handler;

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
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Editor.Expansion);
      Start, Last : Gtk_Text_Iter;
      P      : Param_Access;
      Iter   : Gtk_Tree_Iter;
   begin
      if Editor.Current_Var /= null then
         Iter := Get_Iter_First (Editor.Variables_Model);
         while Iter /= Null_Iter loop
            declare
               Name : constant String :=
                 Get_String (Editor.Variables_Model, Iter, 0);
               Initial : constant String :=
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
               Params    => P));
      end if;
   end Save_Current_Var;

   -----------------------------
   -- Alias_Selection_Changed --
   -----------------------------

   procedure Alias_Selection_Changed
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed : constant Alias_Editor := Alias_Editor (Editor);
      Model : Gtk_Tree_Model;
      Iter : Gtk_Tree_Iter;
      Alias : Alias_Record := No_Alias;
      P : Param_Access;
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
            Set_Variable (Ed,
                          Name     => P.Name.all & ASCII.NUL,
                          Default  => P.Initial.all & ASCII.NUL,
                          From_Env => P.From_Env);
            P := P.Next;
         end loop;
      end if;

      Get_Bounds (Get_Buffer (Ed.Expansion), Start, Last);
      Highlight_Expansion_Range (Ed, Start, Last);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Alias_Selection_Changed;

   -------------------
   -- Alias_Renamed --
   -------------------

   procedure Alias_Renamed (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Alias_Editor := Alias_Editor (Editor);
      Model : Gtk_Tree_Model;
      Iter : Gtk_Tree_Iter;
      Message : Message_Dialog_Buttons;
      pragma Unreferenced (Message);
   begin
      if Ed.Current_Var /= null then
         Get_Selected (Get_Selection (Ed.Aliases), Model, Iter);

         if Iter /= Null_Iter then
            declare
               Old : constant String := Ed.Current_Var.all;
               Name : constant String :=
                 Get_String (Ed.Aliases_Model, Iter, 0);
               Alias : constant Alias_Record := Get_Value (Ed, Old);
            begin
               if not Is_Entity_Letter (Name (Name'First)) then
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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Alias_Renamed;

   -------------------
   -- Alias_Deleted --
   -------------------

   procedure Alias_Deleted (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Alias_Editor := Alias_Editor (Editor);
      Value : Alias_Record;
      Model : Gtk_Tree_Model;
      Iter : Gtk_Tree_Iter;
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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Alias_Deleted;

   -------------------
   -- Alias_Created --
   -------------------

   procedure Alias_Created (Editor : access Gtk_Widget_Record'Class) is
   begin
      Add_New_Alias (Alias_Editor (Editor), "_new_", Selected => True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Alias_Created;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access GObject_Record'Class; K : Kernel_Handle)
   is
      pragma Unreferenced (Kernel);
   begin
      Get_Pref (K, Alias_Key,
                Aliases_Module_Id.Modifier,
                Aliases_Module_Id.Key);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Preferences_Changed;

   -----------------------
   -- Param_Env_Changed --
   -----------------------

   procedure Param_Env_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Ed : constant Alias_Editor := Alias_Editor (Editor);
      Path_String : constant String := Get_String (Nth (Params, 1));
      Iter   : constant Gtk_Tree_Iter :=
        Get_Iter_From_String (Ed.Variables_Model, Path_String);
   begin
      --  Set the default value as no longer editable
      Set (Ed.Variables_Model, Iter, 2,
           Get_Boolean (Ed.Variables_Model, Iter, 3));
      Set (Ed.Variables_Model, Iter, 3,
           not Get_Boolean (Ed.Variables_Model, Iter, 3));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Param_Env_Changed;

   -------------------------
   -- Find_Next_Parameter --
   -------------------------

   procedure Find_Next_Parameter
     (Editor  : access Alias_Editor_Record'Class;
      Start   : out Gtk_Text_Iter;
      Current : in out Gtk_Text_Iter;
      Last    : Gtk_Text_Iter;
      All_Specials : Boolean)
   is
      Last_Offset : constant Gint := Get_Offset (Last);
      Result : Boolean;
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
      Iter : Gtk_Tree_Iter := Get_Iter_First (Editor.Variables_Model);
      Start, Current, Last : Gtk_Text_Iter;
      Found : Boolean;
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
            Name : constant String :=
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
                 (Editor, Name => Name, Default => "", From_Env => False);
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
      Pos, Eol  : Gtk_Text_Iter;
      --   Text : constant String := Get_String (Nth (Params, 2));
      Length : constant Gint := Get_Int (Nth (Params, 3));
      Result : Boolean;
   begin
      Get_Text_Iter (Nth (Params, 1), Pos);
      Copy (Source => Pos, Dest => Eol);

      Set_Line_Index (Pos, 0);
      Forward_Chars (Eol, Length, Result);
      Forward_To_Line_End (Eol, Result);

      Highlight_Expansion_Range (Alias_Editor (Editor), Pos, Eol);
      Update_Vars (Alias_Editor (Editor));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Expansion_Inserted;

   -----------------------
   -- Expansion_Deleted --
   -----------------------

   procedure Expansion_Deleted
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Start, Last : Gtk_Text_Iter;
      Result : Boolean;
   begin
      Get_Text_Iter (Nth (Params, 1), Start);
      Get_Text_Iter (Nth (Params, 2), Last);

      Set_Line_Index (Start, 0);
      Forward_To_Line_End (Last, Result);

      Highlight_Expansion_Range (Alias_Editor (Editor), Start, Last);
      Update_Vars (Alias_Editor (Editor));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
     (Item : out String_Menu_Item;
      View : access Gtk_Text_View_Record'Class;
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
      It : constant String_Menu_Item := String_Menu_Item (Item);
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (It.View);
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
      Insert (Buffer, Iter, It.Special);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
        (Item, "activate",
         Widget_Callback.To_Marshaller (Insert_Special'Access));

      Gtk_New (Item, View, -"Insert Cursor Position", Special & "_");
      Add (Menu, Item);
      Widget_Callback.Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (Insert_Special'Access));

      for C in Aliases_Module_Id.Module_Funcs'Range loop
         Tmp := Aliases_Module_Id.Module_Funcs (C);
         while Tmp /= null loop
            Gtk_New (Item, View, Tmp.Descr, Special & C);
            Add (Menu, Item);
            Widget_Callback.Connect
              (Item, "activate",
               Widget_Callback.To_Marshaller (Insert_Special'Access));
            Tmp := Tmp.Next;
         end loop;
      end loop;

      return Menu;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Contextual_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Alias_Editor; Kernel : access Kernel_Handle_Record'Class)
   is
      Box    : Gtk_Box;
      Pane   : Gtk_Paned;
      Render : Gtk_Cell_Renderer_Text;
      Toggle_Render : Gtk_Cell_Renderer_Toggle;
      Col    : Gtk_Tree_View_Column;
      Number : Gint;
      Event  : Gtk_Event_Box;
      Frame  : Gtk_Frame;
      Color  : Gdk_Color;
      Expansion_Buffer : Gtk_Text_Buffer;
      Scrolled : Gtk_Scrolled_Window;
      Button : Gtk_Button;
      Sep    : Gtk_Separator;
      W      : Gtk_Widget;
      pragma Unreferenced (Number, W);
   begin
      Editor := new Alias_Editor_Record;
      Initialize (Editor,
                  Title  => -"Aliases edition",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Destroy_With_Parent);
      Set_Default_Size (Editor, 640, 400);

      Reset (Editor.Local_Aliases);

      Gtk_New_Hpaned (Pane);
      Pack_Start (Get_Vbox (Editor), Pane, Expand => True, Fill => True);

      --  List of aliases

      Gtk_New (Frame);
      Pack1 (Pane, Frame);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Add (Frame, Scrolled);

      Gtk_New (Editor.Aliases_Model,
               (0 => GType_String, 1 => GType_Boolean));
      Gtk_New (Editor.Aliases, Editor.Aliases_Model);
      Add (Scrolled, Editor.Aliases);
      Set_Mode (Get_Selection (Editor.Aliases), Selection_Single);

      Gtk_New (Render);
      Gtk_New (Editor.Alias_Col);
      Set_Clickable (Editor.Alias_Col, True);
      Set_Sort_Column_Id (Editor.Alias_Col, 0);
      Number := Append_Column (Editor.Aliases, Editor.Alias_Col);
      Set_Title (Editor.Alias_Col, -"Aliases");
      Pack_Start (Editor.Alias_Col, Render, True);
      Add_Attribute (Editor.Alias_Col, Render, "text", 0);
      Add_Attribute (Editor.Alias_Col, Render, "editable", 1);

      Set_Editable_And_Callback (Editor.Aliases_Model, Render, 0);

      Widget_Callback.Object_Connect
        (Get_Selection (Editor.Aliases), "changed",
         Widget_Callback.To_Marshaller (Alias_Selection_Changed'Access),
         Editor);
      Widget_Callback.Object_Connect
        (Render, "edited",
         Widget_Callback.To_Marshaller (Alias_Renamed'Access), Editor);

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
                3 => GType_Boolean, 4 => GType_Boolean));
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

      Gtk_New (Toggle_Render);
      Gtk_New (Col);
      Number := Append_Column (Editor.Variables, Col);
      Set_Title (Col, -"Environment");
      Set_Resizable (Col, True);
      Pack_Start (Col, Toggle_Render, False);
      Add_Attribute (Col, Toggle_Render, "active", 3);

      Widget_Callback.Object_Connect
        (Toggle_Render, "toggled", Param_Env_Changed'Access, Editor);

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
      Modify_Font (Editor.Expansion, Get_Pref (Kernel, Source_Editor_Font));

      Register_Contextual_Menu
        (Widget       => Editor.Expansion,
         Menu_Create  => Contextual_Factory'Access,
         Menu_Destroy => Contextual_Destroy'Access);

      Widget_Callback.Object_Connect
        (Expansion_Buffer, "insert_text", Expansion_Inserted'Access, Editor,
         After => True);
      Widget_Callback.Object_Connect
        (Expansion_Buffer, "delete_range", Expansion_Deleted'Access, Editor,
         After => True);

      --  Buttons

      Gtk_New_From_Stock (Button, Stock_New);
      Pack_Start (Get_Action_Area (Editor), Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Alias_Created'Access), Editor);

      Gtk_New_From_Stock (Button, Stock_Delete);
      Pack_Start (Get_Action_Area (Editor), Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Alias_Deleted'Access), Editor);

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
     (Editor   : access Alias_Editor_Record'Class;
      Name     : String;
      Selected : Boolean := False)
   is
      procedure Set_Alias
        (Tree, Iter : System.Address;
         Col1  : Gint; Name : String;
         Col2  : Gint := 1; Editable : Gboolean := 1;
         Final : Gint := -1);
      pragma Import (C, Set_Alias, "gtk_tree_store_set");

      Alias_Iter : Gtk_Tree_Iter;
   begin
      Append (Editor.Aliases_Model, Alias_Iter, Null_Iter);
      Set_Alias
        (Get_Object (Editor.Aliases_Model), Alias_Iter'Address,
         0, Name & ASCII.NUL);

      if Selected then
         Select_Iter (Get_Selection (Editor.Aliases), Alias_Iter);
      end if;
   end Add_New_Alias;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents (Editor : access Alias_Editor_Record'Class) is
      Iter       : Iterator;
      Value      : Alias_Record;
      It         : Gtk_Tree_Iter;
   begin
      Get_First (Aliases_Module_Id.Aliases, Iter);

      loop
         Value := Get_Element (Iter);
         exit when Value = No_Alias;

         Add_New_Alias (Editor, Get_Key (Iter));
         Get_Next (Aliases_Module_Id.Aliases, Iter);
      end loop;

      Clicked (Editor.Alias_Col);

      It := Get_Iter_First (Editor.Aliases_Model);
      if It /= Null_Iter then
         Select_Iter (Get_Selection (Editor.Aliases), It);
      end if;
   end Update_Contents;

   --------------------
   -- Update_Aliases --
   --------------------

   procedure Update_Aliases (Editor : access Alias_Editor_Record'Class) is
      Iter       : Iterator;
      Value      : Alias_Record;
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
            Save_Aliases
              (Name_As_Directory (Get_Home_Dir (Kernel)) & "aliases");

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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Edit_Aliases;

   -----------------------------------
   -- Register_Special_Alias_Entity --
   -----------------------------------

   procedure Register_Special_Alias_Entity
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Description : String;
      Entity  : Character;
      Func    : Alias_Expansion_Function)
   is
      pragma Unreferenced (Kernel);
   begin
      Assert (Me, Aliases_Module_Id /= null,
              "Register_Special_Alias_Entity: module not initialized");
      Aliases_Module_Id.Module_Funcs (Entity) :=
        new Expansion_Function_Record'
          (Length => Description'Length,
           Descr  => Description,
           Func   => Func,
           Next   => Aliases_Module_Id.Module_Funcs (Entity));
   end Register_Special_Alias_Entity;

   ----------------------
   -- Special_Entities --
   ----------------------

   function Special_Entities (Data : Event_Data; Special : Character)
      return String
   is
      pragma Unreferenced (Data);
   begin
      case Special is
         when 'D'    => return Image (Ada.Calendar.Clock, ISO_Date);
         when 'H'    => return Image (Ada.Calendar.Clock, "%T");
         when others => return Invalid_Expansion;
      end case;

   exception
      when E : others =>
         Trace (Me, "Unexception exception: " & Exception_Information (E));
         return Invalid_Expansion;
   end Special_Entities;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Edit : constant String := "/" & (-"Edit");
   begin
      Aliases_Module_Id := new Aliases_Module_Id_Record;
      Register_Module
        (Module                  => Module_ID (Aliases_Module_Id),
         Kernel                  => Kernel,
         Module_Name             => "Aliases",
         Priority                => Default_Priority);

      Alias_Key := Gnew_Key
        (Name  => "Aliases-Key",
         Nick  => -"Aliases key",
         Blurb => -"Key used for aliases expansion",
         Default_Modifier => Control_Mask,
         Default_Key      => GDK_LC_o);
      Register_Property (Kernel, Param_Spec (Alias_Key), -"General");

      Register_Menu
        (Kernel, Edit, -"_Aliases",
         Ref_Item   => -"Preferences",
         Add_Before => False,
         Callback   => On_Edit_Aliases'Access);

      Load_Aliases
        (Kernel, Name_As_Directory (Get_Home_Dir (Kernel)) & "aliases");

      Kernel_Callback.Connect
        (Kernel, "preferences_changed",
         Kernel_Callback.To_Marshaller (On_Preferences_Changed'Access),
         Kernel_Handle (Kernel));

      On_Preferences_Changed (Kernel, Kernel_Handle (Kernel));

      Register_Key_Handlers (Kernel, Key_Handler'Access);

      Register_Special_Alias_Entity
        (Kernel, "Current Date", 'D', Special_Entities'Access);
      Register_Special_Alias_Entity
        (Kernel, "Current Hour", 'H', Special_Entities'Access);
   end Register_Module;

end Aliases_Module;
