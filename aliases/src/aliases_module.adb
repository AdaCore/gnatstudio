-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GUI_Utils;                use GUI_Utils;
with Gdk.Color;                use Gdk.Color;
with Gdk.Event;                use Gdk.Event;
with Gdk.Types.Keysyms;        use Gdk.Types.Keysyms;
with Gdk.Types;                use Gdk.Types;
with Glib.Xml_Int;             use Glib.Xml_Int;
with Glib;                     use Glib;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel;             use Glide_Kernel;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;             use Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Label;                use Gtk.Label;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Separator;            use Gtk.Separator;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Style;                use Gtk.Style;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
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

package body Aliases_Module is

   Me : constant Debug_Handle := Create ("Aliases");

   Alias_Key : Param_Spec_Key;

   type Param_Record;
   type Param_Access is access Param_Record;
   type Param_Record is record
      Name    : String_Access;
      Initial : String_Access;
      Next    : Param_Access;
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

   type Aliases_Module_Id_Record is new Module_ID_Record with record
      Aliases : Aliases_Hash.String_Hash_Table.HTable;

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

      Current_Var     : String_Access;
   end record;
   type Alias_Editor is access all Alias_Editor_Record'Class;

   procedure Gtk_New
     (Editor : out Alias_Editor; Kernel : access Kernel_Handle_Record'Class);
   --  Initialize the aliases editor

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

   function Expand_Alias (Name : String) return String;
   --  Return the expanded version of Name.
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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Aliases_Module_Id_Record) is
   begin
      Reset (Module.Aliases);
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
              (Name    => new String'(P.Name.all),
               Initial => new String'(P.Initial.all),
               Next    => A.Params);
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
                    (Name    => new String'(Get_Attribute (Child, "name")),
                     Initial => new String'(Child.Value.all),
                     Next    => P);

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
      First := Current_Pos;
      while First >= Text'First
        and then Is_Entity_Letter (Text (First))
      loop
         First := First - 1;
      end loop;

      Last := Current_Pos;
      while Last <= Text'Last
        and then Is_Entity_Letter (Text (Last))
      loop
         Last := Last + 1;
      end loop;
   end Find_Current_Entity;

   ------------------
   -- Expand_Alias --
   ------------------

   function Expand_Alias (Name : String) return String is
      Alias : constant Alias_Record := Get
        (Aliases_Module_Id.Aliases, Name);
   begin
      if Alias = No_Alias then
         return "";
      else
         return Alias.Expansion.all;
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
                     Replace : constant String := Expand_Alias
                       (Text (First + 1 .. Last - 1));
                  begin
                     if Replace /= "" then
                        Delete_Text
                          (Gtk_Editable (W), Gint (First), Gint (Last - 1));
                        Insert_Text (Gtk_Editable (W), Replace, Gint (First));
                        Set_Position (Gtk_Editable (W), Gint (First));
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
               begin
                  Get_Iter_At_Mark (Buffer, First_Iter, Get_Insert (Buffer));
                  Search_Entity_Bounds (First_Iter, Last_Iter);

                  declare
                     Replace : constant String := Expand_Alias
                       (Get_Slice (Buffer, First_Iter, Last_Iter));
                  begin
                     if Replace /= "" then
                        Delete (Buffer, First_Iter, Last_Iter);
                        Insert (Buffer, First_Iter, Replace);
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
         Trace (Me, "Key_Handler: Unexception exception "
                & Exception_Information (E));
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
   begin
      if Editor.Current_Var /= null then
         Get_Start_Iter (Buffer, Start);
         Get_End_Iter   (Buffer, Last);

         --  ??? Should setup params
         Set (Editor.Local_Aliases, Editor.Current_Var.all,
              (Expansion => new String'(Get_Text (Buffer, Start, Last)),
               Params    => null));
      end if;
   end Save_Current_Var;

   -----------------------------
   -- Alias_Selection_Changed --
   -----------------------------

   procedure Alias_Selection_Changed
     (Editor : access Gtk_Widget_Record'Class)
   is
      procedure Set_Variable
        (Tree, Iter : System.Address;
         Col1  : Gint := 0; Name : String;
         Col2  : Gint := 1; Initial : String;
         Col3  : Gint := 2; Editable : Boolean := True;
         Final : Gint := -1);
      pragma Import (C, Set_Variable, "gtk_tree_store_set");


      Ed : constant Alias_Editor := Alias_Editor (Editor);
      Model : Gtk_Tree_Model;
      Iter : Gtk_Tree_Iter;
      Alias : Alias_Record := No_Alias;
      P : Param_Access;
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

      Clear (Ed.Variables_Model);

      if Alias = No_Alias then
         Set_Text (Get_Buffer (Ed.Expansion), "");
      else
         Set_Text (Get_Buffer (Ed.Expansion), Alias.Expansion.all);

         P := Alias.Params;
         while P /= null loop
            Append (Ed.Variables_Model, Iter, Null_Iter);
            Set_Variable (Get_Object (Ed.Variables_Model), Iter'Address,
                          Name    => P.Name.all & ASCII.NUL,
                          Initial => P.Initial.all & ASCII.NUL);
            P := P.Next;
         end loop;
      end if;
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
               for N in Name'Range loop
                  if not Is_Entity_Letter (Name (N)) then
                     Set (Ed.Aliases_Model, Iter, 0, Old);
                     Message := Message_Dialog
                       (Msg => -"Error: invalid name for alias: " & Name
                          & ASCII.LF
                          & (-"Only ASCII letters and digits are authorized"),
                        Dialog_Type => Error,
                        Buttons => Button_OK,
                        Title => -"Invalid alias name",
                        Parent => Gtk_Window (Editor));
                     return;
                  end if;
               end loop;

               Set (Ed.Local_Aliases, Name, Clone (Alias));
               Set (Ed.Local_Aliases, '_' & Old, Clone (Alias));
               Remove (Ed.Local_Aliases, Old);

               Free (Ed.Current_Var);
               Ed.Current_Var := new String'(Name);
               Set_Text (Ed.Current_Alias, Name);
            end;
         end if;
      end if;
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
   end Alias_Deleted;

   -------------------
   -- Alias_Created --
   -------------------

   procedure Alias_Created (Editor : access Gtk_Widget_Record'Class) is
   begin
      Add_New_Alias (Alias_Editor (Editor), "_new_", Selected => True);
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
   end On_Preferences_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Alias_Editor; Kernel : access Kernel_Handle_Record'Class)
   is
      Box    : Gtk_Box;
      Pane   : Gtk_Paned;
      Render : Gtk_Cell_Renderer_Text;
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

      Gtk_New (Editor.Aliases_Model,
               (0 => GType_String, 1 => GType_Boolean));
      Gtk_New (Editor.Aliases, Editor.Aliases_Model);
      Add (Frame, Editor.Aliases);
      Set_Mode (Get_Selection (Editor.Aliases), Selection_Single);

      Gtk_New (Render);
      Gtk_New (Col);
      Set_Clickable (Col, True);
      Set_Sort_Column_Id (Col, 0);
      Number := Append_Column (Editor.Aliases, Col);
      Set_Title (Col, -"Aliases");
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", 0);
      Add_Attribute (Col, Render, "editable", 1);

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
               (0 => GType_String, 1 => GType_String, 2 => GType_Boolean));
      Gtk_New (Editor.Variables, Editor.Variables_Model);
      Pack_Start (Box, Editor.Variables, Expand => False);

      Gtk_New (Render);
      Gtk_New (Col);
      Number := Append_Column (Editor.Variables, Col);
      Set_Title (Col, -"Parameter");
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "text", 0);
      Set_Clickable (Col, True);
      Set_Sort_Column_Id (Col, 0);

      Gtk_New (Render);
      Gtk_New (Col);
      Number := Append_Column (Editor.Variables, Col);
      Set_Title (Col, -"Initial Value");
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "text", 1);
      Add_Attribute (Col, Render, "editable", 2);
      Set_Editable_And_Callback (Editor.Variables_Model, Render, 0);

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

      W := Add_Button (Editor, Stock_Save, Gtk_Response_OK);
      W := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);
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
         Col2  : Gint := 1; Editable : Boolean := True;
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
   begin
      Get_First (Aliases_Module_Id.Aliases, Iter);

      loop
         Value := Get_Element (Iter);
         exit when Value = No_Alias;

         Add_New_Alias (Editor, Get_Key (Iter));
         Get_Next (Aliases_Module_Id.Aliases, Iter);
      end loop;
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

      Destroy (Editor);
   end On_Edit_Aliases;

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
        (Kernel, Edit, -"Aliases...",
         Ref_Item => -"Unit Testing",
         Callback => On_Edit_Aliases'Access);

      Load_Aliases
        (Kernel, Name_As_Directory (Get_Home_Dir (Kernel)) & "aliases");

      Kernel_Callback.Connect
        (Kernel, "preferences_changed",
         Kernel_Callback.To_Marshaller (On_Preferences_Changed'Access),
         Kernel_Handle (Kernel));

      On_Preferences_Changed (Kernel, Kernel_Handle (Kernel));

      Register_Key_Handlers
        (Kernel, Key_Handler'Unrestricted_Access);
   end Register_Module;

end Aliases_Module;
