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

with Glide_Kernel; use Glide_Kernel;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Gdk.Event;    use Gdk.Event;
with Gdk.Types;    use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Glib.Xml_Int; use Glib.Xml_Int;
with Commands;     use Commands;
with HTables;      use HTables;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GUI_Utils;    use GUI_Utils;
with Ada.Unchecked_Conversion;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Box;                  use Gtk.Box;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Gtk.Vbutton_Box;          use Gtk.Vbutton_Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Glide_Intl;               use Glide_Intl;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Enums;                use Gtk.Enums;
with System;                   use System;
with Gtk.Accel_Map;            use Gtk.Accel_Map;
with Traces;                   use Traces;

package body KeyManager_Module is

   Me : constant Debug_Handle := Create ("Keymanager");

   Menu_Context_Name : constant String := "Menus";
   --  -"Menus" will need to be translated

   type Keys_Header_Num is range 0 .. 1000;
   type Key_Binding is record
      Key      : Gdk_Key_Type;
      Modifier : Gdk_Modifier_Type;
   end record;
   No_Binding : constant Key_Binding := (0, 0);

   type Key_Description;
   type Key_Description_List is access Key_Description;
   type Key_Description is record
      Name    : String_Access;
      Tooltip : String_Access;
      Command : Command_Access;
      Context : Key_Context;
      Next    : Key_Description_List;
   end record;
   No_Key : constant Key_Description_List := null;

   function Hash (Key : Key_Binding) return Keys_Header_Num;
   procedure Free (Element : in out Key_Description_List);
   --  Support functions for creating the htable

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Key_Context);
   function Convert is new Ada.Unchecked_Conversion
     (Key_Context, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Command_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Command_Access, System.Address);

   package Key_Htable is new Simple_HTable
     (Header_Num   => Keys_Header_Num,
      Element      => Key_Description_List,
      Free_Element => Free,
      No_Element   => No_Key,
      Key          => Key_Binding,
      Hash         => Hash,
      Equal        => "=");
   use Key_Htable;

   type Key_Manager_Record is new Glide_Kernel.Key_Handler_Record with record
      Kernel : Kernel_Handle;
      Table  : Key_Htable.HTable;

      Active : Boolean := True;
      --  Whether the key manager should process the key events. This is only
      --  deactivated while editing the key bindings through the GUI.
   end record;
   type Key_Manager_Access is access all Key_Manager_Record'Class;

   procedure Register_Key
     (Handler        : access Key_Manager_Record;
      Name           : String;
      Default_Key    : Gdk.Types.Gdk_Key_Type;
      Default_Mod    : Gdk.Types.Gdk_Modifier_Type;
      Command        : access Commands.Root_Command'Class;
      Tooltip        : String := "";
      Context        : Key_Context := null);
   function Process_Event
     (Handler  : access Key_Manager_Record;
      Event    : Event_Data) return Boolean;
   procedure Free (Handler : in out Key_Manager_Record);
   --  See documentation for imported subprograms

   procedure Register_Key_Internal
     (Handler        : access Key_Manager_Record'Class;
      Name           : String;
      Default_Key    : Gdk.Types.Gdk_Key_Type;
      Default_Mod    : Gdk.Types.Gdk_Modifier_Type;
      Command        : Commands.Command_Access := null;
      Tooltip        : String := "";
      Context        : Key_Context := null);
   --  Internal version of Register_Key.

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class;
      Manager : access Key_Manager_Record'Class);
   --  Load the customized key bindings

   procedure On_Edit_Keys
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open a GUI to edit the key bindings

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class);
   --  Handle the "Grab" button

   type Keys_Editor_Record is new Gtk_Dialog_Record with record
      Kernel  : Kernel_Handle;
      View    : Gtk_Tree_View;
      Model   : Gtk_Tree_Store;
   end record;
   type Keys_Editor is access all Keys_Editor_Record'Class;

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class);
   --  Fill the contents of the editor

   procedure Save_Editor (Editor : access Keys_Editor_Record'Class);
   --  Save the contents of the editor

   procedure Lookup_Command_By_Name
     (Handler : access Key_Manager_Record'Class;
      Name    : String;
      Key     : out Key_Binding;
      Binding : out Key_Description_List);
   --  Search the description of a command in the table

   Name_Column    : constant := 0;
   Key_Column     : constant := 1;
   Modif_Column   : constant := 2;
   Image_Column   : constant := 3;
   Context_Column : constant := 4;
   Tooltip_Column : constant := 5;
   Command_Column : constant := 6;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Key_Binding) return Keys_Header_Num is
   begin
      return Keys_Header_Num
      ((Integer (Key.Key) + Integer (Key.Modifier) * 16#FFFF#)
         mod Integer (Keys_Header_Num'Last + 1));
   end Hash;

   ----------
   -- Free --
   ----------

   procedure Free (Element : in out Key_Description_List) is
      Current : Key_Description_List := Element;
      Next    : Key_Description_List;
   begin
      while Current /= null loop
         Next := Current.Next;
         Free (Current.Name);
         Free (Current.Tooltip);

         --  Do not free the command or the context, since these are shared
         --  among key bindings

         Current := Next;
      end loop;
   end Free;

   ----------------------------
   -- Lookup_Command_By_Name --
   ----------------------------

   procedure Lookup_Command_By_Name
     (Handler : access Key_Manager_Record'Class;
      Name    : String;
      Key     : out Key_Binding;
      Binding : out Key_Description_List)
   is
      Iter  : Key_Htable.Iterator;
      Bind  : Key_Description_List;
   begin
      --  We do not use the most efficient method, since we simply
      --  traverse a list, but there aren't hundreds of keybindings...

      Get_First (Handler.Table, Iter);
      loop
         Bind := Get_Element (Iter);
         exit when Bind = No_Key;

         while Bind /= null loop
            if Bind.Name.all = Name then
               Key     := Get_Key (Iter);
               Binding := Bind;
               return;
            end if;
            Bind := Bind.Next;
         end loop;

         Get_Next (Handler.Table, Iter);
      end loop;

      Key     := No_Binding;
      Binding := null;
   end Lookup_Command_By_Name;

   ---------------------------
   -- Register_Key_Internal --
   ---------------------------

   procedure Register_Key_Internal
     (Handler        : access Key_Manager_Record'Class;
      Name           : String;
      Default_Key    : Gdk.Types.Gdk_Key_Type;
      Default_Mod    : Gdk.Types.Gdk_Modifier_Type;
      Command        : Commands.Command_Access := null;
      Tooltip        : String := "";
      Context        : Key_Context := null)
   is
      Binding, Binding2 : Key_Description_List;
      Key     : Key_Binding;
   begin
      --  Chech whether command is already associated with a key binding

      Lookup_Command_By_Name (Handler, Name, Key, Binding);
      if Binding /= null then
         --  Keep the current key binding, since it was probably
         --  customized by the user
         Free (Binding.Name);
         Binding.Name := new String'(Name);
         Free (Binding.Tooltip);
         Binding.Tooltip := new String'(Tooltip);
         Binding.Command := Command;
         Binding.Context := Context;
         return;
      end if;

      Binding2 := new Key_Description'
        (Name           => new String'(Name),
         Tooltip        => new String'(Tooltip),
         Command        => Command,
         Context        => Context,
         Next           => null);
      Binding := Get (Handler.Table, Key_Binding'(Default_Key, Default_Mod));

      if Binding /= null then
         Binding2.Next := Binding.Next;
         Binding.Next  := Binding2;
      else
         Set (Handler.Table, Key_Binding'(Default_Key, Default_Mod), Binding2);
      end if;
   end Register_Key_Internal;

   ------------------
   -- Register_Key --
   ------------------

   procedure Register_Key
     (Handler        : access Key_Manager_Record;
      Name           : String;
      Default_Key    : Gdk.Types.Gdk_Key_Type;
      Default_Mod    : Gdk.Types.Gdk_Modifier_Type;
      Command        : access Commands.Root_Command'Class;
      Tooltip        : String := "";
      Context        : Key_Context := null) is
   begin
      Register_Key_Internal
        (Handler, Name, Default_Key, Default_Mod,
         Command_Access (Command), Tooltip, Context);
   end Register_Key;

   -------------------
   -- Process_Event --
   -------------------

   function Process_Event
     (Handler  : access Key_Manager_Record;
      Event    : Event_Data) return Boolean
   is
      Key   : constant Gdk_Key_Type      := Get_Key_Val (Get_Event (Event));
      Modif : constant Gdk_Modifier_Type := Get_State (Get_Event (Event));
      Binding : Key_Description_List;
   begin
      if Handler.Active
        and then Get_Event_Type (Get_Event (Event)) = Key_Press
      then
         Binding := Get (Handler.Table, (Key, Modif));

         while Binding /= No_Key loop
            if Binding.Command /= null
              and then (Binding.Context = null
                        or else Context_Matches (Binding.Context, Event))
              and then Execute (Binding.Command) = Success
            then
               return True;
            end if;

            Binding := Binding.Next;
         end loop;
      end if;

      return False;
   end Process_Event;

   ----------
   -- Free --
   ----------

   procedure Free (Handler : in out Key_Manager_Record) is
      Filename : constant String := Get_Home_Dir (Handler.Kernel) & "keys.xml";
      File, Child : Node_Ptr;
      Iter : Key_Htable.Iterator;
      Binding : Key_Description_List;
   begin
      File     := new Node;
      File.Tag := new String'("Keys");

      Get_First (Handler.Table, Iter);
      loop
         Binding := Get_Element (Iter);
         exit when Binding = No_Key;

         while Binding /= null loop
            Child := new Node;
            Child.Tag := new String'("Key");
            Set_Attribute (Child, "name", Binding.Name.all);
            Child.Value := new String'
              (Image (Get_Key (Iter).Key, Get_Key (Iter).Modifier));

            Add_Child (File, Child);

            Binding := Binding.Next;
         end loop;

         Get_Next (Handler.Table, Iter);
      end loop;

      Trace (Me, "Saving " & Filename);
      Print (File, Filename);
      Free (File);

      Reset (Handler.Table);
   end Free;

   ----------------------
   -- Load_Custom_Keys --
   ----------------------

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class;
      Manager : access Key_Manager_Record'Class)
   is
      Filename : constant String := Get_Home_Dir (Kernel) & "keys.xml";
      File, Child : Node_Ptr;
      Key : Gdk_Key_Type;
      Modif : Gdk_Modifier_Type;
   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);
         File := Parse (Filename);
         Child := File.Child;

         while Child /= null loop
            Value (Child.Value.all, Key, Modif);
            Register_Key_Internal
              (Manager,
               Name        => Get_Attribute (Child, "name"),
               Default_Key => Key,
               Default_Mod => Modif,
               Command     => null);
            Child := Child.Next;
         end loop;

         Free (File);
      end if;
   end Load_Custom_Keys;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class) is

      Menu_Iter : Gtk_Tree_Iter;

      procedure Process_Menu_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean);
      --  Called for each key binding associated with menus

      function Set
        (Parent  : Gtk_Tree_Iter;
         Descr   : String;
         Key     : Gdk_Key_Type := 0;
         Modif   : Gdk_Modifier_Type := 0;
         Context : Key_Context := null;
         Tooltip : String := "";
         Command : Command_Access := null) return Gtk_Tree_Iter;
      --  Add a new line into the model

      function Find_Parent (Context : Key_Context) return Gtk_Tree_Iter;
      --  Find the parent node for Context.

      ---------
      -- Set --
      ---------

      function Set
        (Parent  : Gtk_Tree_Iter;
         Descr   : String;
         Key     : Gdk_Key_Type := 0;
         Modif   : Gdk_Modifier_Type := 0;
         Context : Key_Context := null;
         Tooltip : String := "";
         Command : Command_Access := null) return Gtk_Tree_Iter
      is
         procedure Internal
           (Tree, Iter : System.Address;
            Col1  : Gint; Value1 : String;
            Col2  : Gint; Value2 : Gint;
            Col3  : Gint; Value3 : Gint;
            Col4  : Gint; Value4 : String;
            Col5  : Gint; Value5 : System.Address;
            Col6  : Gint; Value6 : String;
            Col7  : Gint; Value7 : System.Address;
            Final : Gint := -1);
         pragma Import (C, Internal, "gtk_tree_store_set");

         Iter : Gtk_Tree_Iter;
      begin
         Append (Editor.Model, Iter, Parent);
         Internal
           (Get_Object (Editor.Model), Iter'Address,
            Col1 => Name_Column,    Value1 => Descr & ASCII.NUL,
            Col2 => Key_Column,     Value2 => Gint (Key),
            Col3 => Modif_Column,   Value3 => Gint (Modif),
            Col4 => Image_Column,   Value4 => Image (Key, Modif) & ASCII.NUL,
            Col5 => Context_Column, Value5 => Convert (Context),
            Col6 => Tooltip_Column, Value6 => Tooltip & ASCII.NUL,
            Col7 => Command_Column, Value7 => Convert (Command));
         return Iter;
      end Set;

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
         Iter : Gtk_Tree_Iter;
         pragma Unreferenced (Data, Changed, Iter);
         First : Natural := Accel_Path'First;
      begin
         while First <= Accel_Path'Last
           and then Accel_Path (First) /= '/'
         loop
            First := First + 1;
         end loop;

         if Accel_Key /= 0 then
            Iter := Set (Parent => Menu_Iter,
                         Descr  => Accel_Path (First .. Accel_Path'Last),
                         Key    => Accel_Key,
                         Modif  => Accel_Mods);
         end if;
      end Process_Menu_Binding;

      -----------------
      -- Find_Parent --
      -----------------

      function Find_Parent (Context : Key_Context) return Gtk_Tree_Iter is
         Parent : Gtk_Tree_Iter := Get_Iter_First (Editor.Model);
      begin
         if Context = null then
            while Parent /= Null_Iter loop
               if Get_String (Editor.Model, Parent, Name_Column) =
                 -"General"
               then
                  return Parent;
               end if;
               Next (Editor.Model, Parent);
            end loop;

            return Set (Parent => Null_Iter, Descr => -"General");

         else
            declare
               C : constant String := Get_Description (Context);
            begin
               while Parent /= Null_Iter loop
                  if Get_String (Editor.Model, Parent, Name_Column) = C then
                     return Parent;
                  end if;
                  Next (Editor.Model, Parent);
               end loop;

               return Set (Parent => Null_Iter, Descr => C);
            end;
         end if;
      end Find_Parent;

      Table_Iter   : Key_Htable.Iterator;
      Handler      : constant Key_Manager_Access := Key_Manager_Access
        (Get_Key_Handler (Editor.Kernel));
      Binding      : Key_Description_List;
      Parent       : Gtk_Tree_Iter;
      Sort_Id      : constant Gint := Freeze_Sort (Editor.Model);
   begin
      Clear (Editor.Model);

      Menu_Iter := Set (Null_Iter, -Menu_Context_Name);

      Gtk.Accel_Map.Foreach
        (System.Null_Address, Process_Menu_Binding'Unrestricted_Access);

      Get_First (Handler.Table, Table_Iter);
      loop
         Binding := Get_Element (Table_Iter);
         exit when Binding = No_Key;

         while Binding /= null loop
            Parent := Find_Parent (Binding.Context);
            Parent := Set (Parent  => Parent,
                           Descr   => Binding.Name.all,
                           Key     => Get_Key (Table_Iter).Key,
                           Modif   => Get_Key (Table_Iter).Modifier,
                           Context => Binding.Context,
                           Tooltip => Binding.Tooltip.all,
                           Command => Binding.Command);
            Binding := Binding.Next;
         end loop;

         Get_Next (Handler.Table, Table_Iter);
      end loop;

      Thaw_Sort (Editor.Model, Sort_Id);
   end Fill_Editor;

   -----------------
   -- Save_Editor --
   -----------------

   procedure Save_Editor (Editor : access Keys_Editor_Record'Class) is
      Handler      : constant Key_Manager_Access := Key_Manager_Access
        (Get_Key_Handler (Editor.Kernel));
      Context_Iter : Gtk_Tree_Iter := Get_Iter_First (Editor.Model);
      Child        : Gtk_Tree_Iter;
   begin
      Reset (Handler.Table);

      while Context_Iter /= Null_Iter loop
         --  Special handling for menus

         if Get_String (Editor.Model, Context_Iter, Name_Column) =
           -Menu_Context_Name
         then
            null;

         --  Standard key bindings
         else
            Child := Children (Editor.Model, Context_Iter);
            while Child /= Null_Iter loop
               Register_Key_Internal
                 (Handler,
                  Name         =>
                    Get_String (Editor.Model, Child, Name_Column),
                  Default_Key  =>
                    Gdk_Key_Type (Get_Int (Editor.Model, Child, Key_Column)),
                  Default_Mod  => Gdk_Modifier_Type
                    (Get_Int (Editor.Model, Child, Modif_Column)),
                  Command   => Convert
                    (Get_Address (Editor.Model, Child, Command_Column)),
                  Tooltip   =>
                    Get_String (Editor.Model, Child, Tooltip_Column),
                  Context   => Convert
                    (Get_Address (Editor.Model, Child, Context_Column)));
               Next (Editor.Model, Child);
            end loop;
         end if;

         Next (Editor.Model, Context_Iter);
      end loop;
   end Save_Editor;

   -----------------
   -- On_Grab_Key --
   -----------------

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Keys_Editor := Keys_Editor (Editor);
      Handler   : constant Key_Manager_Access :=
        Key_Manager_Access (Get_Key_Handler (Ed.Kernel));
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Key       : Gdk_Key_Type;
      Modif     : Gdk_Modifier_Type;
   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)
      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Handler.Active := False;
         Key_Grab (Ed.View, Key, Modif);

         if Key /= GDK_Escape or else Modif /= 0 then
            Set (Ed.Model, Iter, Key_Column, Gint (Key));
            Set (Ed.Model, Iter, Modif_Column, Gint (Modif));
            Set (Ed.Model, Iter, Image_Column, Image (Key, Modif));
         end if;

         Handler.Active := True;
      end if;
   end On_Grab_Key;

   ------------------
   -- On_Edit_Keys --
   ------------------

   procedure On_Edit_Keys
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Editor : Keys_Editor;
      Scrolled : Gtk_Scrolled_Window;
      Bbox : Gtk_Vbutton_Box;
      Box    : Gtk_Box;
      Button : Gtk_Button;
      Col    : Gtk_Tree_View_Column;
      Render : Gtk_Cell_Renderer_Text;
      Num    : Gint;
      Action : Gtk_Widget;
      pragma Unreferenced (Widget, Num, Action);
   begin
      Editor := new Keys_Editor_Record;
      Initialize (Editor,
                  Title  => -"Key shortcuts",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Destroy_With_Parent or Modal);
      Set_Default_Size (Editor, 400, 400);
      Editor.Kernel  := Kernel;

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Editor), Box);

      Gtk_New (Scrolled);
      Pack_Start (Box, Scrolled, Expand => True, Fill => True);

      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Start);
      Pack_Start (Box, Bbox, Expand => False);

      Gtk_New_From_Stock (Button, Stock_Add);
      Pack_Start (Bbox, Button);
      Set_Sensitive (Button, False);

      Gtk_New_From_Stock (Button, Stock_Remove);
      Pack_Start (Bbox, Button);
      Set_Sensitive (Button, False);

      Gtk_New (Button, -"Grab");
      Pack_Start (Bbox, Button);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_Grab_Key'Access),
         Editor);

      Gtk_New (Editor.Model,
               (Name_Column    => GType_String,
                Key_Column     => GType_Int,
                Modif_Column   => GType_Int,
                Image_Column   => GType_String,
                Context_Column => GType_Pointer,
                Tooltip_Column => GType_String,
                Command_Column => GType_Pointer));
      Gtk_New (Editor.View, Editor.Model);
      Add (Scrolled, Editor.View);

      Gtk_New (Render);

      Gtk_New (Col);
      Num := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Action");
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", Name_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Name_Column);

      Clicked (Col);

      Gtk_New (Col);
      Num := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Shortcut");
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "text", Image_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Image_Column);

      Fill_Editor (Editor);

      Action := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Action := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Editor);

      if Run (Editor) = Gtk_Response_OK then
         Save_Editor (Editor);
      end if;

      Destroy (Editor);
   end On_Edit_Keys;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Manager : constant Key_Manager_Access := new Key_Manager_Record;
      Edit    : constant String := "/" & (-"Edit");
   begin
      Manager.Kernel := Kernel_Handle (Kernel);
      Load_Custom_Keys (Kernel, Manager);
      Set_Key_Handler (Kernel, Manager);

      Register_Menu
        (Kernel, Edit, -"_Key shortcuts",
         Ref_Item   => -"Preferences",
         Add_Before => False,
         Callback   => On_Edit_Keys'Access);
   end Register_Module;

end KeyManager_Module;
