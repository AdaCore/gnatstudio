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
with Glib.Xml_Int; use Glib.Xml_Int;
with Commands;     use Commands;
with HTables;      use HTables;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GUI_Utils;    use GUI_Utils;
with Ada.Unchecked_Deallocation;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
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
with Gtkada.MDI;               use Gtkada.MDI;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Enums;                use Gtk.Enums;
with System;                   use System;
with Gtk.Accel_Map;            use Gtk.Accel_Map;
with Traces;                   use Traces;

package body KeyManager_Module is

   Me : constant Debug_Handle := Create ("Keymanager");

   Keymanager_Module_Id : Module_ID;

   type Keys_Header_Num is range 0 .. 1000;
   type Key_Binding is record
      Key      : Gdk_Key_Type;
      Modifier : Gdk_Modifier_Type;
   end record;

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

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
   (Key_Description, Key_Description_List);

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
   procedure Register_Key_Internal
     (Handler        : access Key_Manager_Record'Class;
      Name           : String;
      Default_Key    : Gdk.Types.Gdk_Key_Type;
      Default_Mod    : Gdk.Types.Gdk_Modifier_Type;
      Command        : Commands.Command_Access := null;
      Tooltip        : String := "";
      Context        : Key_Context := null);
   function Process_Event
     (Handler  : access Key_Manager_Record;
      Event    : Event_Data) return Boolean;
   procedure Free (Handler : in out Key_Manager_Record);
   --  See documentation for imported subprograms

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class;
      Manager : access Key_Manager_Record'Class);
   --  Load the customized key bindings

   procedure On_Edit_Keys
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open a GUI to edit the key bindings

   type Keys_Editor_Record is new Gtk_Box_Record with record
      Kernel  : Kernel_Handle;
      Model   : Gtk_Tree_Store;
   end record;
   type Keys_Editor is access all Keys_Editor_Record'Class;

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class);
   --  Fill the contents of the editor

   Name_Column  : constant := 0;
   Key_Column   : constant := 1;
   Modif_Column : constant := 2;
   Image_Column : constant := 3;

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
         Destroy (Current.Command);
         Unchecked_Free (Current);
         Current := Next;
      end loop;
   end Free;

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
      Iter    : Key_Htable.Iterator;
      Binding, Binding2 : Key_Description_List;
   begin
      --  Chech whether command is already associated with a key binding
      --  We do not use the most efficient method, since we simply
      --  traverse a list, but there aren't hundreds of keybindings...

      Get_First (Handler.Table, Iter);
      loop
         Binding := Get_Element (Iter);
         exit when Binding = No_Key;

         while Binding /= null loop
            if Binding.Name.all = Name then
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
            Binding := Binding.Next;
         end loop;

         Get_Next (Handler.Table, Iter);
      end loop;

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
      if Get_Event_Type (Get_Event (Event)) = Key_Press then
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
        (Parent : Gtk_Tree_Iter;
         Descr  : String;
         Key    : Gdk_Key_Type := 0;
         Modif  : Gdk_Modifier_Type := 0) return Gtk_Tree_Iter;
      --  Add a new line into the model

      function Find_Parent (Context : Key_Context) return Gtk_Tree_Iter;
      --  Find the parent node for Context.

      ---------
      -- Set --
      ---------

      function Set
        (Parent : Gtk_Tree_Iter;
         Descr  : String;
         Key    : Gdk_Key_Type := 0;
         Modif  : Gdk_Modifier_Type := 0) return Gtk_Tree_Iter
      is
         procedure Internal
           (Tree, Iter : System.Address;
            Col1  : Gint; Value1 : String;
            Col2  : Gint; Value2 : Gint;
            Col3  : Gint; Value3 : Gint;
            Col4  : Gint; Value4 : String;
            Final : Gint := -1);
         pragma Import (C, Internal, "gtk_tree_store_set");

         Iter : Gtk_Tree_Iter;
      begin
         Append (Editor.Model, Iter, Parent);
         Internal
           (Get_Object (Editor.Model), Iter'Address,
            Col1 => Name_Column,  Value1 => Descr & ASCII.NUL,
            Col2 => Key_Column,   Value2 => Gint (Key),
            Col3 => Modif_Column, Value3 => Gint (Modif),
            Col4 => Image_Column, Value4 => Image (Key, Modif) & ASCII.NUL);
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

      Menu_Iter := Set (Null_Iter, -"Menus");

      Gtk.Accel_Map.Foreach
        (System.Null_Address, Process_Menu_Binding'Unrestricted_Access);

      Get_First (Handler.Table, Table_Iter);
      loop
         Binding := Get_Element (Table_Iter);
         exit when Binding = No_Key;

         while Binding /= null loop
            Parent := Find_Parent (Binding.Context);
            Parent := Set (Parent => Parent,
                           Descr  => Binding.Name.all,
                           Key    => Get_Key (Table_Iter).Key,
                           Modif  => Get_Key (Table_Iter).Modifier);
            Binding := Binding.Next;
         end loop;

         Get_Next (Handler.Table, Table_Iter);
      end loop;

      Thaw_Sort (Editor.Model, Sort_Id);
   end Fill_Editor;

   ------------------
   -- On_Edit_Keys --
   ------------------

   procedure On_Edit_Keys
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Editor : Keys_Editor;
      Scrolled : Gtk_Scrolled_Window;
      Bbox : Gtk_Vbutton_Box;
      Button : Gtk_Button;
      View   : Gtk_Tree_View;
      Col    : Gtk_Tree_View_Column;
      Render : Gtk_Cell_Renderer_Text;
      Num    : Gint;
      Child  : MDI_Child;
      pragma Unreferenced (Widget, Num);
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Keys_Editor_Record'Tag);

      if Child = null then
         Editor := new Keys_Editor_Record;
         Initialize_Hbox (Editor, Homogeneous => False);
         Editor.Kernel  := Kernel;

         Gtk_New (Scrolled);
         Pack_Start (Editor, Scrolled, Expand => True, Fill => True);

         Gtk_New (Bbox);
         Set_Layout (Bbox, Buttonbox_Start);
         Pack_Start (Editor, Bbox, Expand => False);

         Gtk_New_From_Stock (Button, Stock_Add);
         Pack_Start (Bbox, Button);

         Gtk_New_From_Stock (Button, Stock_Remove);
         Pack_Start (Bbox, Button);

         Gtk_New (Button, -"Grab");
         Pack_Start (Bbox, Button);

         Gtk_New (Editor.Model,
               (Name_Column    => GType_String,
                Key_Column     => GType_Int,
                Modif_Column   => GType_Int,
                Image_Column   => GType_String));
         Gtk_New (View, Editor.Model);
         Add (Scrolled, View);

         Gtk_New (Render);

         Gtk_New (Col);
         Num := Append_Column (View, Col);
         Set_Title (Col, -"Action");
         Pack_Start (Col, Render, True);
         Add_Attribute (Col, Render, "text", Name_Column);
         Set_Clickable (Col, True);
         Set_Resizable (Col, True);
         Set_Sort_Column_Id (Col, Name_Column);

         Clicked (Col);

         Gtk_New (Col);
         Num := Append_Column (View, Col);
         Set_Title (Col, -"Shortcut");
         Pack_Start (Col, Render, False);
         Add_Attribute (Col, Render, "text", Image_Column);
         Set_Clickable (Col, True);
         Set_Resizable (Col, True);
         Set_Sort_Column_Id (Col, Image_Column);

         Child := Put
           (Kernel,
            Child               => Editor,
            Focus_Widget        => Gtk_Widget (View),
            Module              => Keymanager_Module_Id,
            Desktop_Independent => True);
         Set_Title (Child, -"Key shortcuts", -"Keys");
      else
         Editor := Keys_Editor (Get_Widget (Child));
      end if;

      Fill_Editor (Editor);
      Raise_Child (Child);
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
      Register_Module
        (Module                  => Keymanager_Module_Id,
         Kernel                  => Kernel,
         Module_Name             => "KeyManager");

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
