-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2004                       --
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

with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Custom;      use Glide_Kernel.Custom;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Default_Preferences;      use Default_Preferences;
with GUI_Utils;                use GUI_Utils;
with Glib.Xml_Int;             use Glib.Xml_Int;
with Glide_Intl;               use Glide_Intl;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.Values;              use Glib.Values;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Label;                use Gtk.Label;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Separator;            use Gtk.Separator;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Traces;                   use Traces;
with System.Assertions;        use System.Assertions;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with System;                   use System;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with VFS;

package body Theme_Manager_Module is

   Me : constant Debug_Handle := Create ("Themes");

   Active_Themes : Param_Spec_String;

   type Theme_Description;
   type Theme_Description_Access is access Theme_Description;
   type Theme_Description is record
      Name        : GNAT.OS_Lib.String_Access;
      Description : GNAT.OS_Lib.String_Access;
      Category    : GNAT.OS_Lib.String_Access;
      Active      : Boolean;
      Xml         : Node_Ptr;
      --  The concatenation of all XML nodes that define the theme. They are
      --  stored in an order so that system-wide definitions are parsed first,
      --  then project-wide and user-specific setups.

      Next        : Theme_Description_Access;
   end record;

   type Theme_Editor_Widget_Record is new Gtk_Paned_Record with record
      Kernel            : Kernel_Handle;
      Model             : Gtk_Tree_Store;
      View              : Gtk_Tree_View;
      Current_Theme     : Gtk_Label;
      Description       : Gtk_Text_Buffer;
   end record;
   type Theme_Editor_Widget is access all Theme_Editor_Widget_Record'Class;

   type Theme_Editor_Record is new Preferences_Page_Record with record
      Kernel            : Kernel_Handle;
   end record;
   type Theme_Editor is access all Theme_Editor_Record'Class;

   function Name (Pref : access Theme_Editor_Record) return String;
   function Create
     (Pref : access Theme_Editor_Record) return Gtk.Widget.Gtk_Widget;
   procedure Validate
     (Pref   : access Theme_Editor_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  See doc for inherited subprograms.


   type Theme_Manager_Module_Record is new Module_ID_Record with record
      Themes : Theme_Description_Access;
      --  List of all registered themes.
   end record;
   type Theme_Manager_Module_ID
     is access all Theme_Manager_Module_Record'Class;

   --  ??? Global variable, could be queried from the kernel
   Theme_Manager_Module : Theme_Manager_Module_ID;

   Name_Column      : constant := 0;
   Active_Column    : constant := 1;
   Visible_Column   : constant := 2;

   procedure Destroy (Module : in out Theme_Manager_Module_Record);
   --  Free the memory occupied by Module

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Handles tags read from the customization files

   procedure Selection_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the selection has changed

   function Lookup_Theme
     (Kernel : access Kernel_Handle_Record'Class; Name : String)
      return Theme_Description_Access;
   --  Return the description of a specific theme

   function Find_Category
     (Model : Gtk_Tree_Store; Category : String) return Gtk_Tree_Iter;
   --  Return the node corresponding to the category from the Model

   function Add
     (Model : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Name   : String;
      Active, Visible : Boolean) return Gtk_Tree_Iter;
   --  Add a new entry in the tree.

   procedure Selection_Toggled
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when a theme is selected or unselected

   procedure Load_XML_Theme
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      XML    : Node_Ptr);
   --  Load an XML node as a customization file

   --------------------
   -- Load_XML_Theme --
   --------------------

   procedure Load_XML_Theme
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      XML    : Node_Ptr) is
   begin
      Execute_Customization_String
        (Kernel, File, XML, Level => Themes);
   end Load_XML_Theme;

   -------------------
   -- Find_Category --
   -------------------

   function Find_Category
     (Model : Gtk_Tree_Store; Category : String) return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter := Get_Iter_First (Model);
   begin
      while Iter /= Null_Iter loop
         if Get_String (Model, Iter, Name_Column) = Category then
            return Iter;
         end if;

         Next (Model, Iter);
      end loop;

      return Add
        (Model, Null_Iter, Category, Active => True, Visible => False);
   end Find_Category;

   ------------------
   -- Lookup_Theme --
   ------------------

   function Lookup_Theme
     (Kernel : access Kernel_Handle_Record'Class; Name : String)
      return Theme_Description_Access
   is
      pragma Unreferenced (Kernel);
      T : Theme_Description_Access := Theme_Manager_Module.Themes;
   begin
      while T /= null
        and then T.Name.all /= Name
      loop
         T := T.Next;
      end loop;

      return T;
   end Lookup_Theme;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Theme_Manager_Module_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Theme_Description, Theme_Description_Access);
      Themes : Theme_Description_Access := Module.Themes;
      Tmp    : Theme_Description_Access;
   begin
      while Themes /= null loop
         Tmp := Themes.Next;
         Free (Themes.Name);
         Free (Themes.Description);
         Free (Themes.Category);
         Free (Themes.Xml);
         Unchecked_Free (Themes);

         Themes := Tmp;
      end loop;
   end Destroy;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level);
      N : Node_Ptr := Node;
      Themes : Theme_Description_Access;
      Str : GNAT.OS_Lib.String_Access;
      Active : constant String := Get_Pref (Kernel, Active_Themes);
   begin
      while N /= null loop
         if N.Tag.all = "theme" then
            declare
               Name  : constant String := Get_Attribute (N, "name");
               Descr : constant String := Get_Attribute (N, "description");
               Cat   : constant String := Get_Attribute
                 (N, "category", "General");
            begin
               if Name = "" then
                  Insert (Kernel, -"<theme> nodes must have a name attribute");
                  raise Assert_Failure;
               end if;

               Themes := Theme_Manager_Module.Themes;
               while Themes /= null
                 and then Themes.Name.all /= Name
               loop
                  Themes := Themes.Next;
               end loop;

               if Themes = null then
                  Themes := new Theme_Description'
                    (Name        => new String'(Name),
                     Description => new String'(Descr),
                     Category    => new String'(Cat),
                     Xml         => new Glib.Xml_Int.Node,
                     Active      => Index (Active, '@' & Name & '@') /= 0,
                     Next        => Theme_Manager_Module.Themes);
                  Theme_Manager_Module.Themes := Themes;
                  Trace (Me, "Registering new theme: " & Name
                         & " Active=" & Themes.Active'Img);

               elsif Descr /= "" then
                  Str := new String'
                    (Themes.Description.all & ASCII.LF & Descr);
                  Free (Themes.Description);
                  Themes.Description := Str;
               end if;

               Add_Child (Themes.Xml, Deep_Copy (N.Child), Append => True);

               if Themes.Active then
                  Load_XML_Theme (Kernel, File, N.Child);
               end if;
            end;

         elsif N.Tag.all = "pref" then
            declare
               Name : constant String := Get_Attribute (N, "name");
            begin
               if Name /= "" then
                  Set_Pref (Kernel, Name, N.Value.all);
               end if;
            end;
         end if;

         N := N.Next;
      end loop;
   end Customize;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Theme_Editor_Widget :=
        Theme_Editor_Widget (Editor);
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Theme     : Theme_Description_Access;
   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit leaf nodes
      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Theme := Lookup_Theme
           (Ed.Kernel, Get_String (Model, Iter, Name_Column));

         if Theme.Description.all /= "" then
            Set_Text (Ed.Description, Theme.Description.all);
         else
            Set_Text (Ed.Description, -"No description available");
         end if;

         Set_Text (Ed.Current_Theme, Get_String (Model, Iter, Name_Column));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Selection_Changed;

   ---------
   -- Add --
   ---------

   function Add
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Name   : String;
      Active, Visible : Boolean) return Gtk_Tree_Iter
   is
      procedure Set
        (Tree, Iter : System.Address; Col1 : Gint; Value1 : String;
         Col2 : Gint; Value2 : Gint;
         Col3 : Gint; Value3 : Gint;
         Final : Gint := -1);
      pragma Import (C, Set, "gtk_tree_store_set");
      Iter : Gtk_Tree_Iter;
   begin
      Append (Model, Iter, Parent);
      Set (Get_Object (Model), Iter'Address,
           Col1 => Name_Column,      Value1 => Name & ASCII.NUL,
           Col2 => Active_Column,    Value2 => Boolean'Pos (Active),
           Col3 => Visible_Column,   Value3 => Boolean'Pos (Visible));
      return Iter;
   end Add;

   -----------------------
   -- Selection_Toggled --
   -----------------------

   procedure Selection_Toggled
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Ed        : constant Theme_Editor_Widget := Theme_Editor_Widget (Editor);
      Iter      : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));
      Setting   : Boolean;
      Themes    : Theme_Description_Access := Theme_Manager_Module.Themes;
   begin
      Iter := Get_Iter_From_String (Ed.Model, Path_String);
      if Iter /= Null_Iter then
         declare
            Name : constant String := Get_String
              (Ed.Model, Iter, Name_Column);
         begin
            Setting := not Get_Boolean (Ed.Model, Iter, Active_Column);
            Set (Ed.Model, Iter, Active_Column, Setting);

            if Setting then
               while Themes /= null loop
                  if Themes.Name.all = Name then
                     Load_XML_Theme
                       (Ed.Kernel, VFS.No_File, Themes.Xml.Child);
                     exit;
                  end if;

                  Themes := Themes.Next;
               end loop;
            end if;
         end;
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Selection_Toggled;

   ----------
   -- Name --
   ----------

   function Name (Pref : access Theme_Editor_Record) return String is
      pragma Unreferenced (Pref);
   begin
      return -"Themes";
   end Name;

   --------------
   -- Validate --
   --------------

   procedure Validate
     (Pref   : access Theme_Editor_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Pref);
      W    : constant Theme_Editor_Widget := Theme_Editor_Widget (Widget);
      List : Unbounded_String := To_Unbounded_String ("@");
      Category, Iter : Gtk_Tree_Iter;
   begin
      Category := Get_Iter_First (W.Model);
      while Category /= Null_Iter loop
         Iter := Children (W.Model, Category);
         while Iter /= Null_Iter loop
            if Get_Boolean (W.Model, Iter, Active_Column) then
               List :=
                 List & Get_String (W.Model, Iter, Name_Column) & '@';
            end if;

            Next (W.Model, Iter);
         end loop;

         Next (W.Model, Category);
      end loop;

      Set_Pref (W.Kernel, Pspec_Name (Param_Spec (Active_Themes)),
                To_String (List));
   end Validate;

   ------------
   -- Create --
   ------------

   function Create
     (Pref : access Theme_Editor_Record) return Gtk.Widget.Gtk_Widget
   is
      Action   : Gtk_Widget;
      Scrolled : Gtk_Scrolled_Window;
      Render   : Gtk_Cell_Renderer_Text;
      Toggle_Renderer : Gtk_Cell_Renderer_Toggle;
      Num      : Gint;
      Iter     : Gtk_Tree_Iter;
      Col      : Gtk_Tree_View_Column;
      Pane     : Theme_Editor_Widget;
      Frame    : Gtk_Frame;
      Box      : Gtk_Box;
      Event    : Gtk_Event_Box;
      Sep      : Gtk_Separator;
      Text     : Gtk_Text_View;
      Themes   : Theme_Description_Access := Theme_Manager_Module.Themes;
      pragma Unreferenced (Action, Num, Iter);

   begin
      if Themes = null then
         return null;
      end if;

      Pane := new Theme_Editor_Widget_Record;
      Pane.Kernel := Pref.Kernel;
      Initialize_Vpaned (Pane);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack1 (Pane, Scrolled, True, True);

      Gtk_New (Pane.Model,
               (Name_Column    => GType_String,
                Active_Column  => GType_Boolean,
                Visible_Column => GType_Boolean));
      Gtk_New (Pane.View, Pane.Model);
      Add (Scrolled, Pane.View);

      Gtk_New (Render);
      Gtk_New (Toggle_Renderer);

      Gtk_New (Col);
      Num := Append_Column (Pane.View, Col);
      Set_Title (Col, -"");
      Set_Resizable (Col, True);

      Pack_Start (Col, Toggle_Renderer, False);
      Add_Attribute (Col, Toggle_Renderer, "active",  Active_Column);
      Add_Attribute (Col, Toggle_Renderer, "visible", Visible_Column);

      Gtk_New (Col);
      Num := Append_Column (Pane.View, Col);
      Set_Title (Col, -"Theme");
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Name_Column);

      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", Name_Column);

      Widget_Callback.Object_Connect
        (Get_Selection (Pane.View), "changed",
         Widget_Callback.To_Marshaller (Selection_Changed'Access), Pane);
      Widget_Callback.Object_Connect
        (Toggle_Renderer, "toggled",
         Selection_Toggled'Access, Pane);

      while Themes /= null loop
         Iter := Add (Pane.Model,
                      Find_Category (Pane.Model, Themes.Category.all),
                      name     => Themes.Name.all,
                      Active   => Themes.Active,
                      Visible  => True);
         Themes := Themes.Next;
      end loop;

      Clicked (Col);

      --  Bottom area

      Gtk_New  (Frame);
      Pack2 (Pane, Frame, False, False);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Create_Blue_Label (Pane.Current_Theme, Event);
      Pack_Start (Box, Event, Expand => False);

      Gtk_New_Hseparator (Sep);
      Pack_Start (Box, Sep, Expand => False);

      Gtk_New (Scrolled);
      Pack_Start (Box, Scrolled, Expand => True, Fill => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Gtk_New (Pane.Description);
      Gtk_New (Text, Pane.Description);
      Set_Wrap_Mode (Text, Wrap_Word);
      Set_Editable (Text, False);
      Add (Scrolled, Text);

      return Gtk_Widget (Pane);
   end Create;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Theme     : constant Theme_Editor := new Theme_Editor_Record;
   begin
      Theme_Manager_Module := new Theme_Manager_Module_Record;
      Register_Module
        (Module_ID (Theme_Manager_Module),
         Kernel,
         "theme_manager",
         Customization_Handler => Customize'Access);

      Active_Themes := Param_Spec_String (Gnew_String
        (Name    => "Active-Themes",
         Nick    => -"Active themes",
         Blurb   => -"List of currently active themes",
         Default => "@Default@",
         Flags   => Param_Readable));
      Register_Property (Kernel, Param_Spec (Active_Themes), -"General");

      Theme.Kernel := Kernel_Handle (Kernel);
      Glide_Kernel.Preferences.Register_Page (Kernel, Theme);
   end Register_Module;

end Theme_Manager_Module;

