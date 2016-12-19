------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

with Ada.Characters.Handling;            use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Default_Preferences;                use Default_Preferences;
with Gdk.RGBA;                           use Gdk.RGBA;
with Generic_Views;                      use Generic_Views;
with Glib;                               use Glib;
with Glib.Convert;                       use Glib.Convert;
with Glib.Values;                        use Glib.Values;
with Glib_Values_Utils;                  use Glib_Values_Utils;
with GPS.Kernel.Hooks;                   use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                     use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;              use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;             use GPS.Kernel.Preferences;
with Gtkada.MDI;                         use Gtkada.MDI;
with Gtkada.Tree_View;                   use Gtkada.Tree_View;
with Gtk.Box;                            use Gtk.Box;
with Gtk.Cell_Renderer;                  use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Pixbuf;           use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;             use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                          use Gtk.Enums;
with Gtk.Scrolled_Window;                use Gtk.Scrolled_Window;
with Gtk.Tree_Model;                     use Gtk.Tree_Model;
with Gtk.Tree_View_Column;               use Gtk.Tree_View_Column;
with Gtk.Widget;                         use Gtk.Widget;
with VCS2.Engines;                       use VCS2.Engines;
with VCS2.Views;                         use VCS2.Views;

package body VCS2.Branches is

   Column_Name           : constant := 0;
   Column_Foreground     : constant := 1;
   Column_Emblem         : constant := 2;
   Column_Emblem_Visible : constant := 3;
   Column_Icon           : constant := 4;
   Column_Icon_Visible   : constant := 5;
   subtype All_Columns is Gint range Column_Name .. Column_Icon_Visible;

   package Path_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Gtk_Tree_Path,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Branches_Config is record
      Initialized : Boolean := False;
   end record;

   type Branches_Tree_Record is new Tree_View_Record with record
      Config      : Branches_Config;
      Categories  : Path_Maps.Map;
   end record;
   type Branches_Tree is access all Branches_Tree_Record'Class;

   type Branches_View_Record is new Base_VCS_View_Record with record
      Emblem      : Gtk_Cell_Renderer_Text;
   end record;
   overriding procedure Refresh (Self : not null access Branches_View_Record);
   overriding procedure On_Preferenced_Changed
     (Self    : not null access Branches_View_Record;
      Pref    : Preference);
   overriding procedure On_Create
     (Self    : not null access Branches_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class);

   function Initialize
     (Self : access Branches_View_Record'Class) return Gtk_Widget;
   --  Create a new view

   type Branches_Child_Record is new GPS_MDI_Child_Record with null record;

   package Branches_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Branches",
      View_Name          => "Branches",
      Formal_View_Record => Branches_View_Record,
      Formal_MDI_Child   => Branches_Child_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Both,
      Position           => Position_Left,
      Initialize         => Initialize);
   use Branches_Views;
   subtype Branches_View is Branches_Views.View_Access;

   function Create_Category
     (Self  : not null access Branches_View_Record'Class;
      Name  : String) return Gtk_Tree_Iter;
   --  Create a new top-level category node if none exists already for this
   --  category.

   procedure Create_Node
     (Self      : not null access Branches_View_Record'Class;
      Category  : String;
      Icon_Name : String;
      Info      : Branch_Info);
   --  Create nodes in the tree

   function Create_Group
     (Self      : not null access Branches_View_Record'Class;
      Icon_Name : String;
      Name      : String;
      Parent    : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Create a group of nodes

   type Branches_Visitor is new Task_Visitor with record
      Kernel  : Kernel_Handle;
   end record;
   overriding procedure On_Branches
     (Self     : not null access Branches_Visitor;
      Category : String;
      Iconname : String;
      Branches : Branches_Array);
   overriding procedure On_Terminate
     (Self     : not null access Branches_Visitor;
      VCS      : access VCS_Engine'Class);
      --  Gather the results of the branches list for the current VCS

   type On_Active_VCS_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);

   type On_VCS_Refresh is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_VCS_Refresh;
      Kernel : not null access Kernel_Handle_Record'Class);

   ---------------------
   -- Create_Category --
   ---------------------

   function Create_Category
     (Self  : not null access Branches_View_Record'Class;
      Name  : String) return Gtk_Tree_Iter
   is
      Tree : constant Branches_Tree := Branches_Tree (Self.Tree);
      C    : constant Path_Maps.Cursor := Tree.Categories.Find
        (To_Lower (Name));
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path;
      V    : Glib.Values.GValue_Array (All_Columns);
   begin
      if Path_Maps.Has_Element (C) then
         Path   := Path_Maps.Element (C);
         return Tree.Model.Get_Iter (Path);
      end if;

      Self.Tree.Model.Append (Iter, Parent => Null_Iter);
      Init_Set_String (V (Column_Name), Escape_Text (To_Upper (Name)));

      Init (V (Column_Foreground), Gdk.RGBA.Get_Type);
      Gdk.RGBA.Set_Value (V (Column_Foreground), (0.7, 0.7, 0.7, 1.0));

      Init_Set_String (V (Column_Emblem), "");
      Init_Set_Boolean (V (Column_Emblem_Visible), False);

      Init_Set_String (V (Column_Icon), "");
      Init_Set_Boolean (V (Column_Icon_Visible), False);

      Set_All_And_Clear (Self.Tree.Model, Iter, V);

      Tree.Categories.Include (To_Lower (Name), Tree.Model.Get_Path (Iter));
      return Iter;
   end Create_Category;

   ------------------
   -- Create_Group --
   ------------------

   function Create_Group
     (Self      : not null access Branches_View_Record'Class;
      Icon_Name : String;
      Name      : String;
      Parent    : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      Iter    : Gtk_Tree_Iter;
      V       : Glib.Values.GValue_Array (All_Columns);
   begin
      Iter := Self.Tree.Model.Children (Parent);
      while Iter /= Null_Iter loop
         if Self.Tree.Model.Get_String (Iter, Column_Name) = Name then
            return Iter;
         end if;
         Self.Tree.Model.Next (Iter);
      end loop;

      Self.Tree.Model.Append (Iter, Parent => Parent);

      Init_Set_String (V (Column_Name), Escape_Text (Name));

      Init (V (Column_Foreground), Gdk.RGBA.Get_Type);
      Gdk.RGBA.Set_Value (V (Column_Foreground), Black_RGBA);

      Init_Set_String (V (Column_Emblem), "");
      Init_Set_Boolean (V (Column_Emblem_Visible), False);

      Init_Set_String (V (Column_Icon), Icon_Name);
      Init_Set_Boolean (V (Column_Icon_Visible), Icon_Name /= "");

      Set_All_And_Clear (Self.Tree.Model, Iter, V);
      return Iter;
   end Create_Group;

   -----------------
   -- Create_Node --
   -----------------

   procedure Create_Node
     (Self      : not null access Branches_View_Record'Class;
      Category  : String;
      Icon_Name : String;
      Info      : Branch_Info)
   is
      Parent  : Gtk_Tree_Iter := Create_Category (Self, Category);
      Iter    : Gtk_Tree_Iter;
      V       : Glib.Values.GValue_Array (All_Columns);
      First, Last : Natural := Info.Name'First;

   begin
      while Last <= Info.Name'Last loop
         exit when Info.Name (Last) = '(';
         if Info.Name (Last) = '/' then
            Parent := Create_Group
              (Self, Icon_Name, Info.Name (First .. Last - 1), Parent);
            First := Last + 1;
         end if;
         Last := Last + 1;
      end loop;

      Self.Tree.Model.Append (Iter, Parent => Parent);

      Init (V (Column_Foreground), Gdk.RGBA.Get_Type);

      if Info.Is_Current then
         Init_Set_String
           (V (Column_Name),
            "<b>"
            & Escape_Text (Info.Name (First .. Info.Name'Last)) & "</b>");
         Gdk.RGBA.Set_Value (V (Column_Foreground), (0.0, 0.39, 0.88, 1.0));
      else
         Init_Set_String
           (V (Column_Name),
            Escape_Text (Info.Name (First .. Info.Name'Last)));
         Gdk.RGBA.Set_Value (V (Column_Foreground), Black_RGBA);
      end if;

      Init_Set_String (V (Column_Emblem), Info.Emblem.all);
      Init_Set_Boolean (V (Column_Emblem_Visible), Info.Emblem.all /= "");

      Init_Set_String (V (Column_Icon), Icon_Name);
      Init_Set_Boolean (V (Column_Icon_Visible), Icon_Name /= "");

      Set_All_And_Clear (Self.Tree.Model, Iter, V);
   end Create_Node;

   -----------------
   -- On_Branches --
   -----------------

   overriding procedure On_Branches
     (Self     : not null access Branches_Visitor;
      Category : String;
      Iconname : String;
      Branches : Branches_Array)
   is
      View : constant Branches_View :=
        Branches_Views.Retrieve_View (Self.Kernel);
   begin
      if View /= null then
         for B of Branches loop
            Create_Node (View, Category, Iconname, B);
         end loop;
      end if;
   end On_Branches;

   ------------------
   -- On_Terminate --
   ------------------

   overriding procedure On_Terminate
     (Self     : not null access Branches_Visitor;
      VCS      : access VCS_Engine'Class)
   is
      pragma Unreferenced (VCS);
      View : constant Branches_View :=
        Branches_Views.Retrieve_View (Self.Kernel);
      Iter  : Gtk_Tree_Iter;
      Dummy : Boolean;
   begin
      if View /= null then
         --  Expand all toplevel nodes, as well as all nodes beneath BRANCHES.
         --  The others are not used that often, so we keep them potentially
         --  hidden.

         Iter := View.Tree.Model.Get_Iter_First;
         while Iter /= Null_Iter loop
            Dummy := View.Tree.Expand_Row
              (View.Tree.Get_Filter_Path_For_Store_Iter (Iter),
               Open_All => View.Tree.Model.Get_String (Iter, Column_Name) =
                   "BRANCHES");
            View.Tree.Model.Next (Iter);
         end loop;
      end if;
   end On_Terminate;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_VCS_Refresh;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      View : constant Branches_View := Branches_Views.Retrieve_View (Kernel);
   begin
      if View /= null then
         Refresh (View);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      V    : constant Branches_View := Branches_Views.Retrieve_View (Kernel);
   begin
      if V /= null then
         Refresh (V);
      end if;
   end Execute;

   -------------
   -- Refresh --
   -------------

   overriding procedure Refresh
     (Self : not null access Branches_View_Record)
   is
      Tree : constant Branches_Tree := Branches_Tree (Self.Tree);
      VCS : constant VCS_Engine_Access := Active_VCS (Self.Kernel);
   begin
      for C of Tree.Categories loop
         Path_Free (C);
      end loop;
      Tree.Categories.Clear;

      Self.Tree.Model.Clear;

      if VCS /= null then
         VCS.Queue_Branches
           (new Branches_Visitor'(Task_Visitor with Kernel => Self.Kernel));
      end if;
   end Refresh;

   ----------------------------
   -- On_Preferenced_Changed --
   ----------------------------

   overriding procedure On_Preferenced_Changed
     (Self    : not null access Branches_View_Record;
      Pref    : Preference)
   is
      Tree   : constant Branches_Tree := Branches_Tree (Self.Tree);
      Config : Branches_Config;
   begin
      Base_VCS_View_Record (Self.all).On_Preferenced_Changed (Pref);
      Set_Font_And_Colors (Self.Tree, Fixed_Font => False, Pref => Pref);

      Config :=
        (Initialized      => True);
      if Config /= Tree.Config then
         Tree.Config := Config;
         Self.Refresh;
      end if;
   end On_Preferenced_Changed;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Branches_View_Record'Class) return Gtk_Widget
   is
      Scrolled : Gtk_Scrolled_Window;
      Col      : Gtk_Tree_View_Column;
      Dummy    : Gint;
      Pixbuf   : Gtk_Cell_Renderer_Pixbuf;
   begin
      Initialize_Vbox (Self, Homogeneous => False);

      Gtk_New (Scrolled);
      Self.Pack_Start (Scrolled, Expand => True, Fill => True);

      Self.Tree := new Branches_Tree_Record;
      Initialize
        (Self.Tree,
         (Column_Name           => GType_String,
          Column_Foreground     => Gdk.RGBA.Get_Type,
          Column_Emblem         => GType_String,
          Column_Emblem_Visible => GType_Boolean,
          Column_Icon           => GType_String,
          Column_Icon_Visible   => GType_Boolean),
         Filtered   => False);
      Self.Tree.Set_Headers_Visible (False);
      Self.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Scrolled.Add (Self.Tree);

      Gtk_New (Self.Text_Render);
      Gtk_New (Self.Emblem);
      Gtk_New (Pixbuf);

      Gtk_New (Col);
      Col.Set_Expand (True);
      Dummy := Self.Tree.Append_Column (Col);

      Col.Pack_Start (Pixbuf, False);
      Col.Add_Attribute (Pixbuf, "icon-name", Column_Icon);
      Col.Add_Attribute (Pixbuf, "visible", Column_Icon_Visible);

      Col.Pack_Start (Self.Text_Render, True);
      Col.Add_Attribute (Self.Text_Render, "markup", Column_Name);
      Col.Add_Attribute
        (Self.Text_Render, "foreground-rgba", Column_Foreground);

      Gtk_New (Col);
      Col.Set_Expand (False);
      Dummy := Self.Tree.Append_Column (Col);

      Col.Pack_Start (Self.Emblem, False);
      Col.Add_Attribute (Self.Emblem, "text", Column_Emblem);
      Col.Add_Attribute (Self.Emblem, "visible", Column_Emblem_Visible);
      Self.Emblem.Set_Alignment (1.0, 0.5);
      Set_Property
        (Self.Emblem, Foreground_Rgba_Property, (0.39, 0.64, 0.88, 1.0));

      Setup_Contextual_Menu (Self.Kernel, Self.Tree);

      return Gtk_Widget (Self.Tree);
   end Initialize;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create
     (Self    : not null access Branches_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class)
   is
   begin
      Base_VCS_View_Record (Self.all).On_Create (Child);  --  inherited
      Vcs_Active_Changed_Hook.Add (new On_Active_VCS_Changed, Watch => Self);
      Vcs_Refresh_Hook.Add (new On_VCS_Refresh, Watch => Self);
   end On_Create;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
   begin
      Branches_Views.Register_Module (Kernel);
   end Register_Module;

end VCS2.Branches;
