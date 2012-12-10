------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with Glib.Object;             use Glib, Glib.Object;
with XML_Utils;               use XML_Utils;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Style_Context;       use Gtk.Style_Context;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Tool_Button;         use Gtk.Tool_Button;
with Gtk.Toolbar;             use Gtk.Toolbar;
with Gtk.Widget;              use Gtk.Widget;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.MDI;              use Gtkada.MDI;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;   use GPS.Kernel.Modules.UI;
with GPS.Intl;                use GPS.Intl;
with GPS.Stock_Icons;         use GPS.Stock_Icons;

package body Generic_Views is

   ------------------
   -- Simple_Views --
   ------------------

   package body Simple_Views is

      Module : Module_ID;

      type Toplevel_Box is new Gtk_Box_Record with record
         Initial : View_Access;
      end record;
      --  When using a local toolbar, the contents of the widget as set by the
      --  application is nested inside a box. We use a dedicated tagged type so
      --  that we can more easily find the child in the desktop by tag.

      procedure Create_If_Needed
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access);
      --  Create or reuse a view.

      procedure Find
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access);
      --  Find any existing view

      -----------------------------
      -- On_Display_Local_Config --
      -----------------------------

      procedure On_Display_Local_Config
        (View : access Gtk_Widget_Record'Class)
      is
         V : constant View_Access := View_Access (View);
         Menu : Gtk_Menu;
      begin
         Gtk_New (Menu);
         V.Create_Menu (Menu);
         Menu.Show_All;

         Menu.Popup; --   (Func => Position_Local_Config'Access);
      end On_Display_Local_Config;

      ---------------------
      -- Child_From_View --
      ---------------------

      function Child_From_View
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         View   : not null access Formal_View_Record'Class)
         return MDI_Child
      is
      begin
         if Local_Config or else Local_Toolbar then
            return Find_MDI_Child
              (Get_MDI (Kernel),
               View.Get_Parent);  --  the box
         else
            return Find_MDI_Child (Get_MDI (Kernel), View);
         end if;
      end Child_From_View;

      ----------------------
      -- View_From_Widget --
      ----------------------

      function View_From_Widget
        (Widget : not null access Glib.Object.GObject_Record'Class)
         return View_Access
      is
      begin
         if Local_Toolbar or else Local_Config then
            return Toplevel_Box (Widget.all).Initial;
         else
            return View_Access (Widget);
         end if;
      end View_From_Widget;

      ----------
      -- Find --
      ----------

      procedure Find
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access) is
      begin
         if Local_Toolbar or else Local_Config then
            Child := GPS_MDI_Child
              (Find_MDI_Child_By_Tag
                 (Get_MDI (Kernel), Toplevel_Box'Tag));
         else
            Child := GPS_MDI_Child
              (Find_MDI_Child_By_Tag
                 (Get_MDI (Kernel), Formal_View_Record'Tag));
         end if;

         if Child /= null then
            View := View_From_Widget (Child.Get_Widget);
         else
            View := null;
         end if;
      end Find;

      ----------------------
      -- Create_If_Needed --
      ----------------------

      procedure Create_If_Needed
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access)
      is
         Focus_Widget : Gtk_Widget;
         Toolbar      : Gtk_Toolbar;
         Box          : Gtk_Box;
         W            : Gtk_Widget;
         Button       : Gtk_Tool_Button;
         Sep          : Gtk_Separator_Tool_Item;

      begin
         if Reuse_If_Exist then
            Find (Kernel, Child, View);
            if View /= null then
               return;
            end if;
         end if;

         View := new Formal_View_Record;
         Focus_Widget := Initialize (View, Kernel);
         if Focus_Widget = null then
            Focus_Widget := Gtk_Widget (View);
         end if;

         if Local_Toolbar or else Local_Config then
            Box := new Toplevel_Box;
            Initialize_Vbox (Box);
            Toplevel_Box (Box.all).Initial := View;

            Gtk_New (Toolbar);
            Toolbar.Set_Icon_Size (Icon_Size_Local_Toolbar);
            Toolbar.Set_Style (Toolbar_Icons);
            Get_Style_Context (Toolbar).Add_Class ("gps-local-toolbar");
            Box.Pack_Start (Toolbar, Expand => False, Fill => False);

            Box.Pack_Start (View, Expand => True, Fill => True);
            W := Gtk_Widget (Box);
            View.Create_Toolbar (Toolbar);
            Toolbar.Show_All;
         else
            W := Gtk_Widget (View);
         end if;

         if Local_Config then
            Gtk_New (Sep);
            Sep.Set_Draw (False);
            Sep.Set_Expand (True);
            Toolbar.Insert (Sep);

            Gtk_New_From_Stock (Button, GPS_Stock_Config_Menu);
            Button.Set_Homogeneous (False);
            Button.Set_Tooltip_Text (-"Configure this panel");
            Toolbar.Insert (Button);
            Gtkada.Handlers.Widget_Callback.Object_Connect
              (Button, Gtk.Tool_Button.Signal_Clicked,
               On_Display_Local_Config_Access, View);
         end if;

         --  Child does not exist yet, create it
         Child := new Formal_MDI_Child;
         Initialize (Child, W,
                     Default_Width  => 215,
                     Default_Height => 600,
                     Focus_Widget   => Focus_Widget,
                     Module         => Module,
                     Group          => Group);
         Set_Title (Child, View_Name, View_Name);
         Put (Get_MDI (Kernel), Child, Initial_Position => Position);
      end Create_If_Needed;

      ----------------
      -- Get_Module --
      ----------------

      function Get_Module return GPS.Kernel.Modules.Module_ID is
      begin
         return Module;
      end Get_Module;

      ------------------
      -- Load_Desktop --
      ------------------

      function Load_Desktop
        (MDI  : MDI_Window;
         Node : Node_Ptr;
         User : Kernel_Handle) return MDI_Child
      is
         pragma Unreferenced (MDI);
         View         : View_Access;
         Child        : GPS_MDI_Child;
      begin
         if Node.Tag.all = Module_Name then
            Create_If_Needed (User, Child, View);
            Load_From_XML (View, Node);
            return MDI_Child (Child);
         end if;
         return null;
      end Load_Desktop;

      ------------------
      -- Save_Desktop --
      ------------------

      function Save_Desktop
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
         User   : Kernel_Handle) return Node_Ptr
      is
         pragma Unreferenced (User);
         N : Node_Ptr;
         Tb : constant Boolean := Local_Toolbar or else Local_Config;
      begin
         if Tb and then Widget.all in Toplevel_Box'Class then
            N := new Node;
            N.Tag := new String'(Module_Name);
            Save_To_XML (Toplevel_Box (Widget.all).Initial, N);
            return N;

         elsif not Tb
           and then Widget.all in Formal_View_Record'Class
         then
            N := new Node;
            N.Tag := new String'(Module_Name);
            Save_To_XML (View_Access (Widget), N);
            return N;
         end if;
         return null;
      end Save_Desktop;

      ------------------
      -- On_Open_View --
      ------------------

      procedure On_Open_View
        (Widget : access GObject_Record'Class;
         Kernel : Kernel_Handle)
      is
         Ignore : View_Access;
         pragma Unreferenced (Widget, Ignore);
      begin
         Ignore := Get_Or_Create_View (Kernel);
      end On_Open_View;

      -------------------
      -- Retrieve_View --
      -------------------

      function Retrieve_View
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
         return View_Access
      is
         Child        : GPS_MDI_Child;
         View         : View_Access;
      begin
         Find (Kernel, Child, View);
         return View;
      end Retrieve_View;

      ------------------------
      -- Get_Or_Create_View --
      ------------------------

      function Get_Or_Create_View
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Focus  : Boolean := True)
         return View_Access
      is
         Child        : GPS_MDI_Child;
         View         : View_Access;
      begin
         Create_If_Needed (Kernel, Child, View);

         if Focus then
            Raise_Child (Child);
            Set_Focus_Child (Child);
         end if;

         if Child = null then
            return null;
         else
            return View;
         end if;
      end Get_Or_Create_View;

      ------------------------
      -- Register_Open_Menu --
      ------------------------

      procedure Register_Open_Menu
        (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
         Menu_Name : String;
         Item_Name : String;
         Before    : String := "") is
      begin
         Register_Menu
           (Kernel, Menu_Name, Item_Name, "", On_Open_View_Access,
            Ref_Item => Before);
      end Register_Open_Menu;

      ---------------------
      -- Register_Module --
      ---------------------

      procedure Register_Module
        (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
         ID          : GPS.Kernel.Modules.Module_ID := null;
         Menu_Name   : String := "Views/" & View_Name;
         Before_Menu : String := "") is
      begin
         if ID = null then
            Module := new Module_ID_Record;
         else
            Module := ID;
         end if;

         Register_Module
           (Module      => Module,
            Kernel      => Kernel,
            Module_Name => Module_Name,
            Priority    => GPS.Kernel.Modules.Default_Priority);
         Register_Desktop_Functions (Save_Desktop_Access, Load_Desktop_Access);
         Register_Open_Menu
           (Kernel, '/' & (-"Tools") & '/' & Dir_Name (Menu_Name),
            Base_Name (Menu_Name), Before => Before_Menu);
      end Register_Module;

   end Simple_Views;

end Generic_Views;
