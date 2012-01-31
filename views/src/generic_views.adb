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

with Glib.Object;           use Glib.Object;
with XML_Utils;             use XML_Utils;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.MDI;            use Gtkada.MDI;

with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.MDI;        use GPS.Kernel.MDI;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI; use GPS.Kernel.Modules.UI;
with GPS.Intl;              use GPS.Intl;

package body Generic_Views is

   -------------------
   -- Load_From_XML --
   -------------------

   procedure Load_From_XML
     (View : access View_Record; XML : XML_Utils.Node_Ptr)
   is
      pragma Unreferenced (View, XML);
   begin
      null;
   end Load_From_XML;

   -----------------
   -- Save_To_XML --
   -----------------

   function Save_To_XML
     (View : access View_Record) return XML_Utils.Node_Ptr
   is
      pragma Unreferenced (View);
   begin
      return null;
   end Save_To_XML;

   ------------------
   -- Simple_Views --
   ------------------

   package body Simple_Views is

      Module : Module_ID;

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
         Focus_Widget : Gtk_Widget;
      begin
         if Node.Tag.all = Module_Name then
            if Reuse_If_Exist then
               Child := GPS_MDI_Child
                 (Find_MDI_Child_By_Tag
                    (Get_MDI (User), Formal_View_Record'Tag));
            end if;

            if Child = null then
               View := new Formal_View_Record;
            else
               View := View_Access (Get_Widget (Child));
            end if;

            Focus_Widget := Initialize (View, User);

            if Node.Child /= null then
               Load_From_XML (View, Node.Child);
            end if;

            if Focus_Widget = null then
               Focus_Widget := Get_Child (View);
            end if;

            if Child = null then
               --  Child does not exist yet, create it
               Gtk_New (Child, View,
                        Default_Width  => 215,
                        Default_Height => 600,
                        Focus_Widget   => Focus_Widget,
                        Module         => Module,
                        Group          => Group_View);
               Set_Title (Child, View_Name, View_Name);
            end if;

            Put (Get_MDI (User), Child, Initial_Position => Position_Left);
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
      begin
         if Widget.all in Formal_View_Record'Class then
            N := new Node;
            N.Tag := new String'(Module_Name);
            N.Child := Save_To_XML (View_Access (Widget));
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

      ------------------------
      -- Get_Or_Create_View --
      ------------------------

      function Get_Or_Create_View
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Focus  : Boolean := True;
         Group  : Gtkada.MDI.Child_Group := GPS.Kernel.MDI.Group_View)
         return View_Access
      is
         Child        : GPS_MDI_Child;
         View         : View_Access;
         Focus_Widget : Gtk_Widget;
      begin
         if Reuse_If_Exist then
            Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
              (Get_MDI (Kernel), Formal_View_Record'Tag));
         end if;

         if Child = null then
            View := new Formal_View_Record;
            Focus_Widget := Initialize (View, Kernel);

            if Focus_Widget = null then
               --  Fallback on using the child of the scrolled window, better
               --  than not providing any Focus_Widget.
               Focus_Widget := Get_Child (View);
            end if;

            Gtk_New (Child, View,
                     Default_Width  => 215,
                     Default_Height => 600,
                     Focus_Widget   => Focus_Widget,
                     Group          => Group,
                     Module         => Module);
            Set_Title (Child, View_Name, View_Name);
            Put (Get_MDI (Kernel), Child, Initial_Position => Position_Bottom);
         end if;

         if Focus then
            Raise_Child (Child);
            Set_Focus_Child (Child);
         end if;

         if Child = null then
            return null;
         else
            return View_Access (Get_Widget (Child));
         end if;
      end Get_Or_Create_View;

      ---------------------
      -- Register_Module --
      ---------------------

      procedure Register_Module
        (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
         ID          : GPS.Kernel.Modules.Module_ID := null;
         Menu_Name   : String := View_Name;
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

         Register_Menu
           (Kernel, '/' & (-"Tools") & '/' & (-"_Views"),
            Menu_Name, "", On_Open_View_Access,
            Ref_Item => Before_Menu);
      end Register_Module;

   end Simple_Views;

end Generic_Views;
