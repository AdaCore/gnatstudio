-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gint_Xml;            use Gint_Xml;
with Glib;                use Glib;
with Glide_Kernel;        use Glide_Kernel;
with Project_Trees;       use Project_Trees;
with Scenario_Views;      use Scenario_Views;
with Gtk.Box;             use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;

package body Project_Explorers is

   function Load_Desktop
     (Node : Gint_Xml.Node_Ptr; User : Kernel_Handle)
      return Gtk_Widget;
   --  Save the status of the project explorer to an XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Restore the status of the explorer from a saved XML tree.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer : out Project_Explorer;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Explorer := new Project_Explorer_Record;
      Initialize (Explorer, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Explorer : access Project_Explorer_Record'Class;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
   begin
      Initialize_Vbox (Explorer, Homogeneous => False);

      Gtk_New (Explorer.Scenario, Kernel);
      Pack_Start (Explorer, Explorer.Scenario, Fill => True, Expand => False);

      Gtk_New (Explorer.Tree, Kernel);
      Gtk_New (Scrolled);
      Set_Size_Request (Scrolled, 300, -1);
      Pack_Start (Explorer, Scrolled, Fill => True, Expand => True);
      Add (Scrolled, Explorer.Tree);
   end Initialize;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (Explorer : access Project_Explorer_Record)
      return Project_Trees.Project_Tree is
   begin
      return Explorer.Tree;
   end Get_Tree;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Gint_Xml.Node_Ptr; User : Kernel_Handle)
      return Gtk_Widget
   is
      pragma Warnings (Off, Node);
      pragma Warnings (Off, User);
      Explorer : Project_Explorer;
   begin
      if Node.Tag.all = "Project_Explorer" then
         Gtk_New (Explorer, User);
         return Gtk_Widget (Explorer);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Widget.all in Project_Explorer_Record'Class then
         N := new Node;
         N.Tag := new String' ("Project_Explorer");
         return N;
      end if;

      return null;
   end Save_Desktop;


begin
   Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
     (Save_Desktop'Access, Load_Desktop'Access);
end Project_Explorers;
