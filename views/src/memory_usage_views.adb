--------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Ada.Strings.Maps.Constants;            use Ada.Strings.Maps.Constants;

with Glib;                                  use Glib;
with Gtk.Box;                               use Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf;              use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Progress;            use Gtk.Cell_Renderer_Progress;
with Gtk.Cell_Renderer_Text;                use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                             use Gtk.Enums;
with Gtk.Scrolled_Window;                   use Gtk.Scrolled_Window;
with Gtk.Tree_Model;                        use Gtk.Tree_Model;
with Gtk.Tree_View_Column;                  use Gtk.Tree_View_Column;

package body Memory_Usage_Views is

   Flash_Memory_Icon_Name : constant String := "gps-flash-memory-symbolic";
   --  Name of the icon representing flash memory

   RAM_Memory_Icon_Name   : constant String := "gps-ram-memory-symbolic";
   --  Name of the icon representing RAM memory

   Icon_Column            : constant := 0;
   --  Column containing the name of the memory icon to display.

   Name_Column            : constant := 1;
   --  Column containing the name of the memory section.

   Percentage_Column      : constant := 2;
   --  Column containing the percentage of used memory for the given section.

   Percentage_Text_Column : constant := 3;
   --  Column containing the percentage in a text form so that it can be
   --  displayed in the progress bar.

   Column_Types : constant GType_Array :=
                    (Icon_Column            => GType_String,
                     Name_Column            => GType_String,
                     Percentage_Column      => GType_Int,
                     Percentage_Text_Column => GType_String);

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self           : access Memory_Usage_View_Record'Class;
      Memory_Regions : Memory_Region_Description_Array)
   is
      Iter : Gtk_Tree_Iter;

      function Get_Icon_Name
        (Memory_Region_Name : Unbounded_String) return String;
      --  Return the icon corresponding to RAM memory or FLASH memory depending
      --  on Memory_Region_Name.

      -------------------
      -- Get_Icon_Name --
      -------------------

      function Get_Icon_Name
        (Memory_Region_Name : Unbounded_String) return String is
      begin
         if Index (Memory_Region_Name,
                   Pattern => "RAM",
                   Mapping => Ada.Strings.Maps.Constants.Upper_Case_Map) /= 0
         then
            return RAM_Memory_Icon_Name;
         else
            return Flash_Memory_Icon_Name;
         end if;
      end Get_Icon_Name;

   begin
      Self.Memory_Tree_Model.Clear;

      for Memory_Region of Memory_Regions loop
         declare
            Icon_Name : constant String := Get_Icon_Name (Memory_Region.Name);
         begin
            Self.Memory_Tree_Model.Append (Iter);
            Set
              (Self.Memory_Tree_Model, Iter, Icon_Column, Icon_Name);
            Set (Self.Memory_Tree_Model, Iter, Name_Column,
                 To_String (Memory_Region.Name));
            Set (Self.Memory_Tree_Model, Iter, Percentage_Column,
                 Gint (Memory_Region.Percentage_Used));
            Set (Self.Memory_Tree_Model, Iter, Percentage_Text_Column,
                 To_String
                   (Memory_Region.Used_Size
                    & " / " & Memory_Region.Total_Size));
         end;
      end loop;
   end Refresh;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Memory_Usage_View_Record'Class) return Gtk_Widget
   is
      Scrolled          : Gtk_Scrolled_Window;
      Column            : Gtk_Tree_View_Column;
      Icon_Renderer     : Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer     : Gtk_Cell_Renderer_Text;
      Progress_Renderer : Gtk_Cell_Renderer_Progress;
      Dummy             : Gint;
      pragma Unreferenced (Dummy);
   begin
      --  Initialize the view itself
      Initialize_Vbox (Self, Homogeneous => False);

      --  Create a scrolled window to contain all the view's widgets
      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scrolled, Expand => True, Fill => True);

      --  Create a tree view to display the memory region descriptions
      Gtk_New (Self.Memory_Tree_Model, Column_Types);
      Gtk_New (Self.Memory_Tree, +Self.Memory_Tree_Model);
      Self.Memory_Tree.Set_Headers_Visible (False);
      Scrolled.Add (Self.Memory_Tree);

      --  Create a tree view column to display an icon representing the type
      --  of memory corresponding to the region.
      Gtk_New (Column);
      Gtk_New (Icon_Renderer);
      Column.Pack_Start (Icon_Renderer, Expand => False);
      Column.Add_Attribute (Icon_Renderer, "icon-name", Icon_Column);
      Dummy := Self.Memory_Tree.Append_Column (Column);

      --  Create a tree view column to display the name of the memory region
      Gtk_New (Column);
      Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, Expand => False);
      Column.Add_Attribute (Text_Renderer, "text", Name_Column);
      Dummy := Self.Memory_Tree.Append_Column (Column);

      --  Create a tree view column to display a progress bar representing
      --  the percentage of the region's used memory.
      Gtk_New (Column);
      Gtk_New (Progress_Renderer);
      Column.Pack_Start (Progress_Renderer, Expand => True);
      Column.Add_Attribute (Progress_Renderer, "value", Percentage_Column);
      Column.Add_Attribute (Progress_Renderer, "text", Percentage_Text_Column);
      Dummy := Self.Memory_Tree.Append_Column (Column);

      --  No widget to focus
      return null;
   end Initialize;

end Memory_Usage_Views;
