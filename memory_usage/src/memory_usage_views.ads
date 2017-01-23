------------------------------------------------------------------------------
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

--  This package provides a view that displays the memory usage of an
--  executable. This view is only activated if the '--print-memory-usage'
--  switch is given to the linker when building the executable with the
--  'Build All' or 'Build Main' Build Targets.
--
--  See the Memory_Usage_Views.Linker_Parser package for more information
--  about the way this switch is enabled/disabled and how the linker's output
--  is parsed when this switch is present.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;

with Gtk.List_Store;                     use Gtk.List_Store;
with Gtk.Tree_View;                      use Gtk.Tree_View;
with Gtk.Widget;                         use Gtk.Widget;
with Gtkada.MDI;

with Generic_Views;
with GPS.Kernel;                         use GPS.Kernel;
with GPS.Kernel.MDI;                     use GPS.Kernel.MDI;

package Memory_Usage_Views is

   type Memory_Usage_View_Record is new Generic_Views.View_Record with private;
   type Memory_Usage_View is access all Memory_Usage_View_Record'Class;
   --  Type representing the memory usage view.

   type Memory_Region_Description is private;
   --  Type used to store all the parsed information from the linker's output
   --  regarding memory regions.

   type Memory_Region_Description_Array is
     array (Integer range <>) of Memory_Region_Description;

private

   type Memory_Region_Description is record
      Name            : Unbounded_String;
      Total_Size      : Unbounded_String;
      Used_Size       : Unbounded_String;
      Percentage_Used : Float;
   end record;

   package Memory_Region_Description_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Memory_Region_Description, "=");

   type Memory_Usage_View_Record is new Generic_Views.View_Record with record
      Memory_Tree       : Gtk_Tree_View;
      Memory_Tree_Model : Gtk_List_Store;
      Memory_Regions    : Memory_Region_Description_Lists.List;
   end record;

   function Initialize
     (Self : access Memory_Usage_View_Record'Class) return Gtk_Widget;
   --  Initialize the memory usage view widget

   procedure Refresh
     (Self           : access Memory_Usage_View_Record'Class;
      Memory_Regions : Memory_Region_Description_Array);
   --  Refresh the given memory usage view to display the memory region
   --  descriptions contained in Memory_Regions.

   package Memory_Usage_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name               => "Memory_Usage_Views",
      View_Name                 => "Memory Usage",
      Formal_View_Record        => Memory_Usage_View_Record,
      Formal_MDI_Child          => GPS_MDI_Child_Record,
      Initialize                => Initialize,
      Areas                     => Gtkada.MDI.Sides_Only,
      Position                  => Gtkada.MDI.Position_Left);
   use Memory_Usage_MDI_Views;
   --  Instantiation of the Generic_Views.Simple_Views package with
   --  the parameters we want for our memory usage views.

end Memory_Usage_Views;
