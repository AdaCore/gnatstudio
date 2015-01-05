------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

with Gtkada.Canvas_View;    use Gtkada.Canvas_View;
with GPS.Kernel;
with GVD.Process;           use GVD.Process;
with Items;                 use Items;

package GVD.Canvas is

   procedure Attach_To_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach debugger to a data window.
   --  If an unattached data window exists in the desktop, it is reused.
   --  If none exists, one is created if Create_If_Necessary is true.
   --  Nothing is done when Debugger is already attached to a data window.
   --
   --  The debugger console should be created already. When it is closed (ie
   --  the debugger exits), the data window will be destroyed

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the data windows

   procedure Process_Graph_Cmd
     (Process : access GVD.Process.Visual_Debugger_Record'Class;
      Cmd     : String);
   --  Parse and process a "graph print" or "graph display" command

   procedure Dereference_Item
     (Component : not null access Component_Item_Record'Class);
   --  Dereference a component of Item ("graph display" on it with a link from
   --  the item).

   procedure Change_Visibility
     (Item      : not null access Gtkada.Canvas_View.Canvas_Item_Record'Class;
      Component : not null access Generic_Type'Class);
   --  Change the visibility status of a specific component in the item

end GVD.Canvas;
