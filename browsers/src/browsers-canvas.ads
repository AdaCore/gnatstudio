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

with Gtkada.Canvas;
with Glide_Kernel;
with Gtk.Scrolled_Window;

package Browsers.Canvas is

   type Glide_Browser_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Glide_Browser is access all Glide_Browser_Record'Class;

   procedure Gtk_New
     (Browser : out Glide_Browser;
      Mask    : Browser_Type_Mask;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new browser that can include items related to all types
   --  described in Browser_Type. For instance, if Browser_Type is Any_Browser,
   --  then Glide will include a single browser, and the class,
   --  dependency,... browsers will all be mixed.

   procedure Initialize
     (Browser : out Glide_Browser;
      Mask    : Browser_Type_Mask;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal Initialization function.

   function Get_Mask (Browser : access Glide_Browser_Record)
      return Browser_Type_Mask;
   --  Return the list of browser types supported by Browser.

   function Get_Canvas (Browser : access Glide_Browser_Record)
      return Gtkada.Canvas.Interactive_Canvas;
   --  Return the canvas embedded in Browser

private
   type Glide_Browser_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
      record
         Canvas    : Gtkada.Canvas.Interactive_Canvas;
         Kernel    : Glide_Kernel.Kernel_Handle;
         Mask      : Browser_Type_Mask;
      end record;

   pragma Inline (Get_Mask);
   pragma Inline (Get_Canvas);
end Browsers.Canvas;
