-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gdk.Event;
with Gdk.Window;
with Gtk.Menu;

with Src_Info;
with Glide_Kernel;
with Browsers.Canvas;

package Browsers.Dependency_Items is

   ----------------
   -- File items --
   ----------------
   --  These items represent source files from the application.

   type File_Item_Record is new Browsers.Canvas.Glide_Browser_Item_Record
     with private;
   type File_Item is access all File_Item_Record'Class;

   procedure Gtk_New
     (Item    : out File_Item;
      Win     : Gdk.Window.Gdk_Window;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      File    : Src_Info.Internal_File);
   --  Create a new dependency item that represents Dep.

   procedure Gtk_New
     (Item            : out File_Item;
      Win             : Gdk.Window.Gdk_Window;
      Browser         : access Browsers.Canvas.Glide_Browser_Record'Class;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_Filename : String);
   --  Create a new dependency item directly from a source filename

   procedure Initialize
     (Item    : access File_Item_Record'Class;
      Win     : Gdk.Window.Gdk_Window;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      File    : Src_Info.Internal_File);
   --  Internal initialization function

   procedure On_Button_Click
     (Item  : access File_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button);
   --  Called when the item is clicked on.

   function Get_Source (Item : access File_Item_Record)
      return Src_Info.Internal_File;
   --  Return the source file associated with Item

   procedure Destroy (Item : in out File_Item_Record);
   --  Free the memory associated with the item

   function Contextual_Factory
     (Item  : access File_Item_Record;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use for this item

   ----------------------
   -- Dependency links --
   ----------------------

   type Dependency_Link_Record is new Browsers.Canvas.Glide_Browser_Link_Record
     with private;
   type Dependency_Link is access all Dependency_Link_Record'Class;

   procedure Gtk_New
     (Link : out Dependency_Link;
      Dep  : Src_Info.Dependency_Info);
   --  Create a new link.


private
   type File_Item_Record is new Browsers.Canvas.Glide_Browser_Item_Record
   with record
      Source : Src_Info.Internal_File;
      Kernel : Glide_Kernel.Kernel_Handle;

      Browser : Browsers.Canvas.Glide_Browser := null;
      --  Pointer to the parent browser. Note that this is initialized lazily
      --  the first time we need to access this browser.
   end record;

   type Dependency_Link_Record is new Browsers.Canvas.Glide_Browser_Link_Record
   with record
      Dep : Src_Info.Dependency_Info;
   end record;

   pragma Inline (Get_Source);
end Browsers.Dependency_Items;
