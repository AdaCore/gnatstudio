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

with Find_Utils;
with Glide_Kernel;
with Glib.Object;
with Gtk.Button;
with Gtk.Main;
with Gtk.Menu_Item;
with Gtk.Toggle_Button;
with Gtk.Widget;
with Vsearch_Pkg;

--  This package provides an extended version of the visual search
--  widget that can be found in module vsearch, so that it can be integrated
--  within the project explorer directly.

package Vsearch_Ext is

   type Vsearch_Extended_Record is new Vsearch_Pkg.Vsearch_Record with private;
   type Vsearch_Extended is access all Vsearch_Extended_Record'Class;

   procedure Gtk_New
     (Vsearch : out Vsearch_Extended;
      Handle  : Glide_Kernel.Kernel_Handle);
   --  Create a new extended search dialog.

   procedure Initialize
     (Vsearch : access Vsearch_Extended_Record'Class;
      Handle  : Glide_Kernel.Kernel_Handle);
   --  Internal initialization procedure.

   --------------------------------
   -- Registering search modules --
   --------------------------------

   procedure Register_Search_Function
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Label             : String;
      Factory           : Find_Utils.Module_Search_Context_Factory;
      Extra_Information : Gtk.Widget.Gtk_Widget := null;
      Mask              : Find_Utils.Search_Options_Mask);
   --  Register a new search function.
   --  This will be available under the title Label in the search combo box.
   --
   --  If Extra_Information is not null, then it will be displayed every time
   --  this label is selected. It can be used for instance to ask for more
   --  information like a list of files to search.
   --  Whenever the data in Extra_Information changes, or for some reason the
   --  current status of GPS no longer permits the search, you should raise the
   --  kernel signal Search_Reset_Signal (or call Vsearch_Ext.Reset_Search).
   --
   --  When the user then selects "Find", the function Factory is called to
   --  create the factory. The options and searched string or regexp will be
   --  set automatically on return of Factory, so you do not need to handle
   --  this.
   --  Mask indicates what options are relevant for that module. Options that
   --  are not set will be greyed out.
   --  If Supports_Replace if false, then the button will be greyed out.

   procedure Register_Default_Search
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the default search function

   procedure Reset_Search
     (Object : access Glib.Object.GObject_Record'Class;
      Kernel : Glide_Kernel.Kernel_Handle);
   --  Raises the kernel signal Search_Reset_Signal. This is just a convenience
   --  function. Object is ignored, and can be anything.

private
   type Vsearch_Extended_Record is new Vsearch_Pkg.Vsearch_Record with record
      Kernel                 : Glide_Kernel.Kernel_Handle;
      Search_Next_Button     : Gtk.Button.Gtk_Button;
      Search_Replace_Button  : Gtk.Button.Gtk_Button;
      Search_Previous_Button : Gtk.Button.Gtk_Button;
      Stop_Button            : Gtk.Button.Gtk_Button;
      Options_Toggle         : Gtk.Toggle_Button.Gtk_Toggle_Button;
      Continue               : Boolean := True;
      Extra_Information      : Gtk.Widget.Gtk_Widget;
      Search_Idle_Handler    : Gtk.Main.Idle_Handler_Id := 0;
      Last_Search_Context    : Find_Utils.Search_Context_Access;

      Next_Menu_Item         : Gtk.Menu_Item.Gtk_Menu_Item;

      Find_Next              : Boolean := False;
   end record;

end Vsearch_Ext;
