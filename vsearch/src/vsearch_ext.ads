-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Find_Utils;
with Glide_Kernel;
with Gtk.Button;
with Gtk.Main;
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

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Register_Search_Function
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Data   : Find_Utils.Search_Module_Data);
   --  See Find_Utils.Register_Search_Function;

private
   type Vsearch_Extended_Record is new Vsearch_Pkg.Vsearch_Record with record
      Kernel                 : Glide_Kernel.Kernel_Handle;
      Search_Next_Button     : Gtk.Button.Gtk_Button;
      Search_Replace_Button  : Gtk.Button.Gtk_Button;
      Search_Previous_Button : Gtk.Button.Gtk_Button;
      Stop_Button            : Gtk.Button.Gtk_Button;
      Close_Button           : Gtk.Button.Gtk_Button;
      Options_Toggle         : Gtk.Toggle_Button.Gtk_Toggle_Button;
      Continue               : Boolean := True;
      Extra_Information      : Gtk.Widget.Gtk_Widget;
      Search_Idle_Handler    : Gtk.Main.Idle_Handler_Id := 0;
      Last_Search_Context    : Find_Utils.Search_Context_Access;
      Find_Next              : Boolean := False;
   end record;

end Vsearch_Ext;
