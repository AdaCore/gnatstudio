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

with Glib; use Glib;
with Gtk.Button; use Gtk.Button;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Vsearch_Pkg; use Vsearch_Pkg;
with Glide_Kernel;

--  This package provides an extended version of the visual search
--  widget that can be found in module vsearch, so that it can be integrated
--  within the project explorer directly.

package Vsearch_Ext is

   type Vsearch_Extended_Record is new Vsearch_Record with record
      Kernel                 : Glide_Kernel.Kernel_Handle;
      Search_Next_Button     : Gtk_Button;
      Search_Replace_Button  : Gtk_Button;
      Search_Previous_Button : Gtk_Button;
      Stop_Button            : Gtk_Button;
      Options_Toggle         : Gtk_Toggle_Button;
      Context                : Gint := 0;
      Scope                  : Gint := 0;
      Continue               : Boolean := True;
   end record;

   Context_Current_File  : constant := 0;
   --  Index of the "Current File" choice in the "Look In" entry.

   Context_Explorer      : constant := 1;
   --  Index of the "Explorer" choice in the "Look In" entry.

   Context_Project_Files : constant := 2;
   --  Index of the "Project Files" choice in the "Look In" entry.

   Context_Files         : constant := 3;
   --  Index of the "Files..." choice in the "Look In" entry.

   Scope_Whole_Text       : constant := 0;
   --  Index of the "Whole Text" choice in the "Scope" entry.

   Scope_Comments         : constant := 1;
   --  Index of the "Comments Only" choice in the "Scope" entry.

   Scope_Strings          : constant := 2;
   --  Index of the "Strings Only" choice in the "Scope" entry.

   Scope_Comments_Strings : constant := 3;
   --  Index of the "Comments + Strings" choice in the "Scope" entry.

   Scope_Code             : constant := 4;
   --  Index of the "All but Comments" choice in the "Scope" entry.

   type Vsearch_Extended is access all Vsearch_Extended_Record'Class;

   procedure Gtk_New
     (Vsearch : out Vsearch_Extended;
      Handle  : Glide_Kernel.Kernel_Handle);
   --  Create a new extended search dialog.

   procedure Initialize
     (Vsearch : access Vsearch_Extended_Record'Class;
      Handle  : Glide_Kernel.Kernel_Handle);
   --  Internal initialization procedure.

end Vsearch_Ext;
