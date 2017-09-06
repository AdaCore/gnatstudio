------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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

with Gtk.Button;  use Gtk.Button;
with Gtk.Frame;   use Gtk.Frame;
with Gtk.GEntry;  use Gtk.GEntry;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with GPS.Kernel;      use GPS.Kernel;
with GPS.Kernel.MDI;
with GPS.Dialogs;     use GPS.Dialogs;

package Toolchains_Dialog is

   type Dialog_Record is new GPS_Dialog_Record with private;
   type Dialog is access all Dialog_Record;

   procedure Gtk_New
     (Widget           : out Dialog;
      Kernel           : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Active           : Boolean;
      Tools_Path       : Virtual_File;
      Use_Xrefs_Subdirs : Boolean;
      Compiler_Path     : Virtual_File);

   function Get_Active
     (Widget : access Dialog_Record'Class) return Boolean;
   --  Whether the dual compilation mode should be activated

   function Get_Use_Xrefs_Subdir
     (Widget : access Dialog_Record'Class) return Boolean;
   --  Whether we should read ali files from a separate xrefs subdir

   function Get_Tools_Path
     (Widget : access Dialog_Record'Class) return Virtual_File;
   --  Get the path where to find the Bleeding edge compiler

   function Get_Compiler_Path
     (Widget : access Dialog_Record'Class) return Virtual_File;
   --  Get the path where to find the regular compiler

private

   type Dialog_Record is new GPS_Dialog_Record with record
      OK_Button      : Gtk_Button;
      Active         : Boolean;
      Xrefs_Subdir   : Boolean;
      Frame          : Gtk.Frame.Gtk_Frame;
      Tools_Entry    : Gtk.GEntry.Gtk_Entry;
      Compiler_Entry : Gtk.GEntry.Gtk_Entry;
   end record;

end Toolchains_Dialog;
