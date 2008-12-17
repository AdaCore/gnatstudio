-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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

with GPS.Editors; use GPS.Editors;

package Src_Editor_Module.Editors is

   type Src_Editor_Buffer_Factory is new GPS.Editors.Editor_Buffer_Factory
   with record
      Kernel : Kernel_Handle;
   end record;

   overriding function Get
     (This  : Src_Editor_Buffer_Factory;
      File  : Virtual_File := No_File;
      Force : Boolean := False;
      Open  : Boolean := True) return Editor_Buffer'Class;

end Src_Editor_Module.Editors;
