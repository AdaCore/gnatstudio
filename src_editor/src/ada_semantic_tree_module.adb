-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2007, AdaCore                   --
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

with GNAT.Strings; use GNAT.Strings;

with Language.Tree.Database;  use Language.Tree.Database;
with Ada_Semantic_Tree.Assistants; use Ada_Semantic_Tree.Assistants;
with VFS;                    use VFS;
with Gtkada.MDI; use Gtkada.MDI;
with Gtkada; use Gtkada;
with Src_Editor_Module; use Src_Editor_Module;
with Src_Editor_Buffer; use Src_Editor_Buffer;
with Src_Editor_Box; use Src_Editor_Box;

package body Ada_Semantic_Tree_Module is

   use GPS.Kernel;

   type GPS_Buffer_Provider is new Buffer_Provider with record
      Kernel : Kernel_Handle;
   end record;

   ----------------
   -- Get_Buffer --
   ----------------

   overriding
   function Get_Buffer
     (Provider : access GPS_Buffer_Provider;
      File     : Virtual_File) return String_Access;
   --  Return the buffer from the editor if any, from the file otherwise.

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Provider : access GPS_Buffer_Provider;
      File     : Virtual_File) return String_Access
   is
      Editor : Gtkada.MDI.MDI_Child;
   begin
      --  ??? We have a potential inconsistency here.
      --  In the "then" branch, we return a string encoded in UTF-8, and in
      --  the "else" branch, we return a string encoded in the locale.

      if Is_Open (Provider.Kernel, File) then
         Editor := Find_Editor (Provider.Kernel, File);

         return new String'(Get_Text
           (Get_Buffer (Source_Editor_Box (Get_Widget (Editor))), 1, 1));
      else
         return Read_File (File);
      end if;
   end Get_Buffer;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Initialize
        (Get_Construct_Database (Kernel).all,
         new GPS_Buffer_Provider'
           (Buffer_Provider with Kernel => Kernel_Handle (Kernel)));
      Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
        (Get_Construct_Database (Kernel));
   end Register_Module;

end Ada_Semantic_Tree_Module;
