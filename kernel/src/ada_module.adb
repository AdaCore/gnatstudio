-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Src_Info.ALI;            use Src_Info.ALI;
with Language.Ada;            use Language.Ada;
with Language_Handlers.Glide; use Language_Handlers.Glide;

package body Ada_Module is

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Handler : constant Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));
   begin
      Register_LI_Handler
        (Handler, "Ada", new Src_Info.ALI.ALI_Handler_Record);

      Register_Language (Handler, "Ada", Ada_Lang);
      Add_Language_Info
        (Handler, "Ada",
         LI                  => Get_LI_Handler_By_Name (Handler, "Ada"),
         Default_Spec_Suffix => ".ads",
         Default_Body_Suffix => ".adb");

      Register_Search_Pattern
        (Kernel,
         "Ada array types",
         "\btype\s+(\w+)\s+is\s+array\s+\((.*?)\)\s+of\s+(\w+)\s*;");
      Register_Search_Pattern
        (Kernel,
         "Ada case alternatives and exception handlers",
         "\bwhen\s+((\w+)\s+:\s+)?[\w\s|]+\s*=>");
      Register_Search_Pattern
        (Kernel,
         "Ada types and subtypes",
         "\b((sub)?type\s+(\w+)|type\s+(\w+)\s+(\(.*?\))?)\s+is\b");
      Register_Search_Pattern
        (Kernel,
         "Ada discriminated types",
         "\btype\s+(\w+)\s+\((.*?)\)\s+is\b");
      Register_Search_Pattern
        (Kernel,
         "Ada for loops",
         "\bfor\s+(\w+)\s+in\s+(reverse\s+)?(.+?)(\s+"
         & "range\s+(.*?))?\s+loop\b");
   end Register_Module;

end Ada_Module;
