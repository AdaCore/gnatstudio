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

with Glib.Object;             use Glib.Object;
with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Console;    use Glide_Kernel.Console;
with Glide_Kernel.Project;    use Glide_Kernel.Project;
with Language_Handlers.Glide; use Language_Handlers.Glide;
with Language.C;              use Language.C;
with Language.Cpp;            use Language.Cpp;
with Src_Info;                use Src_Info;
with Src_Info.CPP;            use Src_Info.CPP;
with Unchecked_Deallocation;

package body Cpp_Module is

   CPP_LI_Handler_Name : constant String := "c/c++";
   --  The name the source navigator is registered under.

   procedure Unchecked_Free is new Unchecked_Deallocation
     (CPP_LI_Handler_Record'Class, CPP_LI_Handler);

   procedure Project_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the project view has changed in the kernel.
   --  This resets the internal data for the C/C++ handler.

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (K);
      Handler : constant Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));
   begin
      Reset
        (CPP_LI_Handler
           (Get_LI_Handler_By_Name (Handler, CPP_LI_Handler_Name)),
         Get_Project_View (Kernel));
   end Project_View_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Handler : constant Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));
      LI      : CPP_LI_Handler := new Src_Info.CPP.CPP_LI_Handler_Record;
      Msg     : constant String := Set_Executables (LI);

   begin
      if Msg /= "" then
         --  No parser will be available. However, we still want the
         --  highlighting for C and C++ files
         Insert (Kernel, Msg, Mode => Error);
         Unchecked_Free (LI);
      else
         Kernel_Callback.Connect
           (Kernel, "project_view_changed",
            Kernel_Callback.To_Marshaller (Project_View_Changed'Access),
            Kernel_Handle (Kernel));
      end if;

      Register_LI_Handler (Handler, CPP_LI_Handler_Name, LI_Handler (LI));

      Register_Language (Handler, "c", C_Lang);
      Add_Language_Info
        (Handler, "c",
         LI                  => LI_Handler (LI),
         Default_Spec_Suffix => ".h",
         Default_Body_Suffix => ".c");

      Register_Language (Handler, "c++", Cpp_Lang);
      Add_Language_Info
        (Handler, "c++",
         LI                  => LI_Handler (LI),
         Default_Spec_Suffix => ".h",
         Default_Body_Suffix => ".cc");
   end Register_Module;

end Cpp_Module;
