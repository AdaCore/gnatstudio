-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Intl;           use Glide_Intl;
with Glib.Object;          use Glib.Object;
with VFS;                  use VFS;
with Docgen.Work_On_File;  use Docgen.Work_On_File;
with Docgen;               use Docgen;
with Docgen.Html_Output;   use Docgen.Html_Output;
with Src_Info;             use Src_Info;
with Traces;               use Traces;
with Ada.Exceptions;       use Ada.Exceptions;
with File_Utils;           use File_Utils;

package body Docgen_Module is

   Me : constant Debug_Handle := Create ("Docgen");

   procedure On_Generate_API
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Generate the API for the selected file

   ---------------------
   -- On_Generate_API --
   ---------------------

   procedure On_Generate_API
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      use Type_Source_File_List;
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
      File    : Virtual_File;
      Source_File_List : Type_Source_File_List.List;
      LI : LI_File_Ptr;
      Options : All_Options;
   begin
      if Context.all in File_Selection_Context'Class
        and then Has_File_Information (File_Selection_Context_Access (Context))
      then
         Push_State (Kernel, Busy);
         File := File_Information (File_Selection_Context_Access (Context));
         LI := Locate_From_Source_And_Complete (Kernel, File);

         Options := (Process_Body_Files => False,
                     Ignorable_Comments => False,
                     Comments_Above     => False,
                     Show_Private       => False,
                     References         => False,
                     One_Doc_File       => False,
                     Link_All           => False);

         Append
           (Source_File_List,
            (File_Name        => File,
             Package_Name     => new String'(Get_Unit_Name (LI, File)),
             Other_File_Found => True));

         --  ??? Shouldn't always generate in /tmp/
         --  ??? Should give a choice of the format

         Process_Files
           (Source_File_List,
            Kernel,
            Options,
            Doc_Directory => Name_As_Directory ("/tmp/"),
            Doc_Suffix    => ".htm",
            Converter     => Doc_HTML_Create'Access);
         Free (Source_File_List);

         --  ??? Should open the appropriate file, this one is only valid
         --  for html
         --  ??? <frameset> not support by internal html viewer, so we can't
         --  open /tmp/index.html

         Open_Html
           (Kernel,
            Filename => Create
              (Full_Filename => Get_Doc_File_Name
                 (File, Name_As_Directory ("/tmp/"), ".htm")));

         Pop_State (Kernel);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Pop_State (Kernel);
   end On_Generate_API;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Docgen_Module_Id : Module_ID;
      Tools : constant String := '/' & (-"Tools") & '/';
   begin
      Register_Module
        (Module      => Docgen_Module_Id,
         Kernel      => Kernel,
         Module_Name => "Docgen");
      Register_Menu
        (Kernel, Tools, -"_Generate API doc",
         Callback => On_Generate_API'Access);
   end Register_Module;

end Docgen_Module;
