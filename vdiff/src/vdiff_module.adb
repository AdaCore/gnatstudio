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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Label;                 use Gtk.Label;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.File_Selector;      use Gtkada.File_Selector;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Intl;                use Glide_Intl;
with Diff_Utils;                use Diff_Utils;
with Vdiff_Pkg;                 use Vdiff_Pkg;
with Vdiff_Utils;               use Vdiff_Utils;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Exceptions;            use Ada.Exceptions;
with Traces;                    use Traces;

package body Vdiff_Module is

   Vdiff_Module_Id : Module_ID;
   Vdiff_Module_Name : constant String := "Visual_Diff";

   Default_Width  : constant := 400;
   Default_Height : constant := 400;
   Tmp_Dir        : constant String := "/tmp/";
   --  <preferences>

   Me : Debug_Handle := Create (Vdiff_Module_Name);

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize Vdiff module.

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  Process, if possible, the data sent by the kernel

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->Compare->Two Files...

   --------------------------
   -- On_Compare_Two_Files --
   --------------------------

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Vdiff  : Vdiff_Access;
      Result : Diff_Occurrence_Link;
      File1  : constant String := Select_File (-"Select First File");
      Child  : MDI_Child;

   begin
      if File1 = "" then
         return;
      end if;

      declare
         File2 : constant String := Select_File (-"Select Second File");

      begin
         if File2 = "" then
            return;
         end if;

         Result := Diff (File1, File2);
         Gtk_New (Vdiff);
         Set_Text (Vdiff.File_Label1, File1);
         Set_Text (Vdiff.File_Label2, File2);
         Fill_Diff_Lists (Vdiff.Clist1, Vdiff.Clist2, File1, File2, Result);
         Show_All (Vdiff.Main_Box);
         Child := Put (Get_MDI (Kernel), Vdiff);

         --  ??? Connect to destroy signal so that we can free result:
         --  Free (Result);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Compare_Two_Files;

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      Vdiff   : Vdiff_Access;
      Result  : Diff_Occurrence_Link;
      Child   : MDI_Child;
      Success : Boolean;

   begin
      if Mime_Type = Mime_Diff_File then
         declare
            Orig_File : constant String := Get_String (Data (Data'First));
            New_File  : constant String := Get_String (Data (Data'First + 1));
            Diff_File : constant String := Get_String (Data (Data'First + 2));

         begin
            if Orig_File = "" then
               if New_File = "" then
                  return False;
               end if;

               declare
                  Base     : constant String := Base_Name (New_File);
                  Ref_File : constant String := Tmp_Dir & Base & "_ref";
               begin
                  Result := Diff
                    (Ref_File, New_File, Diff_File, Revert => True);
                  Gtk_New (Vdiff);
                  Set_Text (Vdiff.File_Label1, Base & (-" <reference>"));
                  Set_Text (Vdiff.File_Label2, New_File);
                  Fill_Diff_Lists
                    (Vdiff.Clist1, Vdiff.Clist2, Ref_File, New_File, Result);
                  Delete_File (Ref_File, Success);
               end;

               --  ??? Connect to destroy signal so that we can free result:
               --  Free (Result);

            elsif New_File = "" then
               if Orig_File = "" then
                  return False;
               end if;

               declare
                  Base     : constant String := Base_Name (Orig_File);
                  Ref_File : constant String := Tmp_Dir & Base & "_ref";
               begin
                  Result := Diff (Orig_File, Ref_File, Diff_File);
                  Gtk_New (Vdiff);
                  Set_Text (Vdiff.File_Label1, Orig_File);
                  Set_Text (Vdiff.File_Label2, Base & (-" <reference>"));
                  Fill_Diff_Lists
                    (Vdiff.Clist1, Vdiff.Clist2, Orig_File, Ref_File, Result);
                  Delete_File (Ref_File, Success);
               end;

               --  ??? Connect to destroy signal so that we can free result:
               --  Free (Result);

            else
               --  All arguments are specified

               Result := Diff (Orig_File, New_File, Diff_File);
               Gtk_New (Vdiff);
               Set_Text (Vdiff.File_Label1, Orig_File);
               Set_Text (Vdiff.File_Label2, New_File);
               Fill_Diff_Lists
                 (Vdiff.Clist1, Vdiff.Clist2, Orig_File, New_File, Result);

               --  ??? Connect to destroy signal so that we can free result:
               --  Free (Result);
            end if;

            Set_Size_Request (Vdiff, Default_Width, Default_Height);
            Show_All (Vdiff.Main_Box);
            Child := Put (Get_MDI (Kernel), Vdiff);
            return True;
         end;
      end if;

      return False;
   end Mime_Action;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := '/' & (-"Tools") & '/' & (-"Compare") & '/';
   begin
      Register_Menu
        (Kernel, Tools, -"Two Files...", "", On_Compare_Two_Files'Access);
   end Initialize_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      Vdiff_Module_Id := Register_Module
        (Module_Name  => Vdiff_Module_Name,
         Priority     => Default_Priority,
         Initializer  => Initialize_Module'Access,
         Mime_Handler => Mime_Action'Access);
   end Register_Module;

end Vdiff_Module;
