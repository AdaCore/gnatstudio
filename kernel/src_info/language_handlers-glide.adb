-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2002                            --
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

with Language;                  use Language;
with Basic_Types;               use Basic_Types;
with Src_Info;                  use Src_Info;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Prj_API;                   use Prj_API;
with Types;                     use Types;
with Namet;                     use Namet;
with Prj;                       use Prj;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Language_Handlers.Glide is

   function Get_Index_From_Language
     (Handler       : access Glide_Language_Handler_Record'Class;
      Language_Name : String) return Natural;
   --  Return the index of Language in Handler.Languages, or 0 if no such
   --  language is known.

   function Get_Language_From_File
     (Handler         : access Glide_Language_Handler_Record;
      Source_Filename : String;
      Project         : Prj.Project_Id) return String;
   --  Same as the default one, except we already know the project to which for
   --  Source_Filename belongs.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Language_Info_Array, Language_Info_Access);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Handler : out Glide_Language_Handler;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Handler := new Glide_Language_Handler_Record;
      Handler.Kernel := Kernel_Handle (Kernel);
   end Gtk_New;

   -----------------------------
   -- Get_Index_From_Language --
   -----------------------------

   function Get_Index_From_Language
     (Handler       : access Glide_Language_Handler_Record'Class;
      Language_Name : String) return Natural is
   begin
      if Handler.Languages /= null then
         for Index in Handler.Languages'Range loop
            if Language_Name =
              Handler.Languages (Index).Language_Name.all
            then
               return Index;
            end if;
         end loop;
      end if;
      return 0;
   end Get_Index_From_Language;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (Handler : access Glide_Language_Handler_Record;
      Source_Filename : String) return Language.Language_Access
   is
      Index : Natural;
   begin
      Index := Get_Index_From_Language
        (Handler, Get_Language_From_File (Handler, Source_Filename));
      if Index /= 0 then
         return Handler.Languages (Index).Lang;
      end if;
      raise Unsupported_Language;
      return null;
   end Get_Language_From_File;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (Handler : access Glide_Language_Handler_Record;
      Source_Filename : String) return String
   is
      --  ??? Could be optimized, since both Get_Project_From_File and
      --  ??? Get_Language_Of traverse the project structure
      Lang : Name_Id := Get_Language_Of
        (Get_Project_From_File
           (Get_Project_View (Handler.Kernel), Source_Filename),
         Base_Name (Source_Filename));
   begin
      if Lang = No_Name then
         raise Unsupported_Language;
         return "";
      else
         return Get_Name_String (Lang);
      end if;
   end Get_Language_From_File;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (Handler         : access Glide_Language_Handler_Record;
      Source_Filename : String;
      Project         : Prj.Project_Id) return String
   is
      Proj : Project_Id := Project;
      Lang : Name_Id;
   begin
      if Project = No_Project then
         Proj := Get_Project_From_File
           (Get_Project_View (Handler.Kernel), Source_Filename);
      end if;

      Lang := Get_Language_Of (Proj, Base_Name (Source_Filename));

      if Lang = No_Name then
         raise Unsupported_Language;
         return "";
      else
         return Get_Name_String (Lang);
      end if;
   end Get_Language_From_File;

   -----------------------
   -- Register_Language --
   -----------------------

   procedure Register_Language
     (Handler : access Glide_Language_Handler_Record;
      Name    : String;
      Lang    : Language.Language_Access)
   is
      N : constant String := To_Lower (Name);
      Tmp : Language_Info_Access;
      Index : Natural;
   begin
      if Handler.Languages /= null then
         Index := Get_Index_From_Language (Handler, N);
         if Index /= 0 then
            Handler.Languages (Index).Lang := Lang;
            return;
         end if;

         Tmp := new Language_Info_Array
           (Handler.Languages'First .. Handler.Languages'Last + 1);
         Tmp (Handler.Languages'First .. Handler.Languages'Last) :=
           Handler.Languages.all;
         Unchecked_Free (Handler.Languages);
         Handler.Languages := Tmp;
      else
         Handler.Languages := new Language_Info_Array (1 .. 1);
      end if;

      Handler.Languages (Handler.Languages'Last) :=
        (Language_Name => new String' (Name),
         Handler       => null,
         Lang          => Lang);
   end Register_Language;

   ---------------------
   -- Known_Languages --
   ---------------------

   function Known_Languages
     (Handler : access Glide_Language_Handler_Record)
      return Basic_Types.String_Array is
   begin
      if Handler.Languages /= null then
         declare
            Result : String_Array (Handler.Languages'Range);
         begin
            for Index in Handler.Languages'Range loop
               Result (Index) := new String'
                 (Handler.Languages (Index).Language_Name.all);
            end loop;
            return Result;
         end;
      else
         declare
            Result : String_Array (1 .. 0);
         begin
            return Result;
         end;
      end if;
   end Known_Languages;

   -----------------------
   -- Add_Language_Info --
   -----------------------

   procedure Add_Language_Info
     (Handler             : access Glide_Language_Handler_Record;
      Language_Name       : String;
      LI                  : Src_Info.LI_Handler;
      Default_Spec_Suffix : String;
      Default_Body_Suffix : String)
   is
      N : constant String := To_Lower (Language_Name);
      Index : Natural := Get_Index_From_Language (Handler, N);
      Lang : Name_Id;
      Spec, Impl : Name_Id;
   begin
      if Index /= 0 then
         Handler.Languages (Index).Handler := LI;

         Name_Len := Language_Name'Length;
         Name_Buffer (1 .. Name_Len) := Language_Name;
         Lang := Name_Find;

         Name_Len := Default_Spec_Suffix'Length;
         Name_Buffer (1 .. Name_Len) := Default_Spec_Suffix;
         Spec := Name_Find;

         Name_Len := Default_Body_Suffix'Length;
         Name_Buffer (1 .. Name_Len) := Default_Body_Suffix;
         Impl := Name_Find;

         Register_Default_Naming_Scheme
           (Language => Lang,
            Default_Spec_Suffix => Spec,
            Default_Impl_Suffix => Impl);
      end if;
   end Add_Language_Info;

   ------------------------------
   -- Get_LI_Handler_From_File --
   ------------------------------

   function Get_LI_Handler_From_File
     (Handler         : access Glide_Language_Handler_Record;
      Source_Filename : String;
      Project         : Prj.Project_Id := Prj.No_Project)
      return Src_Info.LI_Handler
   is
      Lang : constant String :=
        Get_Language_From_File (Handler, Source_Filename, Project);
      Index : Natural := Get_Index_From_Language (Handler, Lang);
   begin
      if Index /= 0
        and then Handler.Languages (Index).Handler /= null
      then
         return Handler.Languages (Index).Handler;
      else
         raise Unsupported_Language;
         return null;
      end if;
   end Get_LI_Handler_From_File;

end Language_Handlers.Glide;
