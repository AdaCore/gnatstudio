------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
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

with GPS.Kernel.Contexts;     use GPS.Kernel.Contexts;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Shared_Macros;       use GPS.Shared_Macros;
with String_Utils;            use String_Utils;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.Templates;      use GNATCOLL.Templates;
with Xref;                    use Xref;

package body GPS.Kernel.Macros is

   function Project_From_Param
     (Param   : String;
      Context : GPS.Kernel.Selection_Context) return Project_Type;
   --  Return the project from the parameter. Parameter is the string
   --  following the '%' sign. No_Project is returned if the context doesn't
   --  contain this information

   -------------------
   -- Create_Filter --
   -------------------

   function Create_Filter
     (Command : String;
      Filter  : Macro_Filter := null) return Macro_Filter
   is
      F : Macro_Filter := Filter;

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String;
      --  Check whether the command has a '%' + digit parameter

      ------------------
      -- Substitution --
      ------------------

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String
      is
         pragma Unreferenced (Quoted);
      begin
         if Param = "f"
           or else Param = "F"
           or else Param = "fk"
         then
            F.Requires.File := True;

         elsif Param = "d"
           or else Param = "dk"
         then
            F.Requires.Directory := True;

         elsif Param = "e"
           or else Param = "ek"
         then
            F.Requires.Entity := True;

         elsif Param = "l" then
            F.Requires.Line := True;

         elsif Param = "c" then
            F.Requires.Column := True;

         elsif Param = "a" then
            F.Requires.Category := True;

         elsif Param = "i" then
            F.Requires.Importing := True;

         elsif Param = "s" then
            F.Requires.Single_Line := True;

         elsif Param (Param'First) = 'p' or else Param (Param'First) = 'P' then
            if Param /= "pps" and then Param /= "PPs" then
               F.Requires.Project := Param (Param'First);
            end if;
         end if;

         return "";
      end Substitution;

   begin
      if F = null then
         F := new Macro_Filter_Record;
      end if;

      declare
         Tmp : constant String := Substitute
           (Command,
            Delimiter => Special_Character,
            Callback  => Substitution'Unrestricted_Access,
            Recursive => False);
         pragma Unreferenced (Tmp);
      begin
         null;
      end;

      return F;
   end Create_Filter;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Param     : String;
      Context   : GPS.Kernel.Selection_Context;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String
   is
      --  In this routine it is important to *not* quote backslahes on paths.
      --  This is important because on Windows a backslash is the directory
      --  separator and we do not want to escape it. Doing so will work in most
      --  cases except for remote directory (\\server\drive). Having 4
      --  backslashes at the start of the PATH is not recognized by Windows.

   begin
      Done.all := True;

      if Param = "d" then
         return String_Utils.Protect
           (+To_Remote
              (Directory_Information (Context),
               Get_Nickname (Server)).Full_Name,
            Protect_Quotes      => Quoted,
            Protect_Backslashes => For_Shell);

      elsif Param = "dk" then
         return String_Utils.Protect
           (Krunch
              (+To_Remote
                 (Directory_Information (Context),
                  Get_Nickname (Server)).Full_Name),
            Protect_Quotes      => Quoted,
            Protect_Backslashes => For_Shell);

      elsif Param = "e" then
         if Get_Entity (Context) /= No_Root_Entity then
            --  Get the name from the context, to have the proper casing
            return Entity_Name_Information (Context);
         end if;

      elsif Param = "ef" then
         if Get_Entity (Context) /= No_Root_Entity then
            --  Get the name from the context, to have the proper casing
            if Is_Fuzzy (Get_Entity (Context)) then
               return Entity_Name_Information (Context)
                 & " (best guess)";
            else
               return Entity_Name_Information (Context);
            end if;
         end if;

      elsif Param = "ek" then
         if Get_Entity (Context) /= No_Root_Entity then
            --  Get the name from the context, to have the proper casing
            return Krunch (Entity_Name_Information (Context));
         end if;

      elsif Param = "l" then
         return Image (Line_Information (Context));

      elsif Param = "c" then
         return Image (Integer (Column_Information (Context)));

      elsif Param = "a" then
         if Has_Message_Information (Context) then
            return Message_Information (Context).Get_Category;
         end if;

      elsif Param = "GPS" then
         return Get_Kernel (Context).Get_Home_Dir.Display_Full_Name;

      elsif Param = "system_bin_dir" then
         return Create_From_Dir
           (Get_Kernel (Context).Get_System_Dir, "bin").Display_Full_Name;

      elsif Param = "s" then
         if Has_Entity_Name_Information (Context) then
            return Entity_Name_Information (Context);
         elsif Has_Area_Information (Context) then
            return Text_Information (Context);
         end if;

      elsif Param = "S" then
         if Has_Area_Information (Context) then
            return Text_Information (Context);
         elsif Has_Expression_Information (Context) then
            return Expression_Information (Context);
         elsif Has_Entity_Name_Information (Context) then
            return Entity_Name_Information (Context);
         end if;

      elsif Param = "i" then
         if Importing_Project_Information (Context) /=
           Project_Information (Context)
         then
            return Importing_Project_Information (Context).Name;
         end if;

      elsif Param = "ek" then
         return String_Utils.Protect
           (Krunch (Entity_Name_Information (Context)));

      elsif Param = "gnat" then
         declare
            Target : constant String := Get_Kernel (Context).Get_Target;
         begin
            if Target /= "" then
               return Target & "-gnat";
            else
               return "gnat";
            end if;
         end;

      elsif Param = "target" then
         declare
            Target : constant String := Get_Kernel (Context).Get_Target;
         begin
            if Target /= "" then
               return "--target=" & Target;
            else
               return "";
            end if;
         end;

      else
         return Shared_Macros_Substitute
           (Project_From_Kernel => Get_Project (Get_Kernel (Context)),
            Project_From_Param  => Project_From_Param (Param, Context),
            File_Information    => File_Information (Context),
            Param               => Param,
            Quoted              => Quoted,
            Done                => Done,
            Server              => Server,
            For_Shell           => For_Shell);
      end if;

      --  No substitution
      Done.all := False;
      return "";
   end Substitute;

   ------------------------
   -- Project_From_Param --
   ------------------------

   function Project_From_Param
     (Param   : String;
      Context : GPS.Kernel.Selection_Context) return Project_Type is
   begin
      if Param (Param'First) in 'O' .. 'P' then
         return Get_Project (Get_Kernel (Context));
      else
         return Project_Information (Context);
      end if;
   end Project_From_Param;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Macro_Filter_Record;
      Context : Selection_Context) return Boolean
   is
      Is_Entity_Context : constant Boolean :=
                            Has_Entity_Name_Information (Context);
      Is_Area_Context   : constant Boolean := Has_Area_Information (Context);
      Project           : Project_Type;
      Start, Last       : Integer;
   begin
      if Filter.Requires.Project = 'p'
        or else Filter.Requires.Project = 'P'
      then
         Project := Project_From_Param
           (Filter.Requires.Project & ' ', Context);
         if Project = No_Project then
            return False;
         end if;
      end if;

      if Filter.Requires.File
        and then not Has_File_Information (Context)
      then
         return False;
      end if;

      if Filter.Requires.Directory
        and then not Has_Directory_Information (Context)
      then
         return False;
      end if;

      if Filter.Requires.Single_Line then
         if Is_Area_Context then
            Get_Area (Context, Start, Last);
            if Start /= Last then
               return False;
            end if;
         elsif not Is_Entity_Context then
            return False;
         end if;
      end if;

      if Filter.Requires.Entity then
         if not Is_Entity_Context then
            return False;
         end if;

         --  Avoid cases where we click on a keyword
         if Get_Entity (Context) = No_Root_Entity then
            return False;
         end if;
      end if;

      if Filter.Requires.Line
        and then not Has_Line_Information (Context)
      then
         return False;
      end if;

      if Filter.Requires.Column
        and then not Has_Column_Information (Context)
      then
         return False;
      end if;

      if Filter.Requires.Category
        and then not Has_Message_Information (Context)
      then
         return False;
      end if;

      if Filter.Requires.Importing
        and then not Has_Importing_Project_Information (Context)
      then
         return False;
      end if;

      return True;
   end Filter_Matches_Primitive;

end GPS.Kernel.Macros;
