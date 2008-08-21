-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005-2008, AdaCore              --
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

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Entities;                use Entities;
with GPS.Kernel.Contexts;     use GPS.Kernel.Contexts;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with Projects;                use Projects;
with Projects.Registry;       use Projects.Registry;
with Remote.Path.Translator;  use Remote.Path.Translator;
with String_Utils;            use String_Utils;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNAT.Strings;
with GNATCOLL.Templates;          use GNATCOLL.Templates;

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
            Delimiter         => Special_Character,
            Callback          => Substitution'Unrestricted_Access,
            Recursive         => False);
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
     (Param   : String;
      Context : GPS.Kernel.Selection_Context;
      Quoted  : Boolean;
      Done    : access Boolean;
      Server  : Server_Type := GPS_Server)
      return String
   is
      Project : Project_Type := No_Project;
      Index   : Integer;
      Recurse, List_Dirs, List_Sources : Boolean;
      Entity  : Entity_Information;

   begin
      Done.all := True;

      if Param = "f"
        or else Param = "F"
        or else Param = "fk"
      then
         if Param = "f" then
            return String_Utils.Protect
              (Base_Name (File_Information (Context)),
               Protect_Quotes => Quoted);
         elsif Param = "fk" then
            return String_Utils.Protect
              (Krunch (Base_Name (File_Information (Context))),
               Protect_Quotes => Quoted);
         else
            return String_Utils.Protect
              (To_Remote (Full_Name (File_Information (Context)).all, Server),
               Protect_Quotes => Quoted);
         end if;

      elsif Param = "d" then
         return String_Utils.Protect
           (To_Remote (Directory_Information (Context), Server),
            Protect_Quotes => Quoted);

      elsif Param = "dk" then
         return String_Utils.Protect
           (Krunch (To_Remote (Directory_Information (Context), Server)),
            Protect_Quotes => Quoted);

      elsif Param = "e" then
         Entity := Get_Entity (Context);
         if Entity /= null then
            --  Get the name from the context, to have the proper casing
            return Entity_Name_Information (Context);
         end if;

      elsif Param = "ek" then
         Entity := Get_Entity (Context);
         if Entity /= null then
            --  Get the name from the context, to have the proper casing
            return Krunch (Entity_Name_Information (Context));
         end if;

      elsif Param = "l" then
         return Image (Line_Information (Context));

      elsif Param = "c" then
         return Image (Integer (Column_Information (Context)));

      elsif Param = "a" then
         return Category_Information (Context);

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
            return Project_Name (Importing_Project_Information (Context));
         end if;

      elsif Param = "ek" then
         return String_Utils.Protect
           (Krunch (Entity_Name_Information (Context)));

      elsif Param (Param'First) = 'P' or else Param (Param'First) = 'p' then
         Project := Project_From_Param (Param, Context);

         if Param = "pps" or else Param = "PPs" then
            if Project = No_Project then
               return "";
            else
               return String_Utils.Protect
                 ("-P"
                  & To_Remote (Full_Name (Project_Path (Project)).all, Server),
                  Protect_Spaces => not Quoted,
                  Protect_Quotes => Quoted);
            end if;
         end if;

         if Project = No_Project then
            raise Invalid_Substitution;
         end if;

         if Param = "p" or else Param = "P" then
            return String_Utils.Protect
              (Project_Name (Project), Protect_Quotes => Quoted);

         elsif Param = "pp" or else Param = "PP" then
            return String_Utils.Protect
              (To_Remote (Full_Name (Project_Path (Project)).all, Server),
               Protect_Quotes => Quoted);

         else
            Recurse := Param (Param'First + 1) = 'r';

            if Recurse then
               Index := Param'First + 2;
            else
               Index := Param'First + 1;
            end if;

            if Index <= Param'Last then
               List_Dirs    := Param (Index) = 'd';
               List_Sources := Param (Index) = 's';

               if Index < Param'Last and then Param (Index + 1) = 'f' then
                  --  Append the list to a file.
                  declare
                     use GNAT.Strings;

                     File       : File_Type;
                     Files_List : File_Array_Access;
                     List       : String_List_Access;

                  begin
                     Create (File);

                     if List_Dirs then
                        List := Source_Dirs (Project, Recurse);
                        if List /= null then
                           for K in List'Range loop
                              Put_Line (File,
                                        To_Remote (List (K).all, Server));
                           end loop;
                           Free (List);
                        end if;
                     end if;

                     if List_Sources then
                        Files_List := Get_Source_Files (Project, Recurse);
                        if Files_List /= null then
                           for K in Files_List'Range loop
                              Put_Line
                                (File,
                                 To_Remote
                                   (Full_Name (Files_List (K)).all, Server));
                           end loop;
                           Unchecked_Free (Files_List);
                        end if;
                     end if;

                     declare
                        N : constant String := To_Remote (Name (File), Server);
                     begin
                        Close (File);
                        return String_Utils.Protect
                          (N, Protect_Quotes => Quoted);
                     end;
                  end;

               else
                  declare
                     use GNAT.Strings;

                     Result : Unbounded_String;
                     List   : String_List_Access;
                     Files_List : File_Array_Access;
                  begin
                     if List_Dirs then
                        List := Source_Dirs (Project, Recurse);
                        if List /= null then
                           for K in List'Range loop
                              Append (Result, '"' &
                                      To_Remote (List (K).all, Server) &
                                      """ ");
                           end loop;
                           Free (List);
                        end if;
                     end if;

                     if List_Sources then
                        Files_List := Get_Source_Files (Project, Recurse);
                        if Files_List /= null then
                           for K in Files_List'Range loop
                              Append
                                (Result,
                                '"' &
                                 To_Remote (Full_Name (Files_List (K)).all,
                                            Server) &
                                 """ ");
                           end loop;
                           Unchecked_Free (Files_List);
                        end if;
                     end if;

                     return String_Utils.Protect
                       (To_String (Result), Protect_Quotes => Quoted);
                  end;
               end if;
            end if;
         end if;
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
      Context : GPS.Kernel.Selection_Context) return Project_Type
   is
      Project : Project_Type := No_Project;
   begin
      if Param (Param'First) = 'P' then
         Project := Get_Project (Get_Kernel (Context));

      elsif Has_Project_Information (Context) then
         Project := Project_Information (Context);

      elsif Has_File_Information (Context) then
         --  Since the editor doesn't provide the project, we emulate it
         --  here
         Project := Get_Project_From_File
           (Project_Registry (Get_Registry (Get_Kernel (Context)).all),
            File_Information (Context),
            Root_If_Not_Found => False);
      end if;

      return Project;
   end Project_From_Param;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Macro_Filter_Record;
      Context : Selection_Context) return Boolean
   is
      Project : Project_Type;
      Is_Entity_Context : constant Boolean :=
        Has_Entity_Name_Information (Context);
      Is_Area_Context : constant Boolean := Has_Area_Information (Context);
      Entity  : Entity_Information;
      Start, Last : Integer;
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
         Entity := Get_Entity (Context);
         if Entity = null then
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
        and then not Has_Category_Information (Context)
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
