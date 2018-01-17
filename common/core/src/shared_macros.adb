------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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

--  This package provides handling of the expansion of  %f %F %fk %gnatmake
--  %O %pp %PP %Pb macros based on File_Information & Project parameters.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with String_Utils;            use String_Utils;
with GNATCOLL.Templates;      use GNATCOLL.Templates;

package body Shared_Macros is

   function Shared_Macros_Substitute
     (Project_From_Kernel : Project_Type;
      Project_From_Param  : Project_Type;
      File_Information    : Virtual_File;
      Param               : String;
      Quoted              : Boolean;
      Done                : access Boolean;
      Server              : Server_Type := GPS_Server;
      For_Shell           : Boolean := False) return String
   is
      Project                          : Project_Type := No_Project;
      Recurse, List_Dirs, List_Sources : Boolean;
      Index                            : Integer;

      --  In this routine it is important to *not* quote backslahes on paths.
      --  This is important because on Windows a backslash is the directory
      --  separator and we do not want to escape it. Doing so will work in most
      --  cases except for remote directory (\\server\drive). Having 4
      --  backslashes at the start of the PATH is not recognized by Windows.

   begin
      Done.all := True;

      if Param = "f" then
         if File_Information /= No_File then
            return String_Utils.Protect
              (+File_Information.Base_Name,
               Protect_Quotes      => Quoted,
               Protect_Backslashes => For_Shell);
         end if;

      elsif Param = "fd" then
         if File_Information /= No_File then
            return String_Utils.Protect
              (+File_Information.Dir_Name,
               Protect_Quotes      => Quoted,
               Protect_Backslashes => For_Shell);
         end if;

      elsif Param = "fk" then
         if File_Information /= No_File then
            return String_Utils.Protect
              (Krunch (+File_Information.Base_Name),
               Protect_Quotes      => Quoted,
               Protect_Backslashes => For_Shell);
         end if;

      elsif Param = "F" then
         if File_Information /= No_File then
            return String_Utils.Protect
              (+To_Remote
                 (File_Information,
                  Get_Nickname (Server)).Full_Name,
               Protect_Quotes      => Quoted,
               Protect_Backslashes => For_Shell);
         end if;

      elsif Param = "gnatmake" then
         return Project_From_Kernel.Attribute_Value
                  (Compiler_Command_Attribute,
                   Default => "gnatmake",
                   Index   => "Ada");

      elsif Param = "o" or else Param = "O" then
         return String_Utils.Protect
           (String (Full_Name (Object_Dir
              (Project_From_Param)).all),
            Protect_Quotes      => Quoted,
            Protect_Backslashes => For_Shell);

      elsif Param (Param'First) = 'P' or else Param (Param'First) = 'p' then
         Project := Project_From_Param;

         if Param = "pps" or else Param = "PPs" then
            if Project = No_Project then
               return "";

            else
               return "-P" &
               String_Utils.Protect
                 (+To_Remote (Project.Project_Path,
                     Get_Nickname (Server)).Full_Name,
                  Protect_Quotes      => Quoted,
                  Protect_Backslashes => For_Shell);
            end if;
         end if;

         if Project = No_Project then
            raise Invalid_Substitution;
         end if;

         if Param = "p" or else Param = "P" then
            return String_Utils.Protect
              (Project.Name,
               Protect_Quotes      => Quoted,
               Protect_Backslashes => For_Shell);

         elsif Param = "Pl" then
            return String_Utils.Protect
              (To_Lower (Project.Name),
               Protect_Quotes      => Quoted,
               Protect_Backslashes => For_Shell);

         elsif Param = "Pb" then
            declare
               Name      : constant String :=
                 String (Project_Path (Project).Base_Name);
               Extension : constant String :=
                 String (Project_Path (Project).File_Extension);

            begin
               return Name (Name'First .. Name'Last - Extension'Length);
            end;

         elsif Param = "pp" or else Param = "PP" then
            return String_Utils.Protect
              (+To_Remote
                 (Project.Project_Path,
                  Get_Nickname (Server)).Full_Name,
               Protect_Quotes      => Quoted,
               Protect_Backslashes => For_Shell);

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
                  --  Append the list to a file
                  declare
                     File       : File_Type;
                     Files_List : File_Array_Access;

                  begin
                     Create (File);

                     if List_Dirs then
                        declare
                           List : constant File_Array :=
                             Project.Source_Dirs (Recurse);
                        begin
                           for K in List'Range loop
                              Put_Line
                                (File,
                                 +To_Remote
                                   (List (K),
                                    Get_Nickname (Server)).Full_Name);
                           end loop;
                        end;

                     elsif List_Sources then
                        Files_List := Project.Source_Files (Recurse);
                        if Files_List /= null then
                           for K in Files_List'Range loop
                              Put_Line
                                (File,
                                 +To_Remote
                                   (Files_List (K),
                                    Get_Nickname (Server)).Full_Name);
                           end loop;
                           Unchecked_Free (Files_List);
                        end if;
                     end if;

                     declare
                        N : constant Virtual_File :=
                              To_Remote
                                (Create (+Name (File)),
                                 Get_Nickname (Server));
                     begin
                        Close (File);
                        return String_Utils.Protect
                          (+N.Full_Name,
                           Protect_Quotes      => Quoted,
                           Protect_Backslashes => For_Shell);
                     end;
                  end;

               else
                  declare
                     Result     : Unbounded_String;
                     Files_List : File_Array_Access;
                  begin
                     if List_Dirs then
                        declare
                           List : constant File_Array :=
                             Project.Source_Dirs (Recurse);
                        begin
                           for K in List'Range loop
                              Append
                                (Result, '"' &
                                   (+To_Remote
                                      (List (K),
                                       Get_Nickname (Server)).Full_Name) &
                                 """ ");
                           end loop;
                        end;

                     elsif List_Sources then
                        Files_List := Project.Source_Files (Recurse);
                        if Files_List /= null then
                           for K in Files_List'Range loop
                              Append
                                (Result,
                                '"' &
                                 (+To_Remote
                                      (Files_List (K),
                                       Get_Nickname (Server)).Full_Name) &
                                 """ ");
                           end loop;
                           Unchecked_Free (Files_List);
                        end if;
                     end if;

                     return String_Utils.Protect
                       (To_String (Result),
                        Protect_Quotes      => Quoted,
                        Protect_Backslashes => For_Shell);
                  end;
               end if;
            end if;
         end if;
      end if;

      --  No substitution
      Done.all := False;
      return "";
   end Shared_Macros_Substitute;

end Shared_Macros;
