------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

package body LAL.Unit_Providers is

   --------------
   -- Get_File --
   --------------

   overriding function Get_File
     (Provider : Unit_File_Provider;
      Node     : Libadalang.Analysis.Ada_Node;
      Kind     : Libadalang.Analysis.Unit_Kind) return String is
   begin
      case Node.Kind is
         when Libadalang.Analysis.Ada_Identifier |
              Libadalang.Analysis.Ada_String_Literal =>

            declare
               Token : constant Libadalang.Analysis.Token_Type :=
                 Node.Token_Start;
            begin
               return Provider.Get_File
                 (Libadalang.Analysis.Text (Token), Kind);
            end;

         when Libadalang.Analysis.Ada_Dotted_Name =>
            declare
               use Ada.Strings.Wide_Wide_Unbounded;
               Image : Unbounded_Wide_Wide_String;
            begin
               for Token of Node.Token_Range loop
                  Append (Image, Libadalang.Analysis.Text (Token));
               end loop;

               return Provider.Get_File
                 (To_Wide_Wide_String (Image), Kind);
            end;

         when others =>
            raise Constraint_Error;

      end case;
   end Get_File;

   --------------
   -- Get_File --
   --------------

   overriding function Get_File
     (Provider : Unit_File_Provider;
      Name     : Wide_Wide_String;
      Kind     : Libadalang.Analysis.Unit_Kind) return String
   is
      Map : constant array (Libadalang.Analysis.Unit_Kind) of
        GNATCOLL.Projects.Unit_Parts :=
          (Libadalang.Analysis.Unit_Specification =>
             GNATCOLL.Projects.Unit_Spec,
           Libadalang.Analysis.Unit_Body =>
             GNATCOLL.Projects.Unit_Body);

      Unit_Name : constant String :=
        Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Name);

      File : constant GNATCOLL.VFS.Filesystem_String :=
        Provider.Kernel.Get_Project_Tree.Root_Project.File_From_Unit
          (Unit_Name => Unit_Name,
           Part      => Map (Kind),
           Language  => "Ada");
   begin
      return String (File);
   end Get_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Unit_File_Provider'Class;
      Kernel : GPS.Core_Kernels.Core_Kernel) is
   begin
      Self.Kernel := Kernel;
   end Initialize;

end LAL.Unit_Providers;
