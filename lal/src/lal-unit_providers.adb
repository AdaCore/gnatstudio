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
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Unit_Provider;
      Context     : Libadalang.Analysis.Analysis_Context;
      Node        : Libadalang.Analysis.Ada_Node;
      Kind        : Libadalang.Analysis.Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False)
      return Libadalang.Analysis.Analysis_Unit is
   begin
      case Node.Kind is
         when Libadalang.Analysis.Ada_Identifier |
              Libadalang.Analysis.Ada_String_Literal =>

            declare
               Token : constant Libadalang.Analysis.Token_Type :=
                 Node.Token_Start;
            begin
               return Provider.Get_Unit
                 (Context     => Context,
                  Name        => Libadalang.Analysis.Text (Token),
                  Kind        => Kind,
                  Charset     => Charset,
                  Reparse     => Reparse,
                  With_Trivia => With_Trivia);
            end;

         when Libadalang.Analysis.Ada_Dotted_Name =>
            declare
               use Ada.Strings.Wide_Wide_Unbounded;
               Image : Unbounded_Wide_Wide_String;
            begin
               for Token of Node.Token_Range loop
                  Append (Image, Libadalang.Analysis.Text (Token));
               end loop;

               return Provider.Get_Unit
                 (Context     => Context,
                  Name        => To_Wide_Wide_String (Image),
                  Kind        => Kind,
                  Charset     => Charset,
                  Reparse     => Reparse,
                  With_Trivia => With_Trivia);
            end;

         when others =>
            raise Constraint_Error;

      end case;
   end Get_Unit;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Unit_Provider;
      Context     : Libadalang.Analysis.Analysis_Context;
      Name        : Wide_Wide_String;
      Kind        : Libadalang.Analysis.Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False)
      return Libadalang.Analysis.Analysis_Unit
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

      return Libadalang.Analysis.Get_From_File
        (Context     => Context,
         Filename    => String (File),
         Charset     => Charset,
         Reparse     => Reparse,
         With_Trivia => With_Trivia);
   end Get_Unit;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Unit_Provider'Class;
      Kernel : GPS.Core_Kernels.Core_Kernel) is
   begin
      Self.Kernel := Kernel;
   end Initialize;

end LAL.Unit_Providers;
