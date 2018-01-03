------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with GPS.Editors; use GPS.Editors;

package body LAL.Unit_Providers is

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Unit_Provider;
      Context     : Libadalang.Analysis.Analysis_Context;
      Name        : Wide_Wide_String;
      Kind        : Libadalang.Analysis.Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False)
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

      Buffer : constant Editor_Buffer'Class :=
        Provider.Kernel.Get_Buffer_Factory.Get
          (File        => GNATCOLL.VFS.Create (File),
           Open_Buffer => False,
           Open_View   => False);
   begin
      if Buffer = Nil_Editor_Buffer then

         return Libadalang.Analysis.Get_From_File
           (Context     => Context,
            Filename    => String (File),
            Charset     => Charset,
            Reparse     => Reparse);
      else

         return Libadalang.Analysis.Get_From_Buffer
           (Context     => Context,
            Filename    => String (File),
            Buffer      => Buffer.Get_Chars,
            Charset     => Charset);
      end if;
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
