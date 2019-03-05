------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with GPS.Editors; use GPS.Editors;

package body LAL.Unit_Providers is

   function Get_Unit_Filename
     (Self : Unit_Provider'Class;
      Name : Wide_Wide_String;
      Kind : Libadalang.Common.Analysis_Unit_Kind)
      return GNATCOLL.VFS.Filesystem_String;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   function Get_Unit_Filename
     (Self : Unit_Provider'Class;
      Name : Wide_Wide_String;
      Kind : Libadalang.Common.Analysis_Unit_Kind)
      return GNATCOLL.VFS.Filesystem_String
   is
      Map : constant array (Libadalang.Common.Analysis_Unit_Kind) of
        GNATCOLL.Projects.Unit_Parts :=
          (Libadalang.Common.Unit_Specification =>
             GNATCOLL.Projects.Unit_Spec,
           Libadalang.Common.Unit_Body =>
             GNATCOLL.Projects.Unit_Body);

      Unit_Name : constant String :=
        Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Name);

      File : constant GNATCOLL.VFS.Filesystem_String :=
        Self.Kernel.Get_Project_Tree.Root_Project.File_From_Unit
          (Unit_Name => Unit_Name,
           Part      => Map (Kind),
           Language  => "Ada");
   begin
      if File'Length = 0 then
         return "";
      end if;

      return Self.Kernel.Get_Project_Tree.Create (File).Full_Name;
   end Get_Unit_Filename;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Self : Unit_Provider;
      Name : Wide_Wide_String;
      Kind : Libadalang.Common.Analysis_Unit_Kind) return String
   is
      File : constant GNATCOLL.VFS.Filesystem_String :=
        Get_Unit_Filename (Self, Name, Kind);
   begin
      return String (File);
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Self    : Unit_Provider;
      Context : Libadalang.Analysis.Analysis_Context'Class;
      Name    : Wide_Wide_String;
      Kind    : Libadalang.Common.Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False)
      return Libadalang.Analysis.Analysis_Unit'Class
   is
      File : constant GNATCOLL.VFS.Filesystem_String :=
        Get_Unit_Filename (Self, Name, Kind);

      Buffer : constant Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get
          (File        => GNATCOLL.VFS.Create (File),
           Open_Buffer => False,
           Open_View   => False);

      Text   : Ada.Strings.Unbounded.String_Access;
      Result : Libadalang.Analysis.Analysis_Unit;
   begin
      if File'Length = 0 or else Buffer = Nil_Editor_Buffer then

         Result := Libadalang.Analysis.Get_From_File
           (Context     => Context,
            Filename    => String (File),
            Charset     => Charset,
            Reparse     => Reparse);
      else

         Text := new String'(Buffer.Get_Chars);

         Result := Libadalang.Analysis.Get_From_Buffer
              (Context     => Context,
               Filename    => String (File),
               Buffer      => Text.all,
               Charset     => "UTF-8");

         Ada.Strings.Unbounded.Free (Text);
      end if;

      return Result;
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
