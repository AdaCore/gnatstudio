------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Basic_Types;
with Language.Tree.Database;
with GNATCOLL.VFS;
with GPS.Editors;
with Xref;

package Refactoring is

   ------------------------
   -- Universal_Location --
   ------------------------

   type Universal_Location is private;

   Null_Universal_Location : constant Universal_Location;

   function To_Location
     (File   : Language.Tree.Database.Structured_File_Access;
      Line   : Integer;
      Column : Basic_Types.Visible_Column_Type) return Universal_Location;

   function To_Location
     (File          : Language.Tree.Database.Structured_File_Access;
      Index_In_File : Basic_Types.String_Index_Type) return Universal_Location;

   function Get_File
     (Location : access Universal_Location)
      return Language.Tree.Database.Structured_File_Access;
   function Get_Line (Location : access Universal_Location) return Integer;
   function Get_Column
     (Location : access Universal_Location)
      return Basic_Types.Visible_Column_Type;
   function Get_Index_In_File
     (Location : access Universal_Location)
      return Basic_Types.String_Index_Type;

   function Get_Index_In_Line
     (Location : access Universal_Location)
      return Basic_Types.String_Index_Type;
   --  Return the index of the location in the current line. First index is 1

   procedure Set_Column
     (Location : access Universal_Location;
      Column   : Basic_Types.Visible_Column_Type);
   procedure Set_Index_In_Line
     (Location : access Universal_Location;
      Index    : Basic_Types.String_Index_Type);
   procedure Set_Line_Column
     (Location : access Universal_Location;
      Line     : Integer;
      Column   : Basic_Types.Visible_Column_Type);
   procedure Set_Index_In_File
     (Location : access Universal_Location;
      Index    : Basic_Types.String_Index_Type);

   ---------------------
   -- Factory context --
   ---------------------

   type Factory_Context_Record is tagged record
      Buffer_Factory : GPS.Editors.Editor_Buffer_Factory_Access;
      Db             : Xref.General_Xref_Database;

      Add_Subprogram_Box : Boolean := True;
      --  Whether creating a subprogram body should first insert a subprogram
      --  box

      Add_In_Keyword     : Boolean := False;
      --  Whether adding "in" parameters should explicitly show the "in"
      --  keyword. If False, the keyword is omitted

      Create_Subprogram_Decl : Boolean := True;
      --  Whether to add a subprogram declaration when creating a new
      --  subprogram in a body.
   end record;
   type Factory_Context is access all Factory_Context_Record'Class;
   --  This type groups the common data required by all the
   --  constructors/factories of the various refactoring algorithms.
   --  Among others, its role is to keep all necessary data from the kernel so
   --  that GNATBench does not need to depend on the kernel.
   --  This is a singleton, only one instance is needed in the application.
   --  It has been made a public report for ease of use. In practice, this is a
   --  read-only structure except at creation time.

   procedure Report_Error
     (Self : access Factory_Context_Record;
      Msg  : String) is null;
   --  Report an error message to the user.
   --  The message has *not* been translated, and is in English.

   procedure Report_Location
     (Self     : access Factory_Context_Record;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Basic_Types.Visible_Column_Type := 1;
      Text     : String) is null;
   --  Report a location where a change occurred.
   --  This can then tbe used by the user to quickly navigate across such
   --  locations to review the effects of a refactoring.

   type With_Factory is abstract tagged record
      Context : Factory_Context;
   end record;
   --  Base type for all types that store a factory context. This field is made
   --  public for ease of use and efficiency, rather than go through a
   --  primitive operation.

private

   type Universal_Location is record
      File   : Language.Tree.Database.Structured_File_Access;

      Line          : Integer;
      Column        : Basic_Types.Visible_Column_Type;

      Index_In_Line : Basic_Types.String_Index_Type;
      Index_In_File : Basic_Types.String_Index_Type;

      Is_Column_Computed : Boolean := False;
      Is_Index_In_File_Computed : Boolean := False;
      Is_Index_In_Line_Computed : Boolean := False;
   end record;

   Null_Universal_Location : constant Universal_Location :=
     Universal_Location'
       (File                      => null,
        Line                      => 0,
        Column                    => 0,
        Index_In_Line             => 0,
        Index_In_File             => 0,
        Is_Column_Computed        => False,
        Is_Index_In_File_Computed => False,
        Is_Index_In_Line_Computed => False);

end Refactoring;
