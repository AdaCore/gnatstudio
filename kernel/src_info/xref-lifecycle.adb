------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2016, AdaCore                     --
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

with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Strings;              use GNAT.Strings;
with GNAT.SHA1;                 use GNAT.SHA1;

with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package body Xref.Lifecycle is

   Me : constant Trace_Handle := Create ("Xref");

   Work_In_Memory : constant Boolean := False;
   --  Whether to work in memory
   --  ??? This is work in progress

   procedure Open_Database (Self   : General_Xref_Database;
                            Tree   : Project_Tree_Access);
   --  Open the database connection

   Force_Local_Database : constant Trace_Handle := Create
     ("FORCE_LOCAL_DB", Off);
   --  Whether to use a DB in the temporary directory

   -------------------
   -- Open_Database --
   -------------------

   procedure Open_Database
     (Self   : General_Xref_Database; Tree   : Project_Tree_Access)
   is
      Working_Xref_File : Virtual_File;

      Error : GNAT.Strings.String_Access;
   begin
      Self.Working_Xref_Db := GNATCOLL.VFS.No_File;
      Working_Xref_File := Xref_Database_Location (Self);

      Self.Xref_Db_Is_Temporary := Tree.Status /= From_File;

      Trace (Me, "Set up xref database: " &
             (+Working_Xref_File.Full_Name.all));

      if Self.Disable_SQL_Queries then
         --  Just to avoid errors because we are accessing a non-existing db
         Self.Xref.Setup_DB
           (DB    => GNATCOLL.SQL.Sqlite.Setup
              (Database => ":memory:",
               Errors   => Self.Errors),
            Tree  => Tree,
            Error => Error);
      else
         declare
            Errors : Boolean;
            pragma Unreferenced (Errors);
         begin
            if Work_In_Memory then
               Self.Xref.Setup_DB
                 (DB    => GNATCOLL.SQL.Sqlite.Setup
                    (Database => ":memory:",
                     Errors   => Self.Errors),
                  Tree  => Tree,
                  Error => Error);

               --  Load_From_File
               --    (Self.Xref, +Working_Xref_File.Full_Name.all);
               --  ??? temporary.

            else
               Self.Xref.Setup_DB
                 (DB    => GNATCOLL.SQL.Sqlite.Setup
                    (Database => +Working_Xref_File.Full_Name.all,
                     Errors   => Self.Errors),
                  Tree  => Tree,
                  Error => Error);
            end if;
         exception
            when E : others =>
               --  Catch a corrupted database here and stop propagating
               --  the exception, so as not to block the project loading,
               --  the splash screen, etc
               Trace (Me, "Exception received in Setup_DB: "
                      & Exception_Information (E));
         end;
      end if;

      --  Not interested in schema version errors, gnatinspect already
      --  displays them on the console
      Free (Error);
   end Open_Database;

   --------------------
   -- Close_Database --
   --------------------

   procedure Close_Database (Self   : General_Xref_Database) is
      Success : Boolean;
   begin
      Trace (Me, "Closing xref database, temporary="
             & Self.Xref_Db_Is_Temporary'Img);
      Self.Xref.Free;

      --  If we were already working on a database, first copy the working
      --  database to the database saved between sessions, for future use

      if Self.Xref_Db_Is_Temporary then
         --  This database does not need saving, so we are deleting it
         Trace (Me, "Database was temporary, not saving");

         if Self.Working_Xref_Db /= No_File then
            Self.Working_Xref_Db.Delete (Success);

            if not Success then
               Trace
                 (Me, "Warning: could not delete temporary database file");
            end if;
         end if;
      end if;
   end Close_Database;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed (Self : General_Xref_Database) is
      Error : GNAT.Strings.String_Access;
   begin
      --  Create an initial empty database. It will never be filled, and
      --  will be shortly replaced in Project_View_Changed, but it ensures
      --  that GPS does not raise exceptions if some action is performed
      --  while the project has not been computed (like loading of the
      --  desktop for instance).
      --  ??? We really should not be doing anything until the project has
      --  been computed.

      if Self.Xref /= null then
         Trace (Me, "Closing previous version of the database");
         Close_Database (Self);
      end if;

      Trace (Me, "Set up xref database: :memory:");
      Self.Working_Xref_Db := GNATCOLL.VFS.No_File;
      Self.Xref_Db_Is_Temporary := True;
      Self.Xref.Setup_DB
        (DB    => GNATCOLL.SQL.Sqlite.Setup
           (Database => ":memory:",
            Errors   => Self.Errors),
         Tree  => Self.Registry.Tree,
         Error => Error);

      --  not interested in schema version errors, gnatinspect will
      --  already display those for the user.
      Free (Error);
   end Project_Changed;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed
     (Self   : General_Xref_Database;
      Tree   : Project_Tree_Access)
   is
   begin
      if Self.Xref /= null then
         Trace (Me, "Closing previous version of the database");
         Close_Database (Self);
      end if;

      --  Self.Xref was initialized in Project_Changed.
      Self.Xref.Free;
      Self.Working_Xref_Db := No_File;

      Open_Database (Self, Tree);

      --  ??? Now would be a good opportunity to update the cross-references
      --  rather than wait for the next compilation.
   end Project_View_Changed;

   ----------------------------
   -- Xref_Database_Location --
   ----------------------------

   function Xref_Database_Location
     (Self    : not null access General_Xref_Database_Record'Class)
      return GNATCOLL.VFS.Virtual_File
   is
      Dir  : Virtual_File;
   begin
      if Self.Working_Xref_Db = GNATCOLL.VFS.No_File then
         declare
            Project : constant Project_Type := Self.Registry.Tree.Root_Project;
            Attr : constant String :=
              Project.Attribute_Value
                (Build ("IDE", "Xref_Database"),
                 Default => "",
                 Use_Extended => True);
         begin
            if Attr = "" then
               if Active (Force_Local_Database) then
                  declare
                     Hash : constant String := GNAT.SHA1.Digest
                       (+Project.Project_Path.Full_Name (Normalize => True));
                  begin
                     Self.Working_Xref_Db := Create_From_Dir
                       (Dir  => Get_Tmp_Directory,
                        Base_Name => +("gnatinspect-" & Hash & ".db"));
                  end;
               else
                  Dir    := Project.Object_Dir;

                  if Dir = No_File then
                     Trace (Me, "Object_Dir is unknown for the root project "
                            & Project.Project_Path.Display_Full_Name);
                     Dir := GNATCOLL.VFS.Get_Current_Dir;
                  end if;

                  Self.Working_Xref_Db := Create_From_Dir
                    (Dir        => Dir,
                     Base_Name => +("gnatinspect.db"));
               end if;
            else
               Self.Working_Xref_Db := Create_From_Base
                 (Base_Name => +Attr,
                  Base_Dir  => Project.Project_Path.Dir_Name);
            end if;

            Trace
              (Me, "project db file: " &
                 Self.Working_Xref_Db.Display_Full_Name);

            Self.Disable_SQL_Queries :=
              not Create (Self.Working_Xref_Db.Dir_Name).Is_Writable
              or else
                (Self.Working_Xref_Db.Is_Regular_File
                 and then not Self.Working_Xref_Db.Is_Writable);
         end;
      end if;

      return Self.Working_Xref_Db;
   end Xref_Database_Location;

end Xref.Lifecycle;
