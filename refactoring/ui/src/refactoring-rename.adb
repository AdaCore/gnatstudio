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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Gtk.Stock;                  use Gtk.Stock;

with GPS.Editors;                use GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel;                 use GPS.Kernel;
with Refactoring.UI;             use Refactoring.UI;
with Refactoring.Performers;     use Refactoring.Performers;
with Refactoring.Services;       use Refactoring.Services;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

with Commands;                   use Commands;
with Xref;                       use Xref;

package body Refactoring.Rename is
   Me : constant Trace_Handle := Create ("GPS.REFACTORING.RENAME");

   use Location_Arrays;

   Name_Cst               : aliased constant String := "name";
   Include_Overriding_Cst : aliased constant String := "include_overriding";
   Make_Writable_Cst      : aliased constant String := "make_writable";
   Auto_Save_Cst          : aliased constant String := "auto_save";

   type Renaming_Performer_Record is new Refactor_Performer_Record with record
       Auto_Save   : Boolean;
       Old_Name    : Unbounded_String;
       New_Name    : Unbounded_String;
   end record;
   type Renaming_Performer is access all Renaming_Performer_Record'Class;
   overriding procedure Execute
     (Factory       : access Renaming_Performer_Record;
      Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Root_Entity'Class;
      Refs          : Location_Arrays.List;
      No_LI_List    : Source_File_Set;
      Stale_LI_List : Source_File_Set);
   --  Implements the "Renaming entity" refactoring

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handling of shell commands

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Factory       : access Renaming_Performer_Record;
      Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Root_Entity'Class;
      Refs          : Location_Arrays.List;
      No_LI_List    : Source_File_Set;
      Stale_LI_List : Source_File_Set)
   is
      pragma Unreferenced (No_LI_List, Stale_LI_List);
      Buffer_Factory : constant Editor_Buffer_Factory_Access :=
        Get_Buffer_Factory (Kernel);
      Name           : constant String := Get_Name (Entity);
      Errors         : Source_File_Set;

      procedure Process_Locations
        (File : Virtual_File;
         Locs : Location_Arrays.List);
      --  Process a list of locations that are in the same file

      -----------------------
      -- Process_Locations --
      -----------------------

      procedure Process_Locations
        (File : Virtual_File;
         Locs : Location_Arrays.List)
      is
         Was_Open : constant Boolean := Buffer_Factory.Get
           (File  => File,
            Force => False, Open_View => False) /= Nil_Editor_Buffer;
         Buffer   : constant Editor_Buffer'Class := Buffer_Factory.Get (File);
         G        : constant Group_Block := Buffer.New_Undo_Group;
      begin
         for Loc of Locs loop
            if not Insert_Text
              (Kernel.Refactoring_Context,
               Loc.File,
               Loc.Line,
               Loc.Column,
               To_String (Factory.New_Name),
               Indent            => False,
               Replaced_Length   => Name'Length,
               Only_If_Replacing => To_String (Factory.Old_Name))
            then
               Create_Simple_Message
                 (Get_Messages_Container (Kernel),
                  (-"Refactoring - rename ") & To_String (Factory.Old_Name)
                  & (-" to ") & To_String (Factory.New_Name),
                  Loc.File,
                  Loc.Line,
                  Loc.Column,
                  -"error, failed to rename entity",
                  Unspecified,
                  Side_And_Locations);

               Errors.Include (Loc.File);

            else
               --  Renaming done, insert entry into locations view

               Create_Simple_Message
                 (Get_Messages_Container (Kernel),
                  (-"Refactoring - rename ") & To_String (Factory.Old_Name)
                  & (-" to ") & To_String (Factory.New_Name),
                  Loc.File,
                  Loc.Line,
                  Loc.Column,
                  -"entity renamed",
                  Unspecified,
                  Side_And_Locations);
            end if;
         end loop;

         if Factory.Auto_Save then
            Buffer.Save (Interactive => False);
            if not Was_Open then
               Buffer.Close;
            end if;
         end if;
      end Process_Locations;

      C : Location_Arrays.Cursor;
      Current_File : Virtual_File := No_File;
      Current_Loc  : General_Location;
      Locations_In_Current_File : Location_Arrays.List;

   begin
      --  We process the list of locations:
      --     - file by file, so that we create one undo/redo group per file
      --     - in reverse order, so that we do not invalidate locations
      --       during the renaming process
      --  The loop below takes care of the grouping

      C := Refs.Last;
      while Has_Element (C) loop
         declare
            Loc : constant General_Location := Element (C);
         begin
            if Loc = Current_Loc then
               --  Skip the same location. This could happen for subprogram
               --  without separate declaration, when the location is
               --  reported twice. Find_Next_Location promises that
               --  duplicates come one after the other.
               null;
            else
               --  Do we have a new file?
               if Current_File = No_File
                 or else Current_File /= Loc.File
               then
                  if Current_File /= No_File then
                     --  We had a file before: process all locations on this
                     --  previous current file
                     Process_Locations
                       (Current_File, Locations_In_Current_File);
                     Locations_In_Current_File.Clear;
                  end if;

                  --  Bump the current file
                  Current_File := Loc.File;
               end if;

               --  Add the location to the list of locs for the current file
               Locations_In_Current_File.Append (Loc);
               Current_Loc := Loc;
            end if;
         end;
         Previous (C);
      end loop;

      --  We need to catch the leftovers from the loop above

      if Current_File /= No_File then
         Process_Locations (Current_File, Locations_In_Current_File);
      end if;

      --  The calls to Process_Locations above might have generated entries
      --  in the Errors list. Process this now.

      if not Errors.Is_Empty then
         if not Dialog
           (Kernel,
            Title         => -"References not replaced",
            Msg           =>
            -("Some references could not be replaced because one or more files"
              & " were already modified"),
            Files         => Errors,
            Execute_Label => Gtk.Stock.Stock_Ok,
            Cancel_Label  => Gtk.Stock.Stock_Undo)
         then
            declare
               Filenames   : File_Array (1 .. Integer (Refs.Length));
               First_Empty : Positive := 1;
               Found       : Boolean;
               The_File    : Virtual_File := No_File;
            begin
               for Loc of Refs loop
                  if The_File = No_File
                     or else The_File /= Loc.File
                  then
                     The_File := Loc.File;

                     --  We do not want to undo with No_File, since the
                     --  call to Get below would return the current buffer

                     if The_File /= No_File then

                        --  Check whether this file is already in the array

                        Found := False;
                        for F in 1 .. First_Empty - 1 loop
                           if Filenames (F) = The_File then
                              Found := True;
                              exit;
                           end if;
                        end loop;

                        --  The file is not found: add it

                        if not Found then
                           Filenames (First_Empty) := The_File;
                           First_Empty := First_Empty + 1;
                        end if;
                     end if;
                  end if;

                  --  Undo once for every buffer we have

                  for F in 1 .. First_Empty - 1 loop
                     Buffer_Factory.Get (Filenames (F)).Undo;
                  end loop;
               end loop;
            end;
         end if;
      end if;
   exception
      when E : others =>
         Trace (Me, E);
   end Execute;

   ------------
   -- Rename --
   ------------

   procedure Rename
     (Kernel        : GPS.Kernel.Kernel_Handle;
      Context       : Commands.Interactive.Interactive_Command_Context;
      Old_Name      : Ada.Strings.Unbounded.Unbounded_String;
      New_Name      : Ada.Strings.Unbounded.Unbounded_String;
      Auto_Save     : Boolean;
      Overridden    : Boolean;
      Make_Writable : Boolean)
   is
      Entity : constant Root_Entity'Class := Get_Entity (Context.Context);
   begin
      if Entity = No_Root_Entity then
         return;
      end if;

      if Kernel.Databases.Is_Up_To_Date
        (File_Information (Context.Context))
      then
         declare
            Refactor : constant Renaming_Performer :=
              new Renaming_Performer_Record'
                (Refactor_Performer_Record with
                 Old_Name  => Old_Name,
                 New_Name  => New_Name,
                 Auto_Save => Auto_Save);
         begin
            Get_All_Locations
              (Kernel        => Kernel,
               Entity        => Entity,
               On_Completion => Refactor,
               Overridden    => Overridden,
               Make_Writable => Make_Writable,
               Auto_Compile  => False);
         end;
      else
         Create_Simple_Message
           (Get_Messages_Container (Kernel),
            (-"Refactoring - rename ") & Get_Name (Entity) & " failed",
            File_Information (Context.Context),
            0,
            0,
            -"The navigation information for this file is not up-to-date"
            & " (recompile to regenerate it)",
            Medium,
            Side_And_Locations);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Rename;

   ----------------------------
   -- Entity_Command_Handler --
   ----------------------------

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "rename" then
         Name_Parameters (Data, (1 => Name_Cst'Access,
                                 2 => Include_Overriding_Cst'Access,
                                 3 => Make_Writable_Cst'Access,
                                 4 => Auto_Save_Cst'Access));
         declare
            Entity         : constant Root_Entity'Class := Get_Data (Data, 1);
            Include_Overridding : constant Boolean := Nth_Arg (Data, 3, True);
            Make_Writable       : constant Boolean := Nth_Arg (Data, 4, False);
            Auto_Save           : constant Boolean := Nth_Arg (Data, 5, False);
            Refactor            : constant Renaming_Performer :=
              new Renaming_Performer_Record'
                (Refactor_Performer_Record with
                 New_Name        => To_Unbounded_String
                   (String'(Nth_Arg (Data, 2))),
                 Old_Name        => To_Unbounded_String
                   (Get_Name (Entity)),
                 Auto_Save       => Auto_Save);
         begin
            Get_All_Locations
              (Get_Kernel (Data),
               Entity,
               Refactor,
               Auto_Compile    => False,
               Overridden      => Include_Overridding,
               Make_Writable   => Make_Writable,
               Background_Mode => False);
         end;
      end if;
   end Entity_Command_Handler;

   --------------------------
   -- Register_Refactoring --
   --------------------------

   procedure Register_Refactoring
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Kernel.Scripts.Register_Command
        ("rename", 1, 4, Entity_Command_Handler'Access,
         Get_Entity_Class (Kernel));
   end Register_Refactoring;

end Refactoring.Rename;
