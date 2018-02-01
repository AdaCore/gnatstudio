------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with Commands.Generic_Asynchronous;
with Commands;                use Commands;
with GPS.Editors;             use GPS.Editors;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Kernel;              use GPS.Kernel;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

package body Refactoring.Performers is
   Me : constant Trace_Handle := Create ("GPS.REFACTORING.PERFORMERS");
   use Location_Arrays;

   type Renaming_Error_Record is new File_Error_Reporter_Record with
      record
         No_LI_List : Source_File_Set;
      end record;
   type Renaming_Error is access all Renaming_Error_Record'Class;
   overriding procedure Error
     (Report : in out Renaming_Error_Record; File : Virtual_File);

   type Get_Locations_Data is record
      Refs                : Location_Arrays.List;
      Stale_LI_List       : Source_File_Set;
      Read_Only_Files     : Source_File_Set;
      On_Completion       : Refactor_Performer;
      Kernel              : Kernel_Handle;
      Entity              : Root_Entity_Ref;
      Iter                : Root_Reference_Iterator_Ref;
      Errors              : Renaming_Error;
      Make_Writable       : Boolean;
   end record;
   --  Extra_Entities is the list of entities that are also impacted by the
   --  refactoring

   procedure Free (Data : in out Get_Locations_Data);
   package Get_Locations_Commands is new Commands.Generic_Asynchronous
     (Get_Locations_Data, Free);
   use Get_Locations_Commands;
   --  Commands used to search for all occurrences in the background, and
   --  perform some refactoring afterwards

   procedure Find_Next_Location
     (Data    : in out Get_Locations_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Find the next location, and stores it in Data

   procedure On_End_Of_Search (Data : Get_Locations_Data);
   --  Called when all the related files have been searched and the refactoring
   --  should be performed.

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Get_Locations_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Refactor_Performer_Record'Class, Refactor_Performer);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Renaming_Error_Record'Class, Renaming_Error);
   begin
      Data.Errors.No_LI_List.Clear;
      Data.Stale_LI_List.Clear;
      Destroy (Data.Iter.Reference);
      if Data.On_Completion /= null then
         Free (Data.On_Completion.all);
         Unchecked_Free (Data.On_Completion);
      end if;
      Unchecked_Free (Data.Errors);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Factor : in out Refactor_Performer_Record) is
      pragma Unreferenced (Factor);
   begin
      null;
   end Free;

   -----------
   -- Error --
   -----------

   overriding procedure Error
     (Report : in out Renaming_Error_Record; File : Virtual_File) is
   begin
      Report.No_LI_List.Include (File);
   end Error;

   -----------------------
   -- Get_All_Locations --
   -----------------------

   procedure Get_All_Locations
     (Kernel          : access Kernel_Handle_Record'Class;
      Entity          : Root_Entity'Class;
      On_Completion   : access Refactor_Performer_Record'Class;
      Auto_Compile    : Boolean := False;
      Overridden      : Boolean := True;
      Make_Writable   : Boolean := False;
      Background_Mode : Boolean := True)
   is
      pragma Unreferenced (Auto_Compile);
      Data   : Get_Locations_Data;
      C      : Get_Locations_Commands.Generic_Asynchronous_Command_Access;
      Result : Command_Return_Type;
   begin
      Data.On_Completion := Refactor_Performer (On_Completion);
      Data.Kernel        := Kernel_Handle (Kernel);
      Data.Errors        := new Renaming_Error_Record;
      Data.Make_Writable := Make_Writable;

      Data.Entity.Replace_Element (Entity);
      Data.Iter.Replace_Element
        (Find_All_References
           (Entity                => Entity,
            Include_Renames       => False,
            Include_Overriding    => Overridden,
            Include_Overridden    => Overridden));

      Create (C, -"Refactoring", Data, Find_Next_Location'Access);
      Set_Progress
        (Command_Access (C),
         (Running,
          Get_Current_Progress (Data.Iter.Element),
          Get_Total_Progress   (Data.Iter.Element)));

      if Background_Mode then
         Launch_Background_Command
           (Kernel, Command_Access (C), True, True, "Refactoring");
      else
         loop
            Result := Execute (C);
            exit when Result /= Execute_Again;
         end loop;
         On_End_Of_Search (Data);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Free (Data);
   end Get_All_Locations;

   ----------------------
   -- On_End_Of_Search --
   ----------------------

   procedure On_End_Of_Search (Data : Get_Locations_Data) is
      use Source_File_Sets;
      Confirmed : Boolean;
      C : Source_File_Sets.Cursor;
   begin
      if Data.Make_Writable then
         Confirmed := Confirm_Files
           (Data.Kernel,
            Source_File_Sets.Empty_Set,
            Data.Errors.No_LI_List,
            Data.Stale_LI_List);
      else
         Confirmed := Confirm_Files
           (Data.Kernel,
            Data.Read_Only_Files,
            Data.Errors.No_LI_List,
            Data.Stale_LI_List);
      end if;

      if Confirmed then
         if Data.Make_Writable then
            C := Data.Read_Only_Files.First;
            while Has_Element (C) loop
               Get_Buffer_Factory (Data.Kernel)
                 .Get (Element (C)).Open.Set_Read_Only (False);
               Next (C);
            end loop;
         end if;

         Execute
           (Data.On_Completion,
            Data.Kernel,
            Data.Entity.Element,
            Data.Refs,
            Data.Errors.No_LI_List,
            Data.Stale_LI_List);
      end if;
   end On_End_Of_Search;

   ------------------------
   -- Find_Next_Location --
   ------------------------

   procedure Find_Next_Location
     (Data    : in out Get_Locations_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Loc    : General_Location;
   begin
      if At_End (Data.Iter.Element) then
         On_End_Of_Search (Data);
         Result := Success;
         return;
      end if;

      declare
         Ref : constant Root_Entity_Reference'Class := Get (Data.Iter.Element);
      begin

         if Ref /= No_Root_Entity_Reference then
            Loc := Get_Location (Ref);

            if Data.Kernel.Databases.Is_Up_To_Date (Loc.File) then
               Append (Data.Refs,
                       (File    => Loc.File,
                        Project_Path => Loc.Project_Path,
                        Line    => Loc.Line,
                        Column  => Loc.Column));

               --  If we have duplicates, they will always come one after the
               --  other. So we just have to check the previous one.
            else
               Append (Data.Refs,
                       (File    => Loc.File,
                        Project_Path => Loc.Project_Path,
                        Line    => Loc.Line,
                        Column  => Loc.Column));
               Data.Stale_LI_List.Include (Loc.File);
            end if;

            if not Loc.File.Is_Writable then
               Data.Read_Only_Files.Include (Loc.File);
            end if;

            Next (Data.Iter.Reference);

            Set_Progress (Command,
                          (Running,
                           Get_Current_Progress (Data.Iter.Element),
                           Get_Total_Progress (Data.Iter.Element)));
            Result := Execute_Again;

         else
            Next (Data.Iter.Reference);
            Result := Execute_Again;
         end if;
      end;
   end Find_Next_Location;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Kernel    : access Kernel_Handle_Record'Class;
      From_File : GNATCOLL.VFS.Virtual_File;
      Line      : Integer;
      Column    : Visible_Column_Type;
      Length    : Integer) return String
   is
      Editor : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (From_File);
      Loc_Start : constant Editor_Location'Class := Editor.New_Location
        (Line, Column);
      Loc_End   : constant Editor_Location'Class :=
        Loc_Start.Forward_Char (Length - 1);
      Text : constant String := Editor.Get_Chars (Loc_Start, Loc_End);
   begin
      return Text;
   end Get_Text;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
     (Kernel     : access Kernel_Handle_Record'Class;
      In_File    : GNATCOLL.VFS.Virtual_File;
      Line_Start : Integer;
      Line_End   : Integer)
   is
      Editor : constant Editor_Buffer'Class :=
        Get_Buffer_Factory (Kernel).Get (In_File);
      Loc_Start : constant Editor_Location'Class := Editor.New_Location_At_Line
        (Line_Start);
      Loc_End   : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Line_End).End_Of_Line;
   begin
      --  ??? Removing the final newline (Loc_End.Forward_Char(1)) results in
      --  removing the first char of the next line
      Editor.Delete (Loc_Start, Loc_End);
   end Delete_Text;

end Refactoring.Performers;
