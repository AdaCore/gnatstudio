-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Commands.Generic_Asynchronous;
with Commands;                use Commands;
with Entities.Queries;        use Entities.Queries;
with Entities;                use Entities;
with GPS.Editors;             use GPS.Editors;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Kernel;              use GPS.Kernel;
with String_Utils;            use String_Utils;
with Traces;                  use Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

package body Refactoring.Performers is
   Me : constant Debug_Handle := Create ("Refactoring");

   use Location_Arrays;
   use File_Arrays;

   True_Cst : aliased String := "true";

   type Renaming_Error_Record is new File_Error_Reporter_Record with
      record
         No_LI_List : File_Arrays.Instance := File_Arrays.Empty_Instance;
      end record;
   type Renaming_Error is access all Renaming_Error_Record'Class;
   overriding procedure Error
     (Report : in out Renaming_Error_Record; File : Source_File);

   type Get_Locations_Data is record
      Refs                : Location_Arrays.Instance;
      Stale_LI_List       : File_Arrays.Instance;
      Read_Only_Files     : File_Arrays.Instance;
      On_Completion       : Refactor_Performer;
      Kernel              : Kernel_Handle;
      Entity              : Entity_Information;
      Iter                : Entity_Reference_Iterator_Access;
      Errors              : Renaming_Error;

      Extra_Entities       : Entity_Information_Arrays.Instance :=
        Entity_Information_Arrays.Empty_Instance;
      Extra_Entities_Index : Entity_Information_Arrays.Index_Type :=
        Entity_Information_Arrays.First;
      Make_Writable        : Boolean;
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

   function Escape_Backslash (Pathname : String) return String;
   --  Return Pathname with all backslashes escaped

   ----------------------
   -- Escape_Backslash --
   ----------------------

   function Escape_Backslash (Pathname : String) return String is
      Escaped_Pathname : String (1 .. Pathname'Length * 2);
      J                : Natural := 0;
   begin
      for K in Pathname'Range loop
         J := J + 1;
         if Pathname (K) = '\' then
            Escaped_Pathname (J .. J + 1) := "\\";
            J := J + 1;
         else
            Escaped_Pathname (J) := Pathname (K);
         end if;
      end loop;

      return Escaped_Pathname (1 .. J);
   end Escape_Backslash;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Get_Locations_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Refactor_Performer_Record'Class, Refactor_Performer);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Renaming_Error_Record'Class, Renaming_Error);
   begin
      Free (Data.Refs);
      Free (Data.Errors.No_LI_List);
      Free (Data.Stale_LI_List);
      Destroy (Data.Iter);
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
     (Report : in out Renaming_Error_Record; File : Entities.Source_File) is
   begin
      Append (Report.No_LI_List, File);
   end Error;

   -----------------------
   -- Get_All_Locations --
   -----------------------

   procedure Get_All_Locations
     (Kernel          : access Kernel_Handle_Record'Class;
      Entity          : Entity_Information;
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
      Data.Iter          := new Entity_Reference_Iterator;
      Data.Errors        := new Renaming_Error_Record;
      Data.Make_Writable := Make_Writable;

      Push_State (Data.Kernel, Busy);
      Data.Entity            := Entity;
      Find_All_References
        (Iter                  => Data.Iter.all,
         Entity                => Entity,
         File_Has_No_LI_Report => File_Error_Reporter (Data.Errors),
         Include_Overriding    => Overridden,
         Include_Overridden    => Overridden);

      Create (C, -"Refactoring", Data, Find_Next_Location'Access);
      Set_Progress
        (Command_Access (C),
         (Running,
          Get_Current_Progress (Data.Iter.all),
          Get_Total_Progress   (Data.Iter.all)));

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
         Trace (Exception_Handle, E);
         Free (Data);
   end Get_All_Locations;

   ----------------------
   -- On_End_Of_Search --
   ----------------------

   procedure On_End_Of_Search (Data : Get_Locations_Data) is
      Confirmed : Boolean;
      Args3 : Argument_List (1 .. 2);
   begin
      Pop_State (Data.Kernel);

      if Data.Make_Writable then
         Confirmed := Confirm_Files
           (Data.Kernel,
            File_Arrays.Empty_Instance,
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
         Push_State (Data.Kernel, Busy);

         if Data.Make_Writable then
            Args3 (2) := True_Cst'Access;

            for F in File_Arrays.First .. Last (Data.Read_Only_Files) loop
               Args3 (1) := new String'
                 (Full_Name
                    (Get_Filename (Data.Read_Only_Files.Table (F))).all);
               Execute_GPS_Shell_Command
                 (Data.Kernel, "Editor.edit", Args3 (1 .. 1));
               Execute_GPS_Shell_Command
                 (Data.Kernel, "Editor.set_writable", Args3);
               Free (Args3 (1));
            end loop;
         end if;

         Execute
           (Data.On_Completion,
            Data.Kernel,
            Data.Entity,
            Data.Refs,
            Data.Errors.No_LI_List,
            Data.Stale_LI_List);
         Pop_State (Data.Kernel);
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
      use Entity_Information_Arrays;
      Ref    : constant Entity_Reference := Get (Data.Iter.all);
      Source : Source_File;
   begin
      if At_End (Data.Iter.all) then
         On_End_Of_Search (Data);
         Result := Success;

      elsif Ref /= No_Entity_Reference then
         Source := Get_File (Get_Location (Ref));
         if Is_Up_To_Date (Source) then
            Append (Data.Refs,
                    (File   => Source,
                     Line   => Get_Line (Get_Location (Ref)),
                     Column => Get_Column (Get_Location (Ref))));

         --  If we have duplicates, they will always come one after the
         --  other. So we just have to check the previous one.
         else
            Append (Data.Refs,
                    (File   => Source,
                     Line   => Get_Line (Get_Location (Ref)),
                     Column => Get_Column (Get_Location (Ref))));

            if Length (Data.Stale_LI_List) = 0
              or else Source /=
                Data.Stale_LI_List.Table (Last (Data.Stale_LI_List))
            then
               Append (Data.Stale_LI_List, Source);
            end if;
         end if;

         if not Is_Writable (Get_Filename (Source)) then
            Append (Data.Read_Only_Files, Source);
         end if;

         Next (Data.Iter.all);

         Set_Progress (Command,
                       (Running,
                        Get_Current_Progress (Data.Iter.all),
                        Get_Total_Progress (Data.Iter.all)));
         Result := Execute_Again;

      else
         Next (Data.Iter.all);
         Result := Execute_Again;
      end if;
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
        (Line, Integer (Column));
      Loc_End   : constant Editor_Location'Class :=
        Loc_Start.Forward_Char (Length - 1);
      Text : constant String := Editor.Get_Chars (Loc_Start, Loc_End);
   begin
      return Text;
   end Get_Text;

   -----------------
   -- Insert_Text --
   -----------------

   function Insert_Text
     (Kernel            : access Kernel_Handle_Record'Class;
      In_File           : GNATCOLL.VFS.Virtual_File;
      Line              : Integer;
      Column            : Visible_Column_Type := 1;
      Text              : String;
      Indent            : Boolean;
      Replaced_Length   : Integer := 0;
      Only_If_Replacing : String := "") return Boolean
   is
      Args : Argument_List_Access := new Argument_List'
        (new String'(Full_Name (In_File).all),
         new String'(Integer'Image (Line)),
         new String'(Visible_Column_Type'Image (Column)),
         new String'(Text),
         new String'("0"),
         new String'(Integer'Image (Replaced_Length)));
      Args2 : Argument_List_Access := new Argument_List'
        (new String'(Integer'Image (Line)),
         new String'(Integer'Image (Line + Lines_Count (Text) - 1)));

   begin
      if Replaced_Length /= 0 and then Only_If_Replacing /= "" then
         declare
            Replacing_Str : constant String := To_Lower (Only_If_Replacing);
            Args_Get      : Argument_List_Access := new Argument_List'
              (new String'(Full_Name (In_File).all),
               new String'(Integer'Image (Line)),
               new String'(Visible_Column_Type'Image (Column)),
               new String'("0"),
               new String'(Integer'Image (Replaced_Length)));
            Str           : constant String :=
              To_Lower (Execute_GPS_Shell_Command
                (Kernel,
                   "Editor.get_chars",
                   Args_Get.all));
         begin
            Free (Args_Get);

            if Str /= Replacing_Str then
               return False;
            end if;
         end;
      end if;

      Execute_GPS_Shell_Command (Kernel, "Editor.replace_text", Args.all);

      if Indent then
         Execute_GPS_Shell_Command (Kernel, "Editor.select_text", Args2.all);
         Execute_GPS_Shell_Command
           (Kernel, "Editor.indent", Argument_List'(1 .. 0 => null));
      end if;

      Free (Args2);
      Free (Args);
      return True;
   end Insert_Text;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
     (Kernel     : access Kernel_Handle_Record'Class;
      In_File    : GNATCOLL.VFS.Virtual_File;
      Line_Start : Integer;
      Line_End   : Integer)
   is
      Args : Argument_List_Access;
   begin
      for L in reverse Line_Start .. Line_End loop
         Args := new Argument_List'
           (new String'(Full_Name (In_File).all),
            new String'(Integer'Image (L)),
            new String'(Integer'Image (1)),
            new String'(""));
         Execute_GPS_Shell_Command (Kernel, "Editor.replace_text", Args.all);
         Free (Args);
      end loop;
   end Delete_Text;

   ----------------------
   -- Start_Undo_Group --
   ----------------------

   procedure Start_Undo_Group
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) is
   begin
      Execute_GPS_Shell_Command
        (Kernel,
         "File """ & Escape_Backslash (Full_Name (File).all) & """; "
         & "EditorBuffer.get ""%1""; EditorBuffer.start_undo_group ""%1""");
   end Start_Undo_Group;

   -----------------------
   -- Finish_Undo_Group --
   -----------------------

   procedure Finish_Undo_Group
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) is
   begin
      Execute_GPS_Shell_Command
        (Kernel,
         "File """ & Escape_Backslash (Full_Name (File).all) & """; "
         & "EditorBuffer.get ""%1""; EditorBuffer.finish_undo_group ""%1""");
   end Finish_Undo_Group;

   -----------------------
   -- Get_Initial_Value --
   -----------------------

   function Get_Initial_Value
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information) return String is
   begin
      --  These cannot have an initial value, so we save time
      if Is_Container (Get_Kind (Entity).Kind)
        or else Get_Kind (Entity).Is_Type
      then
         return "";
      end if;

      declare
         Text : constant String := Get_Text
           (Kernel,
            From_File => Get_Filename (Get_File (Get_Declaration_Of (Entity))),
            Line      => Get_Line (Get_Declaration_Of (Entity)),
            Column    => Get_Column (Get_Declaration_Of (Entity)),
            Length    => 1_000);
         Index : Natural := Text'First;
         Last  : Natural;
      begin
         Skip_To_Char (Text, Index, ':');
         Index := Index + 1;

         while Index < Text'Last loop
            if Text (Index .. Index + 1) = ":=" then
               Index := Index + 2;
               Skip_Blanks (Text, Index);
               Last := Index;
               Skip_To_Char (Text, Last, ';');

               Trace (Me, "   " & Text (Index .. Last - 1));
               return Text (Index .. Last - 1);

            elsif Text (Index) = ';'
              or else Text (Index) = ')'
            then
               exit;
            end if;
            Index := Index + 1;
         end loop;
      end;

      return "";
   end Get_Initial_Value;

end Refactoring.Performers;
