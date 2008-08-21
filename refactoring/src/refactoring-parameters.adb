-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005-2008, AdaCore              --
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

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.Contexts;   use GPS.Kernel.Contexts;
with GPS.Kernel.Scripts;    use GPS.Kernel.Scripts;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with Commands.Interactive;  use Commands, Commands.Interactive;
with Entities;              use Entities;
with Entities.Queries;      use Entities.Queries;
with GNATCOLL.VFS;                   use GNATCOLL.VFS;
with String_Utils;          use String_Utils;
with Traces;                use Traces;
with GPS.Intl;              use GPS.Intl;
with Refactoring.Performers; use Refactoring.Performers;

with GNAT.OS_Lib;           use GNAT.OS_Lib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Refactoring.Parameters is
   Me : constant Debug_Handle := Create ("Refactor.Params");

   Location_Cst               : aliased constant String := "location";

   type Is_Subprogram_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Subprogram_Filter;
      Context : Selection_Context) return Boolean;
   --  Filter that checks that the user has clicked on a subprogram entity

   type Name_Parameters_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Name_Parameters_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Name Parameters" menu

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands

   function Name_Parameters
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer;
      Column : Visible_Column_Type) return Command_Return_Type;
   --  Name the parameters for the call to Entity at the given location

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Subprogram_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      Entity := Get_Entity (Context);
      return Entity /= null and then Is_Subprogram (Entity);
   end Filter_Matches_Primitive;

   ---------------------
   -- Name_Parameters --
   ---------------------

   function Name_Parameters
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer;
      Column : Visible_Column_Type) return Command_Return_Type
   is
      --  File needs to be open for get_chars to work, unfortunately
      Args2  : Argument_List_Access :=
        new Argument_List'
          (new String'(Full_Name (File).all),
           new String'("0"),
           new String'("0"));
      Found  : constant String := Execute_GPS_Shell_Command
        (Kernel, "Editor.edit", Args2.all);
      pragma Unreferenced (Found);

      Chars : constant String := Get_Text
        (Kernel, File, Line, Column, 1_000);
      Param : Entity_Information;
      First, Last : Integer := Chars'First;
      Nest_Count  : Integer := 1;
      Result : Unbounded_String;
      Iter : Subprogram_Iterator := Get_Subprogram_Parameters (Entity);

      procedure Add_Parameter_Name;
      --  Add the name of the parameter in Result, skipping any space currently
      --  pointed to by First.
      --  First should point to '(' or ',' in the string Chars. Any character
      --  preceding First and not yet copied to Result is copied first

      procedure Add_Parameter_Name is
         Tmp : Natural;
      begin
         First := First + 1;
         Skip_Blanks (Chars, First);

         --  Do we have a named parameter already ?
         Tmp := First;
         Skip_Word   (Chars, Tmp);
         Skip_Blanks (Chars, Tmp);
         if Tmp + 1 <= Chars'Last and then Chars (Tmp .. Tmp + 1) = "=>" then
            --  No need to replace any more: There won't be any unnamed
            --  parameter afterward
            Param := null;

         else
            Result := Result & Chars (Last .. First - 1);
            Last := First;

            if Chars (First - 1) /= '(' then
               First := First - 1;
            end if;

            Result := Result & Get_Name (Param).all & " => ";

            Next (Iter);
            Get (Iter, Param);
         end if;
      end Add_Parameter_Name;

   begin
      Free (Args2);

      Skip_Word   (Chars, First);
      Skip_Blanks (Chars, First);
      if First > Chars'Last
        or else Chars (First) /= '('
      then
         Trace (Me, "Doesn't appear to be a subprogram call");
         return Failure;
      end if;

      Get (Iter, Param);
      if Param = null then
         Trace (Me, "No parameter for this subprogram");
         return Failure;
      end if;

      Add_Parameter_Name;

      while Param /= null and then First <= Chars'Last loop
         if Chars (First) = '(' then
            Nest_Count := Nest_Count + 1;
         elsif Chars (First) = ')' then
            Nest_Count := Nest_Count - 1;
            exit when Nest_Count = 0;
         elsif Chars (First) = ',' and then Nest_Count = 1 then
            Add_Parameter_Name;
         end if;

         First  := First + 1;
      end loop;

      Result := Result & Chars (Last .. First);
      if Insert_Text
        (Kernel, File, Line, Column, To_String (Result),
         Indent          => False,
         Replaced_Length => First - Chars'First + 1)
      then
         return Success;
      else
         return Failure;
      end if;
   end Name_Parameters;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Name_Parameters_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      return Name_Parameters
        (Kernel => Get_Kernel (Context.Context),
         Entity => Get_Entity (Context.Context, Ask_If_Overloaded => True),
         File   => File_Information (Context.Context),
         Line   => Line_Information (Context.Context),
         Column => Column_Information (Context.Context));
   end Execute;

   ----------------------------
   -- Entity_Command_Handler --
   ----------------------------

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "name_parameters" then
         Name_Parameters (Data, (1 => Location_Cst'Access));
         declare
            Entity   : constant Entity_Information := Get_Data (Data, 1);
            Location : constant File_Location_Info := Get_Data (Data, 2);
            File     : constant Virtual_File := Get_Data (Get_File (Location));
         begin
            if Name_Parameters
              (Kernel => Get_Kernel (Data),
               Entity => Entity,
               File   => File,
               Line   => Get_Line (Location),
               Column => Get_Column (Location)) /= Success
            then
               Set_Error_Msg (Data, -"Couldn't name parameters");
            end if;
         end;
      end if;
   end Entity_Command_Handler;

   --------------------------
   -- Register_Refactoring --
   --------------------------

   procedure Register_Refactoring
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      C : constant Interactive_Command_Access := new Name_Parameters_Command;
      Filter : Action_Filter;
   begin
      Filter := new Is_Subprogram_Filter;
      Register_Contextual_Menu
        (Kernel,
         Name  => "Refactoring name parameters",
         Label => "Refactoring/Name parameters",
         Filter => Filter and Create (Language => "Ada"),
         Action => C);
      Register_Command
        (Kernel, "name_parameters", 1, 1, Entity_Command_Handler'Access,
         Get_Entity_Class (Kernel));
   end Register_Refactoring;

end Refactoring.Parameters;
