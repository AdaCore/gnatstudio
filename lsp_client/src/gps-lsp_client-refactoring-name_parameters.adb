------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Characters.Handling;

with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.Tribooleans;          use GNATCOLL.Tribooleans;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;
with GNATCOLL.Xref;                 use GNATCOLL.Xref;

with GPS.Editors;                   use GPS.Editors;
with GPS.Kernel.Actions;            use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.Modules.UI;         use GPS.Kernel.Modules.UI;
with GPS.LSP_Module;

with Basic_Types;                   use Basic_Types;
with Commands;                      use Commands;
with Commands.Interactive;          use Commands.Interactive;
with Language;                      use Language;

with GPS.LSP_Client.Requests.Execute_Command.Named_Parameters;
use GPS.LSP_Client.Requests.Execute_Command.Named_Parameters;

package body GPS.LSP_Client.Refactoring.Name_Parameters is

   Me : constant Trace_Handle := Create
     ("GPS.REFACTORING.LSP_NAME_PARAMETERS");

   type Name_Parameters_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Name_Parameters_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Name parameters" menu

   type Filter_Name_Parameters is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Name_Parameters;
      Context : Selection_Context)
      return Boolean;

   Is_LSP_Enabled : Triboolean := Indeterminate;
   --  Used by Filter_Name_Parameters to store setting

   type Named_Parameters_Command is
     new Abstract_Named_Parameters_Command_Request with null record;
   type Named_Parameters_Command_Access is
     access all Named_Parameters_Command;
   --  Used for communicate with LSP

   overriding
   procedure On_Result_Message
     (Self : in out Named_Parameters_Command) is null;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Name_Parameters_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      File     : constant GNATCOLL.VFS.Virtual_File :=
        File_Information (Context.Context);
      Buf      : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (File);
      Lang     : constant Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File
          (File_Information (Context.Context));

      Line     : Integer :=
        Integer (Entity_Line_Information (Context.Context));

      Column   : Basic_Types.Visible_Column_Type :=
        Entity_Column_Information (Context.Context);

      Loc      : Editor_Location'Class := Buf.New_Location
        (Line, Column);
      EoB      : constant Editor_Location'Class := Buf.End_Of_Buffer;

      Char     : Integer;
      Is_Param : Boolean := False;

   begin
      Line   := 0;
      Column := 0;

      --  Find the position of the first parameter
      while Loc /= EoB loop
         Char := Loc.Get_Char;

         if Char = Character'Pos (')')
           or else Char = Character'Pos (';')
         then
            exit;

         elsif Char = Character'Pos ('(') then
            Is_Param := True;

         elsif Is_Param
           and then Lang.Is_Word_Char (Wide_Wide_Character'Val (Char))
         then
            Line   := Loc.Line;
            Column := Loc.Column;
            exit;
         end if;

         Loc := Loc.Forward_Char (1);
      end loop;

      if Line = 0
        or else Column = 0
      then
         return Success;
      end if;

      declare
         Command : Named_Parameters_Command_Access :=
           new Named_Parameters_Command;
      begin
         Command.Project       := Kernel.Get_Project_Tree.Root_Project;
         Command.Text_Document := File;
         Command.Line          := Line;
         Command.Column        := Column;

         GPS.LSP_Client.Requests.Execute
           (Lang, GPS.LSP_Client.Requests.Request_Access (Command));
      end;

      return Success;

   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter : access Filter_Name_Parameters;
      Context : Selection_Context)
      return Boolean is
   begin
      if Is_LSP_Enabled = To_TriBoolean (False) then
         return False;
      end if;

      declare
         Kernel : constant Kernel_Handle := Get_Kernel (Context);
         Lang   : constant Language.Language_Access :=
           Kernel.Get_Language_Handler.Get_Language_From_File
             (File_Information (Context));

      begin
         if Ada.Characters.Handling.To_Lower (Lang.Get_Name) /= "ada" then
            return False;
         end if;

         if Is_LSP_Enabled = Indeterminate then
            Is_LSP_Enabled := To_TriBoolean
              (GPS.LSP_Module.LSP_Is_Enabled (Lang));
         end if;
      end;

      if Is_LSP_Enabled = To_TriBoolean (False)
        or else not Has_Entity_Name_Information (Context)
      then
         return False;
      end if;

      return Get_Entity (Context).Is_Subprogram;
   end Filter_Matches_Primitive;

   --------------
   -- Register --
   --------------

   procedure Register
     (Kernel : Kernel_Handle;
      Id     : GPS.Kernel.Modules.Module_ID)
   is
      pragma Unreferenced (Id);

      Name_Parameters_Filter  : constant Action_Filter :=
        new Filter_Name_Parameters;

   begin
      Register_Action
        (Kernel, "refactoring name parameters",
         Command      => new Name_Parameters_Command,
         Description  => "Name parameters in a call",
         Category     => "Refactoring",
         Filter       => Name_Parameters_Filter,
         For_Learning => True);

      Register_Contextual_Menu
        (Kernel,
         Label  => "Refactoring/Name parameters",
         Action => "refactoring name parameters",
         Group  => Editing_Contextual_Group);
   end Register;

end GPS.LSP_Client.Refactoring.Name_Parameters;
