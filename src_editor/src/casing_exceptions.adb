-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2004-2005                    --
--                              AdaCore                              --
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
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Case_Handling.IO;        use Case_Handling.IO;
with Case_Handling;
with Commands.Interactive;    use Commands, Commands.Interactive;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Contexts;     use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with Src_Editor_Module;       use Src_Editor_Module;
with String_Utils;            use String_Utils;
with VFS;                     use VFS;

package body Casing_Exceptions is

   Case_Exceptions_Filename : constant String := "case_exceptions.xml";

   type Casing_Module_Record is new Module_ID_Record with record
      Casing_Exceptions_Table : Case_Handling.Casing_Exceptions;
   end record;
   type Casing_Module is access all Casing_Module_Record'Class;

   Casing_Module_Id : Casing_Module;

   ----------------------
   -- Contextual menus --
   ----------------------

   type Casing_Type is (Lower, Upper, Mixed, Smart_Mixed);

   type Contextual_Label_Record is new Contextual_Menu_Label_Creator_Record
   with record
      Casing : Casing_Type;
   end record;
   type Contextual_Label is access all Contextual_Label_Record'Class;
   function Get_Label
     (Creator   : access Contextual_Label_Record;
      Context   : access Selection_Context'Class) return String;

   type Change_Case_Command is new Interactive_Command with record
      Casing : Casing_Type;
   end record;
   function Execute
     (Command : access Change_Case_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Add_Exception_Command is new Interactive_Command with record
      Remove    : Boolean := False;
      Substring : Boolean;
   end record;
   function Execute
     (Command : access Add_Exception_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Substring_Filter_Record is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Substring_Filter_Record;
      Context : access Selection_Context'Class) return Boolean;

   -----------------
   -- Subprograms --
   -----------------

   procedure Destroy (Id : in out Casing_Module_Record);
   --  Terminate the module and save the casing exceptions on file.

   procedure Set_Casing
     (Context  : Entity_Selection_Context_Access;
      New_Name : String);
   --  Function used by the following callbacks to set the casing

   -------------------
   -- Add_Exception --
   -------------------

   procedure Add_Exception (Ident : String) is
   begin
      Case_Handling.Add_Exception
        (Casing_Module_Id.Casing_Exceptions_Table, Ident, Read_Only => False);
   end Add_Exception;

   -----------------------------
   -- Add_Substring_Exception --
   -----------------------------

   procedure Add_Substring_Exception (Ident : String) is
   begin
      Case_Handling.Add_Substring_Exception
        (Casing_Module_Id.Casing_Exceptions_Table, Ident, Read_Only => False);
   end Add_Substring_Exception;

   ----------------------
   -- Remove_Exception --
   ----------------------

   procedure Remove_Exception (Ident : String) is
   begin
      Case_Handling.Remove_Exception
        (Casing_Module_Id.Casing_Exceptions_Table, Ident);
   end Remove_Exception;

   --------------------------------
   -- Remove_Substring_Exception --
   --------------------------------

   procedure Remove_Substring_Exception (Ident : String) is
   begin
      Case_Handling.Remove_Substring_Exception
        (Casing_Module_Id.Casing_Exceptions_Table, Ident);
   end Remove_Substring_Exception;

   -------------------------
   -- Get_Case_Exceptions --
   -------------------------

   function Get_Case_Exceptions return Case_Handling.Casing_Exceptions is
   begin
      return Casing_Module_Id.Casing_Exceptions_Table;
   end Get_Case_Exceptions;

   ----------------
   -- Set_Casing --
   ----------------

   procedure Set_Casing
     (Context  : Entity_Selection_Context_Access;
      New_Name : String)
   is
      File   : constant Virtual_File := Contexts.File_Information (Context);
      Line   : constant Integer      := Contexts.Line_Information (Context);
      Column : constant Integer      := Entity_Column_Information (Context);
      Args   : Argument_List_Access :=
        new Argument_List'
          (new String'(Full_Name (File).all),
           new String'(Integer'Image (Line)),
           new String'(Integer'Image (Column)),
           new String'(New_Name),
           new String'("0"),
           new String'(Integer'Image (New_Name'Length)));
   begin
      Execute_GPS_Shell_Command
        (Get_Kernel (Context), "Editor.replace_text", Args.all);
      Free (Args);
   end Set_Casing;

   ----------------------
   -- Casing_Customize --
   ----------------------

   procedure Casing_Customize
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Kernel, Level, File);
   begin
      if Node.Tag.all = "case_exceptions" then
         --  Ok this is a case exceptions node

         declare
            Child : Node_Ptr := Node.Child;
         begin
            while Child /= null loop
               if Child.Tag.all = "word" then
                  --  This is a full word exception
                  Add_Exception
                    (Casing_Module_Id.Casing_Exceptions_Table,
                     Child.Value.all,
                     Read_Only => True);

               elsif Child.Tag.all = "substring" then
                  --  This is substring exception
                  Add_Substring_Exception
                    (Casing_Module_Id.Casing_Exceptions_Table,
                     Child.Value.all,
                     Read_Only => True);
               end if;
               Child := Child.Next;
            end loop;
         end;
      end if;
   end Casing_Customize;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Creator : access Contextual_Label_Record;
      Context : access Selection_Context'Class) return String
   is
      C : Entity_Selection_Context_Access;
   begin
      if Context.all in Entity_Selection_Context'Class
        and then Has_Entity_Name_Information
          (Entity_Selection_Context_Access (Context))
      then
         C := Entity_Selection_Context_Access (Context);
         declare
            Name : String := Krunch (Entity_Name_Information (C));
         begin
            case Creator.Casing is
               when Lower =>
                  return "Casing/Lower " & To_Lower (Name);
               when Upper =>
                  return "Casing/Upper " & To_Upper (Name);
               when Mixed =>
                  Mixed_Case (Name);
                  return "Casing/Mixed " & Name;
               when Smart_Mixed =>
                  Smart_Mixed_Case (Name);
                  return "Casing/Smart Mixed " & Name;
            end case;
         end;
      end if;
      return "";
   end Get_Label;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Change_Case_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context.Context);
      Name   : String       := Entity_Name_Information (C);
   begin
      case Command.Casing is
         when Upper       => Set_Casing (C, To_Upper (Name));
         when Lower       => Set_Casing (C, To_Lower (Name));
         when Mixed       => Mixed_Case (Name); Set_Casing (C, Name);
         when Smart_Mixed => Smart_Mixed_Case (Name); Set_Casing (C, Name);
      end case;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Add_Exception_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context.Context);
      Name   : constant String := Entity_Name_Information (C);
   begin
      if Command.Substring then
         if Command.Remove then
            Remove_Substring_Exception (Name);
         else
            Add_Substring_Exception (Name);
         end if;
      else
         if Command.Remove then
            Remove_Exception (Name);
         else
            Add_Exception (Name);
         end if;
      end if;
      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Substring_Filter_Record;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel     : constant Kernel_Handle := Get_Kernel (Context);
      Selection    : Entity_Selection_Context_Access;
   begin
      if Context.all in Entity_Selection_Context'Class
        and then Has_Entity_Name_Information
          (Entity_Selection_Context_Access (Context))
        and then Has_Entity_Column_Information
          (Entity_Selection_Context_Access (Context))
        and then Has_Line_Information
          (Entity_Selection_Context_Access (Context))
      then
         Selection := Entity_Selection_Context_Access (Context);
         declare
            File     : constant Virtual_File := File_Information (Selection);
            E_Name   : constant String := Entity_Name_Information (Selection);
            W_Seps   : constant Character_Set :=
              To_Set (" ;.:=(),/'#*+-""><&" & ASCII.HT & ASCII.CR & ASCII.LF);
            Before   : aliased String := "1";
            After    : aliased String := Integer'Image (E_Name'Length + 1);
            Line     : aliased String :=
              Integer'Image (Line_Information (Selection));
            Col      : aliased String :=
              Integer'Image (Entity_Column_Information (Selection));
            Text     : constant String := Execute_GPS_Shell_Command
              (Kernel, "Editor.get_chars",
               (1 => Full_Name (File).all'Unrestricted_Access,
                2 => Line'Unchecked_Access,
                3 => Col'Unchecked_Access,
                4 => Before'Unchecked_Access,
                5 => After'Unchecked_Access));
         begin
            return Text'Length <= 1
              or else not Is_In (Text (Text'First), W_Seps)
              or else not Is_In (Text (Text'Last), W_Seps);
         end;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filename : constant String :=
                   Get_Home_Dir (Kernel) & Case_Exceptions_Filename;
      Command : Interactive_Command_Access;
      Label   : Contextual_Label;
      Substring_Filter, Full_String_Filter : Action_Filter;
      Filter  : Action_Filter;
   begin
      Casing_Module_Id := new Casing_Module_Record;
      Register_Module
        (Module      => Module_ID (Casing_Module_Id),
         Kernel      => Kernel,
         Module_Name => "Casing",
         Priority    => Default_Priority - 1);
      Load_Exceptions
        (Casing_Module_Id.Casing_Exceptions_Table,
         Filename,
         Read_Only => False);

      Filter := Action_Filter
        (Create (Module => Src_Editor_Module_Name)
         and Lookup_Filter (Kernel, "Entity"));

      Command := new Change_Case_Command;
      Label   := new Contextual_Label_Record;
      Label.Casing := Lower;
      Change_Case_Command (Command.all).Casing := Lower;
      Register_Contextual_Menu
        (Kernel, "Lower case entity",
         Label  => Label,
         Filter => Filter,
         Action => Command);

      Command := new Change_Case_Command;
      Change_Case_Command (Command.all).Casing := Upper;
      Label   := new Contextual_Label_Record;
      Label.Casing := Upper;
      Register_Contextual_Menu
        (Kernel, "Upper case entity",
         Label  => Label,
         Filter => Filter,
         Action => Command);

      Command := new Change_Case_Command;
      Change_Case_Command (Command.all).Casing := Mixed;
      Label   := new Contextual_Label_Record;
      Label.Casing := Mixed;
      Register_Contextual_Menu
        (Kernel, "Mixed case entity",
         Label  => Label,
         Filter => Filter,
         Action => Command);

      Command := new Change_Case_Command;
      Change_Case_Command (Command.all).Casing := Smart_Mixed;
      Label   := new Contextual_Label_Record;
      Label.Casing := Smart_Mixed;
      Register_Contextual_Menu
        (Kernel, "Smart mixed case entity",
         Label  => Label,
         Filter => Filter,
         Action => Command);

      Register_Contextual_Menu
        (Kernel,
         Name   => "casing separator",
         Action => null,
         Filter => Filter,
         Label  => -"Casing/");

      Substring_Filter   := new Substring_Filter_Record;
      Full_String_Filter := Action_Filter (not Substring_Filter);

      Command := new Add_Exception_Command;
      Add_Exception_Command (Command.all).Substring := True;
      Register_Contextual_Menu
        (Kernel, "Add substring casing exception",
         Label  => -"Casing/Add substring exception for %e",
         Action => Command,
         Filter => Action_Filter (Filter and Substring_Filter));

      Command := new Add_Exception_Command;
      Add_Exception_Command (Command.all).Substring := True;
      Add_Exception_Command (Command.all).Remove    := True;
      Register_Contextual_Menu
        (Kernel, "Remove substring casing exception",
         Label  => -"Casing/Remove substring exception for %e",
         Action => Command,
         Filter => Action_Filter (Filter and Substring_Filter));

      Command := new Add_Exception_Command;
      Add_Exception_Command (Command.all).Substring := False;
      Register_Contextual_Menu
        (Kernel, "Add casing exception",
         Label  => -"Casing/Add exception for %e",
         Action => Command,
         Filter => Action_Filter (Filter and Full_String_Filter));

      Command := new Add_Exception_Command;
      Add_Exception_Command (Command.all).Substring := False;
      Add_Exception_Command (Command.all).Remove := True;
      Register_Contextual_Menu
        (Kernel, "Remove casing exception",
         Label  => -"Casing/Remove exception for %e",
         Action => Command,
         Filter => Action_Filter (Filter and Full_String_Filter));
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Casing_Module_Record) is
      Filename : constant String :=
        Get_Home_Dir (Get_Kernel (Id)) & Case_Exceptions_Filename;
   begin
      Save_Exceptions (Id.Casing_Exceptions_Table, Filename);
      Destroy (Id.Casing_Exceptions_Table);
   end Destroy;

end Casing_Exceptions;
