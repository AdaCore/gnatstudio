-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2004-2006                    --
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
     (Creator : access Contextual_Label_Record;
      Context : access Selection_Context'Class) return String;

   type Change_Case_Command (Casing : Casing_Type) is
     new Interactive_Command with null record;
   function Execute
     (Command : access Change_Case_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Add_Exception_Command (Substring, Remove : Boolean) is
     new Interactive_Command with null record;
   function Execute
     (Command : access Add_Exception_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Substring_Filter_Record is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Substring_Filter_Record;
      Context : access Selection_Context'Class) return Boolean;

   type Empty_Filter_Record is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Empty_Filter_Record;
      Context : access Selection_Context'Class) return Boolean;

   -----------------
   -- Subprograms --
   -----------------

   procedure Destroy (Id : in out Casing_Module_Record);
   --  Terminate the module and save the casing exceptions on file

   procedure Set_Casing
     (Context  : Selection_Context_Access;
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
     (Context  : Selection_Context_Access;
      New_Name : String)
   is
      procedure Set_Casing
        (File         : Virtual_File;
         Line, Column : Integer);
      --  Replace test starting at Line:Column with New_Name

      ----------------
      -- Set_Casing --
      ----------------

      procedure Set_Casing
        (File         : Virtual_File;
         Line, Column : Integer)
      is
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

   begin
      if Context.all in Entity_Selection_Context'Class then
         declare
            C      : constant File_Selection_Context_Access :=
                       File_Selection_Context_Access (Context);
            File   : constant Virtual_File :=
                       Contexts.File_Information (C);
            Line   : constant Integer      :=
                       Contexts.Line_Information (C);
            Column : constant Integer      :=
                       Entity_Column_Information
                         (Entity_Selection_Context_Access (C));
         begin
            Set_Casing (File, Line, Column);
         end;

      elsif Context.all in File_Area_Context'Class then
         declare
            C      : constant File_Selection_Context_Access :=
                       File_Selection_Context_Access (Context);
            File   : constant Virtual_File :=
                       Contexts.File_Information (C);
            Line   : constant Integer      :=
                       Contexts.Line_Information (C);
            Column : constant Integer      :=
                       Contexts.Column_Information (C);
         begin
            Set_Casing (File, Line, Column);
         end;
      end if;
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
      function Get_Label (Str : String) return String;
      --  Returns the label for the given string (Entity or Area)

      ---------------
      -- Get_Label --
      ---------------

      function Get_Label (Str : String) return String is
         Name : String := Str;
      begin
         case Creator.Casing is
            when Lower =>
               return "Casing/Lower " & Krunch (To_Lower (Name));
            when Upper =>
               return "Casing/Upper " & Krunch (To_Upper (Name));
            when Mixed =>
               Mixed_Case (Name, False);
               return "Casing/Mixed " & Krunch (Name);
            when Smart_Mixed =>
               Mixed_Case (Name, True);
               return "Casing/Smart Mixed " & Krunch (Name);
         end case;
      end Get_Label;

   begin
      if Context.all in Entity_Selection_Context'Class
        and then Has_Entity_Name_Information
          (Entity_Selection_Context_Access (Context))
      then
         return Get_Label
           (Entity_Name_Information
              (Entity_Selection_Context_Access (Context)));

      elsif Context.all in File_Area_Context'Class then
         return Get_Label
           (Text_Information
              (File_Area_Context_Access (Context)));
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
      procedure Execute (Str : in out String);
      --  Execute the casing command for the given string

      -------------
      -- Execute --
      -------------

      procedure Execute (Str : in out String) is
      begin
         case Command.Casing is
            when Upper       =>
               Set_Casing (Context.Context, To_Upper (Str));
            when Lower       =>
               Set_Casing (Context.Context, To_Lower (Str));
            when Mixed       =>
               Mixed_Case (Str, False);
               Set_Casing (Context.Context, Str);
            when Smart_Mixed =>
               Mixed_Case (Str, True);
               Set_Casing (Context.Context, Str);
         end case;
      end Execute;

   begin
      if Context.Context.all in Entity_Selection_Context'Class then
         declare
            C    : constant Entity_Selection_Context_Access :=
                     Entity_Selection_Context_Access (Context.Context);
            Name : String := Entity_Name_Information (C);
         begin
            Execute (Name);
         end;
      else
         declare
            C    : constant File_Area_Context_Access :=
                     File_Area_Context_Access (Context.Context);
            Area : String := Text_Information (C);
         begin
            Execute (Area);
         end;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Add_Exception_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      function Get_Name return String;
      --  Returns the name of the entity or the content of the selection
      --  depending on the context.

      --------------
      -- Get_Name --
      --------------

      function Get_Name return String is
      begin
         if Context.Context.all in Entity_Selection_Context'Class then
            return Entity_Name_Information
              (Entity_Selection_Context_Access (Context.Context));
         else
            --  An area
            return Text_Information
              (File_Area_Context_Access (Context.Context));
         end if;
      end Get_Name;

      Name : constant String := Get_Name;

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
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Context.all in Entity_Selection_Context'Class then
         --  This is an entity, not a substring
         return  False;

      elsif Context.all in File_Area_Context'Class then
         declare
            Selection : constant File_Area_Context_Access :=
                          File_Area_Context_Access (Context);
            File      : constant Virtual_File := File_Information (Selection);
            Area      : constant String := Text_Information (Selection);
            W_Seps    : constant Character_Set :=
                          To_Set (" ;.:=(),/'#*+-""><&" &
                                  ASCII.HT & ASCII.CR & ASCII.LF);
            Before    : aliased String := "1";
            After     : aliased String := Integer'Image (Area'Length + 1);
            Line      : aliased String :=
                          Integer'Image (Line_Information (Selection));
            Col       : aliased String :=
                          Integer'Image (Column_Information (Selection));
            Text      : constant String := Execute_GPS_Shell_Command
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

   function Filter_Matches_Primitive
     (Filter  : access Empty_Filter_Record;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Context.all in Entity_Selection_Context'Class then
         return Entity_Name_Information
           (Entity_Selection_Context_Access (Context)) = "";

      elsif Context.all in File_Area_Context'Class then
         return Text_Information (File_Area_Context_Access (Context)) = "";

      else
         --  Null context, this is empty
         return True;
      end if;
   end Filter_Matches_Primitive;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filename           : constant String :=
                             Get_Home_Dir (Kernel) & Case_Exceptions_Filename;
      Command            : Interactive_Command_Access;
      Label              : Contextual_Label;
      Substring_Filter   : Action_Filter;
      Full_String_Filter : Action_Filter;
      Empty_Filter       : Action_Filter;
      Filter             : Action_Filter;
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

      Empty_Filter := new Empty_Filter_Record;

      Filter := Action_Filter
        (Create (Module => Src_Editor_Module_Name) and not Empty_Filter);

      Command := new Change_Case_Command (Lower);
      Label   := new Contextual_Label_Record;
      Label.Casing := Lower;
      Register_Contextual_Menu
        (Kernel, "Lower case entity",
         Label  => Label,
         Filter => Filter,
         Action => Command);

      Command := new Change_Case_Command (Upper);
      Label   := new Contextual_Label_Record;
      Label.Casing := Upper;
      Register_Contextual_Menu
        (Kernel, "Upper case entity",
         Label  => Label,
         Filter => Filter,
         Action => Command);

      Command := new Change_Case_Command (Mixed);
      Label   := new Contextual_Label_Record;
      Label.Casing := Mixed;
      Register_Contextual_Menu
        (Kernel, "Mixed case entity",
         Label  => Label,
         Filter => Filter,
         Action => Command);

      Command := new Change_Case_Command (Smart_Mixed);
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

      Command := new Add_Exception_Command (True, Remove => False);
      Register_Contextual_Menu
        (Kernel, "Add substring casing exception",
         Label  => -"Casing/Add substring exception for %s",
         Action => Command,
         Filter => Action_Filter (Filter and Substring_Filter));

      Command := new Add_Exception_Command (True, Remove => True);
      Register_Contextual_Menu
        (Kernel, "Remove substring casing exception",
         Label  => -"Casing/Remove substring exception for %s",
         Action => Command,
         Filter => Action_Filter (Filter and Substring_Filter));

      Command := new Add_Exception_Command (False, Remove => False);
      Register_Contextual_Menu
        (Kernel, "Add casing exception",
         Label  => -"Casing/Add exception for %s",
         Action => Command,
         Filter => Action_Filter (Filter and Full_String_Filter));

      Command := new Add_Exception_Command (False, Remove => True);
      Register_Contextual_Menu
        (Kernel, "Remove casing exception",
         Label  => -"Casing/Remove exception for %s",
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
