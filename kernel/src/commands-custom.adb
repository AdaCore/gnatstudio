-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
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

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Kernel.Contexts; use Glide_Kernel.Contexts;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Timeout; use Glide_Kernel.Timeout;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;
with Glide_Intl;           use Glide_Intl;

with Glib;                 use Glib;
with Glib.Xml_Int;         use Glib.Xml_Int;
with Gtkada.MDI;           use Gtkada.MDI;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Check_Button;     use Gtk.Check_Button;
with Gtk.Tooltips;         use Gtk.Tooltips;
with Gtk.Box;              use Gtk.Box;
with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Label;            use Gtk.Label;
with Gtk.Combo;            use Gtk.Combo;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Size_Group;       use Gtk.Size_Group;
with Gtk.Spin_Button;      use Gtk.Spin_Button;

with Basic_Types;          use Basic_Types;
with Projects;             use Projects;
with Projects.Registry;    use Projects.Registry;
with String_Utils;         use String_Utils;
with VFS;                  use VFS;
with Interactive_Consoles; use Interactive_Consoles;

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Traces;               use Traces;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with GNAT.Regpat;          use GNAT.Regpat;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with System;

package body Commands.Custom is

   Me : constant Debug_Handle := Create ("Commands.Custom", Off);

   Output_Use_Default : constant String := "<use default>";

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Boolean_Array, Boolean_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);

   type Custom_Component_Iterator is new Component_Iterator_Record with record
      Command    : Custom_Command_Access;
      Current    : Integer;
      On_Failure : Integer;
   end record;
   function Get
     (Iter : access Custom_Component_Iterator)
      return Command_Component;
   procedure Next (Iter : access Custom_Component_Iterator);
   procedure Free (Iter : in out Custom_Component_Iterator);
   function On_Failure
     (Iter : access Custom_Component_Iterator) return Component_Iterator;
   --  See doc from inherited subprograms

   type Command_Editor_Record is new Gtk_Box_Record with record
      Show_Command : Gtk_Check_Button;
      Show_Output  : Gtk_Check_Button;
      Output       : Gtk_Entry;
   end record;
   type Command_Editor_Widget is access all Command_Editor_Record'Class;
   --  This widget is used to graphically edit a custom command

   type Custom_Component_Editor is record
      Kernel       : Kernel_Handle;
      Command      : Gtk_Entry;
      Show_Command : Gtk_Check_Button;
      Output       : Gtk_Entry;
   end record;
   --  This widget is used to edit a Custom_Component_Record

   procedure Component_Editor
     (Component : access Custom_Component_Record'Class;
      Kernel    : access Kernel_Handle_Record'Class;
      Editor    : access Gtk_Box_Record'Class;
      Custom    : out Custom_Component_Editor;
      Size      : Gtk_Size_Group);
   --  Add the widgets required to edit Component to Editor

   type Shell_Component_Editor_Record is new Gtk_Box_Record with record
      Custom       : Custom_Component_Editor;
      Lang         : Gtk_Combo;
   end record;
   type Shell_Component_Editor is access
     all Shell_Component_Editor_Record'Class;
   --  This widget is used to graphically edit a shell component

   type External_Component_Editor_Record is new Gtk_Box_Record with record
      Custom       : Custom_Component_Editor;
      Regexp       : Gtk_Entry;
      Current      : Gtk_Spin_Button;
      Final        : Gtk_Spin_Button;
      Hide         : Gtk_Check_Button;
   end record;
   type External_Component_Editor is
     access all External_Component_Editor_Record'Class;
   --  This widget is used to graphically edit an external component

   procedure Check_Save_Output
     (Kernel           : access Kernel_Handle_Record'Class;
      Command          : access Custom_Command'Class;
      Save_Output      : out Boolean_Array;
      Context          : Selection_Context_Access;
      Context_Is_Valid : out Boolean);
   --  Compute whether we should save the output of each commands. This depends
   --  on whether later commands reference this output through %1, %2,...
   --  This also checks that the context contains information for possible
   --  %p, %f,.. parameters (Set Context_Is_Valid otherwise).

   procedure Clear_Consoles
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Custom_Command'Class);
   --  Clear all existing consoles that Command will use. Consoles that do not
   --  exist yet are not created.
   --  This is used so that the new output isn't mix with the output of
   --  previous run.
   --  The default GPS console is never cleared.

   function Project_From_Param
     (Param : String; Context : Selection_Context_Access) return Project_Type;
   --  Return the project from the parameter. Parameter is the string
   --  following the '%' sign. No_Project is returned if the context doesn't
   --  contain this information

   pragma Warnings (Off);
   --  These 2 UCs are safe aliasing-wise, so kill warning
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Custom_Command_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Custom_Command_Access, System.Address);
   pragma Warnings (On);

   procedure Exit_Cb (Data : Process_Data; Status : Integer);
   --  Called when an external process has finished running

   procedure Store_Command_Output (Data : Process_Data; Output : String);
   --  Store the output of the current command

   type Parameters_Filter_Record is new Action_Filter_Record with record
      Need_File, Need_Directory : Boolean := False;
      Need_Project : Character := ' ';
   end record;
   type Parameters_Filter is access all Parameters_Filter_Record'Class;
   --  Check that the current context contains enough information to satisfy
   --  the requirements for a custom command.
   --  Need_Project is 'p' is a current project is needed, 'P' is a root
   --  project is needed, different from the default project loaded by GPS at
   --  startup, and any other character if no project is needed.

   function Filter_Matches_Primitive
     (Filter  : access Parameters_Filter_Record;
      Context : access Selection_Context'Class) return Boolean;
   --  See doc for inherited subprogram.

   procedure Free (Execution : in out Custom_Command_Execution);
   --  Free Execution and its contents

   function External_From_XML
     (Command : Glib.Xml_Int.Node_Ptr) return Custom_Component;
   function Shell_From_XML
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : Glib.Xml_Int.Node_Ptr) return Custom_Component;
   --  Create a command component from an XML node

   function From_XML
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : Glib.Xml_Int.Node_Ptr;
      Name    : String) return Components_Array_Access;
   --  Create a list of components from an XML node

   function Output_Substitution
     (Command   : access Custom_Command'Class;
      Component : Integer;
      Ref       : Integer) return Integer;
   --  Return the index of the component to which a ${REF} in Component
   --  refers. For instance, if the 3rd component contains a $1, the result
   --  will be 2.
   --  This properly takes into account the on-failure components.
   --  Return -1 if no Ref is invalid.


   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Parameters_Filter_Record;
      Context : access Selection_Context'Class) return Boolean
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Project : Project_Type;
   begin
      if Filter.Need_Project = 'p'
        or else Filter.Need_Project = 'P'
      then
         Project := Project_From_Param
           (Filter.Need_Project & ' ', Selection_Context_Access (Context));
         if Project = No_Project then
            Insert (Kernel, -"No project specified", Mode => Error);
            return False;
         end if;
      end if;

      if Filter.Need_File then
         if Context.all not in File_Selection_Context'Class
           or else not Has_File_Information
             (File_Selection_Context_Access (Context))
         then
            Insert (Kernel, -"No file specified", Mode => Error);
            return False;
         end if;
      end if;

      if Filter.Need_Directory then
         if Context.all not in File_Selection_Context'Class
           or else not Has_Directory_Information
             (File_Selection_Context_Access (Context))
         then
            Insert (Kernel, -"No directory specified", Mode => Error);
            return False;
         end if;
      end if;

      return True;
   end Filter_Matches_Primitive;

   -------------------
   -- Create_Filter --
   -------------------

   function Create_Filter
     (Command : Glib.Xml_Int.Node_Ptr) return Action_Filter
   is
      Filter : Parameters_Filter;

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String;
      --  Check whether the command has a '%' + digit parameter

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String
      is
         pragma Unreferenced (Quoted);
      begin
         if Param = "f" or else Param = "F" then
            if Filter = null then
               Filter := new Parameters_Filter_Record;
            end if;
            Filter.Need_File := True;

         elsif Param = "d" then
            if Filter = null then
               Filter := new Parameters_Filter_Record;
            end if;
            Filter.Need_Directory := True;

         elsif Param (Param'First) = 'p' or else Param (Param'First) = 'P' then
            if Param /= "pps" and then Param /= "PPs" then
               if Filter = null then
                  Filter := new Parameters_Filter_Record;
               end if;
               Filter.Need_Project := Param (Param'First);
            end if;
         end if;

         return "";
      end Substitution;

      use type Glib.String_Ptr;
      N : Node_Ptr := Command;
   begin
      while N /= null loop
         if N.Value /= null then
            declare
               Tmp : constant String := Substitute
                 (N.Value.all,
                  Substitution_Char => '%',
                  Callback          => Substitution'Unrestricted_Access,
                  Recursive         => False);
               pragma Unreferenced (Tmp);
            begin
               null;
            end;
         end if;

         N := N.Next;
      end loop;

      return Action_Filter (Filter);
   end Create_Filter;

   -------------
   -- Exit_Cb --
   -------------

   procedure Exit_Cb (Data : Process_Data; Status : Integer) is
      Command : constant Custom_Command_Access := Convert (Data.Callback_Data);
   begin
      Command.Execution.External_Process_In_Progress := False;
      Command.Execution.Process_Exit_Status := Status;
   end Exit_Cb;

   --------------------------
   -- Store_Command_Output --
   --------------------------

   procedure Store_Command_Output (Data : Process_Data; Output : String) is
      Command : constant Custom_Command_Access := Convert (Data.Callback_Data);

      procedure Insert (Message : String);
      --  Insert Message in the current console

      ------------
      -- Insert --
      ------------

      procedure Insert (Message : String) is
         Console : constant Interactive_Console :=
           Command.Execution.External_Process_Console;
      begin
         if Console /= null then
            Insert (Console, Message, Add_LF => False);
            Highlight_Child
              (Find_MDI_Child (Get_MDI (Command.Kernel), Console));
         end if;
      end Insert;

      Old : GNAT.OS_Lib.String_Access := Command.Execution.Current_Output;
      Current, Total : Integer;
      Save_Output : constant Boolean :=
        Command.Execution.Save_Output (Command.Execution.Cmd_Index);
   begin
      if Command.Execution.Progress_Matcher /= null then
         declare
            Matched : Match_Array
              (0 .. Integer'Max (Command.Execution.Current_In_Regexp,
                                 Command.Execution.Total_In_Regexp));
         begin
            Match (Command.Execution.Progress_Matcher.all, Output, Matched);
            if Matched (Command.Execution.Current_In_Regexp) = No_Match
              or else Matched (Command.Execution.Total_In_Regexp) = No_Match
            then
               Insert (Output);
               if Save_Output then
                  Command.Execution.Current_Output :=
                    new String'(Old.all & Output);
                  Free (Old);
               end if;
            else
               if Matched (0).Last < Output'Last then
                  declare
                     Outp : constant String :=
                       Output (Output'First .. Matched (0).First - 1)
                     & Output (Matched (0).Last + 1 .. Output'Last);
                  begin
                     if Command.Execution.Hide_Progress then
                        Insert (Outp);
                     else
                        Insert (Output);
                     end if;

                     if Save_Output then
                        Command.Execution.Current_Output := new String'
                          (Old.all & Outp);
                        Free (Old);
                     end if;
                  end;

               else
                  if Command.Execution.Hide_Progress then
                     Insert (Output (Output'First .. Matched (0).First - 1));
                  else
                     Insert (Output);
                  end if;

                  if Save_Output then
                     Command.Execution.Current_Output := new String'
                       (Old.all
                        & Output (Output'First .. Matched (0).First - 1));
                     Free (Old);
                  end if;
               end if;

               Current := Safe_Value
                 (Output
                    (Matched (Command.Execution.Current_In_Regexp).First
                     .. Matched (Command.Execution.Current_In_Regexp).Last));
               Total := Safe_Value
                 (Output
                    (Matched (Command.Execution.Total_In_Regexp).First
                     .. Matched (Command.Execution.Total_In_Regexp).Last));
               Set_Progress
                 (Command,
                  Progress_Record'
                    (Activity => Running,
                     Current  => Current,
                     Total    => Total));
            end if;
         end;

      elsif Save_Output then
         Insert (Output);
         Command.Execution.Current_Output := new String'(Old.all & Output);
         Free (Old);
      else
         Insert (Output);
      end if;
   end Store_Command_Output;

   --------------------
   -- Clear_Consoles --
   --------------------

   procedure Clear_Consoles
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Custom_Command'Class)
   is
      procedure Clear_Console (Name : GNAT.OS_Lib.String_Access);
      --  Clear a specific console

      procedure Clear_Console (Name : GNAT.OS_Lib.String_Access) is
         Console : Interactive_Console;
      begin
         if Name /= null
           and then Name.all /= No_Output
           and then Name.all /= Console_Output
         then
            Console := Create_Interactive_Console
              (Kernel, Name.all, Create_If_Not_Exist => False);
            if Console /= null then
               Clear (Console);
            end if;
         end if;
      end Clear_Console;

   begin
      Clear_Console (Command.Default_Output_Destination);
      for C in Command.Components'Range loop
         Clear_Console (Command.Components (C).Component.Output);
      end loop;
   end Clear_Consoles;

   -------------------------
   -- Output_Substitution --
   -------------------------

   function Output_Substitution
     (Command   : access Custom_Command'Class;
      Component : Integer;
      Ref       : Integer) return Integer
   is
      Ref_Index : Integer := Component - 1;
      Sub_Index : Integer := Ref;
      Failure   : Integer := Command.Components (Component).On_Failure_For;
   begin
      if Ref > 0 then
         while Ref_Index >= Command.Components'First loop
            --  Handling of recursive on-failure blocks
            if Ref_Index = Failure then
               Failure := Command.Components (Ref_Index).On_Failure_For;
            end if;

            if Command.Components (Ref_Index).On_Failure_For = Failure then
               Sub_Index := Sub_Index - 1;
               exit when Sub_Index = 0;
            end if;

            Ref_Index := Ref_Index - 1;
         end loop;
      end if;

      if Ref_Index >= Command.Components'First then
         return Ref_Index;
      else
         return -1;
      end if;
   end Output_Substitution;

   -----------------------
   -- Check_Save_Output --
   -----------------------

   procedure Check_Save_Output
     (Kernel           : access Kernel_Handle_Record'Class;
      Command          : access Custom_Command'Class;
      Save_Output      : out Boolean_Array;
      Context          : Selection_Context_Access;
      Context_Is_Valid : out Boolean)
   is
      Index : Natural;

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String;
      --  Check whether the command has a '%' + digit parameter

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String
      is
         pragma Unreferenced (Quoted);
         Sub_Index, Ref_Index : Integer;
      begin
         if Param = "f" or else Param = "F" then
            if Context = null
              or else Context.all not in File_Selection_Context'Class
              or else not Has_File_Information
                (File_Selection_Context_Access (Context))
            then
               Context_Is_Valid := False;
               Insert (Kernel,
                       -"Command not executed: file required",
                       Mode => Error);
               raise Invalid_Substitution;
            end if;

         elsif Param = "d" then
            if Context = null
              or else Context.all not in File_Selection_Context'Class
              or else not Has_Directory_Information
                (File_Selection_Context_Access (Context))
            then
               Context_Is_Valid := False;
               Insert (Kernel,
                       -"Command not executed: directory required",
                       Mode => Error);
               raise Invalid_Substitution;
            end if;

         elsif Param (Param'First) = 'p' or else Param (Param'First) = 'P' then
            if Param /= "pps" and then Param /= "PPs" then
               if Project_From_Param (Param, Context) = No_Project then
                  Context_Is_Valid := False;
                  Insert (Kernel,
                            -"Command not executed: project required",
                          Mode => Error);
                  raise Invalid_Substitution;
               end if;
            end if;

         else
            Sub_Index := Safe_Value (Param, Default => 0);
            Ref_Index := Output_Substitution
              (Command, Index, Sub_Index);

            if Ref_Index >= Command.Components'First then
               Save_Output (Ref_Index) := True;
            end if;
         end if;

         return "";
      end Substitution;

   begin
      Context_Is_Valid := True;
      Save_Output := (others => False);

      Index := Command.Components'First;
      while Index <= Command.Components'Last loop
         declare
            S : constant String := String_Utils.Substitute
              (Command.Components (Index).Component.Command.all,
               Substitution_Char => '%',
               Callback          => Substitution'Unrestricted_Access,
               Recursive         => False);
            pragma Unreferenced (S);
         begin
            exit when not Context_Is_Valid;
         end;

         Index := Index + 1;
      end loop;
   end Check_Save_Output;

   ----------
   -- Free --
   ----------

   procedure Free (Execution : in out Custom_Command_Execution) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Custom_Command_Execution_Record, Custom_Command_Execution);
   begin
      if Execution /= null then
         Free             (Execution.Current_Output);
         GNAT.OS_Lib.Free (Execution.Outputs);
         Unchecked_Free   (Execution.Save_Output);
         Unchecked_Free   (Execution.Progress_Matcher);
         Unref            (Execution.Context);
         Unchecked_Free (Execution);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Component : in out Custom_Component_Record) is
   begin
      Free (Component.Output);
      Free (Component.Command);
   end Free;

   procedure Free (Component : in out External_Component_Record) is
   begin
      Free (Component.Progress_Regexp);
      Free (Custom_Component_Record (Component));
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Custom_Command) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Custom_Component_Record'Class, Custom_Component);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Components_Array, Components_Array_Access);
   begin
      Free (X.Default_Output_Destination);
      Free (X.Name);
      Free (X.Execution);

      for C in X.Components'Range loop
         Free (X.Components (C).Component.all);
         Unchecked_Free (X.Components (C).Component);
      end loop;
      Unchecked_Free (X.Components);
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out Custom_Command_Access;
      Name         : String;
      Kernel       : Kernel_Handle;
      Command      : String;
      Script       : Glide_Kernel.Scripts.Scripting_Language) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Name   := new String'(Name);
      Item.Components := new Components_Array'
        (1 => (Component => new Shell_Component_Record'
                 (Command_Component_Record with
                  Show_Command => False,
                  Output       => new String'(No_Output),
                  Command      => new String'(Command),
                  Script       => Script),
               On_Failure_For => -1));
   end Create;

   --------------------
   -- Shell_From_XML --
   --------------------

   function Shell_From_XML
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : Glib.Xml_Int.Node_Ptr) return Custom_Component
   is
      Output : constant String := Get_Attribute (Command, "output", "@@");
      Outp   : GNAT.OS_Lib.String_Access := null;
      Show_Command : constant Boolean :=
        Get_Attribute (Command, "show-command", "true") = "true";
      Script : constant String :=
        Get_Attribute (Command, "lang", GPS_Shell_Name);
   begin
      if Output /= "@@" then
         Outp := new String'(Output);
      end if;

      return new Shell_Component_Record'
        (Command_Component_Record with
         Show_Command => Show_Command,
         Output       => Outp,
         Command      => new String'(Command.Value.all),
         Script       => Lookup_Scripting_Language (Kernel, Script));
   end Shell_From_XML;

   -----------------------
   -- External_From_XML --
   -----------------------

   function External_From_XML
     (Command : Glib.Xml_Int.Node_Ptr) return Custom_Component
   is
      Output : constant String := Get_Attribute (Command, "output", "@@");
      Outp   : GNAT.OS_Lib.String_Access := null;
      Show_Command : constant Boolean :=
        Get_Attribute (Command, "show-command", "true") = "true";
      Progress_Regexp : constant String :=
        Get_Attribute (Command, "progress-regexp", "");
      Progress_Current : constant Integer :=
        Safe_Value (Get_Attribute (Command, "progress-current", "0"));
      Progress_Final : constant Integer :=
        Safe_Value (Get_Attribute (Command, "progress-final", "0"));
      Progress_Hide : constant Boolean :=
        Get_Attribute (Command, "progress-hide", "true") = "true";

   begin
      if Output /= "@@" then
         Outp := new String'(Output);
      end if;

      return new External_Component_Record'
        (Command_Component_Record with
         Show_Command     => Show_Command,
         Output           => Outp,
         Command          => new String'(Command.Value.all),
         Progress_Regexp  => new String'(Progress_Regexp),
         Progress_Current => Progress_Current,
         Progress_Final   => Progress_Final,
         Progress_Hide    => Progress_Hide);
   end External_From_XML;

   --------------
   -- From_XML --
   --------------

   function From_XML
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : Glib.Xml_Int.Node_Ptr;
      Name    : String) return Components_Array_Access
   is
      N, M   : Node_Ptr := Command;
      Count  : Natural := 0;
      Result : Components_Array_Access;
      On_Failure : Integer := -1;
   begin
      while N /= null loop
         if N.Tag.all = "shell"
           or else N.Tag.all = "external"
         then
            Count := Count + 1;

         elsif N.Tag.all = "on-failure" then
            if N.Value /= null
              and then N.Value.all /= ""
            then
               Insert (Kernel,
                       "<on-failure> can only contain <shell> and <external>"
                       & " tags, not a string, in definition of action """
                       & Name & """",
                       Mode => Error);
               return new Components_Array (1 .. 0);
            end if;

            M := N.Child;
            while M /= null loop
               if M.Tag.all = "shell"
                 or else M.Tag.all = "external"
               then
                  Count := Count + 1;
               elsif M.Tag.all = "on-failure" then
                  Insert (Kernel,
                          "Nested <on-failure> nodes not supported, in "
                          & "definition of action """ & Name & """",
                          Mode => Error);
                  return new Components_Array (1 .. 0);
               else
                  Insert (Kernel,
                          "Unknown tag in action definition: " & M.Tag.all
                          & " in definition of action """ & Name & """",
                          Mode => Error);
                  return new Components_Array (1 .. 0);
               end if;

               M := M.Next;
            end loop;

         elsif N.Tag.all = "filter"
           or else N.Tag.all = "filter_and"
           or else N.Tag.all = "filter_or"
           or else N.Tag.all = "description"
         then
            --  Taken care of in custom_module.adb
            null;

         else
            Insert (Kernel,
                    "Unknown tag " & N.Tag.all & " in definition of action """
                    & Name & """",
                    Mode => Error);
            return new Components_Array (1 .. 0);
         end if;
         N := N.Next;
      end loop;

      Result := new Components_Array (1 .. Count);
      Count := Result'First;
      N := Command;

      while N /= null loop
         if N.Tag.all = "shell" then
            Result (Count) := (Shell_From_XML (Kernel, N), -1);
            Count := Count + 1;
         elsif N.Tag.all = "external" then
            Result (Count) := (External_From_XML (N), -1);
            Count := Count + 1;
         elsif N.Tag.all = "on-failure" then
            if Count = Result'First
              or else Result (Count - 1).Component.all not in
                External_Component_Record'Class
            then
               Insert (Kernel,
                       "<on-failure> can only follow an <external> node, in"
                       & " definition of action """ & Name & """",
                       Mode => Error);
               return new Components_Array (1 .. 0);
            end if;

            On_Failure := Count - 1;

            M := N.Child;
            while M /= null loop
               if M.Tag.all = "shell" then
                  Result (Count) := (Shell_From_XML (Kernel, M), On_Failure);
                  Count := Count + 1;
               elsif M.Tag.all = "external" then
                  Result (Count) := (External_From_XML (M), On_Failure);
                  Count := Count + 1;
               end if;

               M := M.Next;
            end loop;
         end if;

         N := N.Next;
      end loop;
      return Result;
   end From_XML;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item           : out Custom_Command_Access;
      Name           : String;
      Kernel         : Kernel_Handle;
      Command        : Glib.Xml_Int.Node_Ptr;
      Default_Output : String := Console_Output;
      Show_Command   : Boolean := True) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Default_Output_Destination := new String'(Default_Output);
      Item.Default_Show_Command := Show_Command;
      Item.Name := new String'(Name);
      Item.Components := From_XML (Kernel, Command, Name);
   end Create;

   ------------------------
   -- Project_From_Param --
   ------------------------

   function Project_From_Param
     (Param   : String;
      Context : Selection_Context_Access) return Project_Type
   is
      File : File_Selection_Context_Access;
      Project : Project_Type := No_Project;
   begin
      if Param (Param'First) = 'P' then
         Project := Get_Project (Get_Kernel (Context));

      elsif Context /= null
        and then Context.all in File_Selection_Context'Class
        and then Has_Project_Information
          (File_Selection_Context_Access (Context))
      then
         File := File_Selection_Context_Access (Context);
         Project := Project_Information (File);

      elsif Context /= null
        and then Context.all in File_Selection_Context'Class
        and then Has_File_Information
          (File_Selection_Context_Access (Context))
      then
         --  Since the editor doesn't provide the project, we emulate it
         --  here
         Project := Get_Project_From_File
           (Project_Registry (Get_Registry (Get_Kernel (Context)).all),
            File_Information (File_Selection_Context_Access (Context)),
            Root_If_Not_Found => False);
      end if;

      return Project;
   end Project_From_Param;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Custom_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Success  : Boolean := True;

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String;
      --  Substitution function for the various '%...' parameters
      --  Index is the number of the current command we are executing
      --  ??? What is the meaning of the comment about Index ?

      function Dollar_Substitution
        (Param  : String;
         Quoted : Boolean) return String;
      --  Substitution function for the "$1" .. "$N", "$*", "$@" parameters.

      function Execute_Simple_Command
        (Component : Custom_Component) return Boolean;
      --  Execute a single command, and return whether it succeeded.

      function Terminate_Command return Command_Return_Type;
      --  Terminate the command and free associated memory

      function Execute_Next_Command return Boolean;
      --  Execute the following commands, until the next external one.
      --  Return True if there are still commands to executed after that one.

      -------------------------
      -- Dollar_Substitution --
      -------------------------

      function Dollar_Substitution
        (Param  : String;
         Quoted : Boolean) return String
      is
         Length    : Natural := 0;
         Interval  : Natural;
         Is_Num    : Boolean;
         Result    : Natural;
         Multiple  : Boolean := False;
         First_Arg : Natural := 1;
      begin
         if Context.Args = null then
            return "";
         end if;

         if Param = "*" then
            Multiple := True;

         elsif Param (Param'Last) = '-' then
            Multiple := True;

            First_Arg := Safe_Value
              (Param (Param'First .. Param'Last - 1), Param'Length + 1);
         end if;

         if Multiple then
            if Quoted then
               Interval := 3;
            else
               Interval := 1;
            end if;

            for J in Context.Args'First - 1 + First_Arg ..
              Context.Args'Last
            loop
               if Context.Args (J) /= null then
                  Length :=
                    Length + (Context.Args (J).all'Length) * 2 + Interval;
               end if;
            end loop;

            declare
               Result : String (1 .. Length);
               Index  : Natural := 1;
            begin
               for J in Context.Args'First - 1 + First_Arg ..
                 Context.Args'Last
               loop
                  if Context.Args (J) /= null then
                     if Interval = 1 then
                        Result
                          (Index .. Index + Context.Args (J).all'Length) :=
                          Context.Args (J).all & ' ';

                        Index := Index +
                          Context.Args (J).all'Length + 1;
                     else
                        declare
                           Protect : constant String :=
                             String_Utils.Protect (Context.Args (J).all,
                                                   Protect_Quotes => Quoted);
                        begin
                           Result
                             (Index .. Index + Protect'Length + 2) :=
                             Protect & """ """;

                           Index := Index + Protect'Length + 3;
                        end;
                     end if;
                  end if;
               end loop;

               if Interval = 1 then
                  return String_Utils.Protect
                    (Result (1 .. Index - 1), Protect_Quotes => Quoted);
               else
                  return Result (1 .. Index - 4);
               end if;
            end;

         else
            Is_Num := True;

            for J in Param'Range loop
               if not Is_Decimal_Digit (Param (J)) then
                  Is_Num := False;
                  exit;
               end if;
            end loop;

            if Is_Num then
               Result := Natural'Value (Param);

               if Result in Context.Args'Range
                 and then Context.Args (Result) /= null
               then
                  return String_Utils.Protect
                    (Context.Args (Result).all, Protect_Quotes => Quoted);
               end if;
            end if;
         end if;

         return "";
      end Dollar_Substitution;

      ------------------
      -- Substitution --
      ------------------

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String
      is
         File    : File_Selection_Context_Access;
         Project : Project_Type := No_Project;
         Num     : Integer;
         Index   : Integer;
         Recurse, List_Dirs, List_Sources : Boolean;
      begin
         if Param = "f" or else Param = "F" then
            --  We know from Check_Save_Output that the context is valid
            File := File_Selection_Context_Access (Command.Execution.Context);

            if Param = "f" then
               return String_Utils.Protect
                 (Base_Name (File_Information (File)),
                  Protect_Quotes => Quoted);
            else
               return String_Utils.Protect
                 (Full_Name (File_Information (File)).all,
                  Protect_Quotes => Quoted);
            end if;

         elsif Param = "d" then
            --  We know from Check_Save_Output that the context is valid
            File := File_Selection_Context_Access (Command.Execution.Context);
            return String_Utils.Protect
              (Directory_Information (File), Protect_Quotes => Quoted);

         elsif Param (Param'First) = 'P' or else Param (Param'First) = 'p' then
            Project := Project_From_Param (Param, Command.Execution.Context);

            if Param = "pps" or else Param = "PPs" then
               if Project = No_Project then
                  return "";
               else
                  return String_Utils.Protect
                    ("-P" & Project_Path (Project), Protect_Quotes => Quoted);
               end if;
            end if;

            if Project = No_Project then
               Success := False;
               raise Invalid_Substitution;
            end if;

            if Param = "p" or else Param = "P" then
               return String_Utils.Protect
                 (Project_Name (Project), Protect_Quotes => Quoted);

            elsif Param = "pp" or else Param = "PP" then
               return String_Utils.Protect
                 (Project_Path (Project), Protect_Quotes => Quoted);

            else
               Recurse := Param (Param'First + 1) = 'r';

               if Recurse then
                  Index := Param'First + 2;
               else
                  Index := Param'First + 1;
               end if;

               if Index <= Param'Last then
                  List_Dirs    := Param (Index) = 'd';
                  List_Sources := Param (Index) = 's';

                  if Index < Param'Last and then Param (Index + 1) = 'f' then
                     --  Append the list to a file.
                     declare
                        File : File_Type;
                        Files_List : File_Array_Access;
                        List : String_Array_Access;
                     begin
                        Create (File);

                        if List_Dirs then
                           List := Source_Dirs (Project, Recurse);
                           if List /= null then
                              for K in List'Range loop
                                 Put_Line (File, List (K).all);
                              end loop;
                              Free (List);
                           end if;
                        end if;

                        if List_Sources then
                           Files_List := Get_Source_Files (Project, Recurse);
                           if Files_List /= null then
                              for K in Files_List'Range loop
                                 Put_Line
                                   (File, Full_Name (Files_List (K)).all);
                              end loop;
                              Unchecked_Free (Files_List);
                           end if;
                        end if;

                        declare
                           N : constant String := Name (File);
                        begin
                           Close (File);
                           return String_Utils.Protect
                             (N, Protect_Quotes => Quoted);
                        end;
                     end;

                  else
                     declare
                        Result : Unbounded_String;
                        List : String_Array_Access;
                        Files_List : File_Array_Access;
                     begin
                        if List_Dirs then
                           List := Source_Dirs (Project, Recurse);
                           if List /= null then
                              for K in List'Range loop
                                 Append (Result, '"' & List (K).all & """ ");
                              end loop;
                              Free (List);
                           end if;
                        end if;

                        if List_Sources then
                           Files_List := Get_Source_Files (Project, Recurse);
                           if Files_List /= null then
                              for K in Files_List'Range loop
                                 Append
                                   (Result,
                                    '"'
                                    & Full_Name (Files_List (K)).all & """ ");
                              end loop;
                              Unchecked_Free (Files_List);
                           end if;
                        end if;

                        return String_Utils.Protect
                          (To_String (Result), Protect_Quotes => Quoted);
                     end;
                  end if;
               end if;
            end if;

         else
            Num := Safe_Value (Param, Default => 0);

            --  Remove surrounding quotes if any. This is needed so that
            --  for instance of the function get_attributes_as_string
            --  from Python can be used to call an external tool with
            --  switches propertly interpreted.

            declare
               Output_Index : constant Integer := Output_Substitution
                 (Command, Command.Execution.Cmd_Index, Num);
               Output  : GNAT.OS_Lib.String_Access;
               Last   : Integer;
            begin
               if Output_Index = -1 then
                  return "";
               end if;

               Output := Command.Execution.Outputs (Output_Index);

               if Output = null or else Output.all = "" then
                  return "";
               end if;

               Last := Output'Last;
               while Last >= Output'First
                 and then Output (Last) = ASCII.LF
               loop
                  Last := Last - 1;
               end loop;

               if Output (Output'First) = '''
                 and then Output (Last) = '''
               then
                  return String_Utils.Protect
                    (Output (Output'First + 1 .. Last - 1),
                     Protect_Quotes => Quoted);

               elsif Output (Output'First) = '"'
                 and then Output (Output'Last) = '"'
               then
                  return String_Utils.Protect
                    (Output (Output'First + 1 .. Last - 1),
                     Protect_Quotes => Quoted);

               else
                  return String_Utils.Protect
                    (Output (Output'First .. Last),
                     Protect_Quotes => Quoted);
               end if;
            end;
         end if;

         --  Keep the percent sign, since this might be useful for the shell
         --  itself
         return '%' & Param;
      end Substitution;

      ----------------------------
      -- Execute_Simple_Command --
      ----------------------------

      function Execute_Simple_Command
        (Component : Custom_Component) return Boolean
      is
         --  Perform arguments substitutions for the command.

         Subst_Percent  : constant String := Substitute
           (Component.Command.all,
            Substitution_Char => '$',
            Callback          => Dollar_Substitution'Unrestricted_Access,
            Recursive         => False);
         Subst_Cmd_Line : constant String := Substitute
           (Subst_Percent,
            Substitution_Char => '%',
            Callback          => Substitution'Unrestricted_Access,
            Recursive         => False);
         Console        : Interactive_Console;
         Output_Location : GNAT.OS_Lib.String_Access;

         function To_String (P : in GNAT.OS_Lib.String_Access) return String;
         --  Return the contents of P, or the empty string if P is null.

         function Execute_Shell
           (Component : Shell_Component_Record'Class) return Boolean;
         function Execute_External
           (Component : External_Component_Record'Class) return Boolean;
         --  Execute a shell or an external component. Return the success
         --  status.

         ---------------
         -- To_String --
         ---------------

         function To_String (P : in GNAT.OS_Lib.String_Access) return String is
         begin
            if P = null then
               return "";
            else
               return P.all;
            end if;
         end To_String;

         -------------------
         -- Execute_Shell --
         -------------------

         function Execute_Shell
           (Component : Shell_Component_Record'Class) return Boolean
         is
            Errors  : aliased Boolean;
            Old_Dir : GNAT.OS_Lib.String_Access;
         begin
            if Context.Dir /= null then
               Old_Dir := new String'(Get_Current_Dir);
               Change_Dir (Context.Dir.all);
            end if;

            if Component.Script /= null then
               if Command.Execution.Save_Output
                 (Command.Execution.Cmd_Index)
               then
                  if Console /= null and then Component.Show_Command then
                     Insert (Console, Subst_Cmd_Line, Add_LF => True);
                  end if;

                  Command.Execution.Outputs (Command.Execution.Cmd_Index) :=
                    new String'
                      (Execute_Command
                           (Component.Script, Subst_Cmd_Line,
                            Hide_Output  => Output_Location.all = No_Output,
                            Show_Command => Component.Show_Command,
                            Console      => Console,
                            Errors       => Errors'Unchecked_Access));
               else
                  Execute_Command
                    (Component.Script, Subst_Cmd_Line,
                     Hide_Output  => Output_Location.all = No_Output,
                     Show_Command => Component.Show_Command,
                     Console      => Console,
                     Errors => Errors);
               end if;
            end if;

            if Context.Dir /= null then
               Change_Dir (Old_Dir.all);
               Free (Old_Dir);
            end if;

            return not Errors;
         end Execute_Shell;

         ----------------------
         -- Execute_External --
         ----------------------

         function Execute_External
           (Component : External_Component_Record'Class) return Boolean
         is
            Tmp  : GNAT.OS_Lib.String_Access;
            Args : String_List_Access;
         begin
            Trace (Me, "Executing external command " & Component.Command.all);

            Free (Command.Execution.Current_Output);
            Command.Execution.Current_Output := new String'("");

            Unchecked_Free (Command.Execution.Progress_Matcher);
            Command.Execution.Current_In_Regexp :=
              Integer'Max (0, Component.Progress_Current);
            Command.Execution.Total_In_Regexp   :=
              Integer'Max (0, Component.Progress_Final);
            Command.Execution.Hide_Progress := Component.Progress_Hide;

            if Component.Progress_Regexp.all /= "" then
               Command.Execution.Progress_Matcher := new Pattern_Matcher'
                 (Compile (Component.Progress_Regexp.all, Multiple_Lines));
            end if;

            Args := Argument_String_To_List (Subst_Cmd_Line);

            for J in Args'Range loop
               Tmp := Args (J);
               Args (J) := new String'(Unprotect (Tmp.all));
               Free (Tmp);
            end loop;

            Launch_Process
              (Command.Kernel,
               Command       => Args (Args'First).all,
               Arguments     => Args (Args'First + 1 .. Args'Last),
               Console       => null,
               Callback      => Store_Command_Output'Access,
               Exit_Cb       => Exit_Cb'Access,
               Success       => Success,
               Show_Command  => Component.Show_Command,
               Callback_Data => Convert (Custom_Command_Access (Command)),
               Line_By_Line  => False,
               Directory     => To_String (Context.Dir));
            Free (Args);

            Command.Execution.External_Process_Console := Console;
            Command.Execution.External_Process_In_Progress := True;
            return Success;
         end Execute_External;

      begin
         if Component.Output = null then
            Output_Location := Command.Default_Output_Destination;
         else
            Output_Location := Component.Output;
         end if;

         if Success and then Output_Location.all /= No_Output then
            Console := Create_Interactive_Console
              (Command.Kernel, Output_Location.all);
         end if;

         --  If substitution failed
         --  ??? Should write message to console
         if not Success then
            return False;
         end if;

         --  ??? Should use dispatching
         if Component.all in Shell_Component_Record'Class then
            Success := Execute_Shell
              (Shell_Component_Record'Class (Component.all));
         else
            Success := Execute_External
              (External_Component_Record'Class (Component.all));
         end if;

         if not Success then
            Trace
              (Me, "Execute_Simple_Command => Command returned with error");
         end if;

         return Success;

      exception
         when E : others =>
            Insert (Command.Kernel,
                    -("An unexpected error occurred while executing the custom"
                      & " command. See the log file for more information."),
                    Mode => Error);
            Trace (Exception_Handle,
                   "Unexpected exception: " & Exception_Information (E));
            return False;
      end Execute_Simple_Command;

      --------------------------
      -- Execute_Next_Command --
      --------------------------

      function Execute_Next_Command return Boolean is
         Current : Command_Component_Description;
      begin
         while Success
           and then Command.Execution.Cmd_Index <= Command.Components'Last
         loop
            Set_Progress
              (Command,
               Progress_Record'
                 (Activity => Running,
                  Current  => Command.Execution.Cmd_Index,
                  Total    => Command.Execution.Outputs'Length));

            Current := Command.Components (Command.Execution.Cmd_Index);
            if Current.On_Failure_For = Command.Execution.Current_Failure then
               Success := Execute_Simple_Command (Current.Component);

               if Current.Component.all in External_Component_Record'Class then
                  --  We'll have to run again to check for completion
                  return True;
               end if;
            end if;

            Command.Execution.Cmd_Index := Command.Execution.Cmd_Index + 1;
         end loop;

         --  No more command to execute
         return False;
      end Execute_Next_Command;

      -----------------------
      -- Terminate_Command --
      -----------------------

      function Terminate_Command return Command_Return_Type is
      begin
         Free (Command.Execution);

         Command_Finished (Command, Success);
         if Success then
            return Commands.Success;
         else
            return Failure;
         end if;
      end Terminate_Command;

      use type Glib.String_Ptr;
      Old_Dir        : GNAT.OS_Lib.String_Access;

   begin  --  Execute
      --  If there was an external command executing:
      if Command.Execution /= null then
         if Command.Execution.External_Process_In_Progress then
            return Execute_Again;
         end if;

         Command.Execution.Outputs (Command.Execution.Cmd_Index) :=
           Command.Execution.Current_Output;
         Command.Execution.Current_Output := null;

         if Command.Execution.Process_Exit_Status /= 0 then
            Command.Execution.Current_Failure := Command.Execution.Cmd_Index;
            Command.Execution.Process_Exit_Status := 0;
         end if;

         Command.Execution.Cmd_Index := Command.Execution.Cmd_Index + 1;

      else
         if Context.Dir /= null then
            Old_Dir := new String'(Get_Current_Dir);
            begin
               Change_Dir (Context.Dir.all);
            exception
               when Directory_Error =>
                  return Terminate_Command;
            end;
         end if;

         Command.Execution := new Custom_Command_Execution_Record;
         Command.Execution.Outputs     :=
           new Argument_List (Command.Components'Range);
         Command.Execution.Save_Output :=
           new Boolean_Array (Command.Components'Range);

         if Context.Context = null then
            Command.Execution.Context := Get_Current_Context (Command.Kernel);
         else
            Command.Execution.Context := Context.Context;
         end if;

         Command.Execution.Cmd_Index   := Command.Components'First;
         Ref (Command.Execution.Context);

         Check_Save_Output
           (Command.Kernel, Command,
            Command.Execution.Save_Output.all,
            Command.Execution.Context, Success);
         Clear_Consoles (Command.Kernel, Command);

         if Context.Dir /= null then
            Change_Dir (Old_Dir.all);
            Free (Old_Dir);
         end if;

         if not Success then
            return Terminate_Command;
         end if;
      end if;

      if Execute_Next_Command then
         return Execute_Again;
      else
         return Terminate_Command;
      end if;
   end Execute;

   ----------
   -- Name --
   ----------

   function Name (Command : access Custom_Command) return String is
   begin
      return Command.Name.all;
   end Name;

   ---------
   -- Get --
   ---------

   function Get
     (Iter : access Custom_Component_Iterator) return Command_Component is
   begin
      if Iter.Current <= Iter.Command.Components'Last then
         return Command_Component
           (Iter.Command.Components (Iter.Current).Component);
      else
         return null;
      end if;
   end Get;

   ----------------
   -- On_Failure --
   ----------------

   function On_Failure
     (Iter : access Custom_Component_Iterator) return Component_Iterator
   is
      Component : constant Command_Component := Get (Iter);
   begin
      if Component = null
        or else Component.all in Shell_Component_Record'Class
        or else Iter.Current = Iter.Command.Components'Last
        or else Iter.Command.Components (Iter.Current + 1).On_Failure_For /=
           Iter.Current
      then
         return null;
      end if;

      return new Custom_Component_Iterator'
        (Component_Iterator_Record with
         Command    => Iter.Command,
         Current    => Iter.Current + 1,
         On_Failure => Iter.Current);
   end On_Failure;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : access Custom_Component_Iterator) is
   begin
      loop
         Iter.Current := Iter.Current + 1;
         exit when Iter.Current > Iter.Command.Components'Last
           or else Iter.Command.Components (Iter.Current).On_Failure_For =
           Iter.On_Failure;
      end loop;
   end Next;

   ----------
   -- Free --
   ----------

   procedure Free (Iter : in out Custom_Component_Iterator) is
      pragma Unreferenced (Iter);
   begin
      null;
   end Free;

   --------------------
   -- Command_Editor --
   --------------------

   function Command_Editor
     (Command : access Custom_Command) return Gtk.Widget.Gtk_Widget
   is
      Box   : constant Command_Editor_Widget := new Command_Editor_Record;
      Hbox  : Gtk_Box;
      Label : Gtk_Label;
   begin
      Initialize_Vbox (Box, Homogeneous => False);

      Gtk_New (Box.Show_Command, -"Show command (default)");
      Set_Tip (Get_Tooltips (Command.Kernel), Box.Show_Command,
               -("Whether the text of the command should be displayed along"
                 & " with the actual output of the command. If the output is"
                 & " hidden, the text will not be shown."));
      Set_Active (Box.Show_Command, Command.Default_Show_Command);
      Pack_Start (Box, Box.Show_Command, Expand => False);

      Gtk_New (Box.Show_Output, -"Show command output (default)");
      Set_Tip (Get_Tooltips (Command.Kernel), Box.Show_Output,
               -("Whether the output of the command should be displayed"));
      Set_Active (Box.Show_Output, Command.Default_Output_Destination /= null
                 and then Command.Default_Output_Destination.all /= "none");
      Pack_Start (Box, Box.Show_Output, Expand => False);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => False);

      Gtk_New (Label, -"Output in:");
      Pack_Start (Hbox, Label, Expand => False);

      Gtk_New (Box.Output);
      if Command.Default_Output_Destination = null
        or else Command.Default_Output_Destination.all = ""
      then
         Set_Text (Box.Output, "Messages");
      else
         Set_Text (Box.Output, Command.Default_Output_Destination.all);
      end if;
      Set_Tip (Get_Tooltips (Command.Kernel), Box.Output,
               -("Name of the window in which the output will be displayed."
                 & " New windows are created as appropriate. This can be"
                 & " overriden for each command below"));
      Pack_Start (Hbox, Box.Output, Expand => True);

      return Gtk.Widget.Gtk_Widget (Box);
   end Command_Editor;

   ------------------------
   -- Update_From_Editor --
   ------------------------

   procedure Update_From_Editor
     (Command : access Custom_Command; Editor : Gtk.Widget.Gtk_Widget)
   is
      Ed : constant Command_Editor_Widget := Command_Editor_Widget (Editor);
   begin
      Command.Default_Show_Command := Get_Active (Ed.Show_Command);
      Free (Command.Default_Output_Destination);

      if Get_Active (Ed.Show_Output) then
         Command.Default_Output_Destination :=
           new String'(Get_Text (Ed.Output));
      else
         Command.Default_Output_Destination := new String'(No_Output);
      end if;
   end Update_From_Editor;

   -----------
   -- Start --
   -----------

   function Start
     (Command : access Custom_Command) return Component_Iterator is
   begin
      --  ??? Case where we don't have an XML, but a simple command
      return new Custom_Component_Iterator'
        (Component_Iterator_Record with
         Command    => Custom_Command_Access (Command),
         Current    => Command.Components'First,
         On_Failure => -1);
   end Start;

   ----------------------
   -- Component_Editor --
   ----------------------

   procedure Component_Editor
     (Component : access Custom_Component_Record'Class;
      Kernel    : access Kernel_Handle_Record'Class;
      Editor    : access Gtk_Box_Record'Class;
      Custom    : out Custom_Component_Editor;
      Size      : Gtk_Size_Group)
   is
      Box   : Gtk_Box;
      Label : Gtk_Label;
   begin
      Custom.Kernel := Kernel_Handle (Kernel);
      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Editor, Box, Expand => False);
      Gtk_New    (Label, -"Command: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New    (Custom.Command);
      Set_Text   (Custom.Command, Component.Command.all);
      Pack_Start (Box, Custom.Command, Expand => True, Fill => True);
      Set_Tip (Get_Tooltips (Kernel), Custom.Command,
               -"The command to execute");

      Gtk_New (Custom.Show_Command, -"Show command");
      Pack_Start (Editor, Custom.Show_Command, Expand => False);
      Set_Active (Custom.Show_Command, Component.Show_Command);
      Set_Tip (Get_Tooltips (Kernel), Custom.Show_Command,
               -("Whether the text of the command should be displayed in the"
                 & " output window. This overrides the default setup for the"
                 & " action"));

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Editor, Box, Expand => False);
      Gtk_New (Label, -"Output in: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Custom.Output);
      Pack_Start (Box, Custom.Output, Expand => True);
      Set_Tip (Get_Tooltips (Kernel), Custom.Output,
               -("Name of the window in which the output of the command should"
                 & " be displayed. A new window will be created if necessary."
                 & " This value can be inherited from the action's own setup"
                 & " by specifying """ & Output_Use_Default & """"));

      if Component.Output = null then
         Set_Text (Custom.Output, Output_Use_Default);
      else
         Set_Text (Custom.Output, Component.Output.all);
      end if;
   end Component_Editor;

   ----------------------
   -- Component_Editor --
   ----------------------

   function Component_Editor
     (Kernel    : access Kernel_Handle_Record'Class;
      Component : access Shell_Component_Record) return Gtk.Widget.Gtk_Widget
   is
      Editor : constant Shell_Component_Editor :=
        new Shell_Component_Editor_Record;
      Box    : Gtk_Box;
      Label  : Gtk_Label;
      List   : Gtk.Enums.String_List.Glist;
      Languages : constant Scripting_Language_Array :=
        Get_Scripting_Languages (Kernel);
      Size   : Gtk_Size_Group;
   begin
      Initialize_Vbox (Editor, Homogeneous => False);

      Gtk_New (Size);
      Component_Editor (Component, Kernel, Editor, Editor.Custom, Size);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Editor, Box, Expand => False);
      Gtk_New (Label, -"Language: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Editor.Lang);
      Pack_Start (Box, Editor.Lang, Expand => True);

      Set_Tip (Get_Tooltips (Kernel), Get_Entry (Editor.Lang),
               -("The language in which the command is written"));

      for L in Languages'Range loop
         Gtk.Enums.String_List.Append (List, Get_Name (Languages (L)));
      end loop;
      Set_Popdown_Strings (Editor.Lang, List);

      Set_Text (Get_Entry (Editor.Lang), Get_Name (Component.Script));

      return Gtk.Widget.Gtk_Widget (Editor);
   end Component_Editor;

   ------------------------
   -- Update_From_Editor --
   ------------------------

   procedure Update_From_Editor
     (Component : access Shell_Component_Record;
      Editor    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Ed : constant Shell_Component_Editor := Shell_Component_Editor (Editor);
   begin
      Component.Show_Command := Get_Active (Ed.Custom.Show_Command);
      Free (Component.Output);
      Component.Output := new String'(Get_Text (Ed.Custom.Output));
      Free (Component.Command);
      Component.Command := new String'(Get_Text (Ed.Custom.Command));
      Component.Script := Lookup_Scripting_Language
        (Ed.Custom.Kernel, Get_Text (Get_Entry (Ed.Lang)));
   end Update_From_Editor;

   ----------------------
   -- Component_Editor --
   ----------------------

   function Component_Editor
     (Kernel    : access Kernel_Handle_Record'Class;
      Component : access External_Component_Record)
      return Gtk.Widget.Gtk_Widget
   is
      Editor : constant External_Component_Editor :=
        new External_Component_Editor_Record;
      Label : Gtk_Label;
      Box   : Gtk_Box;
      Size  : Gtk_Size_Group;
   begin
      Initialize_Vbox (Editor, Homogeneous => False);
      Gtk_New (Size);
      Component_Editor (Component, Kernel, Editor, Editor.Custom, Size);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Editor, Box, Expand => False);
      Gtk_New (Label, -"Progress regexp: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Editor.Regexp);
      Pack_Start (Box, Editor.Regexp, Expand => True, Fill => True);
      Set_Text (Editor.Regexp, Component.Progress_Regexp.all);
      Set_Tip (Get_Tooltips (Kernel), Editor.Regexp,
               -("Regular expression, matched against each line of the output."
                 & " If it matches, its contents is analyzed to find the"
                 & " current progress of the action, and display a progress"
                 & " bar at the bottom of GPS's window. Leave this empty"
                 & " to ignore this feature."));

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Editor, Box, Expand => False);
      Gtk_New (Label, -"Current progress at: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Editor.Current, 0.0, 20.0, 1.0);
      Pack_Start (Box, Editor.Current, Expand => False);
      Set_Value (Editor.Current, Gdouble (Component.Progress_Current));
      Set_Tip (Get_Tooltips (Kernel), Editor.Current,
               -("Index of the open parenthesis the group that matches the"
                 & " current progress of the command. 0 is for the whole"
                 & " string matched by the regexp, 1 for the first open"
                 & " parenthesis, and so on..."));

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Editor, Box, Expand => False);
      Gtk_New (Label, -"Final progress at: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Editor.Final, 0.0, 20.0, 1.0);
      Pack_Start (Box, Editor.Final, Expand => False);
      Set_Value (Editor.Final, Gdouble (Component.Progress_Final));
      Set_Tip (Get_Tooltips (Kernel), Editor.Final,
               -("Index of the open parenthesis the group that matches the"
                 & " final progress of the command. This group should match"
                 & " a number which indicates the total to reach to complete"
                 & " the command. This total might change after each line of"
                 & " the output, since some tools might not know in advance"
                 & " how much processing they have to do."));

      Gtk_New (Editor.Hide, -"Hide progress output");
      Pack_Start (Editor, Editor.Hide, Expand => False);
      Set_Active (Editor.Hide, Component.Progress_Hide);
      Set_Tip (Get_Tooltips (Kernel), Editor.Hide,
               -("Whether the lines matching the regexp should be hidden"
                 & " when the output is displayed in the GPS window. This"
                 & " allows tools to output special lines just for GPS, but"
                 & " which are invisible to the user"));

      Gtk_New (Label, -"External: " & Component.Command.all);
      return Gtk.Widget.Gtk_Widget (Editor);
   end Component_Editor;

   ------------------------
   -- Update_From_Editor --
   ------------------------

   procedure Update_From_Editor
     (Component : access External_Component_Record;
      Editor    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Ed : constant External_Component_Editor :=
        External_Component_Editor (Editor);
   begin
      Component.Show_Command := Get_Active (Ed.Custom.Show_Command);
      Free (Component.Output);
      Component.Output := new String'(Get_Text (Ed.Custom.Output));
      Free (Component.Command);
      Component.Command := new String'(Get_Text (Ed.Custom.Command));

      Free (Component.Progress_Regexp);
      Component.Progress_Regexp := new String'(Get_Text (Ed.Regexp));
      Component.Progress_Current := Natural (Get_Value_As_Int (Ed.Current));
      Component.Progress_Final   := Natural (Get_Value_As_Int (Ed.Final));
      Component.Progress_Hide    := Get_Active (Ed.Hide);
   end Update_From_Editor;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Component : access Custom_Component_Record) return String is
   begin
      return Component.Command.all;
   end Get_Name;

   ------------
   -- To_XML --
   ------------

   procedure To_XML
     (Command     : access Custom_Command;
      Action_Node : Glib.Xml_Int.Node_Ptr)
   is
      Parent, Tmp : Node_Ptr;
   begin
      Set_Attribute (Action_Node, "output",
                     Command.Default_Output_Destination.all);

      if not Command.Default_Show_Command then
         Set_Attribute (Action_Node, "show-command", "false");
      end if;

      Parent := Action_Node;

      for C in Command.Components'Range loop
         if C /= Command.Components'First
           and then Command.Components (C).On_Failure_For >
             Command.Components (C - 1).On_Failure_For
         then
            Tmp := new Node;
            Tmp.Tag := new String'("on-failure");
            Add_Child (Parent, Tmp, Append => True);
            Parent := Tmp;
         end if;

         To_XML (Command.Components (C).Component, Parent);

         if C /= Command.Components'Last
           and then Command.Components (C).On_Failure_For >
             Command.Components (C + 1).On_Failure_For
         then
            Parent := Parent.Parent;
         end if;
      end loop;
   end To_XML;

   ------------
   -- To_XML --
   ------------

   procedure To_XML
     (Component   : access Shell_Component_Record;
      Action_Node : Glib.Xml_Int.Node_Ptr)
   is
      Node : constant Node_Ptr := new Glib.Xml_Int.Node;
   begin
      Node.Tag := new String'("shell");
      Node.Value := new String'(Component.Command.all);
      Add_Child (Action_Node, Node, Append => True);

      if not Component.Show_Command then
         Set_Attribute (Node, "show-command", "false");
      end if;

      if Component.Output /= null
        and then Component.Output.all /= Output_Use_Default
      then
         Set_Attribute (Node, "output", Component.Output.all);
      end if;

      Set_Attribute (Node, "lang", Get_Name (Component.Script));
   end To_XML;

   ------------
   -- To_XML --
   ------------

   procedure To_XML
     (Component   : access External_Component_Record;
      Action_Node : Glib.Xml_Int.Node_Ptr)
   is
      Node : constant Node_Ptr := new Glib.Xml_Int.Node;
   begin
      Node.Tag := new String'("external");
      Node.Value := new String'(Component.Command.all);
      Add_Child (Action_Node, Node, Append => True);

      if not Component.Show_Command then
         Set_Attribute (Node, "show-command", "false");
      end if;

      if Component.Output /= null
        and then Component.Output.all /= Output_Use_Default
      then
         Set_Attribute (Node, "output", Component.Output.all);
      end if;

      if Component.Progress_Regexp.all /= "" then
         Set_Attribute (Node, "progress-regexp",
                        Component.Progress_Regexp.all);
         Set_Attribute (Node, "progress-current",
                        Image (Component.Progress_Current));
         Set_Attribute (Node, "progress-final",
                        Image (Component.Progress_Final));

         if not Component.Progress_Hide then
            Set_Attribute (Node, "progress-hide", "false");
         end if;
      end if;
   end To_XML;

end Commands.Custom;
