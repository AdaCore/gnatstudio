-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2007, AdaCore                  --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Scripts;              use GNAT.Scripts;
with GNAT.Traces;               use GNAT.Traces;
with System;

with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib;                      use Glib;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Object;                use Gtk.Object;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Spin_Button;           use Gtk.Spin_Button;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Vbutton_Box;           use Gtk.Vbutton_Box;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Macros;         use GPS.Kernel.Macros;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Timeout;        use GPS.Kernel.Timeout;
with GUI_Utils;                 use GUI_Utils;
with Interactive_Consoles;      use Interactive_Consoles;
with Password_Manager;          use Password_Manager;
with Remote.Path.Translator;    use Remote, Remote.Path.Translator;
with String_Utils;              use String_Utils;
with Traces;

package body Commands.Custom is

   Me : constant Trace_Handle := Create ("Commands.Custom", Off);

   On_Failure_Node_Name : constant String := "on-failure";
   --  Name of the node that represents an on-failure in the list of components
   --  in the GUI editor for commands.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Boolean_Array, Boolean_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);

   ------------------------
   -- Component iterator --
   ------------------------

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

   -----------------------
   -- Custom components --
   -----------------------

   type Component_Type is (Component_Shell, Component_External);

   type Custom_Component_Record (The_Type : Component_Type) is
     new Command_Component_Record with
      record
         Show_Command : Boolean := True; --  "show-command" attribute
         Output       : String_Access;   --  use Default if this is null
         Command      : String_Access;

         case The_Type is
            when Component_Shell =>
               Script               : Scripting_Language;
            when Component_External =>
               Server               : Server_Type;      --  "server" attribute
               Check_Password       : Boolean := False; --  "check-password"
               Show_In_Task_Manager : Boolean;          --  "show-task-manager"
               Progress_Regexp      : String_Access;    --  "progress-regexp"
               Progress_Current     : Natural := 1;     --  "progress-current"
               Progress_Final       : Natural := 2;     --  "progress-final"
               Progress_Hide        : Boolean := True;  --  "progress-hide"
               --  Changes to this list must be reflected in the record
               --  Custom_Component_Editor_Record as well
         end case;
      end record;
   type Custom_Component is access all Custom_Component_Record'Class;

   function Get_Name
     (Component : access Custom_Component_Record) return String;
   --  See doc from inherited subprogram

   procedure Free (Component : in out Custom_Component);
   --  Free the memory occupied by Component

   -------------------------------
   -- Custom components editors --
   -------------------------------

   type Custom_Component_Editor_Record (The_Type : Component_Type) is
     new Gtk_Frame_Record with
      record
         Kernel       : Kernel_Handle;
         Command      : Gtk_Text_Buffer;
         View         : Gtk_Text_View;
         Show_Command : Gtk_Check_Button;
         Output       : Gtk_Entry;

         case The_Type is
            when Component_Shell =>
               Script       : Scripting_Language;
            when Component_External =>
               Regexp       : Gtk_Entry;
               Current      : Gtk_Spin_Button;
               Final        : Gtk_Spin_Button;
               Hide         : Gtk_Check_Button;
         end case;
      end record;
   type Custom_Component_Editor is
     access all Custom_Component_Editor_Record'Class;
   --  This widget is used to edit a Custom_Component_Record

   function To_XML
     (Editor         : access Custom_Component_Editor_Record'Class)
     return Glib.Xml_Int.Node_Ptr;
   --  Return the XML representation of the component edited in Editor

   procedure On_Command_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the text of a command is being changed

   ----------------------------
   -- Custom commands editor --
   ----------------------------

   type Component_Or_Failure is record
      Component : Custom_Component_Editor;
      On_Failure_For : Integer := -1;
   end record;
   --  List of widgets to edit the various components. When a component has
   --  been removed by the user, the Component field is set to null, but the
   --  element of the Component_Array is not removed, so that the Gtk_Tree
   --  still contains valid indexes into the array.

   type Component_Array is array (Natural range <>)
     of Component_Or_Failure;
   type Component_Array_Access is access all Component_Array;
   type Custom_Command_Editor_Record is new Command_Editor_Record with
      record
         Kernel       : Kernel_Handle;
         Components   : Component_Array_Access;
         Tree         : Gtk_Tree_View;
         Pane         : Gtk_Paned;
         Button_Remove : Gtk_Button;
      end record;
   type Custom_Command_Editor_Widget
     is access all Custom_Command_Editor_Record'Class;
   --  This widget is used to graphically edit a custom command

   function Create_Component_Editor
     (Component      : access Custom_Component_Record'Class;
      Command        : access Custom_Command_Editor_Record'Class;
      Kernel         : access Kernel_Handle_Record'Class;
      Default_Output : String) return Custom_Component_Editor;
   --  Return a widget used to edit Component.

   function Create_Empty_Component_Editor
     (Kernel         : access Kernel_Handle_Record'Class;
      Command        : access Custom_Command_Editor_Record'Class;
      Default_Output : String;
      The_Type       : Component_Type;
      Script         : Scripting_Language := null)
      return Custom_Component_Editor;
   --  Return an empty component editor.
   --  If Component is null, an empty editor is displayed, associated with
   --  The_Type. Otherwise, the parameter The_Type is unused

   function To_XML
     (Editor : access Custom_Command_Editor_Record)
      return Glib.Xml_Int.Node_Ptr;
   --  See inherited documentation

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Component_Array, Component_Array_Access);

   function Get_Selected_Component
     (Editor : access Custom_Command_Editor_Record'Class) return Integer;
   --  Return the index of the currently selected component, or -1 if none
   --  is selected.

   procedure Refresh_And_Select
     (Editor   : access Custom_Command_Editor_Record'Class;
      Selected : Integer := -1);
   --  Refresh the contents of Editor (list of components,...).
   --  If Selected is specified, the line for that component is selected

   procedure On_Component_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when a component was selected in the command editor

   type Component_Type_And_Lang is record
      The_Type : Component_Type;
      Script   : Scripting_Language;
   end record;
   package Component_Type_And_Lang_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Component_Type_And_Lang);
   use Component_Type_And_Lang_Callback;

   procedure On_Add
     (Editor : access Gtk_Widget_Record'Class;
      Data   : Component_Type_And_Lang);
   --  Called when a new component is added after the current one

   procedure On_Remove (Editor : access Gtk_Widget_Record'Class);
   --  Called when a component is being removed

   procedure On_Destroy (Editor : access Gtk_Widget_Record'Class);
   --  Called when the editor is being destroyed

   ----------------------
   -- Misc subprograms --
   ----------------------

   procedure Check_Save_Output
     (Command          : access Custom_Command'Class;
      Save_Output      : out Boolean_Array);
   --  Compute whether we should save the output of each commands. This depends
   --  on whether later commands reference this output through %1, %2,...

   procedure Clear_Consoles
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Custom_Command'Class);
   --  Clear all existing consoles that Command will use. Consoles that do not
   --  exist yet are not created.
   --  This is used so that the new output isn't mix with the output of
   --  previous run.
   --  The default GPS console is never cleared.

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

   procedure Free (Execution : in out Custom_Command_Execution);
   --  Free Execution and its contents

   function External_From_XML
     (Command                      : Glib.Xml_Int.Node_Ptr;
      Default_Show_In_Task_Manager : Boolean;
      Default_Show_Command         : Boolean) return Command_Component;
   function Shell_From_XML
     (Kernel               : access Kernel_Handle_Record'Class;
      Command              : Glib.Xml_Int.Node_Ptr;
      Default_Show_Command : Boolean) return Command_Component;
   --  Create a command component from an XML node

   function From_XML
     (Kernel                       : access Kernel_Handle_Record'Class;
      Command                      : Glib.Xml_Int.Node_Ptr;
      Name                         : String;
      Default_Show_In_Task_Manager : Boolean;
      Default_Show_Command         : Boolean) return Components_Array_Access;
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

   type Custom_Callback_Data is new Callback_Data_Record with record
      Command : Custom_Command_Access;
   end record;
   type Custom_Callback_Data_Access is access all Custom_Callback_Data;

   -------------------
   -- Create_Filter --
   -------------------

   function Create_Filter
     (Command : Glib.Xml_Int.Node_Ptr) return Action_Filter
   is
      Filter : Macro_Filter;
      N      : Node_Ptr := Command;
   begin
      while N /= null loop
         if N.Value /= null then
            Filter := Create_Filter (N.Value.all, Filter);
         end if;
         N := N.Next;
      end loop;

      return Action_Filter (Filter);
   end Create_Filter;

   -------------
   -- Exit_Cb --
   -------------

   procedure Exit_Cb (Data : Process_Data; Status : Integer) is
      D : constant Custom_Callback_Data_Access :=
            Custom_Callback_Data_Access (Data.Callback_Data);
   begin
      D.Command.Execution.External_Process_In_Progress := False;
      D.Command.Execution.Process_Exit_Status := Status;
   end Exit_Cb;

   --------------------------
   -- Store_Command_Output --
   --------------------------

   procedure Store_Command_Output (Data : Process_Data; Output : String) is

      D       : constant Custom_Callback_Data_Access :=
                  Custom_Callback_Data_Access (Data.Callback_Data);
      Command : constant Custom_Command_Access := D.Command;

      procedure Append (S : in out String_Access; Value : String);
      --  Append Value to S

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

      ------------
      -- Append --
      ------------

      procedure Append (S : in out String_Access; Value : String) is
         Previous : GNAT.Strings.String_Access;
      begin
         if S = null then
            S := new String'(Value);
         else
            Previous := S;
            S        := new String'(Previous.all & Value);
            Free (Previous);
         end if;
      end Append;

      Current, Total : Integer := 0;
      Save_Output    : constant Boolean :=
                         Command.Execution.Save_Output
                           (Command.Execution.Cmd_Index);

      Index, EOL     : Integer;

   begin
      if Command.Execution.Check_Password then

         declare
            Matched : Match_Array (0 .. 2);
            Force   : Boolean;
            Idx     : Integer :=
                        Ada.Strings.Fixed.Index (Output, "" & ASCII.LF);
         begin
            --  Only the first received line is taken into account for custom
            --  commands.
            --  This allows custom tools to output text that matches the
            --  password regexp without reacting here.
            if Idx < Output'First then
               Idx := Output'Last;
            end if;

            --  Retrieve password prompt if any
            Match (Get_Default_Password_Regexp,
                   Output (Output'First .. Idx),
                   Matched);

            if Matched (0) /= No_Match then
               Force := Command.Execution.Nb_Password > 0;
               Command.Execution.Nb_Password :=
                 Command.Execution.Nb_Password + 1;

               declare
                  Password : constant String :=
                    Get_Tool_Password (Command.Name.all,
                                       Force);
               begin
                  if Password /= "" then
                     Send (Data.Descriptor.all, Password);

                     --  Do not output password prompt
                     return;

                  end if;
               end;

            end if;

            --  Retrieve passphrase prompt if any
            Match (Get_Default_Passphrase_Regexp,
                   Output (Output'First .. Idx),
                   Matched);

            if Matched (0) /= No_Match then
               Force := Command.Execution.Nb_Password > 0;
               Command.Execution.Nb_Password :=
                 Command.Execution.Nb_Password + 1;

               declare
                  Password : constant String :=
                    Get_Passphrase
                      (Output (Matched (1).First .. Matched (1).Last),
                       Force);
               begin
                  if Password /= "" then
                     Send (Data.Descriptor.all, Password);

                     --  Do not output password prompt
                     return;

                  end if;
               end;
            end if;
         end;

         --  If we received a line without matching any password regexp, and
         --  this line is not empty, then we won't look for password anymore.
         if Output'Length > 0 and then Output /= "" & ASCII.LF then
            Command.Execution.Check_Password := False;
         end if;

      end if;

      if Command.Execution.Progress_Matcher /= null then
         declare
            Matched : Match_Array
              (0 .. Integer'Max (Command.Execution.Current_In_Regexp,
                                 Command.Execution.Total_In_Regexp));
         begin
            Index := Output'First;
            while Index <= Output'Last loop
               EOL := Index;
               while EOL <= Output'Last
                 and then Output (EOL) /= ASCII.LF
               loop
                  EOL := EOL + 1;
               end loop;
               if EOL > Output'Last then
                  EOL := EOL - 1;
               end if;

               Match (Command.Execution.Progress_Matcher.all,
                      Output (Index .. EOL), Matched);
               if Matched (Command.Execution.Current_In_Regexp) = No_Match
                 or else Matched (Command.Execution.Total_In_Regexp) = No_Match
               then
                  Insert (Output (Index .. EOL));
                  if Save_Output then
                     Append
                       (Command.Execution.Current_Output,
                        Output (Index .. EOL));
                  end if;

               else
                  declare
                     Outp : constant String :=
                       Output (Index .. Matched (0).First - 1)
                       & Output (Matched (0).Last + 1 .. EOL);
                  begin
                     if Command.Execution.Hide_Progress then
                        Insert (Outp);
                     else
                        Insert (Output (Index .. EOL));
                     end if;

                     if Save_Output then
                        Append (Command.Execution.Current_Output, Outp);
                     end if;
                  end;

                  Current := Safe_Value
                    (Output
                       (Matched (Command.Execution.Current_In_Regexp).First ..
                          Matched (Command.Execution.Current_In_Regexp).Last));
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
               Index := EOL + 1;
            end loop;
         end;

      elsif Save_Output then
         Insert (Output);
         Append (Command.Execution.Current_Output, Output);
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
      procedure Clear_Console (Name : GNAT.Strings.String_Access);
      --  Clear a specific console

      -------------------
      -- Clear_Console --
      -------------------

      procedure Clear_Console (Name : GNAT.Strings.String_Access) is
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
         Clear_Console
           (Custom_Component (Command.Components (C).Component).Output);
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
     (Command          : access Custom_Command'Class;
      Save_Output      : out Boolean_Array)
   is
      Index : Natural;

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String;
      --  Check whether the command has a '%' + digit parameter

      ------------------
      -- Substitution --
      ------------------

      function Substitution
        (Param  : String;
         Quoted : Boolean) return String
      is
         pragma Unreferenced (Quoted);
         Sub_Index, Ref_Index : Integer;
      begin
         Sub_Index := Safe_Value (Param, Default => 0);
         if Sub_Index /= 0 then
            Ref_Index := Output_Substitution
              (Command, Index, Sub_Index);

            if Ref_Index >= Command.Components'First then
               Save_Output (Ref_Index) := True;
            end if;
         end if;

         return "";
      end Substitution;

   begin
      Save_Output := (others => False);

      Index := Command.Components'First;
      while Index <= Command.Components'Last loop
         declare
            S : constant String := String_Utils.Substitute
              (Custom_Component
                 (Command.Components (Index).Component).Command.all,
               Substitution_Char => GPS.Kernel.Macros.Special_Character,
               Callback          => Substitution'Unrestricted_Access,
               Recursive         => False);
            pragma Unreferenced (S);
         begin
            null;
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
         Free              (Execution.Current_Output);
         GNAT.Strings.Free (Execution.Outputs);
         Unchecked_Free    (Execution.Save_Output);
         Unchecked_Free    (Execution.Progress_Matcher);
         Unchecked_Free    (Execution);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Component : in out Custom_Component) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Custom_Component_Record'Class, Custom_Component);
   begin
      Free (Component.Output);
      Free (Component.Command);
      case Component.The_Type is
         when Component_Shell =>
            null;
         when Component_External =>
            Free (Component.Progress_Regexp);
      end case;
      Unchecked_Free (Component);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Custom_Command) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Components_Array, Components_Array_Access);
   begin
      Free (X.Default_Output_Destination);
      Free (X.Name);
      Free (X.Execution);

      for C in X.Components'Range loop
         Free (Custom_Component (X.Components (C).Component));
      end loop;
      Unchecked_Free (X.Components);
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item    : out Custom_Command_Access;
      Name    : String;
      Kernel  : Kernel_Handle;
      Command : String;
      Script  : Scripting_Language) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Name   := new String'(Name);
      Item.Components := new Components_Array'
        (1 => (Component => new Custom_Component_Record'
                 (Command_Component_Record with
                  The_Type     => Component_Shell,
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
     (Kernel               : access Kernel_Handle_Record'Class;
      Command              : Glib.Xml_Int.Node_Ptr;
      Default_Show_Command : Boolean) return Command_Component
   is
      Output       : constant String :=
                       Get_Attribute (Command, "output", "@@");
      Script       : constant String :=
                       Get_Attribute (Command, "lang", GPS_Shell_Name);
      Show_Command : constant String :=
                       Get_Attribute (Command, "show-command");
      Show_C       : Boolean := Show_Command = "true";
      Outp         : GNAT.Strings.String_Access := null;

   begin
      if Output /= "@@" then
         Outp := new String'(Output);
      end if;

      if Show_Command = "" then
         Show_C := Default_Show_Command;
      end if;

      return new Custom_Component_Record'
        (Command_Component_Record with
         The_Type     => Component_Shell,
         Show_Command => Show_C,
         Output       => Outp,
         Command      => new String'(Command.Value.all),
         Script       =>
           Lookup_Scripting_Language (Get_Scripts (Kernel), Script));
   end Shell_From_XML;

   -----------------------
   -- External_From_XML --
   -----------------------

   function External_From_XML
     (Command                      : Glib.Xml_Int.Node_Ptr;
      Default_Show_In_Task_Manager : Boolean;
      Default_Show_Command         : Boolean) return Command_Component
   is
      Output            : constant String :=
                            Get_Attribute (Command, "output", "@@");
      Show_Command      : constant String :=
                            Get_Attribute (Command, "show-command");
      Show_C            : Boolean := Show_Command = "true";
      Show_Task_Manager : constant String :=
                            Get_Attribute (Command, "show-task-manager");
      Show_TM           : Boolean := Show_Task_Manager = "false";
      Progress_Regexp   : constant String :=
                            Get_Attribute (Command, "progress-regexp", "");
      Progress_Current  : constant Integer :=
                            Safe_Value
                              (Get_Attribute
                                 (Command, "progress-current", "1"));
      Progress_Final    : constant Integer :=
                            Safe_Value
                              (Get_Attribute (Command, "progress-final", "2"));
      Progress_Hide     : constant Boolean :=
                            Get_Attribute
                              (Command, "progress-hide", "true") = "true";
      Server            : constant String :=
                            Get_Attribute (Command, "server", "gps_server");
      Check_Password    : constant Boolean :=
                            Get_Attribute
                              (Command, "check-password", "false") = "true";
      Outp              : GNAT.Strings.String_Access := null;
      Server_T          : Server_Type;
   begin
      if Show_Task_Manager = "" then
         Show_TM := Default_Show_In_Task_Manager or else Progress_Regexp /= "";
      end if;

      if Show_Command = "" then
         Show_C := Default_Show_Command;
      end if;

      if Output /= "@@" then
         Outp := new String'(Output);
      end if;

      begin
         Server_T := Server_Type'Value (Server);
      exception
         when Constraint_Error =>
            Server_T := GPS_Server;
      end;

      return new Custom_Component_Record'
        (Command_Component_Record with
         The_Type             => Component_External,
         Server               => Server_T,
         Check_Password       => Check_Password,
         Show_Command         => Show_C,
         Output               => Outp,
         Command              => new String'(Command.Value.all),
         Show_In_Task_Manager => Show_TM,
         Progress_Regexp      => new String'(Progress_Regexp),
         Progress_Current     => Progress_Current,
         Progress_Final       => Progress_Final,
         Progress_Hide        => Progress_Hide);
   end External_From_XML;

   --------------
   -- From_XML --
   --------------

   function From_XML
     (Kernel                       : access Kernel_Handle_Record'Class;
      Command                      : Glib.Xml_Int.Node_Ptr;
      Name                         : String;
      Default_Show_In_Task_Manager : Boolean;
      Default_Show_Command         : Boolean) return Components_Array_Access
   is
      N, M       : Node_Ptr := Command;
      Count      : Natural := 0;
      Result     : Components_Array_Access;
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
            Result (Count) :=
              (Shell_From_XML (Kernel, N, Default_Show_Command), -1);
            Count := Count + 1;

         elsif N.Tag.all = "external" then
            Result (Count) := (External_From_XML
              (N,
               Default_Show_In_Task_Manager => Default_Show_In_Task_Manager,
               Default_Show_Command         => Default_Show_Command),
              -1);
            Count := Count + 1;

         elsif N.Tag.all = "on-failure" then
            if Count = Result'First
              or else Custom_Component (Result (Count - 1).Component).The_Type
                /= Component_External
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
                  Result (Count) :=
                    (Shell_From_XML (Kernel, M, Default_Show_Command),
                     On_Failure);
                  Count := Count + 1;

               elsif M.Tag.all = "external" then
                  Result (Count) := (External_From_XML
                    (M,
                       Default_Show_In_Task_Manager =>
                         Default_Show_In_Task_Manager,
                       Default_Show_Command         => Default_Show_Command),
                    On_Failure);
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
     (Item                 : out Custom_Command_Access;
      Name                 : String;
      Kernel               : Kernel_Handle;
      Command              : Glib.Xml_Int.Node_Ptr;
      Default_Output       : String := Console_Output;
      Show_Command         : Boolean := True;
      Show_In_Task_Manager : Boolean := False) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Default_Output_Destination := new String'(Default_Output);
      Item.Default_Show_Command := Show_Command;
      Item.Name := new String'(Name);
      Item.Components := From_XML
        (Kernel, Command, Name,
         Default_Show_In_Task_Manager => Show_In_Task_Manager,
         Default_Show_Command         => Show_Command);
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Custom_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Success        : Boolean := True;
      Current_Server : Server_Type := GPS_Server;
      Old_Server     : Server_Type := GPS_Server;

      function Dollar_Substitution
        (Param  : String;
         Quoted : Boolean) return String;
      --  Substitution function for the "$1" .. "$N", "$*", "$@" parameters.

      function Substitution
        (Param  : String; Quoted : Boolean) return String;
      --  Substitution function for '%1', '%2',...

      function Execute_Simple_Command
        (Component : Custom_Component) return Boolean;
      --  Execute a single command, and return whether it succeeded.

      function Terminate_Command return Command_Return_Type;
      --  Terminate the command and free associated memory

      function Execute_Next_Command return Boolean;
      --  Execute the following commands, until the next external one.
      --  Return True if there are still commands to be executed after that
      --  one.

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
         if Param = "repeat" then
            return Integer'Image (Context.Repeat_Count);
         elsif Param = "remaining" then
            return Integer'Image (Context.Remaining_Repeat);
         end if;

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

            for J in
              Context.Args'First - 1 + First_Arg .. Context.Args'Last
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
               for J in
                 Context.Args'First - 1 + First_Arg .. Context.Args'Last
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
                                       String_Utils.Protect
                                         (Context.Args (J).all,
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
         Num   : Integer;
         Done  : aliased Boolean := False;
         Macro : constant String :=
                   Substitute (Param, Command.Execution.Context,
                               Quoted, Done'Access, Current_Server);
      begin
         if Done then
            return Macro;
         end if;

         Num := Safe_Value (Param, Default => 0);

         --  Remove surrounding quotes if any. This is needed so that
         --  for instance the function get_attributes_as_string
         --  from Python can be used to call an external tool with
         --  switches propertly interpreted.

         declare
            Output_Index : constant Integer := Output_Substitution
              (Command, Command.Execution.Cmd_Index, Num);
            Output       : GNAT.Strings.String_Access;
            Last         : Integer;
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
              and then Last > Output'First and then Output (Last) = '''
            then
               return String_Utils.Protect
                 (Output (Output'First + 1 .. Last - 1),
                  Protect_Quotes => Quoted);

            elsif Output (Output'First) = '"'
              and then Last > Output'First and then Output (Output'Last) = '"'
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
      end Substitution;

      ----------------------------
      -- Execute_Simple_Command --
      ----------------------------

      function Execute_Simple_Command
        (Component : Custom_Component) return Boolean
      is
         --  Perform arguments substitutions for the command

         Subst_Percent   : constant String := Substitute
           (Component.Command.all,
            Substitution_Char => '$',
            Callback          => Dollar_Substitution'Unrestricted_Access,
            Recursive         => False);
         Console         : Interactive_Console;
         Output_Location : GNAT.Strings.String_Access;

         function To_String (P : in GNAT.Strings.String_Access) return String;
         --  Return the contents of P, or the empty string if P is null

         function Execute_Shell
           (Component : Custom_Component_Record'Class) return Boolean;
         function Execute_External
           (Component : Custom_Component_Record'Class) return Boolean;
         --  Execute a shell or an external component. Return the success
         --  status.

         ---------------
         -- To_String --
         ---------------

         function To_String
           (P : in GNAT.Strings.String_Access) return String is
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
           (Component : Custom_Component_Record'Class) return Boolean
         is
            Errors         : aliased Boolean;
            Old_Dir        : GNAT.Strings.String_Access;
            --  has to be determined here so that Current_Server is
            --  correctly set:
            Subst_Cmd_Line : constant String := Substitute
              (Subst_Percent,
               Substitution_Char => GPS.Kernel.Macros.Special_Character,
               Callback          => Substitution'Unrestricted_Access,
               Recursive         => False);
         begin
            if Context.Dir /= null then
               Old_Dir := new String'(Get_Current_Dir);
               Change_Dir (Context.Dir.all);
            end if;

            if Component.Script /= null then
               --  Output the command whether or not we are saving its output
               if Console /= null and then Component.Show_Command then
                  Insert (Console, Subst_Cmd_Line, Add_LF => True);
               end if;

               if Command.Execution.Save_Output
                 (Command.Execution.Cmd_Index)
               then
                  Command.Execution.Outputs (Command.Execution.Cmd_Index) :=
                    new String'
                      (Execute_Command
                           (Component.Script, Subst_Cmd_Line,
                            Hide_Output  => Output_Location.all = No_Output,
                            Show_Command => Component.Show_Command,
                            Console      =>
                              Get_Or_Create_Virtual_Console (Console),
                            Errors       => Errors'Unchecked_Access));
               else
                  Execute_Command
                    (Component.Script, Subst_Cmd_Line,
                     Hide_Output  => Output_Location.all = No_Output,
                     Show_Command => Component.Show_Command,
                     Console      => Get_Or_Create_Virtual_Console (Console),
                     Errors       => Errors);
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
           (Component : Custom_Component_Record'Class) return Boolean
         is
            Subst_Cmd_Line  : constant String := Trim
              (Substitute
                 (Subst_Percent,
                  Substitution_Char => GPS.Kernel.Macros.Special_Character,
                  Callback          => Substitution'Unrestricted_Access,
                  Recursive         => False),
               Left  => To_Set (' ' & ASCII.LF & ASCII.HT),
               Right => Ada.Strings.Maps.Null_Set);
            Tmp             : GNAT.Strings.String_Access;
            Args            : String_List_Access;
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
            Command.Execution.Check_Password := Component.Check_Password;
            Command.Execution.Nb_Password    := 0;

            if Component.Progress_Regexp.all /= "" then
               Command.Execution.Progress_Matcher := new Pattern_Matcher'
                 (Compile (Component.Progress_Regexp.all,
                           Multiple_Lines or Single_Line));
            end if;

            Args := GNAT.OS_Lib.Argument_String_To_List (Subst_Cmd_Line);

            for J in Args'Range loop
               Tmp := Args (J);
               Args (J) := new String'(Unprotect (Tmp.all));
               Free (Tmp);
            end loop;

            if Args'Length = 0 then
               Trace (Me, "Cannot launch empty command");
               Success := False;
               Command.Execution.External_Process_In_Progress := False;
               Command.Execution.Process_Exit_Status := 1;

            else
               --  Set the console before launching the process, so that
               --  synchronous calls correctly output.

               Command.Execution.External_Process_Console := Console;

               Launch_Process
                 (Command.Kernel,
                  Command              => Args (Args'First).all,
                  Arguments            => Args (Args'First + 1 .. Args'Last),
                  Server               => Component.Server,
                  Console              => Console,
                  Callback             => Store_Command_Output'Access,
                  Exit_Cb              => Exit_Cb'Access,
                  Success              => Success,
                  Show_Command         => Component.Show_Command,

                  --  Showing the output is already handled by the callback
                  --  Store_Command_Output.

                  Show_Output          => False,

                  Callback_Data        => new Custom_Callback_Data'
                    (Command => Custom_Command_Access (Command)),
                  Show_In_Task_Manager => Component.Show_In_Task_Manager,
                  Line_By_Line         => False,
                  Synchronous          => Context.Synchronous,
                  Directory            => To_Remote (To_String (Context.Dir),
                    Component.Server),
                  Created_Command      => Command.Sub_Command);
               Free (Args);

               Command.Execution.External_Process_In_Progress := Success;
            end if;

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

         case Component.The_Type is
            when Component_Shell =>
               Success := Execute_Shell (Component.all);

            when Component_External =>
               Old_Server     := Current_Server;
               Current_Server := Component.Server;
               Success        := Execute_External (Component.all);
               Current_Server := Old_Server;
         end case;

         if not Success then
            Trace
              (Me, "Execute_Simple_Command => Command returned with error");
         end if;

         return Success;

      exception
         when E : others =>
            Trace (Traces.Exception_Handle, E);
            Insert (Command.Kernel,
                    -("An unexpected error occurred while executing the custom"
                      & " command. See the log file for more information."),
                    Mode => Error);
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
            Current := Command.Components (Command.Execution.Cmd_Index);
            if Current.On_Failure_For = Command.Execution.Current_Failure then
               Success := Execute_Simple_Command
                 (Custom_Component (Current.Component));

               if not Context.Synchronous
                 and then Custom_Component (Current.Component).The_Type =
                   Component_External
               then
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

      Old_Dir        : GNAT.Strings.String_Access;

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
           new GNAT.Strings.String_List (Command.Components'Range);
         Command.Execution.Save_Output :=
           new Boolean_Array (Command.Components'Range);

         if Context.Context = No_Context then
            Command.Execution.Context := Get_Current_Context (Command.Kernel);
         else
            Command.Execution.Context := Context.Context;
         end if;

         Command.Execution.Cmd_Index  := Command.Components'First;

         Check_Save_Output (Command, Command.Execution.Save_Output.all);
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
        or else Component.all not in Custom_Component_Record'Class
        or else Custom_Component (Component).The_Type = Component_Shell
        or else Iter.Current = Iter.Command.Components'Last
        or else Iter.Command.Components
          (Iter.Current + 1).On_Failure_For /= Iter.Current
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

   ---------------------------
   -- Create_Command_Editor --
   ---------------------------

   function Create_Command_Editor
     (Command : access Custom_Command;
      Kernel  : access Kernel_Handle_Record'Class) return Command_Editor
   is
      Box         : constant Custom_Command_Editor_Widget :=
                      new Custom_Command_Editor_Record;
      Command_Cst : aliased String := "command";
      Hbox        : Gtk_Box;
      Bbox        : Gtk_Vbutton_Box;
      Scrolled    : Gtk_Scrolled_Window;
      Button      : Gtk_Button;
   begin
      Initialize_Vbox (Box, Homogeneous => False);
      Box.Kernel := Kernel_Handle (Kernel);

      --  First Pane: The list of components + action buttons
      Gtk_New_Vpaned (Box.Pane);
      Pack_Start (Box, Box.Pane, True, True);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack1 (Box.Pane, Hbox, True, True);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Hbox, Scrolled, Expand => True, Fill => True);

      Box.Tree := Create_Tree_View
        (Column_Types       => (1 => GType_String,
                                2 => GType_Int),
         Column_Names       => (1 => Command_Cst'Unchecked_Access),
         Show_Column_Titles => False,
         Sortable_Columns   => False);
      Add (Scrolled, Box.Tree);

      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Start);
      Pack_Start (Hbox, Bbox, Expand => False);

      Gtk_New_From_Stock_And_Label (Button, Stock_Add, -"Add Python");
      Pack_Start (Bbox, Button);
      Component_Type_And_Lang_Callback.Object_Connect
        (Button, Signal_Clicked, On_Add'Access, Box,
         (Component_Shell,
          Lookup_Scripting_Language (Get_Scripts (Kernel), "Python")));

      Gtk_New_From_Stock_And_Label (Button, Stock_Add, -"Add Shell");
      Pack_Start (Bbox, Button);
      Component_Type_And_Lang_Callback.Object_Connect
        (Button, Signal_Clicked, On_Add'Access, Box,
         (Component_Shell,
          Lookup_Scripting_Language (Get_Scripts (Kernel), GPS_Shell_Name)));

      Gtk_New_From_Stock_And_Label (Button, Stock_Add, -"Add External");
      Pack_Start (Bbox, Button);
      Component_Type_And_Lang_Callback.Object_Connect
        (Button, Signal_Clicked, On_Add'Access, Box,
         (Component_External, null));

      Gtk_New_From_Stock (Box.Button_Remove, Stock_Remove);
      Set_Sensitive (Box.Button_Remove, False);
      Pack_Start (Bbox, Box.Button_Remove);
      Widget_Callback.Object_Connect
        (Box.Button_Remove, Signal_Clicked, On_Remove'Access, Box);

      --  Now fill the tree

      Box.Components := new Component_Array (Command.Components'Range);
      for C in Command.Components'Range loop
         if Command.Default_Output_Destination = null
           or else Command.Default_Output_Destination.all = ""
         then
            Box.Components (C) :=
              (Component => Create_Component_Editor
                 (Custom_Component (Command.Components (C).Component),
                  Box,
                  Kernel,
                  Default_Output => "Messages"),
               On_Failure_For => Command.Components (C).On_Failure_For);
         else
            Box.Components (C) :=
              (Component => Create_Component_Editor
                 (Custom_Component (Command.Components (C).Component),
                  Box,
                  Kernel,
                  Default_Output => Command.Default_Output_Destination.all),
               On_Failure_For => Command.Components (C).On_Failure_For);
         end if;
      end loop;

      Widget_Callback.Object_Connect
        (Get_Selection (Box.Tree), Gtk.Tree_Selection.Signal_Changed,
         On_Component_Changed'Access,
         Slot_Object => Box);

      Refresh_And_Select (Box, Selected => Command.Components'First);

      Widget_Callback.Connect (Box, Signal_Destroy, On_Destroy'Access);

      return Command_Editor (Box);
   end Create_Command_Editor;

   -------------
   -- Refresh --
   -------------

   procedure Refresh_And_Select
     (Editor   : access Custom_Command_Editor_Record'Class;
      Selected : Integer := -1)
   is
      Model         : constant Gtk_Tree_Store :=
                        Gtk_Tree_Store (Get_Model (Editor.Tree));
      Parent        : Gtk_Tree_Iter;
      Iter          : Gtk_Tree_Iter;
      First, Last   : Gtk_Text_Iter;
      Selected_Iter : Gtk_Tree_Iter := Null_Iter;
      On_Failure    : Gtk_Tree_Iter := Null_Iter;
   begin
      Clear (Model);
      Parent := Null_Iter;

      for C in Editor.Components'Range loop
         --  Unless the component has been destroyed by the user
         if Editor.Components (C).Component /= null then
            if C > Editor.Components'First
              and then Editor.Components (C).On_Failure_For >
              Editor.Components (C - 1).On_Failure_For
            then
               Parent := On_Failure;
            end if;

            Get_Start_Iter (Editor.Components (C).Component.Command, First);
            Get_End_Iter   (Editor.Components (C).Component.Command, Last);

            Append (Model, Iter, Parent);
            Set (Model, Iter, 0, Get_Text
                 (Editor.Components (C).Component.Command, First, Last));
            Set (Model, Iter, 1, Gint (C));

            if Editor.Components (C).Component.The_Type
              = Component_External
            then
               --  Always create the on-failure node, so that one can add
               --  failure conditions
               Append (Model, On_Failure, Iter);
               Set (Model, On_Failure, 0, On_Failure_Node_Name);
               Set (Model, On_Failure, 1, -1);
            end if;

            if C = Selected then
               Selected_Iter := Iter;
            end if;

            if C < Editor.Components'Last
              and then Parent /= Null_Iter
              and then
                (Editor.Components (C).On_Failure_For >
                   Editor.Components (C + 1).On_Failure_For
                 or else Editor.Components (C + 1).On_Failure_For = -1)
            then
               Parent := Gtk.Tree_Model.Parent
                 (Gtk_Tree_Model (Model),
                  Gtk.Tree_Model.Parent
                    (Gtk_Tree_Model (Model), Parent));
            end if;
         end if;
      end loop;

      --  ??? Should preserve the expansion status
      Expand_All (Editor.Tree);

      if Selected_Iter /= Null_Iter then
         Select_Iter (Get_Selection (Editor.Tree), Selected_Iter);
      end if;
   end Refresh_And_Select;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Custom_Command_Editor_Widget :=
             Custom_Command_Editor_Widget (Editor);
   begin
      if Ed.Components /= null then
         for C in Ed.Components'Range loop
            Destroy (Ed.Components (C).Component);
         end loop;
         Unchecked_Free (Ed.Components);

         --  All other fields are destroyed with the standard gtk+ mechanism
      end if;
   end On_Destroy;

   ----------------------------
   -- Get_Selected_Component --
   ----------------------------

   function Get_Selected_Component
     (Editor : access Custom_Command_Editor_Record'Class) return Integer
   is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      Get_Selected (Get_Selection (Editor.Tree), Model, Iter);
      if Iter /= Null_Iter then
         return Integer (Get_Int (Model, Iter, 1));
      end if;
      return -1;
   end Get_Selected_Component;

   ---------------
   -- On_Remove --
   ---------------

   procedure On_Remove (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Custom_Command_Editor_Widget :=
                    Custom_Command_Editor_Widget (Editor);
      Component : constant Integer := Get_Selected_Component (Ed);
      Comp      : Component_Array_Access := Ed.Components;
      Count     : Natural := 1;
   begin
      if Component /= -1 then
         Destroy (Comp (Component).Component);

         --  Remove all the commands associated with on-failure:
         for C in Component + 1 .. Comp'Last loop
            exit when Comp (C).On_Failure_For < Component;
            Count := Count + 1;
         end loop;

         Ed.Components := new Component_Array
           (Comp'First .. Comp'Last - Count);
         Ed.Components (Comp'First .. Component - 1) :=
           Comp (Comp'First .. Component - 1);
         if Component + Count <= Comp'Last then
            Ed.Components (Component .. Ed.Components'Last) :=
              Comp (Component + Count .. Comp'Last);
         end if;
         Unchecked_Free (Comp);
         Refresh_And_Select (Ed, Integer'Min (Ed.Components'Last, Component));
      end if;
   end On_Remove;

   --------------------------
   -- On_Component_Changed --
   --------------------------

   procedure On_Component_Changed (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Custom_Command_Editor_Widget :=
                    Custom_Command_Editor_Widget (Editor);
      Component : Integer;
   begin
      --  Preserve the current component editor, since we'll need it to
      --  generate the XML in the end. It will be destroyed when the editor
      --  itself is destroyed.
      if Get_Child2 (Ed.Pane) /= null then
         Ref (Get_Child2 (Ed.Pane));
         Remove (Ed.Pane, Get_Child2 (Ed.Pane));
      end if;

      Component := Get_Selected_Component (Ed);
      if Component /= -1 then
         Pack2 (Ed.Pane, Ed.Components (Component).Component, True, True);
         Show_All (Ed.Pane);
      end if;
      Set_Sensitive (Ed.Button_Remove, Component /= -1);
   end On_Component_Changed;

   ------------------------
   -- On_Command_Changed --
   ------------------------

   procedure On_Command_Changed (Editor : access Gtk_Widget_Record'Class) is
      Ed          : constant Custom_Command_Editor_Widget :=
                      Custom_Command_Editor_Widget (Editor);
      Model       : Gtk_Tree_Model;
      Iter        : Gtk_Tree_Iter;
      Component   : Custom_Component_Editor;
      First, Last : Gtk_Text_Iter;

   begin
      Get_Selected (Get_Selection (Ed.Tree), Model, Iter);
      if Iter /= Null_Iter then
         Component := Custom_Component_Editor (Get_Child2 (Ed.Pane));
         Get_Start_Iter (Component.Command, First);
         Get_End_Iter   (Component.Command, Last);
         Set (Gtk_Tree_Store (Model), Iter, 0,
              Get_Text (Component.Command, First, Last));
      end if;
   end On_Command_Changed;

   ------------
   -- On_Add --
   ------------

   procedure On_Add
     (Editor : access Gtk_Widget_Record'Class;
      Data   : Component_Type_And_Lang)
   is
      Ed             : constant Custom_Command_Editor_Widget :=
                         Custom_Command_Editor_Widget (Editor);
      Component      : Integer;
      Comp           : Component_Array_Access := Ed.Components;
      Model          : Gtk_Tree_Model;
      Iter           : Gtk_Tree_Iter;
      On_Failure_For : Integer := -1;

   begin
      Get_Selected (Get_Selection (Ed.Tree), Model, Iter);
      if Iter /= Null_Iter then
         Component := Integer (Get_Int (Model, Iter, 1));
         if Component = -1
           and then Get_String (Model, Iter, 0) =
           On_Failure_Node_Name
         then
            --  On a "on-failure" node ?
            Iter := Parent (Model, Iter);
            Component := Integer (Get_Int (Model, Iter, 1));
            On_Failure_For := Component;

         else
            --  New component will have same on-failure level as currently
            --  selected one. We should skip all on-failure nodes for the
            --  current node as well.
            On_Failure_For := Comp (Component).On_Failure_For;
            for C in Component + 1 .. Comp'Last loop
               exit when Comp (C).On_Failure_For < On_Failure_For
                 or else Comp (C).On_Failure_For = -1;
               Component := C;
            end loop;
         end if;
      else
         Component := Comp'Last;
      end if;

      Ed.Components := new Component_Array (Comp'First .. Comp'Last + 1);
      Ed.Components (Comp'First .. Component) :=
        Comp (Comp'First .. Component);
      if Component + 1 <= Comp'Last then
         Ed.Components (Component + 2 .. Ed.Components'Last) :=
           Comp (Component + 1 .. Comp'Last);
      end if;

      Unchecked_Free (Comp);
      Ed.Components (Component + 1) :=
        (Component      => Create_Empty_Component_Editor
           (The_Type       => Data.The_Type,
            Command        => Ed,
            Kernel         => Ed.Kernel,
            Default_Output => "",
            Script         => Data.Script),
         On_Failure_For => On_Failure_For);
      Refresh_And_Select (Ed, Component + 1);
      Grab_Focus (Ed.Components (Component + 1).Component.View);
   end On_Add;

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

   -----------------------------------
   -- Create_Empty_Component_Editor --
   -----------------------------------

   function Create_Empty_Component_Editor
     (Kernel         : access Kernel_Handle_Record'Class;
      Command        : access Custom_Command_Editor_Record'Class;
      Default_Output : String;
      The_Type       : Component_Type;
      Script         : Scripting_Language := null)
      return Custom_Component_Editor
   is
      Comp : aliased Custom_Component_Record (The_Type);
   begin
      case The_Type is
         when Component_Shell =>
            Comp.Script := Script;
         when Component_External =>
            null;
      end case;

      return Create_Component_Editor
        (Comp'Unchecked_Access, Command, Kernel, Default_Output);
   end Create_Empty_Component_Editor;

   -----------------------------
   -- Create_Component_Editor --
   -----------------------------

   function Create_Component_Editor
     (Component      : access Custom_Component_Record'Class;
      Command        : access Custom_Command_Editor_Record'Class;
      Kernel         : access Kernel_Handle_Record'Class;
      Default_Output : String) return Custom_Component_Editor
   is
      Editor    : Custom_Component_Editor;
      Size      : Gtk_Size_Group;
      Box, HBox : Gtk_Box;
      Label     : Gtk_Label;
      Scrolled  : Gtk_Scrolled_Window;
      Iter      : Gtk_Text_Iter;
   begin
      Editor := new Custom_Component_Editor_Record
        (The_Type => Component.The_Type);
      Gtk.Frame.Initialize (Editor);
      Editor.Kernel := Kernel_Handle (Kernel);

      case Component.The_Type is
         when Component_Shell =>
            Set_Label (Editor, Get_Name (Component.Script));
         when Component_External =>
            Set_Label (Editor, -"External");
      end case;

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Editor, Box);

      Gtk_New (Size);

      Gtk_New_Hbox (HBox, Homogeneous => False);
      Pack_Start (Box, HBox, Expand => False);

      Gtk_New (Label, -"Output: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (HBox, Label, Expand => False);

      Gtk_New (Editor.Output);
      Pack_Start (HBox, Editor.Output, Expand => True);
      Set_Tip
        (Get_Tooltips (Kernel), Editor.Output,
         -("Name of the window in which the output of the command should"
           & " be displayed. A new window will be created if necessary."));
      if Component.Output = null then
         if Default_Output = Console_Output then
            Set_Text (Editor.Output, "Messages");
         else
            Set_Text (Editor.Output, Default_Output);
         end if;
      else
         Set_Text (Editor.Output, Component.Output.all);
      end if;

      Gtk_New (Editor.Show_Command, -"Display command");
      Pack_Start (HBox, Editor.Show_Command, Expand => False);
      Set_Active (Editor.Show_Command, Component.Show_Command);
      Set_Tip (Get_Tooltips (Kernel), Editor.Show_Command,
               -("Whether the text of the command should be displayed in the"
                 & " output window. This overrides the default setup for the"
                 & " action"));

      Gtk_New_Hbox (HBox, Homogeneous => False);
      Pack_Start (Box, HBox, Expand => False);
      Gtk_New    (Label, -"Command: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (HBox, Label, Expand => False);

      Gtk_New (Scrolled);
      Set_Shadow_Type (Scrolled, Shadow_In);
      Pack_Start (HBox, Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Gtk_New (Editor.Command);
      Gtk_New (Editor.View, Editor.Command);
      Add (Scrolled, Editor.View);

      Get_End_Iter (Editor.Command, Iter);
      if Component.Command /= null then
         Insert (Editor.Command, Iter, Component.Command.all);
      end if;

      Widget_Callback.Object_Connect
        (Editor.Command, Gtk.Text_Buffer.Signal_Changed,
         On_Command_Changed'Access, Command);

      case Editor.The_Type is
         when Component_Shell =>
            Editor.Script := Component.Script;

         when Component_External =>
            Gtk_New_Hbox (HBox, Homogeneous => False);
            Pack_Start (Box, HBox, Expand => False);

            Gtk_New (Label, -"Progress: ");
            Set_Alignment (Label, 0.0, 0.5);
            Add_Widget (Size, Label);
            Pack_Start (HBox, Label, Expand => False);

            Gtk_New (Editor.Regexp);
            Pack_Start (HBox, Editor.Regexp, Expand => True, Fill => True);
            if Component.Progress_Regexp /= null then
               Set_Text (Editor.Regexp, Component.Progress_Regexp.all);
            end if;
            Set_Tip (Get_Tooltips (Kernel), Editor.Regexp,
              -("Regular expression, matched against each line of the output."
                & " If it matches, its contents is analyzed to find the"
                & " current progress of the action, and display a progress"
                & " bar at the bottom of GPS's window. Leave this empty"
                & " to ignore this feature."));

            Gtk_New (Label, -"Current:");
            Set_Alignment (Label, 0.0, 0.5);
            Pack_Start (HBox, Label, Expand => False);

            Gtk_New (Editor.Current, 0.0, 20.0, 1.0);
            Pack_Start (HBox, Editor.Current, Expand => False);
            Set_Value (Editor.Current, Gdouble (Component.Progress_Current));
            Set_Tip (Get_Tooltips (Kernel), Editor.Current,
              -("Index of the open parenthesis the group that matches the"
                & " current progress of the command. 0 is for the whole"
                & " string matched by the regexp, 1 for the first open"
                & " parenthesis, and so on..."));

            Gtk_New (Label, -"Total:");
            Set_Alignment (Label, 0.0, 0.5);
            Pack_Start (HBox, Label, Expand => False);

            Gtk_New (Editor.Final, 0.0, 20.0, 1.0);
            Pack_Start (HBox, Editor.Final, Expand => False);
            Set_Value (Editor.Final, Gdouble (Component.Progress_Final));
            Set_Tip (Get_Tooltips (Kernel), Editor.Final,
              -("Index of the open parenthesis the group that matches the"
                & " final progress of the command. This group should match"
                & " a number which indicates the total to reach to complete"
                & " the command. This total might change after each line of"
                & " the output, since some tools might not know in advance"
                & " how much processing they have to do."));

            Gtk_New (Editor.Hide, -"Hide matches");
            Pack_Start (HBox, Editor.Hide, Expand => False);
            Set_Active (Editor.Hide, Component.Progress_Hide);
            Set_Tip (Get_Tooltips (Kernel), Editor.Hide,
              -("Whether the lines matching the regexp should be hidden"
                & " when the output is displayed in the GPS window. This"
                & " allows tools to output special lines just for GPS, but"
                & " which are invisible to the user"));

            Gtk_New_Hbox (HBox, Homogeneous => False);
            Pack_Start (Box, HBox, Expand => False);

            --  ??? Should be able to edit check-password
            --  ??? Should be able to edit server
            --  ??? Should be able to edit Show_In_Task_Manager
      end case;

      return Editor;
   end Create_Component_Editor;

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

   function To_XML
     (Editor : access Custom_Command_Editor_Record)
      return Glib.Xml_Int.Node_Ptr
   is
      Action, Tmp, Parent, XML : Node_Ptr;
   begin
      Action     := new Node;
      Action.Tag := new String'("action");

      Parent := Action;

      for C in Editor.Components'Range loop
         --  If the component has not been removed
         if Editor.Components (C).Component /= null then
            if C /= Editor.Components'First
              and then Editor.Components (C).On_Failure_For >
              Editor.Components (C - 1).On_Failure_For
            then
               Tmp := new Node;
               Tmp.Tag := new String'("on-failure");
               Add_Child (Parent, Tmp, Append => True);
               Parent := Tmp;
            end if;

            XML := To_XML (Editor.Components (C).Component);
            if XML /= null then
               Add_Child (Parent, XML, Append => True);
            end if;

            if C /= Editor.Components'Last
              and then Editor.Components (C).On_Failure_For >
              Editor.Components (C + 1).On_Failure_For
            then
               Parent := Parent.Parent;
            end if;
         end if;
      end loop;
      return Action;
   end To_XML;

   ------------
   -- To_XML --
   ------------

   function To_XML
     (Editor         : access Custom_Component_Editor_Record'Class)
      return Glib.Xml_Int.Node_Ptr
   is
      Output      : constant String := Get_Text (Editor.Output);
      Node        : Node_Ptr;
      First, Last : Gtk_Text_Iter;

   begin
      Get_Start_Iter (Editor.Command, First);
      Get_End_Iter   (Editor.Command, Last);

      declare
         Command : constant String := Get_Text (Editor.Command, First, Last);
      begin
         if Command = "" then
            return null;
         end if;

         Node := new Glib.Xml_Int.Node;

         case Editor.The_Type is
            when Component_Shell =>
               Node.Tag := new String'("shell");
               Set_Attribute (Node, "lang", Get_Name (Editor.Script));

            when Component_External =>
               Node.Tag := new String'("external");
               if Get_Text (Editor.Regexp) /= "" then
                  Set_Attribute
                    (Node, "progress-regexp", Get_Text (Editor.Regexp));
                  Set_Attribute
                    (Node, "progress-current",
                     Image (Integer (Get_Value_As_Int (Editor.Current))));
                  Set_Attribute
                    (Node, "progress-final",
                     Image (Integer (Get_Value_As_Int (Editor.Final))));
                  if not Get_Active (Editor.Hide) then
                     Set_Attribute (Node, "progress-hide", "false");
                  end if;
               end if;
         end case;

         Node.Value := new String'(Command);

         if not Get_Active (Editor.Show_Command) then
            Set_Attribute (Node, "show-command", "false");
         end if;

         if Output /= "" and then Output /= "Messages" then
            Set_Attribute (Node, "output", Output);
         end if;
         return Node;
      end;
   end To_XML;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Command : in out Custom_Command) is
   begin
      if Command.Execution /= null then
         Command.Execution.Cmd_Index := Command.Components'First;
         Interrupt_Queue (Command.Kernel, Get_Command (Command.Sub_Command));
      end if;
   end Interrupt;

end Commands.Custom;
