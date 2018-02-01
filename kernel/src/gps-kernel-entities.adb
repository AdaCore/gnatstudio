------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with Basic_Types;
with Commands.Generic_Asynchronous; use Commands;
with Commands.Interactive;          use Commands, Commands.Interactive;
with GPS.Kernel.Actions;            use GPS.Kernel.Actions;
with Glib.Convert;
with Gtk.Box;                       use Gtk.Box;
with Gtk.Button;                    use Gtk.Button;
with Gtk.Check_Button;              use Gtk.Check_Button;
with Gtk.Dialog;                    use Gtk.Dialog;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Frame;                     use Gtk.Frame;
with Gtk.Radio_Button;              use Gtk.Radio_Button;
with Gtk.Stock;                     use Gtk.Stock;
with Gtk.Vbutton_Box;               use Gtk.Vbutton_Box;
with Gtk.Widget;                    use Gtk.Widget;
with Gtkada.Handlers;               use Gtkada.Handlers;
with GNAT.Strings;                  use GNAT.Strings;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Projects;             use GNATCOLL.Projects;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.Utils;
with GNATCOLL.Xref;                 use GNATCOLL.Xref;
with GPS.Default_Styles;            use GPS.Default_Styles;
with GPS.Scripts.Commands;          use GPS.Scripts.Commands;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                use GPS.Kernel.MDI;
with GPS.Kernel.Messages;           use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup;    use GPS.Kernel.Messages.Markup;
with GPS.Kernel.Messages.Simple;    use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules.UI;         use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;            use GPS.Kernel.Project;
with GPS.Kernel.Scripts;            use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;       use GPS.Kernel.Task_Manager;
with GPS.Kernel.Xref;               use GPS.Kernel.Xref;
with GPS.Location_View;
with GPS.Intl;                      use GPS.Intl;
with Histories;                     use Histories;
with Language;                      use Language;
with Language.Ada;                  use Language.Ada;
with String_Utils;                  use String_Utils;
with UTF8_Utils;
with Xref;                          use Xref;

package body GPS.Kernel.Entities is
   Me : constant Trace_Handle := Create ("GPS.KERNEL.ENTITIES");

   Locations_At_A_Time : constant := 20;
   --  Number of locations that will be inserted in the locations view in
   --  each idle processing.

   References_Command_Class_Name : constant String := "ReferencesCommand";

   Call_Graph_Message_Flags : constant Message_Flags :=
     (Editor_Side => True,
      Editor_Line => False,
      Locations   => True);
   --  Visibility of call graph's messages in the system at whole

   package Entity_Ref_List is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Root_Entity_Reference'Class);
   use Entity_Ref_List;

   type Custom_Filter is record
      Db        : General_Xref_Database;

      Ref_Kinds : GNAT.Strings.String_List_Access;
      --  The reference kinds' name that should be displayed, or none for all.
      --  Any null value is ignored in this array.

      Filter    : Reference_Kind_Filter;
      --  One of the predefined filters
   end record;

   procedure Find_All_References_Internal
     (Kernel             : access Kernel_Handle_Record'Class;
      Info               : Root_Entity'Class;
      Category_Title     : String;
      Show_Caller        : Boolean;
      Filter             : in out Custom_Filter;
      Include_Overriding : Boolean := False);
   --  Internal implementation for Find_All_References_From_Contextual,
   --  Find_All_Writes_From_Contextual and Find_All_Reads_From_Contextual.
   --  Starts a background search for all references.
   --  This procedure will free Filter.

   function All_Refs_Category
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Root_Entity'Class;
      Local_Only         : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File;
      All_From_Same_File : Boolean) return String;
   --  Return the category title when doing a find all refs on a given entity.
   --  If All_From_Same_File is true, we will in fact list all entities
   --  imported form the same file as Entity.
   --  If Local_Only is true, then the references are only in the current file

   type Find_All_Refs_Command is new Interactive_Command with record
      Locals_Only     : Boolean := False;
      Recurse_Project : Boolean := True;
      Writes_Only     : Boolean := False;
      Reads_Only      : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Find_All_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Find_Specific_Refs_Command
   is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Find_Specific_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Subprogram_Entity_Filter is new Action_Filter_Record with record
      Kernel : Kernel_Handle;
   end record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Entity_Filter;
      Context : Selection_Context) return Boolean;

   type Filters_Buttons is array (Natural range <>) of Gtk_Check_Button;
   type Filters_Buttons_Access is access Filters_Buttons;
   type References_Filter_Dialog_Record is new Gtk_Dialog_Record with record
      Filters : Filters_Buttons_Access;
   end record;
   type References_Filter_Dialog is access all
     References_Filter_Dialog_Record'Class;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Filters_Buttons, Filters_Buttons_Access);

   procedure Unselect_All_Filters (Dialog : access Gtk_Widget_Record'Class);
   procedure Select_All_Filters (Dialog : access Gtk_Widget_Record'Class);
   --  Select or unselect all filters in "Find references..."

   function Is_Valid
     (Self : Custom_Filter; Ref : Root_Entity_Reference'Class) return Boolean;
   procedure Free (Self : in out Custom_Filter);

   procedure Parse_All_Refs
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Root_Entity'Class;
      Locals_Only        : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File;
      All_From_Same_File : Boolean;
      Show_Caller        : Boolean;
      Filter             : in out Custom_Filter;
      Include_Overriding : Boolean := False);
   --  Internal implementation of find_all_references.
   --  If All_From_Same_File is True, then all entities imported from the same
   --  file as Entity and referenced in Local_File, as Entity are
   --  displayed.
   --  This procedure will free Filter.

   function Is_Read_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
     (Ref.Is_Read_Reference);
   function Is_Write_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
     (Ref.Is_Write_Reference);
   function Is_Read_Or_Write_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
     (Ref.Is_Read_Or_Write_Reference);
   function Is_Read_Or_Implicit_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
     (Ref.Is_Read_Or_Implicit_Reference);
   function Is_Read_Or_Write_Or_Implicit_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
     (Ref.Is_Read_Or_Write_Or_Implicit_Reference);

   type Entity_Idle_Data is record
      Kernel             : Kernel_Handle;
      Iter               : Root_Reference_Iterator_Ref;
      Entity             : Root_Entity_Ref;
      Filter             : Custom_Filter;
      Iter_Started       : Boolean;
      Show_Caller        : Boolean;
      Category           : GNAT.Strings.String_Access;
      Include_Overriding : Boolean;
      Count              : Natural := 0;
   end record;

   procedure Destroy_Idle (Data : in out Entity_Idle_Data);
   --  Called when the idle loop is destroyed

   package Xref_Commands is new Commands.Generic_Asynchronous
     (Entity_Idle_Data, Destroy_Idle);

   procedure Find_Next_Reference
     (Data    : in out Entity_Idle_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Find the next reference to the entity in D

   procedure Print_Ref
     (Kernel       : access Kernel_Handle_Record'Class;
      Ref          : Root_Entity_Reference'Class;
      Name         : String;
      Category     : String;
      Show_Caller  : Boolean;
      Sort_In_File : Boolean);
   --  Display a reference in the locations tree, after looking for the
   --  directory containing File.
   --  Category corresponds to the purpose of the print. All references
   --  corresponding to the same category will be printed as a group.
   --  If Show_Caller is true, the full name of the caller will also be
   --  displayed.
   --  If Sort_In_File is true, then the new entry is inserted before the first
   --  entry with a higher line number

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "Entity" commands

   procedure References_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handle shell commands related of ReferencesCommand class

   type Callback_Data_Access is access all Callback_Data'Class;
   type Add_To_List_User_Data is new Commands_User_Data_Record with record
      Data : Callback_Data_Access;

      Use_Parent_For_Key : Boolean := True;
      --  If this is True, then Parent should be used as the key for entries
      --  in the list. Otherwise, Entity will be used.
   end record;
   type Add_To_List_User_Data_Access is access all Add_To_List_User_Data'Class;
   --  Add a new entity to the returned value in D.Data.

   overriding function On_Entity_Found
     (D                   : access Add_To_List_User_Data;
      Entity              : Root_Entity'Class;
      Parent              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean;
   --  See inherited documentation.

   type References_Command is new Root_Command with record
      Kernel        : Kernel_Handle;
      Iter          : Root_Reference_Iterator_Ref;
      Locations     : Entity_Ref_List.List;
      Show_Ref_Kind : Boolean;
   end record;
   type References_Command_Access is access all References_Command'Class;
   overriding function Execute
     (Command : access References_Command) return Command_Return_Type;
   overriding procedure Primitive_Free (Command : in out References_Command);

   procedure Put_Locations_In_Return
     (Command       : access References_Command'Class;
      Data          : in out Callback_Data'Class;
      Show_Ref_Kind : Boolean);
   --  Put on the result of Data the list of entities found in the command

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (Command : in out References_Command) is
      El : Root_Reference_Iterator'Class := Command.Iter.Element;
   begin
      Destroy (El);
   end Primitive_Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access References_Command) return Command_Return_Type
   is
      Iter_Ref : constant Root_Reference_Iterator_Refs.Reference_Type :=
        Command.Iter.Reference;
   begin
      for J in 1 .. 15 loop
         exit when At_End (Iter_Ref);
         declare
            Ref : constant Root_Entity_Reference'Class :=
              Get (Iter_Ref.Element.all);
         begin
            if Ref /= No_Root_Entity_Reference then
               Command.Locations.Append (Ref);
            end if;
         end;

         Next (Iter_Ref);
      end loop;

      Set_Progress
        (Command,
         (Activity => Unknown,
          Current  => Get_Current_Progress (Iter_Ref),
          Total    => Get_Total_Progress (Iter_Ref)));

      if At_End (Iter_Ref) then
         Set_Progress
           (Command,
            (Activity => Unknown,
             Current  => Get_Total_Progress (Iter_Ref),
             Total    => Get_Total_Progress (Iter_Ref)));

         return Success;
      else
         return Execute_Again;
      end if;
   end Execute;

   -----------------------------
   -- Put_Locations_In_Return --
   -----------------------------

   procedure Put_Locations_In_Return
     (Command       : access References_Command'Class;
      Data          : in out Callback_Data'Class;
      Show_Ref_Kind : Boolean)
   is
      Inst : Class_Instance;
      Loc  : General_Location;
   begin
      if not Show_Ref_Kind then
         Set_Return_Value_As_List (Data);
      end if;

      for Ref of Command.Locations loop

         Loc := Get_Location (Ref);
         Inst := Create_File_Location
           (Script => Get_Script (Data),
            File   => Create_File
              (Script => Get_Script (Data),
               File   => Loc.File),
            Line   => Loc.Line,
            Column => Loc.Column);

         if Show_Ref_Kind then
            Set_Return_Value (Data, Get_Display_Kind (Ref));
            Set_Return_Value_Key (Data, Inst);
         else
            Set_Return_Value (Data, Inst);
         end if;
      end loop;
   end Put_Locations_In_Return;

   ---------------------
   -- On_Entity_Found --
   ---------------------

   overriding function On_Entity_Found
     (D                   : access Add_To_List_User_Data;
      Entity              : Root_Entity'Class;
      Parent              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean
   is
      pragma Unreferenced (Through_Dispatching);
      Loc : General_Location;
   begin
      if not Is_Renaming then
         Loc := Get_Location (Ref);
         Set_Return_Value
           (D.Data.all,
            Create_File_Location
              (Get_Script (D.Data.all),
               Create_File (Get_Script (D.Data.all), Loc.File),
               Loc.Line,
               Loc.Column));
      else
         Set_Return_Value (D.Data.all, -"<renaming>");
      end if;

      if D.Use_Parent_For_Key then
         Set_Return_Value_Key
           (D.Data.all,
            Create_Entity (Get_Script (D.Data.all), Parent), Append => True);
      else
         Set_Return_Value_Key
           (D.Data.all,
            Create_Entity (Get_Script (D.Data.all), Entity), Append => True);
      end if;
      return True;
   end On_Entity_Found;

   --------------------------------
   -- References_Command_Handler --
   --------------------------------

   procedure References_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Cmd : constant References_Command_Access :=
        References_Command_Access (Get_Command (Get_Command (Data, 1)));
   begin
      if Cmd /= null and then Command = "get_result" then
         Put_Locations_In_Return
           (Cmd, Data, Show_Ref_Kind => Cmd.Show_Ref_Kind);
      end if;
   end References_Command_Handler;

   ----------------------------
   -- Entity_Command_Handler --
   ----------------------------

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel    : constant Kernel_Handle := Get_Kernel (Data);
      Entity    : constant Root_Entity'Class := Get_Data (Data, 1);
      Filter    : Custom_Filter;
      User_Data : Add_To_List_User_Data_Access;
   begin
      if Command = "find_all_refs" then
         Filter := Custom_Filter'
           (Db        => Kernel.Databases,
            Ref_Kinds => null,
            Filter    => Is_Read_Or_Implicit_Reference'Access);

         if Nth_Arg (Data, 2, False) then
            Filter.Filter := Is_Read_Or_Write_Or_Implicit_Reference'Access;
         end if;

         Find_All_References_Internal
           (Kernel, Entity,
            Category_Title   => All_Refs_Category
              (Entity             => Entity,
               Kernel             => Kernel,
               Local_Only         => False,
               Local_File         => GNATCOLL.VFS.No_File,
               All_From_Same_File => False),
            Show_Caller      => False,
            Filter           => Filter);

      elsif Command = "references" then
         declare
            Ref_Command : References_Command_Access := new References_Command;
            Implicit         : constant Boolean := Nth_Arg (Data, 2, False);
            Synchronous      : constant Boolean := Nth_Arg (Data, 3, True);
            Show_Ref_Type    : constant Boolean := Nth_Arg (Data, 4, False);
            Inst_In_File     : constant Class_Instance :=
              Nth_Arg (Data, 5, Get_File_Class (Kernel),
                       Allow_Null => True);
            Only_If_Kind     : constant String := Nth_Arg (Data, 6, "");
            In_File          : Virtual_File := No_File;
            Launched_Command : Scheduled_Command_Access;
         begin
            Ref_Command.Kernel := Kernel;
            Ref_Command.Show_Ref_Kind := Show_Ref_Type;

            if Inst_In_File /= No_Class_Instance then
               In_File := Get_Data (Inst_In_File);
            end if;

            Ref_Command.Iter.Replace_Element
              (Find_All_References
                 (Entity                => Entity,
                  In_File               => In_File,
                  Include_Implicit      => Implicit,
                  Include_All           => False,
                  Kind                  => Only_If_Kind));

            if Synchronous then
               --  Synchronous, return directly the result

               Launch_Synchronous (Ref_Command);
               Put_Locations_In_Return (Ref_Command, Data, Show_Ref_Type);
               Unref (Command_Access (Ref_Command));

            else
               --  Not synchronous, return a command

               Launched_Command := Launch_Background_Command
                 (Kernel          => Kernel,
                  Command         => Ref_Command,
                  Active          => False,
                  Show_Bar        => False);

               Set_Progress
                 (Ref_Command,
                  (Activity => Unknown,
                   Current  => Get_Current_Progress (Ref_Command.Iter.Element),
                   Total    => Get_Total_Progress (Ref_Command.Iter.Element)));

               Data.Set_Return_Value
                  (Get_Instance
                     (Launched_Command, Data.Get_Script,
                      Class_To_Create => References_Command_Class_Name));
            end if;
         end;

      elsif Command = "calls" then

         --  The following unchecked_access is safe since
         --  Examine_Entity_Call_Graph is called synchronously

         User_Data := new Add_To_List_User_Data;
         User_Data.Data := Data'Unchecked_Access;
         User_Data.Use_Parent_For_Key := False;
         Examine_Entity_Call_Graph
           (User_Data         => User_Data,
            Entity            => Entity,
            Dispatching_Calls => Nth_Arg (Data, 2, False),
            Get_All_Refs      => True);

      elsif Command = "called_by" then

         --  The following unchecked_access is safe since
         --  Examine_Ancestors_Call_Graph is called synchronously

         User_Data := new Add_To_List_User_Data;
         User_Data.Data := Data'Unchecked_Access;
         Examine_Ancestors_Call_Graph
           (Kernel          => Kernel,
            User_Data       => User_Data,
            Entity          => Entity,
            Dispatching_Calls => Nth_Arg (Data, 2, False),
            Background_Mode => False);
      end if;
   end Entity_Command_Handler;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Custom_Filter) is
   begin
      GNAT.Strings.Free (Self.Ref_Kinds);  --  also frees All_Refs
   end Free;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (Self : Custom_Filter; Ref : Root_Entity_Reference'Class) return Boolean
   is
   begin
      if Self.Filter /= null
        and then not Self.Filter (Ref)
      then
         return False;
      end if;

      if Self.Ref_Kinds /= null then
         declare
            Kind : constant String := Get_Display_Kind (Ref);
         begin
            for R in Self.Ref_Kinds'Range loop
               if Self.Ref_Kinds (R) /= null
                 and then Kind = Self.Ref_Kinds (R).all
               then
                  return True;
               end if;
            end loop;
         end;

         return False;
      else
         return True;
      end if;
   end Is_Valid;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Entity_Idle_Data) is
   begin
      Free (Data.Filter);
      Data.Iter.Reference.Destroy;
      Free (Data.Category);
   end Destroy_Idle;

   ---------------
   -- Print_Ref --
   ---------------

   procedure Print_Ref
     (Kernel       : access Kernel_Handle_Record'Class;
      Ref          : Root_Entity_Reference'Class;
      Name         : String;
      Category     : String;
      Show_Caller  : Boolean;
      Sort_In_File : Boolean)
   is
      pragma Unreferenced (Sort_In_File);

      Loc     : constant General_Location := Get_Location (Ref);
      Col     : Basic_Types.Visible_Column_Type := Loc.Column;
      Line    : constant Integer      := Loc.Line;
      File    : constant Virtual_File := Loc.File;
      Message : Markup_Message_Access;

   begin
      if Col <= 0 then
         Col := 1;
      end if;

      if Show_Caller and then Get_Caller (Ref) /= No_Root_Entity then
         Message :=
           Create_Markup_Message
             (Get_Messages_Container (Kernel),
              Category,
              File,
              Line,
              Col,
              "<b>" & Name & "</b> ["
              & Get_Display_Kind (Ref) & "] in: "
              & Glib.Convert.Escape_Text (Qualified_Name (Get_Caller (Ref))),
              0,
              Call_Graph_Message_Flags);

      else
         Message :=
           Create_Markup_Message
             (Get_Messages_Container (Kernel),
              Category,
              File,
              Line,
              Col,
              "<b>" & Name & "</b> [" & Get_Display_Kind (Ref) & "]",
              0,
              Call_Graph_Message_Flags);
      end if;

      Message.Set_Highlighting
        (Search_Results_Style,
         Highlight_Length (UTF8_Utils.UTF8_Length (Name)));
   end Print_Ref;

   -------------------------
   -- Find_Next_Reference --
   -------------------------

   procedure Find_Next_Reference
     (Data    : in out Entity_Idle_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Count : Integer := 0;
   begin
      Result := Execute_Again;

      if not Data.Iter_Started then
         Data.Iter.Replace_Element
           (Find_All_References
              (Entity             => Data.Entity.Element,
               Include_Overriding => Data.Include_Overriding,
               Include_Overridden => Data.Include_Overriding));

         Data.Iter_Started := True;
         Set_Progress
           (Command,
            (Running,
             Get_Current_Progress (Data.Iter.Element),
             Get_Total_Progress (Data.Iter.Element)));
         return;
      end if;

      while Count < Locations_At_A_Time loop
         if At_End (Data.Iter.Element) then
            Result := Success;
            exit;

         else
            declare
               Ref : constant Root_Entity_Reference'Class :=
                 Get (Data.Iter.Element);
            begin
               --  Not done parsing all the files yet
               if Ref = No_Root_Entity_Reference then
                  Next (Data.Iter.Reference);
                  exit;

               elsif Is_Valid (Data.Filter, Ref) then
                  Data.Count := Data.Count + 1;
                  Print_Ref
                    (Data.Kernel,
                     Ref,
                     (Get_Entity_Name (Ref)),
                     Data.Category.all,
                     Show_Caller  => Data.Show_Caller,
                     Sort_In_File => False);

                  if Data.Count = 1 then
                     GPS.Location_View.Raise_Locations_Window
                       (Data.Kernel, Give_Focus => False);
                  end if;
               end if;

               Count := Count + 1;
            end;
         end if;

         Next (Data.Iter.Reference);
      end loop;

      Set_Progress
        (Command,
         (Running,
          Get_Current_Progress (Data.Iter.Element),
          Get_Total_Progress (Data.Iter.Element)));
   end Find_Next_Reference;

   -----------------------
   -- All_Refs_Category --
   -----------------------

   function All_Refs_Category
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Root_Entity'Class;
      Local_Only         : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File;
      All_From_Same_File : Boolean) return String
   is
      pragma Unreferenced (Kernel);
      Decl : constant General_Location := Get_Declaration (Entity).Loc;
   begin
      if All_From_Same_File then
         return -"Entities imported from "
           & (+Decl.File.Base_Name)
           & (-" into ")
           & (+Local_File.Base_Name);

      elsif Local_Only then
         return -"Local references for "
           & Get_Name (Entity)
           & " ("  & (+Decl.File.Base_Name)
           & ":" & Image (Decl.Line) & ") " & (-"in ")
           & (+Local_File.Base_Name);

      else
         return -"References for "
           & Get_Name (Entity)
           & " ("  & (+Decl.File.Base_Name)
           & ":" & Image (Decl.Line) & ")";
      end if;
   end All_Refs_Category;

   ----------------------------------
   -- Find_All_References_Internal --
   ----------------------------------

   procedure Find_All_References_Internal
     (Kernel             : access Kernel_Handle_Record'Class;
      Info               : Root_Entity'Class;
      Category_Title     : String;
      Show_Caller        : Boolean;
      Filter             : in out Custom_Filter;
      Include_Overriding : Boolean := False)
   is
      Data : Entity_Idle_Data;
      C    : Xref_Commands.Generic_Asynchronous_Command_Access;
      H    : Root_Entity_Ref;
   begin
      if Info /= No_Root_Entity then
         begin
            Get_Messages_Container (Kernel).Remove_Category
              (Category_Title, Call_Graph_Message_Flags);
            H.Replace_Element (Info);
            Data := (Kernel             => Kernel_Handle (Kernel),
                     Iter               => <>,
                     Filter             => Filter,
                     Category           => new String'(Category_Title),
                     Iter_Started       => False,
                     Show_Caller        => Show_Caller,
                     Include_Overriding => Include_Overriding,
                     Count              => 0,
                     Entity             => H);

            Xref_Commands.Create  --  Will destroy Data when done
              (C, -"Find all refs", Data, Find_Next_Reference'Access);
            Launch_Background_Command
              (Kernel, Command_Access (C), True, True, "xrefs");

         exception
            when E : others =>
               Trace (Me, E);
               Destroy (Data.Iter.Reference);
         end;
      else
         Free (Filter);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Find_All_References_Internal;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Find_All_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Filter  : Custom_Filter;
   begin
      if Context.Context /= No_Context then
         declare
            Entity : constant Root_Entity'Class :=
              Get_Entity (Context.Context);
            File   : Virtual_File;
         begin
            if Entity /= No_Root_Entity then
               Filter                  := Custom_Filter'
                 (Db                 => Kernel.Databases,
                  Ref_Kinds          => null,
                  Filter             => null);

               if Command.Reads_Only then
                  Filter.Filter := Is_Read_Reference'Access;
               elsif Command.Writes_Only then
                  Filter.Filter := Is_Write_Reference'Access;
               else
                  Filter.Filter := Is_Read_Or_Write_Reference'Access;
               end if;

               File := File_Information (Context.Context);

               Parse_All_Refs
                 (Kernel             => Kernel,
                  Entity             => Entity,
                  Locals_Only        => Command.Locals_Only,
                  Local_File         => File,
                  All_From_Same_File => False,
                  Filter             => Filter,

                  --  Only show caller for Ada xrefs: it is too slow to do
                  --  this in C/C++
                  Show_Caller        => Get_Language_Handler (Kernel).
                      Get_Language_From_File (File) = Ada_Lang,

                  Include_Overriding => True);
            end if;
            return Commands.Success;
         end;
      else
         Kernel.Insert
           (-"Cannot find references: no entity selected",
            Mode => Error);
         return Commands.Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Find_Specific_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel             : constant Kernel_Handle :=
        Get_Kernel (Context.Context);
      Dialog             : References_Filter_Dialog;
      Box                : Gtk_Box;
      Col                : array (1 .. 2) of Gtk_Box;
      Filter_Box         : Gtk_Vbutton_Box;
      Index              : Integer := Col'First;
      Project_And_Recursive,
      File_Only          : Gtk_Radio_Button;
      Show_Caller        : Gtk_Check_Button;
      From_Same_File     : Gtk_Radio_Button;
      Include_Overriding : Gtk_Check_Button;
      Frame              : Gtk_Frame;
      Ignore             : Gtk_Widget;
      Entity             : constant Root_Entity'Class :=
        Get_Entity (Context.Context);
      Current_File       : constant Virtual_File :=
        File_Information (Context.Context);
      Button             : Gtk_Button;
      pragma Unreferenced (Command, Ignore);

      All_Refs : GNAT.Strings.String_List :=
        Kernel.Databases.All_Real_Reference_Kinds;

   begin
      Dialog := new References_Filter_Dialog_Record;
      Dialog.Filters := new Filters_Buttons (All_Refs'Range);

      Initialize (Dialog,
                  Title  => -"Find References Options",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Modal
                  or Use_Header_Bar_From_Settings (Get_Main_Window (Kernel)));

      --  Context choice

      Gtk_New (Frame, -"Context");
      Pack_Start (Get_Content_Area (Dialog), Frame);
      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Gtk_New (Project_And_Recursive, Widget_SList.Null_List,
               -"In all projects");
      Pack_Start (Box, Project_And_Recursive);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_Project_Recursive", True);
      Associate (Get_History (Kernel).all, "Find_Prefs_Project_Recursive",
                 Project_And_Recursive);

      Gtk_New (File_Only, Get_Group (Project_And_Recursive),
               -"In current file");
      Pack_Start (Box, File_Only);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_File_Only", False);
      Associate (Get_History (Kernel).all, "Find_Prefs_File_Only", File_Only);

      Gtk_New
        (From_Same_File, Get_Group (Project_And_Recursive),
         -"All entities imported from same file");
      Pack_Start (Box, From_Same_File);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_From_Same_File", False);
      Associate (Get_History (Kernel).all, "Find_Prefs_From_Same_File",
                 From_Same_File);

      --  Filter choice

      Gtk_New (Frame, -"Filter");
      Pack_Start (Get_Content_Area (Dialog), Frame);
      Gtk_New_Hbox (Box, Homogeneous => False);
      Add (Frame, Box);

      for C in Col'Range loop
         Gtk_New_Vbox (Col (C), Homogeneous => True);
         Pack_Start (Box, Col (C), Expand => True);
      end loop;

      for F in Dialog.Filters'Range loop
         Gtk_New (Dialog.Filters (F), All_Refs (F).all);
         Pack_Start (Col (Index), Dialog.Filters (F));
         Create_New_Boolean_Key_If_Necessary
           (Get_History (Kernel).all,
            History_Key ("Find_Prefs_Filter_" & F'Img), True);
         Associate (Get_History (Kernel).all,
                    History_Key ("Find_Prefs_Filter_" & F'Img),
                    Dialog.Filters (F));
         Index := Index + 1;
         if Index > Col'Last then
            Index := Col'First;
         end if;
      end loop;

      Gtk_New (Filter_Box);
      Set_Layout (Filter_Box, Buttonbox_Spread);
      Pack_Start (Box, Filter_Box, Padding => 5);

      Gtk_New (Button, -"Select all");
      Pack_Start (Filter_Box, Button);
      Widget_Callback.Object_Connect
        (Button, Signal_Clicked, Select_All_Filters'Access, Dialog);

      Gtk_New (Button, -"Unselect all");
      Pack_Start (Filter_Box, Button);
      Widget_Callback.Object_Connect
        (Button, Signal_Clicked, Unselect_All_Filters'Access, Dialog);

      --  Extra info choice

      Gtk_New (Frame, -"Advanced Search");
      Pack_Start (Get_Content_Area (Dialog), Frame);
      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Gtk_New (Show_Caller, -"Show context");
      Pack_Start (Box, Show_Caller);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_Show_Caller", False);
      Associate (Get_History (Kernel).all, "Find_Prefs_Show_Caller",
                 Show_Caller);

      Gtk_New
        (Include_Overriding, -"Include overriding and overridden operations");
      Pack_Start (Box, Include_Overriding);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_Include_Overriding", False);
      Associate (Get_History (Kernel).all, "Find_Prefs_Include_Overriding",
                 Include_Overriding);

      Ignore := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Ignore := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         for F in Dialog.Filters'Range loop
            if not Get_Active (Dialog.Filters (F)) then
               Free (All_Refs (F));
            end if;
         end loop;

         declare
            Filter : Custom_Filter :=
              (Db        => Kernel.Databases,
               Ref_Kinds => new GNAT.Strings.String_List'(All_Refs),
               Filter    => null);
         begin
            Parse_All_Refs  --  will destroy filter
              (Kernel             => Kernel,
               Entity             => Entity,
               Locals_Only        => Get_Active (File_Only),
               Local_File         => Current_File,
               All_From_Same_File => Get_Active (From_Same_File),
               Filter             => Filter,
               Show_Caller        => Get_Active (Show_Caller),
               Include_Overriding => Get_Active (Include_Overriding));
         end;

         Unchecked_Free (Dialog.Filters);
         Destroy (Dialog);

         return Commands.Success;
      else
         Unchecked_Free (Dialog.Filters);
         GNATCOLL.Utils.Free (All_Refs);
         Destroy (Dialog);
         return Commands.Failure;
      end if;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Entity_Filter;
      Context : Selection_Context) return Boolean is
      pragma Unreferenced (Filter);
   begin
      if Has_Entity_Name_Information (Context) then
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            return Entity /= No_Root_Entity
              and then Is_Subprogram (Entity);
         end;
      else
         return False;
      end if;
   end Filter_Matches_Primitive;

   ------------------------
   -- Select_All_Filters --
   ------------------------

   procedure Select_All_Filters (Dialog : access Gtk_Widget_Record'Class) is
      D : constant References_Filter_Dialog :=
        References_Filter_Dialog (Dialog);
   begin
      for F in D.Filters'Range loop
         Set_Active (D.Filters (F), True);
      end loop;
   end Select_All_Filters;

   --------------------------
   -- Unselect_All_Filters --
   --------------------------

   procedure Unselect_All_Filters (Dialog : access Gtk_Widget_Record'Class) is
      D : constant References_Filter_Dialog :=
        References_Filter_Dialog (Dialog);
   begin
      for F in D.Filters'Range loop
         Set_Active (D.Filters (F), False);
      end loop;
   end Unselect_All_Filters;

   --------------------
   -- Parse_All_Refs --
   --------------------

   procedure Parse_All_Refs
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Root_Entity'Class;
      Locals_Only        : Boolean;
      Local_File         : Virtual_File;
      All_From_Same_File : Boolean;
      Show_Caller        : Boolean;
      Filter             : in out Custom_Filter;
      Include_Overriding : Boolean := False)
   is
      Title       : constant String := All_Refs_Category
        (Entity             => Entity,
         Kernel             => Kernel,
         Local_Only         => Locals_Only,
         Local_File         => Local_File,
         All_From_Same_File => All_From_Same_File);
      Decl : constant General_Location := Get_Declaration (Entity).Loc;
      Decl2 : General_Location;
      Entity_Decl : constant Virtual_File := Decl.File;
      Iter2       : Entities_In_File_Cursor;
      Message     : Simple_Message_Access;
      Project     : Project_Type := No_Project;
      Set         : File_Info_Set;
      Imports     : Boolean;
      Is_Limited_With : Boolean;

   begin
      if All_From_Same_File then
         Get_Messages_Container (Kernel).Remove_Category
           (Title, Call_Graph_Message_Flags);

         --  We use a project that is in the same tree as the entity.

         Set := Get_Registry (Kernel).Tree.Info_Set (Local_File);
         for S of Set loop
            declare
               F_Info : constant File_Info'Class := File_Info'Class (S);
            begin
               F_Info.Project.Project_Imports
                 (Get_Project (Decl),
                  Include_Extended => True,
                  Imports          => Imports,
                  Is_Limited_With  => Is_Limited_With);
               if Imports then
                  Project := F_Info.Project;
                  exit;
               end if;
            end;
         end loop;

         Iter2 := Kernel.Databases.Entities_In_File (Local_File, Project);
         while not At_End (Iter2) loop
            declare
               Entity2        : constant Root_Entity'Class := Get (Iter2);
            begin
               Decl2 := Get_Declaration (Entity2).Loc;

               if Decl2.File = Entity_Decl then
                  if Show_Caller then
                     declare
                        Iter : Root_Reference_Iterator'Class :=
                          Find_All_References
                            (Entity             => Entity2,
                             In_File            => Local_File,
                             Include_Overriding => Include_Overriding,
                             Include_Overridden => Include_Overriding);
                     begin

                        declare
                           Name2 : constant String := Get_Name (Entity2);
                           Loc   : General_Location;
                        begin
                           while not At_End (Iter) loop
                              Loc := Get_Location (Get (Iter));

                              if Get (Iter) /= No_Root_Entity_Reference
                                and then Loc.File = Local_File
                                and then Is_Valid (Filter, Ref => Get (Iter))
                              then
                                 Print_Ref (Kernel,
                                            Get (Iter),
                                            Name2,
                                            Title,
                                            Show_Caller => Show_Caller,
                                            Sort_In_File => True);
                              end if;
                              Next (Iter);
                           end loop;
                        end;

                        Destroy (Iter);
                     end;
                  else
                     declare
                        Name2 : constant String := Get_Name (Entity2);
                     begin
                        Message :=
                          Create_Simple_Message
                            (Get_Messages_Container (Kernel),
                             Title,
                             Decl2.File,
                             Decl2.Line,
                             Decl2.Column,
                             Name2,
                             0,
                             Call_Graph_Message_Flags);
                        Message.Set_Highlighting
                          (Search_Results_Style, Name2'Length);
                     end;
                  end if;
               end if;

               Next (Iter2);
            end;
         end loop;

         Free (Filter);

      elsif Locals_Only then
         --  Print the declaration of the entity, but only if it is in the
         --  current file, as expected by users.

         Get_Messages_Container (Kernel).Remove_Category
           (Title, Call_Graph_Message_Flags);

         declare
            Iter : Root_Reference_Iterator'Class :=
              Find_All_References
                (Entity        => Entity,
                 In_File       => Local_File,
                 Include_Overridden => Include_Overriding,
                 Include_Overriding => Include_Overriding);
         begin
            declare
               Name : constant String := Get_Name (Entity);
            begin
               while not At_End (Iter) loop
                  if Get (Iter) /= No_Root_Entity_Reference
                    and then Is_Valid (Filter, Get (Iter))
                  then
                     Print_Ref (Kernel,
                                Get (Iter),
                                Name,
                                Title,
                                Show_Caller => Show_Caller,
                                Sort_In_File => True);
                  end if;
                  Next (Iter);
               end loop;
            end;

            Destroy (Iter);
            Free (Filter);
         end;
      else
         Find_All_References_Internal   --  will destroy filter
           (Kernel,
            Entity,
            Category_Title     => Title,
            Show_Caller        => Show_Caller,
            Filter             => Filter,
            Include_Overriding => Include_Overriding);
      end if;
   end Parse_All_Refs;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      C : constant Class_Type := Get_Entity_Class (Kernel);

      Command_Class : constant Class_Type :=
        Kernel.Scripts.New_Class ("Command");
      References_Command_Class : constant Class_Type :=
        Kernel.Scripts.New_Class
          (References_Command_Class_Name, Command_Class);

      Command  : Interactive_Command_Access;
      Filter   : Action_Filter;
   begin
      Filter := new Subprogram_Entity_Filter;
      Subprogram_Entity_Filter (Filter.all).Kernel := Kernel_Handle (Kernel);
      Register_Filter (Kernel, Filter, "Entity is subprogram");

      Register_Contextual_Submenu
        (Kernel, "References",
         Filter     => Lookup_Filter (Kernel, "Entity"),
         Ref_Item   => "goto other file",
         Add_Before => False);

      Register_Action
        (Kernel, "find all references",
         Command     => new Find_All_Refs_Command,
         Description =>
           -("List all references to the selected entity"
             & " in the Locations window"),
         Filter => Lookup_Filter (Kernel, "Entity"));
      Register_Contextual_Menu
        (Kernel,
         Label      => "References/Find all references to %e",
         Action     => "find all references",
         Ref_Item   => "Browser: entity called by",
         Add_Before => False);

      Register_Action
        (Kernel, "find references...",
         Command     => new Find_Specific_Refs_Command,
         Description =>
           -("List all references to the selected entity"
           & " in the Locations window, with extra filters"),
         Filter => Lookup_Filter (Kernel, "Entity"));
      Register_Contextual_Menu
        (Kernel,
         Label      => "References/Find references to %e...",
         Action     => "find references...");

      Command := new Find_All_Refs_Command;
      Find_All_Refs_Command (Command.all).Locals_Only := True;
      Register_Action
        (Kernel, "find all local references",
         Command     => Command,
         Description =>
           -("List all references in the selected file to the selected entity"
           & " in the Locations window"),
         Filter => Lookup_Filter (Kernel, "Entity"));
      Register_Contextual_Menu
        (Kernel,
         Label  => "References/Find all local references to %e",
         Action => "find all local references");

      Kernel.Scripts.Register_Command
        ("find_all_refs",
         Class        => C,
         Params       => (1 => Param ("include_implicit", Optional => True)),
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("references",
         Class        => C,
         Params       => (Param ("include_implicit", Optional => True),
                          Param ("synchronous",      Optional => True),
                          Param ("show_kind",        Optional => True),
                          Param ("in_file",          Optional => True),
                          Param ("kind_in",          Optional => True)),
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("calls",
         Class        => C,
         Params       => (1 => Param ("dispatching_calls", Optional => True)),
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("called_by",
         Class        => C,
         Params       => (1 => Param ("dispatching_calls", Optional => True)),
         Handler      => Entity_Command_Handler'Access);

      Kernel.Scripts.Register_Command
        ("get_result",
         Class   => References_Command_Class,
         Handler => References_Command_Handler'Access);
   end Register_Module;

end GPS.Kernel.Entities;
