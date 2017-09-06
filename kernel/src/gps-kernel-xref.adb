------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Commands.Generic_Asynchronous;  use Commands;
with Commands;                       use Commands;
with GNATCOLL.Scripts;               use GNATCOLL.Scripts;
with GNATCOLL.SQL.Exec;              use GNATCOLL.SQL.Exec;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.Utils;
with GPS.Intl;                       use GPS.Intl;
with GPS.Kernel.Hooks;               use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                 use GPS.Kernel.MDI;
with GPS.Kernel.Project;             use GPS.Kernel.Project;
with GPS.Kernel.Preferences;         use GPS.Kernel.Preferences;
with GPS.Kernel.Task_Manager;        use GPS.Kernel.Task_Manager;
with GPS.Kernel.Scripts;             use GPS.Kernel.Scripts;
with GPS.Main_Window;                use GPS.Main_Window;
with GPS.Kernel;                     use GPS.Kernel;
with GPS.Dialogs;                    use GPS.Dialogs;

with Glib.Convert;                   use Glib.Convert;
with Glib.Object;                    use Glib.Object;
with Glib_Values_Utils;              use Glib_Values_Utils;

with GUI_Utils;                      use GUI_Utils;
with Gtk.Box;                        use Gtk.Box;
with Gtk.Dialog;                     use Gtk.Dialog;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Scrolled_Window;            use Gtk.Scrolled_Window;
with Gtk.Stock;                      use Gtk.Stock;
with Gtk.Label;                      use Gtk.Label;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Tree_Selection;             use Gtk.Tree_Selection;
with Gtk.Tree_Store;                 use Gtk.Tree_Store;
with Gtk.Tree_View;                  use Gtk.Tree_View;
with Gtk.Widget;                     use Gtk.Widget;
with Gtkada.Handlers;                use Gtkada.Handlers;
with System;                         use System;

package body GPS.Kernel.Xref is

   use Root_Entity_Refs;

   Me : constant Trace_Handle := Create ("Xref");

   type Examine_Callback is record
      Iter              : Root_Reference_Iterator_Ref;
      Kernel            : Kernel_Handle;
      Entity            : Root_Entity_Ref;
      Data              : Commands_User_Data;
      Watch             : Gtk_Widget;
      Dispatching_Calls : Boolean;
      Cancelled         : Boolean;
   end record;
   type Examine_Callback_Access is access Examine_Callback;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Commands_User_Data_Record'Class, Commands_User_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Examine_Callback, Examine_Callback_Access);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Examine_Callback_Access);

   procedure Destroy_Idle (Data : in out Examine_Callback_Access);
   --  Called when the idle loop is destroyed.

   package Ancestor_Commands is new Generic_Asynchronous
     (Examine_Callback_Access, Destroy_Idle);

   procedure Examine_Ancestors_Idle
     (Data    : in out Examine_Callback_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Called for every occurrence of Data.Entity

   procedure Watch_Destroyed_While_Computing
     (Data : System.Address; Object : System.Address);
   pragma Convention (C, Watch_Destroyed_While_Computing);

   procedure Row_Activated (Widget : access Gtk_Widget_Record'Class);
   --  Called when a specific entity declaration has been selected in the
   --  overloaded entities dialog.

   type SQL_Error_Reporter is new GNATCOLL.SQL.Exec.Error_Reporter with record
      Kernel : access Kernel_Handle_Record'Class;

      Warned_About_Corruption : Boolean := False;
      --  Used to avoid duplicate messages about corrupted database
   end record;
   overriding procedure On_Database_Corrupted
     (Self       : in out SQL_Error_Reporter;
      Connection : access Database_Connection_Record'Class);

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the default commands

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Hooks

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Standard.Xref.Project_Changed (Kernel.Databases);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Standard.Xref.Project_View_Changed
        (Kernel.Databases, Get_Project_Tree (Kernel));

      if not Kernel.Databases.Allow_Queries then
         Insert
           (Kernel,
            -"The file '"
            & Kernel.Databases.Xref_Database_Location.Display_Full_Name
            & "' cannot be written. Cross-references are disabled.");
      end if;

   end Execute;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (Self  : GPS_Xref_Database;
      Error : String)
   is
   begin
      Self.Kernel.Insert
        (Text   => Error,
         Add_LF => True,
         Mode   => GPS.Kernel.Error);
   end On_Error;

   -------------------------------------
   -- Watch_Destroyed_While_Computing --
   -------------------------------------

   procedure Watch_Destroyed_While_Computing
     (Data : System.Address; Object : System.Address)
   is
      pragma Unreferenced (Object);
   begin
      Convert (Data).Cancelled := True;
   end Watch_Destroyed_While_Computing;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Examine_Callback_Access) is
   begin
      if not Data.Cancelled
        and then Data.Watch /= null
      then
         Weak_Unref (Data.Watch, Watch_Destroyed_While_Computing'Access,
                     Data.all'Address);
      end if;

      Destroy (Data.Data.all, Data.Cancelled);
      Unchecked_Free (Data.Data);
      Destroy (Data.Iter.Reference);
      Unchecked_Free (Data);
   end Destroy_Idle;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Data : in out Commands_User_Data_Record; Cancelled : Boolean)
   is
      pragma Unreferenced (Data, Cancelled);
   begin
      null;
   end Destroy;

   ----------------------------
   -- Examine_Ancestors_Idle --
   ----------------------------

   procedure Examine_Ancestors_Idle
     (Data    : in out Examine_Callback_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Iter_Ref : constant Root_Reference_Iterator_Refs.Reference_Type :=
        Data.Iter.Reference;
   begin
      if Data.Cancelled then
         Result := Failure;

      elsif At_End (Iter_Ref) then
         Result := Success;

      else
         declare
            Ref : constant Root_Entity_Reference'Class := Get (Iter_Ref);
         begin

            Result := Execute_Again;

            if Ref /= No_Root_Entity_Reference
              and then not Ref.Reference_Is_Declaration
            then
               declare
                  Parent : Root_Entity'Class := Get_Caller (Ref);
               begin
                  if Parent /= No_Root_Entity
                    and then Ref.Show_In_Callgraph
                  then
                     loop
                        declare
                           New_Parent : constant Root_Entity'Class
                             := Caller_At_Declaration (Parent);
                        begin
                           exit when New_Parent = No_Root_Entity
                             or else Is_Container (Parent);
                           Parent := New_Parent;
                        end;
                     end loop;

                     if Parent /= No_Root_Entity then
                        --  If we are seeing a dispatching call to an
                        --  overridden subprogram, this could also result in
                        --  a call to the entity and we report it

                        if Get_Entity (Iter_Ref) /= Data.Entity.Element then
                           if Ref.Is_Dispatching_Call then
                              if not On_Entity_Found
                                (Data.Data, Get_Entity (Iter_Ref),
                                 Parent, Ref,
                                 Through_Dispatching => True,
                                 Is_Renaming         => False)
                              then
                                 Result := Failure;
                              end if;
                           end if;

                        else
                           if not On_Entity_Found
                             (Data.Data, Data.Entity.Element, Parent, Ref,
                              Through_Dispatching    => False,
                              Is_Renaming            => False)
                           then
                              Result := Failure;
                           end if;
                        end if;
                     end if;
                  end if;
               end;
            end if;

         end;

         Next (Iter_Ref);

         if Command /= null then
            Set_Progress
              (Command,
               (Running,
                Get_Current_Progress (Iter_Ref),
                Get_Total_Progress (Iter_Ref)));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Result := Failure;
   end Examine_Ancestors_Idle;

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Kernel            : access Kernel_Handle_Record'Class;
      Entity            : Root_Entity'Class;
      User_Data         : access Commands_User_Data_Record'Class;
      Background_Mode   : Boolean := True;
      Dispatching_Calls : Boolean := False;
      Watch             : Gtk.Widget.Gtk_Widget := null)
   is
      Cb     : Examine_Callback_Access;
      C      : Ancestor_Commands.Generic_Asynchronous_Command_Access;
      Result : Command_Return_Type;
   begin
      Cb := new Examine_Callback'
        (Kernel            => Kernel_Handle (Kernel),
         Data              => Commands_User_Data (User_Data),
         Entity            => To_Holder (Entity),
         Watch             => Watch,
         Cancelled         => False,
         Dispatching_Calls => Dispatching_Calls,
         Iter              =>
           Root_Reference_Iterator_Refs.To_Holder
             (Find_All_References
                  (Entity             => Entity,
                   Include_Overridden => Dispatching_Calls)));

      --  If we have a renaming, report it

      declare
         Rename : constant Root_Entity'Class := Entity.Renaming_Of;
      begin
         if Rename /= No_Root_Entity then
            if not On_Entity_Found
              (User_Data, Entity, Rename, No_Root_Entity_Reference,
               Through_Dispatching => False,
               Is_Renaming         => True)
            then
               Destroy_Idle (Cb);
               return;
            end if;
         end if;
      end;

      if Watch /= null then
         Weak_Ref
           (Watch,
            Watch_Destroyed_While_Computing'Access,
            Cb.all'Address);
      end if;

      if Background_Mode then
         Ancestor_Commands.Create
           (C, -"Called by", Cb, Examine_Ancestors_Idle'Access);
         Launch_Background_Command
           (Kernel, Command_Access (C), True, True, "call graph");
      else
         loop
            Examine_Ancestors_Idle (Cb, Command_Access (C), Result);
            exit when Result /= Execute_Again;
         end loop;
         Destroy_Idle (Cb);
      end if;
   end Examine_Ancestors_Call_Graph;

   ------------------------------
   -- Reference_Is_Declaration --
   ------------------------------

   function Reference_Is_Declaration
     (Ref : Root_Entity_Reference'Class) return Boolean
   is (Ref.Reference_Is_Declaration);

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Entity            : Root_Entity'Class;
      User_Data         : access Commands_User_Data_Record'Class;
      Get_All_Refs      : Boolean;
      Dispatching_Calls : Boolean)
   is
      Calls       : Calls_Iterator;
      Called_E_Decl : General_Location;
      Data        : Commands_User_Data;
      Is_First    : Boolean;
      Through_Dispatching : Boolean;
   begin
      if Entity = No_Root_Entity then
         Destroy (User_Data.all, Cancelled => False);
         Data := Commands_User_Data (User_Data);
         Unchecked_Free (Data);
         return;
      end if;
      declare
         Calls : Abstract_Entities_Cursor'Class :=
           Get_All_Called_Entities (Entity);
      begin
         For_Each_Entity :
         while not At_End (Calls) loop
            declare
               Called_E : constant Root_Entity'Class := Get (Calls);
               Refs        : Root_Reference_Iterator'Class :=
                 Find_All_References
                   (Entity   => Called_E,
                    In_Scope => Entity);
            begin
               if Called_E /= No_Root_Entity
                 and then Called_E.Is_Subprogram
               then
                  Called_E_Decl := Called_E.Get_Declaration.Loc;

                  if Get_All_Refs or Dispatching_Calls then
                     --  Now search for all references. This was either
                     --  requested explicitly or is needed to resolve
                     --  dispatching calls

                     Is_First := True;

                     while not At_End (Refs) loop
                        declare
                           Ref : constant Root_Entity_Reference'Class :=
                             Get (Refs);
                        begin

                           if Ref /= No_Root_Entity_Reference
                             and then Ref.Show_In_Callgraph
                             and then Get_Caller (Ref) = Entity
                             and then Get_Entity (Refs).Is_Subprogram
                             and then Called_E_Decl /= Get_Location (Ref)
                           then
                              --  If we want to see all references, report this
                              --  one now, unless it is a dispatching call
                              --  which is already reported later on

                              if Get_All_Refs then
                                 Through_Dispatching :=
                                   Ref.Is_Dispatching_Call;

                                 if not Dispatching_Calls
                                   or else not Through_Dispatching
                                 then
                                    if not On_Entity_Found
                                      (User_Data,
                                       Entity         => Get_Entity (Refs),
                                       Parent              => Entity,
                                       Ref                 => Ref,
                                       Through_Dispatching =>
                                         Through_Dispatching,
                                       Is_Renaming         => False)
                                    then
                                       exit For_Each_Entity;
                                    end if;
                                 end if;

                                 --  Else we only want to report the callee
                                 --  once, ie on its first reference. We still
                                 --  have to examine all references through to
                                 --  solve dispatching calls.

                              elsif Is_First
                                and then not Ref.Is_Dispatching_Call
                              then
                                 Is_First := False;
                                 if not On_Entity_Found
                                   (User_Data,
                                    Entity              => Get_Entity (Refs),
                                    Parent              => Entity,
                                    Ref                 =>
                                      No_Root_Entity_Reference,
                                    Through_Dispatching => False,
                                    Is_Renaming         => False)
                                 then
                                    exit For_Each_Entity;
                                 end if;
                              end if;

                              --  Now if the reference is in fact a dispatching
                              --  call, report all called entities.

                              if Dispatching_Calls then
                                 declare
                                    Stop      : Boolean := False;
                                    function On_Callee
                                      (Callee : Root_Entity'Class)
                                       return Boolean;

                                    function On_Callee
                                      (Callee : Root_Entity'Class)
                                       return Boolean
                                    is
                                    begin
                                       if not On_Entity_Found
                                         (User_Data,
                                          Entity              => Callee,
                                          Parent              => Entity,
                                          Ref                 => Ref,
                                          Through_Dispatching => True,
                                          Is_Renaming         => False)
                                       then
                                          Stop := True;
                                          return False;
                                       end if;
                                       return True;
                                    end On_Callee;

                                 begin
                                    --  Always compute accurate information
                                    --  for the call graph, since, as opposed
                                    --  to the contextual menu, we have more
                                    --  time to do the computation
                                    Increase_Indent
                                      (Me,
                                       "Searching for all dispatch calls at "
                                       & Get_Location (Ref).Line'Img);

                                    For_Each_Dispatching_Call
                                      (Ref       => Ref,
                                       Filter    =>
                                         Reference_Is_Declaration'Access,
                                       On_Callee => On_Callee'Access);
                                    Decrease_Indent (Me);
                                    exit For_Each_Entity when Stop;
                                 end;
                              end if;
                           end if;

                           Next (Refs);
                        end;
                     end loop;
                     Destroy (Refs);
                  else
                     if not On_Entity_Found
                       (User_Data,
                        Entity              => Called_E,
                        Parent              => Entity,
                        Ref                 => No_Root_Entity_Reference,
                        Through_Dispatching => False,
                        Is_Renaming         => False)
                     then
                        exit For_Each_Entity;
                     end if;
                  end if;
               end if;

               Next (Calls);
            end;
         end loop For_Each_Entity;
      end;

      Destroy (Calls);

      Destroy (User_Data.all, Cancelled => False);
      Data := Commands_User_Data (User_Data);
      Unchecked_Free (Data);
   end Examine_Entity_Call_Graph;

   ---------------------------------
   -- Get_Entity_Information_Type --
   ---------------------------------

   function Get_Entity_Information_Type return Glib.GType is
   begin
      return Glib.GType_Int;
   end Get_Entity_Information_Type;

   ---------------------------
   -- On_Database_Corrupted --
   ---------------------------

   overriding procedure On_Database_Corrupted
     (Self       : in out SQL_Error_Reporter;
      Connection : access Database_Connection_Record'Class)
   is
      pragma Unreferenced (Connection);
   begin
      if not Self.Warned_About_Corruption then
         Self.Warned_About_Corruption := True;
         Insert
           (Self.Kernel,
            "Cross-reference database appears to be corrupted." & ASCII.LF
            & "Please exit GPS, delete the file '"
            & Xref_Database_Location (Self.Kernel.Databases).Display_Full_Name
            & "' and restart GPS",
            Mode => Error);
      end if;
   end On_Database_Corrupted;

   ---------------------
   -- Create_Database --
   ---------------------

   procedure Create_Database
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Result : out Standard.Xref.General_Xref_Database)
   is
      Errors : access SQL_Error_Reporter'Class;
   begin
      if Kernel.Databases = null then
         Result := new GPS_General_Xref_Database_Record;
         GPS_General_Xref_Database_Record (Result.all).Kernel :=
           Kernel_Handle (Kernel);
      else
         Result := Kernel.Databases;
      end if;

      if Result.Xref = null then
         Result.Xref := new GPS.Kernel.Xref.GPS_Xref_Database;
         GPS_Xref_Database (Result.Xref.all).Kernel :=
           Kernel_Handle (Kernel);
      end if;

      Errors := new SQL_Error_Reporter;   --  never freed
      Errors.Kernel := Kernel;

      Result.Initialize
        (Lang_Handler => Kernel.Lang_Handler,
         Symbols      => Kernel.Symbols,
         Registry     => Kernel.Registry,
         Errors       => Errors);
   end Create_Database;

   -------------------------------
   -- Select_Entity_Declaration --
   -------------------------------

   overriding function Select_Entity_Declaration
     (Self    : access GPS_General_Xref_Database_Record;
      File    : Virtual_File;
      Project : Project_Type;
      Entity  : Root_Entity'Class) return Root_Entity'Class
   is
      Column_Types : constant GType_Array :=
        (0 => GType_String,
         1 => GType_Int,
         2 => GType_Int,
         3 => GType_String,
         4 => GType_Int);  --  Contains the number of the iter
      Column_Names : GNAT.Strings.String_List :=
        (1 => new String'("File"),
         2 => new String'("Line"),
         3 => new String'("Column"),
         4 => new String'("Name"));

      Name : constant String := Entity.Get_Name;

      Iter      : Entities_In_File_Cursor;
      Button    : Gtk_Widget;
      OK_Button : Gtk_Widget;
      Count     : Natural := 0;
      Label     : Gtk_Label;
      Model     : Gtk_Tree_Store;
      M         : Gtk_Tree_Model;
      Dialog    : GPS_Dialog;
      It        : Gtk_Tree_Iter;
      Scrolled  : Gtk_Scrolled_Window;
      View      : Gtk_Tree_View;
      Col_Num   : Gint;
      Candidate_Decl : General_Entity_Declaration;
      pragma Unreferenced (Button, Col_Num);

      Number_Selected : Natural := 0;

   begin
      Iter := Self.Entities_In_File
        (File    => File,
         Project => Project,
         Name    => Name);

      while not At_End (Iter) loop
         Count := Count + 1;
         declare
            Candidate : constant Root_Entity'Class := Get (Iter);
         begin
            Candidate_Decl := Candidate.Get_Declaration;

            if Count = 1 then
               Gtk_New (Dialog,
                        Title  => -"Select the declaration",
                        Kernel => Self.Kernel,
                        Flags  => Modal);
               Set_Default_Size_From_History
                  (Dialog, "xref", Self.Kernel, 500, 500);

               Gtk_New (Label, -"This entity is overloaded.");
               Pack_Start (Dialog.Get_Action_Area, Label, Expand => False);

               Gtk_New (Label, -"Please select the appropriate declaration.");
               Pack_Start (Dialog.Get_Action_Area, Label, Expand => False);

               Gtk_New (Scrolled);
               Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
               Pack_Start (Dialog.Get_Action_Area, Scrolled);

               OK_Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
               Button := Add_Button
                 (Dialog, Stock_Cancel, Gtk_Response_Cancel);

               View := Create_Tree_View
                 (Column_Types       => Column_Types,
                  Column_Names       => Column_Names,
                  Initial_Sort_On    => 1);
               Add (Scrolled, View);
               Model := -Get_Model (View);

               Widget_Callback.Object_Connect
                 (View, Signal_Row_Activated, Row_Activated'Access, Dialog);
            end if;

            Append (Model, It, Null_Iter);

            Set_And_Clear
              (Model, It,
               (0 => As_String (+Candidate_Decl.Loc.File.Base_Name),
                1 => As_Int    (Gint (Candidate_Decl.Loc.Line)),
                2 => As_Int    (Gint (Candidate_Decl.Loc.Column)),
                3 => As_String (Candidate.Get_Name),
                4 => As_Int    (Gint (Count))));

            if Candidate = Entity then
               Select_Iter (Get_Selection (View), It);
            end if;
         end;

         Next (Iter);
      end loop;

      if Count > 0 then
         Grab_Default (OK_Button);
         Grab_Toplevel_Focus (Get_MDI (Self.Kernel), OK_Button);
         Show_All (Dialog);

         if Run (Dialog) = Gtk_Response_OK then
            Get_Selected (Get_Selection (View), M, It);
            Number_Selected := Natural (Get_Int (M, It, 4));

            Iter := Self.Entities_In_File
              (File    => File,
               Project => Project,
               Name    => Name);

            Count := 0;

            while not At_End (Iter) loop
               Count := Count + 1;
               if Count = Number_Selected then
                  declare
                     Result : constant Root_Entity'Class := Get (Iter);
                  begin
                     Destroy (Dialog);
                     GNATCOLL.Utils.Free (Column_Names);
                     return Result;
                  end;
               end if;
               Next (Iter);
            end loop;
         end if;

         Destroy (Dialog);
      end if;

      GNATCOLL.Utils.Free (Column_Names);

      return No_Root_Entity;

   exception
      when E : others =>
         Trace (Me, E);

         if Dialog /= null then
            Destroy (Dialog);
         end if;

         raise;
   end Select_Entity_Declaration;

   -------------------
   -- Row_Activated --
   -------------------

   procedure Row_Activated (Widget : access Gtk_Widget_Record'Class) is
   begin
      Response (Gtk_Dialog (Widget), Gtk_Response_OK);
   end Row_Activated;

   -------------------
   -- Add_Parameter --
   -------------------

   overriding procedure Add_Parameter
     (Self    : access HTML_Profile_Formater;
      Name    : String;
      Mode    : String;
      Of_Type : String;
      Default : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Self.Has_Parameter then
         Append (Self.Text, ASCII.LF & " ");
      else
         Append (Self.Text, "<b>Parameters:</b>" & ASCII.LF & " ");
         Self.Has_Parameter := True;
      end if;

      if Default = "" then
         --  Keep the parameters aligned, in case some are
         --  optional and start with '['
         Append (Self.Text, " ");
      else
         Append (Self.Text, "<span foreground=""");
         Append (Self.Text, Self.Color_For_Optional_Param);
         Append (Self.Text, """>[");
      end if;

      Append (Self.Text, Escape_Text (Name));
      Append (Self.Text, " : <b>");
      Append (Self.Text, Mode);
      Append (Self.Text, "</b>");
      Append (Self.Text, Escape_Text (Of_Type));

      if Default /= "" then
         Append (Self.Text, " :=");
         Append (Self.Text, Escape_Text (Default));
         Append (Self.Text, "]</span>");
      end if;
   end Add_Parameter;

   ----------------
   -- Add_Result --
   ----------------

   overriding procedure Add_Result
     (Self    : access HTML_Profile_Formater;
      Mode    : String;
      Of_Type : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Self.Has_Parameter then
         Append (Self.Text, ASCII.LF);
         Self.Has_Parameter := False;
      end if;
      Append (Self.Text, "<b>Return:</b>" & ASCII.LF & " <b>");
      Append (Self.Text, Mode);
      Append (Self.Text, "</b>");
      Append (Self.Text, Escape_Text (Of_Type));
   end Add_Result;

   ------------------
   -- Add_Variable --
   ------------------

   overriding procedure Add_Variable
     (Self    : access HTML_Profile_Formater;
      Mode    : String;
      Of_Type : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Append (Self.Text, "<b>Type: ");
      Append (Self.Text, Mode);
      Append (Self.Text, "</b>");
      Append (Self.Text, Escape_Text (Of_Type));
   end Add_Variable;

   -----------------
   -- Add_Aspects --
   -----------------

   overriding procedure Add_Aspects
     (Self : access HTML_Profile_Formater;
      Text : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Self.Has_Parameter then
         Append (Self.Text, ASCII.LF);
         Self.Has_Parameter := False;
      end if;

      if Length (Self.Text) /= 0 then
         Append (Self.Text, ASCII.LF);
      end if;

      Append (Self.Text, "<b>Aspects:</b>" & ASCII.LF);
      Append (Self.Text, Escape_Text (Text));
   end Add_Aspects;

   ------------------
   -- Add_Comments --
   ------------------

   overriding procedure Add_Comments
     (Self : access HTML_Profile_Formater;
      Text : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Self.Has_Parameter then
         Self.Has_Parameter := False;
      end if;

      if Length (Self.Text) = 0 then
         Append (Self.Text, Escape_Text (Text));
      else
         Self.Text := Self.Text & ASCII.LF & ASCII.LF & Escape_Text (Text);
      end if;
   end Add_Comments;

   --------------
   -- Get_Text --
   --------------

   overriding function Get_Text
     (Self : access HTML_Profile_Formater) return String
   is
      use Ada.Strings.Unbounded;
   begin
      if Self.Has_Parameter then
         Append (Self.Text, ASCII.LF);
         Self.Has_Parameter := False;
      end if;

      return To_String (Self.Text);
   end Get_Text;

   -------------------
   -- Documentation --
   -------------------

   function Documentation
     (Self             : General_Xref_Database;
      Handler          : Language_Handlers.Language_Handler;
      Entity           : Root_Entity'Class;
      Color_For_Optional_Param : String := "#555555";
      Raw_Format       : Boolean := False;
      Check_Constructs : Boolean := True) return String
   is
      pragma Unreferenced (Self);
      use Ada.Strings.Unbounded;
   begin
      if Raw_Format then
         declare
            Formater : aliased Text_Profile_Formater;
         begin
            Documentation
              (Handler           => Handler,
               Entity            => Entity,
               Formater          => Formater'Access,
               Check_Constructs  => Check_Constructs,
               Look_Before_First => Doc_Search_Before_First.Get_Pref);

            return Formater.Get_Text;
         end;
      else
         declare
            Formater : aliased HTML_Profile_Formater;
         begin
            Formater.Color_For_Optional_Param :=
              To_Unbounded_String (Color_For_Optional_Param);

            Documentation
              (Handler           => Handler,
               Entity            => Entity,
               Formater          => Formater'Access,
               Check_Constructs  => Check_Constructs,
               Look_Before_First => Doc_Search_Before_First.Get_Pref);

            return Formater.Get_Text;
         end;
      end if;
   end Documentation;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = "reset_xref_db" then
         Kernel.Databases.Reset;

      elsif Command = "xref_db" then
         declare
            F : constant Virtual_File :=
              Kernel.Databases.Xref_Database_Location;
         begin
            if F /= No_File then
               Set_Return_Value (Data, F.Display_Full_Name);
            end if;
         end;
      end if;
   end Default_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      --  Force the creation of a database in any case, to avoid
      --  internal errors even if an invalid project is loaded
      Standard.Xref.Project_Changed (Kernel.Databases);

      Register_Command
        (Kernel, "reset_xref_db",
         Handler => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "xref_db",
         Handler      => Default_Command_Handler'Access);

      Project_Changed_Hook.Add (new On_Project_Changed);
      Project_View_Changed_Hook.Add (new On_Project_View_Changed);
   end Register_Module;

end GPS.Kernel.Xref;
