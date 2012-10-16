------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Commands.Generic_Asynchronous;  use Commands;
with Commands;                       use Commands;
with GNATCOLL.Projects;              use GNATCOLL.Projects;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.Utils;
with GPS.Intl;                       use GPS.Intl;
with GPS.Kernel.Console;             use GPS.Kernel.Console;
with GPS.Kernel.Contexts;            use GPS.Kernel.Contexts;
with GPS.Kernel.Project;             use GPS.Kernel.Project;
with GPS.Kernel.Task_Manager;        use GPS.Kernel.Task_Manager;
with GPS.Kernel;                     use GPS.Kernel;
with Glib.Object;                    use Glib.Object;
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
with Language.Tree.Database;         use Language.Tree.Database;
with Old_Entities.Queries;
with Old_Entities.Values;
with System;                         use System;
with Traces;

package body GPS.Kernel.Xref is
   use Xref;

   Me : constant Trace_Handle := Create ("Xref");

   type All_LI_Information_Command (Name_Len : Natural)
   is new Root_Command with record
      Iter         : Old_Entities.Queries.Recursive_LI_Information_Iterator;
      Lang_Name    : String (1 .. Name_Len);
      Count, Total : Natural := 0;
      Chunk_Size   : Natural := 10;  --  ??? Should be configurable
   end record;

   overriding function Progress
     (Command : access All_LI_Information_Command) return Progress_Record;
   overriding function Execute
     (Command : access All_LI_Information_Command) return Command_Return_Type;
   overriding function Name
     (Command : access All_LI_Information_Command) return String;

   function C_Filter (Lang : String) return Boolean;
   --  Return true if Lang is C or C++ (case insensitive)

   type Examine_Callback is record
      Iter              : Standard.Xref.Entity_Reference_Iterator;
      Kernel            : Kernel_Handle;
      Entity            : General_Entity;
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

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access All_LI_Information_Command) return String is
   begin
      return Command.Lang_Name;
   end Name;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Command : access All_LI_Information_Command) return Progress_Record is
   begin
      return Progress_Record'
        (Activity => Running,
         Current  => Command.Count,
         Total    => Command.Total);
   end Progress;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access All_LI_Information_Command) return Command_Return_Type
   is
   begin
      Old_Entities.Queries.Next (Command.Iter, Steps => Command.Chunk_Size,
            Count => Command.Count, Total => Command.Total);

      if Command.Count >= Command.Total then
         Trace (Me, "Finished loading xref in memory");
         Old_Entities.Queries.Free (Command.Iter);
         return Success;
      else
         if Active (Me) then
            Trace (Me, "Load xref in memory, count="
                   & Command.Count'Img & " total="
                   & Command.Total'Img);
         end if;
         return Execute_Again;
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
      Insert (Kernel => Self.Kernel,
              Text   => Error,
              Add_LF => True,
              Mode   => GPS.Kernel.Console.Error);
   end On_Error;

   -------------------------------
   -- Ensure_Context_Up_To_Date --
   -------------------------------

   procedure Ensure_Context_Up_To_Date (Context : Selection_Context) is
      use Old_Entities;
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_Entity_Name_Information (Context)
        and then Has_Line_Information (Context)
        and then Has_File_Information (Context)
      then
         declare
            Handler : Old_Entities.LI_Handler;
            File    : Old_Entities.Source_File;

         begin
            File :=
              Old_Entities.Get_Or_Create
                (Db   => Kernel.Databases.Entities,
                 File => File_Information (Context));

            Handler := Old_Entities.Get_LI_Handler
              (Kernel.Databases.Entities);

            if Old_Entities.Has_Unresolved_Imported_Refs (Handler) then
               Old_Entities.Set_Update_Forced (Handler);
               Old_Entities.Update_Xref (File);
            end if;
         end;
      end if;
   end Ensure_Context_Up_To_Date;

   --------------
   -- C_Filter --
   --------------

   function C_Filter (Lang : String) return Boolean is
      Str : constant String := To_Lower (Lang);
   begin
      return Str = "c" or else Str = "c++";
   end C_Filter;

   -------------------------
   -- Load_Xref_In_Memory --
   -------------------------

   procedure Load_Xref_In_Memory
     (Kernel       : access Kernel_Handle_Record'Class;
      C_Only       : Boolean)
   is
      use Old_Entities;
      C : Command_Access;
      C_Name : constant String := "load C/C++ xref";
      All_Name : constant String := "load xref";

   begin
      if Active (Me) then
         Trace (Me, "Load xref in memory, c only ? " & C_Only'Img);
      end if;

      if C_Only then
         C := new All_LI_Information_Command
           (Name_Len => C_Name'Length);
         All_LI_Information_Command (C.all).Lang_Name := C_Name;

         Old_Entities.Queries.Start
           (All_LI_Information_Command (C.all).Iter,
            Get_Language_Handler (Kernel),
            Get_Project (Kernel).Start (Recursive => True),
            C_Filter'Access);
      else
         C := new All_LI_Information_Command
           (Name_Len => All_Name'Length);
         All_LI_Information_Command (C.all).Lang_Name := All_Name;
         Old_Entities.Queries.Start
           (All_LI_Information_Command (C.all).Iter,
            Get_Language_Handler (Kernel),
            Get_Project (Kernel).Start (Recursive => True));
      end if;

      GPS.Kernel.Task_Manager.Launch_Background_Command
        (Kernel,
         C,
         Active     => True,
         Show_Bar   => True,
         Queue_Id   => All_LI_Information_Command (C.all).Lang_Name,
         Block_Exit => False);
   end Load_Xref_In_Memory;

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
      Destroy (Data.Iter);
      Unref (Data.Entity);
      Pop_State (Data.Kernel);
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
      Parent : General_Entity;
      Ref    : General_Entity_Reference;
   begin
      if Data.Cancelled then
         Result := Failure;

      elsif At_End (Data.Iter) then
         Result := Success;

      else
         Ref := Get (Data.Iter);
         Result := Execute_Again;

         if Ref /= No_General_Entity_Reference then
            Parent := Get_Caller (Ref);
            if Parent /= No_General_Entity
              and then Data.Kernel.Databases.Show_In_Callgraph (Ref)
            then
               while Parent /= No_General_Entity
                 and then not Data.Kernel.Databases.Is_Container (Parent)
               loop
                  Parent :=
                    Data.Kernel.Databases.Caller_At_Declaration (Parent);
               end loop;

               if Parent /= No_General_Entity then
                  --  If we are seeing a dispatching call to an overridden
                  --  subprogram, this could also result in a call to the
                  --  entity and we report it

                  if Get_Entity (Data.Iter) /= Data.Entity then
                     if Data.Kernel.Databases.Is_Dispatching_Call (Ref) then
                        if not On_Entity_Found
                          (Data.Data, Get_Entity (Data.Iter), Parent, Ref,
                           Through_Dispatching => True,
                           Is_Renaming         => False)
                        then
                           Result := Failure;
                        end if;
                     end if;

                  else
                     if not On_Entity_Found
                       (Data.Data, Data.Entity, Parent, Ref,
                        Through_Dispatching => False,
                        Is_Renaming         => False)
                     then
                        Result := Failure;
                     end if;
                  end if;
               end if;
            end if;
         end if;

         Next (Data.Iter);

         if Command /= null then
            Set_Progress
              (Command,
               (Running,
                Get_Current_Progress (Data.Iter),
                Get_Total_Progress (Data.Iter)));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         Result := Failure;
   end Examine_Ancestors_Idle;

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Kernel            : access Kernel_Handle_Record'Class;
      Entity            : General_Entity;
      User_Data         : access Commands_User_Data_Record'Class;
      Background_Mode   : Boolean := True;
      Dispatching_Calls : Boolean := False;
      Watch             : Gtk.Widget.Gtk_Widget := null)
   is
      Cb     : Examine_Callback_Access;
      Rename : General_Entity;
      C      : Ancestor_Commands.Generic_Asynchronous_Command_Access;
      Result : Command_Return_Type;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      Cb := new Examine_Callback'
        (Kernel            => Kernel_Handle (Kernel),
         Data              => Commands_User_Data (User_Data),
         Entity            => Entity,
         Watch             => Watch,
         Cancelled         => False,
         Dispatching_Calls => Dispatching_Calls,
         Iter              => <>);
      Ref (Entity);

      --  If we have a renaming, report it

      Rename := Kernel.Databases.Renaming_Of (Entity);
      if Rename /= No_General_Entity then
         if not On_Entity_Found
           (User_Data, Entity, Rename, No_General_Entity_Reference,
            Through_Dispatching => False,
            Is_Renaming         => True)
         then
            Destroy_Idle (Cb);
            return;
         end if;
      end if;

      Kernel.Databases.Find_All_References
        (Iter               => Cb.Iter,
         Entity             => Entity,
         Include_Overridden => Dispatching_Calls);

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

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         Pop_State (Kernel_Handle (Kernel));
   end Examine_Ancestors_Call_Graph;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity            : General_Entity;
      User_Data         : access Commands_User_Data_Record'Class;
      Get_All_Refs      : Boolean;
      Dispatching_Calls : Boolean)
   is
      Calls       : Calls_Iterator;
      Called_E    : General_Entity;
      Called_E_Decl : General_Location;
      Refs        : Standard.Xref.Entity_Reference_Iterator;
      Ref         : General_Entity_Reference;
      Data        : Commands_User_Data;
      Is_First    : Boolean;
      Through_Dispatching : Boolean;
   begin
      if Entity /= No_General_Entity then
         Push_State (Kernel_Handle (Kernel), Busy);
         Calls := Kernel.Database.Get_All_Called_Entities (Entity);

         For_Each_Entity :
         while not At_End (Calls) loop
            Called_E := Get (Calls);

            if Called_E /= No_General_Entity
              and then Kernel.Databases.Is_Subprogram (Called_E)
            then
               Called_E_Decl :=
                 Kernel.Databases.Get_Declaration (Called_E).Loc;

               if Get_All_Refs or Dispatching_Calls then
                  --  Now search for all references. This was either
                  --  requested explicitly or is needed to resolve
                  --  dispatching calls

                  Kernel.Databases.Find_All_References
                    (Iter     => Refs,
                     Entity   => Called_E,
                     In_Scope => Entity);
                  Is_First := True;

                  while not At_End (Refs) loop
                     Ref := Get (Refs);
                     if Ref /= No_General_Entity_Reference
                       and then Kernel.Databases.Show_In_Callgraph (Ref)
                       and then Get_Caller (Ref) = Entity
                       and then Kernel.Databases.Is_Subprogram
                         (Get_Entity (Refs))
                       and then Called_E_Decl /= Get_Location (Ref)
                     then
                        --  If we want to see all references, report this
                        --  one now, unless it is a dispatching call which
                        --  is already reported later on

                        if Get_All_Refs then
                           Through_Dispatching :=
                             Kernel.Databases.Is_Dispatching_Call (Ref);

                           if not Dispatching_Calls
                             or else not Through_Dispatching
                           then
                              if not On_Entity_Found
                                (User_Data,
                                 Entity              => Get_Entity (Refs),
                                 Parent              => Entity,
                                 Ref                 => Ref,
                                 Through_Dispatching => Through_Dispatching,
                                 Is_Renaming         => False)
                              then
                                 exit For_Each_Entity;
                              end if;
                           end if;

                           --  Else we only want to report the callee once,
                           --  ie on its first reference. We still have
                           --  to examine all references through to solve
                           --  dispatching calls.

                        elsif Is_First
                          and then not Kernel.Databases.Is_Dispatching_Call
                            (Ref)
                        then
                           Is_First := False;

                           if not On_Entity_Found
                             (User_Data,
                              Entity            => Get_Entity (Refs),
                              Parent            => Entity,
                              Ref               => No_General_Entity_Reference,
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
                              Stop : Boolean := False;
                              function On_Callee
                                (Callee, Primitive_Of : General_Entity)
                                    return Boolean;

                              function On_Callee
                                (Callee, Primitive_Of : General_Entity)
                                    return Boolean
                              is
                                 pragma Unreferenced (Primitive_Of);
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
                              --  Always compute accurate information for
                              --  the call graph, since, as opposed to the
                              --  contextual menu, we have more time to do
                              --  the computation
                              Kernel.Databases.For_Each_Dispatching_Call
                                (Entity    => Get_Entity (Refs),
                                 Ref       => Ref,
                                 On_Callee => On_Callee'Access,
                                 Policy    => Basic_Types.Accurate);
                              exit For_Each_Entity when Stop;
                           end;
                        end if;
                     end if;

                     Next (Refs);
                  end loop;
                  Destroy (Refs);
               else
                  if not On_Entity_Found
                    (User_Data,
                     Entity              => Called_E,
                     Parent              => Entity,
                     Ref                 => No_General_Entity_Reference,
                     Through_Dispatching => False,
                     Is_Renaming         => False)
                  then
                     exit For_Each_Entity;
                  end if;
               end if;
            end if;

            Next (Calls);
         end loop For_Each_Entity;

         Destroy (Calls);

         Destroy (User_Data.all, Cancelled => False);
         Data := Commands_User_Data (User_Data);
         Unchecked_Free (Data);
         Pop_State (Kernel_Handle (Kernel));
      end if;
   end Examine_Entity_Call_Graph;

   ---------------
   -- To_GValue --
   ---------------

   function To_GValue (Entity : General_Entity) return Glib.Values.GValue is
      use Glib.Values;
      Val : GValue;
   begin
      if Active (SQLITE) then
         Init (Val, Glib.GType_Int);
         Set_Int (Val, Gint (Internal_Id (To_New (Entity))));
         return Val;
      else
         return Old_Entities.Values.To_GValue (To_Old (Entity));
      end if;
   end To_GValue;

   -----------------
   -- From_GValue --
   -----------------

   function From_GValue (Value : Glib.Values.GValue) return General_Entity is
      use Glib.Values;
   begin
      if Active (SQLITE) then
         return From_New (From_Internal_Id (Integer (Get_Int (Value))));
      else
         return From_Old (Old_Entities.Values.From_GValue (Value));
      end if;
   end From_GValue;

   ---------------------------------
   -- Get_Entity_Information_Type --
   ---------------------------------

   function Get_Entity_Information_Type return Glib.GType is
   begin
      if Active (SQLITE) then
         return Glib.GType_Int;
      else
         return Old_Entities.Values.Get_Entity_Information_Type;
      end if;
   end Get_Entity_Information_Type;

   ---------------------
   -- Create_Database --
   ---------------------

   procedure Create_Database
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      if Kernel.Databases = null then
         Kernel.Database := new GPS_General_Xref_Database_Record;
         GPS_General_Xref_Database_Record (Kernel.Database.all).Kernel :=
           Kernel_Handle (Kernel);
      end if;

      if Active (SQLITE) then
         if Kernel.Database.Xref = null then
            Kernel.Database.Xref := new GPS.Kernel.Xref.GPS_Xref_Database;
            GPS_Xref_Database (Kernel.Database.Xref.all).Kernel :=
              Kernel_Handle (Kernel);
         end if;
      end if;

      Kernel.Database.Initialize
        (Lang_Handler => Kernel.Lang_Handler,
         Symbols      => Kernel.Symbols,
         Registry     => Kernel.Registry,
         Subprogram_Ref_Is_Call =>
            not Require_GNAT_Date
              (Kernel, Old_Entities.Advanced_Ref_In_Call_Graph_Date));
   end Create_Database;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed (Self : General_Xref_Database) is
   begin
      if Active (SQLITE) then
         --  Create an initial empty database. It will never be filled, and
         --  will be shortly replaced in Project_View_Changed, but it ensures
         --  that GPS does not raise exceptions if some action is performed
         --  while the project has not been computed (like loading of the
         --  desktop for instance).
         --  ??? We really should not be doing anything until the project has
         --  been computed.
         Self.Xref.Setup_DB (GNATCOLL.SQL.Sqlite.Setup (":memory:"));

      else
         --  When loading a new project, we need to reset the cache containing
         --  LI information, otherwise this cache might contain dangling
         --  references to projects that have been freed. Recompute_View does
         --  something similar but tries to limit the files that are reset, so
         --  the calls below will just speed up the processing in
         --  Recompute_View when a new project is loaded.

         Old_Entities.Reset (Self.Entities);
      end if;
   end Project_Changed;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed
     (Self   : General_Xref_Database;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tree : constant Project_Tree_Access := Get_Project_Tree (Kernel);

      procedure Reset_File_If_External (S : in out Old_Entities.Source_File);
      --  Reset the xref info for a source file that no longer belongs to the
      --  project.

      ----------------------------
      -- Reset_File_If_External --
      ----------------------------

      procedure Reset_File_If_External (S : in out Old_Entities.Source_File) is
         Info : constant File_Info :=
           Tree.Info (Old_Entities.Get_Filename (S));
      begin
         if Info.Project = No_Project then
            Old_Entities.Reset (S);
         end if;
      end Reset_File_If_External;

      Dir  : Virtual_File;
      File : Virtual_File;
   begin
      if Active (SQLITE) then
         --  Self.Xref was initialized in Project_Changed.
         Self.Xref.Free;

         Dir := Tree.Root_Project.Object_Dir;

         if Dir = No_File then
            Dir := GNATCOLL.VFS.Get_Current_Dir;
         end if;

         File := Create_From_Dir
           (Dir       => Dir,
            Base_Name => "gnatinspect.db");

         Trace (Me, "Set up xref database: " & File.Display_Full_Name);
         Self.Xref.Setup_DB (GNATCOLL.SQL.Sqlite.Setup (+File.Full_Name.all));

         --  ??? Now would be a good opportunity to update the cross-references
         --  rather than wait for the next compilation.

      else
         --  The list of source or ALI files might have changed, so we need to
         --  reset the cache containing LI information, otherwise this cache
         --  might contain dangling references to projects that have been
         --  freed. We used to do this only when loading a new project, but
         --  in fact that is not sufficient: when we look up xref info for a
         --  source file, if we haven't reset the cache we might get a reply
         --  pointing to a source file in a directory that is no longer part
         --  of the project in the new scenario.
         --
         --  In fact, we only reset the info for those source files that are no
         --  longer part of the project. This might take longer than dropping
         --  the whole database since in the former case we need to properly
         --  handle refcounting whereas Reset takes a shortcut. It is still
         --  probably cleaner to only reset what's needed.

         Old_Entities.Foreach_Source_File
           (Self.Entities, Reset_File_If_External'Access);
      end if;
   end Project_View_Changed;

   --------------------------
   -- Compilation_Finished --
   --------------------------

   procedure Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      C_Only : Boolean)
   is
   begin
      Trace (Me, "Compilation finished, loading xref");
      if Active (SQLITE) then
         --  Nothing to do: the plugin cross_references.py has a special
         --  target that already takes care of re-running gnatinspect when a
         --  compilation is finished.
         null;

      else
         Load_Xref_In_Memory (Kernel, C_Only => C_Only);

      end if;
   end Compilation_Finished;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   procedure Parse_All_LI_Information
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean)
   is
   begin
      if not Active (SQLITE) then
         declare
            use Old_Entities, Old_Entities.Queries;
            Iter : Recursive_LI_Information_Iterator;
            Count, Total : Natural;
         begin
            Start (Iter, Get_Language_Handler (Kernel),
                   Project => Project.Start (Recursive => Recursive));

            loop
               Next (Iter, Steps => Natural'Last,  --  As much as possible
                     Count => Count, Total => Total);
               exit when Count >= Total;
            end loop;

            Free (Iter);
         end;
      end if;
   end Parse_All_LI_Information;

   -------------------------------
   -- Select_Entity_Declaration --
   -------------------------------

   overriding function Select_Entity_Declaration
     (Self   : access GPS_General_Xref_Database_Record;
      File   : Virtual_File;
      Entity : General_Entity) return General_Entity
   is
      procedure Set
        (Tree : System.Address;
         Iter : Gtk_Tree_Iter;
         Col1 : Gint := 0; Value1 : String;
         Col2 : Gint := 1; Value2 : Gint;
         Col3 : Gint := 2; Value3 : Gint);
      pragma Import (C, Set, "ada_gtk_tree_store_set_ptr_int_int");

      Column_Types : constant GType_Array :=
        (0 => GType_String,
         1 => GType_Int,
         2 => GType_Int,
         3 => GType_String,
         4 => Get_Entity_Information_Type);
      Column_Names : GNAT.Strings.String_List :=
        (1 => new String'("File"),
         2 => new String'("Line"),
         3 => new String'("Column"),
         4 => new String'("Name"));

      Name : constant String := Self.Get_Name (Entity);

      Iter      : Entities_In_File_Cursor;
      Candidate : General_Entity;
      Button    : Gtk_Widget;
      OK_Button : Gtk_Widget;
      Count     : Natural := 0;
      Label     : Gtk_Label;
      Model     : Gtk_Tree_Store;
      Dialog    : Gtk_Dialog;
      It        : Gtk_Tree_Iter;
      Scrolled  : Gtk_Scrolled_Window;
      View      : Gtk_Tree_View;
      Col_Num   : Gint;
      Val       : Glib.Values.GValue;
      Candidate_Decl : General_Entity_Declaration;
      Result    : General_Entity;
      pragma Unreferenced (Button, Col_Num);

   begin
      Iter := Self.Entities_In_File
        (File   => File,
         Name   => Name);

      while not At_End (Iter) loop
         Count := Count + 1;
         Candidate := Get (Iter);
         Candidate_Decl := Self.Get_Declaration (Candidate);

         if Count = 1 then
            Gtk_New (Dialog,
                     Title  => -"Select the declaration",
                     Parent => Get_Main_Window (Self.Kernel),
                     Flags  => Modal or Destroy_With_Parent);
            Set_Default_Size (Dialog, 500, 500);

            Gtk_New (Label, -"This entity is overloaded.");
            Pack_Start (Dialog.Get_Action_Area, Label, Expand => False);

            Gtk_New (Label, -"Please select the appropriate declaration.");
            Pack_Start (Dialog.Get_Action_Area, Label, Expand => False);

            Gtk_New (Scrolled);
            Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
            Pack_Start (Dialog.Get_Action_Area, Scrolled);

            OK_Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            View := Create_Tree_View
              (Column_Types       => Column_Types,
               Column_Names       => Column_Names,
               Initial_Sort_On    => 1);
            Add (Scrolled, View);
            Model := Gtk_Tree_Store (Get_Model (View));

            Widget_Callback.Object_Connect
              (View, Signal_Row_Activated, Row_Activated'Access, Dialog);
         end if;

         Append (Model, It, Null_Iter);
         Set (Get_Object (Model), It,
              0, +Candidate_Decl.Loc.File.Base_Name & ASCII.NUL,
              1, Gint (Candidate_Decl.Loc.Line),
              2, Gint (Candidate_Decl.Loc.Column));
         Set (Model, It, 3, Self.Get_Name (Candidate) & ASCII.NUL);
         Set_Value (Model, It, 4, To_GValue (Candidate));

         if Candidate = Entity then
            Select_Iter (Get_Selection (View), It);
         end if;

         Next (Iter);
      end loop;

      Result := No_General_Entity;

      if Count > 0 then
         Grab_Default (OK_Button);
         Grab_Focus (OK_Button);
         Show_All (Dialog);

         if Run (Dialog) = Gtk_Response_OK then
            Get_Selected (Get_Selection (View), Gtk_Tree_Model (Model), It);
            Get_Value (Model, It, 4, Val);
            Result := From_GValue (Val);
         end if;

         Destroy (Dialog);
      end if;

      GNATCOLL.Utils.Free (Column_Names);
      return Result;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);

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

end GPS.Kernel.Xref;
