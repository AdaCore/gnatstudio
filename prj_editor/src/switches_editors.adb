-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Combo;           use Gtk.Combo;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.List;            use Gtk.List;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Radio_Button;    use Gtk.Radio_Button;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Stock;           use Gtk.Stock;
with Gtk.Table;           use Gtk.Table;
with Gtk.Widget;          use Gtk.Widget;

with GNAT.OS_Lib;         use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;

with Prj_API;              use Prj_API;
with Prj_Normalize;        use Prj_Normalize;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with String_Utils;         use String_Utils;
with Switches_Editor_Pkg;  use Switches_Editor_Pkg;

with Namet;               use Namet;
with Types;               use Types;
with Prj;                 use Prj;
with Prj.Tree;            use Prj.Tree;
with Snames;              use Snames;

package body Switches_Editors is

   procedure Filter_Switches
     (Editor   : access Switches_Edit_Record'Class;
      Tool     : Tool_Names;
      Switches : in out GNAT.OS_Lib.Argument_List);
   --  Remove from Switches all the ones that can be set directly from
   --  the GUI. As a result, on exit Switches will only contain non-null
   --  values for the switches that were set manually by the user, and that
   --  don't have GUI equivalents

   function Get_Switches_From_GUI
     (Editor : access Switches_Edit_Record; Tool : Tool_Names)
      return Argument_List;
   --  Return the list of switches that are set in the GUI (as opposed to the
   --  one in the command lines).

   type Switch_Editor_User_Data is record
      Kernel    : Kernel_Handle;
      Project   : Project_Id;
      Switches  : Switches_Edit;
      File_Name : String_Id;
      Directory : String_Id;
   end record;

   package Switch_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Switch_Editor_User_Data);

   procedure Fill_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Prj.Project_Id;
      File_Name : String);
   --  Fill the editor with the switches information for File_Name (or the
   --  default switches if File_Name is the empty string.

   procedure Close_Switch_Editor
     (Switches : access Gtk_Widget_Record'Class;
      Context  : Selection_Context_Access);
   --  Called when the user has closed a switch editor for a specific file.
   --  This modifies the edited project to reflect the changes done in the
   --  dialog.

   procedure Revert_To_Default
     (Switches : access GObject_Record'Class;
      Context  : Selection_Context_Access);
   --  Revert to the default switches in the editor
   --  ??? Should this be specific to a page

   procedure Internal_Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);
   --  Free the memory occupied by the parameter array, but not the strings
   --  themselves.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Switches_Edit) is
   begin
      Editor := new Switches_Edit_Record;
      Switches_Editor_Pkg.Initialize (Editor);
   end Gtk_New;

   -------------------
   -- Destroy_Pages --
   -------------------

   procedure Destroy_Pages
     (Editor : access Switches_Edit_Record; Pages : Page_Filter) is
   begin
      Editor.Pages := Editor.Pages and not Pages;

      if (Pages and Gnatmake_Page) /= 0 then
         Destroy (Editor.Make_Switches);
         Editor.Make_Switches := null;
      end if;

      if (Pages and Ada_Page) /= 0 then
         Destroy (Editor.Ada_Switches);
         Editor.Ada_Switches := null;
      end if;

      if (Pages and C_Page) /= 0 then
         Destroy (Editor.C_Switches);
         Editor.C_Switches := null;
      end if;

      if (Pages and Cpp_Page) /= 0 then
         Destroy (Editor.Cpp_Switches);
         Editor.Cpp_Switches := null;
      end if;

      if (Pages and Binder_Page) /= 0 then
         Destroy (Editor.Binder_Switches);
         Editor.Binder_Switches := null;
      end if;

      if (Pages and Linker_Page) /= 0 then
         Destroy (Editor.Linker_Switches);
         Editor.Linker_Switches := null;
      end if;
   end Destroy_Pages;

   ---------------
   -- Get_Pages --
   ---------------

   function Get_Pages
     (Editor : access Switches_Edit_Record) return Page_Filter is
   begin
      return Editor.Pages;
   end Get_Pages;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Editor : access Switches_Edit_Record) return Gtk.Widget.Gtk_Widget is
   begin
      if Get_Parent (Editor.Vbox2) = Gtk_Widget (Editor) then
         Ref (Editor.Vbox2);
         Unparent (Editor.Vbox2);
      end if;
      return Gtk_Widget (Editor.Vbox2);
   end Get_Window;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Editor : access Switches_Edit_Record; Tool : Tool_Names)
      return Argument_List
   is
      Cmd_Line           : Gtk_Entry;
      Null_Argument_List : Argument_List (1 .. 0);
      List               : Argument_List_Access;

   begin
      case Tool is
         when Gnatmake     => Cmd_Line := Editor.Make_Switches_Entry;
         when Ada_Compiler => Cmd_Line := Editor.Ada_Switches_Entry;
         when C_Compiler   => Cmd_Line := Editor.C_Switches_Entry;
         when Cpp_Compiler => Cmd_Line := Editor.Cpp_Switches_Entry;
         when Binder       => Cmd_Line := Editor.Binder_Switches_Entry;
         when Linker       => Cmd_Line := Editor.Linker_Switches_Entry;
      end case;

      declare
         Str : constant String := Get_Text (Cmd_Line);
      begin
         if Str /= "" then
            List := Argument_String_To_List (Str);

            declare
               Ret : Argument_List := List.all;
            begin
               Internal_Free (List);
               return Ret;
            end;
         end if;
      end;

      return Null_Argument_List;
   end Get_Switches;

   ---------------------------
   -- Get_Switches_From_GUI --
   ---------------------------

   function Get_Switches_From_GUI
     (Editor : access Switches_Edit_Record; Tool : Tool_Names)
      return Argument_List
   is
      procedure Check_Toggle
        (Button : Gtk_Check_Button;
         Str    : String;
         Arr    : in out Argument_List;
         Index  : in out Natural);

      procedure Check_Combo
        (Combo  : Gtk_Combo;
         Switch : String;
         Arr    : in out Argument_List;
         Index  : in out Natural);
      --  Set the parameter (starting with Switch, followed by a numeric
      --  argument) to use if Switch is set.
      --  If the numeric argument is 0, nothing is inserted into Arr.

      ------------------
      -- Check_Toggle --
      ------------------

      procedure Check_Toggle
        (Button : Gtk_Check_Button;
         Str    : String;
         Arr    : in out Argument_List;
         Index  : in out Natural) is
      begin
         if Get_Active (Button) then
            Arr (Index) := new String' (Str);
            Index := Index + 1;
         end if;
      end Check_Toggle;

      -----------------
      -- Check_Combo --
      -----------------

      procedure Check_Combo
        (Combo  : Gtk_Combo;
         Switch : String;
         Arr    : in out Argument_List;
         Index  : in out Natural)
      is
         use Widget_List;
         List  : Gtk_List := Get_List (Combo);
         Value : Integer;

      begin
         --  Check whether there is an actual selection. With gtk+2.0, the
         --  entry emits the "changed" signal more often, even in some cases
         --  where there is no actual selection in the list. However, the
         --  callback is called again later on.

         if Get_Selection (List) /= Null_List then
            Value := Integer (Child_Position
              (List, Get_Data (Get_Selection (List))));

            if Value /= 0 then
               Arr (Index) := new String' (Switch & Image (Value));
               Index := Index + 1;
            end if;
         end if;
      end Check_Combo;

      Num_Switches : Natural;

   begin  --  Get_Switches_From_GUI
      case Tool is
         when Gnatmake     => Num_Switches :=  7 + 1;  --  +1 is for arg to -j
         when Ada_Compiler => Num_Switches := 22;
         when C_Compiler   => Num_Switches := 11;
         when Cpp_Compiler => Num_Switches := 14;
         when Binder       => Num_Switches :=  4;
         when Linker       => Num_Switches :=  3;
      end case;

      declare
         Arr    : Argument_List (1 .. Num_Switches);
         Index  : Natural := Arr'First;
         Active : Boolean;

      begin
         case Tool is
            when Gnatmake =>
               Check_Toggle (Editor.Make_All_Files, "-a", Arr, Index);
               Check_Toggle (Editor.Make_Recompile_Switches, "-s", Arr, Index);
               Check_Toggle (Editor.Make_Minimal_Recompile, "-m", Arr, Index);
               Check_Toggle (Editor.Make_Keep_Going, "-k", Arr, Index);
               Check_Toggle (Editor.Make_Debug, "-g", Arr, Index);
               Check_Toggle (Editor.Make_Mapping_File, "-C", Arr, Index);
               Active := Get_Active (Editor.Make_Debug);

               if Active /= Editor.Prev_Make_Debug then
                  if (Editor.Pages and Ada_Page) /= 0 then
                     Set_Active (Editor.Ada_Debug, Active);
                     Set_Sensitive (Editor.Ada_Debug, not Active);
                  end if;

                  if (Editor.Pages and C_Page) /= 0 then
                     Set_Active (Editor.C_Debug, Active);
                     Set_Sensitive (Editor.C_Debug, not Active);
                  end if;

                  if (Editor.Pages and Cpp_Page) /= 0 then
                     Set_Active (Editor.Cpp_Debug, Active);
                     Set_Sensitive (Editor.Cpp_Debug, not Active);
                  end if;

                  if (Editor.Pages and Linker_Page) /= 0 then
                     Set_Active (Editor.Linker_Debug, Active);
                     Set_Sensitive (Editor.Linker_Debug, not Active);
                  end if;

                  Editor.Prev_Make_Debug := Active;
               end if;

               if Get_Active (Editor.Make_Multiprocessing) then
                  Arr (Index) := new String' ("-j" &
                    Image (Integer (Get_Value_As_Int (Editor.Num_Processes))));
                  Index := Index + 1;
               end if;

            when Ada_Compiler =>
               Check_Combo (Editor.Ada_Optimization_Level, "-O", Arr, Index);
               Check_Toggle
                 (Editor.Ada_No_Inline, "-fno-inline", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Interunit_Inlining, "-gnatN", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Unroll_Loops, "-funroll-loops", Arr, Index);
               Check_Toggle (Editor.Ada_Pic, "-fPIC", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Code_Coverage, "-ftest-coverage", Arr, Index);
               Set_Active
                 (Editor.Ada_Instrument_Arcs,
                  Get_Active (Editor.Ada_Code_Coverage));
               Check_Toggle
                 (Editor.Ada_Instrument_Arcs, "-fprofile-arcs", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Full_Errors, "-gnatf", Arr, Index);
               Check_Toggle
                 (Editor.Ada_No_Warnings, "-gnatws", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Warning_Error, "-gnatwe", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Elab_Warning, "-gnatwl", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Unused_Warning, "-gnatwu", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Style_Checks, "-gnaty", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Overflow_Checking, "-gnato", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Suppress_All_Checks, "-gnatp", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Stack_Checking, "-fstack-check", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Dynamic_Elaboration, "-gnatE", Arr, Index);
               Check_Toggle (Editor.Ada_Debug, "-g", Arr, Index);
               Check_Toggle (Editor.Ada_Assertions, "-gnata", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Debug_Expanded_Code, "-gnatD", Arr, Index);
               Check_Toggle
                 (Editor.Ada_Language_Extensions, "-gnatX", Arr, Index);
               Check_Toggle (Editor.Ada83_Mode, "-gnat83", Arr, Index);

            when C_Compiler =>
               Check_Combo (Editor.C_Optimization_Level, "-O", Arr, Index);
               Check_Toggle (Editor.C_No_Inline, "-fno-inline", Arr, Index);
               Check_Toggle
                 (Editor.C_Unroll_Loops, "-funroll-loops", Arr, Index);
               Check_Toggle (Editor.C_Pic, "-fPIC", Arr, Index);
               Check_Toggle (Editor.C_Profile, "-pg", Arr, Index);
               Set_Active
                 (Editor.Linker_Profile,
                  (Get_Active (Editor.C_Profile))
                   or else ((Editor.Pages and Cpp_Page) /= 0
                     and then Get_Active (Editor.Cpp_Profile)));
               Check_Toggle
                 (Editor.C_Code_Coverage, "-ftest-coverage", Arr, Index);
               Set_Active
                 (Editor.C_Instrument_Arcs,
                  Get_Active (Editor.C_Code_Coverage));
               Check_Toggle
                 (Editor.C_Instrument_Arcs, "-fprofile-arcs", Arr, Index);
               Check_Toggle (Editor.C_Debug, "-g", Arr, Index);
               Check_Toggle (Editor.C_All_Warnings, "-Wall", Arr, Index);
               Check_Toggle (Editor.C_No_Warnings, "-w", Arr, Index);
               Check_Toggle (Editor.C_Ansi, "-ansi", Arr, Index);

            when Cpp_Compiler =>
               Check_Combo (Editor.Cpp_Optimization_Level, "-O", Arr, Index);
               Check_Toggle (Editor.Cpp_No_Inline, "-fno-inline", Arr, Index);
               Check_Toggle
                 (Editor.Cpp_Unroll_Loops, "-funroll-loops", Arr, Index);
               Check_Toggle (Editor.Cpp_Pic, "-fPIC", Arr, Index);
               Check_Toggle (Editor.Cpp_Profile, "-pg", Arr, Index);
               Set_Active
                 (Editor.Linker_Profile,
                  (Get_Active (Editor.Cpp_Profile))
                   or else ((Editor.Pages and C_Page) /= 0
                     and then Get_Active (Editor.C_Profile)));
               Check_Toggle
                 (Editor.Cpp_Code_Coverage, "-ftest-coverage", Arr, Index);
               Set_Active
                 (Editor.Cpp_Instrument_Arcs,
                  Get_Active (Editor.Cpp_Code_Coverage));
               Check_Toggle
                 (Editor.Cpp_Instrument_Arcs, "-fprofile-arcs", Arr, Index);
               Check_Toggle
                 (Editor.Cpp_Exceptions, "-fexceptions", Arr, Index);
               Check_Toggle
                 (Editor.Cpp_Elide_Constructor, "-felide-constructor",
                  Arr, Index);
               Check_Toggle
                 (Editor.Cpp_Conserve_Space, "-fconserve-space", Arr, Index);
               Check_Toggle (Editor.Cpp_Debug, "-g", Arr, Index);
               Check_Toggle (Editor.Cpp_All_Warnings, "-Wall", Arr, Index);
               Check_Toggle (Editor.Cpp_No_Warnings, "-w", Arr, Index);
               Check_Toggle
                 (Editor.Cpp_Overloaded_Virtual, "-Woverloaded-virtual",
                  Arr, Index);

            when Binder =>
               Check_Toggle (Editor.Binder_Tracebacks, "-E", Arr, Index);
               Check_Toggle (Editor.Binder_Restrictions, "-r", Arr, Index);

               if Get_Active (Editor.Binder_Static_Gnat) then
                  Arr (Index) := new String' ("-static");
               else
                  Arr (Index) := new String' ("-shared");
               end if;

               Index := Index + 1;

            when Linker =>
               Check_Toggle (Editor.Linker_Strip, "-s", Arr, Index);
               Check_Toggle (Editor.Linker_Debug, "-g", Arr, Index);
               Check_Toggle (Editor.Linker_Profile, "-pg", Arr, Index);
         end case;

         return Arr (Arr'First .. Index - 1);
      end;
   end Get_Switches_From_GUI;

   ------------------
   -- Set_Switches --
   ------------------

   procedure Set_Switches
     (Editor   : access Switches_Edit_Record;
      Tool     : Tool_Names;
      Switches : Argument_List)
   is
      function Is_Set (Switch : String) return Boolean;
      --  True if Switch is set in Switches

      procedure Set_Combo (Combo : Gtk_Combo; Switch : String);
      --  Check if a switch starts with Switch, and get the numeric argument
      --  after it (set in the combo box)

      ------------
      -- Is_Set --
      ------------

      function Is_Set (Switch : String) return Boolean is
      begin
         for J in Switches'Range loop
            if Switches (J) /= null
              and then Switches (J).all = Switch
            then
               return True;
            end if;
         end loop;

         return False;
      end Is_Set;

      ---------------
      -- Set_Combo --
      ---------------

      procedure Set_Combo (Combo : Gtk_Combo; Switch : String) is
         Level : Gint;
      begin
         for J in Switches'Range loop
            if Switches (J) /= null
              and then Switches (J)'Length >= Switch'Length
              and then Switches (J) (Switches (J)'First
                                     .. Switches (J)'First + Switch'Length - 1)
              = Switch
            then
               if Switches (J)'Length > Switch'Length then
                  --  We need to catch the exception here: If the user started
                  --  with  "-O2 -gnato" and deleted some characters so as to
                  --  get "-O2-gnato", then we cannot interpret the switches.
                  begin
                     Level := Gint'Value
                       (Switches (J) (Switches (J)'First + Switch'Length
                                      .. Switches (J)'Last));
                  exception
                     when Constraint_Error =>
                        Level := 0;
                  end;
               else
                  Level := 0;
               end if;

               if Switch = "-O"
                 and then Switches (J).all = "-O"
               then
                  Select_Item (Get_List (Combo), 1);
               else
                  Select_Item (Get_List (Combo), Level);
               end if;

               return;
            else
               Select_Item (Get_List (Combo), 0);
            end if;
         end loop;
      end Set_Combo;

      Cmd_Line : Gtk_Entry;

   begin
      pragma Assert
        (Tool /= Gnatmake or else (Editor.Pages and Gnatmake_Page) /= 0);
      pragma Assert
        (Tool /= Ada_Compiler or else (Editor.Pages and Ada_Page) /= 0);
      pragma Assert
        (Tool /= C_Compiler or else (Editor.Pages and C_Page) /= 0);
      pragma Assert
        (Tool /= Cpp_Compiler or else (Editor.Pages and Cpp_Page) /= 0);
      pragma Assert
        (Tool /= Binder or else (Editor.Pages and Binder_Page) /= 0);
      pragma Assert
        (Tool /= Linker or else (Editor.Pages and Linker_Page) /= 0);

      case Tool is
         when Gnatmake =>
            Set_Active (Editor.Make_All_Files, Is_Set ("-a"));
            Set_Active (Editor.Make_Recompile_Switches, Is_Set ("-s"));
            Set_Active (Editor.Make_Minimal_Recompile, Is_Set ("-m"));
            Set_Active (Editor.Make_Keep_Going, Is_Set ("-k"));
            Set_Active (Editor.Make_Debug, Is_Set ("-g"));
            Set_Active (Editor.Make_Mapping_File, Is_Set ("-C"));
            Set_Active (Editor.Make_Multiprocessing, False);

            for J in Switches'Range loop
               if Switches (J) /= null
                 and then Switches (J)'Length > 1
                 and then Switches (J) (Switches (J)'First + 1) = 'j'
               then
                  Set_Active (Editor.Make_Multiprocessing, True);

                  begin
                     if Switches (J)'Length > 2 then
                        Set_Value
                          (Editor.Num_Processes,
                           Grange_Float'Value (Switches (J)
                             (Switches (J)'First + 2 .. Switches  (J)'Last)));

                     else
                        Set_Value (Editor.Num_Processes, 0.0);
                     end if;

                  exception
                     when Constraint_Error =>
                        Set_Value (Editor.Num_Processes, 0.0);
                  end;
               end if;
            end loop;

            Cmd_Line := Editor.Make_Switches_Entry;

         when Ada_Compiler =>
            Set_Combo (Editor.Ada_Optimization_Level, "-O");
            Set_Active (Editor.Ada_No_Inline, Is_Set ("-fno-inline"));
            Set_Active (Editor.Ada_Interunit_Inlining, Is_Set ("-gnatN"));
            Set_Active (Editor.Ada_Unroll_Loops, Is_Set ("-funroll-loops"));
            Set_Active (Editor.Ada_Pic, Is_Set ("-fPIC"));
            Set_Active (Editor.Ada_Code_Coverage, Is_Set ("-ftest-coverage"));
            Set_Active (Editor.Ada_Instrument_Arcs, Is_Set ("-fprofile-arcs"));
            Set_Active (Editor.Ada_Full_Errors, Is_Set ("-gnatf"));
            Set_Active (Editor.Ada_No_Warnings, Is_Set ("-gnatws"));
            Set_Active (Editor.Ada_Warning_Error, Is_Set ("-gnatwe"));
            Set_Active (Editor.Ada_Elab_Warning, Is_Set ("-gnatwl"));
            Set_Active (Editor.Ada_Unused_Warning, Is_Set ("-gnatwu"));
            Set_Active (Editor.Ada_Style_Checks, Is_Set ("-gnaty"));
            Set_Active (Editor.Ada_Overflow_Checking, Is_Set ("-gnato"));
            Set_Active (Editor.Ada_Suppress_All_Checks, Is_Set ("-gnatp"));
            Set_Active (Editor.Ada_Stack_Checking, Is_Set ("-fstack-check"));
            Set_Active (Editor.Ada_Dynamic_Elaboration, Is_Set ("-gnatE"));
            Set_Active (Editor.Ada_Debug, Is_Set ("-g"));
            Set_Active (Editor.Ada_Assertions, Is_Set ("-gnata"));
            Set_Active (Editor.Ada_Debug_Expanded_Code, Is_Set ("-gnatD"));
            Set_Active (Editor.Ada_Language_Extensions, Is_Set ("-gnatX"));
            Set_Active (Editor.Ada83_Mode, Is_Set ("-gnat83"));

            Cmd_Line := Editor.Ada_Switches_Entry;

         when C_Compiler =>
            Set_Combo (Editor.C_Optimization_Level, "-O");
            Set_Active (Editor.C_No_Inline, Is_Set ("-fno-inline"));
            Set_Active (Editor.C_Unroll_Loops, Is_Set ("-funroll-loops"));
            Set_Active (Editor.C_Pic, Is_Set ("-fPIC"));
            Set_Active (Editor.C_Profile, Is_Set ("-pg"));
            Set_Active (Editor.C_Code_Coverage, Is_Set ("-ftest-coverage"));
            Set_Active (Editor.C_Instrument_Arcs, Is_Set ("-fprofile-arcs"));
            Set_Active (Editor.C_Debug, Is_Set ("-g"));
            Set_Active (Editor.C_All_Warnings, Is_Set ("-Wall"));
            Set_Active (Editor.C_No_Warnings, Is_Set ("-w"));
            Set_Active (Editor.C_Ansi, Is_Set ("-ansi"));

            Cmd_Line := Editor.C_Switches_Entry;

         when Cpp_Compiler =>
            Set_Combo (Editor.Cpp_Optimization_Level, "-O");
            Set_Active (Editor.Cpp_No_Inline, Is_Set ("-fno-inline"));
            Set_Active (Editor.Cpp_Unroll_Loops, Is_Set ("-funroll-loops"));
            Set_Active (Editor.Cpp_Pic, Is_Set ("-fPIC"));
            Set_Active (Editor.Cpp_Profile, Is_Set ("-pg"));
            Set_Active (Editor.Cpp_Code_Coverage, Is_Set ("-ftest-coverage"));
            Set_Active (Editor.Cpp_Instrument_Arcs, Is_Set ("-fprofile-arcs"));
            Set_Active (Editor.Cpp_Exceptions, Is_Set ("-fexceptions"));
            Set_Active
              (Editor.Cpp_Elide_Constructor, Is_Set ("-felide-constructor"));
            Set_Active
              (Editor.Cpp_Conserve_Space, Is_Set ("-fconserve-space"));
            Set_Active (Editor.Cpp_Debug, Is_Set ("-g"));
            Set_Active (Editor.Cpp_All_Warnings, Is_Set ("-Wall"));
            Set_Active (Editor.Cpp_No_Warnings, Is_Set ("-w"));
            Set_Active
              (Editor.Cpp_Overloaded_Virtual, Is_Set ("-Woverloaded-virtual"));

            Cmd_Line := Editor.Cpp_Switches_Entry;

         when Binder =>
            Set_Active (Editor.Binder_Tracebacks, Is_Set ("-E"));
            Set_Active (Editor.Binder_Restrictions, Is_Set ("-r"));
            Set_Active (Editor.Binder_Static_Gnat, Is_Set ("-static"));
            Set_Active (Editor.Binder_Shared_Gnat, Is_Set ("-shared"));

            Cmd_Line := Editor.Binder_Switches_Entry;

         when Linker =>
            Set_Active (Editor.Linker_Strip, Is_Set ("-s"));
            Set_Active (Editor.Linker_Debug, Is_Set ("-g"));
            Set_Active (Editor.Linker_Profile, Is_Set ("-pg"));

            Cmd_Line := Editor.Linker_Switches_Entry;
      end case;

      if not Editor.Block_Refresh then
         Editor.Block_Refresh := True;

         Set_Text (Cmd_Line, "");

         for K in Switches'Range loop
            if Switches (K) /= null then
               Append_Text (Cmd_Line, Switches (K).all & " ");
            end if;
         end loop;

         Editor.Block_Refresh := False;
      end if;
   end Set_Switches;

   ---------------------
   -- Filter_Switches --
   ---------------------

   procedure Filter_Switches
     (Editor   : access Switches_Edit_Record'Class;
      Tool     : Tool_Names;
      Switches : in out GNAT.OS_Lib.Argument_List) is
   begin
      --  Note: We do not filter if the page is not displayed, so that the
      --  switches do not actually disappear when the switches editor is
      --  closed.

      case Tool is
         when Gnatmake =>
            if (Editor.Pages and Gnatmake_Page) /= 0 then
               for J in Switches'Range loop
                  if Switches (J) /= null and then
                    (Switches (J).all = "-a"
                     or else Switches (J).all = "-s"
                     or else Switches (J).all = "-m"
                     or else Switches (J).all = "-k"
                     or else Switches (J).all = "-g"
                     or else Switches (J).all = "-C"
                     or else (Switches (J)'Length >= 2 and then
                       Switches (J) (Switches (J)'First ..
                                     Switches (J)'First + 1) = "-j"))
                  then
                     Free (Switches (J));
                  end if;
               end loop;
            end if;

         when Ada_Compiler =>
            if (Editor.Pages and Ada_Page) /= 0 then
               for J in Switches'Range loop
                  if Switches (J) /= null and then
                    ((Switches (J)'Length >= 2
                      and then Switches (J) (Switches (J)'First ..
                                             Switches (J)'First + 1) = "-O")
                     or else Switches (J).all = "-fno-inline"
                     or else Switches (J).all = "-gnatN"
                     or else Switches (J).all = "-funroll-loops"
                     or else Switches (J).all = "-fPIC"
                     or else Switches (J).all = "-ftest-coverage"
                     or else Switches (J).all = "-fprofile-arcs"
                     or else Switches (J).all = "-gnatf"
                     or else Switches (J).all = "-gnatws"
                     or else Switches (J).all = "-gnatwe"
                     or else Switches (J).all = "-gnatwl"
                     or else Switches (J).all = "-gnatwu"
                     or else Switches (J).all = "-gnaty"
                     or else Switches (J).all = "-gnato"
                     or else Switches (J).all = "-gnatp"
                     or else Switches (J).all = "-fstack-check"
                     or else Switches (J).all = "-gnatE"
                     or else Switches (J).all = "-g"
                     or else Switches (J).all = "-gnata"
                     or else Switches (J).all = "-gnatD"
                     or else Switches (J).all = "-gnatX"
                     or else Switches (J).all = "-gnat83")
                  then
                     Free (Switches (J));
                  end if;
               end loop;
            end if;

         when C_Compiler =>
            if (Editor.Pages and C_Page) /= 0 then
               for J in Switches'Range loop
                  if Switches (J) /= null and then
                    ((Switches (J)'Length >= 2
                      and then Switches (J) (Switches (J)'First ..
                                             Switches (J)'First + 1) = "-O")
                     or else Switches (J).all = "-fno-inline"
                     or else Switches (J).all = "-funroll-loops"
                     or else Switches (J).all = "-fPIC"
                     or else Switches (J).all = "-pg"
                     or else Switches (J).all = "-ftest-coverage"
                     or else Switches (J).all = "-fprofile-arcs"
                     or else Switches (J).all = "-g"
                     or else Switches (J).all = "-Wall"
                     or else Switches (J).all = "-w"
                     or else Switches (J).all = "-ansi")
                  then
                     Free (Switches (J));
                  end if;
               end loop;
            end if;

         when Cpp_Compiler =>
            if (Editor.Pages and Cpp_Page) /= 0 then
               for J in Switches'Range loop
                  if Switches (J) /= null and then
                    ((Switches (J)'Length >= 2
                      and then Switches (J) (Switches (J)'First ..
                                             Switches (J)'First + 1) = "-O")
                     or else Switches (J).all = "-fno-inline"
                     or else Switches (J).all = "-funroll-loops"
                     or else Switches (J).all = "-fPIC"
                     or else Switches (J).all = "-pg"
                     or else Switches (J).all = "-ftest-coverage"
                     or else Switches (J).all = "-fprofile-arcs"
                     or else Switches (J).all = "-fexceptions"
                     or else Switches (J).all = "-felide-constructor"
                     or else Switches (J).all = "-fconserve-space"
                     or else Switches (J).all = "-g"
                     or else Switches (J).all = "-Wall"
                     or else Switches (J).all = "-w"
                     or else Switches (J).all = "-Woverloaded-virtual")
                  then
                     Free (Switches (J));
                  end if;
               end loop;
            end if;

         when Binder =>
            if (Editor.Pages and Binder_Page) /= 0 then
               for J in Switches'Range loop
                  if Switches (J) /= null and then
                    (Switches (J).all = "-E"
                     or else Switches (J).all = "-r"
                     or else Switches (J).all = "-static"
                     or else Switches (J).all = "-shared")
                  then
                     Free (Switches (J));
                  end if;
               end loop;
            end if;

         when Linker =>
            if (Editor.Pages and Linker_Page) /= 0 then
               for J in Switches'Range loop
                  if Switches (J) /= null and then
                    (Switches (J).all = "-s"
                     or else Switches (J).all = "-g"
                     or else Switches (J).all = "-pg")
                  then
                     Free (Switches (J));
                  end if;
               end loop;
            end if;

      end case;
   end Filter_Switches;

   --------------------
   -- Update_Cmdline --
   --------------------

   procedure Update_Cmdline
     (Editor : access Switches_Edit_Record; Tool : Tool_Names)
   is
      Cmd_Line : Gtk_Entry;
   begin
      --  Don't do anything if the callbacks were blocked, to avoid infinite
      --  loops while we are updating the command line, and it is updating
      --  the buttons, that are updating the command line,...
      if Editor.Block_Refresh then
         return;
      end if;

      case Tool is
         when Gnatmake     => Cmd_Line := Editor.Make_Switches_Entry;
         when Ada_Compiler => Cmd_Line := Editor.Ada_Switches_Entry;
         when C_Compiler   => Cmd_Line := Editor.C_Switches_Entry;
         when Cpp_Compiler => Cmd_Line := Editor.Cpp_Switches_Entry;
         when Binder       => Cmd_Line := Editor.Binder_Switches_Entry;
         when Linker       => Cmd_Line := Editor.Linker_Switches_Entry;
      end case;

      declare
         Str     : constant String := Get_Text (Cmd_Line);
         Arr     : Argument_List := Get_Switches_From_GUI (Editor, Tool);
         Current : Argument_List_Access;

      begin
         if Str'Length = 0 then
            Current := new Argument_List (1 .. 0);
         else
            Current := Argument_String_To_List (Str);
         end if;

         Editor.Block_Refresh := True;
         Set_Text (Cmd_Line, "");

         for J in Arr'Range loop
            Append_Text (Cmd_Line, Arr (J).all & " ");
         end loop;

         --  Keep the switches set manually by the user

         Filter_Switches (Editor, Tool, Current.all);

         for K in Current'Range loop
            if Current (K) /= null then
               Append_Text (Cmd_Line, Current (K).all & " ");
            end if;
         end loop;

         Editor.Block_Refresh := False;

         Free (Arr);
         Free (Current);
      end;
   end Update_Cmdline;

   -----------------------------
   -- Update_Gui_From_Cmdline --
   -----------------------------

   procedure Update_Gui_From_Cmdline
     (Editor : access Switches_Edit_Record; Tool : Tool_Names)
   is
      Cmd_Line : Gtk_Entry;
   begin
      if Editor.Block_Refresh then
         return;
      end if;

      case Tool is
         when Gnatmake     => Cmd_Line := Editor.Make_Switches_Entry;
         when Ada_Compiler => Cmd_Line := Editor.Ada_Switches_Entry;
         when C_Compiler   => Cmd_Line := Editor.C_Switches_Entry;
         when Cpp_Compiler => Cmd_Line := Editor.Cpp_Switches_Entry;
         when Binder       => Cmd_Line := Editor.Binder_Switches_Entry;
         when Linker       => Cmd_Line := Editor.Linker_Switches_Entry;
      end case;

      declare
         Arg : Argument_List_Access :=
           Argument_String_To_List (Get_Text (Cmd_Line));

      begin
         Editor.Block_Refresh := True;
         Set_Switches (Editor, Tool, Arg.all);
         Free (Arg);
         Editor.Block_Refresh := False;
      end;
   end Update_Gui_From_Cmdline;

   ----------
   -- Free --
   ----------

   procedure Free (Switches : in out Argument_List) is
   begin
      for J in Switches'Range loop
         Free (Switches (J));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Switches : in out GNAT.OS_Lib.Argument_List_Access) is
   begin
      Free (Switches.all);
      Internal_Free (Switches);
   end Free;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page
     (Editor : access Switches_Edit_Record; Tool : Tool_Names) is
   begin
      Set_Page (Editor.Notebook, Tool_Names'Pos (Tool));
   end Set_Page;

   -----------------------
   -- Revert_To_Default --
   -----------------------

   procedure Revert_To_Default
     (Switches : access GObject_Record'Class;
      Context  : Selection_Context_Access) is
   begin
      Fill_Editor
        (Switches_Edit (Switches),
         Project_Information (File_Selection_Context_Access (Context)),
         "");
   end Revert_To_Default;

   -------------------------
   -- Close_Switch_Editor --
   -------------------------

   procedure Close_Switch_Editor
     (Switches : access Gtk_Widget_Record'Class;
      Context  : Selection_Context_Access)
   is
      File    : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      S       : Switches_Edit   := Switches_Edit (Switches);
      Project : Project_Node_Id := Get_Project_From_View
        (Project_Information (File));

      procedure Change_Switches
        (Tool : Tool_Names; Pkg_Name : String; Language : Name_Id);
      --  Changes the switches for a specific package and tool.

      ---------------------
      -- Change_Switches --
      ---------------------

      procedure Change_Switches
        (Tool : Tool_Names; Pkg_Name : String; Language : Name_Id)
      is
         Args     : Argument_List := Get_Switches (S, Tool);
         Args_Tmp : Argument_List := Args;
         Value    : Variable_Value;
         Is_Default_Value : Boolean := False;
      begin
         if Has_File_Information (File) then
            Get_Switches
              (Project          => Project_Information (File),
               In_Pkg           => Pkg_Name,
               File             => "",
               Language         => Language,
               Value            => Value,
               Is_Default_Value => Is_Default_Value);

            --  Check if we in fact have the default value

            declare
               Default_Args : Argument_List := To_Argument_List (Value);
            begin
               for J in Args_Tmp'Range loop
                  for K in Default_Args'Range loop
                     if Default_Args (K) /= null
                       and then Default_Args (K).all = Args_Tmp (J).all
                     then
                        Free (Default_Args (K));
                        Args_Tmp (J) := null;
                        exit;
                     end if;
                  end loop;
               end loop;

               Is_Default_Value := True;
               for J in Args_Tmp'Range loop
                  if Args_Tmp (J) /= null then
                     Is_Default_Value := False;
                     exit;
                  end if;
               end loop;

               for K in Default_Args'Range loop
                  if Default_Args (K) /= null then
                     Is_Default_Value := False;
                     exit;
                  end if;
               end loop;

               Free (Default_Args);
            end;

            --  Editing the default switches for a specific language ?

            if Is_Default_Value then
               Delete_Attribute
                 (Project            => Project,
                  Pkg_Name           => Pkg_Name,
                  Scenario_Variables => Scenario_Variables (S.Kernel),
                  Attribute_Name     => Get_Name_String (Name_Switches),
                  Attribute_Index    => File_Information (File));
            else
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Pkg_Name,
                  Scenario_Variables => Scenario_Variables (S.Kernel),
                  Attribute_Name     => Get_Name_String (Name_Switches),
                  Values             => Args,
                  Attribute_Index    => File_Information (File),
                  Prepend            => False);
            end if;

         else
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Pkg_Name           => Pkg_Name,
               Scenario_Variables => Scenario_Variables (S.Kernel),
               Attribute_Name    => Get_Name_String (Name_Default_Switches),
               Values             => Args,
               Attribute_Index    => Get_Name_String (Language),
               Prepend            => False);
         end if;
         Free (Args);
      end Change_Switches;

   begin
      pragma Assert (Project /= Empty_Node);

      --  Normalize the subproject we are currently working on, since we only
      --  know how to modify normalized subprojects.

      Normalize (Project);

      if (Get_Pages (S) and Gnatmake_Page) /= 0 then
         --  ??? Currently, we only edit the default switches for Ada
         Change_Switches (Gnatmake, "builder", Snames.Name_Ada);
      end if;

      if (Get_Pages (S) and Ada_Page) /= 0 then
         Change_Switches (Ada_Compiler, "compiler", Snames.Name_Ada);
      end if;

      if (Get_Pages (S) and C_Page) /= 0 then
         Change_Switches (C_Compiler, "compiler", Snames.Name_C);
      end if;

      if (Get_Pages (S) and Cpp_Page) /= 0 then
         Change_Switches (Cpp_Compiler, "compiler", Snames.Name_CPP);
      end if;

      if (Get_Pages (S) and Binder_Page) /= 0 then
         --  ??? Currently, we only edit the default switches for Ada
         Change_Switches (Binder, "binder", Snames.Name_Ada);
      end if;

      if (Get_Pages (S) and Linker_Page) /= 0 then
         --  ??? Currently, we only edit the default switches for Ada
         Change_Switches (Linker, "linker", Snames.Name_Ada);
      end if;

      Recompute_View (S.Kernel);
   end Close_Switch_Editor;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Prj.Project_Id;
      File_Name : String)
   is
      Value      : Variable_Value;
      Is_Default : Boolean;
   begin
      if File_Name /= "" then
         --  ??? Need to decide depending on the langage
         Destroy_Pages
           (Switches, Gnatmake_Page or C_Page or Cpp_Page or
            Binder_Page or Linker_Page);
      end if;

      --  Set the switches for all the pages
      if (Get_Pages (Switches) and Gnatmake_Page) /= 0 then
         --  ??? This will only show Ada switches
         Get_Switches (Project, "builder", File_Name,
                       Snames.Name_Ada, Value, Is_Default);
         declare
            List : Argument_List := To_Argument_List (Value);
         begin
            Set_Switches (Switches, Gnatmake, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and Ada_Page) /= 0 then
         Get_Switches (Project, "compiler", File_Name,
                       Snames.Name_Ada, Value, Is_Default);
         declare
            List : Argument_List := To_Argument_List (Value);
         begin
            Set_Switches (Switches, Ada_Compiler, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and C_Page) /= 0 then
         Get_Switches (Project, "compiler", File_Name,
                       Snames.Name_C, Value, Is_Default);
         declare
            List : Argument_List := To_Argument_List (Value);
         begin
            Set_Switches (Switches, C_Compiler, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and Cpp_Page) /= 0 then
         Get_Switches (Project, "compiler", File_Name,
                       Snames.Name_CPP, Value, Is_Default);
         declare
            List : Argument_List := To_Argument_List (Value);
         begin
            Set_Switches (Switches, Cpp_Compiler, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and Binder_Page) /= 0 then
         --  ??? This will only show Ada switches
         Get_Switches (Project, "binder", File_Name,
                       Snames.Name_Ada, Value, Is_Default);
         declare
            List : Argument_List := To_Argument_List (Value);
         begin
            Set_Switches (Switches, Binder, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and Linker_Page) /= 0 then
         --  ??? This will only show Ada switches
         Get_Switches (Project, "linker", File_Name,
                       Snames.Name_Ada, Value, Is_Default);
         declare
            List : Argument_List := To_Argument_List (Value);
         begin
            Set_Switches (Switches, Linker, List);
            Free (List);
         end;
      end if;
   end Fill_Editor;

   -------------------------------
   -- Edit_Switches_For_Context --
   -------------------------------

   procedure Edit_Switches_For_Context
     (Context       : Selection_Context_Access;
      Force_Default : Boolean := False)
   is
      File : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Switches : Switches_Edit;
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      B      : Gtk_Button;
      File_Name : String_Access;

   begin
      pragma Assert (Has_Project_Information (File));

      if not Force_Default and then Has_File_Information (File) then
         File_Name := new String' (File_Information (File));
      else
         File_Name := new String' ("");
      end if;

      if File_Name.all /= "" then
         Gtk_New (Dialog,
                  Title  => "Editing switches for " & File_Name.all,
                  Parent => Get_Main_Window (Get_Kernel (Context)),
                  Flags  => Modal or Destroy_With_Parent);
      else
         Gtk_New (Dialog,
                  Title  => "Editing default switches for project "
                    & Project_Name (Project_Information (File)),
                  Parent => Get_Main_Window (Get_Kernel (Context)),
                  Flags  => Modal or Destroy_With_Parent);
      end if;

      Gtk_New (Switches);
      Switches.Kernel := Kernel_Handle (Get_Kernel (Context));
      Pack_Start (Get_Vbox (Dialog),
                  Get_Window (Switches), Fill => True, Expand => True);

      Fill_Editor (Switches, Project_Information (File), File_Name.all);

      if File_Name.all /= "" then
         Gtk_New_From_Stock (B, Stock_Revert_To_Saved);
         Pack_Start (Get_Action_Area (Dialog), B);
         Context_Callback.Object_Connect
           (B, "clicked",
            Context_Callback.To_Marshaller (Revert_To_Default'Access),
            Slot_Object => Switches, User_Data => Context);
      end if;

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      --  Note: if the dialog is no longer modal, then we need to create a copy
      --  of the context for storing in the callback, since the current context
      --  will be automatically freed by the kernel at some point in the life
      --  of this dialog.

      if Run (Dialog) = Gtk_Response_OK then
         Close_Switch_Editor (Switches, Context);
      end if;

      Free (File_Name);

      Destroy (Dialog);
   end Edit_Switches_For_Context;

   -------------------
   -- Edit_Switches --
   -------------------

   procedure Edit_Switches
     (Item    : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Item);
   begin
      Edit_Switches_For_Context (Context, False);
   end Edit_Switches;

   ---------------------------
   -- Edit_Default_Switches --
   ---------------------------

   procedure Edit_Default_Switches
     (Item : access GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access)
   is
      pragma Unreferenced (Item);
   begin
      Edit_Switches_For_Context (Context, True);
   end Edit_Default_Switches;

end Switches_Editors;
