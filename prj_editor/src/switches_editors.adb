-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;

with GNAT.OS_Lib;         use GNAT.OS_Lib;

with Prj_API;              use Prj_API;
with Prj_Normalize;        use Prj_Normalize;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Intl;           use Glide_Intl;
with Language_Handlers;    use Language_Handlers;
with String_Utils;         use String_Utils;
with Switches_Editor_Pkg;  use Switches_Editor_Pkg;
with Basic_Types;          use Basic_Types;

with Namet;                use Namet;
with Types;                use Types;
with Prj;                  use Prj;
with Prj.Tree;             use Prj.Tree;
with Snames;               use Snames;
with Switch.M;             use Switch.M;

with Ada.Exceptions;       use Ada.Exceptions;
with Traces;               use Traces;

package body Switches_Editors is

   Me : constant Debug_Handle := Create ("Switches_Editors");

   Conversion_Data_String : constant String := "gps_switches_to_window";

   package Object_User_Data is new Glib.Object.User_Data (GObject);

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
      Files     : Argument_List);
   --  Fill the editor with the switches information for Files (or the
   --  default switches if Files is empty).

   procedure Close_Switch_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Project_Node_Id;
      Project_View : Project_Id;
      Files     : Argument_List);
   --  Called when the user has closed a switch editor for a specific file.
   --  This modifies the edited project to reflect the changes done in the
   --  dialog.
   --  File_Name is the name of the file whose switches we are changing, or ""
   --  if we are changing the default switches.

   procedure Revert_To_Default (Switches : access Gtk_Widget_Record'Class);
   --  Revert to the default switches in the editor
   --  ??? Should this be specific to a page

   function Normalize_Compiler_Switches
     (Tool : Tool_Names; Switches : Argument_List) return Argument_List;
   --  Return an equivalent of Switches, but where concatenated switches have
   --  been separated (for instance, -gnatwue = -gnatwu -gnatwe).
   --  Nothing is done if the tool doesn't need this special treatment.
   --  The returned array should be freed. However, you no longer need to free
   --  the memory for the array that was passed as a parameter (we either
   --  return it directly, or reuse the strings from it for the output).

   procedure Set_Visible_Pages
     (Editor : access Switches_Edit_Record'Class; Pages : Page_Filter);
   --  Only the pages described in Pages will be visible. All others pages are
   --  shown

   function Get_Pages
     (Editor : access Switches_Edit_Record'Class) return Page_Filter;
   --  Return the list of pages that are visible in the switches editor.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Switches_Edit) is
      Default_Width : constant := 12;
   begin
      Editor := new Switches_Edit_Record;
      Switches_Editor_Pkg.Initialize (Editor);
      Set_Width_Chars (Editor.Keyword_Casing_Entry, Default_Width);
      Set_Width_Chars (Editor.Attribute_Casing_Entry, Default_Width);
      Set_Width_Chars (Editor.References_Casing_Entry, Default_Width);
      Set_Width_Chars (Editor.Pragma_Casing_Entry, Default_Width);
      Set_Current_Page (Editor.Notebook, 0);
   end Gtk_New;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor : access Switches_Edit_Record'Class; Pages : Page_Filter)
   is
      procedure Hide_Or_Show
        (Page   : Page_Filter; Widget : access Gtk_Widget_Record'Class);
      --  Hide or show a page

      ------------------
      -- Hide_Or_Show --
      ------------------

      procedure Hide_Or_Show
        (Page   : Page_Filter; Widget : access Gtk_Widget_Record'Class) is
      begin
         if (Pages and Page) = 0 then
            Hide (Widget);
         else
            Show (Widget);
         end if;
      end Hide_Or_Show;

      Current : Gint := Get_Current_Page (Editor.Notebook);
   begin
      Hide_Or_Show (Gnatmake_Page, Editor.Make_Switches);
      Hide_Or_Show (Ada_Page, Editor.Ada_Switches);
      Hide_Or_Show (C_Page, Editor.C_Switches);
      Hide_Or_Show (Cpp_Page, Editor.Cpp_Switches);
      Hide_Or_Show (Pretty_Printer_Page, Editor.Pp_Switches);
      Hide_Or_Show (Binder_Page, Editor.Binder_Switches);
      Hide_Or_Show (Linker_Page, Editor.Linker_Switches);
      Editor.Pages := Pages;

      --  Work around an apparent bug in gtk+: when the contents of a page is
      --  hidden, and the shown again, it is always displayed on top of the
      --  current page in the notebook. We thus see the contents of two or more
      --  pages at the same time...
      if Current = -1 then
         Current := 0;
      end if;
      Set_Current_Page (Editor.Notebook, Current);
   end Set_Visible_Pages;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor : access Switches_Edit_Record; Languages : Argument_List)
   is
      Pages : Page_Filter := Gnatmake_Page or Binder_Page or Linker_Page;
   begin
      for J in Languages'Range loop
         declare
            Lang : String := Languages (J).all;
         begin
            Lower_Case (Lang);

            if Lang = "ada" then
               Pages := Pages or Ada_Page or Pretty_Printer_Page;
            elsif Lang = "c" then
               Pages := Pages or C_Page;
            elsif Lang = "c++" then
               Pages := Pages or Cpp_Page;
            end if;
         end;
      end loop;
      Set_Visible_Pages (Editor, Pages);
   end Set_Visible_Pages;

   ---------------
   -- Get_Pages --
   ---------------

   function Get_Pages
     (Editor : access Switches_Edit_Record'Class) return Page_Filter is
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
         Object_User_Data.Set
           (Editor.Vbox2, GObject (Editor), Conversion_Data_String);
      end if;
      return Gtk_Widget (Editor.Vbox2);
   end Get_Window;

   -----------------
   -- From_Window --
   -----------------

   function From_Window
     (Window : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Switches_Edit is
   begin
      return Switches_Edit
        (Object_User_Data.Get (Window, Conversion_Data_String));
   end From_Window;

   ---------------------------------
   -- Normalize_Compiler_Switches --
   ---------------------------------

   function Normalize_Compiler_Switches
     (Tool : Tool_Names; Switches : Argument_List) return Argument_List
   is
      Output, Tmp : Argument_List_Access;
      Out_Index : Natural;
   begin
      --  For Ada switches, use the functions provided by GNAT that
      --  provide the splitting of composite switches like "-gnatwue"
      --  into "-gnatwu -gnatwe"
      if Tool = Ada_Compiler then
         Output := new Argument_List (Switches'Range);
         Out_Index := Switches'First;

         for Index in Switches'Range loop
            declare
               Arr : constant Argument_List :=
                 Normalize_Compiler_Switches (Switches (Index).all);
            begin
               Output (Out_Index) := Switches (Index);

               --  If the switch was already as simple as possible, or wasn't
               --  recognized at all.
               if Arr'Length <= 1 then
                  Out_Index := Out_Index + 1;

               else
                  Free (Output (Out_Index));

                  Tmp := new Argument_List
                    (Output'First .. Output'Last + Arr'Length - 1);

                  Tmp (Tmp'First .. Out_Index - 1) :=
                    Output (Output'First .. Out_Index - 1);
                  for A in Arr'Range loop
                     Tmp (Out_Index) := new String' (Arr (A).all);
                     Out_Index := Out_Index + 1;
                  end loop;

                  Unchecked_Free (Output);
                  Output := Tmp;
               end if;
            end;
         end loop;

         declare
            O : constant Argument_List := Output.all;
         begin
            Unchecked_Free (Output);
            return O;
         end;

      else
         return Switches;
      end if;
   end Normalize_Compiler_Switches;

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
         when Gnatmake       => Cmd_Line := Editor.Make_Switches_Entry;
         when Ada_Compiler   => Cmd_Line := Editor.Ada_Switches_Entry;
         when C_Compiler     => Cmd_Line := Editor.C_Switches_Entry;
         when Cpp_Compiler   => Cmd_Line := Editor.Cpp_Switches_Entry;
         when Pretty_Printer => Cmd_Line := Editor.Pp_Switches_Entry;
         when Binder         => Cmd_Line := Editor.Binder_Switches_Entry;
         when Linker         => Cmd_Line := Editor.Linker_Switches_Entry;
      end case;

      declare
         Str : constant String := Get_Text (Cmd_Line);
      begin
         if Str /= "" then
            List := Argument_String_To_List (Str);

            declare
               Ret : constant Argument_List :=
                 Normalize_Compiler_Switches (Tool, List.all);
            begin
               Unchecked_Free (List);
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
        (Button   : Gtk_Check_Button;
         Str      : String;
         Arr      : in out Argument_List;
         Index    : in out Natural;
         Inverted : Boolean := False);
      --  Handle check buttons, and set parameter Str if Button is checked,
      --  or if button is unchecked, in case Inverted is True.

      procedure Check_Combo
        (Combo          : Gtk_Combo;
         Switch         : String;
         Switch_Details : String;
         Arr            : in out Argument_List;
         Index          : in out Natural);
      --  Set the parameter (starting with Switch, followed by
      --  Switch_Details (combo index)) to use if Switch is set.
      --  If the combo index is 0, nothing is inserted into Arr.

      ------------------
      -- Check_Toggle --
      ------------------

      procedure Check_Toggle
        (Button   : Gtk_Check_Button;
         Str      : String;
         Arr      : in out Argument_List;
         Index    : in out Natural;
         Inverted : Boolean := False)
      is
         Check : Boolean := Get_Active (Button);
      begin
         if Inverted then
            Check := not Check;
         end if;

         if Check then
            Arr (Index) := new String' (Str);
            Index := Index + 1;
         end if;
      end Check_Toggle;

      -----------------
      -- Check_Combo --
      -----------------

      procedure Check_Combo
        (Combo          : Gtk_Combo;
         Switch         : String;
         Switch_Details : String;
         Arr            : in out Argument_List;
         Index          : in out Natural)
      is
         use Widget_List;
         List     : constant Gtk_List := Get_List (Combo);
         Position : Integer;

      begin
         --  Check whether there is an actual selection. With gtk+2.0, the
         --  entry emits the "changed" signal more often, even in some cases
         --  where there is no actual selection in the list. However, the
         --  callback is called again later on.

         if Get_Selection (List) /= Null_List then
            Position := Integer (Child_Position
              (List, Get_Data (Get_Selection (List)))) + 1;

            if Position /= 1 then
               Arr (Index) := new String' (Switch & Switch_Details (Position));
               Index := Index + 1;
            end if;
         end if;
      end Check_Combo;

      Num_Switches : Natural;

   begin  --  Get_Switches_From_GUI
      case Tool is
         when Gnatmake       => Num_Switches :=  7 + 1;  --  +1 is for -jx
         when Ada_Compiler   => Num_Switches := 22;
         when C_Compiler     => Num_Switches := 11;
         when Cpp_Compiler   => Num_Switches := 14;
         when Pretty_Printer => Num_Switches := 13;
         when Binder         => Num_Switches :=  4;
         when Linker         => Num_Switches :=  3;
      end case;

      declare
         Arr    : Argument_List (1 .. Num_Switches);
         Index  : Natural := Arr'First;
         Active : Boolean;
         Value  : Integer;

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
               Check_Combo
                 (Editor.Ada_Optimization_Level, "-O", "0123", Arr, Index);
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
               Check_Combo
                 (Editor.C_Optimization_Level, "-O", "0123", Arr, Index);
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
               Check_Combo
                 (Editor.Cpp_Optimization_Level, "-O", "0123", Arr, Index);
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

            when Pretty_Printer =>
               Value := Integer (Get_Value_As_Int (Editor.Indent_Level));

               --  3 is the default value of this switch
               if Value /= 3 then
                  Arr (Index) := new String' ("-i" & Image (Value));
                  Index := Index + 1;
               end if;

               Value := Integer (Get_Value_As_Int (Editor.Max_Line_Length));

               --  79 is the default value of this switch
               if Value /= 79 then
                  Arr (Index) := new String' ("-M" & Image (Value));
                  Index := Index + 1;
               end if;

               Check_Combo (Editor.Keyword_Casing, "-k", "LU", Arr, Index);
               Check_Combo (Editor.Attribute_Casing, "-a", "MLU", Arr, Index);
               Check_Combo (Editor.References_Casing, "-r", "DM", Arr, Index);
               Check_Combo (Editor.Pragma_Casing, "-p", "MLU", Arr, Index);
               Check_Combo (Editor.Construct_Layout, "-l", "123", Arr, Index);
               Check_Combo (Editor.Comments_Layout, "-c", "12", Arr, Index);
               Check_Toggle (Editor.Gnat_Comment_Begin, "-c3", Arr, Index);
               Check_Toggle (Editor.Reformat_Comment, "-c4", Arr, Index);
               Check_Toggle (Editor.Align_Colons, "-A1", Arr, Index);
               Check_Toggle (Editor.Align_Assign_Decl, "-A2", Arr, Index);
               Check_Toggle (Editor.Align_Assign_Stmt, "-A3", Arr, Index);
               Check_Toggle (Editor.Align_Arrow, "-A4", Arr, Index);
               Check_Toggle
                 (Editor.Set_Labels, "-e", Arr, Index, Inverted => True);

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

      procedure Set_Combo
        (Combo          : Gtk_Combo;
         Switch         : String;
         Switch_Details : String);
      --  Check if a switch starts with Switch, and get the argument after it
      --  (set in the combo box)

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

      procedure Set_Combo
        (Combo          : Gtk_Combo;
         Switch         : String;
         Switch_Details : String)
      is
         Index : Gint := 0;
         Char  : Character;

      begin
         for J in Switches'Range loop
            if Switches (J) /= null
              and then Switches (J)'Length >= Switch'Length
              and then Switches (J) (Switches (J)'First
                                     .. Switches (J)'First + Switch'Length - 1)
              = Switch
            then
               Index := 0;

               if Switches (J)'Length > Switch'Length then
                  Char := Switches (J) (Switches (J)'First + Switch'Length);

                  for K in Switch_Details'Range loop
                     if Switch_Details (K) = Char then
                        Index := Gint (K - Switch_Details'First);
                     end if;
                  end loop;
               end if;

               if Switch = "-O" and then Switches (J).all = "-O" then
                  Select_Item (Get_List (Combo), 1);
               else
                  Select_Item (Get_List (Combo), Index);
               end if;

               return;
            else
               Select_Item (Get_List (Combo), 0);
            end if;
         end loop;
      end Set_Combo;

      Cmd_Line : Gtk_Entry;
      Second   : Natural;

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
        (Tool /= Pretty_Printer
         or else (Editor.Pages and Pretty_Printer_Page) /= 0);
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
                             (Switches (J)'First + 2 .. Switches (J)'Last)));

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
            Set_Combo (Editor.Ada_Optimization_Level, "-O", "0123");
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
            Set_Combo (Editor.C_Optimization_Level, "-O", "0123");
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
            Set_Combo (Editor.Cpp_Optimization_Level, "-O", "0123");
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

         when Pretty_Printer =>
            --  Handle spin buttons first

            for J in Switches'Range loop
               if Switches (J) /= null then
                  Second := Switches (J)'First + 1;

                  if Switches (J)'Length = 3
                    and then Switches (J) (Second) = 'i'
                    and then Switches (J) (Second + 1) in '0' .. '9'
                  then
                     Set_Value
                       (Editor.Indent_Level,
                        Grange_Float'Value
                          (Switches (J) (Second + 1 .. Second + 1)));

                  elsif Switches (J)'Length > 2
                    and then Switches (J) (Second) = 'M'
                  then
                     begin
                        Set_Value
                          (Editor.Max_Line_Length,
                           Grange_Float'Value (Switches (J)
                             (Second + 1 .. Switches (J)'Last)));

                     exception
                        when Constraint_Error =>
                           Set_Value (Editor.Max_Line_Length, 79.0);
                     end;
                  end if;
               end if;
            end loop;

            Set_Combo (Editor.Keyword_Casing, "-k", "LU");
            Set_Combo (Editor.Attribute_Casing, "-a", "MLU");
            Set_Combo (Editor.References_Casing, "-r", "DM");
            Set_Combo (Editor.Pragma_Casing, "-p", "MLU");
            Set_Combo (Editor.Construct_Layout, "-l", "123");
            Set_Combo (Editor.Comments_Layout, "-c", "12");
            Set_Active (Editor.Gnat_Comment_Begin, Is_Set ("-c3"));
            Set_Active (Editor.Reformat_Comment, Is_Set ("-c4"));
            Set_Active (Editor.Align_Colons, Is_Set ("-A1"));
            Set_Active (Editor.Align_Assign_Decl, Is_Set ("-A2"));
            Set_Active (Editor.Align_Assign_Stmt, Is_Set ("-A3"));
            Set_Active (Editor.Align_Arrow, Is_Set ("-A4"));
            Set_Active (Editor.Set_Labels, not Is_Set ("-e"));

            Cmd_Line := Editor.Pp_Switches_Entry;

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

         when Pretty_Printer =>
            if (Editor.Pages and Pretty_Printer_Page) /= 0 then
               for J in Switches'Range loop
                  if Switches (J) /= null and then
                    ((Switches (J)'Length = 3
                      and then Switches (J) (Switches (J)'First ..
                                             Switches (J)'First + 1) = "-i")
                     or else (Switches (J)'Length > 2
                      and then Switches (J) (Switches (J)'First ..
                                             Switches (J)'First + 1) = "-M")
                     or else Switches (J).all = "-A1"
                     or else Switches (J).all = "-A2"
                     or else Switches (J).all = "-A3"
                     or else Switches (J).all = "-A4"
                     or else Switches (J).all = "-aL"
                     or else Switches (J).all = "-aU"
                     or else Switches (J).all = "-aM"
                     or else Switches (J).all = "-c1"
                     or else Switches (J).all = "-c2"
                     or else Switches (J).all = "-c3"
                     or else Switches (J).all = "-c4"
                     or else Switches (J).all = "-e"
                     or else Switches (J).all = "-kL"
                     or else Switches (J).all = "-kU"
                     or else Switches (J).all = "-l1"
                     or else Switches (J).all = "-l2"
                     or else Switches (J).all = "-l3"
                     or else Switches (J).all = "-pL"
                     or else Switches (J).all = "-pU"
                     or else Switches (J).all = "-pM"
                     or else Switches (J).all = "-rD"
                     or else Switches (J).all = "-rM"
                     or else Switches (J).all = "-e")
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
         when Gnatmake       => Cmd_Line := Editor.Make_Switches_Entry;
         when Ada_Compiler   => Cmd_Line := Editor.Ada_Switches_Entry;
         when C_Compiler     => Cmd_Line := Editor.C_Switches_Entry;
         when Cpp_Compiler   => Cmd_Line := Editor.Cpp_Switches_Entry;
         when Pretty_Printer => Cmd_Line := Editor.Pp_Switches_Entry;
         when Binder         => Cmd_Line := Editor.Binder_Switches_Entry;
         when Linker         => Cmd_Line := Editor.Linker_Switches_Entry;
      end case;

      declare
         Arr     : Argument_List := Get_Switches_From_GUI (Editor, Tool);
         Current : Argument_List := Get_Switches (Editor, Tool);

      begin
         Editor.Block_Refresh := True;
         Set_Text (Cmd_Line, "");

         for J in Arr'Range loop
            Append_Text (Cmd_Line, Arr (J).all & " ");
         end loop;

         --  Keep the switches set manually by the user

         Filter_Switches (Editor, Tool, Current);

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
     (Editor : access Switches_Edit_Record; Tool : Tool_Names) is
   begin
      if Editor.Block_Refresh then
         return;
      end if;

      declare
         Arg : Argument_List := Get_Switches (Editor, Tool);
      begin
         Editor.Block_Refresh := True;
         Set_Switches (Editor, Tool, Arg);
         Free (Arg);
         Editor.Block_Refresh := False;
      end;
   end Update_Gui_From_Cmdline;

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
     (Switches : access Gtk_Widget_Record'Class) is
   begin
      Fill_Editor
        (Switches_Edit (Switches),
         Switches_Edit (Switches).Project_View,
         (1 .. 0 => null));
   end Revert_To_Default;

   ----------------------
   -- Generate_Project --
   ----------------------

   function Generate_Project
     (Switches     : access Switches_Edit_Record'Class;
      Kernel       : access Kernel_Handle_Record'Class;
      Project      : Prj.Tree.Project_Node_Id;
      Project_View : Prj.Project_Id;
      Files        : Argument_List) return Project_Node_Array
   is
      Max_Modified_Packages : constant := 5;
      --  Maximum number of packages that can be renamed (Builder, Compiler,
      --  Binder, Linker, IDE).

      Result                : Project_Node_Array (1 .. Max_Modified_Packages);
      Current               : Natural := Result'First;

      procedure Change_Switches
        (Tool      : Tool_Names;
         Pkg_Name  : String;
         Language  : Name_Id;
         File_Name : String);
      --  Changes the switches for a specific package and tool.

      procedure Process_File (File_Name : String);
      --  Generate the switches for a specific file (or the default switches if
      --  File_Name is the empty string). Return True if the project was
      --  changed.

      ---------------------
      -- Change_Switches --
      ---------------------

      procedure Change_Switches
        (Tool : Tool_Names;
         Pkg_Name : String;
         Language : Name_Id;
         File_Name : String)
      is
         Args     : Argument_List := Get_Switches (Switches, Tool);
         Value    : Variable_Value;
         Is_Default_Value : Boolean;
         Rename_Prj : Project_Node_Id;
      begin
         Rename_Prj := Find_Project_Of_Package (Project, Pkg_Name);

         if not Has_Been_Normalized (Rename_Prj) then
            Normalize (Rename_Prj, Recurse => False);
         end if;

         if Project_View = No_Project then
            Is_Default_Value := False;

         else
            Get_Switches
              (Project          => Project_View,
               In_Pkg           => Pkg_Name,
               File             => File_Name,
               Language         => Language,
               Value            => Value,
               Is_Default_Value => Is_Default_Value);

            --  Check if we in fact have the initial value
            declare
               Default_Args : Argument_List :=
                 Normalize_Compiler_Switches (Tool, To_Argument_List (Value));
            begin
               Is_Default_Value := Is_Equal (Default_Args, Args);
               Free (Default_Args);
            end;
         end if;

         if not Is_Default_Value then
            if File_Name /= "" then
               if Args'Length /= 0 then
                  Update_Attribute_Value_In_Scenario
                    (Project            => Project,
                     Pkg_Name           => Pkg_Name,
                     Scenario_Variables =>
                       Scenario_Variables (Kernel),
                     Attribute_Name     => Get_Name_String (Name_Switches),
                     Values             => Args,
                     Attribute_Index    => File_Name,
                     Prepend            => False);
               else
                  Delete_Attribute
                    (Project            => Project,
                     Pkg_Name           => Pkg_Name,
                     Scenario_Variables =>
                       Scenario_Variables (Kernel),
                     Attribute_Name     => Get_Name_String (Name_Switches),
                     Attribute_Index    => File_Name);
               end if;

            elsif Args'Length /= 0 then
               Update_Attribute_Value_In_Scenario
                 (Project           => Project,
                  Pkg_Name          => Pkg_Name,
                  Scenario_Variables =>
                    Scenario_Variables (Kernel),
                  Attribute_Name    => Get_Name_String (Name_Default_Switches),
                  Values            => Args,
                  Attribute_Index   => Get_Name_String (Language),
                  Prepend           => False);

            else
               Delete_Attribute
                 (Project           => Project,
                  Pkg_Name          => Pkg_Name,
                  Scenario_Variables =>
                    Scenario_Variables (Kernel),
                  Attribute_Name    => Get_Name_String (Name_Default_Switches),
                  Attribute_Index   => Get_Name_String (Language));
            end if;
         end if;
         Free (Args);

         if not Is_Default_Value then
            Is_Default_Value := False;
            for C in Result'First .. Current - 1 loop
               if Result (C) = Rename_Prj then
                  Is_Default_Value := True;
               end if;
            end loop;

            if not Is_Default_Value then
               Result (Current) := Rename_Prj;
               Current := Current + 1;
            end if;
         end if;
      end Change_Switches;

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (File_Name : String) is
      begin
         if (Get_Pages (Switches) and Gnatmake_Page) /= 0 then
            --  ??? Currently, we only edit the default switches for Ada
            Change_Switches
              (Gnatmake, "builder", Snames.Name_Ada, File_Name);
         end if;

         if (Get_Pages (Switches) and Ada_Page) /= 0 then
            Change_Switches
              (Ada_Compiler, "compiler", Snames.Name_Ada, File_Name);
         end if;

         if (Get_Pages (Switches) and C_Page) /= 0 then
            Change_Switches (C_Compiler, "compiler", Snames.Name_C, File_Name);
         end if;

         if (Get_Pages (Switches) and Cpp_Page) /= 0 then
            Change_Switches
              (Cpp_Compiler, "compiler", Name_C_Plus_Plus, File_Name);
         end if;

         if (Get_Pages (Switches) and Pretty_Printer_Page) /= 0 then
            --  ??? Currently, we only edit the default switches for Ada
            Change_Switches
              (Pretty_Printer, "pretty_printer", Snames.Name_Ada, File_Name);
         end if;

         if (Get_Pages (Switches) and Binder_Page) /= 0 then
            --  ??? Currently, we only edit the default switches for Ada
            Change_Switches (Binder, "binder", Snames.Name_Ada, File_Name);
         end if;

         if (Get_Pages (Switches) and Linker_Page) /= 0 then
            --  ??? Currently, we only edit the default switches for Ada
            Change_Switches (Linker, "linker", Snames.Name_Ada, File_Name);
         end if;
      end Process_File;

   begin
      pragma Assert (Project /= Empty_Node);

      if Files'Length = 0 then
         Process_File ("");
      else
         for F in Files'Range loop
            Process_File (Files (F).all);
         end loop;
      end if;
      return Result (Result'First .. Current - 1);
   end Generate_Project;

   -------------------------
   -- Close_Switch_Editor --
   -------------------------

   procedure Close_Switch_Editor
     (Switches     : access Switches_Edit_Record'Class;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Files        : Argument_List)
   is
      Result : constant Project_Node_Array := Generate_Project
        (Switches     => Switches,
         Kernel       => Switches.Kernel,
         Project      => Project,
         Project_View => Project_View,
         Files        => Files);
   begin
      for R in Result'Range loop
         Set_Project_Modified (Switches.Kernel, Result (R), True);
      end loop;

      if Result'Length /= 0 then
         Recompute_View (Switches.Kernel);
      end if;
   end Close_Switch_Editor;

   ------------------
   -- Set_Switches --
   ------------------

   procedure Set_Switches
     (Editor : access Switches_Edit_Record; Project_View : Prj.Project_Id) is
   begin
      Fill_Editor (Editor, Project_View, Files => (1 .. 0 => null));
   end Set_Switches;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Prj.Project_Id;
      Files     : Argument_List)
   is
      function Get_Switches (Pkg_Name : String; Language : Name_Id)
         return Argument_List;
      --  Return the list of switches for Files, found in the package Pkg_Name,
      --  for a specific language. The returned array must be freed by the
      --  caller.

      ------------------
      -- Get_Switches --
      ------------------

      function Get_Switches (Pkg_Name : String; Language : Name_Id)
         return Argument_List
      is
         Value      : Variable_Value;
         Is_Default : Boolean;
      begin
         if Files'Length = 0 then
            Get_Switches (Project, Pkg_Name, "",
                          Language, Value, Is_Default);
         else
            --  ??? Should we merge all the switches ?
            Get_Switches (Project, Pkg_Name, Files (Files'First).all,
                          Language, Value, Is_Default);
         end if;
         return To_Argument_List (Value);
      end Get_Switches;

      Pages      : Page_Filter;
   begin
      --  ??? Would be nice to handle the language in a more generic and
      --  flexible way.

      Switches.Project_View := Project;

      if Files'Length = 0 then
         declare
            Langs : Argument_List := Get_Languages (Project);
         begin
            Set_Visible_Pages (Switches, Langs);
            Free (Langs);
         end;

      else
         Pages := 0;

         for F in Files'Range loop
            declare
               Lang : String := Get_Language_From_File
                 (Get_Language_Handler (Switches.Kernel), Files (F).all);
            begin
               Lower_Case (Lang);

               if Lang = "ada" then
                  Pages := Pages or Ada_Page or Pretty_Printer_Page;
               elsif Lang = "c" then
                  Pages := Pages or C_Page;
               elsif Lang = "c++" then
                  Pages := Pages or Cpp_Page;
               end if;
            end;
         end loop;

         Set_Visible_Pages (Switches, Pages);
      end if;

      --  Set the switches for all the pages
      if (Get_Pages (Switches) and Gnatmake_Page) /= 0 then
         declare
            List : Argument_List := Get_Switches ("builder", Snames.Name_Ada);
         begin
            Set_Switches (Switches, Gnatmake, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and Ada_Page) /= 0 then
         declare
            List : Argument_List := Get_Switches ("compiler", Snames.Name_Ada);
            L2   : Argument_List := Normalize_Compiler_Switches
              (Ada_Compiler, List);
         begin
            Set_Switches (Switches, Ada_Compiler, L2);
            Free (L2);
         end;
      end if;

      if (Get_Pages (Switches) and C_Page) /= 0 then
         declare
            List : Argument_List := Get_Switches ("compiler", Snames.Name_C);
         begin
            Set_Switches (Switches, C_Compiler, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and Cpp_Page) /= 0 then
         declare
            List : Argument_List := Get_Switches
              ("compiler", Name_C_Plus_Plus);
         begin
            Set_Switches (Switches, Cpp_Compiler, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and Pretty_Printer_Page) /= 0 then
         declare
            List : Argument_List := Get_Switches
              ("pretty_printer", Snames.Name_Ada);
         begin
            Set_Switches (Switches, Pretty_Printer, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and Binder_Page) /= 0 then
         declare
            List : Argument_List := Get_Switches ("binder", Snames.Name_Ada);
         begin
            Set_Switches (Switches, Binder, List);
            Free (List);
         end;
      end if;

      if (Get_Pages (Switches) and Linker_Page) /= 0 then
         declare
            List : Argument_List := Get_Switches ("linker", Snames.Name_Ada);
         begin
            Set_Switches (Switches, Linker, List);
            Free (List);
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Fill_Editor;

   -------------------------------
   -- Edit_Switches_For_Context --
   -------------------------------

   procedure Edit_Switches_For_Context
     (Context       : Selection_Context_Access;
      Force_Default : Boolean := False)
   is
      File      : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      File_Name : GNAT.OS_Lib.String_Access;
   begin
      pragma Assert (Has_Project_Information (File));

      if not Force_Default and then Has_File_Information (File) then
         File_Name := new String' (File_Information (File));
         Edit_Switches_For_Files
           (Get_Kernel (Context),
            Get_Project_From_View (Project_Information (File)),
            Project_Information (File),
            (1 => File_Name));
         Free (File_Name);
      else
         Edit_Switches_For_Files
           (Get_Kernel (Context),
            Get_Project_From_View (Project_Information (File)),
            Project_Information (File),
            (1 .. 0 => null));
      end if;
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

   -----------------------------
   -- Edit_Switches_For_Files --
   -----------------------------

   procedure Edit_Switches_For_Files
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project : Prj.Tree.Project_Node_Id;
      Project_View : Prj.Project_Id;
      Files : GNAT.OS_Lib.Argument_List)
   is
      Switches  : Switches_Edit;
      Dialog    : Gtk_Dialog;
      Button    : Gtk_Widget;
      B         : Gtk_Button;

   begin
      if Files'Length > 1 then
         Gtk_New (Dialog,
                  Title  => -"Editing switches for multiple files",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);

      elsif Files'Length = 1 then
         Gtk_New (Dialog,
                  Title  => -"Editing switches for specific file",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);

      else
         Gtk_New (Dialog,
                  Title  => (-"Editing default switches for project ")
                    & Project_Name (Project_View),
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);
      end if;

      Gtk_New (Switches);
      Switches.Kernel := Kernel_Handle (Kernel);
      Pack_Start (Get_Vbox (Dialog),
                  Get_Window (Switches), Fill => True, Expand => True);

      Fill_Editor (Switches, Project_View, Files);

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);

      if Files'Length /= 0 then
         Gtk_New_From_Stock (B, Stock_Revert_To_Saved);
         Pack_Start (Get_Action_Area (Dialog), B);
         Widget_Callback.Object_Connect
           (B, "clicked",
            Widget_Callback.To_Marshaller (Revert_To_Default'Access),
            Slot_Object => Switches);
      end if;

      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      --  Note: if the dialog is no longer modal, then we need to create a copy
      --  of the context for storing in the callback, since the current context
      --  will be automatically freed by the kernel at some point in the life
      --  of this dialog.

      if Run (Dialog) = Gtk_Response_OK then
         Close_Switch_Editor (Switches, Project, Project_View, Files);
      end if;

      Destroy (Dialog);
   end Edit_Switches_For_Files;

end Switches_Editors;
