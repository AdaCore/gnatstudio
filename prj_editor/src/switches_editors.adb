with Glib;              use Glib;
with Gtk.Box;           use Gtk.Box;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Combo;         use Gtk.Combo;
with Gtk.List;          use Gtk.List;
with Gtk.Radio_Button;  use Gtk.Radio_Button;
with Gtk.Spin_Button;   use Gtk.Spin_Button;
with Gtk.Table;         use Gtk.Table;
with Gtk.Widget;        use Gtk.Widget;

with Switches_Editor_Pkg; use Switches_Editor_Pkg;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with Unchecked_Deallocation;

package body Switches_Editors is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Switches_Edit) is
   begin
      Editor := new Switches_Edit_Record;
      Initialize (Editor);
      Ref (Editor.Vbox2);
      Unparent (Editor.Vbox2);
   end Gtk_New;

   -------------------
   -- Destroy_Pages --
   -------------------

   procedure Destroy_Pages
     (Editor : access Switches_Edit_Record; Pages : Page_Filter) is
   begin
      Editor.Pages := Pages;

      if (Pages and Gnatmake_Page) = 0 then
         Destroy (Editor.Make_Switches);
         Editor.Make_Switches := null;
      end if;

      if (Pages and Compiler_Page) = 0 then
         Destroy (Editor.Compiler_Switches);
         Editor.Compiler_Switches := null;
      end if;

      if (Pages and Binder_Page) = 0 then
         Destroy (Editor.Binder_Switches);
         Editor.Binder_Switches := null;
      end if;

      if (Pages and Linker_Page) = 0 then
         Destroy (Editor.Linker_Switches);
         Editor.Linker_Switches := null;
      end if;
   end Destroy_Pages;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Editor : access Switches_Edit_Record) return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Editor.Vbox2);
   end Get_Window;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Editor : access Switches_Edit_Record; Tool : Tool_Names)
      return Argument_List
   is
      procedure Check_Toggle
        (Button : Gtk_Check_Button;
         Str : String;
         Arr : in out Argument_List;
         Index : in out Natural);

      procedure Check_Combo
        (Combo : Gtk_Combo;
         Switch : String;
         Arr : in out Argument_List;
         Index : in out Natural);
      --  Set the parameter (starting with Switch, followed by a numeric
      --  argument) to use if Switch is set.
      --  If the numeric argument is 0, nothing is inserted into Arr.

      ------------------
      -- Check_Toggle --
      ------------------

      procedure Check_Toggle
        (Button : Gtk_Check_Button;
         Str : String;
         Arr : in out Argument_List;
         Index : in out Natural) is
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
        (Combo : Gtk_Combo;
         Switch : String;
         Arr : in out Argument_List;
         Index : in out Natural)
      is
         use Widget_List;
         List  : Gtk_List := Get_List (Combo);
         Value : Gint := Child_Position
           (List, Get_Data (Get_Selection (List)));
         Level : constant String := Gint'Image (Value);
      begin
         if Value /= 0 then
            Arr (Index) := new String'
              (Switch & Level (Level'First + 1 .. Level'Last));
            Index := Index + 1;
         end if;
      end Check_Combo;

      Num_Switches : Natural;
   begin
      case Tool is
         when Gnatmake => Num_Switches := 6 + 1;  --  +1 is for arg to -j
         when Compiler => Num_Switches := 20 + 1; --  +1 is for -g
         when Binder   => Num_Switches := 3 + 1;  --  +1 is for -g
         when Linker   => Num_Switches := 0 + 1;  --  +1 is for -g
      end case;

      declare
         Arr   : Argument_List (1 .. Num_Switches);
         Index : Natural := Arr'First;
      begin
         case Tool is
            when Gnatmake =>
               Check_Toggle (Editor.Make_All_Files, "-a", Arr, Index);
               Check_Toggle (Editor.Make_Recompile_Switches, "-s", Arr, Index);
               Check_Toggle (Editor.Make_Minimal_Recompile, "-m", Arr, Index);
               Check_Toggle (Editor.Make_Keep_Going, "-k", Arr, Index);
               Check_Toggle (Editor.Make_Debug, "-g", Arr, Index);
               if Get_Active (Editor.Make_Multiprocessing) then
                  declare
                     Level : constant String := Gint'Image
                       (Get_Value_As_Int (Editor.Num_Processes));
                  begin
                     Arr (Index) := new String'
                       ("-j" & Level (Level'First + 1 .. Level'Last));
                     Index := Index + 1;
                  end;
               end if;

            when Compiler =>
               Check_Combo (Editor.Optimization_Level, "-O", Arr, Index);
               Check_Combo (Editor.Compile_Representation_Info,
                           "-gnatR", Arr, Index);
               Check_Toggle
                 (Editor.Compile_No_Inline, "-fno-inline", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Interunit_Inlining, "-gnatn", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Full_Errors, "-gnatf", Arr, Index);
               Check_Toggle
                 (Editor.Compile_No_Warnings, "-gnatws", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Warning_Error, "-gnatwe", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Elab_Warning, "-gnatwl", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Unused_Warning, "-gnawu", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Style_Checks, "-gnaty", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Overflow_Checking, "-gnato", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Suppress_All_Checks, "-gnatp", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Stack_Checking, "-fstack-check", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Dynamic_Elaboration, "-gnatE", Arr, Index);
               Check_Toggle (Editor.Compile_Assertions, "-gnata", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Debug_Expanded_Code, "-gnatdg", Arr, Index);
               Check_Toggle
                 (Editor.Compile_Language_Extensions, "-gnatX", Arr, Index);
               Check_Toggle (Editor.Compile_Ada83_Mode, "-gnat83", Arr, Index);

            when Binder =>
               Check_Toggle (Editor.Binder_Tracebacks, "-E", Arr, Index);
               Check_Toggle (Editor.Binder_Rm_Elaboration, "-f", Arr, Index);
               if Get_Active (Editor.Binder_Static_Gnat) then
                  Arr (Index) := new String' ("-static");
               else
                  Arr (Index) := new String' ("-shared");
               end if;
               Index := Index + 1;

            when Linker =>
               null;
         end case;

         return Arr (Arr'First .. Index - 1);
      end;
   end Get_Switches;

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
                  Level := Gint'Value
                    (Switches (J)(Switches (J)'First + Switch'Length
                                  .. Switches (J)'Last));
               else
                  Level := 0;
               end if;

               Select_Item (Get_List (Combo), Level);
               return;
            end if;
         end loop;
      end Set_Combo;

   begin
      pragma Assert
        (Tool /= Gnatmake or else (Editor.Pages and Gnatmake_Page) /= 0);
      pragma Assert
        (Tool /= Compiler or else (Editor.Pages and Compiler_Page) /= 0);
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
            Set_Active (Editor.Make_Multiprocessing, False);

            for J in Switches'Range loop
               if Switches (J) /= null
                 and then Switches (J)'Length > 1
                 and then Switches (J) (Switches (J)'First + 1) = 'j'
               then
                  Set_Active (Editor.Make_Multiprocessing, True);
                  if Switches (J)'Length > 2 then
                     Set_Value
                       (Editor.Num_Processes, Gfloat'Value (Switches (J)
                         (Switches (J)'First + 2 .. Switches  (J)'Last)));
                  else
                     Set_Value (Editor.Num_Processes, 0.0);
                  end if;
               end if;
            end loop;

         when Compiler =>
            Set_Combo (Editor.Optimization_Level, "-O");
            Set_Combo (Editor.Compile_Representation_Info, "-gnatR");
            Set_Active (Editor.Compile_No_Inline, Is_Set ("-fno-inline"));
            Set_Active (Editor.Compile_Interunit_Inlining, Is_Set ("-gnatn"));
            Set_Active (Editor.Compile_Full_Errors, Is_Set ("-gnatf"));
            Set_Active (Editor.Compile_No_Warnings, Is_Set ("-gnatws"));
            Set_Active (Editor.Compile_Warning_Error, Is_Set ("-gnatwe"));
            Set_Active (Editor.Compile_Elab_Warning, Is_Set ("-gnatwl"));
            Set_Active (Editor.Compile_Unused_Warning, Is_Set ("-gnawu"));
            Set_Active (Editor.Compile_Style_Checks, Is_Set ("-gnaty"));
            Set_Active (Editor.Compile_Overflow_Checking, Is_Set ("-gnato"));
            Set_Active (Editor.Compile_Suppress_All_Checks, Is_Set ("-gnatp"));
            Set_Active
              (Editor.Compile_Stack_Checking, Is_Set ("-fstack-check"));
            Set_Active (Editor.Compile_Dynamic_Elaboration, Is_Set ("-gnatE"));
            Set_Active (Editor.Compile_Assertions, Is_Set ("-gnata"));
            Set_Active
              (Editor.Compile_Debug_Expanded_Code, Is_Set ("-gnatdg"));
            Set_Active (Editor.Compile_Language_Extensions, Is_Set ("-gnatX"));
            Set_Active (Editor.Compile_Ada83_Mode, Is_Set ("-gnat83"));

         when Binder =>
            Set_Active (Editor.Binder_Tracebacks, Is_Set ("-E"));
            Set_Active (Editor.Binder_Rm_Elaboration, Is_Set ("-f"));
            Set_Active (Editor.Binder_Static_Gnat, Is_Set ("-static"));
            Set_Active (Editor.Binder_Shared_Gnat, Is_Set ("-shared"));

         when Linker =>
            null;
      end case;
   end Set_Switches;

   ---------------------
   -- Filter_Switches --
   ---------------------

   procedure Filter_Switches
     (Editor   : access Switches_Edit_Record;
      Tool     : Tool_Names;
      Switches : in out GNAT.OS_Lib.Argument_List) is
   begin
      case Tool is
         when Gnatmake =>
            for J in Switches'Range loop
               if Switches (J) /= null
                 and then
                 (Switches (J).all = "-a"
                  or else Switches (J).all = "-s"
                  or else Switches (J).all = "-m"
                  or else Switches (J).all = "-k"
                  or else Switches (J).all = "-g"
                  or else
                  (Switches (J)'Length >= 2
                   and then Switches (J)
                   (Switches (J)'First .. Switches (J)'First + 1) = "-j"))
               then
                  Free (Switches (J));
               end if;
            end loop;

         when Compiler =>
            for J in Switches'Range loop
               if Switches (J) /= null
                 and then
                 ((Switches (J)'Length >= 2
                   and then Switches (J)
                   (Switches (J)'First .. Switches (J)'First + 1) = "-O")
                  or else
                  (Switches (J)'Length >= 6
                   and then Switches (J)
                   (Switches (J)'First .. Switches (J)'First + 5) = "-gnatR")
                  or else Switches (J).all = "-fno-inline"
                  or else Switches (J).all = "-gnatn"
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
                  or else Switches (J).all = "-gnata"
                  or else Switches (J).all = "-gnatdg"
                  or else Switches (J).all = "-gnatX"
                  or else Switches (J).all = "-gnat83")
               then
                  Free (Switches (J));
               end if;
            end loop;

         when Binder =>
            for J in Switches'Range loop
               if Switches (J) /= null
                 and then
                 (Switches (J).all = "-E"
                  or else Switches (J).all = "-f"
                  or else Switches (J).all = "-static"
                  or else Switches (J).all = "-shared")
               then
                  Free (Switches (J));
               end if;
            end loop;

         when Linker =>
            null;
      end case;
   end Filter_Switches;

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
      procedure Internal is new Unchecked_Deallocation
        (Argument_List, Argument_List_Access);
   begin
      Free (Switches.all);
      Internal (Switches);
   end Free;

end Switches_Editors;
