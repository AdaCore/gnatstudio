with Glib;              use Glib;
with Gtk.Table;         use Gtk.Table;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Box;           use Gtk.Box;
with Gtk.Object;        use Gtk.Object;
with Gtk.Container;     use Gtk.Container;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Widget;        use Gtk.Widget;

with Switches_Editor_Pkg; use Switches_Editor_Pkg;

package body Switches_Editors is

   package SA is new Gtk.Object.User_Data (String_Access);
   Switches_Data : constant String := "_glide_switches";
   --  String used to identify the matching switch associated with a
   --  push_button.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Switches_Editor;
      Pages  : Page_Filter := All_Pages) is
   begin
      Editor := new Switches_Editor_Record;
      Initialize (Editor, Pages);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor : access Switches_Editor_Record'Class;
      Pages  : Page_Filter := All_Pages)
   is
      Id : constant Glib.GQuark := Quark_From_String (Switches_Data);
      Full : Switches_Editor_Access;
   begin
      Gtk.Frame.Initialize (Editor);
      Gtk_New (Full);

      Editor.Pages := Pages;
      Editor.Num_Pages := 0;

      if (Pages and Gnatmake_Page) = 0 then
         Destroy (Full.Make_Switches);
         Full.Make_Switches := null;
      else
         Editor.Num_Pages := Editor.Num_Pages + 1;
         SA.Set (Full.Make_All_Files, new String' ("-a"), Id);
--           SA.Set (Full.Make_Compile_Only, new String' ("-c"), Id);
--           SA.Set (Full.Make_Force_Recompile, new String' ("-f"), Id);
--           SA.Set (Full.Make_In_Place, new String' ("-i"), Id);
         SA.Set (Full.Make_No_Default, new String' ("-I-"), Id);
         SA.Set (Full.Make_Elab_Warning, new String' ("-gnatl"), Id);
         SA.Set (Full.Make_Unused_Warning, new String' ("-gnatu"), Id);
         SA.Set (Full.Make_Style_Checks, new String' ("-gnaty"), Id);
         SA.Set (Full.Make_Multiprocessing, new String' ("-j"), Id);
         SA.Set (Full.Make_Keep_Going, new String' ("-k"), Id);
         SA.Set (Full.Make_Minimal_Recompile, new String' ("-m"), Id);
         SA.Set (Full.Make_Debug, new String' ("-g"), Id);
         SA.Set (Full.Recompile_Switches, new String' ("-s"), Id);
      end if;

      if (Pages and Compiler_Page) = 0 then
         Destroy (Full.Compiler_Switches);
         Full.Compiler_Switches := null;
      else
         Editor.Num_Pages := Editor.Num_Pages + 1;
         SA.Set (Full.Compile_Optimize, new String' ("-O2"), Id);
         SA.Set (Full.Compile_No_Inline, new String' ("-fno-inline"), Id);
         SA.Set (Full.Compile_Interunit_Inlining, new String' ("-gnatn"), Id);
         SA.Set (Full.Compile_Polling, new String' ("-gnatP"), Id);
         SA.Set (Full.Compile_Full_Errors, new String' ("-gnatf"), Id);
         SA.Set (Full.Compile_No_Warnings, new String' ("-gnatws"), Id);
         SA.Set (Full.Compile_Warning_Error, new String' ("-gnatwe"), Id);
         SA.Set (Full.Compile_Elab_Warning, new String' ("-gnatl"), Id);
         SA.Set (Full.Compile_Unused_Warning, new String' ("-gnatwu"), Id);
         SA.Set (Full.Compile_Style_Checks, new String' ("-gnaty"), Id);
         SA.Set (Full.Compile_Overflow_Checking, new String' ("-gnato"), Id);
         SA.Set (Full.Compile_Suppress_All_Checks, new String' ("-gnatp"), Id);
         SA.Set
           (Full.Compile_Stack_Checking, new String' ("-fstack-check"), Id);
         SA.Set (Full.Compile_Dynamic_Elaboration, new String' ("-gnatE"), Id);
         SA.Set (Full.Compile_Debug_Information, new String' ("-g"), Id);
         SA.Set (Full.Compile_Assertions, new String' ("-gnata"), Id);
         SA.Set (Full.Compile_Representation_Info, new String' ("-gnatR"), Id);
         SA.Set
           (Full.Compile_Debug_Expanded_Code, new String' ("-gnatdg"), Id);
         SA.Set (Full.Compile_Language_Extensions, new String' ("-gnatX"), Id);
         SA.Set (Full.Compile_Ada83_Mode, new String' ("-gnat83"), Id);
      end if;

      Reparent (Full.Vbox2, Editor);
      Editor.Full := Full;
   end Initialize;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches (Editor : access Switches_Editor_Record)
      return Switches_Array
   is
      Id : constant Glib.GQuark := Quark_From_String (Switches_Data);

      function Count_Checks (C : access Gtk_Container_Record'Class)
         return Natural;
      --  Return the number of check buttons in C

      procedure Get_Switches_Recurse
        (C     : access Gtk_Container_Record'Class;
         Arr   : in out Switches_Array;
         Index : in out Natural);
      --  Set in Arr, starting at Index, the switches that are set in C

      ------------------
      -- Count_Checks --
      ------------------

      function Count_Checks (C : access Gtk_Container_Record'Class)
         return Natural
      is
         use type Widget_List.Glist;
         List : Widget_List.Glist := Children (C);
         W    : Gtk_Widget;
         Count : Natural := 0;
      begin
         while List /= Widget_List.Null_List loop
            W := Widget_List.Get_Data (List);
            if W.all in Gtk_Check_Button_Record'Class then
               Count := Count + 1;
            elsif W.all in Gtk_Container_Record'Class then
               Count := Count + Count_Checks (Gtk_Container (W));
            end if;
            List := Widget_List.Next (List);
         end loop;
         return Count;
      end Count_Checks;

      --------------------------
      -- Get_Switches_Recurse --
      --------------------------

      procedure Get_Switches_Recurse
        (C     : access Gtk_Container_Record'Class;
         Arr   : in out Switches_Array;
         Index : in out Natural)
      is
         use type Widget_List.Glist;
         List : Widget_List.Glist := Children (C);
         W    : Gtk_Widget;
      begin
         while List /= Widget_List.Null_List loop
            W := Widget_List.Get_Data (List);
            if W.all in Gtk_Check_Button_Record'Class then
               if Get_Active (Gtk_Check_Button (W)) then
                  Arr (Index) := SA.Get (W, Id);
                  Index := Index + 1;
               end if;
            elsif W.all in Gtk_Container_Record'Class then
               Get_Switches_Recurse (Gtk_Container (W), Arr, Index);
            end if;
            List := Widget_List.Next (List);
         end loop;
      end Get_Switches_Recurse;

      Num : Natural := 0;
   begin
      if Editor.Full.Make_Switches /= null then
         Num := Num + Count_Checks (Editor.Full.Make_Switches);
      end if;
      if Editor.Full.Compiler_Switches /= null then
         Num := Num + Count_Checks (Editor.Full.Compiler_Switches);
      end if;
      Num := Num + Editor.Num_Pages - 1;  --  space for -cargs, -bargs,...

      declare
         Arr   : Switches_Array (1 .. Num);
         Index : Natural := 1;
      begin
         if Editor.Full.Make_Switches /= null then
            Get_Switches_Recurse (Editor.Full.Make_Switches, Arr, Index);
         end if;
         if Editor.Full.Compiler_Switches /= null then
            if Editor.Num_Pages > 1 then
               Arr (Index) := new String' ("-cargs"); --  ??? not freed
               Index := Index + 1;
            end if;
            Get_Switches_Recurse (Editor.Full.Compiler_Switches, Arr, Index);
         end if;
         return Arr (1 .. Index - 1);
      end;
   end Get_Switches;

   ------------------
   -- Set_Switches --
   ------------------

   procedure Set_Switches
     (Editor   : access Switches_Editor_Record;
      Switches : Switches_Array)
   is
      Id : constant Glib.GQuark := Quark_From_String (Switches_Data);

      procedure Set_In_Toggles
        (C : access Gtk_Container_Record'Class; Switch : String);
      --  Find and set the toggle that matches Switch

      procedure Reset_Toggles (C : access Gtk_Container_Record'Class);
      --  Reset all the toggle buttons in C to a False value

      --------------------
      -- Set_In_Toggles --
      --------------------

      procedure Set_In_Toggles
        (C : access Gtk_Container_Record'Class; Switch : String)
      is
         use type Widget_List.Glist;
         List : Widget_List.Glist := Children (C);
         W    : Gtk_Widget;
      begin
         while List /= Widget_List.Null_List loop
            W := Widget_List.Get_Data (List);
            if W.all in Gtk_Check_Button_Record'Class then
               if SA.Get (W, Id).all = Switch then
                  Set_Active (Gtk_Check_Button (W), True);
                  return;
               end if;
            elsif W.all in Gtk_Container_Record'Class then
               Set_In_Toggles (Gtk_Container (W), Switch);
            end if;
            List := Widget_List.Next (List);
         end loop;
      end Set_In_Toggles;

      -------------------
      -- Reset_Toggles --
      -------------------

      procedure Reset_Toggles (C : access Gtk_Container_Record'Class) is
         use type Widget_List.Glist;
         List : Widget_List.Glist := Children (C);
         W    : Gtk_Widget;
      begin
         while List /= Widget_List.Null_List loop
            W := Widget_List.Get_Data (List);
            if W.all in Gtk_Check_Button_Record'Class then
               Set_Active (Gtk_Check_Button (W), False);
            elsif W.all in Gtk_Container_Record'Class then
               Reset_Toggles (Gtk_Container (W));
            end if;
            List := Widget_List.Next (List);
         end loop;
      end Reset_Toggles;

      Page : Page_Filter := Gnatmake_Page;
   begin
      if Editor.Full.Make_Switches /= null then
         Reset_Toggles (Editor.Full.Make_Switches);
      end if;

      if Editor.Full.Compiler_Switches /= null then
         Reset_Toggles (Editor.Full.Compiler_Switches);
      end if;

      --  If there is a single page, -cargs,... are optional
      if Editor.Num_Pages = 1 then
         Page := Editor.Pages;
      end if;

      for J in Switches'Range loop

         if Switches (J).all = "-cargs" then
            Page := Compiler_Page;
         elsif Switches (J).all = "-bargs" then
            Page := Binder_Page;
         elsif Switches (J).all = "-largs" then
            Page := Linker_Page;

         --  Gnatmake switches
         elsif (Page and Gnatmake_Page) /= 0
           and then Editor.Full.Make_Switches /= null
         then
            Set_In_Toggles (Editor.Full.Make_Switches, Switches (J).all);

         --  Compiler switches
         elsif (Page and Compiler_Page) /= 0
           and then Editor.Full.Compiler_Switches /= null
         then
            Set_In_Toggles (Editor.Full.Compiler_Switches, Switches (J).all);
         end if;
      end loop;
   end Set_Switches;

end Switches_Editors;
