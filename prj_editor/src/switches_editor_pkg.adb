with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Switches_Glade; use Callbacks_Switches_Glade;
with Switches_Glade_Intl; use Switches_Glade_Intl;

package body Switches_Editor_Pkg is

procedure Gtk_New (Switches_Editor : out Switches_Editor_Access) is
begin
   Switches_Editor := new Switches_Editor_Record;
   Switches_Editor_Pkg.Initialize (Switches_Editor);
end Gtk_New;

procedure Initialize (Switches_Editor : access Switches_Editor_Record'Class) is
   pragma Suppress (All_Checks);
   Num_Processes_Adj : Gtk_Adjustment;
   Spinbutton2_Adj : Gtk_Adjustment;

begin
   Gtk.Window.Initialize (Switches_Editor, Window_Toplevel);
   Set_Title (Switches_Editor, -"Switches");
   Set_Policy (Switches_Editor, False, True, False);
   Set_Position (Switches_Editor, Win_Pos_None);
   Set_Modal (Switches_Editor, True);
   Set_Default_Size (Switches_Editor, 400, 300);

   Gtk_New_Vbox (Switches_Editor.Vbox2, False, 0);
   Add (Switches_Editor, Switches_Editor.Vbox2);

   Gtk_New (Switches_Editor.Notebook1);
   Set_Scrollable (Switches_Editor.Notebook1, False);
   Set_Show_Border (Switches_Editor.Notebook1, True);
   Set_Show_Tabs (Switches_Editor.Notebook1, True);
   Set_Tab_Hborder (Switches_Editor.Notebook1, 2);
   Set_Tab_Vborder (Switches_Editor.Notebook1, 2);
   Set_Tab_Pos (Switches_Editor.Notebook1, Pos_Top);
   Pack_Start (Switches_Editor.Vbox2, Switches_Editor.Notebook1, True, True, 0);

   Gtk_New (Switches_Editor.Make_Switches, 2, 2, False);
   Set_Row_Spacings (Switches_Editor.Make_Switches, 0);
   Set_Col_Spacings (Switches_Editor.Make_Switches, 0);
   Add (Switches_Editor.Notebook1, Switches_Editor.Make_Switches);

   Gtk_New (Switches_Editor.Frame26, -"Code generation");
   Set_Border_Width (Switches_Editor.Frame26, 5);
   Set_Shadow_Type (Switches_Editor.Frame26, Shadow_Etched_In);
   Attach (Switches_Editor.Make_Switches, Switches_Editor.Frame26, 0, 1, 0, 1,
     Expand or Fill, Expand or Fill,
     0, 0);

   Gtk_New_Vbox (Switches_Editor.Vbox25, False, 0);
   Add (Switches_Editor.Frame26, Switches_Editor.Vbox25);

   Gtk_New (Switches_Editor.Make_All_Files, -"Consider all files");
   Set_Active (Switches_Editor.Make_All_Files, False);
   Pack_Start (Switches_Editor.Vbox25, Switches_Editor.Make_All_Files, False, False, 0);

   Gtk_New (Switches_Editor.Make_Debug, -"Debug Information");
   Set_Active (Switches_Editor.Make_Debug, False);
   Pack_Start (Switches_Editor.Vbox25, Switches_Editor.Make_Debug, False, False, 0);

   Gtk_New (Switches_Editor.Frame27, -"Run time errors");
   Set_Border_Width (Switches_Editor.Frame27, 5);
   Set_Shadow_Type (Switches_Editor.Frame27, Shadow_Etched_In);
   Attach (Switches_Editor.Make_Switches, Switches_Editor.Frame27, 1, 2, 0, 1,
     Expand or Fill, Fill,
     0, 0);

   Gtk_New_Vbox (Switches_Editor.Vbox26, False, 0);
   Add (Switches_Editor.Frame27, Switches_Editor.Vbox26);

   Gtk_New_Hbox (Switches_Editor.Hbox1, False, 0);
   Pack_Start (Switches_Editor.Vbox26, Switches_Editor.Hbox1, False, False, 0);

   Gtk_New (Switches_Editor.Make_Multiprocessing, -"Multiprocessing");
   Set_Active (Switches_Editor.Make_Multiprocessing, False);
   Pack_Start (Switches_Editor.Hbox1, Switches_Editor.Make_Multiprocessing, False, False, 0);

   Gtk_New (Num_Processes_Adj, 1.0, 0.0, 100.0, 1.0, 10.0, 10.0);
   Gtk_New (Switches_Editor.Num_Processes, Num_Processes_Adj, 1.0, 0);
   Set_Numeric (Switches_Editor.Num_Processes, False);
   Set_Snap_To_Ticks (Switches_Editor.Num_Processes, False);
   Set_Update_Policy (Switches_Editor.Num_Processes, Update_Always);
   Set_Value (Switches_Editor.Num_Processes, 1.0);
   Set_Wrap (Switches_Editor.Num_Processes, False);
   Pack_Start (Switches_Editor.Hbox1, Switches_Editor.Num_Processes, True, True, 0);

   Gtk_New (Switches_Editor.Make_Keep_Going, -"Keep going");
   Set_Active (Switches_Editor.Make_Keep_Going, False);
   Pack_Start (Switches_Editor.Vbox26, Switches_Editor.Make_Keep_Going, False, False, 0);

   Gtk_New (Switches_Editor.Make_Minimal_Recompile, -"Minimal recompilation");
   Set_Active (Switches_Editor.Make_Minimal_Recompile, False);
   Pack_Start (Switches_Editor.Vbox26, Switches_Editor.Make_Minimal_Recompile, False, False, 0);

   Gtk_New (Switches_Editor.Recompile_Switches, -"Recompile if switches changed");
   Set_Active (Switches_Editor.Recompile_Switches, False);
   Pack_Start (Switches_Editor.Vbox26, Switches_Editor.Recompile_Switches, False, False, 0);

   Gtk_New (Switches_Editor.Frame28, -"Warnings");
   Set_Border_Width (Switches_Editor.Frame28, 5);
   Set_Shadow_Type (Switches_Editor.Frame28, Shadow_Etched_In);
   Attach (Switches_Editor.Make_Switches, Switches_Editor.Frame28, 0, 1, 1, 2,
     Fill, Expand or Fill,
     0, 0);

   Gtk_New_Vbox (Switches_Editor.Vbox27, False, 0);
   Add (Switches_Editor.Frame28, Switches_Editor.Vbox27);

   Gtk_New (Switches_Editor.Make_No_Default, -"No default");
   Set_Active (Switches_Editor.Make_No_Default, False);
   Pack_Start (Switches_Editor.Vbox27, Switches_Editor.Make_No_Default, False, False, 0);

   Gtk_New (Switches_Editor.Make_Elab_Warning, -"Elab warning");
   Set_Active (Switches_Editor.Make_Elab_Warning, False);
   Pack_Start (Switches_Editor.Vbox27, Switches_Editor.Make_Elab_Warning, False, False, 0);

   Gtk_New (Switches_Editor.Make_Unused_Warning, -"Unused vars warning");
   Set_Active (Switches_Editor.Make_Unused_Warning, False);
   Pack_Start (Switches_Editor.Vbox27, Switches_Editor.Make_Unused_Warning, False, False, 0);

   Gtk_New (Switches_Editor.Make_Style_Checks, -"Style checks");
   Set_Active (Switches_Editor.Make_Style_Checks, False);
   Pack_Start (Switches_Editor.Vbox27, Switches_Editor.Make_Style_Checks, False, False, 0);

   Gtk_New (Switches_Editor.Label17, -("Make"));
   Set_Alignment (Switches_Editor.Label17, 0.5, 0.5);
   Set_Padding (Switches_Editor.Label17, 0, 0);
   Set_Justify (Switches_Editor.Label17, Justify_Center);
   Set_Line_Wrap (Switches_Editor.Label17, False);
   Set_Tab (Switches_Editor.Notebook1, 0, Switches_Editor.Label17);

   Gtk_New (Switches_Editor.Compiler_Switches, 2, 2, False);
   Set_Row_Spacings (Switches_Editor.Compiler_Switches, 0);
   Set_Col_Spacings (Switches_Editor.Compiler_Switches, 0);
   Add (Switches_Editor.Notebook1, Switches_Editor.Compiler_Switches);

   Gtk_New (Switches_Editor.Frame21, -"Code generation");
   Set_Border_Width (Switches_Editor.Frame21, 5);
   Set_Shadow_Type (Switches_Editor.Frame21, Shadow_Etched_In);
   Attach (Switches_Editor.Compiler_Switches, Switches_Editor.Frame21, 0, 1, 0, 1,
     Expand or Fill, Expand or Fill,
     0, 0);

   Gtk_New_Vbox (Switches_Editor.Vbox19, False, 0);
   Add (Switches_Editor.Frame21, Switches_Editor.Vbox19);

   Gtk_New_Hbox (Switches_Editor.Hbox2, False, 0);
   Pack_Start (Switches_Editor.Vbox19, Switches_Editor.Hbox2, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Optimize, -"Optimization");
   Set_Active (Switches_Editor.Compile_Optimize, False);
   Pack_Start (Switches_Editor.Hbox2, Switches_Editor.Compile_Optimize, False, False, 0);

   Gtk_New (Spinbutton2_Adj, 1.0, 0.0, 100.0, 1.0, 10.0, 10.0);
   Gtk_New (Switches_Editor.Spinbutton2, Spinbutton2_Adj, 1.0, 0);
   Set_Numeric (Switches_Editor.Spinbutton2, False);
   Set_Snap_To_Ticks (Switches_Editor.Spinbutton2, False);
   Set_Update_Policy (Switches_Editor.Spinbutton2, Update_Always);
   Set_Value (Switches_Editor.Spinbutton2, 1.0);
   Set_Wrap (Switches_Editor.Spinbutton2, False);
   Pack_Start (Switches_Editor.Hbox2, Switches_Editor.Spinbutton2, True, True, 0);

   Gtk_New (Switches_Editor.Compile_No_Inline, -"No Inlining");
   Set_Active (Switches_Editor.Compile_No_Inline, False);
   Pack_Start (Switches_Editor.Vbox19, Switches_Editor.Compile_No_Inline, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Interunit_Inlining, -"Interunit inlining");
   Set_Active (Switches_Editor.Compile_Interunit_Inlining, False);
   Pack_Start (Switches_Editor.Vbox19, Switches_Editor.Compile_Interunit_Inlining, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Polling, -"polling");
   Set_Active (Switches_Editor.Compile_Polling, False);
   Pack_Start (Switches_Editor.Vbox19, Switches_Editor.Compile_Polling, False, False, 0);

   Gtk_New (Switches_Editor.Frame22, -"Run time errors");
   Set_Border_Width (Switches_Editor.Frame22, 5);
   Set_Shadow_Type (Switches_Editor.Frame22, Shadow_Etched_In);
   Attach (Switches_Editor.Compiler_Switches, Switches_Editor.Frame22, 1, 2, 0, 1,
     Expand or Fill, Fill,
     0, 0);

   Gtk_New_Vbox (Switches_Editor.Vbox20, False, 0);
   Add (Switches_Editor.Frame22, Switches_Editor.Vbox20);

   Gtk_New (Switches_Editor.Compile_Overflow_Checking, -"Overflow checking");
   Set_Active (Switches_Editor.Compile_Overflow_Checking, False);
   Pack_Start (Switches_Editor.Vbox20, Switches_Editor.Compile_Overflow_Checking, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Suppress_All_Checks, -"Suppress all checks");
   Set_Active (Switches_Editor.Compile_Suppress_All_Checks, False);
   Pack_Start (Switches_Editor.Vbox20, Switches_Editor.Compile_Suppress_All_Checks, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Stack_Checking, -"Stack Checking");
   Set_Active (Switches_Editor.Compile_Stack_Checking, False);
   Pack_Start (Switches_Editor.Vbox20, Switches_Editor.Compile_Stack_Checking, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Dynamic_Elaboration, -"Dynamic elaboration");
   Set_Active (Switches_Editor.Compile_Dynamic_Elaboration, False);
   Pack_Start (Switches_Editor.Vbox20, Switches_Editor.Compile_Dynamic_Elaboration, False, False, 0);

   Gtk_New (Switches_Editor.Frame23, -"Messages");
   Set_Border_Width (Switches_Editor.Frame23, 5);
   Set_Shadow_Type (Switches_Editor.Frame23, Shadow_Etched_In);
   Attach (Switches_Editor.Compiler_Switches, Switches_Editor.Frame23, 0, 1, 1, 2,
     Fill, Expand or Fill,
     0, 0);

   Gtk_New_Vbox (Switches_Editor.Vbox21, False, 0);
   Add (Switches_Editor.Frame23, Switches_Editor.Vbox21);

   Gtk_New (Switches_Editor.Compile_Full_Errors, -"Full errors");
   Set_Active (Switches_Editor.Compile_Full_Errors, False);
   Pack_Start (Switches_Editor.Vbox21, Switches_Editor.Compile_Full_Errors, False, False, 0);

   Gtk_New (Switches_Editor.Compile_No_Warnings, -"No warning");
   Set_Active (Switches_Editor.Compile_No_Warnings, False);
   Pack_Start (Switches_Editor.Vbox21, Switches_Editor.Compile_No_Warnings, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Warning_Error, -"Warning=Error");
   Set_Active (Switches_Editor.Compile_Warning_Error, False);
   Pack_Start (Switches_Editor.Vbox21, Switches_Editor.Compile_Warning_Error, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Elab_Warning, -"Elab warning");
   Set_Active (Switches_Editor.Compile_Elab_Warning, False);
   Pack_Start (Switches_Editor.Vbox21, Switches_Editor.Compile_Elab_Warning, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Unused_Warning, -"Unused vars warning");
   Set_Active (Switches_Editor.Compile_Unused_Warning, False);
   Pack_Start (Switches_Editor.Vbox21, Switches_Editor.Compile_Unused_Warning, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Style_Checks, -"Style checks");
   Set_Active (Switches_Editor.Compile_Style_Checks, False);
   Pack_Start (Switches_Editor.Vbox21, Switches_Editor.Compile_Style_Checks, False, False, 0);

   Gtk_New_Vbox (Switches_Editor.Vbox22, False, 0);
   Attach (Switches_Editor.Compiler_Switches, Switches_Editor.Vbox22, 1, 2, 1, 2,
     Fill, Fill,
     0, 0);

   Gtk_New (Switches_Editor.Frame24, -"Debugging");
   Set_Border_Width (Switches_Editor.Frame24, 5);
   Set_Shadow_Type (Switches_Editor.Frame24, Shadow_Etched_In);
   Pack_Start (Switches_Editor.Vbox22, Switches_Editor.Frame24, True, True, 0);

   Gtk_New_Vbox (Switches_Editor.Vbox23, False, 0);
   Add (Switches_Editor.Frame24, Switches_Editor.Vbox23);

   Gtk_New (Switches_Editor.Compile_Debug_Information, -"Debug information");
   Set_Active (Switches_Editor.Compile_Debug_Information, False);
   Pack_Start (Switches_Editor.Vbox23, Switches_Editor.Compile_Debug_Information, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Assertions, -"Assertions");
   Set_Active (Switches_Editor.Compile_Assertions, False);
   Pack_Start (Switches_Editor.Vbox23, Switches_Editor.Compile_Assertions, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Representation_Info, -"Representation info");
   Set_Active (Switches_Editor.Compile_Representation_Info, False);
   Pack_Start (Switches_Editor.Vbox23, Switches_Editor.Compile_Representation_Info, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Debug_Expanded_Code, -"Debug expanded code");
   Set_Active (Switches_Editor.Compile_Debug_Expanded_Code, False);
   Pack_Start (Switches_Editor.Vbox23, Switches_Editor.Compile_Debug_Expanded_Code, False, False, 0);

   Gtk_New (Switches_Editor.Frame25, -"Syntax");
   Set_Border_Width (Switches_Editor.Frame25, 5);
   Set_Shadow_Type (Switches_Editor.Frame25, Shadow_Etched_In);
   Pack_Start (Switches_Editor.Vbox22, Switches_Editor.Frame25, True, True, 0);

   Gtk_New_Vbox (Switches_Editor.Vbox24, False, 0);
   Add (Switches_Editor.Frame25, Switches_Editor.Vbox24);

   Gtk_New (Switches_Editor.Compile_Language_Extensions, -"Language extensions");
   Set_Active (Switches_Editor.Compile_Language_Extensions, False);
   Pack_Start (Switches_Editor.Vbox24, Switches_Editor.Compile_Language_Extensions, False, False, 0);

   Gtk_New (Switches_Editor.Compile_Ada83_Mode, -"Ada 83 mode");
   Set_Active (Switches_Editor.Compile_Ada83_Mode, False);
   Pack_Start (Switches_Editor.Vbox24, Switches_Editor.Compile_Ada83_Mode, False, False, 0);

   Gtk_New (Switches_Editor.Label18, -("Compiler"));
   Set_Alignment (Switches_Editor.Label18, 0.5, 0.5);
   Set_Padding (Switches_Editor.Label18, 0, 0);
   Set_Justify (Switches_Editor.Label18, Justify_Center);
   Set_Line_Wrap (Switches_Editor.Label18, False);
   Set_Tab (Switches_Editor.Notebook1, 1, Switches_Editor.Label18);

   Gtk_New (Switches_Editor.Switches_String);
   Set_Editable (Switches_Editor.Switches_String, True);
   Set_Max_Length (Switches_Editor.Switches_String, 0);
   Set_Text (Switches_Editor.Switches_String, -"");
   Set_Visibility (Switches_Editor.Switches_String, True);
   Pack_Start (Switches_Editor.Vbox2, Switches_Editor.Switches_String, False, False, 0);

   Gtk_New (Switches_Editor.Hbuttonbox1);
   Set_Spacing (Switches_Editor.Hbuttonbox1, 30);
   Set_Layout (Switches_Editor.Hbuttonbox1, Buttonbox_Spread);
   Set_Child_Size (Switches_Editor.Hbuttonbox1, 85, 27);
   Set_Child_Ipadding (Switches_Editor.Hbuttonbox1, 7, 0);
   Pack_Start (Switches_Editor.Vbox2, Switches_Editor.Hbuttonbox1, False, False, 0);

end Initialize;

end Switches_Editor_Pkg;
