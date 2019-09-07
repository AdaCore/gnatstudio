------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Containers;
with Ada.Unchecked_Conversion;

with GNATCOLL.Arg_Lists;             use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;              use GNATCOLL.Projects;
with GNATCOLL.Python;                use GNATCOLL.Python;
with GNATCOLL.Scripts;               use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Gtkada;        use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.Scripts.Python;        use GNATCOLL.Scripts.Python;
with GNATCOLL.Scripts.Python.Gtkada; use GNATCOLL.Scripts.Python.Gtkada;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.Xref;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Basic_Types;

with Glib.Object;                use Glib.Object;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.MDI;                 use Gtkada.MDI;

with Generic_Views;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Custom;          use GPS.Kernel.Custom;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Python_Core;
with GPS.Main_Window;            use GPS.Main_Window;
with Histories;                  use Histories;
with Interactive_Consoles;       use Interactive_Consoles;
with String_Utils;               use String_Utils;
with System;
with XML_Utils;                  use XML_Utils;
with Xref;                       use Xref;

package body Python_Module is
   use type GNATCOLL.Xref.Visible_Column;

   Me  : constant Trace_Handle := Create ("GPS.OTHERS.Python_Module");

   type Hash_Index is range 0 .. 100000;
   function Hash is new String_Utils.Hash (Hash_Index);

   type Python_Module_Record is new Module_ID_Record with null record;
   overriding procedure Destroy (Module : in out Python_Module_Record);

   procedure Load_Dir
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Dir                : Virtual_File;
      Default_Autoload   : Boolean;
      Ignore_User_Config : Boolean);
   --  Load all .py files from Dir, if any.
   --  Default_Autoload indicates whether scripts in this directory should
   --  be autoloaded by default, unless otherwise mentioned in
   --  ~/.gps/startup.xml
   --  Ignore_User_Config should be True for the support scripts that are not
   --  user-configurable plugins.

   type Python_Console_Record is new Interactive_Console_Record
     with null record;

   function Initialize
     (Console : access Python_Console_Record'Class) return Gtk_Widget;
   --  Initialize the python console, and returns the focus widget.

   package Python_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Python_Console",
      View_Name          => -"Python",
      Formal_View_Record => Python_Console_Record,
      Formal_MDI_Child   => GPS_Console_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Local_Toolbar      => False,
      Local_Config       => False,
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Consoles);

   procedure Python_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_GUI_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands related to the various classes

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Support functions for the MDI

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Console : access Python_Console_Record'Class) return Gtk_Widget
   is
      Backend : Virtual_Console;
      Script  : constant Scripting_Language :=
         Console.Kernel.Scripts.Lookup_Scripting_Language (Python_Name);
      Errors  : aliased Boolean;
      Result  : PyObject;

      Hist : constant History_Key := "python_console";
   begin
      Interactive_Consoles.Initialize
        (Console,
         Console.Kernel,
         Prompt => "", Handler => Default_Command_Handler'Access,
         User_Data       => System.Null_Address,
         History_List    => Get_History (Console.Kernel),
         Wrap_Mode       => Wrap_Char,
         Key             => Hist);
      Set_Font_And_Colors (Console.Get_View, Fixed_Font => True);
      Set_Max_Length   (Get_History (Console.Kernel).all, 100, Hist);
      Allow_Duplicates (Get_History (Console.Kernel).all, Hist, True, True);

      Backend := Get_Or_Create_Virtual_Console (Console);
      Set_Default_Console (Script, Backend);

      --  After creating the Python console, import everything from
      --  the plugin GPS_help, to override the default help function

      Console.Enable_Prompt_Display (False);
      Result := Run_Command
        (Python_Scripting (Script),
         "import GPS_help ; help = GPS_help.help",
         Need_Output     => False,
         Console         => Backend,
         Show_Command    => False,
         Hide_Output     => True,
         Hide_Exceptions => True,
         Errors          => Errors'Unchecked_Access);
      Py_XDECREF (Result);
      Console.Enable_Prompt_Display (True);
      Console.Display_Prompt;

      return Gtk_Widget (Console.Get_View);
   end Initialize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Ignored : Integer;
      Tmp     : Boolean;
      pragma Unreferenced (Ignored, Tmp);
      Script  : Scripting_Language;
      MDI     : Class_Type;

   begin
      GPS.Python_Core.Register_Python (Kernel);

      Script := Kernel.Scripts.Lookup_Scripting_Language (Python_Name);
      if Script = null then
         Trace (Me, "Python not supported");
         return;
      end if;

      Init_PyGtk_Support (Script);

      Set_Default_Console (Script, Kernel.Get_Messages_Window);

      Python_Views.Register_Module
        (Kernel, new Python_Module_Record);

      Register_Desktop_Functions (null, Load_Desktop'Access);

      MDI := Kernel.Scripts.New_Class ("MDI");

      Add_PyWidget_Method
        (Kernel.Scripts, Class => Get_GUI_Class (Kernel));
      Kernel.Scripts.Register_Command
        (Command       => "add",
         Handler       => Python_GUI_Command_Handler'Access,
         Class         => MDI,
         Params        =>
            (Param ("widget"),
             Param ("title", Optional => True),
             Param ("short", Optional => True),
             Param ("group", Optional => True),
             Param ("position", Optional => True),
             Param ("save_desktop", Optional => True),
             Param ("flags", Optional => True)),
         Static_Method => True,
         Language      => Python_Name);

      --  Change the screen representation of the various classes. This way,
      --  commands can return classes, but still displayed user-readable
      --  strings.
      --  Also make sure these can be used as keys in dictionaries.

      Kernel.Scripts.Register_Command
        (Command      => "__str__",
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__repr__",
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__hash__",
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__cmp__",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel),
         Language     => Python_Name);

      Kernel.Scripts.Register_Command
        (Command      => "__str__",
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel),
         Language      => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__repr__",
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__hash__",
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__cmp__",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel),
         Language     => Python_Name);

      Kernel.Scripts.Register_Command
        (Command      => "__str__",
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__repr__",
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__hash__",
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__cmp__",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel),
         Language     => Python_Name);

      Kernel.Scripts.Register_Command
        (Command      => "__str__",
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__repr__",
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__hash__",
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel),
         Language     => Python_Name);
      Kernel.Scripts.Register_Command
        (Command      => "__cmp__",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel),
         Language     => Python_Name);
   end Register_Module;

   --------------
   -- Load_Dir --
   --------------

   procedure Load_Dir
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Dir                : Virtual_File;
      Default_Autoload   : Boolean;
      Ignore_User_Config : Boolean)
   is
      function To_Load (File : Virtual_File) return Boolean;
      --  Whether File should be loaded

      -------------
      -- To_Load --
      -------------

      function To_Load (File : Virtual_File) return Boolean is
      begin
         return (Ignore_User_Config and then Default_Autoload)
           or else
             (not Ignore_User_Config and then Load_File_At_Startup
             (Kernel, File, Default => Default_Autoload));
      end To_Load;

      Script : constant Scripting_Language :=
                 Kernel.Scripts.Lookup_Scripting_Language (Python_Name);

   begin
      if Script /= null then
         --  Make sure the error messages will not be lost

         Set_Default_Console (Script, Kernel.Get_Messages_Window);

         --  This adds to sys.path
         Load_Directory (Script, Dir, To_Load'Unrestricted_Access);
      end if;
   end Load_Dir;

   --------------------------------------
   -- Load_System_Python_Startup_Files --
   --------------------------------------

   procedure Load_System_Python_Startup_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Env_Path : constant File_Array := Get_Custom_Path;
   begin
      Load_Dir (Kernel, Support_Core_Dir (Kernel), Default_Autoload => True,
                Ignore_User_Config => True);
      Load_Dir (Kernel, Support_UI_Dir (Kernel), Default_Autoload => True,
                Ignore_User_Config                                => True);
      Load_Dir (Kernel, Support_Languages_Dir (Kernel),
                Default_Autoload   => True,
                Ignore_User_Config => True);
      Load_Dir (Kernel, Support_No_Autoload_Dir (Kernel),
                Default_Autoload => False, Ignore_User_Config => True);
      Load_Dir
        (Kernel, Autoload_System_Dir (Kernel), Default_Autoload => True,
         Ignore_User_Config => False);
      Load_Dir
        (Kernel, No_Autoload_System_Dir (Kernel), Default_Autoload => False,
         Ignore_User_Config => False);

      for J in Env_Path'Range loop
         if Env_Path (J).Is_Directory then
            Load_Dir (Kernel, Env_Path (J), Default_Autoload => True,
                      Ignore_User_Config => False);
         end if;
      end loop;

      declare
         Errors : Boolean;
         Script : constant Scripting_Language :=
           Kernel.Scripts.Lookup_Scripting_Language (Python_Name);
      begin
         --  Now we are ready to import lal_utils (and libadalang)
         Script.Execute_Command
           (CL           => Create ("import lal_utils"),
            Hide_Output  => True,
            Errors       => Errors);

         pragma Assert (not Errors);
      end;
   end Load_System_Python_Startup_Files;

   ------------------------------------
   -- Load_User_Python_Startup_Files --
   ------------------------------------

   procedure Load_User_Python_Startup_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Load_Dir (Kernel, Autoload_User_Dir (Kernel), Default_Autoload => True,
                Ignore_User_Config => False);
   end Load_User_Python_Startup_Files;

   -------------------------------------
   -- Load_No_Autoload_Python_Plugins --
   -------------------------------------

   procedure Load_No_Autoload_Python_Plugins
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Load_Dir
        (Kernel, No_Autoload_System_Dir (Kernel), Default_Autoload => False,
         Ignore_User_Config                                        => False);
   end Load_No_Autoload_Python_Plugins;

   ---------------------------------
   -- Python_File_Command_Handler --
   ---------------------------------

   procedure Python_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle  := Get_Kernel (Data);
      Instance : constant Class_Instance :=
                   Nth_Arg (Data, 1, Get_File_Class (Kernel));
      Info     : constant Virtual_File := Get_Data (Instance);
      function Convert is new Ada.Unchecked_Conversion
        (Ada.Containers.Hash_Type, Integer);
   begin
      if Command = "__str__" or else Command = "__repr__" then
         Set_Return_Value (Data, Full_Name (Info));

      elsif Command = "__cmp__" then
         begin
            declare
               File : constant Virtual_File :=
                        Nth_Arg (Data, 2);
            begin
               if Info < File then
                  Set_Return_Value (Data, -1);
               elsif Info = File then
                  Set_Return_Value (Data, 0);
               else
                  Set_Return_Value (Data, 1);
               end if;
            end;
         exception
            when Invalid_Parameter | No_Such_Parameter =>
               Set_Return_Value (Data, 1);
         end;

      elsif Command = "__hash__" then
         Set_Return_Value (Data, Convert (Full_Name_Hash (Info)));
      end if;

   exception
      when Invalid_Parameter =>
         if Command = "__cmp__" then
            --  We are comparing a File with something else
            Set_Return_Value (Data, -1);
         else
            raise;
         end if;
   end Python_File_Command_Handler;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
      Script : constant Scripting_Language :=
        User.Scripts.Lookup_Scripting_Language (Python_Name);
      Data : Callback_Data'Class := Create (Script, 2);
      Inst : Class_Instance;
   begin
      if Node.Tag = null or else Node.Value = null then
         return null;
      end if;

      Set_Nth_Arg (Data, 1, Node.Tag.all);
      Set_Nth_Arg (Data, 2, Node.Value.all);
      Data.Execute_Command ("modules.Module_Metaclass.load_desktop");

      Inst := Return_Value (Data);
      Free (Data);

      if Inst /= No_Class_Instance then
         return Get_Child (Inst);
      end if;

      return null;

   exception
      when E : Error_In_Command =>
         Trace (Me, E);
         Free (Data);
         return null;
   end Load_Desktop;

   --------------------------------
   -- Python_GUI_Command_Handler --
   --------------------------------

   procedure Python_GUI_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Widget       : Glib.Object.GObject;
      Child        : GPS_MDI_Child;
      C            : MDI_Child;
      Group        : Child_Group;
      Position     : Child_Position;
      Inst         : Class_Instance;
      Should_Unref : Boolean := False;
   begin
      if Command = "add" then
         begin
            Inst := Nth_Arg (Data, 1, Get_GUI_Class (Get_Kernel (Data)));
            Widget := Get_Data (Inst);
         exception
            when Invalid_Parameter =>
               Widget := From_PyGtk (Data, 1);
               Should_Unref := True;
               --  From_PyGtk above adds a Ref to Widget. In this case, this
               --  ref should be transferred to the MDI.
         end;

         if Widget /= null then
            C := Find_MDI_Child_From_Widget (Gtk_Widget (Widget));

            if C = null then
               Group := Child_Group
                 (Nth_Arg (Data, 4, Integer (Group_Default)));
               Position := Child_Position'Val
                 (Nth_Arg (Data, 5, Child_Position'Pos (Position_Automatic)));

               Gtk_New (Child, Gtk_Widget (Widget), Get_Kernel (Data),
                        Flags => Child_Flags
                          (Integer'(Nth_Arg (Data, 7, Integer (All_Buttons)))),
                        Group => Group,
                        Module => Python_Views.Get_Module,
                        Desktop_Independent => False);
               Child.Set_Save_Desktop_Callback
                 (Nth_Arg (Data, 6, Default => null));

               Set_Title (Child, Nth_Arg (Data, 2, ""), Nth_Arg (Data, 3, ""));
               Put (Get_MDI (Get_Kernel (Data)), Child, Position);
               Set_Focus_Child (Child);
               C := MDI_Child (Child);

               if Should_Unref then
                  --  This call to Unref should be done after the MDI has
                  --  added its own ref.
                  Widget.Unref;
               end if;
            end if;

            Set_Return_Value
              (Data,
               Create_MDI_Window_Instance
                 (Get_Script (Data), C));
         end if;
      end if;
   end Python_GUI_Command_Handler;

   ------------------------------------
   -- Python_Project_Command_Handler --
   ------------------------------------

   procedure Python_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      use type Ada.Containers.Hash_Type;
      Project : constant Project_Type := Get_Data (Data, 1);
   begin
      if Command = "__str__" then
         Set_Return_Value (Data, Project.Name);

      elsif Command = "__repr__" then
         Set_Return_Value (Data, Full_Name (Project_Path (Project)));

      elsif Command = "__cmp__" then
         declare
            Project2 : constant Project_Type := Get_Data (Data, 2);
            Name  : constant Virtual_File := Project_Path (Project);
            Name2 : constant Virtual_File := Project_Path (Project2);
         begin
            if Name < Name2 then
               Set_Return_Value (Data, -1);
            elsif Name = Name2 then
               Set_Return_Value (Data, 0);
            else
               Set_Return_Value (Data, 1);
            end if;
         end;

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data,
            Integer
              (Full_Name_Hash (Project_Path (Project))
               mod Ada.Containers.Hash_Type (Integer'Last)));
      end if;

   exception
      when Invalid_Parameter =>
         if Command = "__cmp__" then
            --  We are comparing a Project with something else
            Set_Return_Value (Data, -1);
         else
            raise;
         end if;
   end Python_Project_Command_Handler;

   -----------------------------------
   -- Python_Entity_Command_Handler --
   -----------------------------------

   procedure Python_Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Entity  : constant Root_Entity'Class := Get_Data (Data, 1);
      Decl    : General_Entity_Declaration;
   begin
      if Command = "__str__"
        or else Command = "__repr__"
      then
         if Is_Predefined_Entity (Entity) then
            Set_Return_Value (Data, Get_Name (Entity));
         else
            Decl := Get_Declaration (Entity);

            Set_Return_Value
              (Data,
               Get_Name (Entity) & ':'
               & (+Decl.Loc.File.Base_Name) & ':'
               & Image (Decl.Loc.Line) & ':'
               & Image (Integer (Decl.Loc.Column)));
         end if;

      elsif Command = "__hash__" then
         Set_Return_Value (Data, Hash (Entity));

      elsif Command = "__cmp__" then
         Set_Return_Value
           (Data, Cmp (Entity, Get_Data (Data, 2)));
      end if;

   exception
      when Invalid_Parameter =>
         if Command = "__cmp__" then
            --  We are comparing an Entity with something else
            Set_Return_Value (Data, -1);
         else
            raise;
         end if;
   end Python_Entity_Command_Handler;

   -------------------------------------
   -- Python_Location_Command_Handler --
   -------------------------------------

   procedure Python_Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Info     : constant File_Location_Info := Get_Data (Data, 1);
      Fileinfo : constant Virtual_File := Get_Data (Get_File (Info));
   begin
      if Command = "__str__"
        or else Command = "__repr__"
      then
         Set_Return_Value
           (Data,
            +Base_Name (Fileinfo) & ':'
            & Image (Get_Line (Info)) & ':'
            & Image (Integer (Get_Column (Info))));

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data, Integer
            (Hash (+Full_Name (Fileinfo)
                   & Image (Get_Line (Info))
                   & Image (Integer (Get_Column (Info))))));

      elsif Command = "__cmp__" then
         declare
            use Basic_Types;

            Info2     : constant File_Location_Info := Get_Data (Data, 2);
            Fileinfo2 : constant Virtual_File := Get_Data (Get_File (Info2));
            Line1, Line2 : Integer;
            Col1, Col2   : Visible_Column_Type;
         begin
            if Fileinfo < Fileinfo2 then
               Set_Return_Value (Data, -1);
            elsif Fileinfo = Fileinfo2 then
               Line1 := Get_Line (Info);
               Line2 := Get_Line (Info2);

               if Line1 < Line2 then
                  Set_Return_Value (Data, -1);

               elsif Line1 = Line2 then
                  Col1 := Get_Column (Info);
                  Col2 := Get_Column (Info2);

                  if Col1 < Col2 then
                     Set_Return_Value (Data, -1);
                  elsif Col1 = Col2 then
                     Set_Return_Value (Data, 0);
                  else
                     Set_Return_Value (Data, 1);
                  end if;

               else
                  Set_Return_Value (Data, 1);
               end if;
            else
               Set_Return_Value (Data, 1);
            end if;
         end;
      end if;

   exception
      when Invalid_Parameter =>
         if Command = "__cmp__" then
            --  We are comparing a File_Location with something else
            Set_Return_Value (Data, -1);
         else
            raise;
         end if;
   end Python_Location_Command_Handler;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Python_Module_Record) is
      Script  : constant Scripting_Language :=
        Get_Kernel (Module).Scripts.Lookup_Scripting_Language (Python_Name);
      Errors  : aliased Boolean;
      Result  : PyObject;
   begin
      --  Importing jedi (versions 0.9, 0.12) raises "Error in sys.exitfunc"
      --  in console if future 0.16 is installed because of some exception
      --  when python is finalizing. Following code prevent this.

      Result := Run_Command
        (Python_Scripting (Script),
         "import atexit ; atexit._run_exitfuncs()",
         Need_Output     => False,
         Show_Command    => False,
         Hide_Output     => True,
         Hide_Exceptions => True,
         Errors          => Errors'Unchecked_Access);
      Py_XDECREF (Result);

      Unregister_Python_Scripting (Get_Kernel (Module).Scripts);
   end Destroy;

end Python_Module;
