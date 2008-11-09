-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
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
with Ada.Unchecked_Conversion;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Templates;        use GNATCOLL.Templates;
with GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with System;                    use System;

with Gdk;                       use Gdk;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Window;                use Gdk.Window;

with Glib.Object;               use Glib.Object;
with Glib.Properties;           use Glib.Properties;
with Glib.Xml_Int;              use Glib.Xml_Int;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.Container;             use Gtk.Container;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Icon_Factory;          use Gtk.Icon_Factory;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Basic_Mapper;              use Basic_Mapper;
with Default_Preferences;       use Default_Preferences;
with Entities.Queries;          use Entities.Queries;
with Entities;                  use Entities;
with File_Utils;                use File_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Clipboard;      use GPS.Kernel.Clipboard;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Custom;         use GPS.Kernel.Custom;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Macros;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Properties;     use GPS.Kernel.Properties;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;         use GPS.Kernel.Styles;
with GPS.Kernel.Timeout;        use GPS.Kernel.Timeout;
with GPS.Main_Window;           use GPS.Main_Window;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with Language_Handlers;         use Language_Handlers;
with Language.Tree.Database;    use Language.Tree.Database;
with Namet;                     use Namet;
with Prj.Attr;                  use Prj.Attr;
with Projects.Registry;         use Projects, Projects.Registry;
with Remote_Descriptors;
with Switches_Chooser;          use Switches_Chooser;
with System.Address_Image;
with Traces;                    use Traces;
with XML_Parsers;

package body GPS.Kernel is

   Me        : constant Debug_Handle := Create ("gps_kernel");
   Ref_Me    : constant Debug_Handle :=
                 Create ("Contexts.Ref", GNATCOLL.Traces.Off);
   Create_Me : constant Debug_Handle :=
                 Create ("Contexts.Mem", GNATCOLL.Traces.Off);

   History_Max_Length : constant Positive := 10;
   --  <preferences> Maximum number of entries to store in each history

   Desktop_Name : constant String := "desktop.xml";

   use Action_Filters_Htable.String_Hash_Table;

   function To_Address is new Ada.Unchecked_Conversion
     (Selection_Context_Data, System.Address);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Registry'Class, Project_Registry_Access);

   function Process_Anim (Data : Process_Data) return Boolean;
   --  Process_Timeout callback to handle image animations

   procedure Row_Activated (Widget : access Gtk_Widget_Record'Class);
   --  Called when a specific entity declaration has been selected in the
   --  overloaded entities dialog.

   procedure Free (Tool : in out Tool_Properties_Record);
   procedure Free_Tools (Kernel : access Kernel_Handle_Record'Class);
   --  Free the list of registered tools

   procedure Select_Entity_Declaration
     (Kernel      : access Kernel_Handle_Record'Class;
      File        : Source_File;
      Entity_Name : String;
      Decl        : in out Entity_Information;
      Status      : out Entities.Queries.Find_Decl_Or_Body_Query_Status);
   --  Open a dialog to ask the user to select among multiple declaration for
   --  the entity with name Entity_Name.
   --  Decl is set to No_Entity_Information and Status to Entity_Not_Found if
   --  the user didn't select any declaration. When calling this procedure,
   --  Decl should be put to the closest entity match given line and column
   --  numbers.

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences change

   procedure On_Main_Window_Destroyed
     (Kernel : System.Address; Main_Window : System.Address);
   pragma Convention (C, On_Main_Window_Destroyed);
   --  Called when the main window is destroyed, so that the kernel no longer
   --  points to an invalid window

   --------------------------
   -- Get_Language_Handler --
   --------------------------

   function Get_Language_Handler
     (Handle : access Kernel_Handle_Record)
      return Language_Handlers.Language_Handler is
   begin
      return Handle.Lang_Handler;
   end Get_Language_Handler;

   ----------------------
   -- Get_Icon_Factory --
   ----------------------

   function Get_Icon_Factory
     (Handle : access Kernel_Handle_Record)
      return Gtk.Icon_Factory.Gtk_Icon_Factory is
   begin
      return Handle.Icon_Factory;
   end Get_Icon_Factory;

   ----------
   -- Hash --
   ----------

   function Hash (Hook : Hook_Name) return Hook_Htable_Num is
      function Internal is new HTables.Hash (Hook_Htable_Num);
   begin
      return Internal (String (Hook));
   end Hash;

   --------------------------
   -- Set_Destruction_Flag --
   --------------------------

   procedure Set_Destruction_Flag
     (Handle : access Kernel_Handle_Record;
      Flag   : Boolean) is
   begin
      Handle.Is_In_Destruction := Flag;
   end Set_Destruction_Flag;

   -----------------------
   -- Is_In_Destruction --
   -----------------------

   function Is_In_Destruction
     (Handle : access Kernel_Handle_Record) return Boolean is
   begin
      return Handle.Is_In_Destruction;
   end Is_In_Destruction;

   ------------------
   -- GNAT_Version --
   ------------------

   function GNAT_Version
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.GNAT_Version = null then
         return -"<unknown version>";
      else
         return Handle.GNAT_Version.all;
      end if;
   end GNAT_Version;

   ------------------------------
   -- On_Main_Window_Destroyed --
   ------------------------------

   procedure On_Main_Window_Destroyed
     (Kernel : System.Address; Main_Window : System.Address)
   is
      pragma Unreferenced (Main_Window);
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Kernel_Handle);
   begin
      Convert (Kernel).Main_Window := null;
   end On_Main_Window_Destroyed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Handle           : out Kernel_Handle;
      Main_Window      : Gtk.Window.Gtk_Window;
      Home_Dir         : String;
      Prefix_Directory : String)
   is
      Handler : Language_Handler;
   begin
      Handle := new Kernel_Handle_Record;
      Glib.Object.Initialize (Handle);

      Handle.Main_Window  := Main_Window;
      Weak_Ref (Handle.Main_Window,
                On_Main_Window_Destroyed'Access,
                Handle.all'Address);

      Handle.Home_Dir := new String'(Name_As_Directory (Home_Dir));
      Handle.Prefix   := new String'(Name_As_Directory (Prefix_Directory));

      --  Create the language handler

      Create_Handler (Handler);
      Handle.Lang_Handler := Handler;

      Handle.Registry := new Project_Registry;
      Load_Empty_Project (Handle.Registry.all);

      Set_Registry
        (Language_Handler (Handle.Lang_Handler), Handle.Registry);

      Handle.Gnatls_Cache := null;
      --  by default, the local server
      Handle.Gnatls_Server := new String'("");

      --  Note: we do not compute the view of this project yet. This will be
      --  done only if no other project was loaded from the command line, which
      --  is more efficient in case the current directory has lots of source
      --  files.

      Handle.Database := Create (Handle.Registry);
      Register_Language_Handler (Handle.Database, Handler);

      Gtk_New (Handle.Icon_Factory);
      Add_Default (Handle.Icon_Factory);

      Gtk_New (Handle.Tooltips);
      Ref (Handle.Tooltips);
      Sink (Handle.Tooltips);

      --  Initialize the preferences. We load the file now, even though it
      --  will also be reloaded after the customization files, so that themes
      --  do not override user's preferences.
      --  We need to load now so that for instance the splash screen is
      --  correctly taken into account.
      Handle.Preferences := new GPS_Preferences_Record;
      Register_Global_Preferences (Handle);
      Load_Preferences (Handle);

      --  Load the styles
      Load_Styles (Handle, Create (Handle.Home_Dir.all & "styles.xml"));

      On_Preferences_Changed (Handle);

      Handle.History := new History_Record;
      Load (Handle.History.all, Handle.Home_Dir.all & "histories.xml");
      Set_Max_Length (Handle.History.all, History_Max_Length);

      Handle.Scripts := new Kernel_Scripts_Repository'
        (Scripts_Repository_Record with Kernel => Handle);

      Restore_Persistent_Properties (Handle);

      Create_Clipboard (Handle);

      Add_Hook
        (Handle, Preferences_Changed_Hook,
         Wrapper (On_Preferences_Changed'Access),
         Name => "kernel.preferences_changed");
   end Gtk_New;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      if Kernel.Hidden_File_Matcher /= null then
         Unchecked_Free (Kernel.Hidden_File_Matcher);
      end if;

      declare
         Pattern : constant String := Hidden_Directories_Pattern.Get_Pref;
      begin
         if Pattern /= "" then
            Kernel.Hidden_File_Matcher :=
              new Pattern_Matcher'(Compile (Pattern));
         end if;
      end;

      Set_Trusted_Mode (Get_Registry (Kernel).all, Trusted_Mode.Get_Pref);
   end On_Preferences_Changed;

   ------------------
   -- Get_Database --
   ------------------

   function Get_Database
     (Kernel : access Kernel_Handle_Record) return Entities_Database is
   begin
      return Kernel.Database;
   end Get_Database;

   ----------------------------
   -- Get_Construct_Database --
   ----------------------------

   function Get_Construct_Database
     (Kernel : access Kernel_Handle_Record)
      return Language.Tree.Database.Construct_Database_Access is
   begin
      if Kernel.Construct_Database = null then
         Kernel.Construct_Database := new Construct_Database;
      end if;

      return Kernel.Construct_Database;
   end Get_Construct_Database;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences (Handle : access Kernel_Handle_Record) is
   begin
      Load_Preferences
        (Handle.Preferences, Handle.Home_Dir.all & "preferences");
   end Load_Preferences;

   ------------------------------
   -- Get_Default_Accelerators --
   ------------------------------

   function Get_Default_Accelerators
     (Handle : access Kernel_Handle_Record)
      return Gtk.Accel_Group.Gtk_Accel_Group is
   begin
      return GPS_Window (Handle.Main_Window).Main_Accel_Group;
   end Get_Default_Accelerators;

   ---------------------
   -- Get_Preferences --
   ---------------------

   function Get_Preferences
     (Handle : access Kernel_Handle_Record)
      return Default_Preferences.Preferences_Manager is
   begin
      return Handle.Preferences;
   end Get_Preferences;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Data : Glib.Object.GObject; Id : Gtk.Handlers.Handler_Id) is
   begin
      Add_Watch (Id, Data);
   end Setup;

   ---------------------------
   -- Source_Lines_Revealed --
   ---------------------------

   procedure Source_Lines_Revealed
     (Handle  : access Kernel_Handle_Record;
      Context : Selection_Context)
   is
      Data : aliased Context_Hooks_Args :=
               (Hooks_Data with Context => Context);
   begin
      Run_Hook (Handle, Source_Lines_Revealed_Hook, Data'Unchecked_Access);
   end Source_Lines_Revealed;

   -----------------
   -- File_Edited --
   -----------------

   procedure File_Edited
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Files : File_Array_Access := Handle.Open_Files;
      Data  : aliased File_Hooks_Args;
   begin
      if not Is_Open (Handle, File) then
         if Files = null then
            Handle.Open_Files := new File_Array (1 .. 1);
         else
            Handle.Open_Files :=
              new File_Array (Files'First .. Files'Last + 1);
            Handle.Open_Files (Files'Range) := Files.all;
            Unchecked_Free (Files);
         end if;

         Handle.Open_Files (Handle.Open_Files'Last) := File;
         Data := File_Hooks_Args'(Hooks_Data with File => File);
         Run_Hook (Handle, File_Edited_Hook, Data'Unchecked_Access);
      end if;
   end File_Edited;

   -----------------------
   -- Before_File_Saved --
   -----------------------

   procedure Before_File_Saved
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Data : aliased File_Hooks_Args := (Hooks_Data with File => File);
   begin
      Run_Hook (Handle, Before_File_Saved_Hook, Data'Unchecked_Access);
   end Before_File_Saved;

   ----------------
   -- File_Saved --
   ----------------

   procedure File_Saved
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Data : aliased File_Hooks_Args := (Hooks_Data with File => File);
   begin
      Run_Hook (Handle, File_Saved_Hook, Data'Unchecked_Access);
   end File_Saved;

   -----------------
   -- File_Closed --
   -----------------

   procedure File_Closed
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Files : File_Array_Access;
      Data  : aliased File_Hooks_Args := (Hooks_Data with File => File);
   begin
      Run_Hook (Handle, File_Closed_Hook, Data'Unchecked_Access);

      --  We must compute the open files after having run the hook, in case
      --  the file array has been reallocated.
      Files := Handle.Open_Files;

      if Files /= null then
         for F in Files'Range loop
            if Files (F) = File then
               Handle.Open_Files :=
                 new File_Array (Files'First .. Files'Last - 1);
               Handle.Open_Files (Files'First .. F - 1) :=
                 Files (Files'First .. F - 1);
               Handle.Open_Files (F .. Handle.Open_Files'Last) :=
                 Files (F + 1 .. Files'Last);
               Unchecked_Free (Files);
               exit;
            end if;
         end loop;
      end if;
   end File_Closed;

   ------------------
   -- File_Deleted --
   ------------------

   procedure File_Deleted
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Data : aliased File_Hooks_Args := (Hooks_Data with File => File);
   begin
      Run_Hook (Handle, File_Deleted_Hook, Data'Unchecked_Access);
   end File_Deleted;

   ------------------
   -- File_Renamed --
   ------------------

   procedure File_Renamed
     (Handle   : access Kernel_Handle_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      New_Path : GNATCOLL.VFS.Virtual_File)
   is
      Data : aliased Files_2_Hooks_Args :=
               (Hooks_Data with File => File, Renamed => New_Path);
   begin
      Run_Hook (Handle, File_Renamed_Hook, Data'Unchecked_Access);
   end File_Renamed;

   --------------------------
   -- File_Changed_On_Disk --
   --------------------------

   procedure File_Changed_On_Disk
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      Data : aliased File_Hooks_Args := (Hooks_Data with File => File);
   begin
      Run_Hook (Handle, File_Changed_On_Disk_Hook, Data'Unchecked_Access);
   end File_Changed_On_Disk;

   --------------------------
   -- Compilation_Finished --
   --------------------------

   procedure Compilation_Finished
     (Handle   : access Kernel_Handle_Record;
      Category : String)
   is
      Data : aliased String_Hooks_Args :=
               (Hooks_Data with Length => Category'Length, Value => Category);
   begin
      Run_Hook (Handle, Compilation_Finished_Hook, Data'Unchecked_Access);
   end Compilation_Finished;

   --------------------------
   -- Compilation_Starting --
   --------------------------

   function Compilation_Starting
     (Handle   : access Kernel_Handle_Record;
      Category : String;
      Quiet    : Boolean;
      Shadow   : Boolean) return Boolean
   is
      Data : aliased Compilation_Hooks_Args :=
               (Hooks_Data with
                Length => Category'Length,
                Value  => Category,
                Quiet  => Quiet,
                Shadow => Shadow);
   begin
      return Run_Hook_Until_Failure
        (Handle, Compilation_Starting_Hook, Data'Unchecked_Access);
   end Compilation_Starting;

   -------------
   -- Is_Open --
   -------------

   function Is_Open
     (Kernel   : access Kernel_Handle_Record;
      Filename : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      if Kernel.Open_Files /= null then
         for F in Kernel.Open_Files'Range loop
            if Kernel.Open_Files (F) = Filename then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Is_Open;

   ----------------
   -- Open_Files --
   ----------------

   function Open_Files
     (Kernel : access Kernel_Handle_Record) return GNATCOLL.VFS.File_Array is
   begin
      if Kernel.Open_Files /= null then
         return Kernel.Open_Files.all;
      else
         return (1 .. 0 => GNATCOLL.VFS.No_File);
      end if;
   end Open_Files;

   ---------------
   -- Is_Hidden --
   ---------------

   function Is_Hidden
     (Kernel    : access Kernel_Handle_Record;
      Base_Name : String) return Boolean is
   begin
      return Kernel.Hidden_File_Matcher /= null
        and then Match (Kernel.Hidden_File_Matcher.all, Base_Name);
   end Is_Hidden;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed (Handle : access Kernel_Handle_Record) is
      C    : constant Selection_Context := Get_Current_Context (Handle);
      Data : aliased Context_Hooks_Args := (Hooks_Data with Context => C);
   begin
      --  ??? running the hook results in a memory leak for the open_project
      --  test, since file_information is never freed. Ultimately, this is
      --  because Finalize is called when the program itself finalizes, and
      --  therefore we do not try to free the list of files. But in fact C
      --  should be finalized on exit of this function, since there should be
      --  no reason for anyone to keep a handle on the context). Apparently,
      --  some python object is created that keeps a ref to the context
      Run_Hook (Handle, Context_Changed_Hook, Data'Unchecked_Access);
   end Context_Changed;

   -------------------------
   -- Get_Current_Context --
   -------------------------

   function Get_Current_Context
     (Kernel : access Kernel_Handle_Record) return Selection_Context
   is
      Module  : Module_ID;
      Handle  : constant Kernel_Handle := Kernel_Handle (Kernel);
      Context : Selection_Context := New_Context;
   begin
      --  ??? Shouldn't have to recompute everytime, but this is needed when
      --  in the editor (comment-line for instance relies on accurate info in
      --  the context to get the current line)
      Module := Get_Current_Module (Kernel);

      Set_Context_Information
        (Context, Handle, Abstract_Module_ID (Module));

      if Module /= null then
         Default_Context_Factory
           (Module, Context, Get_Widget (Get_Focus_Child (Get_MDI (Handle))));
      end if;

      return Context;
   end Get_Current_Context;

   ---------------------------
   -- Get_Context_For_Child --
   ---------------------------

   function Get_Context_For_Child
     (Child : Gtkada.MDI.MDI_Child) return Selection_Context
   is
      Module  : Module_ID;
      Context : Selection_Context;
   begin
      if Child = null then
         return No_Context;
      end if;

      Module := Module_ID (Get_Module_From_Child (Child));

      if Module /= null then
         Context.Data.Data := new Selection_Context_Data_Record;
         Default_Context_Factory (Module, Context, Get_Widget (Child));
         return Context;
      else
         return No_Context;
      end if;
   end Get_Context_For_Child;

   ------------------
   -- Save_Desktop --
   ------------------

   procedure Save_Desktop
     (Handle             : access Kernel_Handle_Record;
      As_Default_Desktop : Boolean := False)
   is
      function Get_Project_Name return Virtual_File;
      --  Return the project name to match in the file

      ----------------------
      -- Get_Project_Name --
      ----------------------

      function Get_Project_Name return Virtual_File is
         Project : constant Project_Type := Get_Project (Handle);
      begin
         if As_Default_Desktop or else Status (Project) /= From_File then
            return GNATCOLL.VFS.No_File;
         else
            return Project_Path (Project);
         end if;
      end Get_Project_Name;

      Main_Window : constant Gdk.Window.Gdk_Window :=
                      Get_Window (Handle.Main_Window);
      MDI          : constant MDI_Window := Get_MDI (Handle);
      File_Name    : constant String := Handle.Home_Dir.all & Desktop_Name;
      Project_Name : constant Virtual_File := Get_Project_Name;
      N            : Node_Ptr;
      M            : Node_Ptr;
      Old          : Node_Ptr;
      Err          : GNAT.Strings.String_Access;
      Success      : Boolean;

   begin
      if Project_Name = GNATCOLL.VFS.No_File
        and then not As_Default_Desktop
      then
         Trace (Me, "not saving the default desktop");
         return;
      end if;

      --  Read the previous contents of the file, to save the desktops for
      --  other projects

      Trace (Me, "saving desktop file " & File_Name
             & " for project " & Full_Name (Project_Name).all);

      if Main_Window = null then
         return;
      end if;

      if GNAT.OS_Lib.Is_Regular_File (File_Name) then
         XML_Parsers.Parse (File_Name, Old, Err);

         if Err /= null then
            Insert (Handle, Err.all, Mode => Error);
            Free (Err);
         end if;
      end if;

      N := new Node'
        (Tag           => new String'("GPS_Desktop"),
         Child         => null,
         Parent        => null,
         Value         => null,
         Attributes    => null,
         Next          => null,
         Specific_Data => 0);

      --  Merge the old contents of the file

      if Old /= null then
         M := Old.Child;

         while M /= null loop
            if M.Tag /= null
              and then M.Tag.all = "MDI"
              and then Get_Attribute (M, "project") /=
                 Full_Name (Project_Name).all
            then
               Add_Child (N, Deep_Copy (M));
            end if;

            M := M.Next;
         end loop;

         Free (Old);
      end if;

      --  Add the current content, indexed on the current project

      M := GPS.Kernel.Kernel_Desktop.Save_Desktop
        (MDI, Kernel_Handle (Handle));
      Set_Attribute (M, "project", Full_Name (Project_Name).all);
      Add_Child (N, M);

      Print (N, File_Name, Success);
      Free (N);

      if not Success then
         Report_Preference_File_Error (Handle, File_Name);
      end if;
   end Save_Desktop;

   ----------------------------------
   -- Report_Preference_File_Error --
   ----------------------------------

   procedure Report_Preference_File_Error
     (Handle   : access Kernel_Handle_Record;
      Filename : String)
   is
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);
   begin
      if Is_In_Destruction (Handle) then
         Button := Message_Dialog
           ((-"Could not save the configuration file ") & Filename & ASCII.LF &
            (-"Please verify that you have write access to this file."),
            Error, Button_OK, Justification => Justify_Left);
      else
         GPS.Kernel.Console.Insert
           (Handle,
            (-"Could not save the configuration file ") & Filename & ASCII.LF &
            (-"Please verify that you have write access to this file."),
            Mode => GPS.Kernel.Console.Error);
         Raise_Console (Handle);
      end if;
   end Report_Preference_File_Error;

   ----------------------
   -- Has_User_Desktop --
   ----------------------

   function Has_User_Desktop
     (Handle : access Kernel_Handle_Record) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Regular_File (Handle.Home_Dir.all & Desktop_Name);
   end Has_User_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Handle : access Kernel_Handle_Record) return Boolean
   is
      MDI                     : constant MDI_Window := Get_MDI (Handle);
      File                    : constant String :=
                                  Handle.Home_Dir.all & Desktop_Name;
      Project                 : constant Project_Type := Get_Project (Handle);
      Main_Window             : constant GPS_Window :=
                                  GPS_Window (Handle.Main_Window);
      Predefined_Desktop      : constant String :=
                                  Get_System_Dir (Handle) &
                                    "share/gps/desktop.xml";
      Node                    : Node_Ptr;
      Project_Name            : Virtual_File := GNATCOLL.VFS.No_File;
      Child                   : Node_Ptr;
      Desktop_Node            : Node_Ptr;
      Default_Desktop_Node    : Node_Ptr;
      Success_Loading_Desktop : Boolean := False;
      Err                     : String_Access;
      Is_Default_Desktop      : Boolean := False;
      Try_User_Desktop        : Boolean := True;

   begin
      Main_Window.Desktop_Loaded := True;

      if Status (Project) = From_File then
         Project_Name := Project_Path (Project);
      end if;

      --  We might have to try twice: once to check the user's desktop.xml
      --  file, and if that fails the predefined desktop.xml file

      while not Success_Loading_Desktop
        and then (Try_User_Desktop or else not Is_Default_Desktop)
      loop
         if Try_User_Desktop and then GNAT.OS_Lib.Is_Regular_File (File) then
            Trace (Me, "loading desktop file " & File
                   & " Project=" & Full_Name (Project_Name).all);
            XML_Parsers.Parse (File, Node, Err);

         elsif GNAT.OS_Lib.Is_Regular_File (Predefined_Desktop) then
            Trace (Me, "loading predefined desktop");
            Is_Default_Desktop := True;
            XML_Parsers.Parse (Predefined_Desktop, Node, Err);

         else
            Trace (Me, "No desktop to load");
            Set_Default_Size (Main_Window, 800, 600);
            Show_All (Get_Child (Main_Window));
            return False;
         end if;

         if Node = null then
            Insert (Handle, Err.all, Mode => Error);
            Free (Err);

         else
            Child := Node.Child;
         end if;

         while Child /= null loop
            if Child.Tag /= null then
               if Child.Tag.all = "MDI" then
                  if Get_Attribute (Child, "project") = "" then
                     Default_Desktop_Node := Child;
                  elsif Get_Attribute (Child, "project") =
                    Full_Name (Project_Name).all
                  then
                     Desktop_Node := Child;
                  end if;
               end if;
            end if;

            Child := Child.Next;
         end loop;

         --  Call Show_All before restoring the desktop, in case some
         --  children stored in the desktop have something to hide.

         Show_All (Get_Child (Main_Window));

         Success_Loading_Desktop := False;

         if Desktop_Node /= null then
            Trace (Me, "loading desktop for " & Full_Name (Project_Name).all);
            Success_Loading_Desktop := Kernel_Desktop.Restore_Desktop
              (MDI, Desktop_Node, Kernel_Handle (Handle));

         elsif Default_Desktop_Node /= null then
            Trace (Me, "loading default desktop (from file)");
            Success_Loading_Desktop := Kernel_Desktop.Restore_Desktop
              (MDI, Default_Desktop_Node, Kernel_Handle (Handle));
         end if;

         Free (Node);

         --  If we fail loading the user desktop, we still want to load the
         --  default desktop.

         Try_User_Desktop := False;

         if not Success_Loading_Desktop then
            Trace (Me, "Couldn't load desktop successfully");
         end if;
      end loop;

      --  Report a context changed, so that all views can update themselves
      Context_Changed (Handle);

      if Is_Default_Desktop then
         return False;
      else
         return Desktop_Node /= null
           or else Default_Desktop_Node /= null;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end Load_Desktop;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Context : in out Selection_Context_Controlled)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Selection_Context_Data_Record, Selection_Context_Data);

      procedure Free (Data : in out Selection_Context_Data_Record);
      --  Free memory used by Data

      ----------
      -- Free --
      ----------

      procedure Free (Data : in out Selection_Context_Data_Record) is
      begin
         if Active (Create_Me) then
            Trace (Create_Me, "Freeing context: 0x"
                   & System.Address_Image (Data'Address));
         end if;

         Free (Data.Category_Name);
         Free (Data.Message);
         Free (Data.Text);
         Free (Data.Entity_Name);
         Free (Data.Expression);

         --  Do not unref the entity stored in the context if the kernel is in
         --  destruction or as already been destroyed since the entity has
         --  already been freed as part of the kernel destruction.

         if Data.Kernel /= null and then not Data.Kernel.Is_In_Destruction then
            Unref (Data.Entity);

            --   ??? problem of double deallocation at shutdown time, ideally
            --   the following call should be outside of the conditional.
            GNATCOLL.VFS.Unchecked_Free (Data.Files);
         end if;

         Free (Data.Activity_Id);
         Free (Data.Revision);
         Free (Data.Other_Revision);
         Free (Data.Tag);
         Free (Data.Instances);
      end Free;

      Garbage : Selection_Context_Data;
   begin
      if Context.Data /= null then
         if Active (Ref_Me) then
            Trace (Ref_Me, "Before decref context: ("
                   & System.Address_Image (To_Address (Context.Data))
                   & " " & Context.Data.Ref_Count'Img & ")");
         end if;

         Context.Data.Ref_Count := Context.Data.Ref_Count - 1;

         if Context.Data.Ref_Count = 0 then
            if Active (Create_Me) then
               GNATCOLL.Traces.Increase_Indent
                 (Create_Me, "Destroy selection context ("
                  & System.Address_Image (To_Address (Context.Data)) & ")");
            end if;

            Garbage := Context.Data;
            Context.Data := null;

            --  Do not access Context any more below, since the call to Free
            --  will free instances and their user data, and the current call
            --  to Finalize might come from such a user data.

            Free (Garbage.all);
            Unchecked_Free (Garbage);

            if Active (Create_Me) then
               GNATCOLL.Traces.Decrease_Indent
                 (Create_Me, "Done destroying selection context");
            end if;

         else
            --  In any case, Context is no longer used, so we reset Data to
            --  null. Not sure why, but Finalize seems to be called multiple
            --  time when GNAT finalizes the controlled objects.

            Context.Data := null;
         end if;

      end if;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         if Active (Create_Me) then
            GNATCOLL.Traces.Decrease_Indent (Create_Me);
         end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust
     (Context : in out Selection_Context_Controlled) is
   begin
      if Context.Data /= null then
         Context.Data.Ref_Count := Context.Data.Ref_Count + 1;
         if Active (Ref_Me) then
            Trace
              (Ref_Me, "Adjust selection_context="
               & System.Address_Image (To_Address (Context.Data))
               & " " & Context.Data.Ref_Count'Img & ")");
         end if;
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Adjust;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Context : Selection_Context) return Kernel_Handle is
   begin
      if Context.Data.Data = null then
         return null;
      else
         return Context.Data.Data.Kernel;
      end if;
   end Get_Kernel;

   -----------------
   -- Get_Creator --
   -----------------

   function Get_Creator
     (Context : Selection_Context) return Abstract_Module_ID is
   begin
      if Context.Data.Data = null then
         return null;
      else
         return Context.Data.Data.Creator;
      end if;
   end Get_Creator;

   -----------------
   -- New_Context --
   -----------------

   function New_Context return Selection_Context is
      Context : constant Selection_Context :=
                  (Data => (Ada.Finalization.Controlled with
                            Data => new Selection_Context_Data_Record));
   begin
      if Active (Create_Me) then
         Trace (Create_Me, "Creating new context: 0x"
                & System.Address_Image (Context.Data.Data.all'Address));
      end if;
      return Context;
   end New_Context;

   -----------------------------
   -- Set_Context_Information --
   -----------------------------

   procedure Set_Context_Information
     (Context : in out Selection_Context;
      Kernel  : access Kernel_Handle_Record'Class;
      Creator : Abstract_Module_ID) is
   begin
      Context.Data.Data.Kernel := Kernel_Handle (Kernel);
      Context.Data.Data.Creator := Creator;
   end Set_Context_Information;

   ---------------------
   -- Get_Main_Window --
   ---------------------

   function Get_Main_Window
     (Handle : access Kernel_Handle_Record) return Gtk.Window.Gtk_Window is
   begin
      return Handle.Main_Window;
   end Get_Main_Window;

   ------------------
   -- Get_Tooltips --
   ------------------

   function Get_Tooltips
     (Handle : access Kernel_Handle_Record) return Gtk.Tooltips.Gtk_Tooltips is
   begin
      return Handle.Tooltips;
   end Get_Tooltips;

   -----------------
   -- Get_Toolbar --
   -----------------

   function Get_Toolbar
     (Handle : access Kernel_Handle_Record) return Gtk.Toolbar.Gtk_Toolbar is
   begin
      if Handle.Main_Window /= null then
         return GPS_Window (Handle.Main_Window).Toolbar;
      else
         return null;
      end if;
   end Get_Toolbar;

   ------------------
   -- Process_Anim --
   ------------------

   function Process_Anim (Data : Process_Data) return Boolean is
      Window : constant GPS_Window := GPS_Window (Data.Kernel.Main_Window);
   begin
      if Anim_Cb (Data.Kernel) then
         Window.Animation_Timeout := Process_Timeout.Add
           (Guint32 (Get_Delay_Time (Window.Animation_Iter)),
            Process_Anim'Access, Data);
      end if;

      return False;
   end Process_Anim;

   --------------
   -- Get_Busy --
   --------------

   function Get_Busy
     (Handle : access Kernel_Handle_Record'Class) return Boolean is
   begin
      return GPS_Window (Handle.Main_Window).State_Level > 0;
   end Get_Busy;

   ----------------
   -- Push_State --
   ----------------

   procedure Push_State
     (Handle : access Kernel_Handle_Record'Class;
      State  : Action_Kernel_State)
   is
      Window : GPS_Window;
   begin
      if Handle = null then
         return;
      end if;

      Window := GPS_Window (Handle.Main_Window);

      if Window = null
        or else Gtk.Object.In_Destruction_Is_Set (Window)
      then
         return;
      end if;

      if State = Busy then
         Set_Busy_Cursor (Get_Window (Window), True, True);
         Window.Busy_Level := Window.Busy_Level + 1;
      end if;

      if Window.State_Level = 0
        and then Window.Animation_Timeout = 0
        and then Window.Animation_Iter /= null
      then
         Window.Animation_Timeout := Process_Timeout.Add
           (Guint32 (Get_Delay_Time (Window.Animation_Iter)),
            Process_Anim'Access,
            (Kernel_Handle (Handle), null, null, null, null, null, False));
      end if;

      Window.State_Level := Window.State_Level + 1;
   end Push_State;

   ---------------
   -- Pop_State --
   ---------------

   procedure Pop_State (Handle : access Kernel_Handle_Record'Class) is
      Window : GPS_Window;
   begin
      if Handle = null then
         return;
      end if;

      Window := GPS_Window (Handle.Main_Window);

      if Window = null
        or else Gtk.Object.In_Destruction_Is_Set (Window)
      then
         return;
      end if;

      if Window.State_Level > 0 then
         Window.State_Level := Window.State_Level - 1;

         if Window.Busy_Level > 0 then
            Window.Busy_Level := Window.Busy_Level - 1;

            if Window.Busy_Level = 0 then
               Set_Busy_Cursor (Get_Window (Window), False, False);
            end if;
         end if;

         if Window.State_Level = 0
           and then not Gtk.Object.Destroyed_Is_Set (Get_Main_Window (Handle))
           and then Window.Animation_Timeout /= 0
         then
            Timeout_Remove (Window.Animation_Timeout);
            Window.Animation_Timeout := 0;
            Display_Default_Image (Kernel_Handle (Handle));
         end if;
      end if;
   end Pop_State;

   ------------------
   -- Get_Home_Dir --
   ------------------

   function Get_Home_Dir
     (Handle : access Kernel_Handle_Record) return String is
   begin
      return Handle.Home_Dir.all;
   end Get_Home_Dir;

   --------------------
   -- Get_System_Dir --
   --------------------

   function Get_System_Dir
     (Handle : access Kernel_Handle_Record) return String is
   begin
      return Handle.Prefix.all;
   end Get_System_Dir;

   ---------------------
   -- Get_Logs_Mapper --
   ---------------------

   function Get_Logs_Mapper
     (Handle : access Kernel_Handle_Record)
      return Basic_Mapper.File_Mapper_Access is
   begin
      return Handle.Logs_Mapper;
   end Get_Logs_Mapper;

   ---------------------
   -- Set_Logs_Mapper --
   ---------------------

   procedure Set_Logs_Mapper
     (Handle : access Kernel_Handle_Record;
      Mapper : Basic_Mapper.File_Mapper_Access) is
   begin
      Handle.Logs_Mapper := Mapper;
   end Set_Logs_Mapper;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   procedure Parse_All_LI_Information
     (Kernel    : access Kernel_Handle_Record;
      Project   : Project_Type;
      Recursive : Boolean)
   is
      Handler : constant Language_Handler :=
                  Language_Handler (Get_Language_Handler (Kernel));
      Num     : constant Natural := LI_Handlers_Count (Handler);
      LI      : LI_Handler;
      Count   : Natural := 0;

   begin
      for L in 1 .. Num loop
         LI := Get_Nth_Handler (Handler, L);

         if LI /= null then
            Count := Count + Parse_All_LI_Information (LI, Project, Recursive);
         end if;
      end loop;
   end Parse_All_LI_Information;

   -------------------
   -- Row_Activated --
   -------------------

   procedure Row_Activated (Widget : access Gtk_Widget_Record'Class) is
   begin
      Response (Gtk_Dialog (Widget), Gtk_Response_OK);
   end Row_Activated;

   -------------------------------
   -- Select_Entity_Declaration --
   -------------------------------

   procedure Select_Entity_Declaration
     (Kernel      : access Kernel_Handle_Record'Class;
      File        : Source_File;
      Entity_Name : String;
      Decl        : in out Entity_Information;
      Status      : out Entities.Queries.Find_Decl_Or_Body_Query_Status)
   is
      procedure Set
        (Tree : System.Address;
         Iter : Gtk_Tree_Iter;
         Col1 : Gint; Value1 : String;
         Col2 : Gint; Value2 : Gint;
         Col3 : Gint; Value3 : Gint);
      pragma Import (C, Set, "ada_gtk_tree_store_set_ptr_int_int");

      procedure Set2
        (Tree : System.Address;
         Iter : Gtk_Tree_Iter;
         Col1 : Gint; Value1 : String;
         Col2 : Gint; Value2 : System.Address);
      pragma Import (C, Set2, "ada_gtk_tree_store_set_ptr_ptr");

      pragma Warnings (Off);
      --  This UC is safe aliasing-wise, so kill warning
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Entity_Information);
      pragma Warnings (On);

      Column_Types : constant GType_Array :=
        (0 => GType_String,
         1 => GType_Int,
         2 => GType_Int,
         3 => GType_String,
         4 => GType_Pointer);
      Column_Names : GNAT.Strings.String_List :=
        (1 => new String'("File"),
         2 => new String'("Line"),
         3 => new String'("Column"),
         4 => new String'("Name"));

      Iter      : Entity_Iterator;
      Candidate : Entity_Information;
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
      pragma Unreferenced (Button, Col_Num);

   begin
      Find_All_Entities_In_File
        (Iter        => Iter,
         File        => File,
         Name        => Entity_Name);

      while not At_End (Iter) loop
         Count := Count + 1;
         Candidate := Get (Iter);

         if Count = 1 then
            Gtk_New (Dialog,
                     Title  => -"Select the declaration",
                     Parent => Get_Main_Window (Kernel),
                     Flags  => Modal or Destroy_With_Parent);
            Set_Default_Size (Dialog, 500, 500);

            Gtk_New (Label, -"This entity is overloaded.");
            Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

            Gtk_New (Label, -"Please select the appropriate declaration.");
            Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

            Gtk_New (Scrolled);
            Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
            Pack_Start (Get_Vbox (Dialog), Scrolled);

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
              0, Base_Name
                (Get_Filename (Get_File (Get_Declaration_Of (Candidate))))
              & ASCII.NUL,
              1, Gint (Get_Line (Get_Declaration_Of (Candidate))),
              2, Gint (Get_Column (Get_Declaration_Of (Candidate))));
         Set2 (Get_Object (Model),
               It, 3, Entity_Name & ASCII.NUL,
               4, Candidate.all'Address);

         if Candidate = Decl then
            Select_Iter (Get_Selection (View), It);
         end if;

         Next (Iter);
      end loop;

      Destroy (Iter);

      Decl := null;
      Status := Entity_Not_Found;

      if Count > 0 then
         Grab_Default (OK_Button);
         Grab_Focus (OK_Button);
         Show_All (Dialog);

         if Run (Dialog) = Gtk_Response_OK then
            Status := Success;
            Get_Selected (Get_Selection (View), Gtk_Tree_Model (Model), It);
            Decl := Convert (Get_Address (Model, It, 4));
         end if;

         Destroy (Dialog);
      end if;

      Free (Column_Names);

   exception
      when E : others => Trace (Exception_Handle, E);

         if Dialog /= null then
            Destroy (Dialog);
         end if;

         Destroy (Iter);
         raise;
   end Select_Entity_Declaration;

   ------------------------------------
   -- Find_Declaration_Or_Overloaded --
   ------------------------------------

   procedure Find_Declaration_Or_Overloaded
     (Kernel            : access Kernel_Handle_Record;
      File              : Entities.Source_File;
      Entity_Name       : String;
      Line              : Natural;
      Column            : Basic_Types.Visible_Column_Type;
      Ask_If_Overloaded : Boolean;
      Entity            : out Entities.Entity_Information;
      Closest_Ref       : out Entities.Entity_Reference;
      Status            : out Entities.Queries.Find_Decl_Or_Body_Query_Status)
   is
   begin
      Find_Declaration
        (Kernel.Database, File, Entity_Name,
         Line, Column, Entity, Closest_Ref, Status);

      --  ??? Should have the preference for the handling of fuzzy matches:
      --   - consider it as a no match: set Status to Entity_Not_Found;
      --   - consider it as overloaded entity: same as below;
      --   - use the closest match: nothing to do.

      if Ask_If_Overloaded
        and then Status = Overloaded_Entity_Found
      then
         Select_Entity_Declaration (Kernel, File, Entity_Name, Entity, Status);
      end if;
   end Find_Declaration_Or_Overloaded;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Handle : access Kernel_Handle_Record) is
      Success : Boolean;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (History_Record, History);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Root_Table'Class, Root_Table_Access);
   begin
      Save_Styles
        (Kernel_Handle (Handle),
         Create (Handle.Home_Dir.all & "styles.xml"));

      Reset_Properties (Handle);

      Save (Handle.History.all,
            Handle.Home_Dir.all & "histories.xml",
            Success);
      Free (Handle.History.all);
      Unchecked_Free (Handle.History);

      if not Success then
         Report_Preference_File_Error
           (Handle, Handle.Home_Dir.all & "histories.xml");
      end if;

      Reset (Handle.Startup_Scripts);
      Unchecked_Free (Handle.Startup_Scripts);

      Destroy_Clipboard (Handle);
      Destroy (Handle.Preferences);
      Remote_Descriptors.Finalize;

      Destroy (Handle.Registry.all);
      Unchecked_Free (Handle.Registry);

      --  Do not free the contexts. They can still be stored as Data in a
      --  Class_Instance, and this will be finalized later automatically. If
      --  we call Unref here, this results in a double deallocation.
      --  This code is left here for reference to avoid doing this error in the
      --  future.
      --        Unref (Handle.Current_Context);
      --        Unref (Handle.Last_Context_For_Contextual);

      Reset (Handle.Actions);
      Unchecked_Free (Handle.Actions);

      Reset (Handle.Action_Filters);
      Action_Filters_List.Free (Handle.All_Action_Filters);

      Reset (Handle.Styles);
      Unchecked_Free (Handle.Styles);

      Hooks_Hash.Reset (Handle.Hooks);
      Free_Tools (Handle);

      Destroy (Language_Handler (Handle.Lang_Handler));

      --  ??? Was commented out, with no explanation
      Destroy (Handle.Database);

      Free (Handle.Logs_Mapper);
      Free_Modules (Handle);
      Unref (Handle.Tooltips);

      Commands.Command_Queues.Free (Handle.Perma_Commands);

      Destroy (Handle.Scripts);

      Free (Handle.Gnatls_Cache);
      Free (Handle.Gnatls_Server);
      Free (Handle.GNAT_Version);
      Free (Handle.Home_Dir);
      Free (Handle.Prefix);
      Free (Handle.Construct_Database);

      --  Free the memory allocated by gtk+, and disconnect all the callbacks,
      --  reclaiming the associated memory.
      Trace (Me, "Destroying the GPS kernel");

      --  ??? Do not free the memory in fact, since there are some controlled
      --  types like Class_Instance which might in fact be finalized only after
      --  this point, and they might try to access the kernel. This might show
      --  up as a minor memory leak, which can safely be ignored anyway. See
      --  for instance Unref for a context below.
      --      Unref (Handle);

      Kernel_Desktop.Free_Registered_Desktop_Functions;
   end Destroy;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Handle : access Kernel_Handle_Record) return Histories.History is
   begin
      return Handle.History;
   end Get_History;

   --------------------
   -- Add_To_History --
   --------------------

   procedure Add_To_History
     (Handle    : access Kernel_Handle_Record;
      Key       : Histories.History_Key;
      New_Entry : String) is
   begin
      Add_To_History (Handle.History.all, Key, New_Entry);
   end Add_To_History;

   ----------------------
   -- Bind_Default_Key --
   ----------------------

   procedure Bind_Default_Key
     (Kernel      : access Kernel_Handle_Record;
      Action      : String;
      Default_Key : String)
   is
      Err : constant String := Add_Customization_String
        (Kernel, "<key action=""" & Action & """>" & Default_Key & "</key>",
         From_File => "<gps_internal>");
      pragma Unreferenced (Err);
   begin
      null;
   end Bind_Default_Key;

   ------------------------------
   -- Get_Current_Focus_Widget --
   ------------------------------

   function Get_Current_Focus_Widget
     (Kernel : access Kernel_Handle_Record) return Gtk.Widget.Gtk_Widget
   is
      use Widget_List;
      W, W2       : Gtk_Widget;
      Toplevel    : Gtk_Window;
      List, List2 : Widget_List.Glist;
   begin
      --  First check if a window currently has a grab

      W := Grab_Get_Current;
      if W /= null then
         Toplevel := Gtk_Window (Get_Toplevel (W));
         W := Get_Focus (Toplevel);
      end if;

      --  Then check all toplevel windows and stop at the one that has
      --  the focus.

      if W = null then
         List := List_Toplevels;
         List2 := First (List);

         while List2 /= Widget_List.Null_List loop
            Toplevel := Gtk_Window (Get_Data (List2));

            if Get_Property (Toplevel, Has_Toplevel_Focus_Property) then
               W := Get_Focus (Toplevel);
               if W /= null and then Has_Focus_Is_Set (W) then
                  exit;
               end if;
               W := null;
            end if;

            List2 := Next (List2);
         end loop;

         Free (List);
      end if;

      --  If still no one has the focus, then no window in GPS currently has
      --  it. In this case, we assume that would be the main GPS window unless
      --  a floating child last had the focus. In particular, this is used when
      --  a Command_Window was used, then closed just before calling the
      --  on_activate user callback. Since the gtk+ main loop hasn't been
      --  called in between, the focus has not been transfered by the window
      --  manager yet.
      if W = null then
         declare
            Iter : constant Child_Iterator := First_Child (Get_MDI (Kernel));
         begin
            if Get (Iter) /= null
              and then Is_Floating (Get (Iter))
            then
               --  The toplevel widget is not necessarily a GtkWindow. In some
               --  cases, for instance, it will be a Editor_Child_Record, when
               --  the editor is floating (since in that case the MDI_Child is
               --  detached from the MDI, and its own child is put in a
               --  toplevel window.

               W := Get_Toplevel (Get_Widget (Get (Iter)));
               W := Get_Focus (Gtk_Window (W));
            else
               W := Get_Focus (Get_Main_Window (Kernel));
            end if;
         end;
      end if;

      if W /= null then
         W2 := W;

         while W2 /= null and then W2.all in Gtk_Container_Record'Class loop
            W  := W2;
            W2 := Get_Focus_Child (Gtk_Container (W));
         end loop;

         if W2 /= null then
            W := W2;
         end if;

         if W.all in Gtk_Combo_Record'Class then
            W := Gtk_Widget (Get_Entry (Gtk_Combo (W)));
         end if;
      end if;

      return W;
   end Get_Current_Focus_Widget;

   -------------------
   -- Lookup_Filter --
   -------------------

   function Lookup_Filter
     (Kernel : access Kernel_Handle_Record;
      Name   : String) return Action_Filter is
   begin
      return Get (Kernel.Action_Filters, Name);
   end Lookup_Filter;

   -----------
   -- Start --
   -----------

   function Start (Kernel : access Kernel_Handle_Record'Class)
      return Action_Filter_Iterator
   is
      Iter : Action_Filter_Iterator;
   begin
      Get_First (Kernel.Action_Filters, Iter.Iterator);
      return Iter;
   end Start;

   ----------
   -- Free --
   ----------

   procedure Free (Filter : in out Action_Filter_Record) is
   begin
      Free (Filter.Error_Msg);
      Free (Filter.Name);
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Filter : in out Base_Action_Filter_Record) is
   begin
      case Filter.Kind is
         when Standard_Filter =>
            Free (Filter.Language);
            Free (Filter.Shell);
            Free (Filter.Shell_Lang);
            Free (Filter.Module);
         when Filter_And | Filter_Or | Filter_Not =>
            --  Not need to free anything here: as per Register_Filter, the
            --  subfilters have already been registered if Filter was, and thus
            --  they will be freed in turn. If Filter was not registered, we
            --  have a memory leaks any so things do not really matter
            null;
      end case;
      Free (Action_Filter_Record (Filter));
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Filter : in out Action_Filter) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Action_Filter_Record'Class, Action_Filter);
   begin
      if Filter /= null then
         Free (Filter.all);
         Unchecked_Free (Filter);
      end if;
   end Free;

   ----------
   -- Next --
   ----------

   procedure Next
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : in out Action_Filter_Iterator) is
   begin
      Get_Next (Kernel.Action_Filters, Iter.Iterator);
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Action_Filter_Iterator) return Action_Filter is
   begin
      return Get_Element (Iter.Iterator);
   end Get;

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Filter : access Action_Filter_Record;
      Name   : String) is
   begin
      --  We can't rename an already named filter, since the hash table would
      --  not work correctly anymore

      Assert (Me, Name = ""
              or else Filter.Name = null or else Filter.Name.all = "",
              "A named filter is being renamed");

      if Name /= "" then
         Free (Filter.Name);
         Filter.Name := new String'(Name);
         Set (Kernel.Action_Filters, Name, Action_Filter (Filter));
      end if;

      if not Filter.Registered then
         Action_Filters_List.Append
           (Kernel.All_Action_Filters, Action_Filter (Filter));
         Filter.Registered := True;
      end if;
   end Register_Filter;

   ---------------------
   -- Register_Filter --
   ---------------------

   overriding procedure Register_Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Filter : access Base_Action_Filter_Record;
      Name   : String) is
   begin
      Register_Filter (Kernel, Action_Filter_Record (Filter.all)'Access, Name);
      case Filter.Kind is
         when Standard_Filter =>
            null;

         when Filter_And =>
            Register_Filter (Kernel, Filter.And1, "");
            Register_Filter (Kernel, Filter.And2, "");

         when Filter_Or =>
            Register_Filter (Kernel, Filter.Or1, "");
            Register_Filter (Kernel, Filter.Or2, "");

         when Filter_Not =>
            Register_Filter (Kernel, Filter.Not1, "");
      end case;
   end Register_Filter;

   ------------
   -- Create --
   ------------

   function Create
     (Name            : Glib.UTF8_String;
      Kernel          : access Kernel_Handle_Record;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return GNATCOLL.VFS.Virtual_File is
   begin
      return Projects.Registry.Create
        (Name, Get_Registry (Kernel).all, Use_Source_Path, Use_Object_Path);
   end Create;

   ----------------------
   -- Create_From_Base --
   ----------------------

   function Create_From_Base
     (Name   : Glib.UTF8_String;
      Kernel : access Kernel_Handle_Record) return GNATCOLL.VFS.Virtual_File
   is
      Full : constant String := Get_Full_Path_From_File
        (Registry        => Get_Registry (Kernel).all,
         Filename        => Base_Name (Name),
         Use_Source_Path => True,
         Use_Object_Path => True);

   begin
      if Full = "" then
         return Create (Full_Filename => Name);
      else
         return Create (Full_Filename => Full);
      end if;
   end Create_From_Base;

   ----------
   -- Free --
   ----------

   procedure Free (Tool : in out Tool_Properties_Record) is
   begin
      Free (Tool.Tool_Name);
      Free (Tool.Project_Package);
      Free (Tool.Project_Attribute);
      Free (Tool.Project_Index);
      Free (Tool.Initial_Cmd_Line);
      Free (Tool.Languages);
      Free (Tool.Config);
   end Free;

   ----------------
   -- Free_Tools --
   ----------------

   procedure Free_Tools (Kernel : access Kernel_Handle_Record'Class) is
      use Tools_List;
      Cursor : Tools_List.Cursor := First (Kernel.Tools);
      Tool   : Tool_Properties_Record;
   begin
      while Has_Element (Cursor) loop
         Tool := Element (Cursor);
         Free (Tool);
         Next (Cursor);
      end loop;
      Clear (Kernel.Tools);
   end Free_Tools;

   -------------------
   -- Register_Tool --
   -------------------

   procedure Register_Tool
     (Kernel : access Kernel_Handle_Record;
      Tool   : Tool_Properties_Record)
   is
      Pkg  : Package_Node_Id;
      Attr : Attribute_Node_Id;
      Elm  : Tool_Properties_Record;
      Iter : Tools_List.Cursor;
      use Tools_List;
   begin
      Name_Len := Tool.Project_Package'Length;
      Name_Buffer (1 .. Name_Len) := To_Lower (Tool.Project_Package.all);
      Pkg := Package_Node_Id_Of (Name_Find);

      if Pkg = Empty_Package then
         Register_New_Package (Tool.Project_Package.all, Pkg);
      end if;

      Name_Len := Tool.Project_Attribute'Length;
      Name_Buffer (1 .. Name_Len) := To_Lower (Tool.Project_Attribute.all);
      Attr := Attribute_Node_Id_Of
        (Name  => Name_Find, Starting_At => First_Attribute_Of (Pkg));

      if Attr = Empty_Attribute then
         if Tool.Project_Index.all = "" then
            Register_New_Attribute
              (Name       => To_Lower (Tool.Project_Attribute.all),
               In_Package => Pkg,
               Attr_Kind  => Prj.Attr.Single,
               Var_Kind   => Prj.List);
         else
            Register_New_Attribute
              (Name       => To_Lower (Tool.Project_Attribute.all),
               In_Package => Pkg,
               Attr_Kind  => Prj.Attr.Associative_Array,
               Var_Kind   => Prj.List);
         end if;
      end if;

      Iter := First (Kernel.Tools);
      while Has_Element (Iter) loop
         Elm := Element (Iter);

         if Elm.Project_Index.all = Tool.Project_Index.all
           and then Elm.Project_Package.all = Tool.Project_Package.all
           and then Elm.Project_Attribute.all = Tool.Project_Attribute.all
         then
            Tools_List.Replace_Element (Kernel.Tools, Iter, Tool);
            Free (Elm);

            if not Tool.Override then
               Insert (Kernel,
                       Text   =>  -"Warning: tool " & Tool.Tool_Name.all &
                                 (-" is defined twice"),
                       Add_LF => True,
                       Mode   => Error);
            end if;

            return;
         end if;

         Next (Iter);
      end loop;

      Tools_List.Append (Kernel.Tools, Tool);
   end Register_Tool;

   -------------------
   -- Get_All_Tools --
   -------------------

   function Get_All_Tools
     (Kernel : access Kernel_Handle_Record) return Tool_Properties_Array
   is
      use Tools_List;
      Iter   : Tools_List.Cursor := First (Kernel.Tools);
      Result : Tool_Properties_Array (1 .. Integer (Length (Kernel.Tools)));
      Count  : Natural := Result'First;
   begin
      while Has_Element (Iter) loop
         Result (Count) := Element (Iter);
         Count := Count + 1;
         Next (Iter);
      end loop;
      return Result;
   end Get_All_Tools;

   -------------------------
   -- Get_Tool_Properties --
   -------------------------

   function Get_Tool_Properties
     (Kernel    : access Kernel_Handle_Record;
      Tool_Name : String) return Tool_Properties_Record
   is
      use Tools_List;
      Iter : Tools_List.Cursor := First (Kernel.Tools);
   begin
      while Has_Element (Iter) loop
         if To_Lower (Element (Iter).Tool_Name.all) = To_Lower (Tool_Name) then
            return Element (Iter);
         end if;
         Next (Iter);
      end loop;
      return No_Tool;
   end Get_Tool_Properties;

   ------------
   -- Create --
   ------------

   function Create
     (Language   : String := "";
      Shell      : String := "";
      Shell_Lang : String := "Shell";
      Module     : String := "") return Action_Filter
   is
      F : constant Base_Action_Filter :=
        new Base_Action_Filter_Record (Standard_Filter);
   begin
      if Language /= "" then
         F.Language := new String'(Language);
      end if;

      if Shell /= "" then
         F.Shell := new String'(Shell);
         F.Shell_Lang := new String'(Shell_Lang);
      end if;

      if Module /= "" then
         F.Module := new String'(Module);
      end if;

      return Action_Filter (F);
   end Create;

   -----------
   -- "and" --
   -----------

   function "and"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Action_Filter is
   begin
      return new Base_Action_Filter_Record'
        (Kind => Filter_And, Error_Msg => null, Name => null,
         Registered => False,
         And1 => Action_Filter (Filter1), And2 => Action_Filter (Filter2));
   end "and";

   ----------
   -- "or" --
   ----------

   function "or"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Action_Filter is
   begin
      return new Base_Action_Filter_Record'
        (Kind => Filter_Or, Error_Msg => null, Name => null,
         Registered => False,
         Or1  => Action_Filter (Filter1), Or2 => Action_Filter (Filter2));
   end "or";

   -----------
   -- "not" --
   -----------

   function "not"
     (Filter : access Action_Filter_Record'Class) return Action_Filter is
   begin
      return new Base_Action_Filter_Record'
        (Kind => Filter_Not, Error_Msg => null, Name => null,
         Registered => False,
         Not1  => Action_Filter (Filter));
   end "not";

   -----------------------
   -- Set_Error_Message --
   -----------------------

   procedure Set_Error_Message (Filter : Action_Filter; Msg : String) is
   begin
      Free (Filter.Error_Msg);
      Filter.Error_Msg := new String'(Msg);
   end Set_Error_Message;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message (Filter : Action_Filter) return String is
   begin
      if Filter /= null and then Filter.Error_Msg /= null then
         return Filter.Error_Msg.all;
      else
         return "";
      end if;
   end Get_Error_Message;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Filter : Action_Filter) return String is
   begin
      if Filter /= null and then Filter.Name /= null then
         return Filter.Name.all;
      else
         return "";
      end if;
   end Get_Name;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Base_Action_Filter_Record;
      Context : Selection_Context) return Boolean
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Result : Boolean := True;
   begin
      case Filter.Kind is
         when Standard_Filter =>
            if Filter.Language /= null then
               if not Has_File_Information (Context)
                 or else GNATCOLL.VFS.No_File = File_Information (Context)
               then
                  Result := False;

               else
                  declare
                     Lang : constant String := Get_Language_From_File
                       (Get_Language_Handler (Kernel),
                        File_Information (Context));
                  begin
                     if not Equal (Lang, Filter.Language.all, False) then
                        Result := False;
                     end if;
                  end;
               end if;
            end if;

            if Result
              and then Filter.Module /= null
              and then (Get_Creator (Context) = null
                        or else not Equal
                          (Module_Name (Module_ID (Get_Creator (Context))),
                           Filter.Module.all,
                           False))
            then
               Result := False;
            end if;

            if Result and then Filter.Shell /= null then
               declare
                  Lang : constant Scripting_Language :=
                           Lookup_Scripting_Language
                             (Kernel.Scripts, Filter.Shell_Lang.all);

                  function Substitution
                    (Param  : String; Quoted : Boolean) return String;
                  --  Local substitution of special chars

                  ------------------
                  -- Substitution --
                  ------------------

                  function Substitution
                    (Param : String; Quoted : Boolean) return String
                  is
                     Done : aliased Boolean := False;
                  begin
                     return GPS.Kernel.Macros.Substitute
                       (Param, Context, Quoted, Done'Access);
                  end Substitution;

                  Cmd : constant String := Substitute
                    (Str       => Filter.Shell.all,
                     Delimiter => GPS.Kernel.Macros.Special_Character,
                     Callback  => Substitution'Unrestricted_Access);

               begin
                  if Lang = null then
                     Result := False;

                  else
                     declare
                        Errors : aliased Boolean;
                        R      : constant Boolean :=
                         GNATCOLL.Scripts.Execute_Command
                            (Lang,
                             Cmd,
                             Hide_Output => True,
                             Errors => Errors'Unchecked_Access);
                     begin
                        Result := not Errors and then R;
                     end;
                  end if;
               end;
            end if;

            return Result;

         when Filter_And =>
            return Filter_Matches (Filter.And1, Context)
              and then Filter_Matches (Filter.And2, Context);

         when Filter_Or =>
            return Filter_Matches (Filter.Or1, Context)
              or else Filter_Matches (Filter.Or2, Context);

         when Filter_Not =>
            return not Filter_Matches (Filter.Not1, Context);
      end case;
   end Filter_Matches_Primitive;

   --------------------
   -- Filter_Matches --
   --------------------

   function Filter_Matches
     (Filter  : Action_Filter;
      Context : Selection_Context) return Boolean is
   begin
      return Filter = null
        or else Filter_Matches_Primitive (Filter, Context);
   end Filter_Matches;

   ----------
   -- Free --
   ----------

   procedure Free (Hook : in out Hook_Description_Base) is
      pragma Unreferenced (Hook);
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (L : in out Hook_Description_Base_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Hook_Description_Base'Class, Hook_Description_Base_Access);
   begin
      if L /= null then
         Free (L.all);
         Unchecked_Free (L);
      end if;
   end Free;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Hook : in out Hook_Function_Record) is
      pragma Unreferenced (Hook);
   begin
      null;
   end Destroy;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Hook : Hook_Function_Record) return String is
      pragma Unreferenced (Hook);
   begin
      return -"<internal>";
   end Get_Name;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Marker : in out Location_Marker_Record) is
      pragma Unreferenced (Marker);
   begin
      null;
   end Destroy;

   ----------------------------
   -- Push_Marker_In_History --
   ----------------------------

   procedure Push_Marker_In_History
     (Kernel : access Kernel_Handle_Record'Class;
      Marker : access Location_Marker_Record'Class)
   is
      Data : aliased Marker_Hooks_Args :=
               (Hooks_Data with Marker => Location_Marker (Marker));
   begin
      Run_Hook (Kernel,
                Marker_Added_In_History_Hook,
                Data'Unchecked_Access,
                Set_Busy => False);
   end Push_Marker_In_History;

   ----------------------------
   -- Register_Perma_Command --
   ----------------------------

   procedure Register_Perma_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Commands.Root_Command'Class)
   is
      use Commands.Command_Queues, Commands;
      L : List_Node := First (Kernel.Perma_Commands);
   begin
      while L /= Null_Node loop
         if Data (L) = Commands.Command_Access (Command) then
            return;  --  Already in list, nothing to do
         end if;

         L := Next (L);
      end loop;

      --  Command is not in list: we steal a reference to it

      Commands.Command_Queues.Append
        (Kernel.Perma_Commands, Commands.Command_Access (Command));
   end Register_Perma_Command;

end GPS.Kernel;
