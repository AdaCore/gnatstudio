------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Tags;                  use Ada.Tags;
with Ada.Unchecked_Conversion;
with System.Address_Image;

pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Tribooleans;      use GNATCOLL.Tribooleans;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with System;                    use System;

with Gdk;                       use Gdk;
with Gdk.Window;                use Gdk.Window;

with Glib.Object;               use Glib.Object;
with Glib.Main;                 use Glib.Main;
with XML_Utils;                 use XML_Utils;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.MDI;                use Gtkada.MDI;

with Basic_Mapper;              use Basic_Mapper;
with Basic_Types;               use Basic_Types;
with Default_Preferences;       use Default_Preferences;
with GPS.Intl;                  use GPS.Intl;
with GPS.Editors;               use GPS.Editors;
with GPS.Default_Styles;        use GPS.Default_Styles;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Clipboard;      use GPS.Kernel.Clipboard;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Custom;         use GPS.Kernel.Custom;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Macros;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Messages;       use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Properties;     use GPS.Kernel.Properties;
with GPS.Kernel.Style_Manager;  use GPS.Kernel.Style_Manager;
with GPS.Kernel.Scripts.Hooks;
with GPS.Kernel.Xref;           use GPS.Kernel.Xref;
with GPS.Properties;            use GPS.Properties;
with GPS.VCS;                   use GPS.VCS;
with Histories;                 use Histories;
with Language_Handlers;         use Language_Handlers;
with Language.Tree.Database;    use Language.Tree.Database;
with GPR;                       use GPR;
with GPR.Names;                 use GPR.Names;
with GPR.Attr;                  use GPR.Attr;
with Projects;                  use Projects;
with Refactoring;               use Refactoring;
with String_List_Utils;         use String_List_Utils;
with Switches_Chooser;          use Switches_Chooser;
with Xref;                      use Xref;
with Language.Abstract_Construct_Tree; use Language.Abstract_Construct_Tree;

package body GPS.Kernel is

   Me        : constant Trace_Handle := Create ("gps_kernel");
   Me_Filters : constant Trace_Handle :=
      Create ("FILTERS", GNATCOLL.Traces.Off);
   Create_Me : constant Trace_Handle :=
      Create ("Contexts.Mem", GNATCOLL.Traces.Off);
   Me_Hooks  : constant Trace_Handle := Create ("HOOKS", GNATCOLL.Traces.Off);

   History_Max_Length : constant Positive := 10;
   --  <preferences> Maximum number of entries to store in each history

   Build_Mode_Property : constant String := "Build-Mode";
   --  The name of a GPS.Properties to store the current build mode. Use
   --  Get_Build_Mode below instead

   use Action_Filters_Maps;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Kernel_Handle);
   function Convert is new Ada.Unchecked_Conversion
     (Kernel_Handle, System.Address);

   procedure Free (Tool : in out Tool_Properties);
   procedure Free_Tools (Kernel : access Kernel_Handle_Record'Class);
   --  Free the list of registered tools

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences change

   type On_File_Closed is new File_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_File_Closed;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : Virtual_File);
   --  Called when a file is closed

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_File_Edited;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : Virtual_File);
   --  Called when a file is opened

   type GPS_Refactoring_Factory_Context
     is new Refactoring.Factory_Context_Record with record
      Kernel : Kernel_Handle;
   end record;

   overriding procedure Report_Error
     (Self : access GPS_Refactoring_Factory_Context;
      Msg  : String);
   overriding procedure Report_Location
     (Self     : access GPS_Refactoring_Factory_Context;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Basic_Types.Visible_Column_Type := 1;
      Text     : String);

   procedure On_Main_Window_Destroyed (Data, Self : System.Address)
     with Convention => C;
   --  Called when the main window is destroyed.
   --  We unfortunately cannot reuse the mechanics from GtkApplication, since
   --  the window is unregistered very early (gtk_window_destroy), before its
   --  children are destroyed. As a result, plugins like filepos.py can no
   --  longer access the MDI if the main window is found via Get_Window_By_Id.

   procedure Internal_Add_Hook_Func
     (Self  : in out Hook_Types'Class;
      List  : in out Hook_Func_Lists.List;
      Func  : not null access Hook_Function'Class;
      Last  : Boolean := True;
      Watch : access Glib.Object.GObject_Record'Class := null);
   --  Add a new callback to the specified list

   function Remove
     (List       : in out Hook_Func_Lists.List;
      If_Matches : not null access function
        (F : not null access Hook_Function'Class) return Boolean;
      Hook_Name : String)
      return Boolean;
   --  Remove the first attached function for which the function returns True.
   --  Return True if function has been removed.

   -----------
   -- Hooks --
   -----------

   procedure Remove_Hook_Cb
      (Data : System.Address;
       Obj  : System.Address);
   pragma Convention (C, Remove_Hook_Cb);
   --  Called when an object is destroyed, to disconnect hook functions
   --  that depended on it.

   type Hook_User_Data is record
      Hook : not null access Hook_Types'Class;
      Func : not null access Hook_Function'Class;
   end record;
   type Hook_User_Data_Access is access all Hook_User_Data;
   pragma Convention (C, Hook_User_Data_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Hook_User_Data, Hook_User_Data_Access);
   function Convert is new Ada.Unchecked_Conversion
      (System.Address, Hook_User_Data_Access);

   ------------------
   -- Report_Error --
   ------------------

   overriding procedure Report_Error
     (Self : access GPS_Refactoring_Factory_Context;
      Msg  : String) is
   begin
      Insert (Self.Kernel, Msg, Mode => Error);
   end Report_Error;

   ---------------------
   -- Report_Location --
   ---------------------

   overriding procedure Report_Location
     (Self     : access GPS_Refactoring_Factory_Context;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Basic_Types.Visible_Column_Type := 1;
      Text     : String)
   is
   begin
      Create_Simple_Message
        (Get_Messages_Container (Self.Kernel),
         Category, File, Line, Column, Text,
         0, (Editor_Side => True, Locations => True, Editor_Line => False));
   end Report_Location;

   --------------------------
   -- Get_Language_Handler --
   --------------------------

   function Get_Language_Handler
     (Handle : access Kernel_Handle_Record)
      return Language_Handlers.Language_Handler is
   begin
      return Handle.Lang_Handler;
   end Get_Language_Handler;

   ----------
   -- Hash --
   ----------

   function Hash
     (Element : Commands.Command_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (System.Address_Image (Element.all'Address));
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
      if Handle.GNAT_Version = Null_Unbounded_String then
         return -"<unknown version>";
      else
         return To_String (Handle.GNAT_Version);
      end if;
   end GNAT_Version;

   -----------------------
   -- Require_GNAT_Date --
   -----------------------

   function Require_GNAT_Date
     (Handle : access Kernel_Handle_Record;
      Date   : Date_Type) return Boolean
   is
      Version       : constant String := GNAT_Version (Handle);
      Open_Index    : constant Natural := Index (Version, "(");
      Close_Index   : Natural;
      Compiler_Date : Date_Type;

   begin
      if Open_Index = 0 then
         return False;
      else
         Close_Index := Index (Version (Open_Index + 1 .. Version'Last), "-");

         if Close_Index = 0 then
            Close_Index :=
              Index (Version (Open_Index + 1 .. Version'Last), ")");
         end if;

         if Close_Index = 0 then
            return False;
         else
            Compiler_Date :=
              (Year  => Integer'Value
                 (Version (Open_Index + 1 .. Open_Index + 4)),
               Month => Integer'Value
                 (Version (Open_Index + 5 .. Open_Index + 6)),
               Day   => Integer'Value
                 (Version (Open_Index + 7 .. Open_Index + 8)));

            return Compiler_Date >= Date;
         end if;
      end if;
   exception
      when E : Constraint_Error =>
         --  There has been an error in the date recovery, return false

         Trace (Me, E);

         return False;
   end Require_GNAT_Date;

   ---------------------
   -- Create_Registry --
   ---------------------

   overriding procedure Create_Registry
     (Self   : not null access Kernel_Handle_Record;
      Result : out Projects.Project_Registry_Access) is
   begin
      GPS.Kernel.Project.Create_Registry (Self, Result);
      --  Note: we do not compute the view of this project yet. This will be
      --  done only if no other project was loaded from the command line, which
      --  is more efficient in case the current directory has lots of source
      --  files.

   end Create_Registry;

   ---------------------
   -- Create_Database --
   ---------------------

   overriding procedure Create_Database
     (Self   : not null access Kernel_Handle_Record;
      Result : out Standard.Xref.General_Xref_Database) is
   begin
      GPS.Kernel.Xref.Create_Database (Self, Result);
   end Create_Database;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Handle           : out Kernel_Handle;
      Application      : not null access Gtk_Application_Record'Class;
      Home_Dir         : Virtual_File;
      Prefix_Directory : Virtual_File)
   is
      P : access On_Pref_Changed;
   begin
      Handle := new Kernel_Handle_Record;
      Handle.Home_Dir := Home_Dir;
      Handle.Prefix   := Prefix_Directory;
      Handle.Launcher.Kernel := GPS.Core_Kernels.Core_Kernel (Handle);
      Handle.Env := new GPS.Environments.Environment_Record;
      Handle.Application := Application;

      GPS.Core_Kernels.Initialize (Handle);

      --  Initialize the preferences. We load the file now, even though it
      --  will also be reloaded after the customization files, so that themes
      --  do not override user's preferences.
      --  We need to load now so that for instance the splash screen is
      --  correctly taken into account.
      Handle.Preferences := new GPS_Preferences_Manager_Record;
      GPS_Preferences_Manager_Record (Handle.Preferences.all).
        Set_Kernel (Handle);

      declare
         Style_Manager : Style_Manager_Access;
      begin
         Style_Manager := new Style_Manager_Record;
         Set_Style_Manager (Handle, Style_Manager);
      end;

      Register_Global_Preferences (Handle);
      Load_Preferences (Handle);

      Initialize_Style_Manager (Handle);
      GPS.Default_Styles.Initialize_Default_Styles (Handle);

      --  Create the message container
      Handle.Messages_Container := Create_Messages_Container (Handle);

      Handle.History := new History_Record;
      Trace (Me, "Loading histories.xml");
      Load (Handle.History.all,
            Create_From_Dir (Handle.Home_Dir, "histories.xml"));
      Set_Max_Length (Handle.History.all, History_Max_Length);

      GPS.Properties.Set_Writer (Open_Persistent_Properties_DB (Handle));

      Create_Clipboard (Handle);

      Handle.Construct_Tree :=
        Language.Abstract_Construct_Tree.Create (Handle);

      P := new On_Pref_Changed;
      Preferences_Changed_Hook.Add (P);
      P.Execute (Handle, null);

      File_Closed_Hook.Add (new On_File_Closed);
      File_Edited_Hook.Add (new On_File_Edited);
   end Gtk_New;

   ------------------------------
   -- On_Main_Window_Destroyed --
   ------------------------------

   procedure On_Main_Window_Destroyed (Data, Self : System.Address) is
      Kernel : constant Kernel_Handle := Convert (Data);
      pragma Unreferenced (Self);
   begin
      Kernel.Main_Window := null;
   end On_Main_Window_Destroyed;

   ---------------------
   -- Set_Main_Window --
   ---------------------

   procedure Set_Main_Window
     (Self : not null access Kernel_Handle_Record;
      Win  : not null access Gtk.Window.Gtk_Window_Record'Class) is
   begin
      Self.Main_Window := Win;
      Win.Weak_Ref (On_Main_Window_Destroyed'Access, Convert (Self));
   end Set_Main_Window;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
   begin
      if Pref = null
        or else Pref = Preference (Hidden_Files_Pattern)
      then
         if Kernel.Hidden_File_Matcher /= null then
            Unchecked_Free (Kernel.Hidden_File_Matcher);
         end if;

         declare
            Pattern : constant String := Hidden_Files_Pattern.Get_Pref;
         begin
            if Pattern /= "" then
               Kernel.Hidden_File_Matcher :=
                 new Pattern_Matcher'(Compile (Pattern));
            end if;
         end;
      end if;

      if Pref = null
        or else Pref = Preference (GPS.Kernel.Preferences.Trusted_Mode)
      then
         Get_Registry (Kernel).Environment.Set_Trusted_Mode
           (GPS.Kernel.Preferences.Trusted_Mode.Get_Pref);
      end if;
   end Execute;

   ----------------------
   -- Preferences_File --
   ----------------------

   function Preferences_File
     (Self : access Kernel_Handle_Record)
      return GNATCOLL.VFS.Virtual_File
   is
   begin
      return Create_From_Dir (Self.Home_Dir, "preferences.xml");
   end Preferences_File;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences (Handle : access Kernel_Handle_Record) is
   begin
      Load_Preferences (Handle.Preferences, Handle.Preferences_File);
   end Load_Preferences;

   ---------------------
   -- Get_Preferences --
   ---------------------

   function Get_Preferences
     (Handle : access Kernel_Handle_Record)
      return Default_Preferences.Preferences_Manager is
   begin
      return Handle.Preferences;
   end Get_Preferences;

   -------------
   -- Set_VCS --
   -------------

   procedure Set_VCS
      (Self : not null access Kernel_Handle_Record;
       Repo : not null access GPS.VCS.Abstract_VCS_Repository'Class) is
   begin
      Self.VCS := GPS.VCS.Abstract_VCS_Repository_Access (Repo);
   end Set_VCS;

   ---------
   -- VCS --
   ---------

   function VCS
      (Self : not null access Kernel_Handle_Record)
      return access GPS.VCS.Abstract_VCS_Repository'Class is
   begin
      return Self.VCS;
   end VCS;

   ------------------------------------
   -- Default_Language_Tree_Provider --
   ------------------------------------

   overriding function Default_Language_Tree_Provider
     (Kernel : not null access Kernel_Handle_Record)
      return Semantic_Tree_Provider_Access is
   begin
      return Kernel.Construct_Tree;
   end Default_Language_Tree_Provider;

   ------------------------
   -- Get_Buffer_Factory --
   ------------------------

   overriding function Get_Buffer_Factory
     (Kernel : not null access Kernel_Handle_Record)
      return Editor_Buffer_Factory_Access
   is
   begin
      return Kernel.Editor_Factory;
   end Get_Buffer_Factory;

   ------------------------
   -- Set_Buffer_Factory --
   ------------------------

   procedure Set_Buffer_Factory
     (Kernel  : access Kernel_Handle_Record;
      Factory : Editor_Buffer_Factory_Access) is
   begin
      Kernel.Editor_Factory := Factory;
   end Set_Buffer_Factory;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_File_Edited;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : Virtual_File)
   is
      pragma Unreferenced (Self);
   begin
      Kernel.Open_Files.Include (File);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_File_Closed;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : Virtual_File)
   is
      pragma Unreferenced (Self);
   begin
      if Kernel.Open_Files.Contains (File) then
         Kernel.Open_Files.Delete (File);
      else
         Trace (Me, "file_closed on a file not registered as open: "
             & File.Display_Full_Name);
      end if;
   end Execute;

   -------------
   -- Is_Open --
   -------------

   function Is_Open
     (Kernel   : access Kernel_Handle_Record;
      Filename : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return Kernel.Open_Files.Contains (Filename);
   end Is_Open;

   ----------------
   -- Open_Files --
   ----------------

   function Open_Files
     (Kernel : access Kernel_Handle_Record) return access File_Sets.Set is
   begin
      return Kernel.Open_Files'Access;
   end Open_Files;

   ---------------
   -- Is_Hidden --
   ---------------

   function Is_Hidden
     (Kernel    : access Kernel_Handle_Record;
      File      : GNATCOLL.VFS.Virtual_File) return Boolean
   is
   begin
      return not Show_Hidden_Files.Get_Pref
        and then Kernel.Hidden_File_Matcher /= null
        and then Match (Kernel.Hidden_File_Matcher.all, +File.Base_Dir_Name);
   end Is_Hidden;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed
     (Handle  : access Kernel_Handle_Record;
      Context : Selection_Context) is
   begin
      Handle.Current_Context := Context;
      Context_Changed_Hook.Run (Handle, Context);
   end Context_Changed;

   ----------------------------------
   -- Report_Preference_File_Error --
   ----------------------------------

   procedure Report_Preference_File_Error
     (Handle   : access Kernel_Handle_Record;
      Filename : Virtual_File)
   is
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);
   begin
      if Is_In_Destruction (Handle) then
         Button := Message_Dialog
           ((-"Could not save the configuration file ") &
            Filename.Display_Full_Name & ASCII.LF &
            (-"Please verify that you have write access to this file."),
            Error, Button_OK, Justification => Justify_Left,
            Parent => Handle.Get_Main_Window);
      else
         Handle.Insert
           ((-"Could not save the configuration file ") &
            Filename.Display_Full_Name & ASCII.LF &
            (-"Please verify that you have write access to this file."),
            Mode => Error);
      end if;
   end Report_Preference_File_Error;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Selection_Context_Data_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Addresses_Array, Addresses_Array_Access);
   begin
      if Active (Create_Me) then
         Trace (Create_Me, "Freeing context: 0x"
                & System.Address_Image (Self'Address));
      end if;

      --  Do not unref the entity stored in the context if the kernel is in
      --  destruction or as already been destroyed since the entity has
      --  already been freed as part of the kernel destruction.

      if Self.Kernel /= null and then not Self.Kernel.Is_In_Destruction then
         --   ??? problem of double deallocation at shutdown time, ideally
         --   the following call should be outside of the conditional.
         GNATCOLL.VFS.Unchecked_Free (Self.Files);
         Unchecked_Free (Self.Messages);
      end if;

      Self.Instances.Free;
   end Free;

   -------------------------
   -- Get_Current_Context --
   -------------------------

   function Get_Current_Context
     (Kernel : access Kernel_Handle_Record'Class) return Selection_Context is
   begin
      if Kernel.Current_Context = No_Context then
         Kernel.Current_Context := New_Context (Kernel);
      end if;

      Context_Changed_Hook.Force_Debounce (Kernel, Kernel.Current_Context);

      return Kernel.Current_Context;
   end Get_Current_Context;

   ---------------------
   -- Refresh_Context --
   ---------------------

   procedure Refresh_Context
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Child : MDI_Child;
   begin
      if not Kernel.Is_In_Destruction then
         Child := Get_MDI (Kernel).Get_Focus_Child;

         if Active (Me) then
            if Child /= null then
               Trace (Me, "Refresh_Context " & Child.Get_Title);
            else
               Trace (Me, "Refresh_Context no child");
            end if;
         end if;

         if Child /= null
           and then Child.all in GPS_MDI_Child_Record'Class
         then
            Kernel.Context_Changed (GPS_MDI_Child (Child).Build_Context);
         else
            Kernel.Context_Changed (New_Context (Kernel));
         end if;
      end if;
   end Refresh_Context;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Context : Selection_Context) return Kernel_Handle is
   begin
      if Context.Ref.Is_Null then
         return null;
      else
         return Context.Ref.Get.Kernel;
      end if;
   end Get_Kernel;

   -----------------
   -- Get_Creator --
   -----------------

   function Get_Creator
     (Context : Selection_Context) return Abstract_Module_ID is
   begin
      if Context.Ref.Is_Null then
         return null;
      else
         return Context.Ref.Get.Creator;
      end if;
   end Get_Creator;

   -----------------
   -- New_Context --
   -----------------

   function New_Context
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Creator : access Abstract_Module_Record'Class := null)
      return Selection_Context
   is
      Context : Selection_Context;
   begin
      Context.Ref.Set (Selection_Context_Data_Record'(
         Kernel  => Kernel_Handle (Kernel),
         Creator => Abstract_Module (Creator),
         others  => <>));

      if Active (Create_Me) then
         Trace (Create_Me, "Creating new context: 0x"
                & System.Address_Image (Context.Ref.Get.Element.all'Address));
      end if;
      return Context;
   end New_Context;

   ---------------------
   -- Get_Main_Window --
   ---------------------

   function Get_Main_Window
     (Handle : access Kernel_Handle_Record) return Gtk.Window.Gtk_Window is
   begin
      return Gtk_Window (Handle.Main_Window);
   end Get_Main_Window;

   ------------------
   -- Get_Home_Dir --
   ------------------

   function Get_Home_Dir
     (Handle : access Kernel_Handle_Record) return Virtual_File is
   begin
      return Handle.Home_Dir;
   end Get_Home_Dir;

   --------------------
   -- Get_System_Dir --
   --------------------

   function Get_System_Dir
     (Handle : access Kernel_Handle_Record) return Virtual_File is
   begin
      return Handle.Prefix;
   end Get_System_Dir;

   -------------------
   -- Get_Share_Dir --
   -------------------

   overriding function Get_Share_Dir
     (Self : not null access Kernel_Handle_Record)
      return GNATCOLL.VFS.Virtual_File is
   begin
      return Create_From_Dir (Self.Get_System_Dir, "share/gps/");
   end Get_Share_Dir;

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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Handle : access Kernel_Handle_Record) is
      Success : Boolean;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (History_Record, History);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Root_Table'Class, Root_Table_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Refactoring.Factory_Context_Record'Class,
         Refactoring.Factory_Context);
   begin
      Trace (Me, "Destroying the kernel");

      --  Stop executing actions in the background
      Handle.Tasks.Interrupt_All_Tasks;

      Free_Modules (Handle);

      --  Remove dangling timeout callback, if any.
      --  Do this before the rest, for a minor optimization: so that
      --  the "context_changed" hook won't be run here, which wouldn't
      --  be necessary.

      Hooks.Unregister_Debounce_Timeouts;

      Save_Scenario_Vars_On_Exit (Handle);

      Close_Persistent_Properties_DB (Handle);
      Reset_Properties (Handle);

      Trace (Me, "Saving histories.xml");
      Save (Handle.History.all,
            Create_From_Dir (Handle.Home_Dir, "histories.xml"),
            Success);
      Free (Handle.History.all);
      Unchecked_Free (Handle.History);

      if not Success then
         Report_Preference_File_Error
           (Handle, Create_From_Dir (Handle.Home_Dir, "histories.xml"));
      end if;

      Reset (Handle.Startup_Scripts);
      Unchecked_Free (Handle.Startup_Scripts);

      Destroy_Clipboard (Handle);
      Destroy (Handle.Preferences);

      Free_Style_Manager (Kernel_Handle (Handle));

      --  ??? Already done in remote.db.Destroy
      --  GNAT.Expect.TTY.Remote.Close_All;

      --  Do not free the contexts. They can still be stored as Data in a
      --  Class_Instance, and this will be finalized later automatically. If
      --  we call Unref here, this results in a double deallocation.
      --  This code is left here for reference to avoid doing this error in the
      --  future.
      --        Unref (Handle.Current_Context);
      --        Unref (Handle.Last_Context_For_Contextual);

      --  This also frees all commands
      Reset (Handle.Actions);
      Unchecked_Free (Handle.Actions);

      --  Free the registered filters
      declare
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (Action_Filter_Record'Class, Action_Filter);
      begin
         for Act of Handle.All_Action_Filters loop
            Free (Act.all);
            Unchecked_Free (Act);
         end loop;
         Handle.Action_Filters.Clear;
         Handle.All_Action_Filters.Clear;
      end;

      Handle.Hooks.Clear;
      Free_Tools (Handle);

      Free (Handle.Logs_Mapper);
      Free_Messages_Container (Handle);

      Unchecked_Free (Handle.Refactoring);
      Handle.Last_Context_For_Contextual := No_Context;

      --  Handle.Symbols.Display_Stats;

      GPS.Core_Kernels.Destroy (Core_Kernel_Record (Handle.all)'Access);

      --  Free the memory allocated by gtk+, and disconnect all the callbacks,
      --  reclaiming the associated memory.
      Trace (Me, "Done destroying the GPS kernel");

      --  ??? Do not free the memory in fact, since there are some controlled
      --  types like Class_Instance which might in fact be finalized only after
      --  this point, and they might try to access the kernel. This might show
      --  up as a minor memory leak, which can safely be ignored anyway. See
      --  for instance Unref for a context below.
      --      Unref (Handle);

      Kernel_Desktop.Free_Registered_Desktop_Functions;
   end Destroy;

   ----------------------------
   -- Push_Marker_In_History --
   ----------------------------

   procedure Push_Marker_In_History
     (Kernel : access Kernel_Handle_Record'Class;
      Marker : Location_Marker) is
   begin
      Marker_Added_To_History_Hook.Run (Kernel, Marker);
   end Push_Marker_In_History;

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
      Default_Key : String;
      Exclusive   : Boolean := True)
   is
      Err : constant String := Add_Customization_String
        (Kernel,
         "<key action=""" & Action & """ exclusive='"
         & Boolean'Image (Exclusive) & "'>" & Default_Key & "</key>",
         From_File => "<gps_internal>");
      pragma Unreferenced (Err);
   begin
      null;
   end Bind_Default_Key;

   -------------------
   -- Lookup_Filter --
   -------------------

   function Lookup_Filter
     (Kernel : access Kernel_Handle_Record;
      Name   : String) return Action_Filter
   is
      C : constant Action_Filters_Maps.Cursor :=
        Kernel.Action_Filters.Find (Name);
   begin
      if Has_Element (C) then
         return Element (C);
      else
         return null;
      end if;
   end Lookup_Filter;

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Filter : access Action_Filter_Record;
      Name   : String) is
   begin
      if Name /= "" then
         Assert (Me, Filter.Name = "", "Renaming filter is not allowed");
         Filter.Name := To_Unbounded_String (Name);
         Kernel.Action_Filters.Include (Name, Action_Filter (Filter));
      end if;

      if not Filter.Registered then
         Kernel.All_Action_Filters.Append (Action_Filter (Filter));
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
     (Name            : Filesystem_String;
      Kernel          : access Kernel_Handle_Record;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return GNATCOLL.VFS.Virtual_File
   is
      File : GNATCOLL.VFS.Virtual_File;
   begin
      File := Get_Registry (Kernel).Tree.Create
        (Name, Use_Source_Path => Use_Source_Path,
         Use_Object_Path => Use_Object_Path);

      if File = GNATCOLL.VFS.No_File then
         File := GNATCOLL.VFS.Create_From_Base (Base_Name => Name);
      end if;

      return File;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (Tool : in out Tool_Properties) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Tool_Properties_Record, Tool_Properties);
   begin
      Free (Tool.Languages);
      Free (Tool.Config);
      Unchecked_Free (Tool);
   end Free;

   ----------------
   -- Free_Tools --
   ----------------

   procedure Free_Tools (Kernel : access Kernel_Handle_Record'Class) is
      use Tools_List;
      Cursor : Tools_List.Cursor := First (Kernel.Tools);
      Tool   : Tool_Properties;
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
      Tool   : not null Tool_Properties)
   is
      Pkg  : Package_Node_Id;
      Attr : Attribute_Node_Id;
      Elm  : Tool_Properties;
      Iter : Tools_List.Cursor;
      use Tools_List;
   begin
      Name_Len := Length (Tool.Project_Package);
      Name_Buffer (1 .. Name_Len) :=
        To_Lower (To_String (Tool.Project_Package));
      Pkg := Package_Node_Id_Of (Name_Find);

      if Pkg = Empty_Package then
         Register_New_Package (To_String (Tool.Project_Package), Pkg);
      end if;

      Name_Len := Length (Tool.Project_Attribute);
      Name_Buffer (1 .. Name_Len) :=
        To_Lower (To_String (Tool.Project_Attribute));
      Attr := Attribute_Node_Id_Of
        (Name  => Name_Find, Starting_At => First_Attribute_Of (Pkg));

      if Attr = Empty_Attribute then
         if Tool.Project_Index = "" then
            Register_New_Attribute
              (Name       => To_Lower (To_String (Tool.Project_Attribute)),
               In_Package => Pkg,
               Attr_Kind  => GPR.Attr.Single,
               Var_Kind   => GPR.List);
         else
            Register_New_Attribute
              (Name       => To_Lower (To_String (Tool.Project_Attribute)),
               In_Package => Pkg,
               Attr_Kind  => GPR.Attr.Associative_Array,
               Var_Kind   => GPR.List);
         end if;
      end if;

      Iter := First (Kernel.Tools);
      while Has_Element (Iter) loop
         Elm := Element (Iter);

         if Elm.Project_Index = Tool.Project_Index
           and then Elm.Project_Package = Tool.Project_Package
           and then Elm.Project_Attribute = Tool.Project_Attribute
         then
            Tools_List.Replace_Element (Kernel.Tools, Iter, Tool);
            Free (Elm);

            if not Tool.Override then
               Insert (Kernel,
                       Text   =>  -"Warning: tool "
                                  & To_String (Tool.Tool_Name)
                                  & (-" is defined twice"),
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
      Tool_Name : String) return Tool_Properties
   is
      use Tools_List;
      Iter : Tools_List.Cursor := First (Kernel.Tools);
   begin
      while Has_Element (Iter) loop
         if To_Lower (To_String (Element (Iter).Tool_Name))
           = To_Lower (Tool_Name)
         then
            return Element (Iter);
         end if;
         Next (Iter);
      end loop;
      return null;
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
      if Language = "" and then Shell = "" and then Module = "" then
         return null;
      else
         F.Language := To_Unbounded_String (Language);
         F.Shell := To_Unbounded_String (Shell);
         F.Shell_Lang :=
           (if Shell /= ""
            then To_Unbounded_String (Shell_Lang) else Null_Unbounded_String);
         F.Module := To_Unbounded_String (Module);
         return Action_Filter (F);
      end if;
   end Create;

   -----------
   -- "and" --
   -----------

   function "and"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Action_Filter is
   begin
      if Filter1 = null then
         return Action_Filter (Filter2);
      elsif Filter2 = null then
         return Action_Filter (Filter1);
      else
         --  ??? The use of Unrestricted_Access is ugly, but it allows nicer
         --  user code, since it won't require temporary variable. Also done
         --  for actions themselves.
         return new Base_Action_Filter_Record'
           (Kind       => Filter_And,
            Error_Msg  => Null_Unbounded_String,
            Name       => Null_Unbounded_String,
            Registered => False,
            And1       => Filter1.all'Unrestricted_Access,
            And2       => Filter2.all'Unrestricted_Access);
      end if;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Action_Filter is
   begin
      if Filter1 = null then
         return Action_Filter (Filter2);
      elsif Filter2 = null then
         return Action_Filter (Filter1);
      else
         return new Base_Action_Filter_Record'
           (Kind       => Filter_Or,
            Error_Msg  => Null_Unbounded_String,
            Name       => Null_Unbounded_String,
            Registered => False,
            Or1        => Filter1.all'Unrestricted_Access,
            Or2        => Filter2.all'Unrestricted_Access);
      end if;
   end "or";

   -----------
   -- "not" --
   -----------

   function "not"
     (Filter : access Action_Filter_Record'Class) return Action_Filter is
   begin
      if Filter = null then
         return null;
      else
         return new Base_Action_Filter_Record'
           (Kind       => Filter_Not,
            Registered => False,
            Error_Msg  => Null_Unbounded_String,
            Name       => Null_Unbounded_String,
            Not1       => Filter.all'Unrestricted_Access);
      end if;
   end "not";

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment
     (Self  : access Kernel_Handle_Record;
      Value : GPS.Environments.Environment) is
   begin
      Self.Env := Value;
   end Set_Environment;

   -----------------------
   -- Set_Error_Message --
   -----------------------

   procedure Set_Error_Message
     (Filter : Action_Filter;
      Msg    : Unbounded_String) is
   begin
      Filter.Error_Msg := Msg;
   end Set_Error_Message;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message
     (Filter : access Action_Filter_Record'Class) return Unbounded_String is
   begin
      if Filter /= null then
         return Filter.Error_Msg;
      else
         return Null_Unbounded_String;
      end if;
   end Get_Error_Message;

   ---------------------
   -- Get_Environment --
   ---------------------

   function Get_Environment
     (Self : access Kernel_Handle_Record)
      return GPS.Environments.Environment is
   begin
      return Self.Env;
   end Get_Environment;

   --------------------
   -- Get_Debug_Name --
   --------------------

   function Get_Debug_Name
     (Filter : access Action_Filter_Record) return String is
   begin
      if Filter = null then
         return "";

      elsif Filter.Name /= "" then
         return To_String (Filter.Name);

      else
         return External_Tag (Action_Filter_Record'Class (Filter.all)'Tag);
      end if;
   end Get_Debug_Name;

   --------------------
   -- Get_Debug_Name --
   --------------------

   overriding function Get_Debug_Name
     (Filter : access Base_Action_Filter_Record) return String is
   begin
      case Filter.Kind is
         when Standard_Filter =>
            return "Base"
              & (if Filter.Language /= ""
                 then " lang=" & To_String (Filter.Language) else "")
              & (if Filter.Shell /= ""
                 then " shell=" & To_String (Filter.Shell) else "")
              & (if Filter.Module /= ""
                 then " module=" & To_String (Filter.Module) else "");
         when Filter_And      => return """and""";
         when Filter_Not      => return """not""";
         when Filter_Or       => return """or""";
      end case;
   end Get_Debug_Name;

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
            if Filter.Language /= Null_Unbounded_String then
               if Has_File_Information (Context)
                 and then GNATCOLL.VFS.No_File /= File_Information (Context)
               then
                  declare
                     Lang : constant String := Get_Language_From_File
                       (Get_Language_Handler (Kernel),
                        File_Information (Context));
                  begin
                     if not Equal
                       (Lang, To_String (Filter.Language), False)
                     then
                        Result := False;
                     end if;
                  end;

               elsif Has_Project_Information (Context) then
                  Result := Project_Information (Context)
                    .Has_Language (To_String (Filter.Language));

               else
                  Result := False;
               end if;
            end if;

            if Result
              and then Filter.Module /= Null_Unbounded_String
              and then (Get_Creator (Context) = null
                        or else not Equal
                          (Module_Name (Module_ID (Get_Creator (Context))),
                           To_String (Filter.Module),
                           False))
            then
               Result := False;
            end if;

            if Result and then Filter.Shell /= Null_Unbounded_String then
               declare
                  Lang : constant Scripting_Language :=
                           Lookup_Scripting_Language
                             (Kernel.Scripts, To_String (Filter.Shell_Lang));

                  function Substitution
                    (Param : String;
                     Mode  : Command_Line_Mode) return Arg_List;
                  --  Local substitution of special chars

                  ------------------
                  -- Substitution --
                  ------------------

                  function Substitution
                    (Param : String;
                     Mode  : Command_Line_Mode) return Arg_List
                  is
                     pragma Unreferenced (Mode);
                     Done : aliased Boolean := False;
                  begin
                     return Create (GPS.Kernel.Macros.Substitute
                                    (Param, Context, False, Done'Access));
                  end Substitution;

                  CL : Arg_List;

               begin
                  if Lang = null then
                     Result := False;

                  else
                     CL := Parse_String
                       (To_String (Filter.Shell),
                        Command_Line_Treatment (Lang));

                     Substitute
                       (CL,
                        GPS.Kernel.Macros.Special_Character,
                        Substitution'Unrestricted_Access);

                     declare
                        Errors : aliased Boolean;
                        R      : constant Boolean :=
                         GNATCOLL.Scripts.Execute_Command
                            (Lang,
                             CL,
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
     (Filter  : access Action_Filter_Record'Class;
      Context : Selection_Context) return Boolean
   is
      use Filter_Result_Map;
      C      : Filter_Result_Map.Cursor;
      Result : Boolean;
   begin
      if Filter = null then
         return True;
      end if;

      if Context.Ref.Is_Null then
         return False;
      end if;

      --  Cache the result of each filter on this context in Computed_Filters.

      C := Context.Ref.Get.Computed_Filters.Find (Filter.all'Address);

      if Has_Element (C) then
         return Element (C);
      else
         if Active (Me_Filters) then
            Increase_Indent
               (Me_Filters, "Running filter " & Filter.Get_Debug_Name);
         end if;

         Result := Filter_Matches_Primitive (Filter, Context);

         if Active (Me_Filters) then
            Decrease_Indent (Me_Filters, "");
         end if;

         Context.Ref.Get.Computed_Filters.Insert
           (Filter.all'Address, Result);
         return Result;
      end if;
   end Filter_Matches;

   ----------------------
   -- Enter_Hyper_Mode --
   ----------------------

   procedure Enter_Hyper_Mode (Kernel : access Kernel_Handle_Record) is
   begin
      Kernel.Hyper_Mode := True;
   end Enter_Hyper_Mode;

   ----------------------
   -- Leave_Hyper_Mode --
   ----------------------

   procedure Leave_Hyper_Mode (Kernel : access Kernel_Handle_Record) is
   begin
      Kernel.Hyper_Mode := False;
   end Leave_Hyper_Mode;

   -------------------
   -- In_Hyper_Mode --
   -------------------

   function In_Hyper_Mode
     (Kernel : access Kernel_Handle_Record) return Boolean is
   begin
      return Kernel.Hyper_Mode;
   end In_Hyper_Mode;

   -------------------------
   -- Refactoring_Context --
   -------------------------

   function Refactoring_Context
     (Kernel : access Kernel_Handle_Record)
      return Refactoring.Factory_Context is
   begin
      if Kernel.Refactoring = null then
         Kernel.Refactoring := new GPS_Refactoring_Factory_Context'
           (Kernel                 => Kernel_Handle (Kernel),
            Buffer_Factory         => Get_Buffer_Factory (Kernel),
            Db                     => Kernel.Databases,
            Add_Subprogram_Box     => False,
            Add_In_Keyword         => False,
            Create_Subprogram_Decl => False);
      end if;

      --  Update the flags from the current value of the preferences

      Kernel.Refactoring.Add_Subprogram_Box := Add_Subprogram_Box.Get_Pref;
      Kernel.Refactoring.Add_In_Keyword     := Add_In_Keyword.Get_Pref;
      Kernel.Refactoring.Create_Subprogram_Decl :=
        Create_Subprogram_Decl.Get_Pref;

      return Kernel.Refactoring;
   end Refactoring_Context;

   --------------------
   -- Set_Key_Setter --
   --------------------

   procedure Set_Key_Setter
     (Kernel        : access Kernel_Handle_Record;
      Setter        : Key_Setter;
      Getter        : Key_Getter;
      Getter_Simple : Key_Getter_Simple)
   is
   begin
      Kernel.Key_Setter_Function := Setter;
      Kernel.Key_Getter_Function := Getter;
      Kernel.Key_Getter_Simple_Function := Getter_Simple;
   end Set_Key_Setter;

   ---------------------
   -- Set_Default_Key --
   ---------------------

   procedure Set_Default_Key
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type) is
   begin
      if Kernel.Key_Setter_Function /= null then
         Kernel.Key_Setter_Function (Kernel, Action, Accel_Key, Accel_Mods);
      end if;
   end Set_Default_Key;

   ------------------
   -- Get_Shortcut --
   ------------------

   function Get_Shortcut
     (Kernel          : access Kernel_Handle_Record'Class;
      Action          : String;
      Use_Markup      : Boolean := True;
      Return_Multiple : Boolean := True) return String
   is
   begin
      if Kernel.Key_Getter_Function = null then
         return "";
      else
         return Kernel.Key_Getter_Function
           (Kernel, Action, Use_Markup, Return_Multiple);
      end if;
   end Get_Shortcut;

   -------------------------
   -- Get_Shortcut_Simple --
   -------------------------

   procedure Get_Shortcut_Simple
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Key        : out Gdk.Types.Gdk_Key_Type;
      Mods       : out Gdk.Types.Gdk_Modifier_Type) is
   begin
      if Kernel.Key_Getter_Simple_Function = null then
         Key := 0;
         Mods := 0;
      else
         Kernel.Key_Getter_Simple_Function (Kernel, Action, Key, Mods);
      end if;
   end Get_Shortcut_Simple;

   -------------------------
   -- Set_Messages_Window --
   -------------------------

   procedure Set_Messages_Window
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Console : not null access Abstract_Messages_Window'Class) is
   begin
      Kernel.Messages := Abstract_Messages_Window_Access (Console);
      if Kernel.Pending_Messages /= Null_Unbounded_String then
         Insert (Kernel, To_String (Kernel.Pending_Messages), Add_LF => True);
         Kernel.Pending_Messages := Null_Unbounded_String;
      end if;
   end Set_Messages_Window;

   --------------------------
   -- Get_Messages_Console --
   --------------------------

   function Get_Messages_Console
     (Kernel  : not null access Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Kernel.Messages.Get_Console_Window;
   end Get_Messages_Console;

   ----------------------------
   -- Get_Messages_Container --
   ----------------------------

   function Get_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return not null Messages_Container_Access is
   begin
      return Kernel.Messages_Container;
   end Get_Messages_Container;

   -------------------------
   -- Get_Messages_Window --
   -------------------------

   function Get_Messages_Window
     (Kernel  : not null access Kernel_Handle_Record'Class)
      return Virtual_Console
   is
   begin
      return Kernel.Messages.Get_Virtual_Console;
   end Get_Messages_Window;

   ---------------------
   -- Messages_Window --
   ---------------------

   overriding function Messages_Window
     (Self : not null access Kernel_Handle_Record)
      return GPS.Messages_Windows.Abstract_Messages_Window_Access is
   begin
      return GPS.Messages_Windows.Abstract_Messages_Window_Access
        (Self.Messages);
   end Messages_Window;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Kernel : not null access Kernel_Handle_Record'Class;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info) is
   begin
      if Kernel.Is_In_Destruction then
         Trace (Me, "Message received after destruction: " & Text);
         return;
      end if;

      if Kernel.Messages = null then
         Append (Kernel.Pending_Messages, Text);
         if Add_LF then
            Append (Kernel.Pending_Messages, ASCII.LF);
         end if;
      else
         Kernel.Messages.Insert (Text, Add_LF, Mode);
      end if;
   exception
      when E : Constraint_Error =>
         Trace (Me, "Exception when inserting the follwing message: " & Text);
         Trace (Me, E);
   end Insert;

   -----------------
   -- Insert_UTF8 --
   -----------------

   procedure Insert_UTF8
     (Kernel : not null access Kernel_Handle_Record'Class;
      UTF8   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info) is
   begin
      Kernel.Messages.Insert_UTF8 (UTF8, Add_LF, Mode);
   end Insert_UTF8;

   --------------------
   -- Clear_Messages --
   --------------------

   procedure Clear_Messages
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Kernel.Messages.Clear;
   end Clear_Messages;

   -------------------
   -- Raise_Console --
   -------------------

   procedure Raise_Console
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Kernel.Messages.Raise_Console (Give_Focus => False);
   end Raise_Console;

   --------------------
   -- Set_Build_Mode --
   --------------------

   procedure Set_Build_Mode
     (Kernel : access Kernel_Handle_Record'Class;
      New_Mode : String)
   is
      Prop : aliased String_Property_Access;
   begin
      Trace (Me, "Change build mode to: " & New_Mode);

      if New_Mode /= "default" then
         Prop := new String_Property;
         Prop.Value := new String'(New_Mode);
         Set_Property
           (Kernel,
            GPS.Kernel.Project.Get_Project (Kernel),
            Build_Mode_Property,
            Prop,
            Persistent => True);
      else
         GPS.Kernel.Properties.Remove_Property
           (Kernel,
            GPS.Kernel.Project.Get_Project (Kernel),
            Build_Mode_Property);
      end if;

      Build_Mode_Changed_Hook.Run (Kernel, Str => New_Mode);
   end Set_Build_Mode;

   --------------------
   -- Get_Build_Mode --
   --------------------

   overriding function Get_Build_Mode
     (Kernel : not null access Kernel_Handle_Record) return String
   is
      Prop  : String_Property;
      Found : Boolean;
   begin
      --  This needs to be kept in sync with
      --  Builder_Facility_Module.On_Build_Mode_Changed

      Get_Property
        (Prop,
         Project => GPS.Kernel.Project.Get_Project (Kernel),
         Name    => Build_Mode_Property,
         Found   => Found);
      if Found then
         return Prop.Value.all;
      else
         return "default";
      end if;
   end Get_Build_Mode;

   ----------------------
   -- Process_Launcher --
   ----------------------

   overriding function Process_Launcher
     (Self : not null access Kernel_Handle_Record)
      return GPS.Process_Launchers.Process_Launcher is
   begin
      return Self.Launcher'Access;
   end Process_Launcher;

   ----------------
   -- Get_Target --
   ----------------

   overriding function Get_Target
     (Self : not null access Kernel_Handle_Record) return String is
   begin
      --  First place to get the target: look at the user setting in the
      --  interface.

      --  [placeholder] insert implementation here when the GUI offers a
      --  control for this.

      --  Then ask the project

      return Self.Registry.Tree.Root_Project.Get_Target;
   end Get_Target;

   -----------------
   -- Get_Runtime --
   -----------------

   overriding function Get_Runtime
     (Self : not null access Kernel_Handle_Record) return String is
   begin
      --  First place to get the runtime: look at the user setting in the
      --  interface.

      --  [placeholder] insert implementation here when the GUI offers a
      --  control for this.

      --  Then ask the project

      return Self.Registry.Tree.Root_Project.Get_Runtime;
   end Get_Runtime;

   --------------
   -- Register --
   --------------

   procedure Register
      (Self    : not null access Hook_Types'Class;
       Kernel  : not null access Kernel_Handle_Record'Class)
   is
   begin
      Kernel.Hooks.Include
         (Hook_Type_Prefix & Self.Type_Name, Hook_Types_Access (Self));
      Kernel.Hooks.Include (Self.Name.all, Hook_Types_Access (Self));
   end Register;

   --------------------
   -- Remove_Hook_Cb --
   --------------------

   procedure Remove_Hook_Cb
      (Data : System.Address;
       Obj  : System.Address)
   is
      pragma Unreferenced (Obj);
      D : Hook_User_Data_Access := Convert (Data);
   begin
      D.Hook.Remove_Hook_Func (D.Func);
      Unchecked_Free (D);
   end Remove_Hook_Cb;

   ----------------------
   -- Remove_Hook_Func --
   ----------------------

   procedure Remove_Hook_Func
      (Self  : in out Hook_Types'Class;
       Func  : not null access Hook_Function'Class)
   is
      function If_Matches
         (F : not null access Hook_Function'Class) return Boolean
         is (F = Func);
   begin
      Self.Remove (If_Matches'Access);
   end Remove_Hook_Func;

   ----------------------------
   -- Internal_Add_Hook_Func --
   ----------------------------

   procedure Internal_Add_Hook_Func
     (Self  : in out Hook_Types'Class;
      List  : in out Hook_Func_Lists.List;
      Func  : not null access Hook_Function'Class;
      Last  : Boolean := True;
      Watch : access Glib.Object.GObject_Record'Class := null)
   is
      D : Hook_User_Data_Access;
   begin
      if Active (Me_Hooks) then
         Trace (Me_Hooks, "Adding "
             & GPS.Kernel.Hooks.Name (Func) & " to hook "
             & GPS.Kernel.Hooks.Name (Self));
      end if;

      Func.Refcount := Func.Refcount + 1;

      --  We use Unrestricted_Access here, as we do for Register_Action,
      --  so that users can do a  Hook.Add (new Type)  directly, instead
      --  of using a temporary variable.
      if Last then
         List.Append ((Func => Func.all'Unrestricted_Access));
      else
         List.Prepend ((Func => Func.all'Unrestricted_Access));
      end if;

      if Watch /= null then
         --  D is freed as part of Remove_Hook_Cb.
         --  The access on Self is valid, since a hook is never freed.
         D := new Hook_User_Data'
            (Hook => Self'Unchecked_Access,
             Func => Func.all'Unrestricted_Access);
         Watch.Weak_Ref (Remove_Hook_Cb'Access, D.all'Address);
      end if;
   end Internal_Add_Hook_Func;

   -------------------
   -- Add_Hook_Func --
   -------------------

   procedure Add_Hook_Func
      (Self  : in out Hook_Types'Class;
       Func  : not null access Hook_Function'Class;
       Last  : Boolean := True;
       Watch : access Glib.Object.GObject_Record'Class := null) is
   begin
      Internal_Add_Hook_Func (Self, Self.Funcs, Func, Last, Watch);
   end Add_Hook_Func;

   ----------------------------
   -- Add_Debounce_Hook_Func --
   ----------------------------

   procedure Add_Debounce_Hook_Func
      (Self  : in out Debounce_Hook_Types'Class;
       Func  : not null access Hook_Function'Class;
       Last  : Boolean := True;
       Watch : access Glib.Object.GObject_Record'Class := null) is
   begin
      Internal_Add_Hook_Func (Self, Self.Asynch_Funcs, Func, Last, Watch);
   end Add_Debounce_Hook_Func;

   ------------
   -- Remove --
   ------------

   function Remove
     (List       : in out Hook_Func_Lists.List;
      If_Matches : not null access function
        (F : not null access Hook_Function'Class) return Boolean;
      Hook_Name : String)
      return Boolean
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Hook_Function'Class, Hook_Function_Access);

      use Hook_Func_Lists;

      C : Hook_Func_Lists.Cursor := List.First;
      F : Hook_Function_Access;

   begin
      while Has_Element (C) loop
         F := Hook_Function_Access (Element (C).Func);
         if If_Matches (F) then
            if Active (Me_Hooks) then
               Trace
                 (Me_Hooks, "Removing " & GPS.Kernel.Hooks.Name (F)
                  & " from hook " & Hook_Name);
            end if;
            F.Refcount := F.Refcount - 1;
            if F.Refcount = 0 then
               F.Destroy;
               Unchecked_Free (F);
            end if;

            List.Delete (C);
            return True;
         end if;
         Next (C);
      end loop;

      return False;
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Self       : in out Hook_Types;
       If_Matches : not null access function
          (F : not null access Hook_Function'Class) return Boolean)
   is
      Result : Boolean with Unreferenced;

   begin
      Result := Remove (Self.Funcs, If_Matches, Hooks.Name (Self));
   end Remove;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
      (Self       : in out Debounce_Hook_Types;
       If_Matches : not null access function
          (F : not null access Hook_Function'Class) return Boolean)
   is
      Result : Boolean with Unreferenced;

   begin
      if not Remove (Self.Funcs, If_Matches, Hooks.Name (Self)) then
         Result := Remove (Self.Asynch_Funcs, If_Matches, Hooks.Name (Self));
      end if;
   end Remove;

   --------------------
   -- List_Functions --
   --------------------

   function List_Functions
      (Self : not null access Hook_Types)
      return GNAT.Strings.String_List
   is
      use GPS.Kernel.Scripts.Hooks;
      Result : GNAT.Strings.String_List
        (1 .. Integer (Self.Funcs.Length));
      Idx : Integer := Result'First - 1;

   begin
      for F of Self.Funcs loop
         Idx := Idx + 1;

         if F.Func.all in Python_Hook_Function'Class then
            Result (Idx) := new String'
               (Python_Hook_Function (F.Func.all).Func.Get_Name);
         else
            Result (Idx) := new String'(External_Tag (F.Func'Tag));
         end if;
      end loop;

      return Result (1 .. Idx);
   end List_Functions;

   --------------------
   -- List_Functions --
   --------------------

   overriding function List_Functions
      (Self : not null access Debounce_Hook_Types)
      return GNAT.Strings.String_List
   is
      use GPS.Kernel.Scripts.Hooks;
      Result : GNAT.Strings.String_List
        (1 .. Integer (Self.Funcs.Length) +
             Integer (Self.Asynch_Funcs.Length));
      Idx : Integer := Result'First - 1;

      procedure Append (F : Hook_Func_Info);

      ------------
      -- Append --
      ------------

      procedure Append (F : Hook_Func_Info) is
      begin
         if F.Func.all in Python_Hook_Function'Class then
            Result (Idx) := new String'
               (Python_Hook_Function (F.Func.all).Func.Get_Name);
         else
            Result (Idx) := new String'(External_Tag (F.Func'Tag));
         end if;
      end Append;

   begin
      for F of Self.Funcs loop
         Idx := Idx + 1;
         Append (F);
      end loop;

      for F of Self.Asynch_Funcs loop
         Idx := Idx + 1;
         Append (F);
      end loop;

      return Result (1 .. Idx);
   end List_Functions;

   ---------------------
   -- Get_Application --
   ---------------------

   function Get_Application
      (Self : not null access Kernel_Handle_Record'Class)
      return not null access Gtk_Application_Record'Class
   is
   begin
      return Self.Application;
   end Get_Application;

   ---------------------------
   -- Get_Scheduled_Command --
   ---------------------------

   overriding function Get_Scheduled_Command
     (Kernel  : not null access Kernel_Handle_Record;
      Command : access Commands.Root_Command'Class)
      return Commands.Command_Access
   is
      use Task_Manager, Commands;
   begin
      if Kernel.Tasks = null or else Command = null then
         return null;
      else
         return Command_Access
           (Kernel.Tasks.Scheduled_Command_From_Command (Command));
      end if;
   end Get_Scheduled_Command;

   -----------------------------------
   -- Set_Default_Line_Number_Click --
   -----------------------------------

   procedure Set_Default_Line_Number_Click
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Action    : String) is
   begin
      Free (Kernel.Default_Line_Click_Action);
      Kernel.Default_Line_Click_Action := new String'(Action);
   end Set_Default_Line_Number_Click;

   ---------------------------------------
   -- Execute_Default_Line_Number_Click --
   ---------------------------------------

   procedure Execute_Default_Line_Number_Click
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Context   : Selection_Context)
   is
      Dummy : Boolean;
   begin
      if Kernel.Default_Line_Click_Action /= null then
         Dummy := Execute_Action
           (Kernel,
            Action  => Kernel.Default_Line_Click_Action.all,
            Context => Context);
      end if;
   end Execute_Default_Line_Number_Click;

   ------------------------
   -- Make_File_Writable --
   ------------------------

   procedure Make_File_Writable
     (Kernel   : not null access Kernel_Handle_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Writable : Boolean := True)
   is
      VCS : Abstract_VCS_Engine_Access;
   begin
      VCS := Kernel.VCS.Guess_VCS_For_Directory (File.Dir);
      VCS.Make_File_Writable (File, Writable);
   end Make_File_Writable;

end GPS.Kernel;
