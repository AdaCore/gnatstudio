------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

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
with XML_Utils;                 use XML_Utils;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Dialogs;            use Gtkada.Dialogs;

with Basic_Mapper;              use Basic_Mapper;
with Basic_Types;               use Basic_Types;
with Default_Preferences;       use Default_Preferences;
with GPS.Intl;                  use GPS.Intl;
with GPS.Editors;               use GPS.Editors;
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
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;
with GPS.Kernel.Xref;           use GPS.Kernel.Xref;
with GPS.Properties;            use GPS.Properties;
with Histories;                 use Histories;
with Language_Handlers;         use Language_Handlers;
with Language.Tree.Database;    use Language.Tree.Database;
with Namet;                     use Namet;
with Prj.Attr;                  use Prj.Attr;
with Projects;                  use Projects;
with Refactoring;               use Refactoring;
with String_Utils;
with String_List_Utils;         use String_List_Utils;
with Switches_Chooser;          use Switches_Chooser;
with System.Address_Image;
with Xref;                      use Xref;

package body GPS.Kernel is

   Me        : constant Trace_Handle := Create ("gps_kernel");
   Ref_Me    : constant Trace_Handle :=
                 Create ("Contexts.Ref", GNATCOLL.Traces.Off);
   Create_Me : constant Trace_Handle :=
                 Create ("Contexts.Mem", GNATCOLL.Traces.Off);

   History_Max_Length : constant Positive := 10;
   --  <preferences> Maximum number of entries to store in each history

   Build_Mode_Property : constant String := "Build-Mode";
   --  The name of a GPS.Properties to store the current build mode. Use
   --  Get_Build_Mode below instead

   use Action_Filters_Htable.String_Hash_Table;

   function To_Address is new Ada.Unchecked_Conversion
     (Selection_Context_Data, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Kernel_Handle);
   function Convert is new Ada.Unchecked_Conversion
     (Kernel_Handle, System.Address);

   procedure Free (Tool : in out Tool_Properties_Record);
   procedure Free_Tools (Kernel : access Kernel_Handle_Record'Class);
   --  Free the list of registered tools

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the preferences change

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
         0, (Editor_Side => True, Locations => True));
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

   function Hash (Hook : Hook_Name) return Hook_Htable_Num is
      function Internal is new String_Utils.Hash (Hook_Htable_Num);
   begin
      return Internal (To_String (Hook));
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
      Prefix_Directory : Virtual_File) is
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
      Handle.Preferences := new GPS_Preferences_Record;
      GPS_Preferences_Record (Handle.Preferences.all).Set_Kernel (Handle);

      Register_Global_Preferences (Handle);
      Load_Preferences (Handle);

      GPS.Kernel.Styles.Init (Handle);

      --  Create the message container
      Handle.Messages_Container := Create_Messages_Container (Handle);

      On_Preferences_Changed (Handle, Data => null);

      Handle.History := new History_Record;
      Trace (Me, "Loading histories.xml");
      Load (Handle.History.all,
            Create_From_Dir (Handle.Home_Dir, "histories.xml"));
      Set_Max_Length (Handle.History.all, History_Max_Length);

      Restore_Persistent_Properties (Handle);

      Create_Clipboard (Handle);

      Add_Hook
        (Handle, Preference_Changed_Hook,
         Wrapper (On_Preferences_Changed'Access),
         Name => "kernel.preferences_changed");
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

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      P : constant Preference := Get_Pref (Data);
   begin
      if P = null
        or else P = Preference (Hidden_Directories_Pattern)
      then
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
      end if;

      if P = null
        or else P = Preference (GPS.Kernel.Preferences.Trusted_Mode)
      then
         Get_Registry (Kernel).Environment.Set_Trusted_Mode
           (GPS.Kernel.Preferences.Trusted_Mode.Get_Pref);
      end if;
   end On_Preferences_Changed;

   ----------------------------
   -- Get_Construct_Database --
   ----------------------------

   function Get_Construct_Database
     (Kernel : access Kernel_Handle_Record)
      return Language.Tree.Database.Construct_Database_Access is
   begin
      return Kernel.Databases.Constructs;
   end Get_Construct_Database;

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

   -------------------------
   -- Source_Lines_Folded --
   -------------------------

   procedure Source_Lines_Folded
     (Handle     : access Kernel_Handle_Record;
      Context    : Selection_Context;
      Start_Line : Natural;
      End_Line   : Natural)
   is
      Data : aliased Two_Lines_Hook_Args :=
        (Hooks_Data with Context => Context,
         Line_1 => Start_Line,
         Line_2 => End_Line);
   begin
      Run_Hook (Handle, Source_Lines_Folded_Hook, Data'Unchecked_Access);
   end Source_Lines_Folded;

   ---------------------------
   -- Source_Lines_Unfolded --
   ---------------------------

   procedure Source_Lines_Unfolded
     (Handle     : access Kernel_Handle_Record;
      Context    : Selection_Context;
      Start_Line : Natural;
      End_Line   : Natural)
   is
      Data : aliased Two_Lines_Hook_Args :=
        (Hooks_Data with Context => Context,
         Line_1 => Start_Line,
         Line_2 => End_Line);
   begin
      Run_Hook (Handle, Source_Lines_Unfolded_Hook, Data'Unchecked_Access);
   end Source_Lines_Unfolded;

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
      File   : GNATCOLL.VFS.Virtual_File;
      Force_Hook : Boolean := False)
   is
      Files : File_Array_Access := Handle.Open_Files;
      Data  : aliased File_Hooks_Args;
   begin
      if Force_Hook or else not Is_Open (Handle, File) then
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
     (Handle      : access Kernel_Handle_Record;
      Category    : String;
      Target_Name : String;
      Mode_Name   : String;
      Shadow      : Boolean;
      Background  : Boolean;
      Status      : Integer)
   is
      Data : aliased Compilation_Finished_Hooks_Args :=
               (Hooks_Data with
                Category_Length    => Category'Length,
                Category           => Category,
                Target_Name_Length => Target_Name'Length,
                Target_Name        => Target_Name,
                Mode_Name_Length   => Mode_Name'Length,
                Mode_Name          => Mode_Name,
                Shadow             => Shadow,
                Background         => Background,
                Status             => Status);

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
      Shadow   : Boolean;
      Background : Boolean) return Boolean
   is
      Data : aliased Compilation_Hooks_Args :=
               (Hooks_Data with
                Length => Category'Length,
                Value  => Category,
                Quiet  => Quiet,
                Shadow => Shadow,
                Background => Background);
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
      Base_Name : Filesystem_String) return Boolean is
   begin
      return Kernel.Hidden_File_Matcher /= null
        and then Match (Kernel.Hidden_File_Matcher.all, +Base_Name);
   end Is_Hidden;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed (Handle : access Kernel_Handle_Record) is
      C    : constant Selection_Context := Get_Current_Context (Handle);
      Data : aliased Context_Hooks_Args := (Hooks_Data with Context => C);
   begin
      Run_Hook (Handle, Context_Changed_Hook, Data'Unchecked_Access);
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
            Error, Button_OK, Justification => Justify_Left);
      else
         Handle.Insert
           ((-"Could not save the configuration file ") &
            Filename.Display_Full_Name & ASCII.LF &
            (-"Please verify that you have write access to this file."),
            Mode => Error);
      end if;
   end Report_Preference_File_Error;

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

         Free (Data.Text);
         Free (Data.Expression);
         Free (Data.Entity_Name);

         --  Do not unref the entity stored in the context if the kernel is in
         --  destruction or as already been destroyed since the entity has
         --  already been freed as part of the kernel destruction.

         if Data.Kernel /= null and then not Data.Kernel.Is_In_Destruction then
            --   ??? problem of double deallocation at shutdown time, ideally
            --   the following call should be outside of the conditional.
            GNATCOLL.VFS.Unchecked_Free (Data.Files);
         end if;

         String_List_Utils.String_List.Free (Data.Activities);
         Free (Data.Revision);
         Free (Data.Other_Revision);
         Free (Data.Tag);
         Free (Data.Instances);
      end Free;

      Tmp     : Instance_List_Access;
      Data    : Selection_Context_Data := Context.Data;

   begin
      Context.Data := null;  --  Make Finalize idempotent
      if Data /= null then
         if Active (Ref_Me) then
            Trace (Ref_Me, "Before decref context: ("
                   & System.Address_Image (To_Address (Data))
                   & " " & Data.Ref_Count'Img & ") in instances="
                   & Length (Data.Instances)'Img);
         end if;

         --  Some references to the selection are hold by the instance list
         --  stored in the selection, so we need to break the cycle here

         if Data.Ref_Count = Length (Data.Instances) + 1 then
            Tmp := Data.Instances;
            Data.Instances := null;
            Free (Tmp);
         end if;

         Data.Ref_Count := Data.Ref_Count - 1;

         if Data.Ref_Count = 0 then
            if Active (Create_Me) then
               GNATCOLL.Traces.Increase_Indent
                 (Create_Me, "Destroy selection context ("
                  & System.Address_Image (To_Address (Data)) & ")");
            end if;

            --  Do not access Context any more below, since the call to Free
            --  will free instances and their user data, and the current call
            --  to Finalize might come from such a user data.

            Free (Data.all);
            Unchecked_Free (Data);

            if Active (Create_Me) then
               GNATCOLL.Traces.Decrease_Indent
                 (Create_Me, "Done destroying selection context");
            end if;
         end if;
      end if;
   exception
      when E : others =>
         Trace (Me, E);
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
      when E : others => Trace (Me, E);
   end Adjust;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Context : Selection_Context) return Kernel_Handle is
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
      Save_Scenario_Vars_On_Exit (Handle);
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

      --  ??? Already done in remote.db.Destroy
      --  GNAT.Expect.TTY.Remote.Close_All;

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

      Free (Handle.Logs_Mapper);
      Free_Messages_Container (Handle);

      Commands.Free (Handle.Perma_Commands);

      Unchecked_Free (Handle.Refactoring);

      Free (Handle.GNAT_Version);

      --  Handle.Symbols.Display_Stats;

      GPS.Core_Kernels.Destroy (Core_Kernel_Record (Handle.all)'Access);

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

   procedure Set_Error_Message (Filter : Action_Filter; Msg : String) is
   begin
      Free (Filter.Error_Msg);
      Filter.Error_Msg := new String'(Msg);
   end Set_Error_Message;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message
     (Filter : access Action_Filter_Record'Class) return String is
   begin
      if Filter /= null and then Filter.Error_Msg /= null then
         return Filter.Error_Msg.all;
      else
         return "";
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

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Filter : access Action_Filter_Record'Class) return String is
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
               if Has_File_Information (Context)
                 and then GNATCOLL.VFS.No_File /= File_Information (Context)
               then
                  declare
                     Lang : constant String := Get_Language_From_File
                       (Get_Language_Handler (Kernel),
                        File_Information (Context));
                  begin
                     if not Equal (Lang, Filter.Language.all, False) then
                        Result := False;
                     end if;
                  end;

               elsif Has_Project_Information (Context) then
                  Result := Project_Information (Context)
                    .Has_Language (Filter.Language.all);

               else
                  Result := False;
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
                       (Filter.Shell.all, Command_Line_Treatment (Lang));

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

      if Context = No_Context then
         return False;
      end if;

      --  Cache the result of each filter on this context in Computed_Filters.

      C := Context.Data.Data.Computed_Filters.Find (Filter.all'Address);

      if Has_Element (C) then
         return Element (C);
      else
         Result := Filter_Matches_Primitive (Filter, Context);
         Context.Data.Data.Computed_Filters.Insert
           (Filter.all'Address, Result);
         return Result;
      end if;
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
                Data'Unchecked_Access);
   end Push_Marker_In_History;

   ----------------------------
   -- Register_Perma_Command --
   ----------------------------

   procedure Register_Perma_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Commands.Root_Command'Class)
   is
      use Commands.Command_Lists, Commands;

      L : Command_Lists.Cursor := First (Kernel.Perma_Commands);
   begin
      while Has_Element (L) loop
         if Element (L) = Commands.Command_Access (Command) then
            return;  --  Already in list, nothing to do
         end if;

         L := Next (L);
      end loop;

      --  Command is not in list: we steal a reference to it

      Kernel.Perma_Commands.Append (Commands.Command_Access (Command));
   end Register_Perma_Command;

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

   -----------------------------
   -- Set_Is_Dispatching_Call --
   -----------------------------

   procedure Set_Is_Dispatching_Call
     (Context : Selection_Context; Is_Dispatching : Boolean) is
   begin
      if Context.Data.Data /= null then
         Context.Data.Data.Is_Dispatching_Call :=
           To_TriBoolean (Is_Dispatching);
      end if;
   end Set_Is_Dispatching_Call;

   -------------------------
   -- Is_Dispatching_Call --
   -------------------------

   function Is_Dispatching_Call
     (Context : Selection_Context) return GNATCOLL.Tribooleans.Triboolean is
   begin
      if Context.Data.Data /= null then
         return Context.Data.Data.Is_Dispatching_Call;
      else
         return Indeterminate;
      end if;
   end Is_Dispatching_Call;

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
      Data   : aliased String_Hooks_Args :=
        (Hooks_Data with
         Length => New_Mode'Length,
         Value  => New_Mode);
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

      Run_Hook (Kernel, Build_Mode_Changed_Hook, Data'Access);
      Destroy (Data);
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

end GPS.Kernel;
