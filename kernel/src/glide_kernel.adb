-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with Glib;                      use Glib;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Properties;           use Glib.Properties;
with Glib.Values;               use Glib.Values;
with Gdk.Window;                use Gdk.Window;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.Container;             use Gtk.Container;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with System;                    use System;

with File_Utils;                use File_Utils;
with Glide_Intl;                use Glide_Intl;
with Glide_Main_Window;         use Glide_Main_Window;
with Default_Preferences;       use Default_Preferences;
with Glide_Kernel.Custom;       use Glide_Kernel.Custom;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Main_Window;           use GVD.Main_Window;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Interfaces.C;              use Interfaces.C;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with Basic_Mapper;              use Basic_Mapper;
with Histories;                 use Histories;
with Commands;                  use Commands;
with VFS;                       use VFS;

with Projects.Registry;         use Projects, Projects.Registry;

with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Generic_List;

with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;

with Traces;                    use Traces;

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;

package body Glide_Kernel is

   Signals : constant chars_ptr_array :=
     (1  => New_String (Project_Changed_Signal),
      2  => New_String (Project_View_Changed_Signal),
      3  => New_String (Context_Changed_Signal),
      4  => New_String (Variable_Changed_Signal),
      5  => New_String (Source_Lines_Revealed_Signal),

      6  => New_String (File_Edited_Signal),
      7  => New_String (File_Saved_Signal),
      8  => New_String (File_Closed_Signal),
      9  => New_String (File_Changed_On_Disk_Signal),
      10 => New_String (Compilation_Finished_Signal),

      11 => New_String (Preferences_Changed_Signal),
      12 => New_String (Search_Regexps_Changed_Signal),
      13 => New_String (Search_Reset_Signal),
      14 => New_String (Search_Functions_Changed_Signal));
   --  The list of signals defined for this object

   Kernel_Class : GObject_Class := Uninitialized_Class;
   --  The class structure for this object

   Me : constant Debug_Handle := Create ("glide_kernel");

   History_Max_Length : constant Positive := 10;
   --  <preferences> Maximum number of entries to store in each history

   package Object_Callback is new Gtk.Handlers.Callback
     (Glib.Object.GObject_Record);

   use Actions_Htable.String_Hash_Table;
   use Action_Filters_Htable.String_Hash_Table;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Registry'Class, Project_Registry_Access);

   function Process_Anim (Data : Process_Data) return Boolean;
   --  Process_Timeout callback to handle image animations.

   procedure Row_Activated (Widget : access Gtk_Widget_Record'Class);
   --  Called when a specific entity declaration has been selected in the
   --  overloaded entities dialog.

   procedure Select_Entity_Declaration
     (Kernel        : access Kernel_Handle_Record'Class;
      Lib_Info      : LI_File_Ptr;
      Entity_Name   : String;
      Decl          : out Entity_Information;
      Status        : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status);
   --  Open a dialog to ask the user to select among multiple declaration for
   --  the entity with name Entity_Name.
   --  Decl is set to No_Entity_Information and Status to Entity_Not_Found if
   --  the user didn't select any declaration.

   procedure Select_All_Children (View : access Gtk_Widget_Record'Class);
   --  Callback for the "save all windows" dialog

   procedure Select_Child_When_Saving
     (View   : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback when a specific line is selected in the "save all windows"
   --  dialog

   --------------------------
   -- Get_Language_Handler --
   --------------------------

   function Get_Language_Handler
     (Handle : access Kernel_Handle_Record)
      return Language_Handlers.Language_Handler is
   begin
      return Handle.Lang_Handler;
   end Get_Language_Handler;

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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Handle      : out Kernel_Handle;
      Main_Window : Gtk.Window.Gtk_Window;
      Home_Dir    : String)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 .. 2 | 4 | 11 .. 14 => (1 => GType_None),
         3      | 5            => (1 => GType_Pointer),
         6 .. 10               => (1 => GType_String));
      Handler : Glide_Language_Handler;
      Dir     : constant String := Name_As_Directory (Home_Dir);

   begin
      Handle := new Kernel_Handle_Record;
      Glib.Object.Initialize (Handle);
      Initialize_Class_Record
        (Handle, Signals, Kernel_Class, "GlideKernel", Signal_Parameters);

      Handle.Main_Window  := Main_Window;
      Handle.Home_Dir     := new String'(Dir);

      --  Create the language handler. It is also set for the gvd main window,
      --  so that the embedded gvd uses the same mechanism as the rest of glide
      --  to guess the language for a file name.

      Gtk_New (Handler);
      Handle.Lang_Handler := Language_Handler (Handler);
      Glide_Window (Handle.Main_Window).Lang_Handler :=
        Handle.Lang_Handler;

      Handle.Registry := new Project_Registry;
      Load_Default_Project (Handle.Registry.all, Get_Current_Dir);

      Set_Registry
        (Glide_Language_Handler (Handle.Lang_Handler), Handle.Registry);

      Handle.Gnatls_Cache := null;

      --  Note: we do not compute the view of this project yet. This will be
      --  done only if no other project was loaded from the command line, which
      --  is more efficient in case the current directory has lots of source
      --  files.

      Reset (Handle.Source_Info_List);

      Gtk_New (Handle.Tooltips);
      Ref (Handle.Tooltips);
      Sink (Handle.Tooltips);

      Handle.Preferences := new GPS_Preferences_Record;
      GVD.Preferences.GVD_Prefs := GVD_Preferences (Handle.Preferences);
      Register_Global_Preferences (Handle);
      Load_Preferences (Handle.Preferences, Dir & "preferences");

      Handle.History := new History_Record;
      Load (Handle.History.all, Dir & "history");
      Set_Max_Length (Handle.History.all, History_Max_Length);

      Glide_Kernel.Scripts.Initialize (Handle);
   end Gtk_New;

   ------------------------------
   -- Get_Default_Accelerators --
   ------------------------------

   function Get_Default_Accelerators
     (Handle : access Kernel_Handle_Record)
      return Gtk.Accel_Group.Gtk_Accel_Group is
   begin
      return GVD_Main_Window (Handle.Main_Window).Main_Accel_Group;
   end Get_Default_Accelerators;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Data : Glib.Object.GObject; Id : Gtk.Handlers.Handler_Id) is
   begin
      Add_Watch (Id, Data);
   end Setup;

   ---------------------
   -- Get_File_Editor --
   ---------------------

   function Get_File_Editor
     (Handle : access Kernel_Handle_Record;
      File   : VFS.Virtual_File) return Gtkada.MDI.MDI_Child
   is
      MDI   : constant MDI_Window := Get_MDI (Handle);
      Child : MDI_Child;

   begin
      --  ??? the following implementation assumes that the file editors
      --  are MDI children that have corresponding file names for title, and
      --  that they are the only MDI childs that do so.
      --  ??? We might improve a little by checking the Tag of the child
      --  against that of the source editor module. The ID for that module
      --  needs to be moved to glide_kernel.ads.

      --  First, try to find the editor using the normalized name of File.
      Child := Find_MDI_Child_By_Name
        (MDI, Full_Name (File, Normalize => True).all);

      --  If no editor could be found matching the file name, look in the open
      --  files for a file that matches File, and then try to find an editor
      --  for the non-normalized file name for this file.

      --  ??? A correct implementation would be either to always normalize file
      --  names when opening editors, or to store the MDI Child along with the
      --  files in Handle.Open_Files.
      --  The temporary implementation below was chosen because we didn't want
      --  to make overly massive changes at the time.

      if Child /= null then
         return Child;
      else
         if Handle.Open_Files /= null then
            for J in Handle.Open_Files'Range loop
               if File = Handle.Open_Files (J) then
                  return Find_MDI_Child_By_Name
                    (MDI,
                     Full_Name
                       (Handle.Open_Files (J), Normalize => False).all);
               end if;
            end loop;
         end if;

         return null;
      end if;
   end Get_File_Editor;

   ---------------------------
   -- Get_Module_From_Child --
   ---------------------------

   function Get_Module_From_Child
     (Child : Gtkada.MDI.MDI_Child) return Module_ID is
   begin
      if Child.all in GPS_MDI_Child_Record'Class then
         return GPS_MDI_Child (Child).Module;
      else
         return null;
      end if;
   end Get_Module_From_Child;

   -------------------------
   -- Select_All_Children --
   -------------------------

   procedure Select_All_Children (View : access Gtk_Widget_Record'Class) is
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (Gtk_Tree_View (View)));
      Iter  : Gtk_Tree_Iter := Get_Iter_First (Model);
      Value : Boolean;

   begin
      if Iter /= Null_Iter then
         Value := not Get_Boolean (Model, Iter, 0);

         while Iter /= Null_Iter loop
            Set (Model, Iter, 0, Value);
            Next (Model, Iter);
         end loop;
      end if;
   end Select_All_Children;

   ------------------------------
   -- Select_Child_When_Saving --
   ------------------------------

   procedure Select_Child_When_Saving
     (View   : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Model       : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (Gtk_Tree_View (View)));
      Path_String : constant String := Get_String (Nth (Params, 1));
      Iter        : constant Gtk_Tree_Iter :=
        Get_Iter_From_String (Model, Path_String);

   begin
      Set (Model, Iter, 0, not Get_Boolean (Model, Iter, 0));
   end Select_Child_When_Saving;

   -----------------------
   -- Save_MDI_Children --
   -----------------------

   function Save_MDI_Children
     (Handle   : access Kernel_Handle_Record;
      Children : Gtkada.MDI.MDI_Child_Array := Gtkada.MDI.No_Children;
      Force    : Boolean := False) return Boolean
   is
      Column_Types        : constant GType_Array :=
        (GType_Boolean, GType_String);
      MDI                 : constant MDI_Window := Get_MDI (Handle);
      Project_Description : constant String := -"Project";
      Iter                : Child_Iterator;
      Child               : MDI_Child;
      Num_Unsaved         : Natural := 0;
      Model               : Gtk_Tree_Store;
      Dialog              : Gtk_Dialog;
      It                  : Gtk_Tree_Iter := Null_Iter;
      Renderer            : Gtk_Cell_Renderer_Text;
      Toggle_Renderer     : Gtk_Cell_Renderer_Toggle;
      Scrolled            : Gtk_Scrolled_Window;
      View                : Gtk_Tree_View;
      Col                 : Gtk_Tree_View_Column;
      Col_Num             : Gint;
      pragma Unreferenced (Col_Num);
      Label               : Gtk_Label;
      Button              : Gtk_Widget;
      Response            : Gtk_Response_Type;

      procedure Add_Child_If_Needed (Child : MDI_Child);
      --  Add the child to the model if we should ask for its saving

      procedure Save_Child (Child : MDI_Child);
      --  Save a specific child

      procedure Add_Child_If_Needed (Child : MDI_Child) is
         Module : Module_ID;
      begin
         if Child = null then
            return;
         end if;

         Module := Get_Module_From_Child (Child);

         if Module /= null
           and then Module.Info.Save_Function /= null
           and then Module.Info.Save_Function
             (Handle, Get_Widget (Child), Mode => Query)
         then
            Append (Model, It, Null_Iter);
            Set (Model, It, 0, True);
            Set (Model, It, 1, Get_Title (Child));
            Num_Unsaved := Num_Unsaved + 1;
         end if;
      end Add_Child_If_Needed;

      procedure Save_Child (Child : MDI_Child) is
         Module : constant Module_ID := Get_Module_From_Child (Child);
         Tmp    : Boolean;
         pragma Unreferenced (Tmp);

      begin
         if Module /= null
           and then Module.Info.Save_Function /= null
         then
            Tmp := Module.Info.Save_Function
              (Handle, Get_Widget (Child), Mode => Action);
         end if;
      end Save_Child;

   begin
      if Force then
         if Children /= No_Children then
            for C in Children'Range loop
               if Children (C) /= null then
                  Save_Child (Children (C));
               end if;
            end loop;

         else
            Iter := First_Child (MDI);

            loop
               Child := Get (Iter);
               exit when Child = null;
               Save_Child (Child);
               Next (Iter);
            end loop;
         end if;

         return True;
      end if;

      Gtk_New (Model, Column_Types);

      if Project_Modified (Get_Project (Handle), Recursive => True) then
         Num_Unsaved := Num_Unsaved + 1;
         Append (Model, It, Null_Iter);
         Set (Model, It, 0, True);
         Set (Model, It, 1, Project_Description);
      end if;

      if Children /= No_Children then
         for C in Children'Range loop
            Add_Child_If_Needed (Children (C));
         end loop;
      else
         Iter := First_Child (MDI);

         loop
            Child := Get (Iter);

            exit when Child = null;

            Add_Child_If_Needed (Child);
            Next (Iter);
         end loop;
      end if;

      if Num_Unsaved /= 0 then
         if Num_Unsaved = 1 then
            Gtk_New (Dialog,
                     Title  => -"Confirmation",
                     Parent => Get_Main_Window (Handle),
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Label, -"Do you want to save the following file ?");

         else
            Gtk_New (Dialog,
                     Title  => -"Saving files",
                     Parent => Get_Main_Window (Handle),
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Label, -"Do you want to save the following files ?");
         end if;

         Set_Alignment (Label, 0.0, 0.0);
         Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

         if Num_Unsaved > 1 then
            Gtk_New (Label);
            Set_Markup
              (Label,
               (-"Clicking on the ") &
                 (-"<span style=""oblique"">Select</span>") &
                 (-" label will select/unselect all"));
            Set_Alignment (Label, 0.0, 0.0);
            Pack_Start (Get_Vbox (Dialog), Label, Expand => False);
         end if;

         Gtk_New (Scrolled);
         Set_Size_Request (Scrolled, -1, 150);
         Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
         Pack_Start (Get_Vbox (Dialog), Scrolled, Padding => 10);

         Gtk_New (View, Gtk_Tree_Model (Model));
         Set_Mode (Get_Selection (View), Selection_Single);
         Add (Scrolled, View);

         Gtk_New (Renderer);
         Gtk_New (Toggle_Renderer);

         Gtk_New (Col);
         Set_Clickable (Col, True);
         Col_Num := Append_Column (View, Col);
         Set_Title (Col, -"Select");
         Pack_Start (Col, Toggle_Renderer, False);
         Add_Attribute (Col, Toggle_Renderer, "active", 0);
         Widget_Callback.Object_Connect
           (Col, "clicked",
            Widget_Callback.To_Marshaller (Select_All_Children'Access),
            Slot_Object => View);
         Widget_Callback.Object_Connect
           (Toggle_Renderer, "toggled",
            Select_Child_When_Saving'Access,
            Slot_Object => View);

         Gtk_New (Col);
         Col_Num := Append_Column (View, Col);
         Set_Clickable (Col, True);
         Set_Sort_Column_Id (Col, 1);
         Set_Title (Col, -"Title");
         Pack_Start (Col, Renderer, False);
         Add_Attribute (Col, Renderer, "text", 1);

         Button := Add_Button (Dialog, Stock_Save, Gtk_Response_Apply);
         Grab_Default (Button);
         Grab_Focus (Button);

         if Num_Unsaved = 1 then
            Button := Add_Button (Dialog, -"_Don't Save", Gtk_Response_No);
         else
            Button := Add_Button (Dialog, -"_None", Gtk_Response_No);
         end if;

         Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

         Show_All (Dialog);
         Response := Run (Dialog);

         if Response = Gtk_Response_Apply then
            It := Get_Iter_First (Model);

            while It /= Null_Iter loop
               if Get_Boolean (Model, It, 0) then
                  declare
                     Name : constant String := Get_String (Model, It, 1);
                  begin
                     if Name = Project_Description then
                        Save_Project
                          (Kernel    => Handle,
                           Project   => Get_Project (Handle),
                           Recursive => True);
                     else
                        Child := Find_MDI_Child_By_Name
                          (Get_MDI (Handle), Name);
                        Save_Child (Child);
                     end if;
                  end;
               end if;

               Next (Model, It);
            end loop;

         elsif Response /= Gtk_Response_No then
            Destroy (Dialog);
            return False;
         end if;

         Destroy (Dialog);
      else
         Unref (Model);
      end if;

      return True;
   end Save_MDI_Children;

   ------------------------
   -- Close_All_Children --
   ------------------------

   procedure Close_All_Children (Handle : access Kernel_Handle_Record) is
      MDI   : constant MDI_Window := Get_MDI (Handle);
      Iter  : Child_Iterator := First_Child (MDI);
      Child : MDI_Child;
   begin
      while Get (Iter) /= null loop
         Child := Get (Iter);
         Next (Iter);

         if Child.all not in GPS_MDI_Child_Record'Class
           or else not GPS_MDI_Child (Child).Desktop_Independent
         then
            Close_Child (Child);
         end if;
      end loop;
   end Close_All_Children;

   -------------------------------------
   -- Locate_From_Source_And_Complete --
   -------------------------------------

   function Locate_From_Source_And_Complete
     (Handle          : access Kernel_Handle_Record;
      Source_Filename : VFS.Virtual_File;
      Check_Timestamp : Boolean := True) return Src_Info.LI_File_Ptr
   is
      File : LI_File_Ptr;
      Project : constant Project_Type := Get_Project_From_File
        (Handle.Registry.all, Source_Filename);
      Handler : constant LI_Handler := Get_LI_Handler_From_File
        (Glide_Language_Handler (Handle.Lang_Handler), Source_Filename);

   begin
      if Handler = null then
         Trace (Me,
                "Locate_From_Source_And_Complete: Unsupported_Language for "
                & Base_Name (Source_Filename));
         return No_LI_File;

      else
         Create_Or_Complete_LI
           (Handler         => Handler,
            File            => File,
            Source_Filename => Source_Filename,
            List            => Handle.Source_Info_List,
            Project         => Project,
            Check_Timestamp => Check_Timestamp);
         return File;
      end if;
   end Locate_From_Source_And_Complete;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Project_Changed_Signal);
   end Project_Changed;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Project_View_Changed_Signal);
   end Project_View_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Preferences_Changed_Signal);
   end Preferences_Changed;

   ----------------------------
   -- Search_Regexps_Changed --
   ----------------------------

   procedure Search_Regexps_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Search_Regexps_Changed_Signal);
   end Search_Regexps_Changed;

   ------------------
   -- Search_Reset --
   ------------------

   procedure Search_Reset (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Search_Reset_Signal);
   end Search_Reset;

   ------------------------------
   -- Search_Functions_Changed --
   ------------------------------

   procedure Search_Functions_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Search_Functions_Changed_Signal);
   end Search_Functions_Changed;

   ----------------------
   -- Variable_Changed --
   ----------------------

   procedure Variable_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Variable_Changed_Signal);
   end Variable_Changed;

   ---------------------------
   -- Source_Lines_Revealed --
   ---------------------------

   procedure Source_Lines_Revealed
     (Handle  : access Kernel_Handle_Record;
      Context : access Selection_Context'Class)
   is
      procedure Internal
        (Handle  : System.Address;
         Signal  : String;
         Context : Selection_Context_Access);
      pragma Import (C, Internal, "g_signal_emit_by_name");

   begin
      --  ??? code duplication from Context_Changed, see below.
      Internal
        (Get_Object (Handle),
         Source_Lines_Revealed_Signal & ASCII.NUL,
         Selection_Context_Access (Context));
   end Source_Lines_Revealed;

   -----------------
   -- File_Edited --
   -----------------

   procedure File_Edited
     (Handle : access Kernel_Handle_Record;
      File   : VFS.Virtual_File)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");

      Files : File_Array_Access := Handle.Open_Files;

   begin
      Internal
        (Get_Object (Handle),
         File_Edited_Signal & ASCII.NUL,
         Full_Name (File).all & ASCII.NUL);

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
      end if;
   end File_Edited;

   ----------------
   -- File_Saved --
   ----------------

   procedure File_Saved
     (Handle  : access Kernel_Handle_Record;
      File    : VFS.Virtual_File)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");

   begin
      Internal
        (Get_Object (Handle),
         File_Saved_Signal & ASCII.NUL,
         Full_Name (File).all & ASCII.NUL);
   end File_Saved;

   -----------------
   -- File_Closed --
   -----------------

   procedure File_Closed
     (Handle  : access Kernel_Handle_Record;
      File    : VFS.Virtual_File)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");

      Files : File_Array_Access := Handle.Open_Files;

   begin
      Internal
        (Get_Object (Handle),
         File_Closed_Signal & ASCII.NUL,
         Full_Name (File).all & ASCII.NUL);

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

   --------------------------
   -- File_Changed_On_Disk --
   --------------------------

   procedure File_Changed_On_Disk
     (Handle  : access Kernel_Handle_Record;
      File    : VFS.Virtual_File)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");
   begin
      Internal
        (Get_Object (Handle),
         File_Changed_On_Disk_Signal & ASCII.NUL,
         Full_Name (File).all & ASCII.NUL);
   end File_Changed_On_Disk;

   --------------------------
   -- Compilation_Finished --
   --------------------------

   procedure Compilation_Finished
     (Handle  : access Kernel_Handle_Record;
      File    : VFS.Virtual_File)
   is
      procedure Internal
        (Handle : System.Address;
         Signal : String;
         File   : String);
      pragma Import (C, Internal, "g_signal_emit_by_name");

   begin
      Internal
        (Get_Object (Handle),
         Compilation_Finished_Signal & ASCII.NUL,
         Full_Name (File).all & ASCII.NUL);
   end Compilation_Finished;

   -------------
   -- Is_Open --
   -------------

   function Is_Open
     (Kernel   : access Kernel_Handle_Record;
      Filename : VFS.Virtual_File) return Boolean is
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
     (Kernel : access Kernel_Handle_Record) return VFS.File_Array is
   begin
      if Kernel.Open_Files /= null then
         return Kernel.Open_Files.all;
      else
         return (1 .. 0 => VFS.No_File);
      end if;
   end Open_Files;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed
     (Handle  : access Kernel_Handle_Record;
      Context : access Selection_Context'Class)
   is
      procedure Internal
        (Handle  : System.Address;
         Signal  : String;
         Context : Selection_Context_Access);
      pragma Import (C, Internal, "g_signal_emit_by_name");

      C : Selection_Context_Access := Selection_Context_Access (Context);
   begin
      Ref (C);
      Internal (Get_Object (Handle), Context_Changed_Signal & ASCII.NUL, C);
      Unref (C);
   end Context_Changed;

   ------------------------
   -- Get_Current_Module --
   ------------------------

   function Get_Current_Module
     (Kernel : access Kernel_Handle_Record) return Module_ID
   is
      C : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));

   begin
      if C = null
        or else Gtk.Object.In_Destruction_Is_Set (Get_MDI (Kernel))
      then
         return null;
      end if;

      return Get_Module_From_Child (C);
   end Get_Current_Module;

   -------------------------
   -- Get_Current_Context --
   -------------------------

   function Get_Current_Context
     (Kernel : access Kernel_Handle_Record) return Selection_Context_Access
   is
      Module : Module_ID;
   begin
      if Kernel.Current_Context /= null then
         Unref (Kernel.Current_Context);
         Kernel.Current_Context := null;
      end if;

      Module := Get_Current_Module (Kernel);

      if Module /= null
        and then Module.Info.Default_Factory /= null
      then
         Kernel.Current_Context := Module.Info.Default_Factory
           (Kernel, Get_Widget (Get_Focus_Child (Get_MDI (Kernel))));

         if Kernel.Current_Context /= null then
            Set_Context_Information (Kernel.Current_Context, Kernel, Module);
         end if;
      end if;

      return Kernel.Current_Context;
   end Get_Current_Context;

   ---------------------------
   -- Get_Context_For_Child --
   ---------------------------

   function Get_Context_For_Child
     (Kernel : access Kernel_Handle_Record;
      Child  : Gtkada.MDI.MDI_Child) return Selection_Context_Access
   is
      Module : Module_ID;
   begin
      if Child = null then
         return null;
      end if;

      Module := Get_Module_From_Child (Child);

      if Module /= null
        and then Module.Info.Default_Factory /= null
      then
         return Module.Info.Default_Factory (Kernel, Get_Widget (Child));
      else
         return null;
      end if;
   end Get_Context_For_Child;

   ------------------
   -- Save_Desktop --
   ------------------

   procedure Save_Desktop
     (Handle             : access Kernel_Handle_Record;
      As_Default_Desktop : Boolean := False)
   is
      function Get_Project_Name return String;
      --  Return the project name to match in the file

      function Get_Project_Name return String is
      begin
         if As_Default_Desktop then
            return "";
         else
            return Project_Path (Get_Project (Handle));
         end if;
      end Get_Project_Name;

      MDI          : constant MDI_Window := Get_MDI (Handle);
      File_Name    : constant String := Handle.Home_Dir.all & "desktop";
      Project_Name : constant String := Get_Project_Name;
      File         : File_Type;
      N            : Node_Ptr;
      M            : Node_Ptr;
      Old          : Node_Ptr;
      State        : Gdk_Window_State;
      X, Y         : Gint;

   begin
      --  Read the previous contents of the file, to save the desktops for
      --  other projects

      Trace (Me, "saving desktop file " & File_Name);

      if Is_Regular_File (File_Name) then
         Old := Parse (File_Name);
      end if;

      Create (File, Mode => Out_File, Name => File_Name);
      Set_Output (File);

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
              and then Get_Attribute (M, "project") /= Project_Name
            then
               Add_Child (N, Deep_Copy (M));
            end if;

            M := M.Next;
         end loop;

         Free (Old);
      end if;

      --  Add the current content, indexed on the current project

      M := Glide_Kernel.Kernel_Desktop.Save_Desktop (MDI);
      Set_Attribute (M, "project", Project_Name);
      Add_Child (N, M);

      --  Add GPS-specific nodes

      State := Get_State (Get_Window (Handle.Main_Window));

      if (State and Window_State_Maximized) = 0 then
         M       := new Node;
         M.Tag   := new String'("Width");
         M.Value := new String'
           (Allocation_Int'Image
              (Get_Allocation_Width (Handle.Main_Window)));
         Add_Child (N, M);

         M       := new Node;
         M.Tag   := new String'("Height");
         M.Value := new String'
           (Allocation_Int'Image
              (Get_Allocation_Height (Handle.Main_Window)));
         Add_Child (N, M);

         Get_Root_Origin (Get_Window (Handle.Main_Window), X, Y);

         M       := new Node;
         M.Tag   := new String'("X");
         M.Value := new String'(Gint'Image (X));
         Add_Child (N, M);

         M       := new Node;
         M.Tag   := new String'("Y");
         M.Value := new String'(Gint'Image (Y));
         Add_Child (N, M);
      end if;

      M       := new Node;
      M.Tag   := new String'("State");
      M.Value := new String'(Gdk_Window_State'Image (State));
      Add_Child (N, M);

      Print (N);
      Free (N);

      Set_Output (Standard_Output);
      Close (File);
   end Save_Desktop;

   ----------------------
   -- Has_User_Desktop --
   ----------------------

   function Has_User_Desktop
     (Handle : access Kernel_Handle_Record) return Boolean is
   begin
      return Is_Regular_File (Handle.Home_Dir.all & "desktop");
   end Has_User_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Handle : access Kernel_Handle_Record) return Boolean
   is
      MDI                  : constant MDI_Window := Get_MDI (Handle);
      Node                 : Node_Ptr;
      File                 : constant String :=
        Handle.Home_Dir.all & "desktop";
      Project_Name         : constant String :=
        Project_Path (Get_Project (Handle));
      Child                : Node_Ptr;
      Desktop_Node         : Node_Ptr;
      Default_Desktop_Node : Node_Ptr;
      Width                : Gint := 640;
      Height               : Gint := 480;
      X, Y                 : Gint := -1;
      State                : Gdk_Window_State := 0;
      Main_Window          : constant Glide_Window :=
        Glide_Window (Handle.Main_Window);
      Desktop_Loaded       : constant Boolean :=
        Main_Window.Desktop_Loaded;

   begin
      Main_Window.Desktop_Loaded := True;

      if Is_Regular_File (File) then
         Trace (Me, "loading desktop file " & File
                & " Project=" & Project_Name);
         Node := Parse (File);

         if Node /= null then
            Child := Node.Child;
         end if;

         while Child /= null loop
            if Child.Tag /= null then
               if Child.Tag.all = "MDI" then
                  if Get_Attribute (Child, "project") = "" then
                     Default_Desktop_Node := Child;
                  elsif Get_Attribute (Child, "project") = Project_Name then
                     Desktop_Node := Child;
                  end if;
               elsif Child.Tag.all = "Height" then
                  Height := Gint'Value (Child.Value.all);
               elsif Child.Tag.all = "Width" then
                  Width := Gint'Value (Child.Value.all);
               elsif Child.Tag.all = "X" then
                  X := Gint'Value (Child.Value.all);
               elsif Child.Tag.all = "Y" then
                  Y := Gint'Value (Child.Value.all);
               elsif Child.Tag.all = "State" then
                  State := Gdk_Window_State'Value (Child.Value.all);
               end if;
            end if;

            Child := Child.Next;
         end loop;

         --  Only set the main window attributes the first time, this would be
         --  too confusing to do it during an open session.

         if not Desktop_Loaded then
            Set_Default_Size (Main_Window, Width, Height);
            Set_UPosition (Main_Window, X, Y);

            if (State and Window_State_Maximized) /= 0 then
               Maximize (Main_Window);
            end if;
         end if;

         --  Call Show_All before restoring the desktop, in case some
         --  children stored in the desktop have something to hide.
         Show_All (Main_Window);

         if Desktop_Node /= null then
            Trace (Me, "loading desktop for " & Project_Name);
            Kernel_Desktop.Restore_Desktop
              (MDI, Desktop_Node, Kernel_Handle (Handle));
         elsif Default_Desktop_Node /= null then
            Trace (Me, "loading default desktop (from file)");
            Kernel_Desktop.Restore_Desktop
              (MDI, Default_Desktop_Node, Kernel_Handle (Handle));
         else
            Trace (Me, "loading default desktop");
            Kernel_Desktop.Restore_Desktop
              (MDI, Handle.Default_Desktop, Kernel_Handle (Handle));
         end if;

         Free (Node);
         return Desktop_Node /= null
           or else Default_Desktop_Node /= null;

      else
         Trace (Me, "loading default desktop");
         Show_All (Main_Window);
         Kernel_Desktop.Restore_Desktop
           (MDI, Handle.Default_Desktop, Kernel_Handle (Handle));

         return False;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Load_Desktop;

   ----------
   -- Free --
   ----------

   procedure Free (Module : in out Module_ID) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Module_ID_Record'Class, Module_ID);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Module_ID_Information, Module_ID_Information_Access);

   begin
      Destroy (Module.all);
      Unchecked_Free (Module.Info);
      Unchecked_Free (Module);
   end Free;

   -----------
   -- Unref --
   -----------

   procedure Unref (Context : in out Selection_Context_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Selection_Context'Class, Selection_Context_Access);
   begin
      if Context /= null then
         if Context.Ref_Count > 1 then
            Context.Ref_Count := Context.Ref_Count - 1;
         else
            Destroy (Context.all);
            Internal (Context);
         end if;
      end if;
   end Unref;

   ---------
   -- Ref --
   ---------

   procedure Ref (Context : Selection_Context_Access) is
   begin
      if Context /= null then
         Context.Ref_Count := Context.Ref_Count + 1;
      end if;
   end Ref;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Context : access Selection_Context) return Kernel_Handle is
   begin
      return Context.Kernel;
   end Get_Kernel;

   -----------------
   -- Get_Creator --
   -----------------

   function Get_Creator (Context : access Selection_Context)
      return Module_ID is
   begin
      return Context.Creator;
   end Get_Creator;

   -----------------------------
   -- Set_Context_Information --
   -----------------------------

   procedure Set_Context_Information
     (Context : access Selection_Context;
      Kernel  : access Kernel_Handle_Record'Class;
      Creator : Module_ID) is
   begin
      Context.Kernel := Kernel_Handle (Kernel);
      Context.Creator := Creator;
   end Set_Context_Information;

   -------------
   -- Get_MDI --
   -------------

   function Get_MDI
     (Handle : access Kernel_Handle_Record) return Gtkada.MDI.MDI_Window
   is
      Top : constant Glide_Window := Glide_Window (Handle.Main_Window);
   begin
      return Top.Process_Mdi;
   end Get_MDI;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Selection_Context) is
      pragma Unreferenced (Context);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Module_ID_Record) is
      pragma Unreferenced (Id);
   begin
      null;
   end Destroy;

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
      return Glide_Window (Handle.Main_Window).Toolbar;
   end Get_Toolbar;

   ------------------
   -- Process_Anim --
   ------------------

   function Process_Anim (Data : Process_Data) return Boolean is
      Window : constant Glide_Window := Glide_Window (Data.Kernel.Main_Window);
   begin
      if Anim_Cb (Data.Kernel) then
         Window.Timeout_Id := Process_Timeout.Add
           (Guint32 (Get_Delay_Time (Window.Animation_Iter)),
            Process_Anim'Access, Data);
      end if;

      return False;
   end Process_Anim;

   ----------------
   -- Push_State --
   ----------------

   procedure Push_State
     (Handle : Kernel_Handle;
      State  : Action_Kernel_State)
   is
      Window : Glide_Window;
   begin
      if Handle = null then
         return;
      end if;

      Window := Glide_Window (Handle.Main_Window);

      if State = Busy then
         Set_Busy_Cursor (Get_Window (Window), True, True);
         Window.Busy_Level := Window.Busy_Level + 1;
      end if;

      if Window.State_Level = 0
        and then Window.Timeout_Id = 0
        and then Window.Animation_Iter /= null
      then
         Window.Timeout_Id := Process_Timeout.Add
           (Guint32 (Get_Delay_Time (Window.Animation_Iter)),
            Process_Anim'Access,
            (Handle, null, null, null, System.Null_Address));
      end if;

      Window.State_Level := Window.State_Level + 1;
   end Push_State;

   ---------------
   -- Pop_State --
   ---------------

   procedure Pop_State (Handle : Kernel_Handle) is
      Window  : Glide_Window;
   begin
      if Handle = null then
         return;
      end if;

      Window := Glide_Window (Handle.Main_Window);

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
           and then Window.Timeout_Id /= 0
         then
            Timeout_Remove (Window.Timeout_Id);
            Window.Timeout_Id := 0;
            Display_Default_Image (Handle);
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
      return Name_As_Directory
        (Glide_Window (Handle.Main_Window).Prefix_Directory.all);
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
     (Kernel       : access Kernel_Handle_Record;
      In_Directory : String)
   is
      Handler : constant Glide_Language_Handler :=
        Glide_Language_Handler (Get_Language_Handler (Kernel));
      Num     : constant Natural := LI_Handlers_Count (Handler);
      LI      : LI_Handler;

   begin
      for L in 1 .. Num loop
         LI := Get_Nth_Handler (Handler, L);

         if LI /= null then
            Parse_All_LI_Information
              (LI,
               Kernel.Source_Info_List,
               In_Directory,
               Get_Project (Kernel));
         end if;
      end loop;
   end Parse_All_LI_Information;

   --------------------
   -- Find_Next_Body --
   --------------------

   procedure Find_Next_Body
     (Kernel      : access Kernel_Handle_Record;
      Lib_Info    : Src_Info.LI_File_Ptr;
      Entity      : Src_Info.Queries.Entity_Information;
      Location    : out Src_Info.File_Location;
      Status      : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status) is
   begin
      Find_Next_Body
        (Kernel,
         Lib_Info,
         Get_Declaration_File_Of (Entity),
         Get_Name (Entity),
         Get_Declaration_Line_Of (Entity),
         Get_Declaration_Column_Of (Entity),
         Location,
         Status);
   end Find_Next_Body;

   --------------------
   -- Find_Next_Body --
   --------------------

   procedure Find_Next_Body
     (Kernel      : access Kernel_Handle_Record;
      Lib_Info    : LI_File_Ptr;
      File_Name   : VFS.Virtual_File;
      Entity_Name : String;
      Line        : Positive;
      Column      : Positive;
      Location    : out Src_Info.File_Location;
      Status      : out Find_Decl_Or_Body_Query_Status)
   is
      Project : constant Project_Type := Get_Project_From_File
        (Kernel.Registry.all, File_Name);
      Entity : Entity_Information;

   begin
      Find_Next_Body
        (Lib_Info, File_Name, Entity_Name, Line, Column,
         Get_LI_Handler_From_File
           (Glide_Language_Handler (Kernel.Lang_Handler), File_Name),
         Kernel.Source_Info_List,
         Project,
         Location, Status);

      if Status = Overloaded_Entity_Found then
         --  Ask the user what entity he is speaking about
         Find_Declaration_Or_Overloaded
           (Kernel      => Kernel,
            Lib_Info    => Lib_Info,
            File_Name   => File_Name,
            Entity_Name => Entity_Name,
            Line        => Line,
            Column      => Column,
            Entity      => Entity,
            Status      => Status);

         --  And search for the body of that one
         if Status = Success or else Status = Fuzzy_Match then
            Find_Next_Body
              (Kernel      => Kernel,
               Lib_Info    => Lib_Info,
               File_Name   => Get_Declaration_File_Of (Entity),
               Entity_Name => Entity_Name,
               Line        => Get_Declaration_Line_Of (Entity),
               Column      => Get_Declaration_Column_Of (Entity),
               Location    => Location,
               Status      => Status);
            Destroy (Entity);
         end if;
      end if;
   end Find_Next_Body;

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
     (Kernel        : access Kernel_Handle_Record'Class;
      Lib_Info      : LI_File_Ptr;
      Entity_Name   : String;
      Decl          : out Entity_Information;
      Status        : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status)
   is
      procedure Set
        (Tree, Iter : System.Address;
         Col1 : Gint; Value1 : String;
         Col2, Value2, Col3, Value3 : Gint;
         Col4 : Gint; Value4 : String;
         Col5 : Gint; Value5 : Gint;
         Final : Gint := -1);
      pragma Import (C, Set, "gtk_tree_store_set");

      Column_Types : constant GType_Array :=
        (0 => GType_String,
         1 => GType_Int,
         2 => GType_Int,
         3 => GType_String,
         4 => GType_Int);

      Iter      : Entity_Declaration_Iterator;
      Candidate : Entity_Information;
      Button    : Gtk_Widget;
      pragma Unreferenced (Button);

      Count     : Natural := 0;

      Label     : Gtk_Label;
      Model     : Gtk_Tree_Store;
      Dialog    : Gtk_Dialog;
      It        : Gtk_Tree_Iter;
      Renderer  : Gtk_Cell_Renderer_Text;
      Scrolled  : Gtk_Scrolled_Window;
      View      : Gtk_Tree_View;
      Col       : Gtk_Tree_View_Column;
      Col_Num   : Gint;
      Index     : Gint := 1;
      pragma Unreferenced (Col_Num);

   begin
      Iter := Find_All_Possible_Declarations
        (Lib_Info    => Lib_Info,
         Entity_Name => Entity_Name);

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

            Gtk_New (View);
            Set_Mode (Get_Selection (View), Selection_Single);
            Add (Scrolled, View);

            Gtk_New (Model, Column_Types);
            Set_Model (View, Gtk_Tree_Model (Model));

            Gtk_New (Renderer);

            Gtk_New (Col);
            Col_Num := Append_Column (View, Col);
            Set_Title (Col, -"File");
            Pack_Start (Col, Renderer, False);
            Add_Attribute (Col, Renderer, "text", 0);

            Gtk_New (Col);
            Col_Num := Append_Column (View, Col);
            Set_Title (Col, -"Line");
            Pack_Start (Col, Renderer, False);
            Add_Attribute (Col, Renderer, "text", 1);

            Gtk_New (Col);
            Col_Num := Append_Column (View, Col);
            Set_Title (Col, -"Column");
            Pack_Start (Col, Renderer, False);
            Add_Attribute (Col, Renderer, "text", 2);

            Gtk_New (Col);
            Col_Num := Append_Column (View, Col);
            Set_Title (Col, -"Entity name");
            Pack_Start (Col, Renderer, False);
            Add_Attribute (Col, Renderer, "text", 3);

            Widget_Callback.Object_Connect
              (View, "row_activated",
               Widget_Callback.To_Marshaller (Row_Activated'Access),
               Dialog);

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Button := Add_Button
              (Dialog, Stock_Cancel, Gtk_Response_Cancel);
         end if;

         Append (Model, It, Null_Iter);
         Set (Get_Object (Model), It'Address,
              0, Full_Name (Get_Declaration_File_Of (Candidate)).all
              & ASCII.NUL,
              1, Gint (Get_Declaration_Line_Of (Candidate)),
              2, Gint (Get_Declaration_Column_Of (Candidate)),
              3, Entity_Name & ASCII.NUL,
              4, Index);

         Index := Index + 1;
         Destroy (Candidate);

         Next (Iter);
      end loop;

      Destroy (Iter);

      Decl := No_Entity_Information;
      Status := Entity_Not_Found;

      if Count > 0 then
         Show_All (Dialog);

         if Run (Dialog) = Gtk_Response_OK then
            Status := Success;
            Get_Selected (Get_Selection (View), Gtk_Tree_Model (Model), It);
            Index := Get_Int (Model, It, 4);
            Iter := Find_All_Possible_Declarations
              (Lib_Info    => Lib_Info,
               Entity_Name => Entity_Name);

            while Index /= 1 loop
               Next  (Iter);
            end loop;

            Decl := Get (Iter);
            Destroy (Iter);
         end if;

         Destroy (Dialog);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));

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
     (Kernel        : access Kernel_Handle_Record;
      Lib_Info      : Src_Info.LI_File_Ptr;
      File_Name     : VFS.Virtual_File;
      Entity_Name   : String;
      Line          : Positive;
      Column        : Positive;
      Entity        : out Src_Info.Queries.Entity_Information;
      Status        : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status) is
   begin
      Find_Declaration
        (Lib_Info, File_Name, Entity_Name, Line, Column, Entity, Status);

      --  ??? Should have the preference for the handling of fuzzy matches:
      --   - consider it as a no match: set Status to Entity_Not_Found;
      --   - consider it as overloaded entity: same as below;
      --   - use the closest match: nothing to do.

      if Status = Overloaded_Entity_Found then
         Select_Entity_Declaration
           (Kernel, Lib_Info, Entity_Name, Entity, Status);
      end if;
   end Find_Declaration_Or_Overloaded;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Handle : access Kernel_Handle_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (History_Record, History);
   begin
      Trace
        (Me, "Saving preferences in " & Handle.Home_Dir.all & "preferences");
      Save_Preferences (Handle, Handle.Home_Dir.all & "preferences");

      Save (Handle.History.all, Handle.Home_Dir.all & "history");
      Free (Handle.History.all);
      Unchecked_Free (Handle.History);

      Destroy (Handle.Preferences);
      Free (Handle.Gnatls_Cache);
      Free (Handle.Home_Dir);

      Destroy (Handle.Registry.all);
      Unchecked_Free (Handle.Registry);

      Free (Handle.Default_Desktop);

      if Handle.Current_Context /= null then
         Unref (Handle.Current_Context);
      end if;

      if Handle.Last_Context_For_Contextual /= null then
         Unref (Handle.Last_Context_For_Contextual);
      end if;

      Reset (Handle.Actions);
      Tools_Htable.String_Hash_Table.Reset (Handle.Tools);
      Glide_Kernel.Scripts.Finalize (Handle);

      Destroy (Glide_Language_Handler (Handle.Lang_Handler));
      Reset (Handle.Source_Info_List);
      Free (Handle.Logs_Mapper);
      Free_Modules (Handle);
      Unref (Handle.Tooltips);

      --  Free the memory allocated by gtk+, and disconnect all the callbacks,
      --  reclaiming the associated memory.
      Unref (Handle);

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

   ---------------------
   -- Scope_To_String --
   ---------------------

   function Scope_To_String (Scope : Src_Info.E_Scope) return String is
   begin
      case Scope is
         when Global_Scope => return -"global";
         when Local_Scope  => return -"local";
         when Class_Static => return -"static";
         when Static_Local => return -"static";
      end case;
   end Scope_To_String;

   --------------------
   -- Get_Scope_Tree --
   --------------------

   procedure Get_Scope_Tree
     (Kernel : access Kernel_Handle_Record;
      Entity : Entity_Information;
      Tree   : out Src_Info.Queries.Scope_Tree;
      Node   : out Src_Info.Queries.Scope_Tree_Node;
      Declarations_Only : Boolean := False)
   is
      Lib_Info : LI_File_Ptr;
      Location : File_Location;
      Status   : Find_Decl_Or_Body_Query_Status;
   begin
      Lib_Info := Locate_From_Source_And_Complete
        (Kernel, Get_Declaration_File_Of (Entity));

      if Lib_Info /= No_LI_File then
         --  We need to find the body of the entity in fact. In Ada, this
         --  will always be the same LI as the spec, but this is no
         --  longer true for C or C++.

         Find_Next_Body
           (Kernel      => Kernel,
            Lib_Info    => Lib_Info,
            File_Name   => Get_Declaration_File_Of (Entity),
            Entity_Name => Get_Name (Entity),
            Line        => Get_Declaration_Line_Of (Entity),
            Column      => Get_Declaration_Column_Of (Entity),
            Location    => Location,
            Status      => Status);

         --  In case there is no body, do nothing.
         if Location /= Null_File_Location then
            Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_File (Location));
         end if;
      end if;

      if Lib_Info = No_LI_File then
         Insert (Kernel,
                 -"LI file not found for "
                 & Full_Name (Get_Declaration_File_Of (Entity)).all);
         Tree := Null_Scope_Tree;
         Node := Null_Scope_Tree_Node;
         return;
      end if;

      Tree := Create_Tree (Lib_Info, Declarations_Only);

      if Tree = Null_Scope_Tree then
         Trace (Me, "Couldn't create scope tree for "
                & Base_Name (Get_LI_Filename (Lib_Info)));
         Node := Null_Scope_Tree_Node;
         return;
      end if;

      Node := Find_Entity_Scope (Tree, Entity);

      if Node = Null_Scope_Tree_Node then
         Insert (Kernel,
                 -"Couldn't find the scope tree for " & Get_Name (Entity));
         Trace (Me, "Couldn't find entity "
                & Get_Name (Entity) & " in "
                & Base_Name (Get_LI_Filename (Lib_Info))
                & " at line" & Get_Declaration_Line_Of (Entity)'Img
                & " column"  & Get_Declaration_Column_Of (Entity)'Img);
         Free (Tree);
         Node := Null_Scope_Tree_Node;
      end if;
   end Get_Scope_Tree;

   ---------------------
   -- Other_File_Name --
   ---------------------

   function Other_File_Name
     (Kernel          : access Kernel_Handle_Record;
      Source_Filename : VFS.Virtual_File) return VFS.Virtual_File
   is
      Project : constant Project_Type := Get_Project_From_File
        (Kernel.Registry.all, Source_Filename);
      Other : constant String :=
        Other_File_Base_Name (Project, Source_Filename);
      F : constant Virtual_File := Create (Other, Project);
   begin
      if F = VFS.No_File then
         return Create_From_Base (Other);
      end if;

      return F;
   end Other_File_Name;

   ----------------------
   -- Get_LI_File_List --
   ----------------------

   function Get_LI_File_List (Handle : access Kernel_Handle_Record)
      return Src_Info.LI_File_List is
   begin
      return Handle.Source_Info_List;
   end Get_LI_File_List;

   ---------
   -- Put --
   ---------

   function Put
     (Handle : access Kernel_Handle_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags : Child_Flags := All_Buttons;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Gint := -1;
      Module : access Module_ID_Record'Class;
      Desktop_Independent : Boolean := False) return MDI_Child
   is
      C : GPS_MDI_Child;
   begin
      if Child.all in GPS_MDI_Child_Record'Class then
         C := GPS_MDI_Child (Child);
      elsif Child.all in MDI_Child_Record'Class then
         return Put (Get_MDI (Handle), Child, Flags, Focus_Widget,
                     Default_Width, Default_Height);
      else
         C := new GPS_MDI_Child_Record;
         Initialize (C, Child, Flags);
      end if;

      C.Module := Module_ID (Module);
      C.Desktop_Independent := Desktop_Independent;
      return Put
        (Get_MDI (Handle),
         C, Flags, Focus_Widget, Default_Width, Default_Height);
   end Put;

   ----------------------
   -- Bind_Default_Key --
   ----------------------

   procedure Bind_Default_Key
     (Kernel         : access Kernel_Handle_Record;
      Action         : String;
      Default_Key    : String) is
   begin
      Add_Customization_String
        (Kernel, "<key action=""" & Action & """>" & Default_Key & "</key>");
   end Bind_Default_Key;

   ------------------------------
   -- Get_Current_Focus_Widget --
   ------------------------------

   function Get_Current_Focus_Widget
     (Kernel : access Kernel_Handle_Record) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Kernel);
      use Widget_List;
      W, W2 : Gtk_Widget;
      Toplevel : Gtk_Window;
      List, List2 : Widget_List.Glist;
   begin
      --  First check if a window currently has a grab

      W := Grab_Get_Current;
      if W /= null then
         Toplevel := Gtk_Window (Get_Toplevel (W));
         W := Get_Focus (Toplevel);
      end if;

      --  Then check all toplevel windows and stop at the one that has
      --  the focus

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

   ----------
   -- Free --
   ----------

   procedure Free (Action : in out Action_Record) is
   begin
      Commands.Destroy (Command_Access (Action.Command));
      Free (Action.Description);
   end Free;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Kernel      : access Kernel_Handle_Record;
      Name        : String;
      Command     : access Commands.Interactive.Interactive_Command'Class;
      Description : String := "";
      Filter      : Action_Filter := null) is
   begin
      Set (Kernel.Actions,
           Name,
           (Commands.Interactive.Interactive_Command_Access (Command),
            Filter,
            new String'(Description)));
   end Register_Action;

   -----------
   -- Start --
   -----------

   function Start (Kernel : access Kernel_Handle_Record'Class)
      return Action_Iterator
   is
      Iter : Action_Iterator;
   begin
      Get_First (Kernel.Actions, Iter.Iterator);
      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : in out Action_Iterator) is
   begin
      Get_Next (Kernel.Actions, Iter.Iterator);
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Action_Iterator) return String is
   begin
      return Get_Key (Iter.Iterator);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Iter : Action_Iterator) return Action_Record is
   begin
      return Get_Element (Iter.Iterator);
   end Get;

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
     (Kernel : access Kernel_Handle_Record; Name : String) return Action_Record
   is
   begin
      return Get (Kernel.Actions, Name);
   end Lookup_Action;

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

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (Filter : in out Action_Filter) is
      pragma Unreferenced (Filter);
   begin
      null;
   end Do_Nothing;

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
     (Kernel  : access Kernel_Handle_Record;
      Filter  : access Action_Filter_Record'Class;
      Name    : String) is
   begin
      Free (Filter.Name);
      Filter.Name := new String'(Name);
      Set (Kernel.Action_Filters,
           Name,
           Action_Filter (Filter));
   end Register_Filter;

   ------------
   -- Create --
   ------------

   function Create
     (Name            : Glib.UTF8_String;
      Kernel          : access Kernel_Handle_Record;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return VFS.Virtual_File is
   begin
      if Is_Absolute_Path_Or_URL (Name) then
         return Create (Full_Filename => Name);
      else
         declare
            Full : constant String := Get_Full_Path_From_File
              (Registry        => Get_Registry (Kernel),
               Filename        => Name,
               Use_Source_Path => Use_Source_Path,
               Use_Object_Path => Use_Object_Path);

         begin
            if Full /= "" then
               return Create (Full_Filename => Full);
            end if;
         end;

         --  Else just open the relative paths. This is mostly intended
         --  for files opened from the command line.
         return Create
           (Full_Filename => Locale_To_UTF8
              (Normalize_Pathname (Locale_From_UTF8 (Name))));
      end if;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (Tool : in out Tool_Properties_Record) is
   begin
      Free (Tool.Project_Package);
      Free (Tool.Project_Attribute);
      Free (Tool.Project_Index);
      Free (Tool.Initial_Cmd_Line);
   end Free;

   -------------------
   -- Register_Tool --
   -------------------

   procedure Register_Tool
     (Kernel    : access Kernel_Handle_Record;
      Tool_Name : String;
      Tool      : Tool_Properties_Record) is
   begin
      Tools_Htable.String_Hash_Table.Set (Kernel.Tools, Tool_Name, Tool);
   end Register_Tool;

   -------------------------
   -- Get_Tool_Properties --
   -------------------------

   function Get_Tool_Properties
     (Kernel    : access Kernel_Handle_Record;
      Tool_Name : String) return Tool_Properties_Record is
   begin
      return Tools_Htable.String_Hash_Table.Get (Kernel.Tools, Tool_Name);
   end Get_Tool_Properties;

   -------------------
   -- Get_Tool_Name --
   -------------------

   function Get_Tool_Name
     (Kernel    : access Kernel_Handle_Record;
      Pkg_Name  : String;
      Attribute : String;
      Index     : String) return String
   is
      Iter : Tools_Htable.String_Hash_Table.Iterator;
      Prop : Tool_Properties_Record;
   begin
      Tools_Htable.String_Hash_Table.Get_First (Kernel.Tools, Iter);
      loop
         Prop := Tools_Htable.String_Hash_Table.Get_Element (Iter);
         exit when Prop = No_Tool;

         if Prop.Project_Package /= null
           and then Prop.Project_Index /= null
           and then Prop.Project_Attribute /= null
           and then Case_Insensitive_Equal
             (Prop.Project_Package.all, Pkg_Name)
           and then Case_Insensitive_Equal
             (Prop.Project_Attribute.all, Attribute)
           and then Case_Insensitive_Equal
             (Prop.Project_Index.all, Index)
         then
            return Tools_Htable.String_Hash_Table.Get_Key (Iter);
         end if;

         Tools_Htable.String_Hash_Table.Get_Next (Kernel.Tools, Iter);
      end loop;

      return "";
   end Get_Tool_Name;

   ------------
   -- Create --
   ------------

   function Create
     (Language : String := "";
      Shell    : String := "";
      Shell_Lang : String := "Shell";
      Module     : String := "") return Base_Action_Filter
   is
      F : Base_Action_Filter :=
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

      return F;
   end Create;

   -----------
   -- "and" --
   -----------

   function "and"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Base_Action_Filter is
   begin
      return new Base_Action_Filter_Record'
        (Kind => Filter_And, Error_Msg => null, Name => null,
         And1 => Action_Filter (Filter1), And2 => Action_Filter (Filter2));
   end "and";

   ----------
   -- "or" --
   ----------

   function "or"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Base_Action_Filter is
   begin
      return new Base_Action_Filter_Record'
        (Kind => Filter_Or, Error_Msg => null, Name => null,
         Or1  => Action_Filter (Filter1), Or2 => Action_Filter (Filter2));
   end "or";

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

   function Filter_Matches_Primitive
     (Filter  : access Base_Action_Filter_Record;
      Context : Selection_Context_Access;
      Kernel  : access Kernel_Handle_Record'Class) return Boolean
   is
      Result  : Boolean := True;
   begin
      case Filter.Kind is
         when Standard_Filter =>
            if Filter.Language /= null then
               if Context = null
                 or else Context.all not in File_Selection_Context'Class
                 or else VFS.No_File = File_Information
                   (File_Selection_Context_Access (Context))
               then
                  Result := False;

               else
                  declare
                     File : constant File_Selection_Context_Access :=
                       File_Selection_Context_Access (Context);
                     Lang : constant String := Get_Language_From_File
                       (Get_Language_Handler (Kernel),
                        File_Information (File));
                  begin
                     if not Case_Insensitive_Equal
                       (Lang, Filter.Language.all)
                     then
                        Result := False;
                     end if;
                  end;
               end if;
            end if;

            if Result
              and then Filter.Module /= null
              and then
                (Context = null
                 or else not Case_Insensitive_Equal
                   (Module_Name (Get_Creator (Context)), Filter.Module.all))
            then
               Result := False;
            end if;

            if Result and then Filter.Shell /= null then
               declare
                  Lang : constant Scripting_Language :=
                    Lookup_Scripting_Language
                      (Kernel, Filter.Shell_Lang.all);
               begin
                  if Lang = null then
                     Result := False;

                  else
                     declare
                        Errors : aliased Boolean;
                        R      : constant String :=
                          Trim (Reduce (Glide_Kernel.Scripts.Execute_Command
                                          (Lang, Filter.Shell.all,
                                           Hide_Output => True,
                                           Errors => Errors'Unchecked_Access)),
                                Ada.Strings.Both);
                     begin
                        Result := not Errors
                          and then
                            (R = "1"
                             or else Case_Insensitive_Equal (R, "true"));
                     end;
                  end if;
               end;
            end if;

            return Result;

         when Filter_And =>
            return Filter_Matches (Filter.And1, Context, Kernel)
              and then Filter_Matches (Filter.And2, Context, Kernel);

         when Filter_Or =>
            return Filter_Matches (Filter.Or1, Context, Kernel)
              or else Filter_Matches (Filter.Or2, Context, Kernel);
      end case;
   end Filter_Matches_Primitive;

   --------------------
   -- Filter_Matches --
   --------------------

   function Filter_Matches
     (Filter  : Action_Filter;
      Context : Selection_Context_Access;
      Kernel  : access Kernel_Handle_Record'Class) return Boolean
   is
      Result : Boolean;
      C      : Selection_Context_Access := Context;
   begin
      Ref (C);
      Result := Filter = null
        or else Filter_Matches_Primitive (Filter, C, Kernel);
      Unref (C);
      return Result;
   end Filter_Matches;

   ------------------------
   -- Reset_LI_File_List --
   ------------------------

   procedure Reset_LI_File_List (Handle : access Kernel_Handle_Record) is
   begin
      Reset (Handle.Source_Info_List);
   end Reset_LI_File_List;

end Glide_Kernel;
