-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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

with Ada.Directories;           use Ada.Directories;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Maps;          use Ada.Strings.Maps;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Utils;    use GNATCOLL.Scripts.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Glib.Values;               use Glib.Values;
with Gtk.Accel_Group;           use Gtk.Accel_Group;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.MDI;                use Gtkada.MDI;

with Commands.Custom;           use Commands; use Commands.Custom;
with File_Utils;                use File_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Log_Utils;                 use Log_Utils;
with Projects.Registry;         use Projects.Registry;
with Remote;                    use Remote;
with String_List_Utils;         use String_List_Utils;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_View.Activities;       use VCS_View.Activities;
with VCS_Activities_View_API;   use VCS_Activities_View_API;
with VCS_Module;                use VCS_Module;
with VCS_Status;                use VCS_Status;
with VCS_Utils;                 use VCS_Utils;
with VCS_View;                  use VCS_View;
with VCS_View.Explorer;         use VCS_View.Explorer;

package body VCS_View_API is
   use type GNAT.Strings.String_Access;

   VCS_Menu_Prefix : constant String := "<gps>/VCS/";

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure List_Project_Files
     (Context   : Selection_Context;
      Recursive : Boolean);
   --  List the files contained in the project relative to Context, if any.
   --  If recursive is True, files in imported subprojects will be listed
   --  as well.

   procedure Get_Status_Project
     (Context   : Selection_Context;
      Recursive : Boolean);
   --  Display the status for the files contained in the project relative
   --  to Context, if any.
   --  If recursive is True, files in imported subprojects will be listed
   --  as well.

   procedure Update_Project
     (Context   : Selection_Context;
      Recursive : Boolean);
   --  Update the files contained in the project relative to Context, if any.
   --  If recursive is True, files in imported subprojects will be listed
   --  as well.

   procedure Query_Project_Files
     (Explorer   : VCS_Explorer_View_Access;
      Kernel     : Kernel_Handle;
      Project    : Project_Type;
      Real_Query : Boolean;
      Recursive  : Boolean);
   --  Query/List the status of files belonging to Project.
   --  If Recursive is True, files from sub-projects will also be queried.
   --  If Real_Query is True, a real VCS query will be made, otherwise
   --  the files will simply be listed.
   --  Calling this does NOT open the VCS Explorer.

   procedure Change_Context
     (Explorer : VCS_Explorer_View_Access;
      Context  : Selection_Context);
   --  Fill the explorer with files that correspond to Context.
   --  Context might be null, in which case the contents of the root project is
   --  shown.

   function Get_Files_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := True) return String_List.List;
   --  Return the list of source files in Project.
   --  If Recursive is True, then source files from all included
   --  subprojects will be returned as well.

   function Get_Dirs_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := False) return String_List.List;
   --  Return the source directories contained in Project

   function Get_Current_Ref (Kernel : Kernel_Handle) return VCS_Access;
   --  Return the VCS reference corresponding to the current context in Kernel

   function Get_Selected_Files
     (Context : Selection_Context) return String_List.List;
   --  Return the list of files that are selected, according to Context

   procedure Process_Dirs
     (Context    : Selection_Context;
      Recursive  : Boolean;
      Update     : Boolean;
      Get_Status : Boolean);
   --  Perform VCS operations on directories contained in Context

   procedure Process_Dir
     (Directory  : String;
      Ref        : VCS_Access;
      Kernel     : Kernel_Handle;
      Recursive  : Boolean;
      Update     : Boolean;
      Get_Status : Boolean);
   --  Same as above, working directly on a directory

   procedure On_Log_Action
     (Context : Selection_Context;
      Action  : VCS_Action;
      Files   : in out String_List.List);
   --  Generic callback for an action that requires associated logs. Files will
   --  be freed.

   procedure Comparison
     (Context : Selection_Context;
      One_Rev : Boolean);
   --  Factorize code between On_Menu_Diff_Specific and On_Menu_Diff2

   procedure On_Menu_Clear_Explorer
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Clear the VCS Explorer view

   --  Switch tag support

   type Switch_Tag_Dialog_Record is new Gtk_Dialog_Record with record
      Tag_Name : Gtk_GEntry;
      Dir      : Gtk_GEntry;
   end record;
   type Switch_Tag_Dialog is access all Switch_Tag_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog      : out Switch_Tag_Dialog;
      Default_Tag : String;
      Default_Dir : String);
   --  Create a new dialog for creating a new tag/branch

   --  Create tag support

   type Create_Tag_Dialog_Record is new Switch_Tag_Dialog_Record with record
      Branch : Gtk_Check_Button;
   end record;
   type Create_Tag_Dialog is access all Create_Tag_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog      : out Create_Tag_Dialog;
      Default_Dir : String);
   --  Create a new dialog for creating a new tag/branch

   package Dialog_Callback is
     new Gtk.Handlers.User_Callback (Gtk_Button_Record, Switch_Tag_Dialog);

   procedure On_Select_Dir
     (Widget : access Gtk_Button_Record'Class;
      Params : Glib.Values.GValues;
      Dialog : Switch_Tag_Dialog);

   function Get_Repository_Root (Project : Project_Type) return String;
   --  Return the repository root as defined in project, ensure that it has an
   --  ending directory separator.

   function Get_Branches_Root
     (Project    : Project_Type;
      Repository : Boolean := False) return String;
   --  Return the root directory containing the branches. This routines expects
   --  a Subversion standard layout (i.e. trunk, branches, tags). If Repository
   --  is set the returned directory is referencing the repository root instead
   --  of the local copy.

   function Get_Tags_Root
     (Project    : Project_Type;
      Repository : Boolean := False) return String;
   --  Return the root directory containing the tags. This routines expects
   --  a Subversion standard layout (i.e. trunk, branches, tags). If Repository
   --  is set the returned directory is referencing the repository root instead
   --  of the local copy.

   function Get_Repository_Path
     (Kernel : Kernel_Handle; File : Virtual_File; Tag : String) return String;
   --  Return the "trunk" repository path for File or the path to the
   --  corresponding tag/branch if specificed.

   -------------------------
   -- Get_Repository_Root --
   -------------------------

   function Get_Repository_Root (Project : Project_Type) return String is
      Rep_Root : constant String :=
                   Get_Attribute_Value (Project, VCS_Repository_Root);
   begin
      if Rep_Root = "" then
         return "";

      elsif Rep_Root (Rep_Root'Last) = '/' then
         return Rep_Root (Rep_Root'First .. Rep_Root'Last - 1);

      else
         return Rep_Root;
      end if;
   end Get_Repository_Root;

   -----------------------
   -- Get_Branches_Root --
   -----------------------

   function Get_Branches_Root
     (Project    : Project_Type;
      Repository : Boolean := False) return String
   is
      Pattern : constant String := Dir_Separator & "trunk" & Dir_Separator;

      function Rep_Root return String;
      --  Return the repository root

      function Loc_Root return String;
      --  Return the local root

      --------------
      -- Loc_Root --
      --------------

      function Loc_Root return String is
         Proj : constant Virtual_File := Project_Path (Project);
         Path : constant String := Full_Name (Proj, True).all;
         J    : constant Natural := Index (Path, Pattern);
      begin
         if J = 0 then
            return "";

         else
            return Path (Path'First .. J) & "branches";
         end if;
      end Loc_Root;

      --------------
      -- Rep_Root --
      --------------

      function Rep_Root return String is
         Rep_Root : constant String := Get_Repository_Root (Project);
      begin
         if Rep_Root = "" then
            return "";
         else
            return Rep_Root & "/branches";
         end if;
      end Rep_Root;

   begin
      if Repository then
         return Rep_Root;
      else
         return Loc_Root;
      end if;
   end Get_Branches_Root;

   -------------------
   -- Get_Tags_Root --
   -------------------

   function Get_Tags_Root
     (Project    : Project_Type;
      Repository : Boolean := False) return String
   is
      Pattern : constant String := Dir_Separator & "trunk" & Dir_Separator;

      function Rep_Root return String;
      --  Return the repository root

      function Loc_Root return String;
      --  Return the local root

      --------------
      -- Loc_Root --
      --------------

      function Loc_Root return String is
         Proj : constant Virtual_File := Project_Path (Project);
         Path : constant String := Full_Name (Proj, True).all;
         J    : constant Natural := Index (Path, Pattern);
      begin
         if J = 0 then
            return "";

         else
            return Path (Path'First .. J) & "tags";
         end if;
      end Loc_Root;

      --------------
      -- Rep_Root --
      --------------

      function Rep_Root return String is
         Rep_Root : constant String := Get_Repository_Root (Project);
      begin
         if Rep_Root = "" then
            return "";

         else
            return Rep_Root & "/tags";
         end if;
      end Rep_Root;

   begin
      if Repository then
         return Rep_Root;
      else
         return Loc_Root;
      end if;
   end Get_Tags_Root;

   -------------------------
   -- Get_Repository_Path --
   -------------------------

   function Get_Repository_Path
     (Kernel : Kernel_Handle; File : Virtual_File; Tag : String) return String
   is
      Slash     : constant Character_Mapping := To_Mapping ("\", "/");
      DS        : Character renames Dir_Separator;
      H_Pattern : constant String := DS & "trunk" & DS;
      T_Pattern : constant String := DS & "tags" & DS;
      B_Pattern : constant String := DS & "branches" & DS;
      Project   : constant Project_Type := Get_Project (Kernel);
      Rep_Root  : constant String := Get_Repository_Root (Project);
      Full      : constant String := Full_Name (File).all;
      K, P, N   : Natural;
   begin
      if Rep_Root = "" then
         --  No repository root defined
         return "";
      end if;

      if File = No_File then
         --  Just return the directory for the given tag
         return Rep_Root & Tag;
      end if;

      --  look for part after the repository path

      K := Index (Full, H_Pattern);

      if K = 0 then
         K := Index (Full, T_Pattern);

         if K = 0 then
            K := Index (Full, B_Pattern);

            if K = 0 then
               --  Unknown layout
               return "";
            end if;
         end if;
      end if;

      if Tag = "" then
         return Rep_Root & Translate (Full (K .. Full'Last), Slash);

      else
         if Full (K + 1 .. K + 5) = "trunk" then
            --  Skip trunk, and the next name which is part of the taf
            --  Note that this is true for Subversion and is the only VCS with
            --  repository PATH for now.

            P := K + 7;
            N := 1;

         else
            --  Skip tags or branches and the symbolic name
            P := K + 1;
            N := 2;
         end if;

         loop
            if Full (P) = DS then
               N := N - 1;
            end if;

            exit when N = 0;

            P := P + 1;
         end loop;

         return Rep_Root & Tag & Translate (Full (P .. Full'Last), Slash);
      end if;
   end Get_Repository_Path;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog      : out Create_Tag_Dialog;
      Default_Dir : String)
   is
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Browse : Gtk_Button;
      Button : Gtk_Widget;
      pragma Unreferenced (Button);
   begin
      Dialog := new Create_Tag_Dialog_Record;
      Gtk.Dialog.Initialize
        (Dialog,
         Title  => -"Create tag/branch",
         Parent => null,
         Flags  => Destroy_With_Parent);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => False);

      Gtk_New (Label, -"From: ");
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Dialog.Dir);
      Set_Text (Dialog.Dir, Default_Dir);
      Pack_Start (Box, Dialog.Dir);
      Gtk_New (Browse, -"Browse");
      Pack_Start (Box, Browse);
      Dialog_Callback.Connect
        (Browse, Signal_Clicked,
         On_Select_Dir'Access, Switch_Tag_Dialog (Dialog));

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => False);

      Gtk_New (Label, -"Tag name: ");
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Dialog.Tag_Name);
      Pack_Start (Box, Dialog.Tag_Name);

      Gtk_New (Dialog.Branch, -"Is a branch tag");
      Pack_Start (Get_Vbox (Dialog), Dialog.Branch, Expand => False);

      Grab_Default (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
   end Gtk_New;

   procedure Gtk_New
     (Dialog      : out Switch_Tag_Dialog;
      Default_Tag : String;
      Default_Dir : String)
   is
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Browse : Gtk_Button;
      Button : Gtk_Widget;
      pragma Unreferenced (Button);
   begin
      Dialog := new Switch_Tag_Dialog_Record;
      Gtk.Dialog.Initialize
        (Dialog,
         Title  => -"Switch tag/branch",
         Parent => null,
         Flags  => Destroy_With_Parent);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => False);

      Gtk_New (Label, -"From: ");
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Dialog.Dir);
      Set_Text (Dialog.Dir, Default_Dir);
      Pack_Start (Box, Dialog.Dir);
      Gtk_New (Browse, -"Browse");
      Pack_Start (Box, Browse);
      Dialog_Callback.Connect
        (Browse, Signal_Clicked, On_Select_Dir'Access, Dialog);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => False);

      Gtk_New (Label, -"Tag name: ");
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Dialog.Tag_Name);
      Set_Text (Dialog.Tag_Name, Default_Tag);
      Pack_Start (Box, Dialog.Tag_Name);

      Grab_Default (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
   end Gtk_New;

   -------------------
   -- On_Select_Dir --
   -------------------

   procedure On_Select_Dir
     (Widget : access Gtk_Button_Record'Class;
      Params : Glib.Values.GValues;
      Dialog : Switch_Tag_Dialog)
   is
      pragma Unreferenced (Params);
      Name : constant GNATCOLL.VFS.Virtual_File := Select_Directory
        (Title             => -"Select root directory",
         Parent            => Gtk_Window (Get_Toplevel (Widget)),
         Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
         Base_Directory    => GNATCOLL.VFS.Create (Get_Text (Dialog.Dir)));

   begin
      if Name /= GNATCOLL.VFS.No_File then
         Set_Text (Dialog.Dir, Full_Name (Name).all);
      end if;
   end On_Select_Dir;

   ----------------------------
   -- On_Menu_Remove_Project --
   ----------------------------

   procedure On_Menu_Remove_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Explorer : constant VCS_Explorer_View_Access := Get_Explorer (Kernel);
   begin
      if Has_Project_Information (Context) then
         On_Remove_Project
           (Explorer, Project_Name (Project_Information (Context)));
      else
         On_Remove_Project (Explorer, "No project");
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Remove_Project;

   ----------------------------
   -- On_Menu_Clear_Explorer --
   ----------------------------

   procedure On_Menu_Clear_Explorer
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Explorer : constant VCS_Explorer_View_Access := Get_Explorer (Kernel);
   begin
      Clear (Explorer);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Clear_Explorer;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref (Project : Project_Type) return VCS_Access is
   begin
      --  ??? maybe we could cache this information
      return Get_VCS_From_Id
        (Get_Attribute_Value (Project, VCS_Kind_Attribute));
   end Get_Current_Ref;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref (Kernel : Kernel_Handle) return VCS_Access is
      C : constant Selection_Context := Get_Current_Context (Kernel);
   begin
      if C = No_Context then
         return Get_Current_Ref (Get_Project (Kernel));
      else
         return Get_Current_Ref (C);
      end if;
   end Get_Current_Ref;

   -------------------------
   -- VCS_Contextual_Menu --
   -------------------------

   procedure VCS_Contextual_Menu
     (Kernel          : Kernel_Handle;
      Context         : Selection_Context;
      Menu            : access Gtk.Menu.Gtk_Menu_Record'Class;
      Show_Everything : Boolean)
   is
      Item            : Gtk_Menu_Item;
      Menu_Item       : Gtk_Menu_Item;
      Submenu         : Gtk_Menu;
      Ref             : VCS_Access;
      Actions         : Action_Array;

      File_Section    : Boolean;
      Dir_Section     : Boolean;
      Project_Section : Boolean;
      Section_Active  : Boolean;
      Items_Inserted  : Boolean := False;

      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);

      procedure Add_Action
        (Action   : VCS_Action;
         Callback : Context_Callback.Marshallers.Void_Marshaller.Handler;
         Via_Log  : Boolean := False);
      --  Add a menu item corresponding to Action.
      --  If Via_Log is True, ???

      procedure Add_Separator;
      --  Add a separator in the menu if needed

      function Create_Activity_Menu (Menu : Gtk_Menu) return Boolean;
      --  Return True if some activities have been added into Menu

      ----------------
      -- Add_Action --
      ----------------

      procedure Add_Action
        (Action   : VCS_Action;
         Callback : Context_Callback.Marshallers.Void_Marshaller.Handler;
         Via_Log  : Boolean := False) is
      begin
         if Actions (Action) /= null then
            if Via_Log then
               Gtk_New (Item, Actions (Action).all & (-" (via revision log)"));
            else
               Gtk_New (Item, Actions (Action).all);
            end if;

            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               Context_Callback.To_Marshaller (Callback),
               Context);

            if not Section_Active then
               Set_Sensitive (Item, False);
            end if;

            if Show_Everything then
               Set_Accel_Path
                 (Item, VCS_Menu_Prefix & Actions (Action).all, Group);
            end if;

            Items_Inserted := True;
         end if;
      end Add_Action;

      -------------------
      -- Add_Separator --
      -------------------

      procedure Add_Separator is
      begin
         if Items_Inserted then
            Gtk_New (Item);
            Append (Menu, Item);
            Items_Inserted := False;
         end if;
      end Add_Separator;

      --------------------------
      -- Create_Activity_Menu --
      --------------------------

      function Create_Activity_Menu (Menu : Gtk_Menu) return Boolean is
         Activity  : Activity_Id := First;
         Item      : Gtk_Menu_Item;
         Found     : Boolean := False;
         A_Context : Selection_Context;
      begin
         while Activity /= No_Activity loop
            if Project_Path (Get_Root_Project (Get_Registry (Kernel).all))
              = Get_Project_Path (Activity)
              and then not Is_Closed (Activity)
            then
               Found := True;
               Gtk_New (Item, Label => Emphasize (Get_Name (Activity)));
               Set_Use_Markup (Gtk_Label (Get_Child (Item)), True);
               Append (Menu, Item);

               A_Context := New_Context;

               Set_Context_Information
                 (A_Context, Kernel, Get_Creator (Context));

               Set_Activity_Information (A_Context, Image (Activity));

               Set_File_Information (A_Context, File_Information (Context));

               Context_Callback.Connect
                 (Item, Gtk.Menu_Item.Signal_Activate,
                  On_Menu_Add_To_Activity'Access,
                  A_Context);
               Set_Sensitive (Item, Section_Active);
            end if;

            Activity := Next;
         end loop;

         return Found;
      end Create_Activity_Menu;

      Log_File   : Boolean := False;
      Log_Action : VCS_Action;
      Log_Exists : Boolean;

   begin
      if Context = No_Context then
         Ref := Get_Current_Ref (Get_Project (Kernel));
      else
         Ref := Get_Current_Ref (Context);
      end if;

      if Ref = null then
         --  ??? Should add a menu item to add a VCS in the project
         return;
      end if;

      Actions := Get_Identified_Actions (Ref);

      --  Determine which sections should be displayed

      File_Section := Has_File_Information (Context);
      Dir_Section := Has_Directory_Information (Context);
      Project_Section := Has_Project_Information (Context);

      --  Look for the special case for handling log files

      if File_Section then
         declare
            File_S : constant String :=
                       Full_Name (File_Information (Context)).all;
         begin
            if File_S'Length > 4
              and then File_S (File_S'Last - 3 .. File_S'Last) = "$log"
            then
               --  By default, the log is a "commit" log
               Log_Action := Commit;

               declare
                  Index : Natural;
               begin
                  --  Attempt to read Action from the name of the log file

                  Index := File_S'Last - 4;
                  Skip_To_Char (File_S, Index, '$', -1);

                  if Index - 1 in File_S'Range then
                     Log_Action := VCS_Action'Value
                       (File_S (Index + 1 .. File_S'Last - 4));
                  end if;

               exception
                  when others =>
                     --  If the log does not describe a valid action, leave
                     --  the log action to Commit.
                     null;
               end;

               Log_File := True;
            end if;
         end;
      end if;

      if Get_Creator (Context) = Abstract_Module_ID (VCS_Module_ID) then
         Items_Inserted := True;
         Gtk_New (Item, Label => -"Expand all");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Menu_Expand_All'Access, Context);
         Set_Sensitive (Item, True);

         Gtk_New (Item, Label => -"Collapse all");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Menu_Collapse_All'Access, Context);
         Set_Sensitive (Item, True);

         if not Has_Activity_Information (Context) then
            Gtk_New (Item, Label => -"Clear view");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Clear_Explorer'Access, Context);
            Set_Sensitive (Item, True);
         end if;

         if File_Section or else Project_Section or else Dir_Section then
            Add_Separator;
         end if;
      end if;

      --  Fill the section relative to files
      --  ??? This should be done when building the context, not when filling
      --  the contextual menu...

      Section_Active := File_Section;
      Items_Inserted := False;

      if File_Section or else Show_Everything then
         if Log_File then
            declare
               Original  : constant Virtual_File :=
                             Get_File_From_Log
                               (Kernel, File_Information (Context));
               L_Context : Selection_Context;
            begin
               if Original /= GNATCOLL.VFS.No_File
                 and then Actions (Commit) /= null
               then
                  Items_Inserted := True;

                  L_Context := New_Context;

                  Set_Context_Information
                    (L_Context, Kernel, Get_Creator (Context));

                  Set_File_Information
                    (L_Context,
                     Files   => (1 => Original),
                     Project => Get_Project_From_File
                       (Get_Registry (Kernel).all, Original));

                  Gtk_New (Item, Label => Actions (Log_Action).all & " ("
                           & Krunch (Base_Name (Original)) & ")");

                  Append (Menu, Item);

                  case Log_Action is
                     when Add =>
                        Context_Callback.Connect
                          (Item, Gtk.Menu_Item.Signal_Activate,
                           On_Menu_Add'Access, L_Context);

                     when Add_No_Commit =>
                        Context_Callback.Connect
                          (Item, Gtk.Menu_Item.Signal_Activate,
                           On_Menu_Add_No_Commit'Access, L_Context);

                     when Remove =>
                        Context_Callback.Connect
                          (Item, Gtk.Menu_Item.Signal_Activate,
                           On_Menu_Remove'Access, L_Context);

                     when Remove_No_Commit =>
                        Context_Callback.Connect
                          (Item, Gtk.Menu_Item.Signal_Activate,
                           On_Menu_Remove_No_Commit'Access, L_Context);

                     when others =>
                        Context_Callback.Connect
                          (Item, Gtk.Menu_Item.Signal_Activate,
                           On_Menu_Commit'Access, L_Context);
                  end case;
               end if;
            end;

         else
            Log_Exists := Has_File_Information (Context) and then
              Get_Log_From_File
                (Kernel, File_Information (Context), False) /= No_File;

            Add_Action (Status_Files, On_Menu_Get_Status'Access);
            Add_Action (Update, On_Menu_Update'Access);

            --  Removed for files belonging to activities as we only want
            --  group commit here.

            if not Has_Activity_Information (Context) then
               Add_Action (Commit, On_Menu_Commit'Access, not Log_Exists);
            end if;

            Add_Separator;

            Add_Action (Open, On_Menu_Open'Access);
            Add_Action (History_Text, On_Menu_View_Log_Text'Access);
            Add_Action (History, On_Menu_View_Log'Access);
            Add_Action (History_Revision, On_Menu_View_Log_Rev'Access);

            Add_Separator;

            Add_Action (Diff_Head, On_Menu_Diff'Access);
            Add_Action (Diff_Working, On_Menu_Diff_Working'Access);
            Add_Action (Diff, On_Menu_Diff_Specific'Access);
            Add_Action (Diff2, On_Menu_Diff2'Access);
            Add_Action (Diff_Base_Head, On_Menu_Diff_Base_Head'Access);

            if Has_Tag_Information (Context) then
               Add_Action (Diff_Tag, On_Menu_Diff_Tag'Access);
            end if;

            if Has_Revision_Information (Context)
              and then Has_Other_Revision_Information (Context)
            then
               Gtk_New
                 (Item,
                  Label => -"Compare against previous revision ("
                              & Other_Revision_Information (Context) & ')');
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, Gtk.Menu_Item.Signal_Activate,
                  On_Menu_Diff_Other_Revision'Access, Context);
               Set_Sensitive (Item, Section_Active);
            end if;

            Add_Separator;

            if Actions (Annotate) /= null then
               Gtk_New (Item, Label => -"Add " & Actions (Annotate).all);
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, Gtk.Menu_Item.Signal_Activate,
                  On_Menu_Annotate'Access, Context);
               Set_Sensitive (Item, Section_Active);

               Gtk_New (Item, Label => -"Remove " & Actions (Annotate).all);
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, Gtk.Menu_Item.Signal_Activate,
                  On_Menu_Remove_Annotate'Access, Context);

               Set_Sensitive (Item, Section_Active);
            end if;

            Gtk_New (Item, Label => -"Edit revision log");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Edit_Log'Access, Context);
            Set_Sensitive (Item, Section_Active);

            Gtk_New (Item, Label => -"Edit global ChangeLog");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Edit_ChangeLog'Access, Context);
            Set_Sensitive (Item, Section_Active);

            Gtk_New (Item, Label => -"Remove revision log");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Remove_Log'Access, Context);
            Set_Sensitive (Item, Section_Active);

            Add_Separator;

            --  Removed for files inside activities. See previous comments

            if not Has_Activity_Information (Context) then
               Add_Action (Add, On_Menu_Add'Access, not Log_Exists);
            end if;

            Add_Action
              (Add_No_Commit, On_Menu_Add_No_Commit'Access, False);

            --  Removed for files inside activities. See previous comments

            if not Has_Activity_Information (Context) then
               Add_Action (Remove, On_Menu_Remove'Access, not Log_Exists);
            end if;

            Add_Action
              (Remove_No_Commit, On_Menu_Remove_No_Commit'Access, False);
            Add_Action (Revert, On_Menu_Revert'Access);
            Add_Action (Resolved, On_Menu_Resolved'Access);
         end if;
      end if;

      if Show_Everything
        or else (File_Section and then (Project_Section or else Dir_Section))
      then
         Add_Separator;
      end if;

      if File_Section then
         Items_Inserted := False;

         if Show_Everything or else Has_Tag_Information (Context) then
            Add_Action (Switch, On_Menu_Switch_Tag'Access);
         end if;

         if Show_Everything or else Has_Tag_Information (Context) then
            Add_Action (Merge, On_Menu_Merge'Access);
         end if;

         if Show_Everything or else Has_Revision_Information (Context) then
            Add_Action (Revision, On_Menu_View_File_Revision'Access);
         end if;

         Add_Separator;
      end if;

      --  Fill the section for the activity

      if File_Section then
         Check_Activity : declare
            Files      : constant File_Array := File_Information (Context);
            Activity   : constant Activity_Id :=
                           Get_File_Activity (File_Information (Context));
            Consistent : Boolean := True;
         begin
            --  Check that all files are belonging to the same activity (which
            --  can be no activity). This consistency check is done as the
            --  following menu should only be displayed if the file selection
            --  context is consistent about the activity.

            for K in Files'Range loop
               Consistent := Consistent
                 and then Get_File_Activity (Files (K)) = Activity;
            end loop;

            if (File_Section
                 and then not Has_Activity_Information (Context)
                 and then not Log_File
                 and then Consistent
                 and then Activity = No_Activity)
              or else Show_Everything
            then
               Items_Inserted := True;

               Gtk_New (Menu_Item, Label => -"Commit as new Activity");
               Append (Menu, Menu_Item);
               Context_Callback.Connect
                 (Menu_Item, Gtk.Menu_Item.Signal_Activate,
                  On_Menu_Commit_As_Activity'Access, Context);
               Set_Sensitive (Menu_Item, Section_Active);
            end if;

            if (File_Section
                 and then First /= No_Activity
                 and then not Has_Activity_Information (Context))
              or else Show_Everything
            then
               Items_Inserted := True;

               if Activity = No_Activity and then Consistent then
                  --  File not in an activity

                  Gtk_New (Menu_Item, Label => -"Add to Activity");
                  Append (Menu, Menu_Item);
                  Gtk_New (Submenu);
                  Set_Submenu (Menu_Item, Gtk_Widget (Submenu));

                  declare
                     Found : Boolean := False;
                  begin
                     Found := Create_Activity_Menu (Submenu);
                     Set_Sensitive (Menu_Item, Found);
                  end;

               else
                  --  Some files are not part of an activity or part of
                  --  multiple activities, we propose to remove from any
                  --  activity.

                  --  If the activities listed in selected files are not
                  --  consistent we do not list the activity name. This is
                  --  because the selected files may belong to multiple
                  --  activities. We save here a loop to buil a list of all
                  --  unique activity name.

                  if Consistent then
                     Gtk_New
                       (Menu_Item,
                        Label => -"Remove from Activity " &
                        Emphasize (Get_Name (Activity)));
                  else
                     Gtk_New
                       (Menu_Item, Label => -"Remove from Activities");
                  end if;

                  Set_Use_Markup (Gtk_Label (Get_Child (Menu_Item)), True);
                  Append (Menu, Menu_Item);
                  Context_Callback.Connect
                    (Menu_Item, Gtk.Menu_Item.Signal_Activate,
                     On_Menu_Remove_From_Activity'Access, Context);
                  Set_Sensitive (Menu_Item, Section_Active);
               end if;
            end if;
         end Check_Activity;
      end if;
      --  Fill the section relative to directory

      Section_Active := Dir_Section;

      if Show_Everything or else Dir_Section then
         if Show_Everything
           or else Project_Section
           or else File_Section
         then
            Gtk_New (Menu_Item, Label => -"Directory");
            Append (Menu, Menu_Item);
            Gtk_New (Submenu);
            Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
            Set_Sensitive (Menu_Item, Section_Active);
         else
            Submenu := Gtk_Menu (Menu);
         end if;

         if not File_Section then
            --  Add or remove a directory

            Gtk_New (Item, -"Add/No commit");
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Add_Directory_No_Commit'Access, Context);

            Gtk_New (Item, -"Remove/No commit");
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Remove_Directory_No_Commit'Access, Context);

            if not Has_Activity_Information (Context) then
               if Commit_Directory (Ref) then
                  --  Add Commit and Add to Activity menu entry only if
                  --  directories are handled by the underlying VCS.

                  Gtk_New (Item, -"Commit");
                  Append (Submenu, Item);
                  Context_Callback.Connect
                    (Item, Gtk.Menu_Item.Signal_Activate,
                     On_Menu_Commit'Access, Context);

                  declare
                     A_Menu    : Gtk_Menu;
                     Menu_Item : Gtk_Menu_Item;
                  begin
                     Gtk_New (Menu_Item, Label => -"Add to Activity");
                     Append (Submenu, Menu_Item);
                     Gtk_New (A_Menu);
                     Set_Submenu (Menu_Item, Gtk_Widget (A_Menu));

                     declare
                        Found : Boolean := False;
                     begin
                        Found := Create_Activity_Menu (A_Menu);
                        Set_Sensitive (Menu_Item, Found);
                     end;
                  end;
               end if;
            end if;

            Items_Inserted := True;
            Gtk_New (Item);
            Append (Submenu, Item);
         end if;

         if Actions (Status_Files) /= null then
            Gtk_New
              (Item,
               Label => Actions (Status_Files).all & (-" for directory"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Get_Status_Dir'Access, Context);
            Set_Sensitive (Item, Section_Active);

            Items_Inserted := True;
         end if;

         if Actions (Update) /= null then
            Gtk_New
              (Item, Label => Actions (Update).all & (-" directory"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate, On_Menu_Update_Dir'Access,
               Context);
            Set_Sensitive (Item, Section_Active);

            Items_Inserted := True;
         end if;

         if Actions (Status_Files) /= null then
            Gtk_New
              (Item, Label => Actions (Status_Files).all
               & (-" for directory recursively"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Get_Status_Dir_Recursive'Access, Context);
            Set_Sensitive (Item, Section_Active);

            Items_Inserted := True;
         end if;

         if Actions (Update) /= null then
            Gtk_New
              (Item, Label => Actions (Update).all
               & (-" directory recursively"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Update_Dir_Recursive'Access, Context);
            Set_Sensitive (Item, Section_Active);

            Items_Inserted := True;
         end if;

         if Show_Everything
           or else Project_Section
           or else File_Section
         then
            Set_Sensitive (Menu_Item, Section_Active and then Items_Inserted);
         end if;
      end if;

      if Show_Everything
        or else ((File_Section or else Dir_Section) and then Project_Section)
      then
         Add_Separator;
      end if;

      --  Fill the section relative to project

      Section_Active := Project_Section;

      if Show_Everything or else Project_Section then
         if Show_Everything
           or else Dir_Section
           or else File_Section
         then
            Gtk_New (Menu_Item, Label => -"Project");
            Append (Menu, Menu_Item);
            Gtk_New (Submenu);
            Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
         else
            Submenu := Gtk_Menu (Menu);
         end if;

         Items_Inserted := True;

         Gtk_New (Item, Label => -"List all files in project");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Menu_List_Project_Files'Access, Context);

         if Actions (Status_Files) /= null then
            Gtk_New
              (Item, Label => Actions (Status_Files).all & (-" for project"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Get_Status_Project'Access, Context);
            Set_Sensitive (Item, Section_Active);
         end if;

         if Actions (Update) /= null then
            Gtk_New (Item, Label => Actions (Update).all & (-" project"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Update_Project'Access, Context);
            Set_Sensitive (Item, Section_Active);
         end if;

         Gtk_New (Item, Label => -"List all files in project and subprojects");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Menu_List_Project_Files_Recursive'Access, Context);

         if Actions (Status_Files) /= null then
            Gtk_New
              (Item, Label => Actions (Status_Files).all &
                 (-" for project and subprojects"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Get_Status_Project_Recursive'Access, Context);
            Set_Sensitive (Item, Section_Active);
         end if;

         if Actions (Update) /= null then
            Gtk_New
              (Item, Label => Actions (Update).all
               & (-" project and subprojects"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               On_Menu_Update_Project_Recursive'Access, Context);
            Set_Sensitive (Item, Section_Active);
         end if;

         if Show_Everything
           or else Dir_Section
           or else File_Section
         then
            Set_Sensitive (Menu_Item, Section_Active);
         end if;
      end if;

      if (File_Section
          and then Get_Creator (Context) = Abstract_Module_ID (VCS_Module_ID))
        or else Show_Everything
      then
         Add_Separator;
         Gtk_New (Item, Label => -"Select files same status");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Menu_Select_Files_Same_Status'Access, Context);
         Set_Sensitive (Item, True);
      end if;
   end VCS_Contextual_Menu;

   --------------------
   -- Change_Context --
   --------------------

   procedure Change_Context
     (Explorer : VCS_Explorer_View_Access;
      Context  : Selection_Context)
   is
      Status : File_Status_List.List;
      Dirs   : String_List.List;
      Ref    : VCS_Access;
      use String_List;
   begin
      if Explorer = null then
         return;
      end if;

      if Context = No_Context then
         Query_Project_Files
           (Explorer,
            Get_Kernel (Explorer),
            Get_Project (Get_Kernel (Explorer)),
            False, False);
         return;
      end if;

      Ref := Get_Current_Ref (Context);
      Set_Current_Context (Explorer, Context);

      if Has_Directory_Information (Context)
        and then not Has_File_Information (Context)
      then
         String_List.Append (Dirs, Directory_Information (Context));
         Status := Local_Get_Status (Ref, Dirs);
         String_List.Free (Dirs);
         Display_File_Status
           (Get_Kernel (Context), Status, Ref, False, True);
         File_Status_List.Free (Status);
         String_List.Free (Dirs);

      elsif Has_Project_Information (Context)
        and then not Has_Directory_Information (Context)
      then
         Query_Project_Files
           (Explorer,
            Get_Kernel (Context),
            Project_Information (Context),
            False, False);

      elsif Has_File_Information (Context) then
         Query_Project_Files
           (Explorer,
            Get_Kernel (Context),
            Get_Project_From_File
              (Registry          => Get_Registry (Get_Kernel (Context)).all,
               Source_Filename   => File_Information (Context),
               Root_If_Not_Found => True),
            False, False);

      else
         Query_Project_Files
           (Explorer,
            Get_Kernel (Context),
            Get_Project (Get_Kernel (Context)),
            False, False);
      end if;
   end Change_Context;

   -------------------
   -- Open_Explorer --
   -------------------

   procedure Open_Explorer
     (Kernel  : Kernel_Handle;
      Context : Selection_Context)
   is
      Explorer : VCS_Explorer_View_Access;
   begin
      Explorer := Get_Explorer (Kernel, True, True);
      Change_Context (Explorer, Context);
   end Open_Explorer;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Context : Selection_Context) return String_List.List
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Explorer : VCS_Explorer_View_Access;
      List     : String_List.List;
   begin
      if Has_File_Information (Context) then
         if Get_Creator (Context) = Abstract_Module_ID (VCS_Module_ID) then
            if Has_Activity_Information (Context) then
               --  This is a selection from the Activities Explorer
               List := VCS_View.Activities.Get_Selected_Files (Kernel);

            else
               Explorer := Get_Explorer (Kernel, False);
               List := Get_Selected_Files (VCS_View_Access (Explorer));
            end if;

         else
            String_List.Append
              (List, Full_Name (File_Information (Context)).all);
         end if;
      end if;

      return List;
   end Get_Selected_Files;

   ----------------------------
   -- On_Menu_Edit_ChangeLog --
   ----------------------------

   procedure On_Menu_Edit_ChangeLog
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      List   : String_List.List       := Get_Selected_Files (Context);

      procedure Get_Location
        (File, ChangeLog_File : Virtual_File;
         Line, Column         : out Natural);
      --  Returns the line/column where the cursor needs to be located.
      --  This function must be called only when the global ChangeLog file
      --  contains an entry for file.

      ------------------
      -- Get_Location --
      ------------------

      procedure Get_Location
        (File, ChangeLog_File : Virtual_File;
         Line, Column         : out Natural)
      is
         Filename           : constant String := Base_Name (File);
         --  The filename to look for in the ChangeLog file

         ChangeLog_Filename : aliased String := Full_Name (ChangeLog_File).all;
         --  The global ChangeLog file

         Last               : Natural;
         Entry_Found        : Boolean;

      begin
         Line   := 1;
         Column := 0;

         --  Get last line in the file

         Last := Natural'Value
           (Execute_GPS_Shell_Command
              (Kernel,
               "Editor.get_last_line",
               (1 => ChangeLog_Filename'Unchecked_Access)));

         --  First, look for the filename entry

         loop
            declare
               L_Img  : aliased String  := Image (Line);
               B_Line : constant String :=
                 Execute_GPS_Shell_Command
                   (Kernel,
                    "Editor.get_chars",
                    (ChangeLog_Filename'Unchecked_Access,
                     L_Img'Unchecked_Access));

            begin
               Entry_Found := Index (B_Line, Filename) /= 0;

               exit when Entry_Found or else Line = Last;

               Line := Line + 1;
            end;
         end loop;

         --  Look for the first empty line, this is where the cursor
         --  needs to be set.

         if not Entry_Found then
            --  No entry found, this should not happen, returns the first
            --  position in the file.
            Line   := 1;
            Column := 1;
            return;
         end if;

         Line := Line + 1;

         loop
            declare
               L_Img   : aliased String := Image (Line);
               B_Line  : constant String :=
                 Execute_GPS_Shell_Command
                   (Kernel,
                    "Editor.get_chars",
                    (ChangeLog_Filename'Unchecked_Access,
                     L_Img'Unchecked_Access));
               Is_Empty : Boolean := True;
            begin
               for K in B_Line'Range loop
                  if B_Line (K) /= ' '
                    and then B_Line (K) /= ASCII.HT
                    and then B_Line (K) /= ASCII.CR
                    and then B_Line (K) /= ASCII.LF
                  then
                     Is_Empty := False;
                     exit;
                  end if;
               end loop;

               if Is_Empty then
                  if B_Line'Length = 0
                    or else B_Line (B_Line'First) = ASCII.LF
                  then
                     --  An empty line, insert an HT

                     declare
                        L_Img : aliased String := Image (Line);
                        C_Img : aliased String := "1";
                        Text1 : aliased String :=
                          String'(1 => ASCII.HT);
                        Text2 : aliased String :=
                          ASCII.HT & ASCII.LF & ASCII.LF;
                        Args  : GNAT.OS_Lib.Argument_List (1 .. 4);

                     begin
                        Args (1) := ChangeLog_Filename'Unchecked_Access;
                        Args (2) := L_Img'Unchecked_Access;
                        Args (3) := C_Img'Unchecked_Access;

                        if Line >= Last then
                           --  This is the end of the file
                           Args (4) := Text1'Unchecked_Access;
                        else
                           Args (4) := Text2'Unchecked_Access;
                        end if;

                        Execute_GPS_Shell_Command
                          (Kernel, "Editor.replace_text", Args);
                     end;

                     Column := 2;

                  elsif B_Line (B_Line'First) = ASCII.HT then
                     --  A line with a single HT, place cursor just after
                     Column := 2;

                  else
                     --  Only spaces, put cursor at the end of the line
                     Column := B_Line'Last;
                  end if;

                  exit;
               end if;

               Line := Line + 1;
            end;
         end loop;
      end Get_Location;

   begin
      while not String_List.Is_Empty (List) loop
         declare
            File           : constant Virtual_File :=
                               Create (String_List.Head (List));
            ChangeLog_File : constant Virtual_File :=
                               Get_ChangeLog_From_File (Kernel, File);
            Already_Open   : Boolean;
            Line, Column   : Natural;
         begin
            Already_Open := Is_Open (Kernel, ChangeLog_File);
            Open_File_Editor (Kernel, ChangeLog_File);

            --  At this point we know that there is an entry for the current
            --  file for the current data into the ChangeLog file. Set the
            --  cursor location at the right position.

            Get_Location (File, ChangeLog_File, Line, Column);

            declare
               L_Img              : aliased String := Image (Line);
               C_Img              : aliased String := Image (Column);
               ChangeLog_Filename : aliased String :=
                 Full_Name (ChangeLog_File).all;

            begin
               Execute_GPS_Shell_Command
                 (Kernel,
                  "Editor.edit",
                  (ChangeLog_Filename'Unchecked_Access,
                   L_Img'Unchecked_Access, C_Img'Unchecked_Access));
            end;

            if not Already_Open then
               Split (Get_MDI (Kernel), Gtk.Enums.Orientation_Vertical,
                      Reuse_If_Possible => True, After => True);
            end if;
         end;

         String_List.Next (List);
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Edit_ChangeLog;

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      List   : String_List.List;
      Kernel : Kernel_Handle;

   begin
      Kernel := Get_Kernel (Context);
      List   := Get_Selected_Files (Context);

      while not String_List.Is_Empty (List) loop
         declare
            File : constant Virtual_File :=
                     Create (String_List.Head (List));
         begin
            Get_Log_From_ChangeLog (Kernel, File);

            Open_File_Editor
              (Kernel,
               Get_Log_From_File (Kernel, File, True),
               Group            => Group_Consoles,
               Initial_Position => Position_Bottom);
         end;

         String_List.Next (List);
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Edit_Log;

   ------------------------
   -- On_Menu_Create_Tag --
   ------------------------

   procedure On_Menu_Create_Tag
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context);
      Project : constant Project_Type := Get_Project (Kernel);
      Dialog  : Create_Tag_Dialog;
   begin
      Gtk_New (Dialog, Full_Name (Project_Directory (Project)).all);
      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Dir       : constant String := Get_Text (Dialog.Dir);
            Tag       : constant String := Get_Text (Dialog.Tag_Name);
            As_Branch : constant Boolean := Get_Active (Dialog.Branch);
         begin
            Create_Tag
              (Get_Current_Ref (Context), Create (Dir), Tag, As_Branch);
         end;
      end if;

      Destroy (Dialog);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Create_Tag;

   ------------------------
   -- On_Menu_Switch_Tag --
   ------------------------

   procedure On_Menu_Switch_Tag
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context);
      Project : constant Project_Type := Get_Project (Kernel);
      Dialog  : Switch_Tag_Dialog;
   begin
      if Has_Tag_Information (Context) then
         Gtk_New
           (Dialog,
            Tag_Information (Context),
            Full_Name (Project_Directory (Project)).all);
      else
         Gtk_New (Dialog, "", Full_Name (Project_Directory (Project)).all);
      end if;

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Dir : constant String := Get_Text (Dialog.Dir);
            Tag : constant String := Get_Text (Dialog.Tag_Name);
         begin
            Switch (Get_Current_Ref (Context), Create (Dir), Tag);
         end;
      end if;

      Destroy (Dialog);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Switch_Tag;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref
     (Context : Selection_Context) return VCS_Access
   is
      Prj : Project_Type;
   begin
      if Context = No_Context then
         return Get_VCS_From_Id ("");

      elsif Has_Project_Information (Context) then
         --  If the context has project information, retrieve the VCS for that
         --  project
         return Get_Current_Ref (Project_Information (Context));

      elsif Has_File_Information (Context) then
         --  If the context has a file information, try to find the project
         --  for this file.

         Prj := Get_Project_From_File
           (Get_Registry (Get_Kernel (Context)).all,
            File_Information (Context), False);

         if Prj /= No_Project then
            return Get_Current_Ref (Prj);
         end if;
      end if;

      --  If all else fails, fallback on the VCS for the root project
      return Get_Current_Ref (Get_Project (Get_Kernel (Context)));
   end Get_Current_Ref;

   ------------------------
   -- On_Menu_Remove_Log --
   ------------------------

   procedure On_Menu_Remove_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      use String_List;
      Kernel     : constant Kernel_Handle := Get_Kernel (Context);
      Files      : String_List.List;
      Files_Temp : String_List.List_Node;
      Explorer   : VCS_Explorer_View_Access;
      A_Explorer : VCS_Activities_View_Access;

   begin
      Files := Get_Selected_Files (Context);
      Explorer := Get_Explorer (Kernel, False);
      A_Explorer := Get_Activities_Explorer (Kernel, False);
      Files_Temp := First (Files);

      while Files_Temp /= Null_Node loop
         declare
            File    : constant Virtual_File :=
                        Create (Full_Filename => Data (Files_Temp));
            Log     : constant Virtual_File :=
                        Get_Log_From_File (Kernel, File, False);
            Success : Boolean;
         begin
            if Is_Regular_File (Log) then
               Delete (Log, Success);
               Close_File_Editors (Kernel, Log);
            end if;

            Remove_File_From_Mapping (Kernel, File);

            if Explorer /= null then
               Refresh_Log (Explorer, File);
            end if;

            if A_Explorer /= null then
               --  ??? revisit when child package implemented
               VCS_View.Refresh_Log
                 (VCS_View.VCS_View_Access (A_Explorer), File);
            end if;
         end;

         Files_Temp := Next (Files_Temp);
      end loop;

      Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Remove_Log;

   --------------------
   -- On_Menu_Commit --
   --------------------

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
   begin
      if Has_Directory_Information (Context)
        and then not Has_File_Information (Context)
      then
         --  This is a directory, to commit we need to remove the trailing
         --  directory separator.

         declare
            Dir : constant String := Directory_Information (Context);
         begin
            String_List.Append (Files, Dir (Dir'First .. Dir'Last - 1));
         end;

      else
         Files := Get_Selected_Files (Context);
      end if;

      On_Log_Action (Context, Commit, Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Commit;

   -------------------
   -- On_Log_Action --
   -------------------

   procedure On_Log_Action
     (Context : Selection_Context;
      Action  : VCS_Action;
      Files   : in out String_List.List)
   is
      Kernel         : constant Kernel_Handle := Get_Kernel (Context);
      Suffix         : constant String := Action_To_Log_Suffix (Action);
      Real_Files     : String_List.List;
      Files_Temp     : String_List.List_Node;
      All_Logs_Exist : Boolean := True;
      File           : Virtual_File;

      use String_List;
   begin
      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot commit", Mode => Error);
         return;
      end if;

      --  Add all files in Files, if we have a log selected then we add the
      --  corresponding file.

      Files_Temp := String_List.First (Files);

      while Files_Temp /= Null_Node loop
         declare
            S : constant String := Data (Files_Temp);
         begin
            if S'Length > 4
              and then S (S'Last - 3 .. S'Last) = "$log"
            then
               --  This is a log file, add the corresponding file
               declare
                  L : constant Virtual_File :=
                        Get_File_From_Log
                          (Kernel, Create (Full_Filename => S));
               begin
                  if L /= GNATCOLL.VFS.No_File then
                     Append (Real_Files, Full_Name (L).all);
                  end if;
               end;

            else
               --  Not a log file
               Append (Real_Files, S);
            end if;
         end;

         Files_Temp := Next (Files_Temp);
      end loop;

      Files_Temp := String_List.First (Real_Files);

      --  Open log editors for files that don't have a log

      while Files_Temp /= String_List.Null_Node loop
         File := Create (Full_Filename => String_List.Data (Files_Temp));

         if Get_Log_From_File (Kernel, File, False) = GNATCOLL.VFS.No_File then
            Get_Log_From_ChangeLog (Kernel, File, Suffix);
            All_Logs_Exist := False;

            Open_File_Editor
              (Kernel,
               Get_Log_From_File (Kernel, File, True, Suffix),
               Group            => Group_Consoles,
               Initial_Position => Position_Bottom);
         end if;

         Files_Temp := String_List.Next (Files_Temp);
      end loop;

      --  If All files have a log, commit the whole lot

      if All_Logs_Exist then
         Log_Action_Files
           (Kernel, Get_Current_Ref (Context),
            Action, Real_Files, No_Activity);
      end if;

      String_List.Free (Real_Files);
      String_List.Free (Files);
   end On_Log_Action;

   ------------------
   -- On_Menu_Open --
   ------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      List : String_List.List;

      Ref  : VCS_Access;
   begin
      List := Get_Selected_Files (Context);

      if String_List.Is_Empty (List) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot open file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Open (Ref, List);
      Get_Status (Ref, List);
      String_List.Free (List);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Open;

   -----------------
   -- On_Menu_Add --
   -----------------

   procedure On_Menu_Add
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Context);
   begin
      On_Log_Action (Context, Add, Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Add;

   -------------------------------------
   -- On_Menu_Add_Directory_No_Commit --
   -------------------------------------

   procedure On_Menu_Add_Directory_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      Ref   : VCS_Access;
      Dir   : constant String := Directory_Information (Context);
   begin
      --  Do not pass the ending directory separator as we really want the last
      --  part to be the name we add.

      String_List.Append (Files, Dir (Dir'First .. Dir'Last - 1));

      Ref := Get_Current_Ref (Context);

      Add (Ref, Files, Log => "", Commit => False);

      String_List.Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Add_Directory_No_Commit;

   ---------------------------
   -- On_Menu_Add_No_Commit --
   ---------------------------

   procedure On_Menu_Add_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot remove file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Add (Ref, Files, Log => "", Commit => False);
      Get_Status (Ref, Files);

      String_List.Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Add_No_Commit;

   --------------------
   -- On_Menu_Revert --
   --------------------

   procedure On_Menu_Revert
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot remove file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Revert (Ref, Files);
      Get_Status (Ref, Files);

      String_List.Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Revert;

   ----------------------
   -- On_Menu_Resolved --
   ----------------------

   procedure On_Menu_Resolved
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context),
            -"VCS: No file selected, cannot mark file as resolved",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Resolved (Ref, Files);
      Get_Status (Ref, Files);

      String_List.Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Resolved;

   --------------------
   -- On_Menu_Remove --
   --------------------

   procedure On_Menu_Remove
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Context);
   begin
      On_Log_Action (Context, Remove, Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Remove;

   ----------------------------------------
   -- On_Menu_Remove_Directory_No_Commit --
   ----------------------------------------

   procedure On_Menu_Remove_Directory_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      Ref   : VCS_Access;
      Dir   : constant String := Directory_Information (Context);
   begin
      --  Do not pass the ending directory separator as we really want the last
      --  part to be the name we add.

      String_List.Append (Files, Dir (Dir'First .. Dir'Last - 1));

      Ref := Get_Current_Ref (Context);

      Remove (Ref, Files, Log => "", Commit => False);

      String_List.Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Remove_Directory_No_Commit;

   ------------------------------
   -- On_Menu_Remove_No_Commit --
   ------------------------------

   procedure On_Menu_Remove_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot remove file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Remove (Ref, Files, Log => "", Commit => False);
      Get_Status (Ref, Files);

      String_List.Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Remove_No_Commit;

   ----------------------
   -- On_Menu_Annotate --
   ----------------------

   procedure On_Menu_Annotate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      File  : Virtual_File;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot annotate",
            Mode => Error);
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         File := Create (Full_Filename => String_List.Head (Files));

         if not Is_Open (Get_Kernel (Context), File) then
            Open_File_Editor (Get_Kernel (Context), File);
         end if;

         Create_Line_Information_Column
           (Kernel        => Get_Kernel (Context),
            File          => File,
            Identifier    => Annotation_Id,
            Every_Line    => False);

         Annotate (Get_Current_Ref (Context), File);
         String_List.Next (Files);
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Annotate;

   -----------------------------
   -- On_Menu_Remove_Annotate --
   -----------------------------

   procedure On_Menu_Remove_Annotate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Files  : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot remove annotations",
            Mode => Error);
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Remove_Line_Information_Column
           (Kernel,
            Create (Full_Filename => String_List.Head (Files)),
            Annotation_Id);
         String_List.Next (Files);
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Remove_Annotate;

   --------------------
   -- On_Menu_Update --
   --------------------

   procedure On_Menu_Update
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Ref   : constant VCS_Access := Get_Current_Ref (Context);
      Files : String_List.List;

   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot update file",
            Mode => Error);
         return;
      end if;

      Update (Ref, Files);
      Get_Status (Ref, Files);
      String_List.Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Update;

   -------------------
   -- On_Menu_Merge --
   -------------------

   procedure On_Menu_Merge
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Ref   : constant VCS_Access := Get_Current_Ref (Context);
      Files : String_List.List;
   begin
      if Has_Tag_Information (Context) then
         Files := Get_Selected_Files (Context);
         Merge (Ref, Files, Tag_Information (Context));
         String_List.Free (Files);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Merge;

   ------------------------
   -- On_Menu_Get_Status --
   ------------------------

   procedure On_Menu_Get_Status
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Files  : String_List.List;

   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot get status",
            Mode => Error);
         return;
      end if;

      Open_Explorer (Kernel, Context);
      Get_Status (Get_Current_Ref (Context), Files);
      String_List.Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Get_Status;

   -----------------
   -- Process_Dir --
   -----------------

   procedure Process_Dir
     (Directory  : String;
      Ref        : VCS_Access;
      Kernel     : Kernel_Handle;
      Recursive  : Boolean;
      Update     : Boolean;
      Get_Status : Boolean)
   is
      Files : String_List.List;

   begin
      --  Do not process hidden directories
      if Is_Hidden (Kernel, Simple_Name (Directory)) then
         return;
      end if;

      String_List.Append (Files, Directory);

      if Update then
         VCS.Update (Ref, Files);
      end if;

      if Get_Status then
         if Recursive then
            VCS.Get_Status_Dirs_Recursive (Ref, Files);
         else
            VCS.Get_Status_Dirs (Ref, Files);
         end if;
      end if;

      String_List.Free (Files);
   end Process_Dir;

   ------------------
   -- Process_Dirs --
   ------------------

   procedure Process_Dirs
     (Context    : Selection_Context;
      Recursive  : Boolean;
      Update     : Boolean;
      Get_Status : Boolean) is
   begin
      Open_Explorer (Get_Kernel (Context), Context);

      if Has_Directory_Information (Context) then
         Process_Dir
           (Directory_Information (Context),
            Get_Current_Ref (Context),
            Get_Kernel (Context),
            Recursive, Update, Get_Status);
      end if;
   end Process_Dirs;

   ------------------------
   -- On_Menu_Update_Dir --
   ------------------------

   procedure On_Menu_Update_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

   begin
      Process_Dirs
        (Context, Recursive => False, Update => True, Get_Status => True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Update_Dir;

   ----------------------------------
   -- On_Menu_Update_Dir_Recursive --
   ----------------------------------

   procedure On_Menu_Update_Dir_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Process_Dirs
        (Context, Recursive => True, Update => True, Get_Status => True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Update_Dir_Recursive;

   ----------------------------
   -- On_Menu_Get_Status_Dir --
   ----------------------------

   procedure On_Menu_Get_Status_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Process_Dirs
        (Context, Recursive => False, Update => False, Get_Status => True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Get_Status_Dir;

   --------------------------------------
   -- On_Menu_Get_Status_Dir_Recursive --
   --------------------------------------

   procedure On_Menu_Get_Status_Dir_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Process_Dirs
        (Context, Recursive => True, Update => False, Get_Status => True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Get_Status_Dir_Recursive;

   --------------------
   -- Update_Project --
   --------------------

   procedure Update_Project
     (Context   : Selection_Context;
      Recursive : Boolean)
   is
      Files : String_List.List;
      Ref   : constant VCS_Access := Get_Current_Ref (Context);

   begin
      Open_Explorer (Get_Kernel (Context), Context);

      if Has_Project_Information (Context) then
         Files := Get_Files_In_Project
           (Project_Information (Context), Recursive);
      else
         Files := Get_Files_In_Project
           (Get_Project (Get_Kernel (Context)), Recursive);
      end if;

      Update (Ref, Files);
      Get_Status (Ref, Files);

      String_List.Free (Files);
   end Update_Project;

   ----------------------------
   -- On_Menu_Update_Project --
   ----------------------------

   procedure On_Menu_Update_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Update_Project (Context, False);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Update_Project;

   --------------------------------------
   -- On_Menu_Update_Project_Recursive --
   --------------------------------------

   procedure On_Menu_Update_Project_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Update_Project (Context, True);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Update_Project_Recursive;

   -------------------------
   -- Query_Project_Files --
   -------------------------

   procedure Query_Project_Files
     (Explorer   : VCS_Explorer_View_Access;
      Kernel     : Kernel_Handle;
      Project    : Project_Type;
      Real_Query : Boolean;
      Recursive  : Boolean)
   is
      pragma Unreferenced (Explorer);

      procedure Query_Status_For_Project (The_Project : Project_Type);
      --  Display the status for The_Project only

      ------------------------------
      -- Query_Status_For_Project --
      ------------------------------

      procedure Query_Status_For_Project (The_Project : Project_Type) is
         use String_List;
         Ref    : constant VCS_Access := Get_Current_Ref (The_Project);
         Status : File_Status_List.List;
         Files  : String_List.List;

      begin
         if Ref = Unknown_VCS_Reference then
            Insert
              (Kernel,
               -"Warning: no VCS set in project properties for project "
               & Project_Name (The_Project));

         else
            if Real_Query then
               if Group_Query_Status_By_Dir (Ref) then
                  declare
                     Dirs : String_List.List;
                     Node : String_List.List_Node;
                  begin
                     Dirs := Get_Dirs_In_Project (The_Project, False);
                     Node := First (Dirs);

                     while Node /= Null_Node loop
                        Process_Dir
                          (Directory  => Data (Node),
                           Ref        => Ref,
                           Kernel     => Kernel,
                           Recursive  => False,
                           Update     => False,
                           Get_Status => True);
                        Node := Next (Node);
                     end loop;

                     Free (Dirs);
                  end;

               else
                  Files := Get_Files_In_Project (The_Project, False);
                  Get_Status (Ref, Files);
                  String_List.Free (Files);
               end if;

            else
               Files := Get_Files_In_Project (The_Project, False);
               Status := Local_Get_Status (Ref, Files);
               Display_File_Status (Kernel, Status, Ref, False, True);
               File_Status_List.Free (Status);
               String_List.Free (Files);
            end if;
         end if;
      end Query_Status_For_Project;

      Iterator        : Imported_Project_Iterator :=
                          Start (Project, Recursive);
      Current_Project : Project_Type := Current (Iterator);
   begin
      while Current_Project /= No_Project loop
         Query_Status_For_Project (Current_Project);
         Next (Iterator);
         Current_Project := Current (Iterator);
      end loop;
   end Query_Project_Files;

   ------------------------
   -- List_Project_Files --
   ------------------------

   procedure List_Project_Files
     (Context   : Selection_Context;
      Recursive : Boolean)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      Open_Explorer (Get_Kernel (Context), Context);

      if Has_Project_Information (Context) then
         Query_Project_Files
           (Get_Explorer (Kernel),
            Kernel,
            Project_Information (Context),
            False, Recursive);
      else
         Query_Project_Files
           (Get_Explorer (Kernel),
            Kernel,
            Get_Project (Kernel),
            False, Recursive);
      end if;
   end List_Project_Files;

   --------------------------------
   -- On_Menu_List_Project_Files --
   --------------------------------

   procedure On_Menu_List_Project_Files
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      List_Project_Files (Context, False);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_List_Project_Files;

   ------------------------------------------
   -- On_Menu_List_Project_Files_Recursive --
   ------------------------------------------

   procedure On_Menu_List_Project_Files_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      List_Project_Files (Context, True);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_List_Project_Files_Recursive;

   ------------------------
   -- Get_Status_Project --
   ------------------------

   procedure Get_Status_Project
     (Context   : Selection_Context;
      Recursive : Boolean)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      Open_Explorer (Kernel, Context);

      if Has_Project_Information (Context) then
         Query_Project_Files
           (Get_Explorer (Kernel),
            Kernel,
            Project_Information (Context),
            True, Recursive);
      else
         Query_Project_Files
           (Get_Explorer (Kernel),
            Kernel,
            Get_Project (Kernel),
            True, Recursive);
      end if;
   end Get_Status_Project;

   --------------------------------
   -- On_Menu_Get_Status_Project --
   --------------------------------

   procedure On_Menu_Get_Status_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Get_Status_Project (Context, False);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Get_Status_Project;

   ------------------------------------------
   -- On_Menu_Get_Status_Project_Recursive --
   ------------------------------------------

   procedure On_Menu_Get_Status_Project_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Get_Status_Project (Context, True);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Get_Status_Project_Recursive;

   ------------------
   -- On_Menu_Diff --
   ------------------

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      if not Save_Files (Get_Kernel (Context), Files) then
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Diff (Get_Current_Ref (Context),
               Create (Full_Filename => String_List.Head (Files)));
         String_List.Next (Files);
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Diff;

   --------------------------
   -- On_Menu_Diff_Working --
   --------------------------

   procedure On_Menu_Diff_Working
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      if not Save_Files (Get_Kernel (Context), Files) then
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Diff_Working (Get_Current_Ref (Context),
                       Create (Full_Filename => String_List.Head (Files)));
         String_List.Next (Files);
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Diff_Working;

   ----------------------------
   -- On_Menu_Diff_Base_Head --
   ----------------------------

   procedure On_Menu_Diff_Base_Head
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Diff_Base_Head
           (Get_Current_Ref (Context),
            Create (Full_Filename => String_List.Head (Files)));
         String_List.Next (Files);
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Diff_Base_Head;

   ----------------------
   -- On_Menu_View_Log --
   ----------------------

   procedure On_Menu_View_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      procedure Get_Log (VCS : VCS_Access; Filename : String);
      --  Get log information for the file, handles tags & branches

      -------------
      -- Get_Log --
      -------------

      procedure Get_Log (VCS : VCS_Access; Filename : String) is

         procedure Get_Log_For_Root (File : Virtual_File; Root : String);
         --  Get the log for the file as found in one of the Root
         --  subdirectories. This is to handle branches and tags stored in a
         --  separate directory as done by Subversion.

         ----------------------
         -- Get_Log_For_Root --
         ----------------------

         procedure Get_Log_For_Root (File : Virtual_File; Root : String) is
            Dir          : constant Virtual_File := Create (Root);
            F_Dir        : constant String := Full_Name (Dir, True).all;
            F_File       : constant String := Full_Name (File, True).all;
            Subdirs      : File_Array_Access;
            F_Sep, D_Sep : Natural := 0;
         begin
            if not Is_Directory (Dir) then
               return;
            end if;

            Subdirs := Read_Dir (Dir);
            --  ??? what if Root is not a full pathname ?

            --  Works only if the branches/tags are sharing the same root as
            --  trunk.

            --  First look for the root parent directory

            D_Sep := Root'Last - 1;

            while D_Sep > F_Dir'First loop
               D_Sep := D_Sep - 1;
               exit when F_Dir (D_Sep) = Dir_Separator;
            end loop;

            if F_File'Length > D_Sep
              and then F_File (F_File'First .. F_File'First + D_Sep - 1)
              = F_Dir (F_Dir'First .. D_Sep)
            then
               --  Compute the index for the suffix
               for K in F_File'First + D_Sep + 1 .. F_File'Last loop
                  if F_File (K) = Dir_Separator then
                     F_Sep := K;
                     exit;
                  end if;
               end loop;

               if F_Sep /= 0 then
                  for K in Subdirs'Range loop
                     declare
                        Name   : constant String := Base_Name (Subdirs (K));
                        R_File : constant Virtual_File :=
                                   Create (Full_Name (Dir).all & Name &
                                           F_File (F_Sep .. F_File'Last));
                     begin
                        if Is_Regular_File (R_File) then
                           Log (VCS, R_File, "", As_Text => False);
                        end if;
                     end;
                  end loop;
               end if;
            end if;
         end Get_Log_For_Root;

         Kernel        : constant Kernel_Handle := Get_Kernel (Context);
         File          : constant Virtual_File :=
                           Create (Full_Filename => Filename);
         Project       : constant Project_Type :=
                           Get_Project_From_File
                             (Get_Registry (Kernel).all, File);
         Root_Branches : constant String := Get_Branches_Root (Project);
         Root_Tags     : constant String := Get_Tags_Root (Project);
         Script        : constant Scripting_Language :=
                           Lookup_Scripting_Language
                             (Get_Scripts (Kernel), GPS_Shell_Name);
         Command       : Custom_Command_Access;

      begin
         --  Clear the revision view for this file

         Create
           (Command, -"clear revision view", Kernel,
            "Revision.clear_view "
            & Argument_To_Quoted_String (Full_Name (File).all), Script);

         Launch_Background_Command
           (Kernel, Command_Access (Command), True, False, "");

         --  Get information for the trunk

         Log (VCS, File, "", As_Text => False);

         --  Get information fro the branches if defined

         if Root_Branches /= "" then
            Get_Log_For_Root (File, Root_Branches);
         end if;

         --  Get information fro the tags if defined

         if Root_Tags /= "" then
            Get_Log_For_Root (File, Root_Tags);
         end if;
      end Get_Log;

      Files : String_List.List;
      VCS   : VCS_Access;

   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot view log",
            Mode => Error);
         return;
      end if;

      VCS := Get_Current_Ref (Context);

      while not String_List.Is_Empty (Files) loop
         Get_Log (VCS, String_List.Head (Files));
         String_List.Next (Files);
      end loop;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_View_Log;

   ---------------------------
   -- On_Menu_View_Log_Text --
   ---------------------------

   procedure On_Menu_View_Log_Text
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      VCS   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot view log",
            Mode => Error);
         return;
      end if;

      VCS := Get_Current_Ref (Context);

      while not String_List.Is_Empty (Files) loop
         Log (VCS,
              Create (Full_Filename => String_List.Head (Files)),
              "",
              As_Text => True);
         String_List.Next (Files);
      end loop;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_View_Log_Text;

   --------------------------
   -- On_Menu_View_Log_Rev --
   --------------------------

   procedure On_Menu_View_Log_Rev
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Files    : String_List.List;
      Revision : GNAT.Strings.String_Access;
      Status   : File_Status_Record;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot view log",
            Mode => Error);
         return;
      end if;

      Status := Get_Cache
        (Get_Status_Cache, Create (String_List.Head (Files))).Status;

      if Status.Repository_Revision = null then
         if Status.Working_Revision = null then
            Revision := new String'("");
         else
            Revision := new String'(Protect (Status.Working_Revision.all));
         end if;

      else
         Revision := new String'(Protect (Status.Repository_Revision.all));
      end if;

      declare
         Str : constant String :=
           Execute_GPS_Shell_Command
            (Kernel,
             "MDI.input_dialog"
             & " ""Query history for revision:"""
             & " ""Revision=" & Revision.all & """");
      begin
         if Str /= "" then
            Log
              (Get_Current_Ref (Context),
               Create (Full_Filename => String_List.Head (Files)),
               Str);
         end if;
      end;

      GNAT.Strings.Free (Revision);
      String_List.Free (Files);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_View_Log_Rev;

   --------------------------------
   -- On_Menu_View_File_Revision --
   --------------------------------

   procedure On_Menu_View_File_Revision
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Ref : constant VCS_Access := Get_Current_Ref (Context);
   begin
      if Has_Revision_Information (Context) then
         File_Revision
           (Ref,
            File_Information (Context),
            Revision_Information (Context));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_View_File_Revision;

   ---------------------------
   -- On_Menu_Diff_Specific --
   ---------------------------

   procedure On_Menu_Diff_Specific
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Comparison (Context, True);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Diff_Specific;

   -------------------
   -- On_Menu_Diff2 --
   -------------------

   procedure On_Menu_Diff2
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Comparison (Context, False);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Diff2;

   ----------------------
   -- On_Menu_Diff_Tag --
   ----------------------

   procedure On_Menu_Diff_Tag
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Ref : constant VCS_Access := Get_Current_Ref (Context);
   begin
      if Has_File_Information (Context)
        and then Has_Tag_Information (Context)
      then
         Diff_Tag (Ref, File_Information (Context), Tag_Information (Context));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Diff_Tag;

   ---------------------------------
   -- On_Menu_Diff_Other_Revision --
   ---------------------------------

   procedure On_Menu_Diff_Other_Revision
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Ref : constant VCS_Access := Get_Current_Ref (Context);
   begin
      if Has_File_Information (Context)
        and then Has_Revision_Information (Context)
        and then Has_Other_Revision_Information (Context)
      then
         Diff
           (Ref,
            File_Information (Context),
            Version_1 => Other_Revision_Information (Context),
            Version_2 => Revision_Information (Context));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Diff_Other_Revision;

   ----------------
   -- Comparison --
   ----------------

   procedure Comparison
     (Context : Selection_Context;
      One_Rev : Boolean)
   is
      use String_List;

      Ref        : constant VCS_Access := Get_Current_Ref (Context);
      Kernel     : constant Kernel_Handle := Get_Kernel (Context);
      Files      : String_List.List;
      Status     : File_Status_Record;
      Revision_1 : GNAT.Strings.String_Access;
      Revision_2 : GNAT.Strings.String_Access;
      Str        : GNAT.Strings.String_Access;
      Index      : Natural;
      Confirm    : Boolean := True;

   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      if not Save_Files (Kernel, Files) then
         return;
      end if;

      Status := Get_Cache (Get_Status_Cache, Create (Head (Files))).Status;

      if not One_Rev then
         if Status.Repository_Revision = null then
            Revision_2 := new String'("");
         else
            Revision_2 := new String'
              (Protect (Status.Repository_Revision.all));
         end if;
      end if;

      if Has_Revision_Information (Context) then
         Str := new String'(Revision_Information (Context));
         Confirm := False;

      else
         if Status.Working_Revision /= null
           and then Status.Repository_Revision /= null
         then
            if Revision_Lower
              (Status.Working_Revision.all,
               Status.Repository_Revision.all)
            then
               --  The repository revision is newer than the working copy, use
               --  this version by default.
               Revision_1 := new String'
                 (Protect (Status.Repository_Revision.all));
            else
               Revision_1 := new String'
                 (Protect (Status.Working_Revision.all));
            end if;

         elsif Status.Repository_Revision /= null then
            Revision_1 := new String'
              (Protect (Status.Repository_Revision.all));

         elsif Status.Working_Revision /= null then
            Revision_1 := new String'(Protect (Status.Working_Revision.all));
         else
            Revision_1 := new String'("");
         end if;
      end if;

      if Confirm then
         if One_Rev then
            Str := new String'
              (Execute_GPS_Shell_Command
                 (Kernel,
                  "MDI.input_dialog"
                  & " ""Compare against revision:"""
                  & " ""Revision=" & Revision_1.all & """"));
         else
            Str := new String'
              (Execute_GPS_Shell_Command
                 (Kernel,
                  "MDI.input_dialog"
                  & " ""Compare between two revisions:"""
                  & " ""Revision 1=" & Revision_1.all & """"
                  & " ""Revision 2=" & Revision_2.all & """"));
         end if;
      end if;

      if One_Rev then
         if Str'Length /= 0 then
            Diff
              (Ref,
               Create (Full_Filename => Head (Files)),
               Str.all,
               "");
         end if;

      else
         if Str'Length /= 0 then
            Index := Str'First;
            Skip_To_Char (Str.all, Index, ASCII.LF);
            Diff
              (Ref,
               Create (Full_Filename => Head (Files)),
               Str (Str'First .. Index - 1),
               Str (Index + 1 .. Str'Last));
         end if;

         GNAT.Strings.Free (Revision_2);
      end if;

      GNAT.Strings.Free (Revision_1);
      GNAT.Strings.Free (Str);

      String_List.Free (Files);
   end Comparison;

   --------------------------
   -- Get_Files_In_Project --
   --------------------------

   function Get_Files_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := True) return String_List.List
   is
      Result : String_List.List;
      Files  : File_Array_Access;
   begin
      Files := Get_Source_Files (Project, Recursive);

      for J in reverse Files.all'Range loop
         String_List.Prepend (Result, Full_Name (Files (J)).all);
      end loop;

      Unchecked_Free (Files);

      return Result;
   end Get_Files_In_Project;

   -------------------------
   -- Get_Dirs_In_Project --
   -------------------------

   function Get_Dirs_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := False) return String_List.List
   is
      use String_List;

      Result : List;
      Final  : List;
      Node   : List_Node;
      Prev   : List_Node;
   begin
      declare
         A : GNAT.Strings.String_List_Access :=
               Source_Dirs (Project, Recursive, Has_VCS => True);
      begin
         for J in A'Range loop
            Append (Result, A (J).all);
         end loop;

         GNAT.Strings.Free (A);
      end;

      --  The result of Source_Dirs can contain duplicate entries.
      --  We need to remove these duplicates from the final result. The most
      --  efficient way to do this (in O(N*log (N))) is to sort the list and
      --  eliminate duplicates on the sorted list.

      if Is_Case_Sensitive (Build_Server) then
         Sort (Result);
      else
         Sort_Case_Insensitive (Result);
      end if;

      Node := First (Result);

      while Node /= Null_Node loop
         declare
            S : constant String := Data (Node);
         begin
            if Prev = Null_Node
              or else not File_Equal (Data (Prev), S, Build_Server)
            then
               Append (Final, S);
            end if;

            Prev := Node;
            Node := Next (Node);
         end;
      end loop;

      Free (Result);

      return Final;
   end Get_Dirs_In_Project;

   ------------------------------
   -- Query_Status_For_Project --
   ------------------------------

   procedure Query_Status_For_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Explorer : VCS_Explorer_View_Access;
   begin
      Open_Explorer (Kernel, No_Context);
      Explorer := Get_Explorer (Kernel);
      Query_Project_Files (Explorer, Kernel, Get_Project (Kernel), True, True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Query_Status_For_Project;

   ----------------
   -- Update_All --
   ----------------

   procedure Update_All
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Dirs : constant String_List.List :=
               Get_Dirs_In_Project (Get_Project (Kernel), True);
      Ref  : constant VCS_Access := Get_Current_Ref (Kernel);
   begin
      Update (Ref, Dirs);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Update_All;

   ---------------------
   -- Context_Factory --
   ---------------------

   function Context_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context
   is
      pragma Unreferenced (Child);
      Explorer : VCS_Explorer_View_Access;
   begin
      Explorer := Get_Explorer (Kernel_Handle (Kernel), False);

      if Explorer /= null then
         return Get_Current_Context (Explorer);
      else
         return No_Context;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
         return No_Context;
   end Context_Factory;

   -------------------------
   -- VCS_Command_Handler --
   -------------------------

   procedure VCS_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      File   : constant String := Nth_Arg (Data, 1, "");
      Full   : Virtual_File;
      Prj    : Project_Type;
      Ref    : VCS_Access;
      Files  : String_List.List;

   begin
      if Command = "repository_dir" then
         Set_Return_Value
           (Data,
            Get_Repository_Path (Kernel, No_File, Nth_Arg (Data, 1, "")));
         return;
      end if;

      if File = "" then
         Insert
           (Kernel,
            -"Command " & Command & " must have at least one parameter.",
            Mode => Error);
      end if;

      Full := Create (File, Kernel, Use_Object_Path => False);

      if Dir_Name (Full).all = "" then
         Insert (Kernel, -"Could not find file: " & File, Mode => Error);
         return;
      end if;

      --  At this point Full must not be null

      Prj := Get_Project_From_File (Get_Registry (Kernel).all, Full, True);

      if Prj = No_Project then
         Insert
           (Kernel,
            -"Could not find project for file: " & File,
            Mode => Error);

         return;
      end if;

      Ref := Get_Current_Ref (Prj);

      if Ref = null then
         Insert
           (Kernel,
            -"Could not find VCS for project: " & Project_Name (Prj),
            Mode => Error);
         return;
      end if;

      if Ref = Unknown_VCS_Reference then
         Insert
           (Kernel,
              -"There is no VCS associated to project: " & Project_Name (Prj),
            Mode => Error);
         return;
      end if;

      String_List.Append (Files, Full_Name (Full).all);

      --  Process the command

      if Command = "get_status" then
         Open_Explorer (Kernel, No_Context);
         Get_Status (Ref, Files);

      elsif Command = "update" then
         Update (Ref, Files);
         Get_Status (Ref, Files);

      elsif Command = "commit" then
         Log_Action_Files (Kernel, Ref, Commit, Files, No_Activity);

      elsif Command = "diff_head" then
         if Save_Files (Kernel, Files) then
            Diff (Ref, Full);
         end if;

      elsif Command = "log" then
         if Number_Of_Arguments (Data) = 2 then
            Log (Ref, Full, Nth_Arg (Data, 2));
         else
            Log (Ref, Full, "");
         end if;

      elsif Command = "diff_working" then
         declare
            Status : File_Status_List.List;
         begin
            if not Save_Files (Kernel, Files) then
               return;
            end if;

            Status := Local_Get_Status (Ref, Files);

            Diff
              (Ref,
               File_Status_List.Head (Status).File,
               "",
               File_Status_List.Head (Status).Working_Revision.all);
            File_Status_List.Free (Status);
         end;

      elsif Command = "annotate" then
         Annotate (Ref, Full);

      elsif Command = "remove_annotations" then
         Remove_Line_Information_Column (Kernel, Full, Annotation_Id);

      elsif Command = "repository_path" then
         Set_Return_Value
           (Data, Get_Repository_Path (Kernel, Full, Nth_Arg (Data, 2, "")));
      end if;

      String_List.Free (Files);
   end VCS_Command_Handler;

end VCS_View_API;
