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
with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Commands.Custom;           use Commands; use Commands.Custom;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Strings;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts.Utils;    use GNATCOLL.Scripts.Utils;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Core_Kernels;          use GPS.Core_Kernels;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Glib.Values;               use Glib.Values;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.MDI;                use Gtkada.MDI;
with Log_Utils;                 use Log_Utils;
with Projects;                  use Projects;
with String_Utils;              use String_Utils;
with UTF8_Utils;                use UTF8_Utils;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_Activities_View_API;   use VCS_Activities_View_API;
with VCS_Module;                use VCS_Module;
with VCS_Module.Actions;        use VCS_Module.Actions;
with VCS_Status;                use VCS_Status;
with VCS_Utils;                 use VCS_Utils;
with VCS_View.Activities;       use VCS_View.Activities;
with VCS_View.Explorer;         use VCS_View.Explorer;
with VCS_View;                  use VCS_View;

package body VCS_View_API is
   use type GNAT.Strings.String_Access;

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

   function Get_Dirs_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := False) return File_Array_Access;
   --  Return the source directories contained in Project

   function Get_Current_Ref (Kernel : Kernel_Handle) return VCS_Access;
   --  Return the VCS reference corresponding to the current context in Kernel

   function Get_Selected_Files
     (Context : Selection_Context) return File_Array_Access;
   --  Return the list of files that are selected, according to Context

   procedure Process_Dirs
     (Context    : Selection_Context;
      Recursive  : Boolean;
      Update     : Boolean;
      Get_Status : Boolean);
   --  Perform VCS operations on directories contained in Context

   procedure Process_Dir
     (Directory  : Virtual_File;
      Ref        : VCS_Access;
      Kernel     : Kernel_Handle;
      Recursive  : Boolean;
      Update     : Boolean;
      Get_Status : Boolean);
   --  Same as above, working directly on a directory

   procedure On_Log_Action
     (Context : Selection_Context;
      Action  : VCS_Action;
      Files   : in out File_Array_Access);
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

   function Get_Repository_Root
     (Project : Project_Type) return Filesystem_String;
   --  Return the repository root as defined in project, ensure that it has an
   --  ending directory separator.

   function Get_Branches_Root
     (Project    : Project_Type;
      Repository : Boolean := False) return Filesystem_String;
   --  Return the root directory containing the branches. This routines expects
   --  a Subversion standard layout (i.e. trunk, branches, tags). If Repository
   --  is set the returned directory is referencing the repository root instead
   --  of the local copy.

   function Get_Tags_Root
     (Project    : Project_Type;
      Repository : Boolean := False) return Filesystem_String;
   --  Return the root directory containing the tags. This routines expects
   --  a Subversion standard layout (i.e. trunk, branches, tags). If Repository
   --  is set the returned directory is referencing the repository root instead
   --  of the local copy.

   function Get_Repository_Path
     (Kernel : Kernel_Handle;
      File   : Virtual_File; Tag : String) return Filesystem_String;
   --  Return the "trunk" repository path for File or the path to the
   --  corresponding tag/branch if specificed.

   -------------------------
   -- Get_Repository_Root --
   -------------------------

   function Get_Repository_Root
     (Project : Project_Type) return Filesystem_String
   is
      Rep_Root : constant Filesystem_String :=
                   +Project.Attribute_Value (VCS_Repository_Root);
   begin
      if Rep_Root'Length = 0 then
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
      Repository : Boolean := False) return Filesystem_String
   is
      Pattern : constant String :=
                  Dir_Separator & "trunk" & Dir_Separator;

      function Rep_Root return Filesystem_String;
      --  Return the repository root

      function Loc_Root return Filesystem_String;
      --  Return the local root

      --------------
      -- Loc_Root --
      --------------

      function Loc_Root return Filesystem_String is
         Proj : constant Virtual_File := Project_Path (Project);
         Path : constant Filesystem_String := Full_Name (Proj, True);
         J    : constant Natural := Index (+Path, Pattern);
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

      function Rep_Root return Filesystem_String is
         Rep_Root : constant Filesystem_String :=
                      Get_Repository_Root (Project);
      begin
         if Rep_Root'Length = 0 then
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
      Repository : Boolean := False) return Filesystem_String
   is
      Pattern : constant String := Dir_Separator & "trunk" & Dir_Separator;

      function Rep_Root return Filesystem_String;
      --  Return the repository root

      function Loc_Root return Filesystem_String;
      --  Return the local root

      --------------
      -- Loc_Root --
      --------------

      function Loc_Root return Filesystem_String is
         Proj : constant Virtual_File := Project_Path (Project);
         Path : constant Filesystem_String := Full_Name (Proj, True);
         J    : constant Natural := Index (+Path, Pattern);
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

      function Rep_Root return Filesystem_String is
         Rep_Root : constant Filesystem_String :=
                      Get_Repository_Root (Project);
      begin
         if Rep_Root'Length = 0 then
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
     (Kernel : Kernel_Handle;
      File   : Virtual_File; Tag : String) return Filesystem_String
   is
      Slash     : constant Character_Mapping := To_Mapping ("\", "/");
      DS        : Character renames Dir_Separator;
      H_Pattern : constant String := DS & "trunk" & DS;
      T_Pattern : constant String := DS & "tags" & DS;
      B_Pattern : constant String := DS & "branches" & DS;
      Project   : constant Project_Type := Get_Project (Kernel);
      Rep_Root  : constant Filesystem_String := Get_Repository_Root (Project);
      Full      : constant Filesystem_String := Full_Name (File);
      K, P, N   : Natural;
   begin
      if Rep_Root'Length = 0 then
         --  No repository root defined
         return "";
      end if;

      if File = No_File then
         --  Just return the directory for the given tag
         return Rep_Root & (+Tag);
      end if;

      --  look for part after the repository path

      K := Index (+Full, H_Pattern);

      if K = 0 then
         K := Index (+Full, T_Pattern);

         if K = 0 then
            K := Index (+Full, B_Pattern);

            if K = 0 then
               --  Unknown layout
               return "";
            end if;
         end if;
      end if;

      if Tag = "" then
         return Rep_Root & (+Translate (+Full (K .. Full'Last), Slash));

      else
         if Equal (Full (K + 1 .. K + 5), "trunk") then
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

         return Rep_Root &
           (+(Tag & Translate (+Full (P .. Full'Last), Slash)));
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
      Ignore : Gtk_Widget;
      pragma Unreferenced (Ignore);
   begin
      Dialog := new Create_Tag_Dialog_Record;
      Gtk.Dialog.Initialize
        (Dialog,
         Title  => -"Create tag/branch",
         Parent => null,
         Flags  => Destroy_With_Parent or Use_Header_Bar_From_Settings);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => False);

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
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => False);

      Gtk_New (Label, -"Tag name: ");
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Dialog.Tag_Name);
      Pack_Start (Box, Dialog.Tag_Name);

      Gtk_New (Dialog.Branch, -"Is a branch tag");
      Pack_Start (Get_Content_Area (Dialog), Dialog.Branch, Expand => False);

      Grab_Default (Add_Button (Dialog, "OK", Gtk_Response_OK));
      Ignore := Add_Button (Dialog, "Cancel", Gtk_Response_Cancel);
   end Gtk_New;

   procedure Gtk_New
     (Dialog      : out Switch_Tag_Dialog;
      Default_Tag : String;
      Default_Dir : String)
   is
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Browse : Gtk_Button;
      Ignore : Gtk_Widget;
      pragma Unreferenced (Ignore);
   begin
      Dialog := new Switch_Tag_Dialog_Record;
      Gtk.Dialog.Initialize
        (Dialog,
         Title  => -"Switch tag/branch",
         Parent => null,
         Flags  => Destroy_With_Parent or Use_Header_Bar_From_Settings);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => False);

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
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => False);

      Gtk_New (Label, -"Tag name: ");
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Dialog.Tag_Name);
      Set_Text (Dialog.Tag_Name, Default_Tag);
      Pack_Start (Box, Dialog.Tag_Name);

      Grab_Default (Add_Button (Dialog, "OK", Gtk_Response_OK));
      Ignore := Add_Button (Dialog, "Cancel", Gtk_Response_Cancel);
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
         Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
         Base_Directory    => GNATCOLL.VFS.Create (+Get_Text (Dialog.Dir)));
      --  ??? What if the filesystem path is non-UTF8?

   begin
      if Name /= GNATCOLL.VFS.No_File then
         Set_Text (Dialog.Dir, Display_Full_Name (Name));
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
         On_Remove_Project (Explorer, Project_Information (Context).Name);
      else
         On_Remove_Project (Explorer, "No project");
      end if;
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
   end On_Menu_Clear_Explorer;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type) return VCS_Access
   is
      use VCS_Project_Cache_Map;

      procedure Check (Project : Project_Type; VCS : out VCS_Access);
      --  Check for the actual VCS for the given project. The project directory
      --  is checked and all the sources directories.

      procedure Check (Dir : Virtual_File; VCS : out VCS_Access);
      --  Check Dir for a specific VCS, set VCS if found

      procedure Check_Root (Project : Project_Type; VCS : out VCS_Access);
      --  Check root directory of all sources directories

      -----------
      -- Check --
      -----------

      procedure Check (Dir : Virtual_File; VCS : out VCS_Access) is

         procedure Check_VCS (Ref : VCS_Access);
         --  Check for the given VCS

         ---------------
         -- Check_VCS --
         ---------------

         procedure Check_VCS (Ref : VCS_Access) is
         begin
            if VCS = null then
               declare
                  Adir : constant Filesystem_String :=
                           Ref.Administrative_Directory;
               begin
                  if Adir'Length > 0
                    and then Is_Directory (Create_From_Dir (Dir, Adir))
                  then
                     VCS := Ref;
                  end if;
               end;
            end if;
         end Check_VCS;

      begin
         VCS := null;

         if Dir /= No_File then
            For_Every_VCS (Check_VCS'Access);
         end if;
      end Check;

      procedure Check (Project : Project_Type; VCS : out VCS_Access) is
      begin
         if Project /= No_Project then
            --  Check current project

            declare
               Pos : constant Cursor :=
                       VCS_Module_ID.VCS_Project_Cache.Find (Project);
            begin
               if Has_Element (Pos) then
                  VCS := Element (Pos);

               else
                  Check (Project_Directory (Project), VCS);

                  if VCS = null then
                     --  Check all source directories

                     declare
                        Srcs : constant File_Array := Source_Dirs (Project);
                     begin
                        for K in Srcs'Range loop
                           Check (Srcs (K), VCS);
                           exit when VCS /= null;
                        end loop;
                     end;
                  end if;

                  if VCS /= null then
                     VCS_Module_ID.VCS_Project_Cache.Include (Project, VCS);
                  end if;
               end if;
            end;
         end if;
      end Check;

      ----------------
      -- Check_Root --
      ----------------

      procedure Check_Root (Project : Project_Type; VCS : out VCS_Access) is
      begin
         if Project /= No_Project then
            declare
               Srcs : constant File_Array   := Project.Source_Dirs;
               Dir  : constant Virtual_File := Greatest_Common_Path (Srcs);
               Pdir : constant Virtual_File := Get_Parent (Dir);
            begin
               Check (Get_Parent (Pdir), VCS);
            end;
         end if;
      end Check_Root;

      Pos : constant Cursor := VCS_Module_ID.VCS_Project_Cache.Find (Project);
      VCS : VCS_Access;

   begin
      if Get_Registry (Kernel).Tree.Status = Empty then
         --  Special case the empty project which is loaded initialy. We really
         --  do not want any VCS support for this project.

         return Unknown_VCS.Unknown_VCS_Reference;

      elsif Has_Element (Pos) then
         VCS := Element (Pos);

      else
         declare
            VCS_Name : constant String :=
                         Project.Attribute_Value
                           (VCS_Kind_Attribute,
                            Default => Default_VCS.Get_Pref);
         begin
            if To_Lower (VCS_Name) = "auto" then
               Check (Project, VCS);

               if VCS = null then
                  Check (Get_Registry (Kernel).Tree.Root_Project, VCS);
               end if;

               if VCS = null then
                  Check (Extended_Project (Project), VCS);
               end if;

               --  Finally let's check parent of all source directories

               if VCS = null then
                  Check_Root (Project, VCS);
               end if;

               if VCS = null then
                  Check_Root (Get_Registry (Kernel).Tree.Root_Project, VCS);
               end if;

               if VCS = null then
                  --  No VCS found for this name, default to unknown vcs
                  VCS := Unknown_VCS.Unknown_VCS_Reference;
               end if;

               if not VCS.Is_Used
                 and not (VCS = Unknown_VCS.Unknown_VCS_Reference)
               then
                  VCS.Used;
                  Kernel.Insert (-("Auto-VCS: using " & Name (VCS)));
               end if;

            else
               VCS := Get_VCS_From_Id (VCS_Name);
            end if;

            VCS_Module_ID.VCS_Project_Cache.Include (Project, VCS);
         end;
      end if;

      return VCS;
   end Get_Current_Ref;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref (Kernel : Kernel_Handle) return VCS_Access is
      C : constant Selection_Context := Get_Current_Context (Kernel);
   begin
      if C = No_Context then
         return Get_Current_Ref (Kernel, Get_Project (Kernel));
      else
         return Get_Current_Ref (C);
      end if;
   end Get_Current_Ref;

   ----------------------------------
   -- VCS_Explorer_Contextual_Menu --
   ----------------------------------

   procedure VCS_Explorer_Contextual_Menu
     (Context         : Selection_Context;
      Menu            : access Gtk.Menu.Gtk_Menu_Record'Class;
      Show_Everything : Boolean)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
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

      procedure Add_Separator;
      --  Add a separator in the menu if needed

      function Create_Activity_Menu (Menu : Gtk_Menu) return Boolean;
      --  Return True if some activities have been added into Menu

      -------------------
      -- Add_Separator --
      -------------------

      procedure Add_Separator is
         Sep : Gtk_Separator_Menu_Item;
      begin
         if Items_Inserted then
            Gtk_New (Sep);
            Append (Menu, Sep);
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
            if Get_Registry (Kernel).Tree.Root_Project.Project_Path
              = Get_Project_Path (Activity)
              and then not Is_Closed (Activity)
            then
               Found := True;
               Gtk_New (Item, Label => Emphasize (Get_Name (Activity)));
               Set_Use_Markup (Gtk_Label (Get_Child (Item)), True);
               Append (Menu, Item);

               A_Context := New_Context (Kernel, Get_Creator (Context));

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

   begin
      if Context = No_Context then
         Ref := Get_Current_Ref (Kernel, Get_Project (Kernel));
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
            File_S : constant Filesystem_String :=
                       Full_Name (File_Information (Context));
            Index : Natural;
         begin
            if Is_A_Log (File_Information (Context)) then
               --  By default, the log is a "commit" log
               Log_Action := Commit;

               begin
                  --  Attempt to read Action from the name of the log file

                  Index := File_S'Last - 4;
                  Skip_To_Char (+File_S, Index, '$', -1);

                  if Index - 1 in File_S'Range then
                     Log_Action := VCS_Action'Value
                       (+File_S (Index + 1 .. File_S'Last - 4));
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

      if Get_Creator (Context) = Abstract_Module (VCS_Explorer_Module_Id) then
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

                  L_Context := New_Context (Kernel, Get_Creator (Context));

                  declare
                     F_Info : constant File_Info'Class :=
                       File_Info'Class
                         (Get_Registry (Kernel).Tree.Info_Set (Original)
                          .First_Element);
                  begin
                     Set_File_Information
                       (L_Context,
                        Files   => (1 => Original),
                        Project => F_Info.Project);
                  end;

                  Gtk_New (Item, Label => Actions (Log_Action).all & " ("
                           & Krunch (+Base_Name (Original)) & ")");

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
         end if;
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

            if (not Has_Activity_Information (Context)
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

            if (First /= No_Activity
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
   end VCS_Explorer_Contextual_Menu;

   --------------------
   -- Change_Context --
   --------------------

   procedure Change_Context
     (Explorer : VCS_Explorer_View_Access;
      Context  : Selection_Context)
   is
      Status : File_Status_List.Vector;
      Ref    : VCS_Access;

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

      if Has_Directory_Information (Context)
        and then not Has_File_Information (Context)
      then
         Status := Local_Get_Status
           (Ref, (1 => Directory_Information (Context)));
         Display_File_Status
           (Get_Kernel (Context), Status, Ref, False, True);
         Status.Clear;

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
            Project_Information (Context), --  will compute as needed
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
     (Context : Selection_Context) return File_Array_Access
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Explorer : VCS_Explorer_View_Access;
      List     : File_Array_Access;

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
            Append (List, File_Array'(File_Information (Context)));
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
      List   : File_Array_Access      := Get_Selected_Files (Context);

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
         Filename           : constant Filesystem_String := Base_Name (File);
         --  The filename to look for in the ChangeLog file

         ChangeLog_Filename : constant String :=
                                +Full_Name (ChangeLog_File);
         --  The global ChangeLog file

         Last               : Natural;
         Entry_Found        : Boolean;

         CL : Arg_List;
      begin
         Line   := 1;
         Column := 0;

         --  Get last line in the file

         CL := Create ("Editor.get_last_line");
         Append_Argument (CL, ChangeLog_Filename, One_Arg);

         Last := Natural'Value (Execute_GPS_Shell_Command (Kernel, CL));

         --  First, look for the filename entry

         loop
            CL := Create ("Editor.get_chars");
            Append_Argument (CL, ChangeLog_Filename, One_Arg);
            Append_Argument (CL, Image (Line), One_Arg);
            declare
               B_Line : constant String :=
                          Execute_GPS_Shell_Command (Kernel, CL);

            begin
               Entry_Found := Index (B_Line, +Filename) /= 0;

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
            CL := Create ("Editor.get_chars");
            Append_Argument (CL, ChangeLog_Filename, One_Arg);
            Append_Argument (CL, Image (Line), One_Arg);
            declare
               B_Line : constant String :=
                 Execute_GPS_Shell_Command (Kernel, CL);
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

                     CL := Create ("Editor.replace_text");
                     Append_Argument (CL, ChangeLog_Filename, One_Arg);
                     Append_Argument (CL, Image (Line), One_Arg);
                     Append_Argument (CL, "1", One_Arg);

                     if Line >= Last then
                        --  This is the end of the file
                        Append_Argument (CL, String'(1 => ASCII.HT), One_Arg);
                     else
                        Append_Argument
                          (CL, ASCII.HT & ASCII.LF & ASCII.LF, One_Arg);
                     end if;

                     Execute_GPS_Shell_Command (Kernel, CL);

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
      for J in List'Range loop
         declare
            File           : Virtual_File renames List (J);
            ChangeLog_File : constant Virtual_File :=
                               Get_ChangeLog_From_File (Kernel, File);
            Already_Open   : Boolean;
            Line, Column   : Natural;
         begin
            Already_Open := Is_Open (Kernel, ChangeLog_File);
            Open_File_Action_Hook.Run
               (Kernel, ChangeLog_File, Project => No_Project);

            --  At this point we know that there is an entry for the current
            --  file for the current data into the ChangeLog file. Set the
            --  cursor location at the right position.

            Get_Location (File, ChangeLog_File, Line, Column);

            declare
               CL : Arg_List;
            begin
               --  ??? We should use the Editors API
               CL := Create ("Editor.edit");
               Append_Argument (CL, +Full_Name (ChangeLog_File), One_Arg);
               Append_Argument (CL, Image (Line), One_Arg);
               Append_Argument (CL, Image (Column), One_Arg);
               Execute_GPS_Shell_Command (Kernel, CL);
            end;

            if not Already_Open then
               Split (Get_MDI (Kernel), Gtk.Enums.Orientation_Vertical,
                      Mode => After_Reuse);
            end if;
         end;
      end loop;

      Unchecked_Free (List);
   end On_Menu_Edit_ChangeLog;

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      List   : File_Array_Access;
      Kernel : Kernel_Handle;

   begin
      Kernel := Get_Kernel (Context);
      List   := Get_Selected_Files (Context);

      for J in List'Range loop
         declare
            File : Virtual_File renames List (J);
         begin
            Get_Log_From_ChangeLog (Kernel, File);

            Open_File_Action_Hook.Run
              (Kernel,
               Get_Log_From_File (Kernel, File, True),
               Project          => No_Project,
               Group            => Group_Consoles,
               Initial_Position => Position_Bottom,
               Title            => +Base_Name (File) & " [log]",
               Areas            => Gtkada.MDI.Both);
         end;
      end loop;

      Unchecked_Free (List);
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
      Gtk_New (Dialog, Display_Full_Name (Project_Directory (Project)));
      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Dir       : constant Virtual_File :=
                          Create_From_UTF8 (Get_Text (Dialog.Dir));
            Tag       : constant String := Get_Text (Dialog.Tag_Name);
            As_Branch : constant Boolean := Get_Active (Dialog.Branch);
         begin
            Create_Tag
              (Get_Current_Ref (Context), Dir, Tag, As_Branch);
         end;
      end if;

      Destroy (Dialog);
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
            +Full_Name (Project_Directory (Project)));
      else
         Gtk_New (Dialog, "", +Full_Name (Project_Directory (Project)));
      end if;

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Dir : constant Filesystem_String := +Get_Text (Dialog.Dir);
            --  ??? What if the filesystem path is non-UTF8?
            Tag : constant String := Get_Text (Dialog.Tag_Name);
         begin
            Switch (Get_Current_Ref (Context), Create (Dir), Tag);
         end;
      end if;

      Destroy (Dialog);
   end On_Menu_Switch_Tag;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref
     (Context : Selection_Context) return VCS_Access
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Prj    : Project_Type;
   begin
      if Context = No_Context then
         return Get_VCS_From_Id ("");

      elsif Has_Project_Information (Context) then
         --  If the context has project information, retrieve the VCS for that
         --  project
         return Get_Current_Ref (Kernel, Project_Information (Context));

      elsif Has_File_Information (Context) then
         --  let the context guess the best project to use
         Prj := Project_Information (Context);

         if Prj /= No_Project then
            return Get_Current_Ref (Kernel, Prj);
         end if;
      end if;

      --  If all else fails, fallback on the VCS for the root project
      return Get_Current_Ref (Kernel, Get_Project (Kernel));
   end Get_Current_Ref;

   ------------------------
   -- On_Menu_Remove_Log --
   ------------------------

   procedure On_Menu_Remove_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Kernel     : constant Kernel_Handle := Get_Kernel (Context);
      Files      : File_Array_Access;
      Explorer   : VCS_Explorer_View_Access;
      A_Explorer : VCS_Activities_View_Access;

   begin
      Files := Get_Selected_Files (Context);
      Explorer := Get_Explorer (Kernel, False);
      A_Explorer := Get_Activities_Explorer (Kernel, False);

      for J in Files'Range loop
         declare
            File    : Virtual_File renames Files (J);
            Log     : constant Virtual_File :=
                        Get_Log_From_File (Kernel, File, False);
            Success : Boolean;
         begin
            if Is_Regular_File (Log) then
               Delete (Log, Success);
               Open_File_Action_Hook.Run
                  (Kernel, File => Log, Project => No_Project,
                   Line => -1);  --  close all editors
            end if;

            Remove_File_From_Mapping (Kernel, File);

            if Explorer /= null then
               Refresh_File (Explorer, File, Log => True);
            end if;

            if A_Explorer /= null then
               --  ??? revisit when child package implemented
               VCS_View.Refresh_File
                 (VCS_View.VCS_View_Access (A_Explorer), File, Log => True);
            end if;
         end;

      end loop;

      Unchecked_Free (Files);
   end On_Menu_Remove_Log;

   --------------------
   -- On_Menu_Commit --
   --------------------

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : File_Array_Access;
   begin
      if Has_Directory_Information (Context)
        and then not Has_File_Information (Context)
      then
         --  This is a directory, to commit we need to remove the trailing
         --  directory separator.

         Append (Files, Directory_Information (Context));
      else
         Files := Get_Selected_Files (Context);
      end if;

      On_Log_Action (Context, Commit, Files);
   end On_Menu_Commit;

   -------------------
   -- On_Log_Action --
   -------------------

   procedure On_Log_Action
     (Context : Selection_Context;
      Action  : VCS_Action;
      Files   : in out File_Array_Access)
   is
      Kernel         : constant Kernel_Handle := Get_Kernel (Context);
      Suffix         : constant String := Action_To_Log_Suffix (Action);
      VCS            : constant VCS_Access := Get_Current_Ref (Context);
      Real_Files     : File_Array_Access;
      All_Logs_Exist : Boolean := True;
      File           : Virtual_File;

   begin
      if Files = null or else Files'Length = 0 then
         if Files /= null then
            Unchecked_Free (Files);
         end if;

         Kernel.Insert
           (-"VCS: No file selected, cannot commit", Mode => Error);
         return;
      end if;

      --  Add all files in Files, if we have a log selected then we add the
      --  corresponding file.

      for J in Files'Range loop
         declare
            File : Virtual_File renames Files (J);
         begin
            if File.Has_Suffix ("$log") then
               --  This is a log file, add the corresponding file
               declare
                  L : constant Virtual_File :=
                        Get_File_From_Log (Kernel, File);
               begin
                  if L /= GNATCOLL.VFS.No_File then
                     Append (Real_Files, L);
                  end if;
               end;

            else
               --  Not a log file
               Append (Real_Files, File);
            end if;
         end;
      end loop;

      Unchecked_Free (Files);

      --  Open log editors for files that don't have a log

      if VCS.Require_Log then

         for J in Real_Files'Range loop
            File := Real_Files (J);

            if Get_Log_From_File (Kernel, File, False)
              = GNATCOLL.VFS.No_File
            then
               Get_Log_From_ChangeLog (Kernel, File, Suffix);
               All_Logs_Exist := False;

               Open_File_Action_Hook.Run
                 (Kernel,
                  Get_Log_From_File (Kernel, File, True, Suffix),
                  Project          => No_Project,
                  Group            => Group_Consoles,
                  Initial_Position => Position_Bottom,
                  Title            => +Base_Name (File) & " [log]");
            end if;
         end loop;
      end if;

      --  If All files have a log, commit the whole lot

      if All_Logs_Exist then
         Log_Action_Files (Kernel, VCS, Action, Real_Files.all, No_Activity);
      end if;

      Unchecked_Free (Real_Files);
   end On_Log_Action;

   ------------------
   -- On_Menu_Open --
   ------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      List : File_Array_Access;

      Ref  : VCS_Access;
   begin
      List := Get_Selected_Files (Context);

      if List = null or else List'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot open file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Open (Ref, List.all);
      Get_Status (Ref, List.all);
      Unchecked_Free (List);
   end On_Menu_Open;

   -----------------
   -- On_Menu_Add --
   -----------------

   procedure On_Menu_Add
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : File_Array_Access := Get_Selected_Files (Context);
   begin
      On_Log_Action (Context, Add, Files);
   end On_Menu_Add;

   -------------------------------------
   -- On_Menu_Add_Directory_No_Commit --
   -------------------------------------

   procedure On_Menu_Add_Directory_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Ref   : VCS_Access;
      Dir   : Virtual_File renames Directory_Information (Context);
   begin
      --  Do not pass the ending directory separator as we really want the last
      --  part to be the name we add.

      Ref := Get_Current_Ref (Context);
      Add (Ref, (1 => Dir), Log => "", Commit => False);
   end On_Menu_Add_Directory_No_Commit;

   ---------------------------
   -- On_Menu_Add_No_Commit --
   ---------------------------

   procedure On_Menu_Add_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : File_Array_Access;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot remove file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Add (Ref, Files.all, Log => "", Commit => False);

      if Implicit_Status.Get_Pref then
         Get_Status (Ref, Files.all);
      end if;

      Unchecked_Free (Files);
   end On_Menu_Add_No_Commit;

   --------------------
   -- On_Menu_Revert --
   --------------------

   procedure On_Menu_Revert
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : File_Array_Access;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot remove file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Revert (Ref, Files.all);

      if Implicit_Status.Get_Pref then
         Get_Status (Ref, Files.all);
      end if;

      Unchecked_Free (Files);
   end On_Menu_Revert;

   ----------------------
   -- On_Menu_Resolved --
   ----------------------

   procedure On_Menu_Resolved
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : File_Array_Access;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot mark file as resolved",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Resolved (Ref, Files.all);

      if Implicit_Status.Get_Pref then
         Get_Status (Ref, Files.all);
      end if;

      Unchecked_Free (Files);
   end On_Menu_Resolved;

   --------------------
   -- On_Menu_Remove --
   --------------------

   procedure On_Menu_Remove
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : File_Array_Access := Get_Selected_Files (Context);
   begin
      On_Log_Action (Context, Remove, Files);
   end On_Menu_Remove;

   ----------------------------------------
   -- On_Menu_Remove_Directory_No_Commit --
   ----------------------------------------

   procedure On_Menu_Remove_Directory_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Ref   : VCS_Access;
      Dir   : Virtual_File renames Directory_Information (Context);
   begin
      --  Do not pass the ending directory separator as we really want the last
      --  part to be the name we add.

      Ref := Get_Current_Ref (Context);

      Remove (Ref, (1 => Dir), Log => "", Commit => False);
   end On_Menu_Remove_Directory_No_Commit;

   ------------------------------
   -- On_Menu_Remove_No_Commit --
   ------------------------------

   procedure On_Menu_Remove_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : File_Array_Access;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot remove file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Remove (Ref, Files.all, Log => "", Commit => False);

      if Implicit_Status.Get_Pref then
         Get_Status (Ref, Files.all);
      end if;

      Unchecked_Free (Files);
   end On_Menu_Remove_No_Commit;

   ----------------------
   -- On_Menu_Annotate --
   ----------------------

   procedure On_Menu_Annotate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : File_Array_Access;

   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot annotate",
            Mode => Error);
         return;
      end if;

      for J in Files'Range loop
         if not Is_Open (Get_Kernel (Context), Files (J)) then
            Open_File_Action_Hook.Run
               (Get_Kernel (Context), Files (J), Project => No_Project);
         end if;

         Annotate (Get_Current_Ref (Context), Files (J));
      end loop;

      Unchecked_Free (Files);
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
      Files  : File_Array_Access;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Kernel.Insert
           (-"VCS: No file selected, cannot remove annotations",
            Mode => Error);
         return;
      end if;

      for J in Files'Range loop
         Remove_Line_Information_Column
           (Kernel,
            Files (J),
            Annotation_Id);
      end loop;

      Unchecked_Free (Files);
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
      Files : File_Array_Access;

   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot update file",
            Mode => Error);
         return;
      end if;

      Update (Ref, Files.all);

      if Implicit_Status.Get_Pref then
         Get_Status (Ref, Files.all);
      end if;

      Unchecked_Free (Files);
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
      Files : File_Array_Access;
   begin
      if Has_Tag_Information (Context) then
         Files := Get_Selected_Files (Context);
         Merge (Ref, Files.all, Tag_Information (Context));
         Unchecked_Free (Files);
      end if;
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
      Files  : File_Array_Access;

   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Kernel.Insert
           (-"VCS: No file selected, cannot get status",
            Mode => Error);
         return;
      end if;

      Open_Explorer (Kernel, Context);
      Get_Status (Get_Current_Ref (Context), Files.all);
      Unchecked_Free (Files);
   end On_Menu_Get_Status;

   -----------------
   -- Process_Dir --
   -----------------

   procedure Process_Dir
     (Directory  : Virtual_File;
      Ref        : VCS_Access;
      Kernel     : Kernel_Handle;
      Recursive  : Boolean;
      Update     : Boolean;
      Get_Status : Boolean)
   is
   begin
      --  Do not process hidden directories
      if Kernel.Is_Hidden (Directory) then
         return;
      end if;

      if Update then
         VCS.Update (Ref, (1 => Directory));
      end if;

      if Get_Status then
         if Recursive then
            VCS.Get_Status_Dirs_Recursive (Ref, (1 => Directory));
         else
            VCS.Get_Status_Dirs (Ref, (1 => Directory));
         end if;
      end if;
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
   end On_Menu_Get_Status_Dir_Recursive;

   --------------------
   -- Update_Project --
   --------------------

   procedure Update_Project
     (Context   : Selection_Context;
      Recursive : Boolean)
   is
      Files : File_Array_Access;
      Ref   : constant VCS_Access := Get_Current_Ref (Context);

   begin
      Open_Explorer (Get_Kernel (Context), Context);

      if Has_Project_Information (Context) then
         Files := Project_Information (Context).Source_Files (Recursive);
      else
         Files := Get_Project (Get_Kernel (Context)).Source_Files (Recursive);
      end if;

      Update (Ref, Files.all);

      if Implicit_Status.Get_Pref then
         Get_Status (Ref, Files.all);
      end if;

      Unchecked_Free (Files);
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
      procedure Query_Status_For_Project (The_Project : Project_Type);
      --  Display the status for The_Project only

      No_VCS_Defined : Boolean := True;

      ------------------------------
      -- Query_Status_For_Project --
      ------------------------------

      procedure Query_Status_For_Project (The_Project : Project_Type) is
         Ref    : constant VCS_Access := Get_Current_Ref (Kernel, The_Project);
         Status : File_Status_List.Vector;
         Files  : File_Array_Access;

      begin
         if Ref = Unknown_VCS_Reference then
            Insert
              (Kernel,
               -"Warning: no VCS set in project properties for project "
               & The_Project.Name);

         else
            No_VCS_Defined := False;

            if Real_Query then
               if Group_Query_Status_By_Dir (Ref) then
                  declare
                     Dirs : File_Array_Access;
                  begin
                     Dirs := Get_Dirs_In_Project (The_Project, False);

                     for J in Dirs'Range loop
                        Process_Dir
                          (Directory  => Dirs (J),
                           Ref        => Ref,
                           Kernel     => Kernel,
                           Recursive  => False,
                           Update     => False,
                           Get_Status => True);
                     end loop;

                     Unchecked_Free (Dirs);
                  end;

               else
                  Files := The_Project.Source_Files (False);
                  Get_Status (Ref, Files.all);
                  Unchecked_Free (Files);
               end if;

            else
               Files := The_Project.Source_Files (False);
               Status := Local_Get_Status (Ref, Files.all);
               Display_File_Status (Kernel, Status, Ref, False, True);
               Status.Clear;
               Unchecked_Free (Files);
            end if;
         end if;
      end Query_Status_For_Project;

      Iterator        : Project_Iterator := Project.Start (Recursive);
      Current_Project : Project_Type := Current (Iterator);
   begin
      while Current_Project /= No_Project loop
         Query_Status_For_Project (Current_Project);
         Next (Iterator);
         Current_Project := Current (Iterator);
      end loop;

      if No_VCS_Defined then
         No_VCS_Message (Explorer);
      end if;
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
   end On_Menu_Get_Status_Project_Recursive;

   ------------------
   -- On_Menu_Diff --
   ------------------

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Files : File_Array_Access;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      if not Save_Files (Get_Kernel (Context), Files.all) then
         return;
      end if;

      for J in Files'Range loop
         Diff (Get_Current_Ref (Context), Files (J));
      end loop;

      Unchecked_Free (Files);
   end On_Menu_Diff;

   --------------------------
   -- On_Menu_Diff_Working --
   --------------------------

   procedure On_Menu_Diff_Working
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Files : File_Array_Access;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      if not Save_Files (Get_Kernel (Context), Files.all) then
         return;
      end if;

      for J in Files'Range loop
         Diff_Working (Get_Current_Ref (Context), Files (J));
      end loop;

      Unchecked_Free (Files);
   end On_Menu_Diff_Working;

   ----------------------------
   -- On_Menu_Diff_Base_Head --
   ----------------------------

   procedure On_Menu_Diff_Base_Head
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      Files : File_Array_Access;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      for J in Files'Range loop
         Diff_Base_Head (Get_Current_Ref (Context), Files (J));
      end loop;

      Unchecked_Free (Files);
   end On_Menu_Diff_Base_Head;

   ----------------------
   -- On_Menu_View_Log --
   ----------------------

   procedure On_Menu_View_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      procedure Get_Log (VCS : VCS_Access; Filename : Virtual_File);
      --  Get log information for the file, handles tags & branches

      -------------
      -- Get_Log --
      -------------

      procedure Get_Log (VCS : VCS_Access; Filename : Virtual_File) is

         procedure Get_Log_For_Root
           (File : Virtual_File; Root : Filesystem_String);
         --  Get the log for the file as found in one of the Root
         --  subdirectories. This is to handle branches and tags stored in a
         --  separate directory as done by Subversion.

         ----------------------
         -- Get_Log_For_Root --
         ----------------------

         procedure Get_Log_For_Root
           (File : Virtual_File; Root : Filesystem_String)
         is
            Dir          : constant Virtual_File := Create (Root);
            F_Dir        : constant Filesystem_String :=
                             Full_Name (Dir, True);
            F_File       : constant Filesystem_String :=
                             Full_Name (File, True);
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
              and then Equal
                (F_File (F_File'First .. F_File'First + D_Sep - 1),
                 F_Dir (F_Dir'First .. D_Sep))
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
                        Name   : constant Filesystem_String :=
                                   Base_Name (Subdirs (K));
                        R_File : constant Virtual_File :=
                                   Create_From_Dir
                                     (Dir,
                                      Name & F_File (F_Sep .. F_File'Last));
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
         Project       : constant Project_Type :=
           File_Info'Class (Get_Registry (Kernel).Tree.Info_Set
           (Filename).First_Element).Project;
         Root_Branches : constant Filesystem_String :=
           Get_Branches_Root (Project);
         Root_Tags     : constant Filesystem_String := Get_Tags_Root (Project);
         Script        : constant Scripting_Language :=
            Kernel.Scripts.Lookup_Scripting_Language (GPS_Shell_Name);
         Command       : Custom_Command_Access;

      begin
         --  Clear the revision view for this file

         Create
           (Command, -"clear revision view", Kernel,
            "Revision.clear_view "
            & Argument_To_Quoted_String (+Full_Name (Filename)), Script);

         Launch_Background_Command
           (Kernel, Command_Access (Command), True, False, "");

         --  Get information for the trunk

         Log (VCS, Filename, "", As_Text => False);

         --  Get information fro the branches if defined

         if Root_Branches'Length > 0 then
            Get_Log_For_Root (Filename, Root_Branches);
         end if;

         --  Get information from the tags if defined

         if Root_Tags'Length > 0 then
            Get_Log_For_Root (Filename, Root_Tags);
         end if;
      end Get_Log;

      Files   : File_Array_Access;
      VCS     : VCS_Access;

   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot view log",
            Mode => Error);
         return;
      end if;

      VCS := Get_Current_Ref (Context);

      for J in Files'Range loop
         Get_Log (VCS, Files (J));
      end loop;

      Unchecked_Free (Files);
   end On_Menu_View_Log;

   ---------------------------
   -- On_Menu_View_Log_Text --
   ---------------------------

   procedure On_Menu_View_Log_Text
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Files : File_Array_Access;
      VCS   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot view log",
            Mode => Error);
         return;
      end if;

      VCS := Get_Current_Ref (Context);

      for J in Files'Range loop
         Log (VCS,
              Files (J),
              "",
              As_Text => True);
      end loop;

      Unchecked_Free (Files);
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
      Files    : File_Array_Access;
      Revision : GNAT.Strings.String_Access;
      Status   : File_Status_Record;
   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot view log",
            Mode => Error);
         return;
      end if;

      Status := Get_Cache
        (Get_Status_Cache, Files (Files'First)).Status;

      if Status.Repository_Revision = null
        or else Status.Repository_Revision.all = "n/a"
      then
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
              Parse_String ("MDI.input_dialog"
                & " ""Query history for revision:"""
                & " ""Revision=" & Revision.all & """", Separate_Args));
      begin
         if Str /= "" then
            Log
              (Get_Current_Ref (Context),
               Files (Files'First),
               Str);
         end if;
      end;

      GNAT.Strings.Free (Revision);
      Unchecked_Free (Files);
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
   end On_Menu_Diff_Other_Revision;

   ----------------
   -- Comparison --
   ----------------

   procedure Comparison
     (Context : Selection_Context;
      One_Rev : Boolean)
   is
      Ref        : constant VCS_Access := Get_Current_Ref (Context);
      Kernel     : constant Kernel_Handle := Get_Kernel (Context);
      Files      : File_Array_Access;
      Status     : File_Status_Record;
      Revision_1 : GNAT.Strings.String_Access;
      Revision_2 : GNAT.Strings.String_Access;
      Str        : GNAT.Strings.String_Access;
      Index      : Natural;
      Confirm    : Boolean := True;

   begin
      Files := Get_Selected_Files (Context);

      if Files = null or else Files'Length = 0 then
         Get_Kernel (Context).Insert
           (-"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      if not Save_Files (Kernel, Files.all) then
         return;
      end if;

      Status := Get_Cache (Get_Status_Cache, Files (Files'First)).Status;

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
            Revision_1 := new String'(Ref.Get_Default_Revision (Head));
         end if;
      end if;

      if not One_Rev then
         if Status.Repository_Revision = null
           or else Status.Repository_Revision.all = "n/a"
         then
            Revision_2 := Revision_1;
            Revision_1 := new String'(Ref.Get_Default_Revision (Prev));
         else
            Revision_2 := new String'
              (Protect (Status.Repository_Revision.all));
         end if;
      end if;

      if Confirm then
         if One_Rev then
            Str := new String'
              (Execute_GPS_Shell_Command
                 (Kernel,
                  Parse_String ("MDI.input_dialog"
                    & " ""Compare against revision:"""
                    & " ""Revision=" & Revision_1.all & """",
                   Separate_Args)));
         else
            Str := new String'
              (Execute_GPS_Shell_Command
                 (Kernel,
                  Parse_String ("MDI.input_dialog"
                    & " ""Compare between two revisions:"""
                    & " ""Revision 1=" & Revision_1.all & """"
                    & " ""Revision 2=" & Revision_2.all & """",
                   Separate_Args)));
         end if;
      end if;

      if One_Rev then
         if Str'Length /= 0 then
            Diff (Ref, Files (Files'First), Str.all, "");
         end if;

      else
         if Str'Length /= 0 then
            Index := Str'First;
            Skip_To_Char (Str.all, Index, ASCII.LF);
            Diff
              (Ref,
               Files (Files'First),
               Str (Str'First .. Index - 1),
               Str (Index + 1 .. Str'Last));
         end if;

         GNAT.Strings.Free (Revision_2);
      end if;

      GNAT.Strings.Free (Revision_1);
      GNAT.Strings.Free (Str);

      Unchecked_Free (Files);
   end Comparison;

   -------------------------
   -- Get_Dirs_In_Project --
   -------------------------

   function Get_Dirs_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := False) return File_Array_Access
   is
      Result : File_Array_Access;

   begin
      declare
         A : File_Array := Source_Dirs_With_VCS (Project, Recursive);
      begin
         --  The result of Source_Dirs can contain duplicate entries.
         --  We need to remove these duplicates from the final result. The most
         --  efficient way to do this (in O(N*log (N))) is to sort the list and
         --  eliminate duplicates on the sorted list.

         Sort (A);

         for J in A'Range loop
            --  Remove potential duplicated entries
            if Result = null
              or else A (J) /= Result (Result'Last)
            then
               Append (Result, A (J));
            end if;
         end loop;
      end;

      if Result = null then
         return new File_Array (1 .. 0);
      else
         return Result;
      end if;
   end Get_Dirs_In_Project;

   ------------------------------
   -- Query_Status_For_Project --
   ------------------------------

   procedure Query_Status_For_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Explorer : VCS_Explorer_View_Access;
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      Open_Explorer (Kernel, No_Context);
      Explorer := Get_Explorer (Kernel);
      Query_Project_Files (Explorer, Kernel, Get_Project (Kernel), True, True);
   end Query_Status_For_Project;

   ----------------
   -- Update_All --
   ----------------

   procedure Update_All
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);

      Dirs : File_Array_Access :=
               Get_Dirs_In_Project (Get_Project (Kernel), True);
      Ref  : constant VCS_Access := Get_Current_Ref (Kernel);
   begin
      Update (Ref, Dirs.all);

      Unchecked_Free (Dirs);
   end Update_All;

   -------------------------
   -- VCS_Command_Handler --
   -------------------------

   procedure VCS_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      File   : constant Filesystem_String := Nth_Arg (Data, 1, "");
      Full   : Virtual_File;
      Prj    : Project_Type;
      Ref    : VCS_Access;

   begin
      if Command = "repository_dir" then
         Set_Return_Value
           (Data,
            Get_Repository_Path (Kernel, No_File, Nth_Arg (Data, 1, "")));
         return;
      end if;

      if File'Length = 0 then
         Insert
           (Kernel,
            -"Command " & Command & " must have at least one parameter.",
            Mode => Error);
      end if;

      Full := Create (File, Kernel, Use_Object_Path => False);

      if Dir (Full) = No_File then
         Insert (Kernel, -"Could not find file: " &
                 Unknown_To_UTF8 (+File), Mode => Error);
         return;
      end if;

      --  At this point Full must not be null

      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry (Kernel).Tree.Info_Set (Full).First_Element);
      begin
         Prj := F_Info.Project (True);
      end;

      if Prj = No_Project then
         Insert
           (Kernel,
            -"Could not find project for file: " & Unknown_To_UTF8 (+File),
            Mode => Error);

         return;
      end if;

      Ref := Get_Current_Ref (Kernel, Prj);

      if Ref = null then
         Insert
           (Kernel,
            -"Could not find VCS for project: " & Prj.Name,
            Mode => Error);
         return;
      end if;

      if Ref = Unknown_VCS_Reference then
         Insert
           (Kernel,
              -"There is no VCS associated to project: " & Prj.Name,
            Mode => Error);
         return;
      end if;

      --  Process the command

      if Command = "get_status" then
         Open_Explorer (Kernel, No_Context);
         Get_Status (Ref, (1 => Full));

      elsif Command = "update" then
         Update (Ref, (1 => Full));
         Get_Status (Ref, (1 => Full));

      elsif Command = "commit" then
         Log_Action_Files (Kernel, Ref, Commit, (1 => Full), No_Activity);

      elsif Command = "diff_head" then
         if Save_Files (Kernel, (1 => Full)) then
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
            Status : File_Status_List.Vector;
         begin
            if not Save_Files (Kernel, (1 => Full)) then
               return;
            end if;

            Status := Local_Get_Status (Ref, (1 => Full));

            Diff
              (Ref,
               Status.First_Element.File,
               "",
               Status.First_Element.Working_Revision.all);
            Status.Clear;
         end;

      elsif Command = "annotate" then
         Annotate (Ref, Full);

      elsif Command = "remove_annotations" then
         Remove_Line_Information_Column (Kernel, Full, Annotation_Id);

      elsif Command = "repository_path" then
         Set_Return_Value
           (Data, Get_Repository_Path (Kernel, Full, Nth_Arg (Data, 2, "")));

      elsif Command = "set_reference" then
         Set_Reference
           (Create (Nth_Arg (Data, 1)), Create (Nth_Arg (Data, 2)));

      elsif Command = "get_log_file" then
         Set_Return_Value
           (Data,
            Create_File (Get_Script (Data),
              Get_Log_From_File (Kernel, Full, False)));
      end if;
   end VCS_Command_Handler;

end VCS_View_API;
