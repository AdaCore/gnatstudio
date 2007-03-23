-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                              AdaCore                              --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Handling;   use Ada.Characters.Handling;

with Namet;                     use Namet;

with Gdk;
with Gdk.Pixbuf;                use Gdk.Pixbuf;

with Gtk;                       use Gtk;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;

with Gtkada.Handlers;           use Gtkada.Handlers;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with Log_Utils;                 use Log_Utils;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with Traces;                    use Traces;
with VCS_Activities;            use VCS_Activities;
with VCS_Module;                use VCS_Module;
with VCS_Utils;                 use VCS_Utils;
with VCS_View_API;              use VCS_View_API;
with Ignore_Db;                 use Ignore_Db;

package body VCS_View.Explorer is

   --------------------
   -- Local packages --
   --------------------

   package Page_Status_Callback is new Gtk.Handlers.User_Callback
     (GObject_Record, Natural);

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types
     (Explorer : access VCS_Explorer_View_Record) return GType_Array
   is
      pragma Unreferenced (Explorer);
   begin
      return GType_Array'
        (Base_Name_Column          => GType_String,
         Name_Column               => GType_String,
         Key_Column                => GType_String,
         Local_Rev_Column          => GType_String,
         Rep_Rev_Column            => GType_String,
         Status_Description_Column => GType_String,
         Status_Pixbuf_Column      => Gdk.Pixbuf.Get_Type,
         Has_Log_Column            => GType_Boolean,
         Activity_Column           => GType_String,
         Control_Column            => GType_Boolean);
   end Columns_Types;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types (Explorer : access VCS_Explorer_View_Record);
   --  Sets the types of columns to be displayed in the tree_view

   ---------------
   -- Callbacks --
   ---------------

   procedure Show_All_Status
     (Explorer : access Gtk_Widget_Record'Class);
   --  Callback for activation of "Show all status"

   procedure Hide_All_Status
     (Explorer : access Gtk_Widget_Record'Class);
   --  Callback for activation of "Show all status"

   procedure Toggle_Show_Status
     (Explorer : access GObject_Record'Class;
      Index    : Natural);
   --  Callback for activation of each filter

   type File_Hook_Record is new Function_With_Args with record
      Explorer : VCS_Explorer_View_Access;
   end record;
   type File_Hook is access all File_Hook_Record'Class;
   procedure Execute
     (Hook      : File_Hook_Record;
      Kernel    : access Kernel_Handle_Record'Class;
      File_Data : access Hooks_Data'Class);
   --  Callback for the "file_edited" signal

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Default context factory

   function To_History_Key (S : in String) return History_Key;
   --  Return history key corresponding to S

   function Match_Filter
     (Explorer : VCS_Explorer_View_Record;
      Label    : String) return Boolean;
   --  Returns True if Status can be displayed in the explorer.

   procedure Display_File_Status
     (Kernel         : Kernel_Handle;
      File_Status    : File_Status_Record;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Display        : Boolean := True;
      Displayed      : out Boolean);

   procedure Remove_Empty_Root
     (Explorer : access VCS_Explorer_View_Record'Class);
   --  Removes root node with no children

   --------------------
   -- To_History_Key --
   --------------------

   function To_History_Key (S : in String) return History_Key is
      Result : History_Key (S'First .. S'Last);
   begin
      for J in S'Range loop
         if not Is_Alphanumeric (S (J)) then
            Result (J) := '_';
         else
            Result (J) := S (J);
         end if;
      end loop;

      return Result;
   end To_History_Key;

   ---------------
   -- Do_Delete --
   ---------------

   procedure Do_Delete
     (Explorer : VCS_Explorer_View_Record)
   is
      pragma Unreferenced (Explorer);
   begin
      Hide_VCS_Explorer;
   end Do_Delete;

   ----------------
   -- Do_Refresh --
   ----------------

   procedure Do_Refresh (Explorer : access VCS_Explorer_View_Record) is

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean);
      --  Check if Iter match the current filter, remove node if not

      ----------------
      -- Check_Iter --
      ----------------

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean)
      is
         pragma Unreferenced (Quit);
         C_Stat : constant String :=
                    Get_String
                      (Explorer.Model, Iter, Status_Description_Column);
      begin
         if not Root then
            if not Match_Filter (Explorer.all, C_Stat) then
               --  Current status does not match or the status has changed
               Set (Explorer.Hidden,
                    Get_String (Explorer.Model, Iter, Key_Column), 0);
               Remove (Explorer.Model, Iter);

            else
               declare
                  File    : constant Virtual_File := Create
                    (Get_String (Explorer.Model, Iter, Name_Column));
                  Line    : Line_Record := Get_Cache (Get_Status_Cache, File);
                  Success : Boolean;
               begin
                  Fill_Info (Explorer, Iter, Line, Success);
               end;
            end if;
         end if;
      end Check_Iter;

      ----------------------
      -- Check_File_Nodes --
      ----------------------

      procedure Check_File_Nodes is new For_Every_Nodes (Check_Iter);

   begin
      --  First update the current nodes

      Check_File_Nodes (Explorer, Root_Only => False);

      --  Now go through all files that are not displayed due to filter

      declare
         use String_List;
         Status    : File_Status_Record;
         Displayed : Boolean;
         Iter      : File_Hash.String_Hash_Table.Iterator;
      begin
         Get_First (Explorer.Hidden, Iter);

         while Get_Element (Iter) /= 1 loop

            Status.File := Create (Get_Key (Iter));

            Display_File_Status
              (Explorer.Kernel, Status, Explorer.VCS,
               False, True, Displayed => Displayed);

            if Displayed then
               Remove (Explorer.Hidden, Get_Key (Iter));
            end if;

            Get_Next (Explorer.Hidden, Iter);
         end loop;
      end;

      Remove_Empty_Root (Explorer);
   end Do_Refresh;

   -----------------------
   -- Remove_Empty_Root --
   -----------------------

   procedure Remove_Empty_Root
     (Explorer : access VCS_Explorer_View_Record'Class)
   is

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean);
      --  Check if Iter is an empty node, remove it in this case

      ----------------
      -- Check_Iter --
      ----------------

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean)
      is
         pragma Unreferenced (Root, Quit);
      begin
         if not Has_Child (Explorer.Model, Iter) then
            Remove (Explorer.Model, Iter);
         end if;
      end Check_Iter;

      ----------------------
      -- Check_Root_Nodes --
      ----------------------

      procedure Check_Root_Nodes is new For_Every_Nodes (Check_Iter);

   begin
      Check_Root_Nodes (Explorer, Root_Only => True);
   end Remove_Empty_Root;

   ------------------
   -- Match_Filter --
   ------------------

   function Match_Filter
     (Explorer : VCS_Explorer_View_Record;
      Label    : String) return Boolean is
   begin
      declare
         Filter_Status : constant Status_Array_Access :=
                           Get (Explorer.Status, Name (Explorer.VCS));
      begin
         for K in Filter_Status'Range loop
            if Filter_Status (K).Status.Label.all = Label then
               return Filter_Status (K).Display;
            end if;
         end loop;
      end;

      --  No corresponding status, ok to display
      return True;
   end Match_Filter;

   -----------------------
   -- On_Remove_Project --
   -----------------------

   procedure On_Remove_Project
     (Explorer : access VCS_Explorer_View_Record;
      Project  : String)
   is
      Iter : Gtk_Tree_Iter :=
               Get_Iter_For_Root_Node (Explorer, Name_Column, Project);
   begin
      if Iter /= Null_Iter then
         Remove (Explorer.Model, Iter);
         Refresh (Explorer);
      end if;
   end On_Remove_Project;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Natural) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   -------------------------
   -- Display_File_Status --
   -------------------------

   procedure Display_File_Status
     (Kernel         : Kernel_Handle;
      File_Status    : File_Status_Record;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Display        : Boolean := True;
      Displayed      : out Boolean)
   is
      Explorer : constant VCS_Explorer_View_Access :=
                   Get_Explorer (Kernel, False, False);

      File     : constant Virtual_File := File_Status.File;

      New_Root : Boolean := False;
      --  Set to true if we have created a project node

      function Get_Or_Create_Project_Iter
        (File : Virtual_File) return Gtk_Tree_Iter;
      --  This routine creates a project from the explorer if needed. It
      --  returns the tree iterator for the newly created project or Null_Iter
      --  otherwise.

      --------------------------------
      -- Get_Or_Create_Project_Iter --
      --------------------------------

      function Get_Or_Create_Project_Iter
        (File : Virtual_File) return Gtk_Tree_Iter
      is
         function Get_Or_Create_Name
           (Name : String; Display_VCS : Boolean) return Gtk_Tree_Iter;
         --  Get or create node named Name

         ------------------------
         -- Get_Or_Create_Name --
         ------------------------

         function Get_Or_Create_Name
           (Name : String; Display_VCS : Boolean) return Gtk_Tree_Iter
         is
            R_Iter : Gtk_Tree_Iter;
         begin
            R_Iter := Get_Iter_For_Root_Node (Explorer, Name_Column, Name);

            if R_Iter = Null_Iter then
               Append (Explorer.Model, R_Iter, Null_Iter);
               Set (Explorer.Model, R_Iter, Name_Column, Name);

               if Display_VCS then
                  Set (Explorer.Model, R_Iter, Base_Name_Column,
                       Name & " (" & VCS.Name (VCS_Identifier) & ')');
               else
                  Set (Explorer.Model, R_Iter, Base_Name_Column, Name);
               end if;

               Set (Explorer.Model, R_Iter, Control_Column, False);
               New_Root := True;
            end if;

            return R_Iter;
         end Get_Or_Create_Name;

         Project : constant Projects.Project_Type :=
                     Get_Project_From_File
                       (Get_Registry (Kernel).all, File, False);
      begin
         if Project = No_Project then
            return Get_Or_Create_Name ("No project", False);
         else
            return Get_Or_Create_Name (Project_Name (Project), True);
         end if;
      end Get_Or_Create_Project_Iter;

      P_Iter : Gtk_Tree_Iter := Null_Iter;
      Line   : Line_Record;
      Log    : Boolean;

   begin
      Displayed := False;

      if not VFS.Is_Directory (File)
        and then not Ignore_File (VCS_Identifier, File)
      then
         Line := Get_Cache (Get_Status_Cache, File);

         if Line = No_Data or else Override_Cache then
            Log := Get_Log_From_File
              (Kernel, File, False) /= VFS.No_File;

            Line := (Copy_File_Status (File_Status), Log);
            Set_Cache (Get_Status_Cache, File, Line);
         end if;

         --  The info that we want to display is now in Line,
         --  Check wether the current file matches the filer.

         if Display then
            declare
               Iter    : Gtk_Tree_Iter := Null_Iter;
               Success : Boolean;
            begin
               Iter := Get_Iter_From_File (Explorer, Line.Status.File);

               if Match_Filter
                 (VCS_Explorer_View_Record (Explorer.all),
                  Line.Status.Status.Label.all)
               then
                  if Iter = Null_Iter and then Force_Display then
                     if P_Iter = Null_Iter then
                        P_Iter := Get_Or_Create_Project_Iter (File);
                     end if;
                     Append (Explorer.Model, Iter, P_Iter);
                     Displayed := True;
                  end if;

                  if Iter /= Null_Iter then
                     Fill_Info (Explorer, Iter, Line, Success);
                  end if;

               else
                  --  Register this file as part of the explorer

                  if Get (Explorer.Hidden, File_Key (File)) = 1 then
                     Set (Explorer.Hidden, File_Key (File), 0);
                  end if;

                  --  And if present on the explorer, remove it

                  if Iter /= Null_Iter then
                     Remove (Explorer.Model, Iter);
                  end if;
               end if;
            end;
         end if;
      end if;

      if New_Root then
         --  Expand this new node
         declare
            Path : Gtk_Tree_Path;
            Res  : Boolean;
            pragma Unreferenced (Res);
         begin
            Path := Get_Path (Explorer.Model, P_Iter);
            Res := Expand_Row (Explorer.Tree, Path, True);
            Path_Free (Path);
         end;
      end if;

      Remove_Empty_Root (Explorer);
   end Display_File_Status;

   procedure Display_File_Status
     (Kernel         : Kernel_Handle;
      Status         : File_Status_List.List;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Clear_Logs     : Boolean := False;
      Display        : Boolean := True)
   is
      Explorer          : constant VCS_Explorer_View_Access :=
                            Get_Explorer (Kernel, False, False);
      Registered_Status : constant VCS.Status_Array :=
                            Get_Registered_Status (VCS_Identifier);

      Status_Temp       : File_Status_List.List_Node;
      Sort_Id           : Gint;

      Up_To_Date_Status : VCS.File_Status;
      Filter_Status     : Status_Array_Access :=
                            Get (Explorer.Status, Name (VCS_Identifier));

      use type File_Status_List.List_Node;
   begin
      if Registered_Status'Length >= 2 then
         Up_To_Date_Status := Registered_Status (Registered_Status'First + 1);
      end if;

      Explorer.VCS := VCS_Identifier;

      if Filter_Status = null then
         --  No status registered for this VCS
         Filter_Status := new Status_Array (Registered_Status'Range);

         for J in Registered_Status'Range loop
            Filter_Status (J).Status  := Registered_Status (J);
            declare
               Key : constant History_Key :=
                       To_History_Key (Filter_Status (J).Status.Label.all);
            begin
               Create_New_Boolean_Key_If_Necessary
                 (Get_History (Explorer.Kernel).all, Key, True);
               Filter_Status (J).Display := Get_History
                 (Get_History (Explorer.Kernel).all, Key);
            end;
         end loop;

         Set (Explorer.Status, Name (VCS_Identifier), Filter_Status);
      end if;

      --  Free the logs associated to the files that are up-to-date, and
      --  update the vcs label in the editors.

      Update_Files_Status
        (Kernel, Status, VCS_Identifier, Clear_Logs, Up_To_Date_Status);

      if Explorer = null then
         return;
      end if;

      Push_State (Kernel, Busy);
      Sort_Id := Freeze_Sort (Explorer.Model);

      --  Iterate over each file

      Status_Temp := File_Status_List.First (Status);

      while Status_Temp /= File_Status_List.Null_Node loop
         declare
            Displayed : Boolean;
         begin
            Display_File_Status
              (Kernel, File_Status_List.Data (Status_Temp), Explorer.VCS,
               Override_Cache, Force_Display, Display, Displayed);
         end;

         Status_Temp := File_Status_List.Next (Status_Temp);
      end loop;

      Remove_Empty_Root (Explorer);

      Thaw_Sort (Explorer.Model, Sort_Id);
      Pop_State (Kernel);
   end Display_File_Status;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Do_Fill_Info
     (Explorer  : VCS_Explorer_View_Record;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean) is
   begin
      Set (Explorer.Model, Iter, Activity_Column,
           Get_Name (Get_File_Activity (Line_Info.Status.File)));
      Set (Explorer.Model, Iter, Control_Column, True);
      Success := True;
   end Do_Fill_Info;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Explorer : access VCS_Explorer_View_Record) is
      Col         : Gtk_Tree_View_Column;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Toggle_Rend : Gtk_Cell_Renderer_Toggle;
      Dummy       : Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);
      Gtk_New (Toggle_Rend);

      Set_Rules_Hint (Explorer.Tree, True);

      Gtk_New (Explorer.File_Column);
      Set_Title (Explorer.File_Column, -"Project / File");
      Pack_Start (Explorer.File_Column, Text_Rend, True);
      Add_Attribute
        (Explorer.File_Column, Text_Rend, "text", Base_Name_Column);
      Set_Clickable (Explorer.File_Column, True);
      Set_Sort_Column_Id (Explorer.File_Column, Base_Name_Column);
      Set_Resizable (Explorer.File_Column, True);
      Dummy := Append_Column (Explorer.Tree, Explorer.File_Column);

      Gtk_New (Explorer.Status_Column);
      Set_Title (Explorer.Status_Column, -"Status");
      Pack_Start (Explorer.Status_Column, Pixbuf_Rend, False);
      Add_Attribute
        (Explorer.Status_Column, Pixbuf_Rend, "pixbuf", Status_Pixbuf_Column);
      Set_Clickable (Explorer.Status_Column, True);
      Set_Sort_Column_Id (Explorer.Status_Column, Status_Description_Column);
      Dummy := Append_Column (Explorer.Tree, Explorer.Status_Column);

      Gtk_New (Explorer.Log_Column);
      Set_Title (Explorer.Log_Column, -"Log");
      Pack_Start (Explorer.Log_Column, Toggle_Rend, False);
      Add_Attribute
        (Explorer.Log_Column, Toggle_Rend, "active", Has_Log_Column);
      Add_Attribute
        (Explorer.Log_Column, Toggle_Rend, "visible", Control_Column);
      Set_Clickable (Explorer.Log_Column, False);
      Dummy := Append_Column (Explorer.Tree, Explorer.Log_Column);

      Gtk_New (Explorer.Activity_Column);
      Set_Title (Explorer.Activity_Column, -"Activity");
      Pack_Start (Explorer.Activity_Column, Text_Rend, True);
      Add_Attribute
        (Explorer.Activity_Column, Text_Rend, "text", Activity_Column);
      Dummy := Append_Column (Explorer.Tree, Explorer.Activity_Column);

      Gtk_New (Col);
      Set_Title (Col, -"Working rev.");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Local_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Head rev.");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Rep_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);
   end Set_Column_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer : out VCS_Explorer_View_Access;
      Kernel   : Kernel_Handle := null) is
   begin
      Explorer := new VCS_Explorer_View_Record;
      Initialize (Explorer, Kernel);
   end Gtk_New;

   ---------------------
   -- Show_All_Status --
   ---------------------

   procedure Show_All_Status
     (Explorer : access Gtk_Widget_Record'Class)
   is
      E : constant VCS_Explorer_View_Access :=
            VCS_Explorer_View_Access (Explorer);
      S : Status_Array_Access;
   begin
      S := Get (E.Status, Name (E.VCS));

      for J in S'Range loop
         S (J).Display := True;

         Set_History
           (Get_History (E.Kernel).all,
            To_History_Key (S (J).Status.Label.all),
            True);
      end loop;

      Refresh (E);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Show_All_Status;

   ---------------------
   -- Hide_All_Status --
   ---------------------

   procedure Hide_All_Status
     (Explorer : access Gtk_Widget_Record'Class)
   is
      E : constant VCS_Explorer_View_Access :=
            VCS_Explorer_View_Access (Explorer);
      S : constant Status_Array_Access :=
            Get (E.Status, Name (E.VCS));
   begin
      for J in S'Range loop
         S (J).Display := False;
         Set_History
           (Get_History (E.Kernel).all,
            To_History_Key (S (J).Status.Label.all),
            False);
      end loop;

      Refresh (E);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Hide_All_Status;

   ------------------------
   -- Toggle_Show_Status --
   ------------------------

   procedure Toggle_Show_Status
     (Explorer : access GObject_Record'Class;
      Index    : Natural)
   is
      E : constant VCS_Explorer_View_Access :=
            VCS_Explorer_View_Access (Explorer);
      S : constant Status_Array_Access :=
            Get (E.Status, Name (E.VCS));
   begin
      if Index in S'Range then
         S (Index).Display := not S (Index).Display;
      end if;

      Refresh (E);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Toggle_Show_Status;

   ------------------
   -- Context_Func --
   ------------------

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget);

      Explorer : constant VCS_Explorer_View_Access :=
                   VCS_Explorer_View_Access (Object);

      Check     : Gtk_Check_Menu_Item;
      Mitem     : Gtk_Menu_Item;
      Submenu   : Gtk_Menu;
      Files     : String_List.List;
      Path      : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
      Project   : Project_Type := No_Project;
      Root_Node : Boolean := False;

   begin
      --  If there is no selection, select the item under the cursor

      Iter := Find_Iter_For_Event
        (Explorer.Tree, Get_Model (Explorer.Tree), Event);

      if Iter /= Null_Iter then
         Path := Get_Path (Get_Model (Explorer.Tree), Iter);

         if not Path_Is_Selected (Get_Selection (Explorer.Tree), Path) then
            --  Right click over a line which is not the current selection,
            --  this line becomes the new selection.
            Unselect_All (Get_Selection (Explorer.Tree));
            Select_Path (Get_Selection (Explorer.Tree), Path);
         end if;

         if Get_Depth (Path) = 1 then
            Iter := Get_Iter (Explorer.Model, Path);

            Name_Len := 0;
            Add_Str_To_Name_Buffer
              (Get_String (Explorer.Model, Iter, Name_Column));

            Project := Get_Project_From_Name
              (Get_Registry (Kernel).all, Name_Find);

            Set_File_Information
              (Context,
               File    => VFS.No_File,
               Project => Project);

            Root_Node := True;

         elsif Get_Depth (Path) > 1 then
            Files := Get_Selected_Files (VCS_View_Access (Explorer));

            declare
               First_File : constant Virtual_File := Create
                 (Full_Filename => String_List.Head (Files));
            begin
               Iter :=
                 Parent (Explorer.Model, Get_Iter (Explorer.Model, Path));

               Name_Len := 0;
               Add_Str_To_Name_Buffer
                 (Get_String (Explorer.Model, Iter, Name_Column));

               Project := Get_Project_From_Name
                 (Get_Registry (Kernel).all, Name_Find);

               Set_File_Information
                 (Context,
                  File => First_File,
                  Project => Project);
            end;

            String_List.Free (Files);
         end if;

         Path_Free (Path);
      end if;

      if Project /= No_Project then
         Explorer.VCS := Get_VCS_From_Id
           (Get_Attribute_Value (Project, Vcs_Kind_Attribute));
      end if;

      Set_Context_Information
        (Context, Kernel, Abstract_Module_ID (VCS_Module_ID));
      Set_Current_Context (Explorer, Context);

      if Root_Node then
         if Project = No_Project then
            Gtk_New (Mitem, Label => -"Remove node");
         else
            Gtk_New (Mitem, Label => -"Remove project");
         end if;

         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate", On_Menu_Remove_Project'Access, Context);
         Set_Sensitive (Mitem, True);
      end if;

      VCS_Contextual_Menu (Kernel_Handle (Kernel), Context, Menu, False);

      Gtk_New (Mitem, -"Filters");
      Append (Menu, Mitem);

      Gtk_New (Submenu);
      Set_Submenu (Mitem, Submenu);

      Gtk_New (Mitem, -"Show all status");
      Append (Submenu, Mitem);
      Widget_Callback.Object_Connect
         (Mitem, "activate", Show_All_Status'Access, Explorer);

      Gtk_New (Mitem, -"Hide all status");
      Append (Submenu, Mitem);
      Widget_Callback.Object_Connect
         (Mitem, "activate", Hide_All_Status'Access, Explorer);

      Gtk_New (Mitem);
      Append (Submenu, Mitem);

      declare
         S : constant Status_Array_Access :=
               Get (Explorer.Status, Name (Explorer.VCS));
      begin
         for J in S'Range loop
            Gtk_New (Check, Label => -"Show " & S (J).Status.Label.all);
            Set_Active (Check, S (J).Display);
            Append (Submenu, Check);
            Page_Status_Callback.Object_Connect
              (Check, "activate", Toggle_Show_Status'Access, Explorer, J);
            Associate
              (Get_History (Kernel).all,
               To_History_Key (S (J).Status.Label.all),
               Check);
         end loop;
      end;

      if Has_File_Information (Context) then
         Gtk_New (Mitem);
         Append (Menu, Mitem);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Context_Func;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook      : File_Hook_Record;
      Kernel    : access Kernel_Handle_Record'Class;
      File_Data : access Hooks_Data'Class)
   is
      D        : constant File_Hooks_Args := File_Hooks_Args (File_Data.all);
      Log_Name : constant String := Full_Name (D.File).all;
      Line     : Line_Record;
   begin
      if Log_Name'Length > 4
        and then Log_Name (Log_Name'Last - 3 .. Log_Name'Last) = "$log"
      then
         declare
            File : constant Virtual_File :=
                     Get_File_From_Log (Kernel, D.File);
         begin
            Line := Get_Cache (Get_Status_Cache, File);

            if Line /= No_Data then
               Line.Log := True;
               Set_Cache (Get_Status_Cache, File, Line);
            end if;

            Refresh (Hook.Explorer);
         end;
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Execute;

   -------------------
   -- Do_Initialize --
   -------------------

   procedure Do_Initialize
     (Explorer : access VCS_Explorer_View_Record;
      Kernel   : Kernel_Handle)
   is
      Hook : File_Hook;
   begin
      Register_Contextual_Menu
        (Explorer.Kernel,
         Explorer.Tree,
         Explorer,
         Module_ID (VCS_Module_ID),
         Context_Func'Access);

      Set_Column_Types (Explorer);

      Hook := new File_Hook_Record'
        (Function_With_Args
         with Explorer => VCS_Explorer_View_Access (Explorer));
      Add_Hook (Kernel, File_Edited_Hook, Hook,
                Name => "vcs_view.file_edited",
                Watch => GObject (Explorer));
   end Do_Initialize;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return String_List.List is
   begin
      return Get_Selected_Files
        (VCS_View_Access (Get_Explorer (Kernel, False, False)));
   end Get_Selected_Files;

end VCS_View.Explorer;
