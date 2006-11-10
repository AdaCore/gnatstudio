-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2006                      --
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

with Glib.Values;              use Glib.Values;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;                use Gtk.Label;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;

with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.Handlers;          use Gtkada.Handlers;

with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Main_Window;          use GPS.Main_Window;
with Projects;                 use Projects;
with VFS;                      use VFS;

package body GPS.Kernel.MDI is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Select_All_Children (View : access Gtk_Widget_Record'Class);
   --  Callback for the "save all windows" dialog

   procedure Select_Child_When_Saving
     (View   : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback when a specific line is selected in the "save all windows"
   --  dialog

   ------------------------
   -- Get_Current_Window --
   ------------------------

   function Get_Current_Window
     (Handle : access Kernel_Handle_Record'Class) return Gtk.Window.Gtk_Window
   is
      Child  : constant MDI_Child := Get_Focus_Child (Get_MDI (Handle));
      Widget : Gtk_Widget;
   begin
      if Child /= null then
         Widget := Get_Widget (Child);

         if Realized_Is_Set (Widget) then
            return Gtk_Window (Get_Toplevel (Widget));
         end if;
      end if;

      return Get_Main_Window (Handle);
   end Get_Current_Window;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child               : out GPS_MDI_Child;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class;
      Desktop_Independent : Boolean := False) is
   begin
      Child := new GPS_MDI_Child_Record;
      Initialize (Child, Widget, Flags, Group, Focus_Widget,
                  Default_Width, Default_Height, Module, Desktop_Independent);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Child               : access GPS_MDI_Child_Record'Class;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class;
      Desktop_Independent : Boolean := False) is
   begin
      Gtkada.MDI.Initialize
        (Child, Widget, Flags, Group, Focus_Widget);

      if Default_Width /= -1 or else Default_Height /= -1 then
         Set_Size_Request (Child, Default_Width, Default_Height);
      end if;

      Child.Module := Abstract_Module_ID (Module);
      Child.Desktop_Independent := Desktop_Independent;
   end Initialize;

   -------------
   -- Get_MDI --
   -------------

   function Get_MDI
     (Handle : access Kernel_Handle_Record'Class) return Gtkada.MDI.MDI_Window
   is
      Top : constant GPS_Window := GPS_Window (Handle.Main_Window);
   begin
      if Top = null then
         return null;
      else
         return Top.MDI;
      end if;
   end Get_MDI;

   ---------------------------
   -- Get_Module_From_Child --
   ---------------------------

   function Get_Module_From_Child
     (Child : Gtkada.MDI.MDI_Child) return Module_ID is
   begin
      if Child.all in GPS_MDI_Child_Record'Class then
         return Module_ID (GPS_MDI_Child (Child).Module);
      else
         return null;
      end if;
   end Get_Module_From_Child;

   ---------------------
   -- Get_File_Editor --
   ---------------------

   function Get_File_Editor
     (Handle : access Kernel_Handle_Record'Class;
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
      --  needs to be moved to gps.kernel.ads.

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

   -----------------------
   -- Save_MDI_Children --
   -----------------------

   function Save_MDI_Children
     (Handle   : access Kernel_Handle_Record'Class;
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

      function Save_Child (Child : MDI_Child) return Boolean;
      --  Save a specific child.
      --  Return True if the child was successfully saved

      -------------------------
      -- Add_Child_If_Needed --
      -------------------------

      procedure Add_Child_If_Needed (Child : MDI_Child) is
         Module : Module_ID;
      begin
         if Child = null then
            return;
         end if;

         Module := Get_Module_From_Child (Child);

         if Module /= null
           and then Save_Function
             (Module, Get_Widget (Child),
              Mode         => Query,
              Single_Child => Children'Length = 1)
         then
            Append (Model, It, Null_Iter);
            Set (Model, It, 0, True);
            Set (Model, It, 1, Get_Title (Child));
            Num_Unsaved := Num_Unsaved + 1;
         end if;
      end Add_Child_If_Needed;

      ----------------
      -- Save_Child --
      ----------------

      function Save_Child (Child : MDI_Child) return Boolean is
         Module : constant Module_ID := Get_Module_From_Child (Child);
      begin
         if Module /= null then
            return Save_Function
              (Module, Get_Widget (Child),
               Mode         => Action,
               Single_Child => Children'Length = 1);
         else
            return True;
         end if;
      end Save_Child;

      Tmp  : Boolean;
      Tmp2 : Message_Dialog_Buttons;
      pragma Unreferenced (Tmp, Tmp2);
   begin
      if Force then
         if Project_Modified (Get_Project (Handle), Recursive => True) then
            Tmp := Save_Project
              (Kernel    => Handle,
               Project   => Get_Project (Handle),
               Recursive => True);
         end if;

         if Children /= No_Children then
            for C in Children'Range loop
               if Children (C) /= null then
                  Tmp := Save_Child (Children (C));
               end if;
            end loop;

         else
            Iter := First_Child (MDI);

            loop
               Child := Get (Iter);
               exit when Child = null;
               Tmp := Save_Child (Child);
               Next (Iter);
            end loop;
         end if;

         return True;
      end if;

      Gtk_New (Model, Column_Types);

      if Children /= No_Children then
         for C in Children'Range loop
            Add_Child_If_Needed (Children (C));
         end loop;
      else
         if Project_Modified (Get_Project (Handle), Recursive => True) then
            Num_Unsaved := Num_Unsaved + 1;
            Append (Model, It, Null_Iter);
            Set (Model, It, 0, True);
            Set (Model, It, 1, Project_Description);
         end if;

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
                     Parent => Get_Current_Window (Handle),
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Label, -"Do you want to save the following file?");

         else
            Gtk_New (Dialog,
                     Title  => -"Saving files",
                     Parent => Get_Current_Window (Handle),
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Label, -"Do you want to save the following files?");
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
           (Col, "clicked", Select_All_Children'Access, Slot_Object => View);
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
                        if not Save_Project
                          (Kernel    => Handle,
                           Project   => Get_Project (Handle),
                           Recursive => True)
                        then
                           Destroy (Dialog);
                           Tmp2 := Message_Dialog
                             (Msg     => -"Couldn't save the project",
                              Buttons => Button_OK,
                              Parent  => Get_Current_Window (Handle));
                           return False;
                        end if;
                     else
                        Child := Find_MDI_Child_By_Name
                          (Get_MDI (Handle), Name);
                        if not Save_Child (Child) then
                           Destroy (Dialog);
                           Tmp2 := Message_Dialog
                             (Msg     => -"Couldn't save " & Name,
                              Buttons => Button_OK,
                              Parent  => Get_Current_Window (Handle));
                           return False;
                        end if;
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

   procedure Close_All_Children
     (Handle : access Kernel_Handle_Record'Class)
   is
      procedure Recurse (Iter : in out Child_Iterator);
      --  Recursively close children of Iter

      -------------
      -- Recurse --
      -------------

      procedure Recurse (Iter : in out Child_Iterator) is
         Child : constant MDI_Child := Get (Iter);
      begin
         if Child /= null then
            Next (Iter);
            Recurse (Iter);

            if Child.all not in GPS_MDI_Child_Record'Class
              or else not GPS_MDI_Child (Child).Desktop_Independent
            then
               Close_Child (Child);
            end if;
         end if;
      end Recurse;

      Iter : Child_Iterator := First_Child (Get_MDI (Handle));

   begin
      --  ??? We used to have a simple loop here to close MDI children,
      --  but using Gtk+ 2.8 on e.g. Windows, we are getting SEGV when
      --  traversing the list, so apparently some data is freed while
      --  we were still trying to access it. Not clear whether this is
      --  a Gtk+/Glib bug, or a bug in GPS/GtkAda code.

      Recurse (Iter);
   end Close_All_Children;

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
   -- Get_Command_Queue --
   -----------------------

   function Get_Command_Queue
     (Child : access GPS_MDI_Child_Record) return Commands.Command_Queue
   is
      pragma Unreferenced (Child);
   begin
      return Commands.Null_Command_Queue;
   end Get_Command_Queue;

end GPS.Kernel.MDI;
