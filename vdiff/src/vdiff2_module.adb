-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2003                    --
--                            ACT-Europe                             --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Intl;                use Glide_Intl;
with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with VFS;                       use VFS;

with Pixmaps_Vdiff2;            use Pixmaps_Vdiff2;
with Diff_Utils2;               use Diff_Utils2;
with Vdiff2_Command;            use Vdiff2_Command;
with Vdiff2_Module.Callback;    use Vdiff2_Module.Callback;
with Vdiff2_Module.Utils;       use Vdiff2_Module.Utils;

with Gdk.Bitmap;                use Gdk.Bitmap;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Pixmap;                use Gdk.Pixmap;

with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Window;                use Gtk.Window;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;

package body Vdiff2_Module is

   use Diff_Head_List;

   procedure VDiff_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Generate the contextual menu entries for contextual menus in other
   --  modules than the visual diff.

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar      : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Window       : constant Gtk_Window  := Get_Main_Window (Kernel);
      Tools        : constant String := '/' & (-"Tools") & '/'
        & (-"Visual Diff") & '/';
      Image   : Gtk_Image;
      Mask    : Gdk_Bitmap;
      PixMap  : Gdk_Pixmap;

   begin
      Vdiff_Module_ID := new VDiff2_Module_Record;
      VDiff2_Module (Vdiff_Module_ID).Kernel := Kernel_Handle (Kernel);
      VDiff2_Module (Vdiff_Module_ID).List_Diff :=
        new Diff_Head_List.List;
      VDiff2_Module (Vdiff_Module_ID).File_Closed_Id :=
        Kernel_Callback.Connect
          (Kernel,
           File_Closed_Signal,
           File_Closed_Cb'Access,
           Kernel_Handle (Kernel));

      Create (VDiff2_Module (Vdiff_Module_ID).Command_Last,
                VDiff2_Module (Vdiff_Module_ID).Kernel,
                VDiff2_Module (Vdiff_Module_ID).List_Diff,
                Last_Difference'Access);

      Create (VDiff2_Module (Vdiff_Module_ID).Command_First,
                VDiff2_Module (Vdiff_Module_ID).Kernel,
                VDiff2_Module (Vdiff_Module_ID).List_Diff,
                First_Difference'Access);

      Create (VDiff2_Module (Vdiff_Module_ID).Command_Next,
                VDiff2_Module (Vdiff_Module_ID).Kernel,
                VDiff2_Module (Vdiff_Module_ID).List_Diff,
                Next_Difference'Access);

      Create (VDiff2_Module (Vdiff_Module_ID).Command_Prev,
                VDiff2_Module (Vdiff_Module_ID).Kernel,
                VDiff2_Module (Vdiff_Module_ID).List_Diff,
                Prev_Difference'Access);

      Register_Module
        (Module                  => Vdiff_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Vdiff_Module_Name,
         Priority                => Default_Priority,
         Mime_Handler            => Mime_Action'Access,
         Contextual_Menu_Handler => VDiff_Contextual'Access);

      Diff3_Cmd := Param_Spec_String
      (Gnew_String
       (Name  => "Diff-Utils-Diff3",
        Nick  => -"Diff3 command",
        Blurb => -("Command used to compute differences between three files."
                         & " Arguments can also be specified"),
            Default => "diff3"));
      Register_Property
        (Kernel, Param_Spec (Diff3_Cmd), -"Visual diff");

      Diff_Default_Color := Param_Spec_Color
        (Gnew_Color
           (Name     =>  "Diff-Default-Color",
            Nick     => -"Default Color",
            Blurb    => -"Color used for highlighting in Visual Diff2",
            Default  => "#AAAAFF"));
      Register_Property
        (Kernel, Param_Spec (Diff_Default_Color), -"Visual diff");

      Diff_Old_Color := Param_Spec_Color
        (Gnew_Color
           (Name     =>  "Diff-Old-Color",
            Nick     => -"Old Color",
            Blurb    => -"Color used for highlighting in Visual Diff2",
            Default  => "#C1C1C1"));
      Register_Property
        (Kernel, Param_Spec (Diff_Old_Color), -"Visual diff");

      Diff_Append_Color := Param_Spec_Color
        (Gnew_Color
           (Name     =>  "Diff-Append-Color",
            Nick     => -"Append Color",
            Blurb    => -"Color used for highlighting in Visual Diff2",
            Default  => "#88EEAA"));
      Register_Property
        (Kernel, Param_Spec (Diff_Append_Color), -"Visual diff");

      Diff_Remove_Color := Param_Spec_Color
        (Gnew_Color
           (Name     =>  "Diff-Remove-Color",
            Nick     => -"Remove Color",
            Blurb    => -"Color used for highlighting in Visual Diff2",
            Default  => "#FFA0A0"));
      Register_Property
        (Kernel, Param_Spec (Diff_Remove_Color), -"Visual diff");

      Diff_Change_Color := Param_Spec_Color
        (Gnew_Color
           (Name     =>  "Diff-Change-Color",
            Nick     => -"Change Color",
            Blurb    => -"Color used for highlighting in Visual Diff2",
            Default  => "#ECECAA"));
      Register_Property
        (Kernel, Param_Spec (Diff_Change_Color), -"Visual diff");

      Kernel_Callback.Connect
        (Kernel, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (On_Preferences_Changed'Access),
         Kernel_Handle (Kernel));

      Register_Menu
        (Kernel, Tools, -"Compare Two Files...", "",
         On_Compare_Two_Files'Access);
      Register_Menu
        (Kernel, Tools, -"Compare Three Files...", "",
         On_Compare_Three_Files'Access);
      Register_Menu
        (Kernel, Tools, -"Merge Two Files...", "",
         On_Merge_Two_Files'Access);
      Register_Menu
        (Kernel, Tools, -"Merge Three Files...", "",
         On_Merge_Three_Files'Access);
      Append_Space (Toolbar);

      Create_From_Xpm_D
        (PixMap, Get_Window (Window), Mask, Null_Color, up_diff_xpm);
      Gtk_New (Image, PixMap, Mask);
      Register_Button
        (Kernel, -"Prev",
         Command_Access (VDiff2_Module (Vdiff_Module_ID).Command_Prev),
         Image, -"Go to prev mark");

      Create_From_Xpm_D
        (PixMap, Get_Window (Window), Mask, Null_Color, down_diff_xpm);
      Gtk_New (Image, PixMap, Mask);
      Register_Button (Kernel, -"Next",
                         Command_Access
                           (VDiff2_Module (Vdiff_Module_ID).Command_Next),
                         Image, -"Go to next mark");

      Create_From_Xpm_D
        (PixMap, Get_Window (Window), Mask, Null_Color, last_diff_xpm);
      Gtk_New (Image, PixMap, Mask);
      Register_Button (Kernel, -"Last",
                         Command_Access
                           (VDiff2_Module (Vdiff_Module_ID).Command_Last),
                         Image, -"Go to the last difference");
      Create_From_Xpm_D
        (PixMap, Get_Window (Window), Mask, Null_Color, first_diff_xpm);
      Gtk_New (Image, PixMap, Mask);
      Register_Button (Kernel, -"First",
                         Command_Access
                           (VDiff2_Module (Vdiff_Module_ID).Command_First),
                       Image, -"Go to the First difference");
      Register_Action
        (Kernel,
         "First difference",
         Interactive_Command_Access
           (VDiff2_Module (Vdiff_Module_ID).Command_First),
         -"Go to the First difference");
      Register_Action
        (Kernel,
         "Last difference",
         Interactive_Command_Access
           (VDiff2_Module (Vdiff_Module_ID).Command_Last),
         -"Go to the Last difference");
      Register_Action
        (Kernel,
         "Prev difference",
         Interactive_Command_Access
           (VDiff2_Module (Vdiff_Module_ID).Command_Prev),
         -"Go to the previous difference");
      Register_Action
        (Kernel,
         "Next difference",
         Interactive_Command_Access
           (VDiff2_Module (Vdiff_Module_ID).Command_Next),
         -"Go to the next difference");

      Bind_Default_Key
        (Handler     => Get_Key_Handler (Kernel),
         Action      => "Next difference",
         Default_Key => "control-2");
      Bind_Default_Key
        (Handler     => Get_Key_Handler (Kernel),
         Action      => "Prev difference",
         Default_Key => "control-1");
      Bind_Default_Key
        (Handler     => Get_Key_Handler (Kernel),
         Action      => "First difference",
         Default_Key => "control-4");
      Bind_Default_Key
        (Handler     => Get_Key_Handler (Kernel),
         Action      => "Last difference",
         Default_Key => "control-3");
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out VDiff2_Module_Record) is
   begin
      Free_List (Id.List_Diff.all);
      Free (Id.List_Diff.all);
      Free (Root_Command (Id.Command_Prev.all));
      Free (Root_Command (Id.Command_Next.all));
      Free (Root_Command (Id.Command_First.all));
      Free (Root_Command (Id.Command_Last.all));
   end Destroy;

   ----------------------
   -- VDiff_Contextual --
   ----------------------

   procedure VDiff_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      File    : File_Selection_Context_Access;
      Submenu : Gtk_Menu;
      Mitem   : Gtk_Menu_Item;
      Dummy   : Diff_Head_List.List_Node;
      Selected_File : Virtual_File;

   begin

      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

         if Has_File_Information (File) and then
           Has_Directory_Information (File_Selection_Context_Access (Context))
         then
            Selected_File :=
              File_Information (File_Selection_Context_Access (Context));
            Dummy := Is_In_Diff_List
              (Selected_File,
               VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

            if Dummy /= Diff_Head_List.Null_Node then
               Gtk_New (Mitem, Label => -"Visual Diff");
               Gtk_New (Submenu);
               Set_Submenu (Mitem, Gtk_Widget (Submenu));
               Append (Menu, Mitem);

               Gtk_New (Mitem, -"Is New Diff Ref");
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller (On_Ref_Change'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Mitem, -"Recaculate Difference");
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller (On_Recalculate'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Mitem, -"Hide Difference");
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller (On_Hide_Differences'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Mitem, -"Close Difference");
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller (On_Close_Difference'Access),
                  Selection_Context_Access (Context));
            end if;
         end if;
      end if;
   end VDiff_Contextual;

end Vdiff2_Module;
