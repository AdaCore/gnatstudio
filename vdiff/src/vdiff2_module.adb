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

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Actions;      use Glide_Kernel.Actions;
with Glide_Kernel.Contexts;     use Glide_Kernel.Contexts;
with Glide_Kernel.Hooks;        use Glide_Kernel.Hooks;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Standard_Hooks; use Glide_Kernel.Standard_Hooks;
with Glide_Intl;                use Glide_Intl;
with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;

with Pixmaps_Vdiff2;            use Pixmaps_Vdiff2;
with Diff_Utils2;               use Diff_Utils2;
with Vdiff2_Command_Block;      use Vdiff2_Command_Block;
with Vdiff2_Module.Callback;    use Vdiff2_Module.Callback;
with Vdiff2_Module.Utils;       use Vdiff2_Module.Utils;

with Gdk.Bitmap;                use Gdk.Bitmap;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Pixmap;                use Gdk.Pixmap;

with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Window;                use Gtk.Window;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Handlers;              use Gtk.Handlers;

package body Vdiff2_Module is

   use Diff_Head_List;


   type In_Diff_List_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access In_Diff_List_Filter;
      Context : access Selection_Context'Class) return Boolean;
   --  ??? See In_Diff_List subprogram


   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools  : constant String := '/' & (-"Tools") & '/'
        & (-"Visual Diff") & '/';
      Filter : Action_Filter;
      Command : Interactive_Command_Access;
   begin
      Vdiff_Module_ID := new VDiff2_Module_Record;
      VDiff2_Module (Vdiff_Module_ID).Kernel := Kernel_Handle (Kernel);
      VDiff2_Module (Vdiff_Module_ID).List_Diff :=
        new Diff_Head_List.List;

      Add_Hook (Kernel, File_Closed_Hook, File_Closed_Cb'Access);

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
         Priority                => Default_Priority);

      Filter := new In_Diff_List_Filter;

      Command := new Change_Ref_File_Command;
      Register_Contextual_Menu
        (Kernel, "Vdiff change reference file",
         Label  => -"Visual Diff/Change Reference File",
         Action => Command,
         Filter => Filter);

      Command := new Recompute_Diff_Command;
      Register_Contextual_Menu
        (Kernel, "Vdiff recompute difference",
         Label  => -"Visual Diff/Recompute Difference",
         Action => Command,
         Filter => Filter);

      Command := new Hide_Difference_Command;
      Register_Contextual_Menu
        (Kernel, "Vdiff hide difference",
         Label  => -"Visual Diff/Hide Difference",
         Action => Command,
         Filter => Filter);

      Command := new Close_Difference_Command;
      Register_Contextual_Menu
        (Kernel, "Vdiff close difference",
         Label  => -"Visual Diff/Close Difference",
         Action => Command,
         Filter => Filter);

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
            Default  => "#C1C1C1"));
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

      Diff_Fine_Change_Color := Param_Spec_Color
        (Gnew_Color
           (Name     =>  "Horizontal-Diff-Change-Color",
            Nick     => -"Fine Change Color",
            Blurb    => -"Color used for highlighting in Visual Diff2",
            Default  => "#FDE66A"));
      Register_Property
        (Kernel, Param_Spec (Diff_Fine_Change_Color), -"Visual diff");

      Add_Hook
        (Kernel, Preferences_Changed_Hook, On_Preferences_Changed'Access);
      Add_Hook (Kernel, Diff_Action_Hook, Diff_Hook'Access);

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

      Register_Action
        (Kernel,
         "First difference",
         Interactive_Command_Access
           (VDiff2_Module (Vdiff_Module_ID).Command_First),
         -"Go to the first difference");
      Register_Action
        (Kernel,
         "Last difference",
         Interactive_Command_Access
           (VDiff2_Module (Vdiff_Module_ID).Command_Last),
         -"Go to the last difference");
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
        (Kernel      => Kernel,
         Action      => "Next difference",
         Default_Key => "control-2");
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "Prev difference",
         Default_Key => "control-1");
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "First difference",
         Default_Key => "control-4");
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "Last difference",
         Default_Key => "control-3");

      Register_Command
        (Kernel, "visual_diff",
         Minimum_Args => 2,
         Maximum_Args => 3,
         Handler      => Diff_Command_Handler'Access);
   end Register_Module;

   ------------------------
   -- Init_Prev_Diff_Cmd --
   ------------------------

   procedure Init_Prev_Diff_Cmd (Diff : Diff_Head) is
   begin
      VDiff2_Module (Vdiff_Module_ID).Command_Prev.Last_Active_Diff  := Diff;
      VDiff2_Module (Vdiff_Module_ID).Command_Next.Last_Active_Diff  := Diff;
      VDiff2_Module (Vdiff_Module_ID).Command_First.Last_Active_Diff := Diff;
      VDiff2_Module (Vdiff_Module_ID).Command_Last.Last_Active_Diff  := Diff;
   end Init_Prev_Diff_Cmd;

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
      Vdiff_Module_ID := null;
   end Destroy;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access In_Diff_List_Filter;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Context.all in File_Selection_Context'Class
        and then Has_File_Information (File_Selection_Context_Access (Context))
        and then Has_Directory_Information
          (File_Selection_Context_Access (Context))
      then
         return Is_In_Diff_List
           (File_Information (File_Selection_Context_Access (Context)),
            VDiff2_Module (Vdiff_Module_ID).List_Diff.all) /=
           Diff_Head_List.Null_Node;
      end if;
      return False;
   end Filter_Matches_Primitive;

   -------------------
   -- VDiff_Toolbar --
   -------------------

   procedure VDiff_Toolbar
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Window  : constant Gtk_Window  := Get_Main_Window (Kernel);
      Image   : Gtk_Image;
      Mask    : Gdk_Bitmap;
      PixMap  : Gdk_Pixmap;

   begin
      VDiff2_Module (Vdiff_Module_ID).Is_Active := True;

      Append_Space (Toolbar);

      Create_From_Xpm_D
        (PixMap, Get_Window (Window), Mask, Null_Color, down_diff_xpm);
      Gtk_New (Image, PixMap, Mask);
      Register_Button
        (Kernel, -"Next difference",
         Command_Access
           (VDiff2_Module (Vdiff_Module_ID).Command_Next),
         Image, -"Go to the next difference");

      Create_From_Xpm_D
        (PixMap, Get_Window (Window), Mask, Null_Color, up_diff_xpm);
      Gtk_New (Image, PixMap, Mask);
      Register_Button
        (Kernel, -"Previous difference",
         Command_Access (VDiff2_Module (Vdiff_Module_ID).Command_Prev),
         Image, -"Go to the previous difference");

      Create_From_Xpm_D
        (PixMap, Get_Window (Window), Mask, Null_Color, last_diff_xpm);
      Gtk_New (Image, PixMap, Mask);
      Register_Button
        (Kernel, -"Last difference",
         Command_Access
           (VDiff2_Module (Vdiff_Module_ID).Command_Last),
         Image, -"Go to the last difference");

      Create_From_Xpm_D
        (PixMap, Get_Window (Window), Mask, Null_Color, first_diff_xpm);
      Gtk_New (Image, PixMap, Mask);
      Register_Button
        (Kernel, -"First difference",
         Command_Access
           (VDiff2_Module (Vdiff_Module_ID).Command_First),
         Image, -"Go to the first difference");
   end VDiff_Toolbar;

   --------------------------
   -- Remove_VDiff_Toolbar --
   --------------------------

   procedure Remove_VDiff_Toolbar
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
      pragma Unreferenced (Kernel);
   begin
      null;
      --  VDiff2_Module (Vdiff_Module_ID).Is_Active := false;
   end Remove_VDiff_Toolbar;
end Vdiff2_Module;
