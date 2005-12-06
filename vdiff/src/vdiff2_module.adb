-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
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

with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Vdiff2_Module.Callback;    use Vdiff2_Module.Callback;
with Vdiff2_Module.Utils;       use Vdiff2_Module.Utils;

package body Vdiff2_Module is

   use Diff_Head_List;

   type In_Diff_List_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access In_Diff_List_Filter;
      Context : access Selection_Context'Class) return Boolean;
   --  ??? See In_Diff_List subprogram

   type In_3Diff_List_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access In_3Diff_List_Filter;
      Context : access Selection_Context'Class) return Boolean;
   --  Filter for 3-way diff contextual menus

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use Default_Preferences;
      Tools          : constant String := '/' & (-"Tools") & '/'
        & (-"Visual Diff") & '/';
      Filter         : Action_Filter;
      Filter_3_Files : Action_Filter;
      Command        : Interactive_Command_Access;
   begin
      Vdiff_Module_ID := new VDiff2_Module_Record;
      VDiff2_Module (Vdiff_Module_ID).List_Diff := new Diff_Head_List.List;

      Add_Hook (Kernel, File_Closed_Hook,
                Wrapper (File_Closed_Cb'Access),
                Name => "vdiff2.file_closed");

      Register_Module
        (Module      => Vdiff_Module_ID,
         Kernel      => Kernel,
         Module_Name => Vdiff_Module_Name,
         Priority    => Default_Priority);

      Filter := new In_Diff_List_Filter;
      Filter_3_Files := new In_3Diff_List_Filter;

      Command := new Recompute_Diff_Command;
      Register_Contextual_Menu
        (Kernel, "Vdiff recompute difference",
         Label  => -"Visual Diff/Recompute",
         Action => Command,
         Filter => Filter);

      Command := new Hide_Difference_Command;
      Register_Contextual_Menu
        (Kernel, "Vdiff hide difference",
         Label  => -"Visual Diff/Hide",
         Action => Command,
         Filter => Filter);

      Command := new Close_Difference_Command;
      Register_Contextual_Menu
        (Kernel, "Vdiff close difference",
         Label  => -"Visual Diff/Close editors",
         Action => Command,
         Filter => Filter);

      Command := new Change_Ref_File_Command;
      Register_Contextual_Menu
        (Kernel, "Vdiff change reference file",
         Label  => -"Visual Diff/Use this editor as reference",
         Action => Command,
         Filter => Filter_3_Files);

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
        (Kernel, Preferences_Changed_Hook,
         Wrapper (On_Preferences_Changed'Access),
         Name => "vdiff2.preferences_changed");
      Add_Hook
        (Kernel, Diff_Action_Hook,
         Wrapper (Diff_Hook'Access),
         Name => "vdiff2.diff");

      Register_Menu
        (Kernel, '/' & (-"Tools") & '/', (-"Visual Diff"),
         Callback => null,
         Ref_Item => -"Interrupt", Add_Before => True);

      Register_Menu
        (Kernel, Tools, -"Compare Two Files...", "",
         On_Compare_Two_Files'Access);
      Register_Menu
        (Kernel, Tools, -"Compare Three Files...", "",
         On_Compare_Three_Files'Access);

      --  ??? Disable these menus for now, since the "Merge" interface doesn't
      --  work yet.

--        Register_Menu
--          (Kernel, Tools, -"Merge Two Files...", "",
--           On_Merge_Two_Files'Access);
--        Register_Menu
--          (Kernel, Tools, -"Merge Three Files...", "",
--           On_Merge_Three_Files'Access);

      Register_Command
        (Kernel, "visual_diff",
         Minimum_Args => 2,
         Maximum_Args => 3,
         Handler      => Diff_Command_Handler'Access);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out VDiff2_Module_Record) is
   begin
      Free_List (Id.List_Diff.all);
      Free (Id.List_Diff.all);
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

   function Filter_Matches_Primitive
     (Filter  : access In_3Diff_List_Filter;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Context.all in File_Selection_Context'Class
        and then Has_File_Information (File_Selection_Context_Access (Context))
        and then Has_Directory_Information
          (File_Selection_Context_Access (Context))
      then
         return Is_In_3Diff_List
           (File_Information (File_Selection_Context_Access (Context)),
            VDiff2_Module (Vdiff_Module_ID).List_Diff.all) /=
           Diff_Head_List.Null_Node;
      end if;
      return False;
   end Filter_Matches_Primitive;

end Vdiff2_Module;
