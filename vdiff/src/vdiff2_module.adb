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

with Config;
with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Traces;                    use Traces;
with Vdiff2_Command_Block;      use Vdiff2_Command_Block;
with Vdiff2_Module.Callback;    use Vdiff2_Module.Callback;
with Vdiff2_Module.Utils;       use Vdiff2_Module.Utils;
with GNATCOLL.VFS;                       use GNATCOLL.VFS;

package body Vdiff2_Module is

   Me : constant Debug_Handle := Create ("Vdiff2_Module");

   use Diff_Head_List;

   type In_Diff_List_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access In_Diff_List_Filter;
      Context : Selection_Context) return Boolean;
   --  ??? See In_Diff_List subprogram

   type In_3Diff_List_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access In_3Diff_List_Filter;
      Context : Selection_Context) return Boolean;
   --  Filter for 3-way diff contextual menus

   function Instance_From_Vdiff
     (Vdiff  : access Diff_Head;
      Class  : Class_Type;
      Script : access Scripting_Language_Record'Class) return Class_Instance;
   --  Get or create the class instance associated with a visual diff

   File1_Cst : aliased constant String := "file1";
   File2_Cst : aliased constant String := "file2";
   File3_Cst : aliased constant String := "File3";

   Vdiff_Create_Parameters : constant Cst_Argument_List :=
                               (1 => File1_Cst'Access,
                                2 => File2_Cst'Access,
                                3 => File3_Cst'Access);

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class);
   --  Register commands specific to the Vdiff2 module

   procedure Vdiff_Cmds
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for the Vdiff class

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Vdiff_Class : constant Class_Type :=
                      New_Class (Kernel, Vdiff_Class_Name);
   begin
      Register_Command
        (Kernel, Constructor_Method, 0, 0, Vdiff_Cmds'Access, Vdiff_Class);
      Register_Command
        (Kernel, "close_editors", 0, 0, Vdiff_Cmds'Access, Vdiff_Class);
      Register_Command
        (Kernel, "create", 2, 3, Vdiff_Cmds'Access, Vdiff_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "files", 0, 0, Vdiff_Cmds'Access, Vdiff_Class);
      Register_Command
        (Kernel, "get", 1, 3, Vdiff_Cmds'Access, Vdiff_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "list", 0, 0, Vdiff_Cmds'Access, Vdiff_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "recompute", 0, 0, Vdiff_Cmds'Access, Vdiff_Class);
   end Register_Commands;

   ----------------
   -- Vdiff_Cmds --
   ----------------

   procedure Vdiff_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      Vdiff_Class : constant Class_Type :=
                      New_Class (Get_Kernel (Data), Vdiff_Class_Name);
   begin
      if Command = Constructor_Method then
         Set_Error_Msg (Data, -("Cannot build instances of Vdiff."
           & " Use Vdiff.get() or Vdiff.create() instead"));

      elsif Command = "close_editors" then
         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Vdiff_Class);
            Property : constant Vdiff_Property_Access :=
              Vdiff_Property_Access
                (Instance_Property'(Get_Data (Instance, Vdiff_Class_Name)));
            Cmd      : Diff_Command_Access;
         begin
            if Property.Vdiff = null then
               Set_Error_Msg (Data, "Visual diff has been destroyed");
            else
               Create
                 (Cmd,
                  Get_Kernel (Vdiff_Module_ID.all),
                  VDiff2_Module (Vdiff_Module_ID).List_Diff,
                  Close_Difference'Access);
               Unchecked_Execute (Cmd, Property.Vdiff);
               Free (Root_Command (Cmd.all));
            end if;
         end;

      elsif Command = "create" then
         declare
            File1 : constant Virtual_File := Get_Data (Data, 1);
            File2 : constant Virtual_File := Get_Data (Data, 2);
            File3 : Virtual_File := No_File;
            Vdiff : Diff_Head_Access;
         begin
            Name_Parameters (Data, Vdiff_Create_Parameters);

            if Number_Of_Arguments (Data) > 2 then
               File3 := Get_Data (Data, 3);
            end if;

            Vdiff := Visual_Diff (File1, File2, File3);

            if Vdiff /= null then
               Set_Return_Value
                 (Data,
                  Instance_From_Vdiff
                    (Vdiff, Vdiff_Class, Get_Script (Data)));
            end if;
         end;

      elsif Command = "get" then
         declare
            File1 : constant Virtual_File := Get_Data (Data, 1);
            File2 : Virtual_File := No_File;
            File3 : Virtual_File := No_File;
            Vdiff : Diff_Head_Access;
         begin
            if Number_Of_Arguments (Data) > 1 then
               File2 := Get_Data (Data, 2);

               if Number_Of_Arguments (Data) > 2 then
                  File3 := Get_Data (Data, 3);
               end if;
            end if;

            Vdiff := Get_Vdiff (File1, File2, File3);

            if Vdiff /= null then
               Set_Return_Value
                 (Data,
                  Instance_From_Vdiff (Vdiff, Vdiff_Class, Get_Script (Data)));
            end if;
         end;

      elsif Command = "files" then
         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Vdiff_Class);
            Property : constant Vdiff_Property_Access := Vdiff_Property_Access
              (Instance_Property'(Get_Data (Instance, Vdiff_Class_Name)));
         begin
            if Property.Vdiff = null then
               Set_Error_Msg (Data, "Visual diff has been destroyed");
               return;
            end if;

            Set_Return_Value_As_List (Data);

            for Index in Property.Vdiff.Files'Range loop
               if Property.Vdiff.Files (Index) /= GNATCOLL.VFS.No_File then
                  Set_Return_Value
                    (Data,
                     Create_File
                       (Get_Script (Data), Property.Vdiff.Files (Index)));
               end if;
            end loop;
         end;

      elsif Command = "list" then
         declare
            Vdiff_List : constant Diff_Head_List_Access := Get_Vdiff_List;
            Vdiff_Node : Diff_Head_List.List_Node;
         begin
            Set_Return_Value_As_List (Data);

            if Vdiff_List = null
              or else Vdiff_List.all = Null_List
              or else Is_Empty (Vdiff_List.all)
            then
               return;
            end if;

            Vdiff_Node := First (Vdiff_List.all);

            while Vdiff_Node /= Null_Node loop
               Set_Return_Value
                 (Data,
                  Instance_From_Vdiff
                    (Diff_Head_List.Data (Vdiff_Node),
                     Vdiff_Class,
                     Get_Script (Data)));
               Vdiff_Node := Next (Vdiff_Node);
            end loop;
         end;

      elsif Command = "recompute" then
         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Vdiff_Class);
            Property : constant Vdiff_Property_Access := Vdiff_Property_Access
              (Instance_Property'(Get_Data (Instance, Vdiff_Class_Name)));
            Cmd      : Diff_Command_Access;
         begin
            if Property.Vdiff = null then
               Set_Error_Msg (Data, "Visual diff has been destroyed");
               return;
            end if;

            Create
              (Cmd,
               Get_Kernel (Vdiff_Module_ID.all),
               VDiff2_Module (Vdiff_Module_ID).List_Diff,
               Reload_Difference'Access);
            Unchecked_Execute (Cmd, Property.Vdiff);
            Free (Root_Command (Cmd.all));
         end;
      end if;
   end Vdiff_Cmds;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use Default_Preferences;
      Tools          : constant String :=
                         '/' & (-"Tools") & '/' & (-"Compare");
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
            Blurb => -("Command used to compute differences between three" &
              "files. Arguments can also be specified"),
            Default => Config.Default_Diff3_Cmd));
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
        (Kernel, '/' & (-"Tools") & '/', (-"C_ompare"),
         Callback => null,
         Ref_Item   => -"Consoles",
         Add_Before => True);

      Register_Menu
        (Kernel, Tools, -"_Two Files...", "",
         On_Compare_Two_Files'Access);
      Register_Menu
        (Kernel, Tools, -"T_hree Files...", "",
         On_Compare_Three_Files'Access);

      --  ??? Disable these menus for now, since the "Merge" interface doesn't
      --  work yet.

--        Register_Menu
--          (Kernel, Tools, -"Merge Two Files...", "",
--           On_Merge_Two_Files'Access);
--        Register_Menu
--          (Kernel, Tools, -"Merge Three Files...", "",
--           On_Merge_Three_Files'Access);

      Register_Commands (Kernel);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out VDiff2_Module_Record) is
   begin
      Free (Id.List_Diff);
      Vdiff_Module_ID := null;
   end Destroy;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access In_Diff_List_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_File_Information (Context)
        and then Has_Directory_Information (Context)
      then
         return Get_Diff_Node
           (File_Information (Context),
            VDiff2_Module (Vdiff_Module_ID).List_Diff.all) /=
           Diff_Head_List.Null_Node;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access In_3Diff_List_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_File_Information (Context)
        and then Has_Directory_Information (Context)
        and then Is_In_3Diff_List
          (File_Information (Context),
           VDiff2_Module (Vdiff_Module_ID).List_Diff.all);
   end Filter_Matches_Primitive;

   -------------------------
   -- Instance_From_Vdiff --
   -------------------------

   function Instance_From_Vdiff
        (Vdiff  : access Diff_Head;
         Class  : Class_Type;
         Script : access Scripting_Language_Record'Class) return Class_Instance
   is
      Instance : Class_Instance;
   begin
      if Vdiff.Instances = null then
         Vdiff.Instances := new Instance_List'(Null_Instance_List);
      end if;

      Instance := Get (Vdiff.Instances.all, Script);

      if Instance = No_Class_Instance then
         Trace (Me, "Create a new instance of the current visual diff");
         Instance := New_Instance (Script, Class);
         Set_Vdiff_Data (Instance, Diff_Head_Access (Vdiff));
         Set (Vdiff.Instances.all, Script, Instance);
      end if;

      return Instance;
   end Instance_From_Vdiff;

end Vdiff2_Module;
