------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Config;
with Commands;                  use Commands;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Vdiff2_Command_Block;      use Vdiff2_Command_Block;
with Vdiff2_Module.Callback;    use Vdiff2_Module.Callback;
with Vdiff2_Module.Utils;       use Vdiff2_Module.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Vdiff2_Module is

   use Diff_Head_List;

   type In_Diff_List_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access In_Diff_List_Filter;
      Context : Selection_Context) return Boolean;
   --  ??? See In_Diff_List subprogram

   type In_3Diff_List_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access In_3Diff_List_Filter;
      Context : Selection_Context) return Boolean;
   --  Filter for 3-way diff contextual menus

   type Has_File_And_Dir_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_File_And_Dir_Filter;
      Context : Selection_Context) return Boolean;
   --  Check wether context contains a file and directory

   File1_Cst : aliased constant String := "file1";
   File2_Cst : aliased constant String := "file2";
   File3_Cst : aliased constant String := "file3";

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
            Inst : constant Class_Instance := Data.Nth_Arg (1, Vdiff_Class);
            Vdiff : constant Diff_Head_Access :=
               Diff_Script_Proxies.From_Instance (Inst);
            Cmd      : Diff_Command_Access;
         begin
            if Vdiff = null then
               Data.Set_Error_Msg ("Visual diff has been destroyed");
            else
               Create
                 (Cmd,
                  Get_Kernel (Data),
                  VDiff2_Module (Vdiff_Module_ID).List_Diff,
                  Close_Difference'Access);
               Unchecked_Execute (Cmd, Vdiff);
               Unref (Command_Access (Cmd));
            end if;
         end;

      elsif Command = "create" then
         declare
            File1 : constant Virtual_File := Nth_Arg (Data, 1);
            File2 : constant Virtual_File := Nth_Arg (Data, 2);
            File3 : Virtual_File := No_File;
            Vdiff : Diff_Head_Access;
         begin
            Name_Parameters (Data, Vdiff_Create_Parameters);

            if Number_Of_Arguments (Data) > 2 then
               File3 := Nth_Arg (Data, 3);
            end if;

            Vdiff := Visual_Diff (Diff_Mode.Get_Pref, File1, File2, File3);

            if Vdiff /= null then
               Data.Set_Return_Value
                 (Diff_Script_Proxies.Get_Or_Create_Instance
                     (Vdiff.Instances, Vdiff, Data.Get_Script));
            end if;
         end;

      elsif Command = "get" then
         declare
            File1 : constant Virtual_File := Nth_Arg (Data, 1);
            File2 : Virtual_File := No_File;
            File3 : Virtual_File := No_File;
            Vdiff : Diff_Head_Access;
         begin
            if Number_Of_Arguments (Data) > 1 then
               File2 := Nth_Arg (Data, 2);

               if Number_Of_Arguments (Data) > 2 then
                  File3 := Nth_Arg (Data, 3);
               end if;
            end if;

            Vdiff := Get_Vdiff (File1, File2, File3);

            if Vdiff /= null then
               Data.Set_Return_Value
                 (Diff_Script_Proxies.Get_Or_Create_Instance
                    (Vdiff.Instances, Vdiff, Data.Get_Script));
            end if;
         end;

      elsif Command = "files" then
         declare
            Inst : constant Class_Instance := Data.Nth_Arg (1, Vdiff_Class);
            Vdiff : constant Diff_Head_Access :=
               Diff_Script_Proxies.From_Instance (Inst);
         begin
            if Vdiff = null then
               Set_Error_Msg (Data, "Visual diff has been destroyed");
            else
               Data.Set_Return_Value_As_List;
               for Index in Vdiff.Files'Range loop
                  if Vdiff.Files (Index) /= GNATCOLL.VFS.No_File then
                     Data.Set_Return_Value
                       (Create_File (Data.Get_Script, Vdiff.Files (Index)));
                  end if;
               end loop;
            end if;
         end;

      elsif Command = "list" then
         declare
            Vdiff_List : constant Diff_Head_List_Access := Get_Vdiff_List;
         begin
            Set_Return_Value_As_List (Data);

            if Vdiff_List = null
              or else Vdiff_List.Is_Empty
            then
               return;
            end if;

            for Vdiff of Vdiff_List.all loop
               Data.Set_Return_Value
                 (Diff_Script_Proxies.Get_Or_Create_Instance
                    (Vdiff.Instances, Vdiff, Data.Get_Script));
            end loop;
         end;

      elsif Command = "recompute" then
         declare
            Inst : constant Class_Instance := Data.Nth_Arg (1, Vdiff_Class);
            Vdiff : constant Diff_Head_Access :=
               Diff_Script_Proxies.From_Instance (Inst);
            Cmd      : Diff_Command_Access;
         begin
            if Vdiff = null then
               Set_Error_Msg (Data, "Visual diff has been destroyed");
            else
               Create
                 (Cmd,
                  Get_Kernel (Data),
                  VDiff2_Module (Vdiff_Module_ID).List_Diff,
                  Reload_Difference'Access);
               Unchecked_Execute (Cmd, Vdiff);
               Unref (Command_Access (Cmd));
            end if;
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
      Filter         : Action_Filter;
      Filter_3_Files : Action_Filter;
      Submenu_Filter : Action_Filter;
      Patch_Path     : constant String := -"Visual diff:Patch Colors";
      Side_Path      : constant String := -"Visual diff:Side by Side Colors";
   begin
      Vdiff_Module_ID := new VDiff2_Module_Record;
      VDiff2_Module (Vdiff_Module_ID).List_Diff := new Diff_Head_List.Vector;

      File_Closed_Hook.Add (new On_File_Closed);

      Register_Module
        (Module      => Vdiff_Module_ID,
         Kernel      => Kernel,
         Module_Name => Vdiff_Module_Name,
         Priority    => Default_Priority);

      Filter         := new In_Diff_List_Filter;
      Filter_3_Files := new In_3Diff_List_Filter;
      Submenu_Filter := new Has_File_And_Dir_Filter;

      Register_Contextual_Submenu
        (Kernel,
         Name   => -"Visual Diff",
         Filter => Submenu_Filter);

      Register_Action
        (Kernel, "vdiff remove difference",
         Command     => new Remove_Difference_Command,
         Description => "Remove the current diff block",
         Category    => -"Diff",
         Filter      => Filter);
      Register_Contextual_Menu
        (Kernel,
         Label  => -"Visual Diff/Close",
         Action => "vdiff remove difference");

      Register_Action
        (Kernel, "vdiff recompute difference",
         Command     => new Recompute_Diff_Command,
         Description => "Recompute the differences between the two files",
         Category    => -"Diff",
         Filter      => Filter);
      Register_Contextual_Menu
        (Kernel,
         Label  => -"Visual Diff/Recompute",
         Action => "vdiff recompute difference");

      Register_Action
        (Kernel, "vdiff remove difference",
         Command     => new Remove_Difference_Command,
         Description => "Remove the current diff block",
         Category    => -"Diff",
         Filter      => Filter);
      Register_Contextual_Menu
        (Kernel,
         Label  => -"Visual Diff/Remove difference",
         Action => "vdiff remove difference");

      Register_Action
        (Kernel, "vdiff change reference file",
         Command  => new Change_Ref_File_Command,
         Category => -"Diff",
         Filter   => Filter_3_Files);
      Register_Contextual_Menu
        (Kernel,
         Label  => -"Visual Diff/Use this editor as reference",
         Action => "vdiff change reference file");

      Diff3_Cmd := Create
        (Get_Preferences (Kernel),
         Name  => "Diff-Utils-Diff3",
         Label => -"Diff3 command",
         Doc   =>
            -("Command and arguments to compute differences between"
              & " three files."),
         Path  => -"Visual diff:General",
         Default => Config.Default_Diff3_Cmd);

      Side_Default_Color := Create
        (Get_Preferences (Kernel),
         Name    => "Diff-Side-Default-Color",
         Label   => -"Old Color",
         Doc     => "",
         Path    => Side_Path,
         Default => "rgba(85,87,83,1)");

      Side_Change_Color := Create
        (Get_Preferences (Kernel),
         Name     => "Diff-Side-Change-Color",
         Label    => -"Change Color",
         Doc      => "",
         Path     => Side_Path,
         Default  => "rgba(236,236,170,1)");

      Side_Append_Color := Create
        (Get_Preferences (Kernel),
         Name    => "Diff-Side-Append-Color",
         Label   => -"Append Color",
         Doc     => "",
         Path    => Side_Path,
         Default => "rgba(10,100,10,1)");

      Side_Remove_Color := Create
        (Get_Preferences (Kernel),
         Name    =>  "Diff-Side-Remove-Color",
         Label   => -"Remove Color",
         Doc     => "",
         Path    => Side_Path,
         Default => "rgba(200,10,10,1)");

      Patch_Append_Color := Create
        (Get_Preferences (Kernel),
         Name            => "Diff-Patch-Append-Variant",
         Label           => -"Patch Append Color",
         Doc             => "",
         Path            => Patch_Path,
         Base            => Default_Style,
         Default_Variant => Default,
         Default_Fg      => "rgba(10,100,10,1)");

      Patch_Remove_Color := Create
        (Get_Preferences (Kernel),
         Name            => "Diff-Patch-Remove-Variant",
         Label           => -"Patch Remove Color",
         Doc             => "",
         Base            => Default_Style,
         Default_Variant => Default,
         Path            => Patch_Path,
         Default_Fg      => "rgba(200,10,10,1)");

      Patch_File_Color := Create
        (Get_Preferences (Kernel),
         Name            => "Diff-Patch-File-Header-Variant",
         Label           => -"Patch File Header Color",
         Doc             => "",
         Base            => Default_Style,
         Default_Variant => Default,
         Path            => Patch_Path,
         Default_Fg      => "rgba(100,163,224,1)");

      Patch_Code_Color := Create
        (Get_Preferences (Kernel),
         Name            => "Diff-Patch-Code-Header-Variant",
         Label           => -"Patch Code Header Color",
         Doc             => "",
         Base            => Default_Style,
         Default_Variant => Default,
         Path            => Patch_Path,
         Default_Fg      => "rgba(51,204,179,1)");

      Preferences_Changed_Hook.Add
         (new Standard.Vdiff2_Module.Callback.On_Pref_Changed);
      Diff_Action_Hook.Add (new On_Diff);

      Register_Action
        (Kernel, "compare two files",
         Command     => new Compare_Two_Files,
         Description => -"Compare two files",
         Category    => -"Diff");

      Register_Action
        (Kernel, "compare three files",
         Command     => new Compare_Three_Files,
         Description => -"Compare three files",
         Category    => -"Diff");

      Register_Commands (Kernel);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out VDiff2_Module_Record) is
   begin
      Free (Id.List_Diff);
      Vdiff_Module_ID := null;
   end Destroy;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access In_Diff_List_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_File_Information (Context)
        and then Has_Directory_Information (Context)
      then
         return Std_Vectors.Has_Element
           (Get_Diff_Node
              (File_Information (Context),
               VDiff2_Module (Vdiff_Module_ID).List_Diff.all));
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
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

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_File_And_Dir_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_File_Information (Context)
        and then Has_Directory_Information (Context);
   end Filter_Matches_Primitive;

end Vdiff2_Module;
