------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

with GNAT.IO;                   use GNAT.IO;
with Csets;                     use Csets;
with Prj;                       use Prj;
with Prj.Env;                   use Prj.Env;
with Prj.Pars;                  use Prj.Pars;
with Prj.Tree;                  use Prj.Tree;
with Prj.Util;                  use Prj.Util;
with Namet;                     use Namet;
with Snames;                    use Snames;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Command_Line;          use Ada.Command_Line;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Convert.Gpr is

   procedure Output_Path (Path : String; Prefix : String);
   --  Output one line for each directory in path. Each line starts with
   --  Prefix.

   function Output_Single_Attr
     (Project_View : Project_Id;
      Tree         : Project_Tree_Ref;
      Gpr_Name     : String;
      Default      : String := "";
      Pkg_Name     : String := "") return String;
   --  Get the value of Gpr_Name in Project_View, and return it.
   --  Default is used if the attribute was not found in Project_View
   --
   --  The attribute is search in the package Pkg_Name, if this is not the
   --  empty string.

   function Output_Array_Attr
     (Project_View : Project_Id;
      Tree         : Project_Tree_Ref;
      Gpr_Name     : String;
      Default      : String := "";
      Pkg_Name     : String := "";
      Index        : String := "") return String;
   --  Same as Output_Single_Attr, but for an array attribute

   -----------------
   -- Output_Path --
   -----------------

   procedure Output_Path (Path : String; Prefix : String) is
      Files : constant File_Array := From_Path (+Path);
   begin
      for J in Files'Range loop
         Ensure_Directory (Files (J));
         Put_Line (Prefix & (+Files (J).Unix_Style_Full_Name));
      end loop;
   end Output_Path;

   ------------------------
   -- Output_Single_Attr --
   ------------------------

   function Output_Single_Attr
     (Project_View : Project_Id;
      Tree         : Project_Tree_Ref;
      Gpr_Name     : String;
      Default      : String := "";
      Pkg_Name     : String := "") return String
   is
      Attr_Id : Name_Id;
      Value : Prj.Variable_Value;
      Str : String_List_Id;
      Pkg_Id : Package_Id := No_Package;
      Decl : Variable_Id;
   begin
      Name_Len := Gpr_Name'Length;
      Name_Buffer (1 .. Name_Len) := Gpr_Name;
      Attr_Id := Name_Find;

      if Pkg_Name /= "" then
         Name_Len := Pkg_Name'Length;
         Name_Buffer (1 .. Name_Len) := Pkg_Name;
         Pkg_Id := Value_Of
           (Name_Find,
            In_Packages => Project_View.Decl.Packages,
            Shared      => Tree.Shared);
      end if;

      if Pkg_Id = No_Package then
         Decl := Project_View.Decl.Attributes;
      else
         Decl := Tree.Shared.Packages.Table (Pkg_Id).Decl.Attributes;
      end if;

      Value := Prj.Util.Value_Of
        (Variable_Name => Attr_Id,
         In_Variables  => Decl,
         Shared        => Tree.Shared);

      if Value.Kind = Single then
         Get_Name_String (Value.Value);
         return Name_Buffer (1 .. Name_Len);

      elsif Value.Kind = List then
         Str := Value.Values;
         Get_Name_String (Tree.Shared.String_Elements.Table (Str).Value);
         return Name_Buffer (1 .. Name_Len);
      end if;

      return Default;

   exception
      when others =>
         return Default;
   end Output_Single_Attr;

   -----------------------
   -- Output_Array_Attr --
   -----------------------

   function Output_Array_Attr
     (Project_View : Project_Id;
      Tree         : Project_Tree_Ref;
      Gpr_Name     : String;
      Default      : String := "";
      Pkg_Name     : String := "";
      Index        : String := "") return String
   is
      Attr_Id : Name_Id;
      Pkg_Id : Package_Id := No_Package;
      Decl : Array_Id;
      Elemt : Array_Element_Id;
      Idx : Name_Id;
   begin
      Name_Len := Gpr_Name'Length;
      Name_Buffer (1 .. Name_Len) := Gpr_Name;
      Attr_Id := Name_Find;

      if Pkg_Name /= "" then
         Name_Len := Pkg_Name'Length;
         Name_Buffer (1 .. Name_Len) := Pkg_Name;
         Pkg_Id := Value_Of
           (Name_Find,
            In_Packages => Project_View.Decl.Packages,
            Shared      => Tree.Shared);
      end if;

      if Pkg_Id = No_Package then
         Decl := Project_View.Decl.Arrays;
      else
         Decl := Tree.Shared.Packages.Table (Pkg_Id).Decl.Arrays;
      end if;

      Elemt := Value_Of (Attr_Id, In_Arrays => Decl, Shared => Tree.Shared);

      Name_Len := Index'Length;
      Name_Buffer (1 .. Name_Len) := Index;
      Idx := Name_Find;

      Idx := Value_Of (Idx, In_Array => Elemt, Shared => Tree.Shared);
      if Idx = No_Name then
         return Default;
      else
         return Get_Name_String (Idx);
      end if;

   exception
      when others =>
         return Default;
   end Output_Array_Attr;

   -----------------------------
   -- Convert_From_Gpr_To_Adp --
   -----------------------------

   procedure Convert_From_Gpr_To_Adp (Gpr_Filename : String) is
      Project_View : Project_Id;
      Ide_Package : constant String := "ide";
      Tree      : constant Project_Node_Tree_Ref := new Project_Node_Tree_Data;
      Env       : Prj.Tree.Environment;
      View_Tree : constant Project_Tree_Ref := new Project_Tree_Data;
   begin
      Namet.Initialize;
      Csets.Initialize;
      Snames.Initialize;
      Prj.Initialize (View_Tree);
      Prj.Tree.Initialize (Env, Gnatmake_Flags);
      Prj.Tree.Initialize (Tree);

      Change_Dir (Dir_Name (Gpr_Filename));
      Prj.Pars.Parse
        (In_Tree           => View_Tree,
         Project           => Project_View,
         Project_File_Name => Gpr_Filename,
         Packages_To_Check => All_Packages,
         Env               => Env);

      if Project_View /= No_Project then
         declare
            Compiler : constant String := Output_Array_Attr
              (Project_View, View_Tree, "compiler_command", "gnatmake",
               Ide_Package, "ada");
            Debugger : constant String := Output_Single_Attr
              (Project_View, View_Tree, "debugger_command", "", Ide_Package);
            List : constant String := Output_Single_Attr
              (Project_View, View_Tree, "gnatlist", "", Ide_Package);
            Default_List : constant String := "gnatls";
         begin
            Output_Path
              (Ada_Include_Path (Project_View, View_Tree, True), "src_dir=");
            Output_Path (Ada_Objects_Path (Project_View, View_Tree).all,
                         "obj_dir=");
            Put_Line
              ("build_dir=" &
               Get_Name_String (Project_View.Exec_Directory.Display_Name));
            Put_Line ("comp_opt=-gnatQ -P" & Gpr_Filename);
            Put_Line ("make_cmd=" & Compiler & " ${comp_opt} ${main}");
            Put_Line ("comp_cmd=" & Compiler
                      & " -u -f ${comp_opt} ${full_current}");
            Put_Line ("run_cmd=cd ${build_dir}");
            Put_Line ("run_cmd=${main}");
            Put_Line ("check_cmd=cd ${build_dir}");
            Put_Line ("check_cmd=" & Compiler
                      & " -u -f -gnats ${comp_opt} ${full_current}");
            Put_Line ("debug_pre_cmd=cd ${build_dir}");
            Put_Line ("debug_post_cmd=");

            if Debugger /= "" then
               Put_Line ("debug_cmd=" & Debugger & " ${main}");
            end if;

            if List /= ""
              and then List'Length > Default_List'Length
              and then List (List'Last - Default_List'Length + 1 .. List'Last)
                = Default_List
            then
               Put_Line ("cross_prefix=" &
                         List (List'First .. List'Last - Default_List'Length));
            end if;

            Put_Line ("main_unit=");
            Put_Line ("remote_machine=" & Output_Single_Attr
                        (Project_View, View_Tree,
                         "remote_host", "", Ide_Package));
            Put_Line ("main=" & Output_Single_Attr
                        (Project_View, View_Tree,
                         "main", "${full_current}", ""));
         end;
      else
         Set_Exit_Status (Failure);
      end if;
   end Convert_From_Gpr_To_Adp;

end Convert.Gpr;
