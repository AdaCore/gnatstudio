-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Prj.Env;  use Prj.Env;
with Prj.Part; use Prj.Part;

with GNATCOLL.Traces; use GNATCOLL.Traces;
with Namet; use Namet;
with Types; use Types;
with Sinput; use Sinput;
with Prj.PP; use Prj.PP;
with Ada.Text_IO; use Ada.Text_IO;

package body Toolchains.Project_Parsers is

   Me : constant Trace_Handle := Create ("TARGET_PARSER");

   -----------
   -- Parse --
   -----------

   procedure Parse
     (This         : Project_Parser;
      Manager      : Toolchain_Manager;
      Path         : Virtual_File;
      Project_Path : String)
   is
      Decl_Id : Project_Node_Id;
      Item_Id : Project_Node_Id;
      Name    : Name_Id;
   begin
      This.Manager := Manager;
      This.Tree_Data := new Project_Tree_Data;
      Prj.Initialize (This.Tree_Data);

      This.Node_Data := new Project_Node_Tree_Data;
      Initialize (This.Node_Data);

      Set_Path (This.Node_Data.Project_Path, Project_Path);

      Parse (In_Tree                => This.Node_Data,
             Project                => This.Enclosing_Project_Node,
             Project_File_Name      => String (Path.Base_Name),
             Always_Errout_Finalize => True,
             Packages_To_Check      => All_Packages,
             Store_Comments         => True,
             Current_Directory      => String (Path.Dir_Name),
             Is_Config_File         => False,
             Flags                  => Create_Flags
               (Report_Error               => null,
                When_No_Sources            => Silent,
                Require_Sources_Other_Lang => False,
                Allow_Duplicate_Basenames  => True,
                Compiler_Driver_Mandatory  => False,
                Error_On_Unknown_Language  => False,
                Require_Obj_Dirs           => Silent,
                Allow_Invalid_External     => Silent,
                Missing_Source_Files       => Silent));

      This.Root_Project := new Parsed_Project_Record;
      Initialize
        (This.Root_Project,
         This,
         This.Node_Data,
         This.Enclosing_Project_Node);

      This.Root_Project_Node := Project_Declaration_Of
        (This.Enclosing_Project_Node, This.Node_Data);

      Decl_Id := First_Declarative_Item_Of
        (This.Root_Project_Node, This.Node_Data);

      --  Look for an ide package. Create a toolchain parser on this IDE
      --  package to extract the toolchain information for this project.

      while Decl_Id /= Empty_Node loop
         Item_Id := Current_Item_Node (Decl_Id, This.Node_Data);

         if Kind_Of (Item_Id, This.Node_Data) = N_Package_Declaration then
            Name := Name_Of (Item_Id, This.Node_Data);

            if Get_Name_String (Name) = "ide" then
               This.Toolchain_Found := new Toolchain_Parser_Record;
               Parse (This.Toolchain_Found.all, This, This.Node_Data, Item_Id);
            end if;
         end if;

         Decl_Id := Next_Declarative_Item (Decl_Id, This.Node_Data);
      end loop;

      if This.Toolchain_Found = null then
         This.Toolchain_Found := new Toolchain_Parser_Record;
         Parse (This.Toolchain_Found.all, This, This.Node_Data, Empty_Node);
      end if;

      This.Is_Valid := True;

   exception
      when E : others =>
         Trace (Me, E);
         This.Is_Valid := False;
   end Parse;

   -----------------
   -- Get_Manager --
   -----------------

   function Get_Manager
     (This : Project_Parser_Record) return Toolchain_Manager
   is
   begin
      return This.Manager;
   end Get_Manager;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Project_Parser_Record) return Boolean is
   begin
      return This.Is_Valid;
   end Is_Valid;

   --------------------------
   -- Get_Toolchain_Parser --
   --------------------------

   function Get_Toolchain_Parser
     (This : Project_Parser_Record) return Toolchain_Parser is
   begin
      return This.Toolchain_Found;
   end Get_Toolchain_Parser;

   ----------------------
   -- Get_Root_Project --
   ----------------------

   function Get_Root_Project
     (This : Project_Parser_Record) return Parsed_Project
   is
   begin
      return This.Root_Project;
   end Get_Root_Project;

   ------------------------
   -- Get_Parsed_Project --
   ------------------------

   function Get_Parsed_Project
     (This : Project_Parser_Record;
      Node : Project_Node_Id) return Parsed_Project
   is
   begin
      return This.Parsed_Projects.Element (Node);
   end Get_Parsed_Project;

   ------------------
   -- Get_Variable --
   ------------------

   function Get_Variable
     (This : Parsed_Project_Record;
      Name : String) return Project_Node_Id
   is
   begin
      if This.Variables.Contains (Name) then
         return This.Variables.Element (Name);
      else
         return Empty_Node;
      end if;
   end Get_Variable;

   ----------------------
   -- Get_Project_Node --
   ----------------------

   function Get_Project_Node
     (This : Parsed_Project_Record) return Project_Node_Id is
   begin
      return This.Project_Node;
   end Get_Project_Node;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : Parsed_Project;
      Parser       : Project_Parser;
      Node_Data    : Project_Node_Tree_Ref;
      Project_Node : Project_Node_Id)
   is
      Source_File    : Source_File_Index;
      Item_Id        : Project_Node_Id;
      With_Id        : Project_Node_Id;
      With_Prj_Id    : Project_Node_Id;
      Decl_Id        : Project_Node_Id;
      Name           : Name_Id;
      Withed_Project : Parsed_Project;
   begin
      This.Is_Root := True;
      This.Node_Data := Node_Data;
      This.Project_Node := Project_Node;

      Source_File := Get_Source_File_Index
        (Location_Of (This.Project_Node, This.Node_Data));
      This.Path := Create
        (+Get_Name_String (Full_File_Name (Source_File)));

      Parser.Parsed_Projects.Insert (This.Project_Node, This);

      --  Creates a parser for all withed project recursively

      With_Id := First_With_Clause_Of (This.Project_Node, This.Node_Data);

      while With_Id /= Empty_Node loop

         --  consider only non-limited with
         if Non_Limited_Project_Node_Of
           (With_Id, This.Node_Data) /= Empty_Node
         then
            With_Prj_Id := Project_Node_Of (With_Id, This.Node_Data);

            if not Parser.Parsed_Projects.Contains (With_Prj_Id) then
               Withed_Project := new Parsed_Project_Record;
               Initialize
                 (Withed_Project,
                  Parser,
                  This.Node_Data,
                  With_Prj_Id);
               Withed_Project.Is_Root := False;
            end if;
         end if;

         With_Id := Next_With_Clause_Of (With_Id, This.Node_Data);
      end loop;

      --  Extract all scenario variables declared in this project

      Decl_Id := First_Declarative_Item_Of
        (Project_Declaration_Of
           (This.Project_Node, This.Node_Data), This.Node_Data);

      while Decl_Id /= Empty_Node loop
         Item_Id := Current_Item_Node (Decl_Id, This.Node_Data);

         if Kind_Of (Item_Id, This.Node_Data)
           = N_Typed_Variable_Declaration
         then
            Name := Name_Of (Item_Id, This.Node_Data);
            This.Variables.Insert (Get_Name_String (Name), Item_Id);
         end if;

         Decl_Id := Next_Declarative_Item (Decl_Id, This.Node_Data);
      end loop;
   end Initialize;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (This : Parsed_Project_Record) return Boolean is
   begin
      return This.Is_Root;
   end Is_Root;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (This : Parsed_Project_Record) return Virtual_File is
   begin
      return This.Path;
   end Get_Path;

   ----------
   -- Save --
   ----------

   procedure Save (This : Parsed_Project_Record) is
   begin
      Save (This, This.Path);
   end Save;

   ----------
   -- Save --
   ----------

   procedure Save (This : Parsed_Project_Record; To : Virtual_File) is
      File : Ada.Text_IO.File_Type;

      procedure Write_Char_Ap (C : Character);
      procedure Write_Eol_Ap;
      procedure Write_Str_Ap (S : String);

      procedure Write_Char_Ap (C : Character) is
      begin
         Put (File, C);
      end Write_Char_Ap;

      procedure Write_Eol_Ap is
      begin
         New_Line (File);
      end Write_Eol_Ap;

      procedure Write_Str_Ap (S : String) is
      begin
         Put (File, S);
      end Write_Str_Ap;

   begin
      Create (File, Out_File, String (To.Full_Name.all));

      Pretty_Print
        (Project                => This.Project_Node,
         In_Tree                => This.Node_Data,
         W_Char                 => Write_Char_Ap'Unrestricted_Access,
         W_Eol                  => Write_Eol_Ap'Unrestricted_Access,
         W_Str                  => Write_Str_Ap'Unrestricted_Access,
         Backward_Compatibility => False);

      Close (File);
   end Save;

end Toolchains.Project_Parsers;
