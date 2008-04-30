-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2007, AdaCore                    --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Strings;            use GNAT.Strings;
with Code_Coverage;           use Code_Coverage;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with Projects.Registry;       use Projects.Registry;
with Language.Tree.Database;  use Language.Tree.Database;

package body Code_Analysis_XML is

   --------------
   -- Dump_XML --
   --------------

   procedure Dump_XML
     (Projects : Code_Analysis_Tree;
      Parent   : Node_Ptr)
   is
      use Project_Maps;
      Map_Cur  : Cursor := Projects.First;
      Sort_Arr : Project_Array (1 .. Integer (Projects.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := (Element (Map_Cur));
         Next (Map_Cur);
      end loop;

      Sort_Projects (Sort_Arr);

      for J in Sort_Arr'Range loop
         Dump_Project (Sort_Arr (J), Parent);
      end loop;
   end Dump_XML;

   ---------------
   -- Parse_XML --
   ---------------

   procedure Parse_XML
     (Project  : Project_Type;
      Projects : Code_Analysis_Tree;
      Child    : in out Node_Ptr)
   is
      Prj_Node  : Project_Access;
      Registry  : Project_Registry;
   begin
      Registry := Project_Registry (Get_Registry (Project));

      while Child /= null loop
         if Child.Tag.all = "Project" then
            Prj_Node := Get_Or_Create
              (Projects, Load_Or_Find
                 (Registry, To_Lower (Get_Attribute (Child, "name"))));
            Parse_Project (Prj_Node, Child);
         end if;

         Child := Child.Next;
      end loop;
   end Parse_XML;

   ------------------
   -- Dump_Project --
   ------------------

   procedure Dump_Project
     (Prj_Node : Project_Access;
      Parent   : Node_Ptr)
   is
      use File_Maps;
      Loc      : constant Node_Ptr := new Glib.Xml_Int.Node;
      Map_Cur  : Cursor := Prj_Node.Files.First;
      Sort_Arr : Code_Analysis.File_Array
        (1 .. Integer (Prj_Node.Files.Length));
   begin
      Loc.Tag := new String'("Project");
      Add_Child (Parent, Loc, True);
      Set_Attribute (Loc, "name", Project_Name (Prj_Node.Name));
      XML_Dump_Coverage (Prj_Node.Analysis_Data.Coverage_Data, Loc);

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         Dump_File (Sort_Arr (J), Loc);
      end loop;
   end Dump_Project;

   -------------------
   -- Parse_Project --
   -------------------

   procedure Parse_Project
     (Prj_Node : Project_Access;
      Parent   : Node_Ptr)
   is
      File_Node : Code_Analysis.File_Access;
      Child     : Node_Ptr;
   begin
      XML_Parse_Coverage (Prj_Node.Analysis_Data.Coverage_Data, Parent);

      if Parent.Child /= null then
         Child := Parent.Child;
         while Child /= null loop
            if Child.Tag.all = "File" then
               File_Node := Get_Or_Create
                 (Prj_Node, Create (Get_Attribute (Child, "name")));
               --  Create a Line_Array with exactly the same number of elements
               --  than to the number of code lines in the original src code
               --  file. It will contain the lines with analysis information.
               File_Node.Lines := new Line_Array
                 (1 .. Positive'Value (Get_Attribute (Child, "line_count")));
               File_Node.Lines.all := (others => Null_Line);
               Parse_File (File_Node, Child);
            end if;

            Child := Child.Next;
         end loop;
      end if;
   end Parse_Project;

   ---------------
   -- Dump_File --
   ---------------

   procedure Dump_File
     (File_Node : Code_Analysis.File_Access; Parent : Node_Ptr)
   is
      use Subprogram_Maps;
      Loc       : constant Node_Ptr := new Glib.Xml_Int.Node;
      Map_Cur   : Cursor := File_Node.Subprograms.First;
      Sort_Arr  : Subprogram_Array
        (1 .. Integer (File_Node.Subprograms.Length));
   begin
      Loc.Tag := new String'("File");
      Add_Child (Parent, Loc, True);
      Set_Attribute (Loc, "name", GNATCOLL.VFS.Full_Name (File_Node.Name).all);
      Set_Attribute
        (Loc, "line_count", Positive'Image (File_Node.Lines'Length));
      XML_Dump_Coverage (File_Node.Analysis_Data.Coverage_Data, Loc);

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := (Element (Map_Cur));
         Next (Map_Cur);
      end loop;

      Sort_Subprograms (Sort_Arr);

      for J in Sort_Arr'Range loop
         Dump_Subprogram (Sort_Arr (J), Loc);
      end loop;

      for J in 1 .. File_Node.Lines'Length loop
         Dump_Line (File_Node.Lines (J), Loc);
      end loop;
   end Dump_File;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File
     (File_Node : Code_Analysis.File_Access;
      Parent    : Node_Ptr)
   is
      Child     : Node_Ptr;
   begin
      XML_Parse_Coverage (File_Node.Analysis_Data.Coverage_Data, Parent);

      if Parent.Child /= null then
         Child := Parent.Child;

         while Child /= null loop
            if Child.Tag.all = "Subprogram" then
               --  We parse a subprogram node
               declare
                  Subp_Node : Subprogram_Access;
               begin
                  Subp_Node := Get_Or_Create
                    (File_Node, new String'(Get_Attribute (Child, "name")));
                  Subp_Node.Line :=
                    Natural'Value (Get_Attribute (Child, "line"));
                  Subp_Node.Column :=
                    Natural'Value (Get_Attribute (Child, "column"));
                  Subp_Node.Start :=
                    Natural'Value (Get_Attribute (Child, "start"));
                  Subp_Node.Stop :=
                    Natural'Value (Get_Attribute (Child, "stop"));
                  XML_Parse_Coverage
                    (Subp_Node.Analysis_Data.Coverage_Data, Child);
               end;
            elsif Child.Tag.all = "Line" then
               --  We parse a line node
               declare
                  Line_Node : Line;
                  Line_Num  : Natural;
                  Line_Contents : constant String :=
                                    Get_Attribute (Child, "contents");
               begin
                  Line_Num := Natural'Value (Get_Attribute (Child, "number"));
                  Line_Node.Number  := Line_Num;

                  if Line_Contents /= "" then
                     Line_Node.Contents := new String'(Line_Contents);
                  end if;

                  XML_Parse_Coverage
                    (Line_Node.Analysis_Data.Coverage_Data, Child);
                  File_Node.Lines (Line_Num) := Line_Node;
               end;
            end if;

            Child := Child.Next;
         end loop;
      end if;
   end Parse_File;

   ---------------------
   -- Dump_Subprogram --
   ---------------------

   procedure Dump_Subprogram
     (Subp_Node : Subprogram_Access; Parent : Node_Ptr)
   is
      use Subprogram_Maps;
      Loc : constant Node_Ptr := new Glib.Xml_Int.Node;
   begin
      Loc.Tag := new String'("Subprogram");
      Add_Child (Parent, Loc, True);
      Set_Attribute (Loc, "name", Subp_Node.Name.all);
      Set_Attribute (Loc, "line", Natural'Image (Subp_Node.Line));
      Set_Attribute (Loc, "column", Natural'Image (Subp_Node.Column));
      Set_Attribute (Loc, "start", Natural'Image (Subp_Node.Start));
      Set_Attribute (Loc, "stop", Natural'Image (Subp_Node.Stop));
      XML_Dump_Coverage (Subp_Node.Analysis_Data.Coverage_Data, Loc);
   end Dump_Subprogram;

   ---------------
   -- Dump_Line --
   ---------------

   procedure Dump_Line (Line_Node : Code_Analysis.Line; Parent : Node_Ptr) is
      Loc : Node_Ptr;
   begin
      if Line_Node.Number /= 0 then
         Loc := new Glib.Xml_Int.Node;
         Loc.Tag := new String'("Line");
         Add_Child (Parent, Loc, True);
         Set_Attribute (Loc, "number", Natural'Image (Line_Node.Number));
         XML_Dump_Coverage (Line_Node.Analysis_Data.Coverage_Data, Loc);

         if Line_Node.Contents /= null then
            Set_Attribute (Loc, "contents", Line_Node.Contents.all);
         end if;
      end if;
   end Dump_Line;

end Code_Analysis_XML;
