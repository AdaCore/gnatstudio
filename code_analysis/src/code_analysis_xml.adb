------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Language.Tree.Database;  use Language.Tree.Database;
with Traces;                  use Traces;

package body Code_Analysis_XML is

   --------------
   -- Dump_XML --
   --------------

   procedure Dump_XML
     (Projects : Code_Analysis_Tree;
      Parent   : Node_Ptr)
   is
      use Project_Maps, File_Maps;
      Prj_Cur   : Project_Maps.Cursor := Projects.First;
      Prj_Node  : Node_Ptr;
      File_Cur  : File_Maps.Cursor;
      File_Node : Node_Ptr;

   begin
      while Has_Element (Prj_Cur) loop
         --  Create the project node
         Prj_Node := new XML_Utils.Node;
         Prj_Node.Tag := new String'("Project");
         Add_Child (Parent, Prj_Node, True);
         Add_File_Child
           (Prj_Node, "name", Project_Path (Element (Prj_Cur).Name));

         --  And add files as children
         File_Cur := Element (Prj_Cur).Files.First;
         while Has_Element (File_Cur) loop
            File_Node := new XML_Utils.Node;
            File_Node.Tag := new String'("File");
            Add_Child (Prj_Node, File_Node, True);
            Add_File_Child (File_Node, "name", Element (File_Cur).Name);

            Next (File_Cur);
         end loop;

         Next (Prj_Cur);
      end loop;
   end Dump_XML;

   ---------------
   -- Parse_XML --
   ---------------

   procedure Parse_XML
     (Project  : Project_Type;
      Node     : Node_Ptr)
   is
      Iter       : Project_Iterator;
      Prj_Child  : Node_Ptr;
      File       : Virtual_File;
      File_Child : Node_Ptr;

   begin
      if Node = null then
         return;
      end if;

      Prj_Child := Node.Child;

      while Prj_Child /= null loop
         if Prj_Child.Tag.all = "Project" then

            --  Let's find the corresponding Project_Type
            File    := Get_File_Child (Prj_Child, "name");
            Iter    := Start (Project, True, False, True);

            while Current (Iter) /= No_Project loop
               exit when Project_Path (Current (Iter)) = File;
               Next (Iter);
            end loop;

            --  Found the project: now get its source files
            if Current (Iter) /= No_Project then
               File_Child := Prj_Child.Child;

               while File_Child /= null loop
                  if File_Child.Tag.all = "File" then
                     File := Get_File_Child (File_Child, "name");
                     On_New_File (Current (Iter), File);
                  end if;

                  File_Child := File_Child.Next;
               end loop;
            end if;
         end if;

         Prj_Child := Prj_Child.Next;
      end loop;

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end Parse_XML;

end Code_Analysis_XML;
