-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2007                      --
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

--  The case exception file is an XML file with the following structure.
--
--  <?xml version="1.0"?>
--  <custom_section>
--     <case_exceptions>
--        <word>OS_Lib</word>
--        <word>GNAT</word>
--        <substring>IO</substring>
--     </case_exceptions>
--  </custom_section>
--
--  <word>       : A full case exception word.
--  <substring>  : A substring exception. A substring is defined as a part
--                 of the word separated by underscores.

with GNAT.OS_Lib;    use GNAT.OS_Lib;

with Traces;         use Traces;
with Glib.Xml_Int;   use Glib.Xml_Int;
with XML_Parsers;

package body Case_Handling.IO is

   Me : constant Debug_Handle := Create ("Case_Handling.IO");

   ---------------------
   -- Load_Exceptions --
   ---------------------

   procedure Load_Exceptions
     (C         : in out Casing_Exceptions;
      Filename  : String;
      Read_Only : Boolean)
   is
      File, Child : Node_Ptr;
      Err         : String_Access;
   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);

         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Trace (Me, Err.all);
            Free (Err);

         else
            --  Get node exceptions

            Child := File.Child;

            --  Get node exception

            Child := Child.Child;

            while Child /= null loop
               if Child.Tag.all = "word" then
                  Add_Exception (C, Child.Value.all, Read_Only);
               elsif Child.Tag.all = "substring" then
                  Add_Substring_Exception (C, Child.Value.all, Read_Only);
               else
                  Trace (Exception_Handle,
                         "Unknown casing exceptions node " & Child.Tag.all);
               end if;
               Child := Child.Next;
            end loop;

            Free (File);
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Load_Exceptions;

   ---------------------
   -- Save_Exceptions --
   ---------------------

   procedure Save_Exceptions (C : in Casing_Exceptions; Filename : String) is
      File, Ada_Child : Node_Ptr;
      Child           : Node_Ptr;
      Iter            : String_Hash_Table.Iterator;
      N               : W_Node;

   begin
      if C.E = null and then C.S = null then
         return;
      end if;

      File     := new Node;
      File.Tag := new String'("custom_section");

      Ada_Child     := new Node;
      Ada_Child.Tag := new String'("case_exceptions");
      Add_Child (File, Ada_Child);

      --  Word exceptions

      if C.E /= null then
         String_Hash_Table.Get_First (C.E.all, Iter);

         loop
            N := String_Hash_Table.Get_Element (Iter);
            exit when N = Null_Node;

            if not N.Read_Only then
               Child       := new Node;
               Child.Tag   := new String'("word");
               Child.Value := new String'(N.Word.all);
               Add_Child (Ada_Child, Child);
            end if;

            String_Hash_Table.Get_Next (C.E.all, Iter);
         end loop;
      end if;

      --  Substring exceptions

      if C.S /= null then
         String_Hash_Table.Get_First (C.S.all, Iter);

         loop
            N := String_Hash_Table.Get_Element (Iter);
            exit when N = Null_Node;

            if not N.Read_Only then
               Child       := new Node;
               Child.Tag   := new String'("substring");
               Child.Value := new String'(N.Word.all);
               Add_Child (Ada_Child, Child);
            end if;

            String_Hash_Table.Get_Next (C.S.all, Iter);
         end loop;
      end if;

      Trace (Me, "Saving " & Filename);
      Print (File, Filename);
      Free (File);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Save_Exceptions;

end Case_Handling.IO;
