-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2005                         --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This unit records the docgen format descriptions. It is used by the docgen
--  backend.

package Docgen_Registry is

   type Output_Type is (Text, Binary);
   --  Text   : the output format is text-based, the rendering is done through
   --           a set of template files.
   --  Binary : a binary output format, the rendering is done internally.

   type Entities_Kind is
     (File_Header_Kind, File_Footer_Kind,
      Comment_Kind, Keyword_Kind, String_Kind, Char_Kind,
      Subtitle_Kind, Package_Desc_Kind, Package_Kind, With_Kind,
      Variable_Kind, Exception_Kind, Type_Kind, Tagged_Type_Kind,
      Calls_References_Kind, Caller_References_Kind,
      Entity_Kind, Subprogram_Kind, Header_Kind, Footer_Kind,
      Header_Private_Kind, Main_Frame_Kind,
      Unit_Index_Kind, Subprogram_Index_Kind, Index_Tagged_Type_Kind,
      Type_Index_Kind, Tagged_Type_Index_Kind, Index_Item_Kind,
      Private_Index_Kind, Public_Index_Kind, End_Index_Kind, Block_Kind,
      Link_Kind, Description_Kind);
   --  The entities supported by the textual docgen backend, for each one a
   --  separate template file will be used for the final rendering.

   type String_Access is access String;

   type Templates_Set is array (Entities_Kind) of String_Access;

   type Output_Description is record
      Format             : Output_Type;
      Name               : String_Access;
      --  The name of the backend used dialogs
      Description        : String_Access;
      --  A description for this backend
      Extension          : String_Access;
      --  The extension to be used for the generated files
      Entities_Templates : Templates_Set;
      --  The templates files for each entity
   end record;
   --  Contains all the output information
   type Output_Description_Access is access Output_Description;

   procedure Insert (O : Output_Description);
   --  Insert a new format into the registry

   function Length return Natural;
   --  Returns the number of entry into the output registry

   function Get (Index : in Positive) return Output_Description_Access;
   --  Returns the Index'th output in the table or null if Index > Length

end Docgen_Registry;
