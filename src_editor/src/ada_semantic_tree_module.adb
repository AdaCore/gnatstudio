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

with GNAT.Strings;                 use GNAT.Strings;

with Language.Tree.Database;       use Language.Tree.Database;
with Ada_Semantic_Tree.Assistants; use Ada_Semantic_Tree.Assistants;
with Basic_Types;                  use Basic_Types;
with Gtkada.MDI;                   use Gtkada.MDI;
with Gtkada;                       use Gtkada;
with Src_Editor_Module;            use Src_Editor_Module;
with Src_Editor_Buffer;            use Src_Editor_Buffer;
with Src_Editor_Box;               use Src_Editor_Box;
with Traces;                       use Traces;
with UTF8_Utils;                   use UTF8_Utils;
with Entities.Construct_Assistant;

package body Ada_Semantic_Tree_Module is

   use GPS.Kernel;

   type GPS_Buffer_Provider is new Buffer_Provider with record
      Kernel : Kernel_Handle;
   end record;

   ----------------
   -- Get_Buffer --
   ----------------

   overriding function Get_Buffer
     (Provider : access GPS_Buffer_Provider;
      File     : Virtual_File) return String_Access;
   --  Return the buffer from the editor if any, from the file otherwise

   overriding function Get_Timestamp
     (Provider : access GPS_Buffer_Provider;
      File     : Virtual_File) return Integer;

   -------------------
   -- Get_Timestamp --
   -------------------

   overriding function Get_Timestamp
     (Provider : access GPS_Buffer_Provider;
      File     : Virtual_File) return Integer
   is
      Editor : Gtkada.MDI.MDI_Child;
   begin
      if Is_Open (Provider.Kernel, File) then
         Editor := Find_Editor (Provider.Kernel, File);

         if Editor /= null then
            return
              Get_Timestamp
                (Get_Buffer (Source_Editor_Box (Get_Widget (Editor))));
         end if;
      end if;

      return -1;
   end Get_Timestamp;

   ----------------
   -- Get_Buffer --
   ----------------

   overriding function Get_Buffer
     (Provider : access GPS_Buffer_Provider;
      File     : Virtual_File) return String_Access
   is
      Editor  : Gtkada.MDI.MDI_Child;
      Tmp     : String_Access;
      Tmp2    : Unchecked_String_Access;
      Len     : Natural;
      Success : Boolean;
      pragma Unreferenced (Success);

   begin
      if Is_Open (Provider.Kernel, File) then
         Editor := Find_Editor (Provider.Kernel, File);

         if Editor /= null then
            return new String'
              (Get_Text
                 (Get_Buffer (Source_Editor_Box (Get_Widget (Editor))), 1, 1));
         end if;
      end if;

      --  Ensure result is UTF8 encoded

      Tmp := Read_File (File);
      Unknown_To_UTF8 (Tmp.all, Tmp2, Len, Success);

      if Tmp2 /= null then
         Free (Tmp);
         Tmp := new String'(Tmp2 (1 .. Len));
         Free (Tmp2);
      end if;

      return Tmp;
   end Get_Buffer;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Std_Entities_Files : Virtual_File)
   is
   begin
      Initialize
        (Get_Construct_Database (Kernel),
         new GPS_Buffer_Provider'
           (Buffer_Provider with Kernel => Kernel_Handle (Kernel)),
         Abstract_Language_Handler (Kernel.Get_Language_Handler));
      Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
        (Get_Construct_Database (Kernel), Std_Entities_Files);
      Entities.Construct_Assistant.Register_Assistant
        (Kernel.Get_Construct_Database,
         Kernel.Get_Database);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Register_Module;

end Ada_Semantic_Tree_Module;
