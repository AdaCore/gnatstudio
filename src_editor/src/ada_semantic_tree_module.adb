------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
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

with Ada.Calendar;                 use Ada.Calendar;
with Interfaces.C.Strings;         use Interfaces.C.Strings;

with GNAT.Strings;                 use GNAT.Strings;
with GNATCOLL.Projects;            use GNATCOLL.Projects;
with GNATCOLL.Traces;              use GNATCOLL.Traces;

with Basic_Types;                  use Basic_Types;
with GPS.Kernel.Charsets;          use GPS.Kernel.Charsets;
with Language.Tree.Database;       use Language.Tree.Database;
with Ada_Semantic_Tree.Assistants;
with Gtkada.MDI;                   use Gtkada.MDI;
with Gtkada;                       use Gtkada;
with Src_Editor_Module;            use Src_Editor_Module;
with Src_Editor_Buffer;            use Src_Editor_Buffer;
with Src_Editor_Box;               use Src_Editor_Box;
with Time_Utils;                   use Time_Utils;

package body Ada_Semantic_Tree_Module is
   Me : constant Trace_Handle := Create ("Ada_Semantic_Tree_Module", On);

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

      Stamp : Time;
      Y     : Year_Number;
      M     : Month_Number;
      D     : Day_Number;
      S     : Day_Duration;
   begin
      if Is_Open (Provider.Kernel, File) then
         Editor := Find_Editor
           (Provider.Kernel, File, No_Project);  --   ??? any project

         if Editor /= null then
            return
              Get_Version
                (Get_Buffer (Source_Editor_Box (Get_Widget (Editor))));
         end if;
      end if;

      Stamp := File.File_Time_Stamp;

      Local_Split (Stamp, Y, M, D, S);

      return D * 86400 + Integer (S);
   end Get_Timestamp;

   ----------------
   -- Get_Buffer --
   ----------------

   overriding function Get_Buffer
     (Provider : access GPS_Buffer_Provider;
      File     : Virtual_File) return String_Access
   is
      Editor  : Gtkada.MDI.MDI_Child;

   begin
      if Is_Open (Provider.Kernel, File) then
         Editor := Find_Editor
           (Provider.Kernel, File, No_Project); --  ??? any project

         if Editor /= null then
            return new String'
              (Get_Text
                 (Get_Buffer (Source_Editor_Box (Get_Widget (Editor))), 1, 1));
         end if;
      end if;

      --  Ensure result is UTF8 encoded

      declare
         UTF8     : chars_ptr;
         UTF8_Len : Natural;
         Props    : File_Props;
         Result   : String_Access;
      begin
         Read_File_With_Charset (File, UTF8, UTF8_Len, Props);
         --  We don't use Interfaces.C.Strings.Value function here to
         --  avoid stack overflow.

         if UTF8 = Null_Ptr then
            --  Defensive programming
            Trace (Me, "Could not read/convert contents of" &
                   (+File.Full_Name));
            return new String'("");
         end if;

         Result := new String (1 .. UTF8_Len);
         Result.all := To_Unchecked_String (UTF8) (1 .. UTF8_Len);
         Interfaces.C.Strings.Free (UTF8);

         return Result;
      end;
   end Get_Buffer;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Std_Entities_Files : Virtual_File)
   is
   begin
      Set_Provider
        (Get_Construct_Database (Kernel),
         Provider => new GPS_Buffer_Provider'
           (Buffer_Provider with Kernel => Kernel_Handle (Kernel)));
      Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
        (Get_Construct_Database (Kernel), Std_Entities_Files);
   exception
      when E : others =>
         Trace (Me, E);
   end Register_Module;

end Ada_Semantic_Tree_Module;
