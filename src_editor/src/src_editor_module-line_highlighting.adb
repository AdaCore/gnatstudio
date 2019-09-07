------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with GPS.Editors;            use GPS.Editors;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Scripts;     use GPS.Kernel.Scripts;

package body Src_Editor_Module.Line_Highlighting is

   Category_Cst  : aliased constant String := "category";
   Color_Cst     : aliased constant String := "color";
   Speedbar_Cst  : aliased constant String := "speedbar";
   File_Cst      : aliased constant String := "file";
   Line_Cst      : aliased constant String := "line";
   Start_Col_Cst : aliased constant String := "start_column";
   End_Col_Cst   : aliased constant String := "end_column";

   Register_Highlighting_Parameters : constant Cst_Argument_List :=
     (1 => Category_Cst'Access,
      2 => Color_Cst'Access,
      3 => Speedbar_Cst'Access);

   Highlight_Parameters : constant Cst_Argument_List :=
     (1 => File_Cst'Access,
      2 => Category_Cst'Access,
      3 => Line_Cst'Access);

   Highlight_Range_Parameters : constant Cst_Argument_List :=
     (1 => File_Cst'Access,
      2 => Category_Cst'Access,
      3 => Line_Cst'Access,
      4 => Start_Col_Cst'Access,
      5 => End_Col_Cst'Access);

   procedure Create_Category (Style : Style_Access);
   --  Create a new category with Style and Mark_In_Speedbar.

   --------------------------
   -- Edit_Command_Handler --
   --------------------------

   procedure Edit_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);

   begin
      if Command = "highlight" or else Command = "unhighlight" then
         Name_Parameters (Data, Highlight_Parameters);
         declare
            File     : constant Virtual_File  :=
                         Create (Nth_Arg (Data, 1), Kernel);
            Style_ID : constant String  := Nth_Arg (Data, 2);
            Line     : constant Integer := Nth_Arg (Data, 3, Default => 0);
            Buffer   : constant Editor_Buffer'Class :=
              Get_Buffer_Factory (Kernel).Get (File, Open_View => False);
         begin
            if Buffer /= Nil_Editor_Buffer then
               if Command = "highlight" then
                  Buffer.Apply_Style (Style => Style_ID, Line  => Line);
               else
                  Buffer.Remove_Style (Style => Style_ID, Line => Line);
               end if;
            else
               Set_Error_Msg
                 (Data, -"File editor not found for file "
                  & Display_Full_Name (File));
            end if;
         end;

      elsif Command = "register_highlighting" then
         Name_Parameters (Data, Register_Highlighting_Parameters);
         declare
            Category         : constant String := Nth_Arg (Data, 1);
            Color            : constant String := Nth_Arg (Data, 2);
            Mark_In_Speedbar : constant Boolean := Nth_Arg (Data, 3, False);
            Style            : Style_Access;
         begin
            --  Create the style corresponding to the higlighing category
            Style := Get_Style_Manager (Kernel).Get_Or_Create (Category);

            Set_Background (Style, Parse_Color (Color));
            Set_In_Speedbar (Style, Mark_In_Speedbar);

            Add_Category (Style);
         end;

      elsif Command = "highlight_range"
        or else Command = "unhighlight_range"
      then
         Name_Parameters (Data, Highlight_Range_Parameters);
         declare
            File      : constant Virtual_File  :=
              Create (Nth_Arg (Data, 1), Kernel);
            Style_ID  : constant String  := Nth_Arg (Data, 2);
            Line      : constant Integer := Nth_Arg (Data, 3, Default => 0);
            Start_Col : constant Visible_Column_Type :=
              Visible_Column_Type (Nth_Arg (Data, 4, Default => 0));
            End_Col   : constant Visible_Column_Type :=
              Visible_Column_Type (Nth_Arg (Data, 5, Default => -1));
            Buffer    : constant Editor_Buffer'Class :=
              Get_Buffer_Factory (Kernel).Get (File, Open_View => False);
         begin
            if Buffer /= Nil_Editor_Buffer then
               if Command = "highlight_range" then
                  Buffer.Apply_Style
                    (Style       => Style_ID,
                     Line        => Line,
                     From_Column => Start_Col,
                     To_Column   => End_Col);
               else
                  Buffer.Remove_Style
                    (Style       => Style_ID,
                     Line        => Line,
                     From_Column => Start_Col,
                     To_Column   => End_Col);
               end if;
            else
               Set_Error_Msg
                 (Data, -"File editor not found for file "
                  & Display_Full_Name (File));
            end if;
         end;
      end if;
   end Edit_Command_Handler;

   ---------------------
   -- Create_Category --
   ---------------------

   procedure Create_Category (Style : Style_Access) is
      Module_Id : constant Source_Editor_Module :=
                    Source_Editor_Module (Src_Editor_Module_Id);
      A         : Highlighting_Category_Array_Access;

   begin
      if Module_Id.Categories = null then
         Module_Id.Categories := new Highlighting_Category_Array (1 .. 1);
         Module_Id.Categories (1) := new Highlighting_Category_Record'
           (Style => Style);

      else
         A := new Highlighting_Category_Array
           (1 .. Module_Id.Categories'Last + 1);

         A (1 .. A'Last - 1) := Module_Id.Categories.all;

         Unchecked_Free (Module_Id.Categories);

         A (A'Last) := new Highlighting_Category_Record'(Style => Style);

         Module_Id.Categories := A;
      end if;
   end Create_Category;

   ------------------
   -- Add_Category --
   ------------------

   procedure Add_Category (Style : Style_Access) is
      Module_Id : constant Source_Editor_Module :=
                    Source_Editor_Module (Src_Editor_Module_Id);
      N         : Natural;
   begin
      --  If this category is already registered, change its parameters.

      N := Lookup_Category (Style);

      if N /= 0 then
         Module_Id.Categories (N).Style := Style;
         return;
      end if;

      --  If we reach this point, the category wasn't previously found.
      Create_Category (Style);
   end Add_Category;

   ---------------------
   -- Lookup_Category --
   ---------------------

   function Lookup_Category (Style : Style_Access) return Natural is
      Module_Id : constant Source_Editor_Module :=
                    Source_Editor_Module (Src_Editor_Module_Id);

   begin
      if Style = null then
         return 0;
      end if;

      if Module_Id.Categories /= null then
         for J in Module_Id.Categories'Range loop
            if Module_Id.Categories (J).Style = Style then
               return J;
            end if;
         end loop;
      end if;

      --  If the Category doesn't exist, but the style exists, create a
      --  category for this style.

      Create_Category (Style);
      return Module_Id.Categories'Last;
   end Lookup_Category;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color (Index : Natural) return Gdk_RGBA is
      Module_Id : constant Source_Editor_Module :=
                    Source_Editor_Module (Src_Editor_Module_Id);

   begin
      if Index > 0 and then Index <= Module_Id.Categories'Last then
         return Get_Background (Module_Id.Categories (Index).Style);
      else
         return Null_RGBA;
      end if;
   end Get_Color;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Index : Natural) return String is
      Module_Id : constant Source_Editor_Module :=
                    Source_Editor_Module (Src_Editor_Module_Id);

   begin
      if Index > 0 and then Index <= Module_Id.Categories'Last then
         return Get_Name (Module_Id.Categories (Index).Style);
      else
         return "";
      end if;
   end Get_Name;

   --------------------
   -- Get_Last_Index --
   --------------------

   function Get_Last_Index return Natural is
      Module_Id : constant Source_Editor_Module :=
                    Source_Editor_Module (Src_Editor_Module_Id);
   begin
      if Module_Id.Categories = null then
         return 0;
      else
         return Module_Id.Categories'Last;
      end if;
   end Get_Last_Index;

end Src_Editor_Module.Line_Highlighting;
