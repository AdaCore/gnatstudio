-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
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

with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GPS.Intl;           use GPS.Intl;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with Src_Editor_Box;     use Src_Editor_Box;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;

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

   procedure Create_Category
     (Style            : Style_Access;
      Mark_In_Speedbar : Boolean := False);
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
            Style    : constant Style_Access :=
                         Get_Or_Create_Style (Kernel, Style_ID, False);
            Box      : Source_Editor_Box;
            Child    : MDI_Child;
         begin
            if Style = null then
               Set_Error_Msg (Data, -"No such style: " & Style_ID);
               return;
            end if;

            Child := Find_Editor (Kernel, File);

            if Child /= null then
               Box := Source_Editor_Box (Get_Widget (Child));
               if Command = "highlight" then
                  Add_Line_Highlighting
                    (Get_Buffer (Box),
                     Editable_Line_Type (Line),
                     Style,
                     Highlight_In => (others => True));
               else
                  Remove_Line_Highlighting
                    (Get_Buffer (Box), Editable_Line_Type (Line), Style);
               end if;
            else
               Set_Error_Msg
                 (Data, -"File editor not found for file "
                  & Full_Name (File).all);
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
            Style := Get_Or_Create_Style (Kernel, Category, Create => True);
            Set_Background (Style, Color);

            Add_Category (Style, Mark_In_Speedbar => Mark_In_Speedbar);
         end;

      elsif Command = "highlight_range"
        or else Command = "unhighlight_range"
      then
         Name_Parameters (Data, Highlight_Range_Parameters);
         declare
            Module_Id : constant Source_Editor_Module :=
              Source_Editor_Module (Src_Editor_Module_Id);
            File      : constant Virtual_File  :=
              Create (Nth_Arg (Data, 1), Kernel);
            Style_ID  : constant String  := Nth_Arg (Data, 2);
            Line      : constant Integer := Nth_Arg (Data, 3, Default => 0);
            Start_Col : constant Visible_Column_Type :=
              Visible_Column_Type (Nth_Arg (Data, 4, Default => 0));
            End_Col   : constant Visible_Column_Type :=
              Visible_Column_Type (Nth_Arg (Data, 5, Default => -1));
            Style     : constant Style_Access :=
                          Get_Or_Create_Style (Kernel, Style_ID, False);
            Box       : Source_Editor_Box;
            Child     : MDI_Child;
            Category_Index : Integer;
         begin
            if Style = null then
               Set_Error_Msg (Data, -"No such style: " & Style_ID);
               return;
            end if;

            Child := Find_Editor (Kernel, File);

            if Child /= null then
               Box := Source_Editor_Box (Get_Widget (Child));
               Category_Index := Lookup_Category (Style);

               if Command = "highlight_range" then
                  Highlight_Range
                    (Get_Buffer (Box), Style,
                     Editable_Line_Type (Line),
                     Start_Col, End_Col);

                  if Category_Index /= 0
                    and then
                      Module_Id.Categories (Category_Index).Mark_In_Speedbar
                  then
                     Add_Line_Highlighting
                       (Get_Buffer (Box),
                        Editable_Line_Type (Line), Style,
                        Highlight_In => (Highlight_Speedbar => True,
                                         others             => False));
                  end if;
               else
                  Highlight_Range
                    (Get_Buffer (Box), Style,
                     Editable_Line_Type (Line),
                     Start_Col, End_Col, Remove => True);

                  if Category_Index /= 0
                    and then
                      Module_Id.Categories (Category_Index).Mark_In_Speedbar
                  then
                     Remove_Line_Highlighting
                       (Get_Buffer (Box),
                        Editable_Line_Type (Line),
                        Style);
                  end if;
               end if;
            else
               Set_Error_Msg
                 (Data, -"File editor not found for file "
                  & Full_Name (File).all);
            end if;
         end;
      end if;
   end Edit_Command_Handler;

   ---------------------
   -- Create_Category --
   ---------------------

   procedure Create_Category
     (Style            : Style_Access;
      Mark_In_Speedbar : Boolean := False)
   is
      Module_Id : constant Source_Editor_Module :=
                    Source_Editor_Module (Src_Editor_Module_Id);
      A         : Highlighting_Category_Array_Access;

   begin
      if Module_Id.Categories = null then
         Module_Id.Categories := new Highlighting_Category_Array (1 .. 1);
         Module_Id.Categories (1) := new Highlighting_Category_Record'
           (Mark_In_Speedbar => Mark_In_Speedbar,
            Style => Style);

      else
         A := new Highlighting_Category_Array
           (1 .. Module_Id.Categories'Last + 1);

         A (1 .. A'Last - 1) := Module_Id.Categories.all;

         Unchecked_Free (Module_Id.Categories);

         A (A'Last) := new Highlighting_Category_Record'
           (Mark_In_Speedbar => Mark_In_Speedbar,
            Style => Style);

         Module_Id.Categories := A;
      end if;
   end Create_Category;

   ------------------
   -- Add_Category --
   ------------------

   procedure Add_Category
     (Style            : Style_Access;
      Mark_In_Speedbar : Boolean := False)
   is
      Module_Id : constant Source_Editor_Module :=
                    Source_Editor_Module (Src_Editor_Module_Id);
      N         : Natural;
   begin
      --  If this category is already registered, change its parameters.

      N := Lookup_Category (Style);

      if N /= 0 then
         Module_Id.Categories (N).Style := Style;
         Module_Id.Categories (N).Mark_In_Speedbar := Mark_In_Speedbar;
         return;
      end if;

      --  If we reach this point, the category wasn't previously found.
      Create_Category (Style, Mark_In_Speedbar);
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

      Create_Category (Style, False);
      return Module_Id.Categories'Last;
   end Lookup_Category;

   ------------
   -- Get_GC --
   ------------

   function Get_GC (Index : Natural) return Gdk_GC is
      Module_Id : constant Source_Editor_Module :=
                    Source_Editor_Module (Src_Editor_Module_Id);

      use type Gdk_GC;
   begin
      if Index > 0 and then Index <= Module_Id.Categories'Last then
         return Get_Background_GC (Module_Id.Categories (Index).Style);
      else
         return null;
      end if;
   end Get_GC;

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
