-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
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

with Gdk.Color;         use Gdk.Color;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Glide_Kernel;      use Glide_Kernel;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Intl;        use Glide_Intl;
with Traces;            use Traces;
with VFS;               use VFS;

with Src_Editor_Box;    use Src_Editor_Box;
with Src_Editor_Buffer; use Src_Editor_Buffer;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;

package body Src_Editor_Module.Line_Highlighting is

   Me : constant Debug_Handle := Create
     ("Src_Editor_Module.Line_Highlighting");

   --------------------------
   -- Edit_Command_Handler --
   --------------------------

   procedure Edit_Command_Handler
     (Data : in out Glide_Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      Module_Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

   begin
      if Command = "highlight" or else Command = "unhighlight" then
         declare
            File     : constant Virtual_File  :=
              Create (Nth_Arg (Data, 1), Kernel);
            Category : constant String  := Nth_Arg (Data, 2);
            Line     : constant Integer := Nth_Arg (Data, 3, Default => 0);
            Box   : Source_Box;
            Child : MDI_Child;
         begin
            Child := Find_Editor (Kernel, File);

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));
               if Command = "highlight" then
                  Add_Line_Highlighting
                    (Box.Editor, Editable_Line_Type (Line), Category);
               else
                  Remove_Line_Highlighting
                    (Box.Editor, Editable_Line_Type (Line), Category);
               end if;
            else
               Set_Error_Msg
                 (Data, -"File editor not found for file " & Full_Name (File));
            end if;
         end;

      elsif Command = "register_highlighting" then
         declare
            Category : constant String := Nth_Arg (Data, 1);
            Color_Id : constant String := Nth_Arg (Data, 2);
            GC       : Gdk_GC;
            Color    : Gdk_Color;
            Success  : Boolean;
         begin
            --  Create a GC from the color.

            Gdk_New (GC, Get_Window (Get_Main_Window (Module_Id.Kernel)));

            begin
               Color := Parse (Color_Id);
            exception
               when Wrong_Color =>
                  Set_Error_Msg (Data, -"Could not parse color: " & Color_Id);
            end;

            Alloc_Color (Get_Default_Colormap, Color, False, True, Success);
            Set_Foreground (GC, Color);
            Add_Category (Category, GC, Color);
         end;

      elsif Command = "highlight_range"
        or else Command = "unhighlight_range"
      then
         declare
            File      : constant Virtual_File  :=
              Create (Nth_Arg (Data, 1), Kernel);
            Category  : constant String  := Nth_Arg (Data, 2);
            Line      : constant Integer := Nth_Arg (Data, 3, Default => 0);
            Start_Col : constant Integer := Nth_Arg (Data, 4, Default => 0);
            End_Col   : constant Integer := Nth_Arg (Data, 5, Default => -1);
            Box       : Source_Box;
            Child     : MDI_Child;
         begin
            Child := Find_Editor (Kernel, File);

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));
               if Command = "highlight_range" then
                  Highlight_Range
                    (Get_Buffer (Box.Editor), Category, Line,
                     Start_Col, End_Col);
               else
                  Highlight_Range
                    (Get_Buffer (Box.Editor), Category, Line,
                     Start_Col, End_Col, Remove => True);
               end if;
            else
               Set_Error_Msg
                 (Data, -"File editor not found for file " & Full_Name (File));
            end if;
         end;
      end if;
   end Edit_Command_Handler;

   ------------------
   -- Add_Category --
   ------------------

   procedure Add_Category
     (Id    : String;
      GC    : Gdk_GC;
      Color : Gdk_Color)
   is
      Module_Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      A : Highlighting_Category_Array_Access;
      N : Natural;
   begin
      --  If this category is already registered, change its parameters.

      N := Lookup_Category (Id);

      if N /= 0 then
         Module_Id.Categories (N).GC := GC;
         return;
      end if;

      --  If we reach this point, the category wasn't previously found.

      if Module_Id.Categories = null then
         Module_Id.Categories := new Highlighting_Category_Array (1 .. 1);
         Module_Id.Categories (1) :=
           new Highlighting_Category_Record'(L  => Id'Length,
                                             Id => Id,
                                             GC => GC,
                                             Color => Color);

      else
         A := new Highlighting_Category_Array
           (1 .. Module_Id.Categories'Last + 1);

         A (1 .. A'Last - 1) := Module_Id.Categories.all;

         Unchecked_Free (Module_Id.Categories);

         A (A'Last) := new Highlighting_Category_Record'
           (L  => Id'Length,
            Id => Id,
            GC => GC,
            Color => Color);

         Module_Id.Categories := A;
      end if;
   end Add_Category;

   ---------------------
   -- Lookup_Category --
   ---------------------

   function Lookup_Category (Id : String) return Natural is
      Module_Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
   begin
      if Module_Id.Categories /= null then
         for J in Module_Id.Categories'Range loop
            if Module_Id.Categories (J).Id = Id then
               return J;
            end if;
         end loop;
      end if;

      return 0;
   end Lookup_Category;

   ------------
   -- Get_GC --
   ------------

   function Get_GC (Index : Natural) return Gdk_GC is
      Module_Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
   begin
      if Index > 0 and then Index <= Module_Id.Categories'Last then
         return Module_Id.Categories (Index).GC;
      else
         Trace (Me, -"Wrong category Id when getting color");
         return null;
      end if;
   end Get_GC;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color (Index : Natural) return Gdk_Color is
      Module_Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
   begin
      if Index > 0 and then Index <= Module_Id.Categories'Last then
         return Module_Id.Categories (Index).Color;
      else
         Trace (Me, -"Wrong category Id when getting color");
         return Null_Color;
      end if;
   end Get_Color;

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
