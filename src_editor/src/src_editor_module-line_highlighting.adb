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

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Gdk.Color;         use Gdk.Color;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Glide_Kernel;      use Glide_Kernel;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Projects.Registry; use Projects.Registry;
with Glide_Intl;        use Glide_Intl;
with Traces;            use Traces;

with Src_Editor_Box;    use Src_Editor_Box;

package body Src_Editor_Module.Line_Highlighting is

   Me : constant Debug_Handle := Create
     ("Src_Editor_Module.Line_Highlighting");

   --------------------------
   -- Edit_Command_Handler --
   --------------------------

   function Edit_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String
   is
      Filename : GNAT.OS_Lib.String_Access;
      Line     : Natural := 0;
      Category : GNAT.OS_Lib.String_Access;
      Color_Id : GNAT.OS_Lib.String_Access;
      GC       : Gdk_GC;
      Color    : Gdk_Color;
      Success  : Boolean;

      Module_Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

   begin
      if Command = "src.highlight" or else Command = "src.unhighlight" then
         if Args'Length = 3 then
            declare
            begin
               Line := Positive'Value (Args (Args'First + 1).all);
            exception
               when others =>
                  return Command & ": " &
                    (-"option line requires a numerical value.");
            end;
         end if;

         Filename := new String'(Args (Args'First).all);
         Category := new String'(Args (Args'Last).all);

         declare
            Box   : Source_Box;
            Child : MDI_Child;
            File  : constant String := Get_Full_Path_From_File
              (Registry        => Get_Registry (Kernel),
               Filename        => Filename.all,
               Use_Source_Path => True,
               Use_Object_Path => False);

         begin
            Child := Find_Editor (Kernel, Filename.all);
            Free (Filename);

            if Child /= null then
               Box := Source_Box (Get_Widget (Child));
               if Command = "src.highlight" then
                  Add_Line_Highlighting (Box.Editor, Line, Category.all);
               else
                  Remove_Line_Highlighting (Box.Editor, Line, Category.all);
               end if;
            else
               Free (Category);

               return
                 Command & ": " & (-"File editor not found for file " & File);
            end if;
         end;

         Free (Filename);
         Free (Category);

      elsif Command = "src.register_highlighting" then
         if Args'Length /= 2 then
            return Command & ": " & (-"wrong number of arguments");
         end if;

         Category := new String'(Args (Args'First).all);
         Color_Id := new String'(Args (Args'Last).all);

         --  Create a GC from the color.

         Gdk_New (GC, Get_Window (Get_Main_Window (Module_Id.Kernel)));

         Color := Parse (Color_Id.all);
         Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

         Set_Foreground (GC, Color);

         Add_Category (Category.all, GC);

         Free (Category);
         Free (Color_Id);
      end if;

      return "";
   end Edit_Command_Handler;

   ------------------
   -- Add_Category --
   ------------------

   procedure Add_Category
     (Id : String;
      GC : Gdk_GC)
   is
      Module_Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      A : Highlighting_Category_Array_Access;
   begin
      if Module_Id.Categories = null then
         Module_Id.Categories := new Highlighting_Category_Array (1 .. 1);
         Module_Id.Categories (1) :=
           new Highlighting_Category_Record'(L  => Id'Length,
                                             Id => Id,
                                             GC => GC);

      else
         A := new Highlighting_Category_Array
           (1 .. Module_Id.Categories'Last + 1);

         A (1 .. A'Last - 1) := Module_Id.Categories.all;

         Unchecked_Free (Module_Id.Categories);

         A (A'Last) := new Highlighting_Category_Record'
           (L  => Id'Length,
            Id => Id,
            GC => GC);

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
