-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
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

with GNAT.Regpat;

with Glib.Convert;
with Gtk.Tree_Model;            use Gtk.Tree_Model;

with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;
with GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;         use GPS.Kernel.Styles;
with GPS.Location_Model;        use GPS.Location_Model;
with GPS.Location_View;         use GPS.Location_View;
with Traces;                    use Traces;

package body GPS.Kernel.Locations is

   -----------
   -- Hooks --
   -----------

   type File_Edited_Hook_Record is new Function_With_Args with null record;
   type File_Edited_Hook is access File_Edited_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" hook

   function Location_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class) return Boolean;
   --  Called when the user executes Location_Action_Hook

   --------------------
   -- Category_Count --
   --------------------

   function Category_Count
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String) return Natural
   is
      View  : constant GPS.Location_View.Location_View :=
                GPS.Location_View.Get_Or_Create_Location_View
                  (Kernel, Allow_Creation => False);

   begin
      if View /= null then
         return Category_Count (View.Model, Category);

      else
         return 0;
      end if;
   end Category_Count;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Hook);

      View  : constant GPS.Location_View.Location_View :=
                GPS.Location_View.Get_Or_Create_Location_View (Kernel, False);
      File  : constant Virtual_File := File_Hooks_Args (Data.all).File;

      Category_Iter : Gtk_Tree_Iter;
      File_Iter     : Gtk_Tree_Iter;
      Line_Iter     : Gtk_Tree_Iter;
   begin
      if View = null then
         return;
      end if;

      --  Loop on the files in the result view and highlight lines as
      --  necessary.

      Category_Iter := View.Model.Get_Iter_First;

      while Category_Iter /= Null_Iter loop
         File_Iter := View.Model.Children (Category_Iter);

         while File_Iter /= Null_Iter loop
            if File = Get_File (View.Model, File_Iter) then
               --  The file which has just been opened was in the locations
               --  view, highlight lines as necessary.
               Line_Iter := View.Model.Children (File_Iter);

               while Line_Iter /= Null_Iter loop
                  Highlight_Line
                    (Kernel,
                     File,
                     Integer
                       (View.Model.Get_Int (Line_Iter, Line_Column)),
                     Basic_Types.Visible_Column_Type
                       (View.Model.Get_Int (Line_Iter, Column_Column)),
                     Integer
                       (View.Model.Get_Int (Line_Iter, Length_Column)),
                     Get_Highlighting_Style (View.Model, Line_Iter));

                  View.Model.Next (Line_Iter);
               end loop;
            end if;

            View.Model.Next (File_Iter);
         end loop;

         View.Model.Next (Category_Iter);
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Execute;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename           : GNATCOLL.VFS.Virtual_File;
      Line               : Natural;
      Column             : Basic_Types.Visible_Column_Type;
      Length             : Natural;
      Highlight_Category : GPS.Kernel.Styles.Style_Access;
      Highlight          : Boolean := True)
   is
   begin
      if Highlight_Category = null then
         return;
      end if;

      if Highlight then
         if Length /= 0 then
            Get_Buffer_Factory (Kernel)
              .Get (Filename, Open_View => False)
              .Apply_Style
                (Style => Highlight_Category,
                 Line  => Line,
                 From_Column => Integer (Column),
                 To_Column   => Integer (Column) + Length);

         else
            Get_Buffer_Factory (Kernel)
              .Get (Filename, Open_View => False)
              .Apply_Style (Style => Highlight_Category, Line  => Line);
         end if;

      else
         Get_Buffer_Factory (Kernel)
           .Get (Filename, Open_View => False)
           .Remove_Style (Style       => Highlight_Category,
                          Line        => Line,
                          From_Column => Integer (Column),
                          To_Column   => Integer (Column) + Length);
      end if;
   end Highlight_Line;

   ---------------------
   -- Insert_Location --
   ---------------------

   procedure Insert_Location
     (Kernel             : access Kernel_Handle_Record'Class;
      Category           : Glib.UTF8_String;
      File               : GNATCOLL.VFS.Virtual_File;
      Text               : Glib.UTF8_String;
      Line               : Positive;
      Column             : Basic_Types.Visible_Column_Type;
      Length             : Natural := 0;
      Highlight          : Boolean := False;
      Highlight_Category : GPS.Kernel.Styles.Style_Access := null;
      Quiet              : Boolean := False;
      Remove_Duplicates  : Boolean := True;
      Enable_Counter     : Boolean := True;
      Has_Markups        : Boolean := False;
      Sort_In_File       : Boolean := False;
      Look_For_Secondary : Boolean := False)
   is
      View : constant GPS.Location_View.Location_View :=
               GPS.Location_View.Get_Or_Create_Location_View (Kernel);
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;

   begin
      if View /= null then
         if Has_Markups then
            GPS.Location_View.Add_Location
              (View,
               Glib.Convert.Escape_Text (Category),
               File, Line, Column, Length,
               Highlight, Text, Highlight_Category,
               Quiet              => Quiet,
               Remove_Duplicates  => Remove_Duplicates,
               Enable_Counter     => Enable_Counter,
               Sort_In_File       => Sort_In_File,
               Parent_Iter        => Iter,
               Look_For_Secondary => Look_For_Secondary);

         else
            GPS.Location_View.Add_Location
              (View,
               Glib.Convert.Escape_Text (Category),
               File, Line, Column, Length,
               Highlight, Glib.Convert.Escape_Text (Text), Highlight_Category,
               Quiet              => Quiet,
               Remove_Duplicates  => Remove_Duplicates,
               Enable_Counter     => Enable_Counter,
               Sort_In_File       => Sort_In_File,
               Parent_Iter        => Iter,
               Look_For_Secondary => Look_For_Secondary);
         end if;

         Gtkada.MDI.Highlight_Child
           (Gtkada.MDI.Find_MDI_Child (GPS.Kernel.MDI.Get_MDI (Kernel), View));
      end if;
   end Insert_Location;

   -------------------
   -- Location_Hook --
   -------------------

   function Location_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class) return Boolean
   is
      View : constant GPS.Location_View.Location_View :=
               GPS.Location_View.Get_Or_Create_Location_View (Kernel, False);
      D    : constant GPS.Kernel.Standard_Hooks.Location_Hooks_Args :=
               GPS.Kernel.Standard_Hooks.Location_Hooks_Args (Data.all);

   begin
      GPS.Location_View.Add_Action_Item
        (View, D.Identifier, D.Category, D.File,
         Integer (D.Line), Integer (D.Column), D.Message, D.Action);

      return True;
   end Location_Hook;

   ---------------
   -- Next_Item --
   ---------------

   procedure Next_Item
     (Kernel    : access Kernel_Handle_Record'Class;
      Backwards : Boolean := False)
   is
      View : constant GPS.Location_View.Location_View :=
               GPS.Location_View.Get_Or_Create_Location_View (Kernel, False);

   begin
      if View /= null then
         GPS.Location_View.Next_Item (View, Backwards);
      end if;
   end Next_Item;

   --------------------------
   -- Parse_File_Locations --
   --------------------------

   procedure Parse_File_Locations
     (Kernel                  : access Kernel_Handle_Record'Class;
      Text                    : String;
      Category                : String;
      Highlight               : Boolean := False;
      Highlight_Category      : GPS.Kernel.Styles.Style_Access := null;
      Style_Category          : GPS.Kernel.Styles.Style_Access := null;
      Warning_Category        : GPS.Kernel.Styles.Style_Access := null;
      File_Location_Regexp    : String := "";
      File_Index_In_Regexp    : Integer := -1;
      Line_Index_In_Regexp    : Integer := -1;
      Col_Index_In_Regexp     : Integer := -1;
      Msg_Index_In_Regexp     : Integer := -1;
      Style_Index_In_Regexp   : Integer := -1;
      Warning_Index_In_Regexp : Integer := -1;
      Quiet                   : Boolean := False;
      Remove_Duplicates       : Boolean := False)
   is
      use type GNAT.Regpat.Match_Location;
      use type Basic_Types.Visible_Column_Type;

      function Get_File_Location return GNAT.Regpat.Pattern_Matcher;
      --  Return the pattern matcher for the file location

      function Get_Index
        (Pref  : access Default_Preferences.Integer_Preference_Record'Class;
         Value : Integer) return Integer;
      --  If Value is -1, return Pref, otherwise return Value

      function Get_Message (Last : Natural) return Glib.UTF8_String;
      --  Return the error message. For backward compatibility with existing
      --  preferences file, we check that the message Index is still good.
      --  Otherwise, we return the last part of the regexp

      -----------------------
      -- Get_File_Location --
      -----------------------

      function Get_File_Location return GNAT.Regpat.Pattern_Matcher is
      begin
         if File_Location_Regexp = "" then
            return
              GNAT.Regpat.Compile
                (GPS.Kernel.Preferences.File_Pattern.Get_Pref);

         else
            return GNAT.Regpat.Compile (File_Location_Regexp);
         end if;
      end Get_File_Location;

      Max : Integer := 0;
      --  Maximal value for the indexes

      ---------------
      -- Get_Index --
      ---------------

      function Get_Index
        (Pref  : access Default_Preferences.Integer_Preference_Record'Class;
         Value : Integer) return Integer
      is
         Location : Integer;
      begin
         if Value = -1 then
            Location := Pref.Get_Pref;
         else
            Location := Value;
         end if;

         Max := Integer'Max (Max, Location);
         return Location;
      end Get_Index;

      File_Location : constant GNAT.Regpat.Pattern_Matcher :=
                        Get_File_Location;
      File_Index    : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.File_Pattern_Index,
                           File_Index_In_Regexp);
      Line_Index    : constant Integer :=
                        Get_Index
                         (GPS.Kernel.Preferences.Line_Pattern_Index,
                          Line_Index_In_Regexp);
      Col_Index     : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.Column_Pattern_Index,
                           Col_Index_In_Regexp);
      Msg_Index     : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.Message_Pattern_Index,
                           Msg_Index_In_Regexp);
      Style_Index   : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.Style_Pattern_Index,
                           Style_Index_In_Regexp);
      Warning_Index : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.Warning_Pattern_Index,
                           Warning_Index_In_Regexp);
      Matched       : GNAT.Regpat.Match_Array (0 .. Max);
      Start         : Natural := Text'First;
      Last          : Natural;
      Real_Last     : Natural;
      Line          : Natural := 1;
      Column        : Basic_Types.Visible_Column_Type := 1;
      C             : GPS.Kernel.Styles.Style_Access;
      View          : GPS.Location_View.Location_View := null;
      Expand        : Boolean := Quiet;

      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;

      -----------------
      -- Get_Message --
      -----------------

      function Get_Message (Last : Natural) return Glib.UTF8_String is
      begin
         if Matched (Msg_Index) /= GNAT.Regpat.No_Match then
            return Text
              (Matched (Msg_Index).First .. Matched (Msg_Index).Last);
         else
            return Text (Last + 1 .. Real_Last);
         end if;
      end Get_Message;

   begin
      while Start <= Text'Last loop
         --  Parse Text line by line and look for file locations

         while Start < Text'Last
           and then (Text (Start) = ASCII.CR
                     or else Text (Start) = ASCII.LF)
         loop
            Start := Start + 1;
         end loop;

         Real_Last := Start;

         while Real_Last < Text'Last
           and then Text (Real_Last + 1) /= ASCII.CR
           and then Text (Real_Last + 1) /= ASCII.LF
         loop
            Real_Last := Real_Last + 1;
         end loop;

         GNAT.Regpat.Match (File_Location, Text (Start .. Real_Last), Matched);

         if Matched (0) /= GNAT.Regpat.No_Match then
            if Matched (Line_Index) /= GNAT.Regpat.No_Match then
               Line := Integer'Value
                 (Text
                    (Matched (Line_Index).First .. Matched (Line_Index).Last));

               if Line <= 0 then
                  Line := 1;
               end if;
            end if;

            if Matched (Col_Index) = GNAT.Regpat.No_Match then
               Last := Matched (Line_Index).Last;

            else
               Last := Matched (Col_Index).Last;
               Column := Basic_Types.Visible_Column_Type'Value
                 (Text (Matched (Col_Index).First ..
                            Matched (Col_Index).Last));

               if Column <= 0 then
                  Column := 1;
               end if;
            end if;

            if Matched (Warning_Index) /= GNAT.Regpat.No_Match then
               C := Warning_Category;
            elsif  Matched (Style_Index) /= GNAT.Regpat.No_Match then
               C := Style_Category;
            else
               C := Highlight_Category;
            end if;

            if View = null then
               View := GPS.Location_View.Get_Or_Create_Location_View (Kernel);
            end if;

            GPS.Location_View.Add_Location
              (View               => View,
               Category           => Glib.Convert.Escape_Text (Category),
               File               => Create
                 (+Text (Matched
                          (File_Index).First .. Matched (File_Index).Last),
                  Kernel),
               Line               => Positive (Line),
               Column             => Column,
               Length             => 0,
               Highlight          => Highlight,
               Message            => Glib.Convert.Escape_Text
                 (Get_Message (Last)),
               Highlight_Category => C,
               Quiet              => Expand,
               Remove_Duplicates  => Remove_Duplicates,
               Enable_Counter     => False,
               Sort_In_File       => False,
               Parent_Iter        => Iter,
               Look_For_Secondary => True);
            Expand := False;
         end if;

         Start := Real_Last + 1;
      end loop;

      Recount_Category (Kernel, Category);
   end Parse_File_Locations;

   ----------------------
   -- Recount_Category --
   ----------------------

   procedure Recount_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String)
   is
      View  : constant GPS.Location_View.Location_View :=
                Get_Or_Create_Location_View (Kernel, Allow_Creation => False);

   begin
      if View = null then
         return;
      end if;

      Recount_Category (View.Model, Category);
      View.Redraw_Totals;
   end Recount_Category;

   --------------
   -- Register --
   --------------

   procedure Register
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File_Hook : File_Edited_Hook;

   begin
      GPS.Kernel.Hooks.Add_Hook
        (Kernel,
         GPS.Kernel.Standard_Hooks.Location_Action_Hook,
         GPS.Kernel.Hooks.Wrapper (Location_Hook'Access),
         Name => "location_view.location");

      File_Hook := new File_Edited_Hook_Record;
      Add_Hook
        (Kernel,
         GPS.Kernel.File_Edited_Hook,
         File_Hook,
         Name => "location_view.file_edited");
   end Register;

   ------------------------------
   -- Remove_Location_Category --
   ------------------------------

   procedure Remove_Location_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Line     : Natural := 0)
   is
      View : constant GPS.Location_View.Location_View :=
               GPS.Location_View.Get_Or_Create_Location_View
                 (Kernel, Allow_Creation => False);

   begin
      if View /= null then
         GPS.Location_Model.Remove_Category
           (Kernel,
            View.Model,
            Glib.Convert.Escape_Text (Category),
            File,
            Line);
         View.Redraw_Totals;
      end if;
   end Remove_Location_Category;

end GPS.Kernel.Locations;
