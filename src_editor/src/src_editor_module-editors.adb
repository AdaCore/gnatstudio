-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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

with Ada.Unchecked_Deallocation;

with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Mark; use Gtk.Text_Mark;

with GPS.Intl; use GPS.Intl;

with Src_Editor_Module.Line_Highlighting;
use Src_Editor_Module.Line_Highlighting;

with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;

with Src_Editor_Box; use Src_Editor_Box;

package body Src_Editor_Module.Editors is

   Editor_Exception : exception;

   type Src_Editor_Buffer is new GPS.Editors.Editor_Buffer with record
      Kernel : Kernel_Handle;
      Buffer : Source_Buffer;
   end record;

   type Src_Editor_Location is new GPS.Editors.Editor_Location with record
      Buffer : Src_Editor_Buffer;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
   end record;

   type Mark_Reference is record
      Mark : Gtk_Text_Mark;
      Refs : Integer := 1;
   end record;

   type Mark_Reference_Access is access all Mark_Reference;

   procedure Free is new Ada.Unchecked_Deallocation
     (Mark_Reference, Mark_Reference_Access);

   type Src_Editor_Mark is new GPS.Editors.Editor_Mark with record
      Buffer : Src_Editor_Buffer;
      Mark   : Mark_Reference_Access;
   end record;

   function Create_Editor_Location
     (Buffer   : Src_Editor_Buffer'Class;
      Location : Gtk_Text_Iter) return Src_Editor_Location'Class;
   --  Return an instance of Editor_Location

   procedure Get_Location
     (Iter     : out Gtk_Text_Iter;
      Location : Src_Editor_Location'Class;
      Default  : Gtk_Text_Iter;
      Success  : out Boolean);
   --  Return the iter stored in Location.
   --  If no location could be obtain from the arguments, Iter is Set to
   --  Default and Success to False.

   procedure Get_Locations
     (Iter1                : out Gtk_Text_Iter;
      Iter2                : out Gtk_Text_Iter;
      Buffer               : Source_Buffer;
      Loc1                 : Src_Editor_Location'Class;
      Loc2                 : Src_Editor_Location'Class;
      Compensate_Last_Iter : Boolean := True);
   --  Set the two iteratiors from the given locations.
   --  Buffer is reset to null in case of errors, or if it was null when
   --  Get_Locations was called.
   --  If Compensate_Last_Iter is True, then the highest iterator is moved one
   --  additional character. This is used for gtk+ commands that take two
   --  iterators, since they stop to operate just before the last iterator.

   function Create_Editor_Mark
     (Buffer : Src_Editor_Buffer'Class;
      Mark   : Gtk_Text_Mark) return Src_Editor_Mark'Class;
   --  Return an instance of Editor_Mark encapsulating Mark

   ---------------------
   -- Editor_Location --
   ---------------------

   overriding function Beginning_Of_Line
     (This : Src_Editor_Location) return Editor_Location'Class;

   overriding function End_Of_Line
     (This : Src_Editor_Location) return Editor_Location'Class;

   overriding function Forward_Char
     (This  : Src_Editor_Location;
      Count : Integer) return Editor_Location'Class;

   -----------------
   -- Editor_Mark --
   -----------------

   overriding procedure Adjust (This : in out Src_Editor_Mark);

   overriding procedure Finalize (This : in out Src_Editor_Mark);

   -------------------
   -- Editor_Buffer --
   -------------------

   overriding function New_Location
     (This   : Src_Editor_Buffer;
      Line   : Integer;
      Column : Integer) return Editor_Location'Class;

   overriding function Add_Special_Line
     (This       : Src_Editor_Buffer;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "") return Editor_Mark'Class;

   overriding procedure Remove_Special_Lines
     (This  : Src_Editor_Buffer;
      Mark  : Editor_Mark'Class;
      Lines : Integer);

   overriding function Get_Chars
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) return String;

   overriding procedure Insert
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class;
      Text : String);

   overriding procedure Delete
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location);

   overriding procedure Indent
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location);

   overriding function Beginning_Of_Buffer
     (This : Src_Editor_Buffer) return Editor_Location'Class;

   overriding function End_Of_Buffer
     (This : Src_Editor_Buffer) return Editor_Location'Class;

   overriding function Get_Mark
     (This : Src_Editor_Buffer;
      Name : String) return Editor_Mark'Class;

   ----------------------------
   -- Create_Editor_Location --
   ----------------------------

   function Create_Editor_Location
     (Buffer   : Src_Editor_Buffer'Class;
      Location : Gtk_Text_Iter) return Src_Editor_Location'Class
   is
      Editor_Loc : Src_Editor_Location;
   begin
      Get_Iter_Position
        (Buffer.Buffer, Location, Editor_Loc.Line, Editor_Loc.Column);

      Editor_Loc.Buffer := Src_Editor_Buffer (Buffer);

      return Editor_Loc;
   end Create_Editor_Location;

   ------------------
   -- Get_Location --
   ------------------

   procedure Get_Location
     (Iter     : out Gtk_Text_Iter;
      Location : Src_Editor_Location'Class;
      Default  : Gtk_Text_Iter;
      Success  : out Boolean)
   is
   begin
      Success := True;

      if Location.Buffer.Buffer = null
        or else not Is_Valid_Position
          (Location.Buffer.Buffer,
           Location.Line,
           Location.Column)
      then
         Copy (Source => Default, Dest => Iter);
         Success := False;

      else
         Get_Iter_At_Screen_Position
           (Location.Buffer.Buffer, Iter,
            Line   => Location.Line,
            Column => Location.Column);
      end if;
   end Get_Location;

   -------------------
   -- Get_Locations --
   -------------------

   procedure Get_Locations
     (Iter1                : out Gtk_Text_Iter;
      Iter2                : out Gtk_Text_Iter;
      Buffer               : Source_Buffer;
      Loc1                 : Src_Editor_Location'Class;
      Loc2                 : Src_Editor_Location'Class;
      Compensate_Last_Iter : Boolean := True)
   is
      Success : Boolean;

      procedure Forward_Iter (Iter : in out Gtk_Text_Iter);
      --  Forward Iter one char

      ------------------
      -- Forward_Iter --
      ------------------

      procedure Forward_Iter (Iter : in out Gtk_Text_Iter) is
         Line, End_Line : Editable_Line_Type;
         Col, End_Col   : Character_Offset_Type;
      begin
         Get_Iter_Position (Buffer, Iter, Line, Col);
         Forward_Position (Buffer, Line, Col, 1, End_Line, End_Col);
         Get_Iter_At_Screen_Position (Buffer, Iter, End_Line, End_Col);
      end Forward_Iter;

   begin
      if Buffer /= null then
         Get_Start_Iter (Buffer, Iter1);
         Get_Location (Iter1, Loc1, Iter1, Success);

         Get_End_Iter (Buffer, Iter2);
         Get_Location (Iter2, Loc2, Iter2, Success);

         if Get_Buffer (Iter1) /= Get_Buffer (Iter2)
           or else Get_Buffer (Iter1) /= Gtk_Text_Buffer (Buffer)
         then
            raise Editor_Exception
              with -"Locations are not in the correct buffer";
         elsif Compensate_Last_Iter then
            --  All operations that take two iterators stop just before the
            --  second one. This is harder to use in scripts, though, so we
            --  compensate for that here.
            if Compare (Iter1, Iter2) <= 0 then
               --  ??? temporarily commented out, since it breaks ctrl-k in
               --  Emacs mode (F707-004)
               --  if not (Is_End (Iter2) or else Ends_Line (Iter2)) then
               if not Is_End (Iter2) then
                  Forward_Iter (Iter2);
               end if;

            else
               --  ??? temporarily commented out, since it breaks ctrl-k in
               --  Emacs mode (F707-004)
               --  if not (Is_End (Iter1) or else Ends_Line (Iter1)) then
               if not Is_End (Iter1) then
                  Forward_Iter (Iter1);
               end if;
            end if;
         end if;
      end if;
   end Get_Locations;

   ------------------------
   -- Create_Editor_Mark --
   ------------------------

   function Create_Editor_Mark
     (Buffer : Src_Editor_Buffer'Class;
      Mark   : Gtk_Text_Mark) return Src_Editor_Mark'Class
   is
   begin
      return Src_Editor_Mark'
        (Editor_Mark with
         Buffer => Src_Editor_Buffer (Buffer),
         Mark   => new Mark_Reference'(Mark => Mark, Refs => 1));
   end Create_Editor_Mark;

   ---------
   -- Get --
   ---------

   overriding function Get
     (This  : Src_Editor_Buffer_Factory;
      File  : Virtual_File := No_File;
      Force : Boolean := False;
      Open  : Boolean := True) return Editor_Buffer'Class
   is
      Child : MDI_Child;
      Box   : Source_Editor_Box;
   begin
      if File /= GNATCOLL.VFS.No_File then
         Child := Find_Editor (This.Kernel, File);
      else
         Child := Find_Current_Editor (This.Kernel);
      end if;

      if Child = null then
         if Open then
            Box := Open_File
              (This.Kernel, File, Line => 0, Column => 0, Column_End => 0);
         else
            return Nil_Editor_Buffer;
         end if;
      else
         Box := Get_Source_Box_From_MDI (Child);

         if File /= GNATCOLL.VFS.No_File and Force then
            Check_Timestamp_And_Reload (Box, False, True);
         end if;
      end if;

      return Src_Editor_Buffer'
        (Editor_Buffer with Kernel => This.Kernel, Buffer => Get_Buffer (Box));
   end Get;

   -----------------------
   -- Beginning_Of_Line --
   -----------------------

   overriding function Beginning_Of_Line
     (This : Src_Editor_Location) return Editor_Location'Class
   is
      Success : Boolean;
      Iter    : Gtk_Text_Iter;
   begin
      Get_Location (Iter, This, Iter, Success);

      if Success then
         Set_Line_Offset (Iter, 0);

         return Create_Editor_Location (This.Buffer, Iter);
      else
         raise Editor_Exception with -"Invalid location";
      end if;
   end Beginning_Of_Line;

   -----------------
   -- End_Of_Line --
   -----------------

   overriding function End_Of_Line
     (This : Src_Editor_Location) return Editor_Location'Class
   is
      Success : Boolean;
      Iter    : Gtk_Text_Iter;
   begin
      Get_Location (Iter, This, Iter, Success);

      if Success then
         if not Ends_Line (Iter) then
            Forward_To_Line_End (Iter, Success);
         end if;

         return Create_Editor_Location (This.Buffer, Iter);
      else
         raise Editor_Exception with -"Invalid location";
      end if;
   end End_Of_Line;

   ------------------
   -- Forward_Char --
   ------------------

   overriding function Forward_Char
     (This  : Src_Editor_Location;
      Count : Integer) return Editor_Location'Class
   is
      Success : Boolean;
      Iter    : Gtk_Text_Iter;
   begin
      Get_Location (Iter, This, Iter, Success);

      if Success then
         declare
            Chars : constant Gint := Gint (Count);
         begin
            if Chars >= 0 then
               Forward_Chars (Iter, Chars, Success);
            else
               Backward_Chars (Iter, -Chars, Success);
            end if;
         end;

         return Create_Editor_Location (This.Buffer, Iter);
      else
         raise Editor_Exception with -"Invalid location";
      end if;
   end Forward_Char;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (This : in out Src_Editor_Mark) is
   begin
      This.Mark.Refs := This.Mark.Refs + 1;
   end Adjust;

   overriding procedure Finalize (This : in out Src_Editor_Mark) is
   begin
      This.Mark.Refs := This.Mark.Refs - 1;

      if This.Mark.Refs = 0 then
         Delete_Mark (This.Buffer.Buffer, This.Mark.Mark);
         Free (This.Mark);
      end if;
   end Finalize;

   ------------------
   -- New_Location --
   ------------------

   overriding function New_Location
     (This   : Src_Editor_Buffer;
      Line   : Integer;
      Column : Integer) return Editor_Location'Class
   is
      Result : Src_Editor_Location;
   begin
      Result.Buffer := This;
      Result.Line   := Editable_Line_Type'Max (1, Editable_Line_Type (Line));
      Result.Column := Visible_Column_Type'Max
        (1, Visible_Column_Type (Column));

      return Result;
   end New_Location;

   ----------------------
   -- Add_Special_Line --
   ----------------------

   overriding function Add_Special_Line
     (This       : Src_Editor_Buffer;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "") return Editor_Mark'Class
   is
      Mark : Gtk_Text_Mark;
   begin
      if This.Buffer /= null then
         declare
            Highlight_Category : Natural := 0;
            Style              : Style_Access;
         begin
            if Category /= "" then
               Style := Get_Or_Create_Style (This.Kernel, Category, False);

               if Style = null then
                  raise Editor_Exception
                    with -"No such style: " & Category;
               else
                  Highlight_Category := Lookup_Category (Style);
               end if;
            end if;

            Mark := Add_Blank_Lines
              (This.Buffer,
               Editable_Line_Type (Start_Line),
               Highlight_Category,
               Text,
               1,
               Name);

            return Create_Editor_Mark (This, Mark);
         end;
      else
         return Nil_Editor_Mark;
      end if;
   end Add_Special_Line;

   --------------------------
   -- Remove_Special_Lines --
   --------------------------

   overriding procedure Remove_Special_Lines
     (This  : Src_Editor_Buffer;
      Mark  : Editor_Mark'Class;
      Lines : Integer)
   is
   begin
      if This.Buffer /= null and then Mark /= Nil_Editor_Mark then
         declare
            Src_Mark : Src_Editor_Mark renames Src_Editor_Mark (Mark);
         begin
            if Src_Mark.Mark /= null then
               Remove_Blank_Lines (This.Buffer, Src_Mark.Mark.Mark, Lines);
            end if;
         end;
      end if;
   end Remove_Special_Lines;

   ---------------
   -- Get_Chars --
   ---------------

   overriding function Get_Chars
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) return String
   is
      Src_From    : Src_Editor_Location;
      Src_To      : Src_Editor_Location;
      Iter, Iter2 : Gtk_Text_Iter;
   begin
      if This.Buffer /= null then
         if From = Nil_Editor_Location then
            Src_From := Src_Editor_Location (Beginning_Of_Buffer (This));
         else
            Src_From := Src_Editor_Location (From);
         end if;

         if To = Nil_Editor_Location then
            Src_To := Src_Editor_Location (End_Of_Buffer (This));
         else
            Src_To := Src_Editor_Location (To);
         end if;

         Get_Locations (Iter, Iter2, This.Buffer, Src_From, Src_To);

         declare
            Begin_Line : Editable_Line_Type;
            Begin_Col  : Character_Offset_Type;
            End_Line   : Editable_Line_Type;
            End_Col    : Character_Offset_Type;
         begin
            Get_Iter_Position (This.Buffer, Iter, Begin_Line, Begin_Col);
            Get_Iter_Position (This.Buffer, Iter2, End_Line, End_Col);

            return Get_Text
              (This.Buffer, Begin_Line, Begin_Col, End_Line, End_Col);
         end;
      else
         return "";
      end if;
   end Get_Chars;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class;
      Text : String)
   is
      Src_From : Src_Editor_Location renames Src_Editor_Location (From);
      Iter     : Gtk_Text_Iter;
      Success  : Boolean;
   begin
      Get_Location (Iter, Src_From, Iter, Success);

      if Success then
         if Get_Buffer (Iter) = Gtk_Text_Buffer (This.Buffer) then
            if Get_Writable (This.Buffer) then
               Insert (This.Buffer, Iter, Text);
               End_Action (This.Buffer);
            else
               raise Editor_Exception with -"Buffer is not writable";
            end if;
         else
            raise Editor_Exception with -"Location is not in the same buffer";
         end if;
      end if;
   end Insert;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location)
   is
      Iter, Iter2 : Gtk_Text_Iter;
      Src_From    : Src_Editor_Location;
      Src_To      : Src_Editor_Location;
   begin
      if This.Buffer /= null then
         if From = Nil_Editor_Location then
            Src_From := Src_Editor_Location (Beginning_Of_Buffer (This));
         else
            Src_From := Src_Editor_Location (From);
         end if;

         if To = Nil_Editor_Location then
            Src_To := Src_Editor_Location (End_Of_Buffer (This));
         else
            Src_To := Src_Editor_Location (To);
         end if;

         Get_Locations (Iter, Iter2, This.Buffer, Src_From, Src_To);

         if Get_Writable (This.Buffer) then
            Delete (This.Buffer, Iter, Iter2);
            End_Action (This.Buffer);
         else
            raise Editor_Exception with -"Buffer is not writable";
         end if;
      end if;
   end Delete;

   ------------
   -- Indent --
   ------------

   overriding procedure Indent
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location)
   is
      Iter, Iter2 : Gtk_Text_Iter;
      Src_From    : Src_Editor_Location;
      Src_To      : Src_Editor_Location;
   begin
      if This.Buffer /= null then
         if From = Nil_Editor_Location then
            Src_From := Src_Editor_Location (Beginning_Of_Buffer (This));
         else
            Src_From := Src_Editor_Location (From);
         end if;

         if To = Nil_Editor_Location then
            Src_To := Src_Editor_Location (End_Of_Buffer (This));
         else
            Src_To := Src_Editor_Location (To);
         end if;

         Get_Locations (Iter, Iter2, This.Buffer, Src_From, Src_To);

         if not Do_Indentation (This.Buffer, Iter, Iter2) then
            raise Editor_Exception with -"Error while indenting";
         end if;
      end if;

      End_Action (This.Buffer);
   end Indent;

   -------------------------
   -- Beginning_Of_Buffer --
   -------------------------

   overriding function Beginning_Of_Buffer
     (This : Src_Editor_Buffer) return Editor_Location'Class
   is
      Iter   : Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
   begin
      if This.Buffer /= null then
         Get_Start_Iter (This.Buffer, Iter);
         Get_Iter_Position
           (This.Buffer, Iter, Line, Column);

         return Src_Editor_Location'
           (Editor_Location with
            Buffer => This,
            Line   => Line,
            Column => Column);
      else
         return Nil_Editor_Location;
      end if;
   end Beginning_Of_Buffer;

   -------------------
   -- End_Of_Buffer --
   -------------------

   overriding function End_Of_Buffer
     (This : Src_Editor_Buffer) return Editor_Location'Class
   is
      Iter   : Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
   begin
      if This.Buffer /= null then
         Get_End_Iter (This.Buffer, Iter);
         Get_Iter_Position
           (This.Buffer, Iter, Line, Column);

         return Src_Editor_Location'
           (Editor_Location with
            Buffer => This,
            Line   => Line,
            Column => Column);
      else
         return Nil_Editor_Location;
      end if;
   end End_Of_Buffer;

   --------------
   -- Get_Mark --
   --------------

   overriding function Get_Mark
     (This : Src_Editor_Buffer;
      Name : String) return Editor_Mark'Class
   is
      Mark : Gtk_Text_Mark;
   begin
      if This.Buffer /= null then
         Mark := Get_Mark (This.Buffer, Name);

         if Mark /= null then
            return Create_Editor_Mark (This, Mark);
         else
            raise Editor_Exception with "No such mark";
         end if;
      else
         return Nil_Editor_Mark;
      end if;
   end Get_Mark;

end Src_Editor_Module.Editors;
