-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008-2009, AdaCore               --
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
with GPS.Kernel.MDI; use GPS.Kernel.MDI;

with Src_Editor_Module.Line_Highlighting;
use Src_Editor_Module.Line_Highlighting;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Box;  use Src_Editor_Box;
with Src_Editor_View; use Src_Editor_View;
with Src_Editor_Module.Markers; use Src_Editor_Module.Markers;

with Commands; use Commands;
with Language; use Language;
with Traces;   use Traces;

package body Src_Editor_Module.Editors is
   Me : constant Debug_Handle := Create ("Editor.Buffer");

   Editor_Exception : exception;

   type Src_Editor_Buffer is new GPS.Editors.Editor_Buffer with record
      Kernel : Kernel_Handle;
      Buffer : Source_Buffer;
      File   : Virtual_File;
   end record;

   type Src_Editor_Location is new GPS.Editors.Editor_Location with record
      Buffer : Src_Editor_Buffer;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
   end record;

   type Mark_Reference is record
      Mark : File_Marker;
      Refs : Integer := 1;
   end record;

   type Mark_Reference_Access is access all Mark_Reference;

   procedure Free is new Ada.Unchecked_Deallocation
     (Mark_Reference, Mark_Reference_Access);

   type Src_Editor_Mark is new GPS.Editors.Editor_Mark with record
      Kernel : Kernel_Handle;
      Mark   : Mark_Reference_Access;
   end record;

   type Src_Editor_View is new GPS.Editors.Editor_View with record
      Buffer : Src_Editor_Buffer;
      Box    : Source_Editor_Box;
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
   --  Returns an instance of Editor_Mark encapsulating Mark. Mark must not be
   --  null.

   -------------------------
   -- Src_Editor_Location --
   -------------------------

   overriding function Beginning_Of_Line
     (This : Src_Editor_Location) return Editor_Location'Class;

   overriding function End_Of_Line
     (This : Src_Editor_Location) return Editor_Location'Class;

   overriding function Block_Start
     (This : Src_Editor_Location) return Editor_Location'Class;

   overriding function Block_End
     (This : Src_Editor_Location) return Editor_Location'Class;

   overriding function Block_Type
     (This : Src_Editor_Location) return Language_Category;

   overriding function Line (This : Src_Editor_Location) return Integer;

   overriding function Column (This : Src_Editor_Location) return Integer;

   overriding function Buffer
     (This : Src_Editor_Location) return Editor_Buffer'Class;

   overriding function Create_Mark
     (This : Src_Editor_Location; Name : String := "")
      return Editor_Mark'Class;

   overriding function Forward_Char
     (This  : Src_Editor_Location;
      Count : Integer) return Editor_Location'Class;

   ---------------------
   -- Src_Editor_Mark --
   ---------------------

   overriding function Line (This : Src_Editor_Mark) return Integer;

   overriding function Column (This : Src_Editor_Mark) return Integer;

   overriding function Is_Present (This : Src_Editor_Mark) return Boolean;

   overriding procedure Delete (This : Src_Editor_Mark);

   overriding procedure Adjust (This : in out Src_Editor_Mark);

   overriding procedure Finalize (This : in out Src_Editor_Mark);

   overriding function Location
     (This : Src_Editor_Mark) return Editor_Location'Class;

   -----------------------
   -- Src_Editor_Buffer --
   -----------------------

   overriding function New_Location
     (This   : Src_Editor_Buffer;
      Line   : Integer;
      Column : Integer) return Editor_Location'Class;

   overriding function New_View
     (This : Src_Editor_Buffer) return Editor_View'Class;

   overriding function Open
     (This : Src_Editor_Buffer) return Editor_View'Class;

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

   overriding function Current_View
     (This : Src_Editor_Buffer) return Editor_View'Class;

   overriding function Lines_Count (This : Src_Editor_Buffer) return Integer;

   overriding procedure Select_Text
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location);

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

   overriding procedure Save
     (This        : Src_Editor_Buffer;
      Interactive : Boolean := True;
      File        : Virtual_File := No_File);

   overriding function Get_Mark
     (This : Src_Editor_Buffer;
      Name : String) return Editor_Mark'Class;

   overriding procedure Start_Undo_Group (This : Src_Editor_Buffer);

   overriding procedure Finish_Undo_Group (This : Src_Editor_Buffer);

   overriding procedure Undo (This : Src_Editor_Buffer);

   overriding procedure Set_Read_Only
     (This : Src_Editor_Buffer; Read_Only : Boolean);

   overriding procedure Apply_Style
     (This  : Src_Editor_Buffer;
      Style : not null access Simple_Style_Record'Class;
      Line  : Integer;
      From_Column, To_Column : Integer := -1);

   overriding procedure Remove_Style
     (This  : Src_Editor_Buffer;
      Style : not null access Simple_Style_Record'Class;
      Line  : Integer;
      From_Column, To_Column : Integer := -1);

   overriding procedure Get_Constructs
     (This       : Src_Editor_Buffer;
      Constructs : out Language.Construct_List;
      Timestamp  : out Natural);

   ---------------------
   -- Src_Editor_View --
   ---------------------

   overriding procedure Set_Read_Only
     (This : Src_Editor_View; Read_Only : Boolean);

   overriding procedure Center
     (This     : Src_Editor_View;
      Location : Editor_Location'Class := Nil_Editor_Location);

   overriding procedure Cursor_Goto
     (This       : Src_Editor_View;
      Location   : Editor_Location'Class;
      Raise_View : Boolean := False);

   overriding function Cursor
     (This : Src_Editor_View) return Editor_Location'Class;

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
      New_Ref : constant Mark_Reference_Access :=
        new Mark_Reference'
          (Mark => Create_File_Marker (Buffer.Kernel, Buffer.File, Mark),
           Refs => 1);
   begin
      pragma Assert (Mark /= null);

      return Src_Editor_Mark'
        (Editor_Mark with Kernel => Buffer.Kernel, Mark => New_Ref);
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
         end if;
      else
         Box := Get_Source_Box_From_MDI (Child);

         if File /= GNATCOLL.VFS.No_File and Force then
            Check_Timestamp_And_Reload (Box, False, True);
         end if;
      end if;

      if Box = null then
         return Nil_Editor_Buffer;
      else
         return Src_Editor_Buffer'
           (Editor_Buffer with
            File   => File,
            Kernel => This.Kernel,
            Buffer => Get_Buffer (Box));
      end if;
   end Get;

   --------------
   -- New_Mark --
   --------------

   overriding function New_Mark
     (This   : Src_Editor_Buffer_Factory;
      File   : Virtual_File := No_File;
      Line   : Integer;
      Column : Integer) return Editor_Mark'Class
   is
      New_Ref : constant Mark_Reference_Access :=
        new Mark_Reference'
          (Mark => Create_File_Marker
               (This.Kernel,
                File,
                Editable_Line_Type (Line),
                Visible_Column_Type (Column)),
           Refs => 1);
   begin
      return Src_Editor_Mark'
        (Editor_Mark with Kernel => This.Kernel, Mark => New_Ref);
   end New_Mark;

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

   -----------------
   -- Block_Start --
   -----------------

   overriding function Block_Start
     (This : Src_Editor_Location) return Editor_Location'Class
   is
      Success : Boolean;
      Line    : Buffer_Line_Type;
      Block   : Block_Record;
      Iter    : Gtk_Text_Iter;
   begin
      Get_Location (Iter, This, Iter, Success);

      if Success then
         Line := Buffer_Line_Type (Get_Line (Iter) + 1);
         Block := Get_Block (Source_Buffer (Get_Buffer (Iter)), Line);

         return Src_Editor_Location'
           (Editor_Location with
            Buffer => This.Buffer,
            Line   => Block.First_Line,
            Column => 1);
      else
         return Nil_Editor_Location;
      end if;
   end Block_Start;

   ---------------
   -- Block_End --
   ---------------

   overriding function Block_End
     (This : Src_Editor_Location) return Editor_Location'Class
   is
      Success     : Boolean;
      Line        : Buffer_Line_Type;
      Block       : Block_Record;
      Iter, Iter2 : Gtk_Text_Iter;
   begin
      Get_Location (Iter, This, Iter, Success);

      if Success then
         Line := Buffer_Line_Type (Get_Line (Iter) + 1);
         Block := Get_Block (Source_Buffer (Get_Buffer (Iter)), Line);

         Get_Iter_At_Line_Offset
           (Source_Buffer (Get_Buffer (Iter)), Iter2,
            Line_Number => Gint (Block.Last_Line),
            Char_Offset => 1);
         Forward_Lines
           (Iter2,
            Count  => -1,
            Result => Success);

         if not Success then
            return Nil_Editor_Location;
         end if;

         Forward_To_Line_End (Iter2, Success);

         if not Success then
            return Nil_Editor_Location;
         end if;

         return Create_Editor_Location (This.Buffer, Iter2);
      else
         return Nil_Editor_Location;
      end if;
   end Block_End;

   ---------------
   -- Bloc_Type --
   ---------------

   overriding function Block_Type
     (This : Src_Editor_Location) return Language_Category
   is
      Success : Boolean;
      Line    : Buffer_Line_Type;
      Block   : Block_Record;
      Iter    : Gtk_Text_Iter;
   begin
      Get_Location (Iter, This, Iter, Success);

      if Success then
         Line := Buffer_Line_Type (Get_Line (Iter) + 1);
         Block := Get_Block (Source_Buffer (Get_Buffer (Iter)), Line);

         return Block.Block_Type;
      else
         return Cat_Unknown;
      end if;
   end Block_Type;

   ----------
   -- Line --
   ----------

   overriding function Line (This : Src_Editor_Location) return Integer is
   begin
      return Integer (This.Line);
   end Line;

   ------------
   -- Column --
   ------------

   overriding function Column (This : Src_Editor_Location) return Integer is
   begin
      return Integer (This.Column);
   end Column;

   ------------
   -- Buffer --
   ------------

   overriding function Buffer
     (This : Src_Editor_Location) return Editor_Buffer'Class is
   begin
      return This.Buffer;
   end Buffer;

   -----------------
   -- Create_Mark --
   -----------------

   overriding function Create_Mark
     (This : Src_Editor_Location; Name : String := "")
      return Editor_Mark'Class
   is
      Success : Boolean;
      Iter    : Gtk_Text_Iter;
      Mark    : Gtk_Text_Mark;
   begin
      Get_Location (Iter, This, Iter, Success);

      if Success then
         if Name /= "" then
            Mark := Get_Mark (Get_Buffer (Iter), Name);
         end if;

         if Mark = null then
            Mark := Create_Mark
              (Get_Buffer (Iter),
               Mark_Name => Name,
               Where     => Iter);
         else
            Move_Mark (Get_Buffer (Iter), Mark, Where => Iter);
         end if;

         return Create_Editor_Mark (This.Buffer, Mark);
      else
         raise Editor_Exception with -"Invalid location";
      end if;
   end Create_Mark;

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

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Src_Editor_Mark) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (File_Marker_Record'Class, File_Marker);
   begin
      This.Mark.Refs := This.Mark.Refs - 1;

      if This.Mark.Refs = 0 then
         if This.Mark.Mark /= null then
            if (Get_Mark (This.Mark.Mark) /= null
                and then not Get_Deleted (Get_Mark (This.Mark.Mark))
                and then Get_Name (Get_Mark (This.Mark.Mark)) = "")
              or else Get_Mark (This.Mark.Mark) = null
            then
               --  Do not delete named marks, since we can still access them
               --  through get_mark() anyway

               Trace (Me, "Deleting unnamed mark");
               Destroy (This.Mark.Mark.all);
               Unchecked_Free (This.Mark.Mark);
            end if;
         end if;

         Free (This.Mark);
      end if;
   end Finalize;

   --------------
   -- Location --
   --------------

   overriding function Location
     (This : Src_Editor_Mark) return Editor_Location'Class
   is
      Mark : Gtk_Text_Mark;
      Iter   : Gtk_Text_Iter;
      Buffer : Src_Editor_Buffer;
   begin
      if This.Mark /= null then
         --  Open the buffer, if necessary. This will also automatically create
         --  the gtk+ mark
         Buffer := Src_Editor_Buffer
           (Get_Buffer_Factory (This.Kernel).Get
            (Get_File (This.Mark.Mark)));

         Mark := Get_Mark (This.Mark.Mark);

         if Mark = null or else Get_Deleted (Mark) then
            return Nil_Editor_Location;
         end if;

         Get_Iter_At_Mark (Buffer.Buffer, Iter, Mark);

         return Create_Editor_Location (Buffer, Iter);
      else
         return Nil_Editor_Location;
      end if;
   end Location;

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

   --------------
   -- New_View --
   --------------

   overriding function New_View
     (This : Src_Editor_Buffer) return Editor_View'Class
   is
      Result : Src_Editor_View;
   begin
      if This.Buffer /= null then
         declare
            Views : constant Views_Array := Get_Views (This.Buffer);
         begin
            Result.Box := New_View (This.Kernel, Views (Views'First));
            Result.Buffer := This;

            return Result;
         end;
      end if;

      return Nil_Editor_View;
   end New_View;

   ----------
   -- Open --
   ----------

   overriding function Open
     (This : Src_Editor_Buffer) return Editor_View'Class
   is
      Current_View : constant Editor_View'Class := This.Current_View;
   begin
      if Current_View = Nil_Editor_View then
         return This.New_View;
      else
         return This.Current_View;
      end if;
   end Open;

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

            Mark := Add_Special_Lines
              (This.Buffer,
               Editable_Line_Type (Start_Line),
               Highlight_Category,
               Text,
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
            if Src_Mark.Mark /= null
              and then Get_Mark (Src_Mark.Mark.Mark) /= null
            then
               Remove_Blank_Lines
                 (This.Buffer, Get_Mark (Src_Mark.Mark.Mark), Lines);
            end if;
         end;
      end if;
   end Remove_Special_Lines;

   ------------------
   -- Current_View --
   ------------------

   overriding function Current_View
     (This : Src_Editor_Buffer) return Editor_View'Class
   is
      Child : MDI_Child;
   begin
      if This.Buffer /= null then
         declare
            File : GNATCOLL.VFS.Virtual_File := Get_Filename (This.Buffer);
         begin
            if File = GNATCOLL.VFS.No_File then
               File := Get_File_Identifier (This.Buffer);
            end if;

            Child := Find_Editor (This.Kernel, File);
         end;

         if Child = null then
            Trace (Me, -"Editor not found");

            return Nil_Editor_View;
         else
            return Src_Editor_View'
              (Editor_View with
               Box    => Source_Editor_Box (Get_Widget (Child)),
               Buffer => This);
         end if;
      else
         return Nil_Editor_View;
      end if;
   end Current_View;

   -----------------
   -- Lines_Count --
   -----------------

   overriding function Lines_Count (This : Src_Editor_Buffer) return Integer is
      Iter : Gtk_Text_Iter;
   begin
      if This.Buffer /= null then
         Get_End_Iter (This.Buffer, Iter);

         declare
            Line   : Editable_Line_Type;
            Column : Visible_Column_Type;
         begin
            Get_Iter_Position (This.Buffer, Iter, Line, Column);

            return Integer (Line);
         end;
      else
         return 0;
      end if;
   end Lines_Count;

   -----------------
   -- Select_Text --
   -----------------

   overriding procedure Select_Text
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location)
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

         Get_Locations (Iter, Iter2, This.Buffer, Src_From, Src_To, False);
         Select_Region
           (This.Buffer,
            Cursor_Iter  => Iter2,
            Bound_Iter   => Iter);
      end if;
   end Select_Text;

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

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (This        : Src_Editor_Buffer;
      Interactive : Boolean := True;
      File        : Virtual_File := No_File)
   is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      if This.Buffer /= null then
         if File = No_File then
            Success := Save_MDI_Children
              (This.Kernel,
               Children =>
                 (1 => Find_Editor
                    (This.Kernel, Get_Filename (This.Buffer))),
               Force    => not Interactive);
         else
            Save_To_File
              (This.Buffer,
               Filename => File,
               Success  => Success);
         end if;
      end if;
   end Save;

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
         end if;
      end if;

      return Nil_Editor_Mark;
   end Get_Mark;

   -----------------
   -- Apply_Style --
   -----------------

   overriding procedure Apply_Style
     (This  : Src_Editor_Buffer;
      Style : not null access Simple_Style_Record'Class;
      Line  : Integer;
      From_Column, To_Column : Integer := -1)
   is
   begin
      --  ??? The interface for Add_Line_Highlighting only works on a single
      --  line. It should probably be enhanced rather than do it in this
      --  procedure

      if From_Column = To_Column then
         Add_Line_Highlighting
           (This.Buffer,
            Editable_Line_Type (Line),
            Style_Access (Style),
            Highlight_In => (others => True));

      else
         Highlight_Range
           (This.Buffer, Style_Access (Style),
            Editable_Line_Type (Line),
            Visible_Column_Type (From_Column),
            Visible_Column_Type (To_Column));
      end if;
   end Apply_Style;

   ------------------
   -- Remove_Style --
   ------------------

   overriding procedure Remove_Style
     (This  : Src_Editor_Buffer;
      Style : not null access Simple_Style_Record'Class;
      Line  : Integer;
      From_Column, To_Column : Integer := -1) is
   begin
      if From_Column = To_Column then
         Remove_Line_Highlighting
           (This.Buffer, Editable_Line_Type (Line), Style_Access (Style));
      else
         Highlight_Range
           (This.Buffer, Style_Access (Style),
            Editable_Line_Type (Line),
            Visible_Column_Type (From_Column),
            Visible_Column_Type (To_Column),
            Remove => True);
      end if;
   end Remove_Style;

   ----------------------
   -- Start_Undo_Group --
   ----------------------

   overriding procedure Start_Undo_Group (This : Src_Editor_Buffer) is
   begin
      End_Action (This.Buffer);
      Start_Group (Get_Command_Queue (This.Buffer));
   end Start_Undo_Group;

   -----------------------
   -- Finish_Undo_Group --
   -----------------------

   overriding procedure Finish_Undo_Group (This : Src_Editor_Buffer) is
   begin
      End_Action (This.Buffer);
      End_Group (Get_Command_Queue (This.Buffer));
   end Finish_Undo_Group;

   ----------
   -- Undo --
   ----------

   overriding procedure Undo (This : Src_Editor_Buffer) is
   begin
      if Get_Writable (This.Buffer) then
         Undo (This.Buffer);
      else
         raise Editor_Exception with -"Buffer is not writable";
      end if;
   end Undo;

   -------------------
   -- Set_Read_Only --
   -------------------

   overriding procedure Set_Read_Only
     (This : Src_Editor_Buffer; Read_Only : Boolean)
   is
   begin
      if This.Buffer /= null then
         Set_Writable
           (This.Buffer, not Read_Only, Explicit => True);
      end if;
   end Set_Read_Only;

   --------------------
   -- Get_Constructs --
   --------------------

   overriding procedure Get_Constructs
     (This       : Src_Editor_Buffer;
      Constructs : out Language.Construct_List;
      Timestamp  : out Natural) is
   begin
      if This.Buffer = null then
         Constructs := (null, null, null, 0);
         Timestamp := 0;
         return;
      end if;

      Constructs := Get_Constructs (This.Buffer, Exact);
      Timestamp  := Get_Constructs_Timestamp (This.Buffer);
   end Get_Constructs;

   ----------
   -- Line --
   ----------

   overriding function Line (This : Src_Editor_Mark) return Integer is
   begin
      return Integer (This.Mark.Mark.Get_Line);
   end Line;

   ------------
   -- Column --
   ------------

   overriding function Column (This : Src_Editor_Mark) return Integer is
   begin
      return Integer (This.Mark.Mark.Get_Column);
   end Column;

   ----------------
   -- Is_Present --
   ----------------

   overriding function Is_Present (This : Src_Editor_Mark) return Boolean is
   begin
      if This.Mark /= null and then Get_Mark (This.Mark.Mark) /= null then
         return not Get_Mark (This.Mark.Mark).Get_Deleted;

      else
         return False;
      end if;
   end Is_Present;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete (This : Src_Editor_Mark) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (File_Marker_Record'Class, File_Marker);
   begin
      if This.Mark /= null then
         Destroy (This.Mark.Mark.all);
         Unchecked_Free (This.Mark.Mark);
      end if;
   end Delete;

   -------------------
   -- Set_Read_Only --
   -------------------

   overriding procedure Set_Read_Only
     (This : Src_Editor_View; Read_Only : Boolean)
   is
   begin
      if This.Box /= null then
         Set_Writable (This.Box, not Read_Only, Explicit => True);
      end if;
   end Set_Read_Only;

   ------------
   -- Center --
   ------------

   overriding procedure Center
     (This     : Src_Editor_View;
      Location : Editor_Location'Class := Nil_Editor_Location)
   is
      Iter       : Gtk_Text_Iter;
      Success    : Boolean;
      Actual_Loc : Src_Editor_Location;
   begin
      if This.Box /= null then
         if Location = Nil_Editor_Location then
            Actual_Loc := Src_Editor_Location (This.Cursor);
         else
            Actual_Loc := Src_Editor_Location (Location);
         end if;

         Get_Cursor_Position (Get_View (This.Box), Iter);
         Get_Location (Iter, Actual_Loc, Iter, Success);

         if Success then
            declare
               M : constant Gtk_Text_Mark :=
                 Create_Mark (Get_Buffer (This.Box), "", Iter);
            begin
               Scroll_To_Mark
                 (Get_View (This.Box),
                  Mark          => M,
                  Within_Margin => 0.2,
                  Use_Align     => False,
                  Xalign        => 0.5,
                  Yalign        => 0.5);
               Delete_Mark (Get_Buffer (This.Box), M);
            end;
         else
            raise Editor_Exception with -"Invalid location";
         end if;
      end if;
   end Center;

   -----------------
   -- Cursor_Goto --
   -----------------

   overriding procedure Cursor_Goto
     (This       : Src_Editor_View;
      Location   : Editor_Location'Class;
      Raise_View : Boolean := False)
   is
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
   begin
      if This.Box /= null then
         Get_Location (Iter, Src_Editor_Location (Location), Iter, Success);

         if Success then
            declare
               Line : Editable_Line_Type;
               Col  : Character_Offset_Type;
            begin
               Get_Iter_Position (Get_Buffer (This.Box), Iter, Line, Col);

               Set_Cursor_Location
                 (This.Box,
                  Line        => Line,
                  Column      => Col,
                  Force_Focus => Raise_View);
            end;

         end if;
      end if;
   end Cursor_Goto;

   ------------
   -- Cursor --
   ------------

   overriding function Cursor
     (This : Src_Editor_View) return Editor_Location'Class
   is
      Iter : Gtk_Text_Iter;
   begin
      if This.Box /= null then
         Get_Cursor_Position (Get_View (This.Box), Iter);
         return Create_Editor_Location (This.Buffer, Iter);
      else
         return Nil_Editor_Location;
      end if;
   end Cursor;

end Src_Editor_Module.Editors;
