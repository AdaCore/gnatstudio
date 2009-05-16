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

with System;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Gdk.Color;          use Gdk.Color;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Gtk.Text_Iter;      use Gtk.Text_Iter;
with Gtk.Text_Mark;      use Gtk.Text_Mark;
with Gtk.Text_Tag;       use Gtk.Text_Tag;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Pango.Enums;        use Pango.Enums;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Clipboard;      use GPS.Kernel.Clipboard;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

with Src_Editor_Module.Line_Highlighting;
use Src_Editor_Module.Line_Highlighting;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_View;           use Src_Editor_View;
with Src_Editor_Module.Markers; use Src_Editor_Module.Markers;

with Commands;                use Commands;
with Find_Utils;              use Find_Utils;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;
with Language;                use Language;
with Src_Contexts;            use Src_Contexts;
with Traces;                  use Traces;

package body Src_Editor_Module.Editors is
   Me : constant Debug_Handle := Create ("Editor.Buffer");

   type Src_Editor_Buffer is new GPS.Editors.Editor_Buffer with record
      Kernel  : Kernel_Handle;
      Buffer  : Source_Buffer;  --  ??? What happens when buffer is destroyed
      File    : Virtual_File;
      Factory : Src_Editor_Buffer_Factory;
   end record;

   type Src_Editor_Location is new GPS.Editors.Editor_Location with record
      Buffer : Src_Editor_Buffer;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
      Offset : Natural;
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

   type Src_Editor_Overlay is new GPS.Editors.Editor_Overlay with record
      Tag    : Gtk_Text_Tag;
   end record;

   type Editor_Properties_Type is (Marks, Locations);

   type Editors_Props_Record (Typ : Editor_Properties_Type)
     is new Instance_Property_Record
   with record
      case Typ is
         when Marks =>
            Mark    : Editor_Mark_Access;
         when Locations =>
            Loc     : Editor_Location_Access;
      end case;
   end record;

   type Editors_Props is access all Editors_Props_Record'Class;
   overriding procedure Destroy (Prop : in out Editors_Props_Record);
   --  See inherited documentation

   function Get
     (This   : Src_Editor_Buffer_Factory'Class;
      Buffer : access Source_Buffer_Record'Class)
      return Editor_Buffer'Class;
   --  Wrap a gtk+ buffer into an abstract representation. If Buffer is null,
   --  Nil_Editor_Buffer is returned

   function Create_Editor_Location
     (Buffer   : Src_Editor_Buffer'Class;
      Location : Gtk_Text_Iter) return Src_Editor_Location'Class;
   --  Return an instance of Editor_Location

   function Create_Editor_Overlay
     (Tag    : Gtk_Text_Tag) return Src_Editor_Overlay'Class;
   --  Return an instance of Src_Editor_Overlay

   procedure Get_Location
     (Iter     : out Gtk_Text_Iter;
      Location : Editor_Location'Class;
      Default  : Gtk_Text_Iter;
      Success  : out Boolean);
   --  Return the iter stored in Location.
   --  If no location could be obtain from the arguments (for instance Location
   --  is Nil_Editor_Location), Iter is Set to Default and Success to False.

   procedure Get_Block
     (Location : Editor_Location'Class;
      Block    : out Block_Record;
      Success  : out Boolean;
      As_Subprogram : Boolean := False);
   --  Similar to Get_Location, but return the block information instead

   procedure Get_Locations
     (Iter1                : out Gtk_Text_Iter;
      Iter2                : out Gtk_Text_Iter;
      Buffer               : Source_Buffer;
      Loc1                 : Editor_Location'Class;
      Loc2                 : Editor_Location'Class;
      Compensate_Last_Iter : Boolean := True);
   --  Set the two iteratiors from the given locations.
   --  If Loc1 is not a src_editor_location (for instance nil_editor_location)
   --  iter1 defaults to the beginning of the buffer. Similarly, Iter2 defaults
   --  to the end of the buffer.
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

   overriding function Block_Name
     (This : Src_Editor_Location; Subprogram : Boolean) return String;
   overriding function Block_Level (This : Src_Editor_Location) return Natural;
   overriding procedure Block_Fold (This : Src_Editor_Location);
   overriding procedure Block_Unfold (This : Src_Editor_Location);

   overriding function Line (This : Src_Editor_Location) return Integer;
   overriding function Column (This : Src_Editor_Location) return Integer;
   overriding function Offset (This : Src_Editor_Location) return Natural;

   overriding procedure Search
     (This              : Src_Editor_Location;
      Pattern           : String;
      Backward          : Boolean := False;
      Case_Sensitive    : Boolean := False;
      Regexp            : Boolean := False;
      Whole_Word        : Boolean := False;
      Scope             : String := "Whole";
      Dialog_On_Failure : Boolean := True;
      Success           : out Boolean;
      Starts            : out Src_Editor_Location;
      Ends              : out Src_Editor_Location);

   overriding function Buffer
     (This : Src_Editor_Location) return Editor_Buffer'Class;

   overriding function Create_Mark
     (This : Src_Editor_Location; Name : String := "")
      return Editor_Mark'Class;

   overriding function Forward_Char
     (This  : Src_Editor_Location;
      Count : Integer) return Editor_Location'Class;
   overriding function Forward_Word
     (This  : Src_Editor_Location;
      Count : Integer) return Editor_Location'Class;
   overriding function Forward_Line
     (This  : Src_Editor_Location;
      Count : Integer) return Editor_Location'Class;
   overriding function Starts_Word
     (This  : Src_Editor_Location) return Boolean;
   overriding function Ends_Word
     (This  : Src_Editor_Location) return Boolean;

   overriding function Get_Overlays
     (This    : Src_Editor_Location) return Overlay_Lists.List;
   overriding function Has_Overlay
     (This    : Src_Editor_Location;
      Overlay : Editor_Overlay'Class) return Boolean;
   overriding function Forward_Overlay
     (This    : Src_Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class;
   overriding function Backward_Overlay
     (This    : Src_Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class;

   overriding function Get_Char (This : Src_Editor_Location) return Integer;

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
     (This : Src_Editor_Mark;
      Open : Boolean) return Editor_Location'Class;

   overriding function Name (This : Src_Editor_Mark) return String;

   overriding procedure Move
     (This : Src_Editor_Mark; Location : Editor_Location'Class);

   -----------------------
   -- Src_Editor_Buffer --
   -----------------------

   overriding procedure Close (This : Src_Editor_Buffer; Force : Boolean);

   overriding function New_Location
     (This   : Src_Editor_Buffer;
      Line   : Integer;
      Column : Integer) return Editor_Location'Class;

   overriding function New_View
     (This : Src_Editor_Buffer) return Editor_View'Class;

   overriding function Open
     (This : Src_Editor_Buffer) return Editor_View'Class;

   overriding function File (This : Src_Editor_Buffer) return Virtual_File;

   overriding procedure Blocks_Fold (This : Src_Editor_Buffer);
   overriding procedure Blocks_Unfold (This : Src_Editor_Buffer);

   overriding function Add_Special_Line
     (This       : Src_Editor_Buffer;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "";
      Column_Id  : String := "";
      Info       : Line_Information_Data := null) return Editor_Mark'Class;

   overriding procedure Remove_Special_Lines
     (This  : Src_Editor_Buffer;
      Mark  : Editor_Mark'Class;
      Lines : Integer);

   overriding function Create_Overlay
     (This : Src_Editor_Buffer;
      Name : String := "") return Editor_Overlay'Class;
   overriding procedure Apply_Overlay
     (This    : Src_Editor_Buffer;
      Overlay : Editor_Overlay'Class;
      From    : Editor_Location'Class := Nil_Editor_Location;
      To      : Editor_Location'Class := Nil_Editor_Location);
   overriding procedure Remove_Overlay
     (This    : Src_Editor_Buffer;
      Overlay : Editor_Overlay'Class;
      From    : Editor_Location'Class := Nil_Editor_Location;
      To      : Editor_Location'Class := Nil_Editor_Location);

   overriding function Current_View
     (This : Src_Editor_Buffer) return Editor_View'Class;
   overriding function Views
     (This : Src_Editor_Buffer) return View_Lists.List;

   overriding function Lines_Count (This : Src_Editor_Buffer) return Integer;
   overriding function Characters_Count
     (This : Src_Editor_Buffer) return Natural;

   overriding function Is_Modified (This : Src_Editor_Buffer) return Boolean;

   overriding procedure Copy
     (This   : Src_Editor_Buffer;
      From   : Editor_Location'Class := Nil_Editor_Location;
      To     : Editor_Location'Class := Nil_Editor_Location;
      Append : Boolean := False);
   overriding procedure Cut
     (This   : Src_Editor_Buffer;
      From   : Editor_Location'Class := Nil_Editor_Location;
      To     : Editor_Location'Class := Nil_Editor_Location;
      Append : Boolean := False);
   overriding procedure Paste
     (This   : Src_Editor_Buffer;
      From   : Editor_Location'Class);

   overriding procedure Select_Text
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location);
   overriding function Selection_Start
     (This : Src_Editor_Buffer) return Editor_Location'Class;
   overriding function Selection_End
     (This : Src_Editor_Buffer) return Editor_Location'Class;
   overriding procedure Unselect (This : Src_Editor_Buffer);

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

   overriding procedure Refill
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
   overriding procedure Redo (This : Src_Editor_Buffer);

   overriding procedure Set_Read_Only
     (This : Src_Editor_Buffer; Read_Only : Boolean);
   overriding function Is_Read_Only
     (This : Src_Editor_Buffer) return Boolean;

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

   overriding procedure Add_File_Information
     (This       : Src_Editor_Buffer;
      Identifier : String;
      Info       : Line_Information_Data);

   ---------------------
   -- Src_Editor_View --
   ---------------------

   overriding procedure Set_Read_Only
     (This : Src_Editor_View; Read_Only : Boolean);
   overriding function Is_Read_Only (This : Src_Editor_View) return Boolean;

   overriding procedure Center
     (This     : Src_Editor_View;
      Location : Editor_Location'Class := Nil_Editor_Location);

   overriding procedure Cursor_Goto
     (This       : Src_Editor_View;
      Location   : Editor_Location'Class;
      Raise_View : Boolean := False;
      Centering  : Centering_Type := With_Margin);

   overriding function Cursor
     (This : Src_Editor_View) return Editor_Location'Class;

   overriding function Title
     (This : Src_Editor_View; Short : Boolean) return String;

   overriding function Get_MDI_Child
     (This : Src_Editor_View) return System.Address;

   overriding function Buffer
     (This : Src_Editor_View) return Editor_Buffer'Class;

   ------------------------
   -- Src_Editor_Overlay --
   ------------------------

   overriding function Name (This : Src_Editor_Overlay) return String;
   overriding function Get_Property
     (This : Src_Editor_Overlay; Name : String) return String;
   overriding function Get_Property
     (This : Src_Editor_Overlay; Name : String) return Boolean;
   overriding procedure Set_Property
     (This : Src_Editor_Overlay; Name : String; Value : String);
   overriding procedure Set_Property
     (This : Src_Editor_Overlay; Name : String; Value : Boolean);

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
      Editor_Loc.Offset := Integer (Get_Offset (Location));

      Editor_Loc.Buffer := Src_Editor_Buffer (Buffer);

      return Editor_Loc;
   end Create_Editor_Location;

   ------------------
   -- Get_Location --
   ------------------

   procedure Get_Location
     (Iter     : out Gtk_Text_Iter;
      Location : Editor_Location'Class;
      Default  : Gtk_Text_Iter;
      Success  : out Boolean)
   is
   begin
      Success := True;

      if Location not in Src_Editor_Location'Class
        or else Src_Editor_Location (Location).Buffer.Buffer = null
        or else not Is_Valid_Position
          (Src_Editor_Location (Location).Buffer.Buffer,
           Src_Editor_Location (Location).Line,
           Src_Editor_Location (Location).Column)
      then
         Copy (Source => Default, Dest => Iter);
         Success := False;

      else
         Get_Iter_At_Screen_Position
           (Src_Editor_Location (Location).Buffer.Buffer, Iter,
            Line   => Src_Editor_Location (Location).Line,
            Column => Src_Editor_Location (Location).Column);
      end if;
   end Get_Location;

   -------------------
   -- Get_Locations --
   -------------------

   procedure Get_Locations
     (Iter1                : out Gtk_Text_Iter;
      Iter2                : out Gtk_Text_Iter;
      Buffer               : Source_Buffer;
      Loc1                 : Editor_Location'Class;
      Loc2                 : Editor_Location'Class;
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

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (This : Src_Editor_Buffer; Force : Boolean)
   is
      pragma Unreferenced (Force);
   begin
      --  ??? We never took a ref, we shouldn't unref what we do not hold
      Unref (This.Buffer);

      --  ??? We must close the views, not just remove from the htable
      --  Python had the following implementation:
--              declare
--                 Views : constant Views_Array := Get_Views (Buffer);
--              begin
--                 for V in Views'Range loop
--                    Close (Get_MDI (Get_Kernel (Data)), Views (V),
--                           Force => Force);
--                 end loop;
--              end;

      while Pure_Editors_Hash.Get
        (This.Factory.Pure_Buffers.all, This.File).Box /= null
      loop
         Pure_Editors_Hash.Remove (This.Factory.Pure_Buffers.all, This.File);
      end loop;
   end Close;

   ---------
   -- Get --
   ---------

   overriding function Get
     (This        : Src_Editor_Buffer_Factory;
      File        : Virtual_File;
      Force       : Boolean := False;
      Open_Buffer : Boolean := False;
      Open_View   : Boolean := True) return Editor_Buffer'Class
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
         if Open_View then
            Box := Open_File
              (This.Kernel, File, Line => 0, Column => 0, Column_End => 0);
         else
            Box := Pure_Editors_Hash.Get (This.Pure_Buffers.all, File).Box;

            if Box = null then
               if Open_Buffer then
                  Box := Create_File_Editor (This.Kernel, File, False);
                  Pure_Editors_Hash.Set
                    (This.Pure_Buffers.all, File, (Box => Box));
               end if;
            end if;
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
         return Get (This, Get_Buffer (Box));
      end if;
   end Get;

   -------------
   -- Get_New --
   -------------

   overriding function Get_New
     (This : Src_Editor_Buffer_Factory) return Editor_Buffer'Class
   is
      Box   : Source_Editor_Box;
   begin
      Box := Open_File
        (This.Kernel, No_File, Line => 1, Column => 1, Column_End => 1);
      return Get (This, Get_Buffer (Box));
   end Get_New;

   ---------
   -- Get --
   ---------

   function Get
     (This   : Src_Editor_Buffer_Factory'Class;
      Buffer : access Source_Buffer_Record'Class)
      return Editor_Buffer'Class
   is
   begin
      --  ??? We must setup callbacks so that when the editor is closed we
      --  reset the internal Buffer field to null to avoid errors
      return Src_Editor_Buffer'
        (Editor_Buffer with
         File    => Get_Filename (Buffer),
         Factory => Src_Editor_Buffer_Factory (This),
         Kernel  => This.Kernel,
         Buffer  => Source_Buffer (Buffer));
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

   ---------------
   -- Get_Block --
   ---------------

   procedure Get_Block
     (Location : Editor_Location'Class;
      Block    : out Block_Record;
      Success  : out Boolean;
      As_Subprogram : Boolean := False)
   is
      Line    : Buffer_Line_Type;
      Iter    : Gtk_Text_Iter;
      Buffer  : Source_Buffer;
   begin
      Get_Location (Iter, Location, Iter, Success);

      if Success then
         Buffer := Source_Buffer (Get_Buffer (Iter));
         Line   := Buffer_Line_Type (Get_Line (Iter) + 1);

         if As_Subprogram then
            Block  := Get_Subprogram_Block
              (Buffer, Get_Editable_Line (Buffer, Line));
         else
            Block  := Get_Block
              (Buffer, Get_Editable_Line (Buffer, Line));
         end if;
      end if;
   end Get_Block;

   -----------------
   -- Block_Start --
   -----------------

   overriding function Block_Start
     (This : Src_Editor_Location) return Editor_Location'Class
   is
      Success : Boolean;
      Block   : Block_Record;
   begin
      Get_Block (This, Block, Success);

      if Success then
         return Src_Editor_Location'
           (Editor_Location with
            Buffer => This.Buffer,
            Line   => Block.First_Line,
            Column => 1,
            Offset => 1);
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
      Block       : Block_Record;
      Iter, Iter2 : Gtk_Text_Iter;
   begin
      Get_Block (This, Block, Success);

      if Success then
         Get_Location (Iter, This, Iter, Success);
         Get_Iter_At_Screen_Position
           (Source_Buffer (Get_Buffer (Iter)), Iter2, Block.Last_Line,
            Character_Offset_Type'(1));

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
      Block   : Block_Record;
   begin
      Get_Block (This, Block, Success);

      if Success then
         return Block.Block_Type;
      else
         return Cat_Unknown;
      end if;
   end Block_Type;

   ----------------
   -- Block_Name --
   ----------------

   overriding function Block_Name
     (This : Src_Editor_Location; Subprogram : Boolean) return String
   is
      Success : Boolean;
      Block   : Block_Record;
   begin
      Get_Block (This, Block, Success, As_Subprogram => Subprogram);

      if Success and then Block.Name /= null then
         return Block.Name.all;
      end if;

      return "";
   end Block_Name;

   -----------------
   -- Block_Level --
   -----------------

   overriding function Block_Level
     (This : Src_Editor_Location) return Natural
   is
      Success : Boolean;
      Block   : Block_Record;
   begin
      Get_Block (This, Block, Success);

      if Success then
         return Block.Indentation_Level;
      end if;

      return 0;
   end Block_Level;

   ----------------
   -- Block_Fold --
   ----------------

   overriding procedure Block_Fold (This : Src_Editor_Location) is
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
      Buffer  : Source_Buffer;
   begin
      Get_Location (Iter, This, Iter, Success);
      if Success then
         Buffer := Source_Buffer (Get_Buffer (Iter));
         Fold_Block
           (Buffer,
            Get_Editable_Line (Buffer,
              Buffer_Line_Type (Get_Line (Iter) + 1)));
      end if;
   end Block_Fold;

   ------------------
   -- Block_Unfold --
   ------------------

   overriding procedure Block_Unfold (This : Src_Editor_Location) is
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
      Buffer  : Source_Buffer;
   begin
      Get_Location (Iter, This, Iter, Success);
      if Success then
         Buffer := Source_Buffer (Get_Buffer (Iter));
         Unfold_Line
           (Buffer,
            Get_Editable_Line (Buffer,
              Buffer_Line_Type (Get_Line (Iter) + 1)));
      end if;
   end Block_Unfold;

   --------------
   -- Get_Char --
   --------------

   overriding function Get_Char (This : Src_Editor_Location) return Integer is
      Unichar : Gunichar;
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
   begin
      Get_Location (Iter, This, Iter, Success);

      if Success then
         Unichar := Get_Char (Iter);
         if Unichar = 0 then
            raise Editor_Exception with -"Invalid buffer position";
         end if;

         return Integer (Unichar);
      else
         raise Editor_Exception with -"Invalid location";
      end if;
   end Get_Char;

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
   -- Offset --
   ------------

   overriding function Offset (This : Src_Editor_Location) return Natural is
   begin
      return Integer (This.Offset);
   end Offset;

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
         if Count >= 0 then
            Forward_Chars (Iter, Gint (Count), Success);
         else
            Backward_Chars (Iter, -Gint (Count), Success);
         end if;
         return Create_Editor_Location (This.Buffer, Iter);
      end if;

      raise Editor_Exception with -"Invalid location";
   end Forward_Char;

   ------------------
   -- Forward_Word --
   ------------------

   overriding function Forward_Word
     (This  : Src_Editor_Location;
      Count : Integer) return Editor_Location'Class
   is
      Success : Boolean;
      Iter    : Gtk_Text_Iter;
   begin
      Get_Location (Iter, This, Iter, Success);
      if Success then
         if Count >= 0 then
            Forward_Word_Ends (Iter, Gint (Count), Success);
         else
            Backward_Word_Starts (Iter, -Gint (Count), Success);
         end if;
         return Create_Editor_Location (This.Buffer, Iter);
      end if;

      raise Editor_Exception with -"Invalid location";
   end Forward_Word;

   ------------------
   -- Forward_Line --
   ------------------

   overriding function Forward_Line
     (This  : Src_Editor_Location;
      Count : Integer) return Editor_Location'Class
   is
      Success : Boolean;
      Iter    : Gtk_Text_Iter;
   begin
      Get_Location (Iter, This, Iter, Success);
      if Success then
         if Count >= 0 then
            Forward_Lines (Iter, Gint (Count), Success);
         else
            Backward_Lines (Iter, -Gint (Count), Success);
         end if;
         return Create_Editor_Location (This.Buffer, Iter);
      end if;

      raise Editor_Exception with -"Invalid location";
   end Forward_Line;

   -----------------
   -- Starts_Word --
   -----------------

   overriding function Starts_Word
     (This  : Src_Editor_Location) return Boolean
   is
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
   begin
      Get_Location (Iter, This, Iter, Success);
      return Success and then Standard.Src_Editor_Buffer.Starts_Word (Iter);
   end Starts_Word;

   ---------------
   -- Ends_Word --
   ---------------

   overriding function Ends_Word
     (This  : Src_Editor_Location) return Boolean
   is
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
   begin
      Get_Location (Iter, This, Iter, Success);
      return Success and then Standard.Src_Editor_Buffer.Ends_Word (Iter);
   end Ends_Word;

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
     (This : Src_Editor_Mark;
      Open : Boolean) return Editor_Location'Class
   is
      Mark : Gtk_Text_Mark;
      Iter   : Gtk_Text_Iter;
      Buffer : Src_Editor_Buffer;
   begin
      if This.Mark /= null then
         --  Open the buffer, if necessary. This will also automatically create
         --  the gtk+ mark

         declare
            Buf : constant Editor_Buffer'Class :=
              Get_Buffer_Factory (This.Kernel).Get
              (Get_File (This.Mark.Mark),
               Force       => False,
               Open_Buffer => False,
               Open_View   => Open);
         begin
            if Buf = Nil_Editor_Buffer then
               return Nil_Editor_Location;
            end if;

            Buffer := Src_Editor_Buffer (Buf);
         end;

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

   ----------
   -- Move --
   ----------

   overriding procedure Move
     (This : Src_Editor_Mark; Location : Editor_Location'Class)
   is
      Mark    : Gtk_Text_Mark;
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
   begin
      if This.Mark /= null then
         Get_Location (Iter, Location, Iter, Success);
         if Success then
            Mark := Get_Mark (This.Mark.Mark);

            if Mark /= null and then not Get_Deleted (Mark) then
               --  Do we need to update the coordinates stored in This.Mark ?
               Move_Mark (Get_Buffer (Iter), Mark, Where => Iter);

            else
               --  Mark is not set in an existing buffer, we just change our
               --  internal coordinates
               Mark := Create_Mark (Get_Buffer (Iter), Where => Iter);
               Move (This.Mark.Mark, Mark);
            end if;

         else
            raise Editor_Exception with -"Invalid location";
         end if;
      end if;
   end Move;

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
   -- File --
   ----------

   overriding function File (This : Src_Editor_Buffer) return Virtual_File is
   begin
      return This.File;
   end File;

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
      Name       : String := "";
      Column_Id  : String := "";
      Info       : Line_Information_Data := null) return Editor_Mark'Class
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
               Name,
               Column_Id,
               Info);

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
            --  ??? We must setup callbacks so that when the editor is closed
            --  we reset the internal Box field to null to avoid errors

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
         --  ??? Should we check in the file on the disk
         return 0;
      end if;
   end Lines_Count;

   ----------------------
   -- Characters_Count --
   ----------------------

   overriding function Characters_Count
     (This : Src_Editor_Buffer) return Natural is
   begin
      if This.Buffer /= null then
         return Integer (Get_Char_Count (This.Buffer));
      else
         --  ??? Should we check in the file on the disk
         return 0;
      end if;
   end Characters_Count;

   -----------------
   -- Select_Text --
   -----------------

   overriding procedure Select_Text
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location)
   is
      Iter, Iter2 : Gtk_Text_Iter;
   begin
      if This.Buffer /= null then
         Get_Locations (Iter, Iter2, This.Buffer, From, To, False);
         Select_Region (This.Buffer, Cursor_Iter => Iter2, Bound_Iter => Iter);
      end if;
   end Select_Text;

   ---------------------
   -- Selection_Start --
   ---------------------

   overriding function Selection_Start
     (This : Src_Editor_Buffer) return Editor_Location'Class is
   begin
      if This.Buffer /= null then
         declare
            Mark, Cursor   : Gtk_Text_Mark;
            IMark, ICursor : Gtk_Text_Iter;
            Mark_First     : Boolean;
         begin
            Mark   := Get_Selection_Bound (This.Buffer);
            Cursor := Get_Insert (This.Buffer);
            Get_Iter_At_Mark (This.Buffer, IMark, Mark);
            Get_Iter_At_Mark (This.Buffer, ICursor, Cursor);

            Mark_First := Compare (IMark, ICursor) <= 0;

            if Mark_First then
               return Create_Editor_Location (This, IMark);
            else
               return Create_Editor_Location (This, ICursor);
            end if;
         end;
      else
         return Nil_Editor_Location;
      end if;
   end Selection_Start;

   -------------------
   -- Selection_End --
   -------------------

   overriding function Selection_End
     (This : Src_Editor_Buffer) return Editor_Location'Class is
   begin
      if This.Buffer /= null then
         declare
            Mark, Cursor   : Gtk_Text_Mark;
            IMark, ICursor : Gtk_Text_Iter;
            Mark_First     : Boolean;
         begin
            Mark   := Get_Selection_Bound (This.Buffer);
            Cursor := Get_Insert (This.Buffer);
            Get_Iter_At_Mark (This.Buffer, IMark, Mark);
            Get_Iter_At_Mark (This.Buffer, ICursor, Cursor);

            Mark_First := Compare (IMark, ICursor) <= 0;

            if Mark_First then
               return Create_Editor_Location (This, ICursor);
            else
               return Create_Editor_Location (This, IMark);
            end if;
         end;
      else
         return Nil_Editor_Location;
      end if;
   end Selection_End;

   --------------
   -- Unselect --
   --------------

   overriding procedure Unselect (This : Src_Editor_Buffer) is
   begin
      if This.Buffer /= null then
         Select_Region
           (This.Buffer,
            Start_Line   => Gint'(0),
            Start_Column => 0,
            End_Line     => 0,
            End_Column   => 0);
      end if;
   end Unselect;

   ---------------
   -- Get_Chars --
   ---------------

   overriding function Get_Chars
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) return String
   is
      Iter, Iter2 : Gtk_Text_Iter;
      Begin_Line : Editable_Line_Type;
      Begin_Col  : Character_Offset_Type;
      End_Line   : Editable_Line_Type;
      End_Col    : Character_Offset_Type;
   begin
      if This.Buffer /= null then
         Get_Locations (Iter, Iter2, This.Buffer, From, To);
         Get_Iter_Position (This.Buffer, Iter, Begin_Line, Begin_Col);
         Get_Iter_Position (This.Buffer, Iter2, End_Line, End_Col);
         return Get_Text
           (This.Buffer, Begin_Line, Begin_Col, End_Line, End_Col);
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
   begin
      if This.Buffer /= null then
         Get_Locations (Iter, Iter2, This.Buffer, From, To);

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
   begin
      if This.Buffer /= null then
         Get_Locations (Iter, Iter2, This.Buffer, From, To);

         if not Do_Indentation (This.Buffer, Iter, Iter2) then
            raise Editor_Exception with -"Error while indenting";
         end if;
      end if;

      End_Action (This.Buffer);
   end Indent;

   ------------
   -- Refill --
   ------------

   overriding procedure Refill
     (This : Src_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location)
   is
      Iter, Iter2 : Gtk_Text_Iter;
   begin
      if This.Buffer /= null then
         Get_Locations (Iter, Iter2, This.Buffer, From, To);

         if Get_Writable (This.Buffer) then
            Select_Region
              (This.Buffer,
               Cursor_Iter  => Iter2,
               Bound_Iter   => Iter);
            if not Do_Refill (This.Buffer) then
               raise Editor_Exception with -"Error while refilling buffer";
            end if;
         else
            raise Editor_Exception with -"Buffer is not writable";
         end if;
      end if;

      End_Action (This.Buffer);
   end Refill;

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
         Get_Iter_Position (This.Buffer, Iter, Line, Column);

         return Src_Editor_Location'
           (Editor_Location with
            Buffer => This,
            Line   => Line,
            Column => Column,
            Offset => Integer (Get_Offset (Iter)));
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
         Get_Iter_Position (This.Buffer, Iter, Line, Column);

         return Src_Editor_Location'
           (Editor_Location with
            Buffer => This,
            Line   => Line,
            Column => Column,
            Offset => Integer (Get_Offset (Iter)));
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
               Success  => Success,
               Force    => not Interactive);
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
         else
            raise Editor_Exception with -"No such mark";
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

   ----------
   -- Redo --
   ----------

   overriding procedure Redo (This : Src_Editor_Buffer) is
   begin
      if Get_Writable (This.Buffer) then
         Redo (This.Buffer);
      else
         raise Editor_Exception with -"Buffer is not writable";
      end if;
   end Redo;

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

         declare
            Views : constant Views_Array := Get_Views (This.Buffer);
         begin
            --  Update the labels on the view as well
            for J in Views'Range loop
               Check_Writable (Views (J));
            end loop;
         end;
      end if;
   end Set_Read_Only;

   ------------------
   -- Is_Read_Only --
   ------------------

   overriding function Is_Read_Only
     (This : Src_Editor_Buffer) return Boolean is
   begin
      return This.Buffer = null or else not Get_Writable (This.Buffer);
   end Is_Read_Only;

   --------------------------
   -- Add_File_Information --
   --------------------------

   overriding procedure Add_File_Information
     (This       : Src_Editor_Buffer;
      Identifier : String;
      Info       : Line_Information_Data) is
   begin
      if This.Buffer = null then
         return;
      end if;

      Add_File_Information (This.Buffer, Identifier, Info);
   end Add_File_Information;

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

   ------------------
   -- Is_Read_Only --
   ------------------

   overriding function Is_Read_Only (This : Src_Editor_View) return Boolean is
   begin
      --  If no box, return False so that we do not raise further errors later
      return This.Box /= null
        and then not Get_Writable (Get_Buffer (This.Box));
   end Is_Read_Only;

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
      Raise_View : Boolean := False;
      Centering  : Centering_Type := With_Margin)
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
                  Force_Focus => Raise_View,
                  Centering   => Centering);
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

   -----------
   -- Title --
   -----------

   overriding function Title
     (This : Src_Editor_View; Short : Boolean) return String
   is
      Child : Gtkada.MDI.MDI_Child;
   begin
      if This.Box /= null then
         Child := Find_MDI_Child_From_Widget (This.Box);
         if Child /= null then
            if Short then
               return Get_Title (Child);
            else
               return Get_Short_Title (Child);
            end if;
         end if;
      end if;
      return "";
   end Title;

   ------------
   -- Buffer --
   ------------

   overriding function Buffer
     (This : Src_Editor_View) return Editor_Buffer'Class is
   begin
      return This.Buffer;
   end Buffer;

   -------------------
   -- Get_MDI_Child --
   -------------------

   overriding function Get_MDI_Child
     (This : Src_Editor_View) return System.Address
   is
      Child : Gtkada.MDI.MDI_Child;
      function Unchecked is new Ada.Unchecked_Conversion
        (Gtkada.MDI.MDI_Child, System.Address);
   begin
      Child := Find_MDI_Child_From_Widget (This.Box);

      if Child = null then
         return System.Null_Address;
      else
         return Unchecked (Child);
      end if;
   end Get_MDI_Child;

   ------------
   -- Create --
   ------------

   function Create (Kernel : Kernel_Handle) return Src_Editor_Buffer_Factory is
      R : Src_Editor_Buffer_Factory;
   begin
      R.Kernel := Kernel;
      R.Pure_Buffers := new Pure_Editors_Hash.HTable;
      return R;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (X : in out Src_Editor_Buffer_Factory) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Pure_Editors_Hash.HTable, Table_Access);
   begin
      Free (X.Pure_Buffers);
   end Destroy;

   -----------------
   -- Is_Modified --
   -----------------

   overriding function Is_Modified (This : Src_Editor_Buffer) return Boolean is
   begin
      return This.Buffer /= null
        and then Get_Status (This.Buffer) = Modified;
   end Is_Modified;

   ----------
   -- Copy --
   ----------

   overriding procedure Copy
     (This   : Src_Editor_Buffer;
      From   : Editor_Location'Class := Nil_Editor_Location;
      To     : Editor_Location'Class := Nil_Editor_Location;
      Append : Boolean := False)
   is
      Iter, Iter2 : Gtk_Text_Iter;
   begin
      if This.Buffer /= null then
         Get_Locations (Iter, Iter2, This.Buffer, From, To);

         External_End_Action (This.Buffer);
         Select_Range (This.Buffer, Iter, Iter2);

         Copy_Clipboard (Get_Clipboard (This.Kernel), This.Buffer);

         if Append then
            Merge_Clipboard (Get_Clipboard (This.Kernel), 1, 2);
         end if;
      end if;
   end Copy;

   ---------
   -- Cut --
   ---------

   overriding procedure Cut
     (This   : Src_Editor_Buffer;
      From   : Editor_Location'Class := Nil_Editor_Location;
      To     : Editor_Location'Class := Nil_Editor_Location;
      Append : Boolean := False)
   is
      Iter, Iter2 : Gtk_Text_Iter;
   begin
      if This.Buffer /= null then
         Get_Locations (Iter, Iter2, This.Buffer, From, To);

         External_End_Action (This.Buffer);
         Select_Range (This.Buffer, Iter, Iter2);

         if Get_Writable (This.Buffer) then
            Cut_Clipboard (Get_Clipboard (This.Kernel), This.Buffer);
            End_Action (This.Buffer);
         else
            raise Editor_Exception with -"Buffer is not writable";
         end if;

         if Append then
            Merge_Clipboard (Get_Clipboard (This.Kernel), 1, 2);
         end if;
      end if;
   end Cut;

   -----------
   -- Paste --
   -----------

   overriding procedure Paste
     (This   : Src_Editor_Buffer;
      From   : Editor_Location'Class)
   is
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
   begin
      Get_Location (Iter, From, Iter, Success);
      if not Success then
         return;

      elsif Get_Buffer (Iter) /= Gtk_Text_Buffer (This.Buffer) then
         raise Editor_Exception with -"Location is not in the same buffer";

      elsif This.Buffer /= null then
         if Get_Writable (This.Buffer) then
            Place_Cursor (This.Buffer, Iter);
            Paste_Clipboard (Get_Clipboard (This.Kernel), This.Buffer);
            End_Action (This.Buffer);
         else
            raise Editor_Exception with -"Buffer is not writable";
         end if;
      end if;
   end Paste;

   -----------------
   -- Blocks_Fold --
   -----------------

   overriding procedure Blocks_Fold (This : Src_Editor_Buffer) is
   begin
      if This.Buffer /= null then
         Fold_All (This.Buffer);
      end if;
   end Blocks_Fold;

   -------------------
   -- Blocks_Unfold --
   -------------------

   overriding procedure Blocks_Unfold (This : Src_Editor_Buffer) is
   begin
      if This.Buffer /= null then
         Unfold_All (This.Buffer);
      end if;
   end Blocks_Unfold;

   ----------
   -- Name --
   ----------

   overriding function Name (This : Src_Editor_Mark) return String is
      Mark : Gtk_Text_Mark;
   begin
      if This.Mark /= null then
         Mark := Get_Mark (This.Mark.Mark);
         if Mark /= null then
            return Get_Name (Mark);
         end if;
      end if;
      return "";
   end Name;

   ------------
   -- Search --
   ------------

   overriding procedure Search
     (This              : Src_Editor_Location;
      Pattern           : String;
      Backward          : Boolean := False;
      Case_Sensitive    : Boolean := False;
      Regexp            : Boolean := False;
      Whole_Word        : Boolean := False;
      Scope             : String := "Whole";
      Dialog_On_Failure : Boolean := True;
      Success           : out Boolean;
      Starts            : out Src_Editor_Location;
      Ends              : out Src_Editor_Location)
   is
      Context : Current_File_Context_Access :=
        Current_File_Context_Access
          (Current_File_Factory
               (Kernel          => This.Buffer.Kernel,
                All_Occurrences => False,
                Scope           => Search_Scope'Value (Scope)));
      Match_From  : Gtk_Text_Iter;
      Match_Up_To : Gtk_Text_Iter;
      Iter        : Gtk_Text_Iter;
   begin
      Set_Context
        (Context,
         Look_For => Pattern,
         Options  => (Case_Sensitive => Case_Sensitive,
                      Whole_Word     => Whole_Word,
                      Regexp         => Regexp));

      Get_Location (Iter, This, Iter, Success);
      if Success then
         Search_In_Editor
           (Context           => Context,
            Start_At          => Iter,
            Kernel            => This.Buffer.Kernel,
            Search_Backward   => Backward,
            Dialog_On_Failure => Dialog_On_Failure,
            Match_From        => Match_From,
            Match_Up_To       => Match_Up_To,
            Found             => Success);

         if Success then
            Starts := Src_Editor_Location
              (Create_Editor_Location (This.Buffer, Match_From));
            Ends   := Src_Editor_Location
              (Create_Editor_Location (This.Buffer, Match_Up_To));
         end if;
      end if;

      Free (Search_Context_Access (Context));
   end Search;

   ---------------------------
   -- Create_Editor_Overlay --
   ---------------------------

   function Create_Editor_Overlay
     (Tag : Gtk_Text_Tag) return Src_Editor_Overlay'Class
   is
      Overlay : Src_Editor_Overlay;
   begin
      Overlay.Tag    := Tag;
      return Overlay;
   end Create_Editor_Overlay;

   --------------------
   -- Create_Overlay --
   --------------------

   overriding function Create_Overlay
     (This : Src_Editor_Buffer;
      Name : String := "") return Editor_Overlay'Class
   is
      Tag     : Gtk_Text_Tag;
   begin
      if This.Buffer /= null then
         if Name /= "" then
            Tag := Lookup (Get_Tag_Table (This.Buffer), Name);
         end if;

         if Tag = null then
            Gtk_New (Tag, Name);
            Add (Get_Tag_Table (This.Buffer), Tag);
         end if;

         return Create_Editor_Overlay (Tag);
      end if;

      return Nil_Editor_Overlay;
   end Create_Overlay;

   -------------------
   -- Apply_Overlay --
   -------------------

   overriding procedure Apply_Overlay
     (This    : Src_Editor_Buffer;
      Overlay : Editor_Overlay'Class;
      From    : Editor_Location'Class := Nil_Editor_Location;
      To      : Editor_Location'Class := Nil_Editor_Location)
   is
      Ovy        : constant Src_Editor_Overlay := Src_Editor_Overlay (Overlay);
      Iter1, Iter2 : Gtk_Text_Iter;
   begin
      if This.Buffer /= null and then Ovy.Tag /= null then
         Get_Locations (Iter1, Iter2, This.Buffer, From, To);
         Apply_Tag (This.Buffer, Ovy.Tag, Iter1, Iter2);
      end if;
   end Apply_Overlay;

   --------------------
   -- Remove_Overlay --
   --------------------

   overriding procedure Remove_Overlay
     (This    : Src_Editor_Buffer;
      Overlay : Editor_Overlay'Class;
      From    : Editor_Location'Class := Nil_Editor_Location;
      To      : Editor_Location'Class := Nil_Editor_Location)
   is
      Ovy        : constant Src_Editor_Overlay := Src_Editor_Overlay (Overlay);
      Iter1, Iter2 : Gtk_Text_Iter;
   begin
      if This.Buffer /= null and then Ovy.Tag /= null then
         Get_Locations (Iter1, Iter2, This.Buffer, From, To);
         Remove_Tag (This.Buffer, Ovy.Tag, Iter1, Iter2);
      end if;
   end Remove_Overlay;

   ----------
   -- Name --
   ----------

   overriding function Name (This : Src_Editor_Overlay) return String is
   begin
      if This.Tag /= null then
         return Get_Property (This.Tag, Gtk.Text_Tag.Name_Property);
      end if;
      return "";
   end Name;

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (This : Src_Editor_Overlay; Name : String) return String
   is
      Color : Gdk_Color;
      W     : Weight;
      S     : Style;
   begin
      if This.Tag /= null then
         if Name = "foreground" then
            Color := Get_Property (This.Tag, Foreground_Gdk_Property);
            return To_String (Color);

         elsif Name = "background" then
            Color := Get_Property (This.Tag, Background_Gdk_Property);
            return To_String (Color);

         elsif Name = "font" then
            return Get_Property (This.Tag, Font_Property);

         elsif Name = "weight" then
            W := Get_Property (This.Tag, Weight_Property);
            case W is
               when Pango_Weight_Ultralight .. Pango_Weight_Light =>
                  return "light";
               when Pango_Weight_Normal .. Pango_Weight_Medium =>
                  return "normal";
               when others =>
                  return "bold";
            end case;

         elsif Name = "style" then
            S := Get_Property (This.Tag, Gtk.Text_Tag.Style_Property);
            case S is
               when Pango_Style_Normal =>
                  return "normal";
               when Pango_Style_Oblique =>
                  return "oblique";
               when Pango_Style_Italic =>
                  return "italic";
            end case;
         else
            raise Editor_Exception with -"Invalid property";
         end if;
      end if;

      return "";
   end Get_Property;

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (This : Src_Editor_Overlay; Name : String) return Boolean is
   begin
      if This.Tag /= null then
         if Name = "editable" then
            return Get_Property (This.Tag, Gtk.Text_Tag.Editable_Property);
         else
            raise Editor_Exception with -"Invalid property";
         end if;
      end if;

      return False;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (This : Src_Editor_Overlay; Name : String; Value : String) is
   begin
      if This.Tag /= null then
         if Name = "foreground" then
            Set_Property (This.Tag, Foreground_Property, Value);
         elsif Name = "background" then
            Set_Property (This.Tag, Background_Property, Value);
         elsif Name = "font" then
            Set_Property (This.Tag, Font_Property, Value);
         elsif Name = "weight" then
            if Value = "light" then
               Set_Property (This.Tag, Weight_Property, Pango_Weight_Light);
            elsif Value = "normal" then
               Set_Property (This.Tag, Weight_Property, Pango_Weight_Normal);
            elsif Value = "bold" then
               Set_Property (This.Tag, Weight_Property, Pango_Weight_Bold);
            else
               raise Editor_Exception
                 with -"Invalid weight: use light, normal or bold";
            end if;
         elsif Name = "style" then
            if Value = "normal" then
               Set_Property
                 (This.Tag, Gtk.Text_Tag.Style_Property, Pango_Style_Normal);
            elsif Value = "oblique" then
               Set_Property
                 (This.Tag, Gtk.Text_Tag.Style_Property, Pango_Style_Oblique);
            elsif Value = "italic" then
               Set_Property
                 (This.Tag, Gtk.Text_Tag.Style_Property, Pango_Style_Italic);
            else
               raise Editor_Exception
                 with -"Invalid style: use normal, oblique or italic";
            end if;
         else
            raise Editor_Exception with -"Invalid property";
         end if;
      end if;
   end Set_Property;

   overriding procedure Set_Property
     (This : Src_Editor_Overlay; Name : String; Value : Boolean) is
   begin
      if This.Tag /= null then
         if Name = "editable" then
            Set_Property (This.Tag, Gtk.Text_Tag.Editable_Property, Value);
         else
            raise Editor_Exception with -"Invalid property";
         end if;
      end if;
   end Set_Property;

   ------------------
   -- Get_Overlays --
   ------------------

   overriding function Get_Overlays
     (This    : Src_Editor_Location) return Overlay_Lists.List
   is
      use Overlay_Lists, Gtk.Text_Tag.Text_Tag_List;
      Iter     : Gtk_Text_Iter;
      Success  : Boolean;
      List     : Overlay_Lists.List := Overlay_Lists.Empty_List;
      Tag_List : GSlist;
      Tag_Iter : GSlist;
   begin
      Get_Location (Iter, This, Iter, Success);
      if Success then
         Tag_List := Get_Tags (Iter);
         Tag_Iter := Tag_List;

         while Tag_Iter /= Null_List loop
            Append (List, Create_Editor_Overlay (Get_Data (Tag_Iter)));
            Tag_Iter := Next (Tag_Iter);
         end loop;

         Free (Tag_List);
      end if;

      return List;
   end Get_Overlays;

   -----------------
   -- Has_Overlay --
   -----------------

   overriding function Has_Overlay
     (This    : Src_Editor_Location;
      Overlay : Editor_Overlay'Class) return Boolean
   is
      Iter    : Gtk_Text_Iter;
      Success : Boolean;
      Tag     : Gtk_Text_Tag := null;
   begin
      if Overlay in Src_Editor_Overlay'Class then
         Tag := Src_Editor_Overlay (Overlay).Tag;
      end if;

      Get_Location (Iter, This, Iter, Success);
      return Success and then Has_Tag (Iter, Tag);
   end Has_Overlay;

   ---------------------
   -- Forward_Overlay --
   ---------------------

   overriding function Forward_Overlay
     (This    : Src_Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class
   is
      Iter : Gtk_Text_Iter;
      Success : Boolean;
      Tag     : Gtk_Text_Tag := null;
   begin
      if Overlay in Src_Editor_Overlay'Class then
         Tag := Src_Editor_Overlay (Overlay).Tag;
      end if;

      Get_Location (Iter, This, Iter, Success);
      if Success then
         Forward_To_Tag_Toggle (Iter, Tag, Success);
         if Success then
            return Create_Editor_Location (This.Buffer, Iter);
         end if;
      end if;

      return This.Buffer.End_Of_Buffer;
   end Forward_Overlay;

   ----------------------
   -- Backward_Overlay --
   ----------------------

   overriding function Backward_Overlay
     (This    : Src_Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class
   is
      Iter : Gtk_Text_Iter;
      Success : Boolean;
      Tag     : Gtk_Text_Tag := null;
   begin
      if Overlay in Src_Editor_Overlay'Class then
         Tag := Src_Editor_Overlay (Overlay).Tag;
      end if;

      Get_Location (Iter, This, Iter, Success);
      if Success then
         Backward_To_Tag_Toggle (Iter, Tag, Success);
         if Success then
            return Create_Editor_Location (This.Buffer, Iter);
         end if;
      end if;

      return This.Buffer.Beginning_Of_Buffer;
   end Backward_Overlay;

   -----------
   -- Views --
   -----------

   overriding function Views
     (This : Src_Editor_Buffer) return View_Lists.List
   is
      List  : View_Lists.List := View_Lists.Empty_List;
   begin
      if This.Buffer /= null then
         declare
            Views : constant Views_Array := Get_Views (This.Buffer);
         begin
            for V in Views'Range loop
               View_Lists.Append
                 (List,
                  Src_Editor_View'
                    (Editor_View with
                     Box    => Views (V),
                     Buffer => This));
            end loop;
         end;
      end if;

      return List;
   end Views;

   -------------
   -- Buffers --
   -------------

   overriding function Buffers
     (This   : Src_Editor_Buffer_Factory) return Buffer_Lists.List
   is
      Result : Buffer_Lists.List := Buffer_Lists.Empty_List;
      List : constant Source_Buffer_Array := Buffer_List (This.Kernel);
   begin
      --  ??? Should we look in This.Pure_Buffers ?

      for L in List'Range loop
         Buffer_Lists.Append (Result, This.Get (Buffer => List (L)));
      end loop;

      return Result;
   end Buffers;

   --------------------------
   -- Buffer_From_Instance --
   --------------------------

   function Buffer_From_Instance
     (This       : Src_Editor_Buffer_Factory;
      Instance   : Class_Instance) return Editor_Buffer'Class
   is
      Buffer : Source_Buffer;
   begin
      Buffer := Source_Buffer (GObject'(Get_Data (Instance)));

      if Buffer = null then
         raise Editor_Exception with -"Buffer was destroyed";
      end if;

      return This.Get (Buffer);
   end Buffer_From_Instance;

   --------------------------
   -- Instance_From_Buffer --
   --------------------------

   function Instance_From_Buffer
     (Script  : access Scripting_Language_Record'Class;
      Class   : Class_Type;
      Buffer  : Editor_Buffer'Class) return Class_Instance
   is
      Inst : Class_Instance;
   begin
      if Buffer not in Src_Editor_Buffer'Class
        or else Src_Editor_Buffer (Buffer).Buffer = null
      then
         return No_Class_Instance;
      end if;

      Inst := Get_Instance (Script, Src_Editor_Buffer (Buffer).Buffer);

      if Inst = No_Class_Instance then
         Inst := New_Instance (Script, Class);
         Set_Data (Inst, GObject (Src_Editor_Buffer (Buffer).Buffer));
      end if;

      return Inst;
   end Instance_From_Buffer;

   ------------------------
   -- View_From_Instance --
   ------------------------

   function View_From_Instance
     (This       : Src_Editor_Buffer_Factory;
      Instance   : Class_Instance) return Editor_View'Class
   is
      View : Source_Editor_Box;
   begin
      View := Source_Editor_Box (GObject'(Get_Data (Instance)));

      if View = null then
         raise Editor_Exception with -"View was destroyed";
      end if;

      --  ??? We must setup callbacks so that when the editor is closed
      --  we reset the internal Box field to null to avoid errors

      return Src_Editor_View'
        (Editor_View with
         Box    => View,
         Buffer => Src_Editor_Buffer (This.Get (Get_Buffer (View))));
   end View_From_Instance;

   ------------------------
   -- Instance_From_View --
   ------------------------

   function Instance_From_View
     (Script  : access Scripting_Language_Record'Class;
      Class   : Class_Type;
      View    : Editor_View'Class) return Class_Instance
   is
      Inst : Class_Instance;
   begin
      if View not in Src_Editor_View'Class
        or else Src_Editor_View (View).Box = null
      then
         return No_Class_Instance;
      end if;

      Inst := Get_Instance (Script, Src_Editor_View (View).Box);

      if Inst = No_Class_Instance then
         Inst := New_Instance (Script, Class);
         Set_Data (Inst, GObject (Src_Editor_View (View).Box));
      end if;

      return Inst;
   end Instance_From_View;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance   : Class_Instance;
      View       : Editor_View'Class) is
   begin
      Set_Data (Instance, GObject (Src_Editor_View (View).Box));
   end Set_Data;

   ---------------------------
   -- Instance_From_Overlay --
   ---------------------------

   function Instance_From_Overlay
     (Script  : access Scripting_Language_Record'Class;
      Class   : Class_Type;
      Overlay : Editor_Overlay'Class) return Class_Instance
   is
      Inst : Class_Instance;
   begin
      if Overlay not in Src_Editor_Overlay'Class
        or else Src_Editor_Overlay (Overlay).Tag = null
      then
         return No_Class_Instance;
      end if;

      Inst := Get_Instance (Script, Src_Editor_Overlay (Overlay).Tag);

      if Inst = No_Class_Instance then
         Inst := New_Instance (Script, Class);
         Set_Data (Inst, GObject (Src_Editor_Overlay (Overlay).Tag));
      end if;

      return Inst;
   end Instance_From_Overlay;

   ---------------------------
   -- Overlay_From_Instance --
   ---------------------------

   function Overlay_From_Instance
     (Instance   : Class_Instance) return Editor_Overlay'Class
   is
      Tag : Gtk_Text_Tag;
   begin
      Tag := Gtk_Text_Tag (GObject'(Get_Data (Instance)));

      if Tag = null then
         raise Editor_Exception with -"Overlay was destroyed";
      end if;

      return Create_Editor_Overlay (Tag => Tag);
   end Overlay_From_Instance;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Prop : in out Editors_Props_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Editor_Mark'Class, Editor_Mark_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Editor_Location'Class, Editor_Location_Access);
   begin
      case Prop.Typ is
         when Marks     => Unchecked_Free (Prop.Mark);
         when Locations => Unchecked_Free (Prop.Loc);
      end case;
   end Destroy;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance   : Class_Instance;
      Class_Name : String;
      Mark       : Editor_Mark'Class) is
   begin
      Set_Data
        (Instance, Class_Name, Editors_Props_Record'
           (Typ => Marks, Mark => new Editor_Mark'Class'(Mark)));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance   : Class_Instance;
      Class_Name : String;
      Location   : Editor_Location'Class) is
   begin
      Set_Data
        (Instance, Class_Name, Editors_Props_Record'
           (Typ => Locations, Loc => new Editor_Location'Class'(Location)));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance; Class_Name : String) return Editor_Mark_Access
   is
      Props : Editors_Props;
   begin
      if Instance /= No_Class_Instance then
         Props := Editors_Props
           (Instance_Property'(Get_Data (Instance, Class_Name)));
         if Props /= null then
            return Props.Mark;
         end if;
      end if;
      return null;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance; Class_Name : String)
      return Editor_Location_Access
   is
      Props : Editors_Props;
   begin
      if Instance /= No_Class_Instance then
         Props := Editors_Props
           (Instance_Property'(Get_Data (Instance, Class_Name)));
         if Props /= null then
            return Props.Loc;
         end if;
      end if;
      return null;
   end Get_Data;

end Src_Editor_Module.Editors;
