-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2009, AdaCore                        --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Gtk.Text_Mark;   use Gtk.Text_Mark;

with Entities;         use Entities;
with Entities.Queries; use Entities.Queries;

with GPS.Kernel;       use GPS.Kernel;

with GUI_Utils;       use GUI_Utils;
with Src_Editor_Box;   use Src_Editor_Box;

package body Src_Editor_Buffer.Hyper_Mode is

   -----------------------------
   -- Hyper_Mode_Highlight_On --
   -----------------------------

   procedure Hyper_Mode_Highlight_On
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter)
   is
      Entity_Start, Entity_End : Gtk_Text_Iter;
   begin
      --  Remove the previous highlight
      Remove_Highlight (Buffer);

      --  Get the current word / entity

      --  ??? if we need to support special regexps, add support here

      Copy (Iter, Entity_Start);

      Search_Entity_Bounds (Entity_Start, Entity_End);

      if Buffer.Hyper_Mode_Highlight_Begin = null then
         Buffer.Hyper_Mode_Highlight_Begin :=
           Create_Mark (Buffer, Where => Entity_Start);
      else
         Move_Mark (Buffer, Buffer.Hyper_Mode_Highlight_Begin, Entity_Start);
      end if;

      if Buffer.Hyper_Mode_Highlight_End = null then
         Buffer.Hyper_Mode_Highlight_End :=
           Create_Mark (Buffer, Where => Entity_End);
      else
         Move_Mark (Buffer, Buffer.Hyper_Mode_Highlight_End, Entity_End);
      end if;

      Buffer.Hyper_Mode_Has_Highlight := True;

      --  Highlight the section that we want to highlight
      Apply_Tag (Buffer, Buffer.Hyper_Mode_Tag, Entity_Start, Entity_End);
   end Hyper_Mode_Highlight_On;

   -------------------------
   -- Hyper_Mode_Click_On --
   -------------------------

   procedure Hyper_Mode_Click_On
     (Buffer    : Source_Buffer;
      Alternate : Boolean := False)
   is
      Entity_Start,
      Entity_End     : Gtk_Text_Iter;
      Line           : Editable_Line_Type;
      Column         : Visible_Column_Type;

      Entity         : Entity_Information;
      Closest        : Entity_Reference;
      Status         : Find_Decl_Or_Body_Query_Status;

      Location       : File_Location;
      Current        : File_Location;

      use type GNATCOLL.VFS.Virtual_File;
   begin
      if not Buffer.Hyper_Mode_Has_Highlight then
         --  If we are not highlighting anything, clicking should do nothing
         return;
      end if;

      Get_Iter_At_Mark
        (Buffer, Entity_Start, Buffer.Hyper_Mode_Highlight_Begin);
      Get_Iter_At_Mark
        (Buffer, Entity_End, Buffer.Hyper_Mode_Highlight_End);

      Get_Iter_Position (Buffer, Entity_Start, Line, Column);

      Find_Declaration_Or_Overloaded
        (Kernel            => Buffer.Kernel,
         File              => Get_Or_Create
           (Db        => Get_Database (Buffer.Kernel),
            File      => Buffer.Filename),
         Entity_Name       => Get_Slice (Buffer, Entity_Start, Entity_End),
         Line              => Integer (Line),
         Column            => Column,
         Ask_If_Overloaded => False,
         Entity            => Entity,
         Closest_Ref       => Closest,
         Status            => Status);

      case Status is
         when Entity_Not_Found | Internal_Error =>
            return;
         when Fuzzy_Match
            | Success
            | No_Body_Entity_Found
            | Overloaded_Entity_Found =>

            Location := Get_Declaration_Of (Entity);

            if Alternate
              or else
                (Get_Line (Location) = Natural (Line)
                 and then Get_Column (Location) = Column
                 and then Get_Filename (Get_File (Location)) = Buffer.Filename)
            then
               --  We asked for the alternate behavior, or we are already on
               --  the spec: in this case, go to the body
               Current := Location;
               Find_Next_Body (Entity, Current, Location);
               if Location = No_File_Location then
                  Location := Current;
               end if;
            end if;

            Go_To_Closest_Match
              (Buffer.Kernel,
               Get_Filename (Get_File (Location)),
               Editable_Line_Type (Get_Line (Location)),
               Get_Column (Location),
               Entity);
      end case;
   end Hyper_Mode_Click_On;

   ----------------------
   -- Hyper_Mode_Enter --
   ----------------------

   procedure Hyper_Mode_Enter (Buffer : Source_Buffer) is
   begin
      if Buffer.Hyper_Mode then
         return;
      end if;

      Buffer.Hyper_Mode := True;
   end Hyper_Mode_Enter;

   ----------------------
   -- Remove_Highlight --
   ----------------------

   procedure Remove_Highlight (Buffer : Source_Buffer) is
      Entity_Start, Entity_End : Gtk_Text_Iter;
   begin
      if not Buffer.Hyper_Mode_Has_Highlight then
         return;
      end if;

      if Buffer.Hyper_Mode_Highlight_Begin /= null then
         Get_Iter_At_Mark
           (Buffer, Entity_Start, Buffer.Hyper_Mode_Highlight_Begin);
         Get_Iter_At_Mark
           (Buffer, Entity_End, Buffer.Hyper_Mode_Highlight_End);
         Remove_Tag (Buffer, Buffer.Hyper_Mode_Tag, Entity_Start, Entity_End);
      end if;

      Buffer.Hyper_Mode_Has_Highlight := False;
   end Remove_Highlight;

   ----------------------
   -- Hyper_Mode_Leave --
   ----------------------

   procedure Hyper_Mode_Leave (Buffer : Source_Buffer) is
   begin
      if not Buffer.Hyper_Mode then
         return;
      end if;

      Buffer.Hyper_Mode := False;

      --  Clean up the tag
      Remove_Highlight (Buffer);
   end Hyper_Mode_Leave;

end Src_Editor_Buffer.Hyper_Mode;
