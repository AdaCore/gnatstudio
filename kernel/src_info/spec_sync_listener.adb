------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.Xref; use GNATCOLL.Xref;

package body Spec_Sync_Listener is

   File_Locs : VF_Sets.Set;

   procedure Lock (VF : Virtual_File);
   procedure Unlock (VF : Virtual_File);
   function Debug_Info (Subp_Def : Subprogram_Definition) return String;
   pragma Unreferenced (Debug_Info);
   function To_String (Ed_Loc : Editor_Location'Class) return String;
   pragma Unreferenced (To_String);
   procedure Update_Specs
     (This : in out Spec_Sync_Listener;
      Location : Editor_Location'Class);

   function Is_Locked (This : in out Spec_Sync_Listener) return Boolean is
   begin
      return File_Locs.Contains (This.Ed_Buf.File);
   end Is_Locked;

   procedure Lock (VF : Virtual_File) is
   begin
      File_Locs.Include (VF);
   end Lock;

   procedure Unlock (VF : Virtual_File) is
   begin
      File_Locs.Exclude (VF);
   end Unlock;

   overriding procedure Before_Insert_Text
     (This     : in out Spec_Sync_Listener;
      Location : Editor_Location'Class;
      Text     : String := "";
      From_User      : Boolean) is
   begin
      pragma Unreferenced (Text);
      pragma Unreferenced (From_User);
      if This.Is_Locked then
         return;
      end if;
      Update_Specs (This, Location);
   end Before_Insert_Text;

   function Str (Loc : Editor_Location'Class) return String
   is
     ("<" & String (Loc.Buffer.File.Full_Name.all) & "," &
        Loc.Line'Img & "," & Loc.Column'Img & ">");

   procedure Sync_Specs
     (This : in out Spec_Sync_Listener)
   is
      Model_Spec : constant Subprogram_Definition := This.State.New_Spec;
      To_Sync_Spec : Subprogram_Definition renames This.State.To_Sync_Spec;
      Mark : constant Editor_Mark'Class :=
        This.Ed_Buf.Current_View.Cursor.Create_Mark;
   begin
      if not This.State.Need_Sync then
         return;
      end if;

      if This.Is_Locked then
         return;
      else
         Lock (Model_Spec.Buffer.File);
         Lock (To_Sync_Spec.Buffer.File);
      end if;

      This.Ed_Buf.Start_Undo_Group;

      if To_Sync_Spec.Kind = Kind_Function then
         Copy_Structure (Model_Spec.Spec.Ret_Type.Contents.all,
                         To_Sync_Spec.Spec.Ret_Type.Contents.all);
         To_Sync_Spec.Spec.Ret_Type.Contents.Hard_Replace;
      end if;

      for Arg of Model_Spec.Spec.Args loop
         Arg.Comments := Comments_Lists.Empty_List;
      end loop;

      if This.State.Layout_Vertical = Indeterminate then
         This.State.Layout_Vertical := To_TriBoolean
           (To_Sync_Spec.Spec.Is_Args_Layout_Vertical);
      end if;

      if This.State.Layout_Aligned = Indeterminate then
         This.State.Layout_Aligned := To_TriBoolean
           (To_Sync_Spec.Spec.Is_Args_Layout_Aligned);
      end if;

      declare
         Args : Args_Lists.List renames To_Sync_Spec.Spec.Args;
         Start_Loc : constant Editor_Location'Class
           := To_Sync_Spec.Spec.Start_Loc.all;
         Used_Comments : Comments_Lists_Lists.List;
         use Args_Lists;
         use Ada.Containers;
      begin
         --  Delete everything between parens
         To_Sync_Spec.Buffer.Delete
           (Start_Loc, To_Sync_Spec.Spec.Args_Profile_End_Loc.all);

         --  Saving the comments of the old spec into the comments map
         for Arg of Args loop
            if not Arg.Comments.Is_Empty then
               This.State.Comments_Map.Include
                 (To_Unbounded_String (Arg.Unparse), Arg.Comments);
            end if;
         end loop;

         --  Adding the comments from the comments map into the new spec
         for Arg of Model_Spec.Spec.Args loop
            declare
               Arg_Unparse : constant Unbounded_String :=
                 To_Unbounded_String (Arg.Unparse);
            begin
               if Arg.Comments.Is_Empty
                 and then This.State.Comments_Map.Contains (Arg_Unparse)
               then
                  Arg.Comments :=
                    This.State.Comments_Map.Element (Arg_Unparse);
                  Used_Comments.Append (Arg.Comments);
               end if;
            end;
         end loop;

         if
           Model_Spec.Spec.Args.Length = To_Sync_Spec.Spec.Args.Length
         then
            declare
               Old_Spec_Cursor : Args_Lists.Cursor
                 := To_Sync_Spec.Spec.Args.First;
            begin
               for New_Arg of Model_Spec.Spec.Args loop
                  if not Element (Old_Spec_Cursor).Comments.Is_Empty
                    and then not Used_Comments.Contains
                      (Element (Old_Spec_Cursor).Comments)
                  then
                     New_Arg.Comments :=
                       Element (Old_Spec_Cursor).Comments;
                  end if;
                  Next (Old_Spec_Cursor);
               end loop;
            end;
         end if;

         --  Insert the new spec
         declare
            Spec : constant String := Model_Spec.Spec.Unparse_Spec
              (Vertical => To_Boolean (This.State.Layout_Vertical),
               Align_Params => To_Boolean (This.State.Layout_Aligned));
         begin
            To_Sync_Spec.Buffer.Insert (Start_Loc, Spec);
         end;

         --  Replace the procedure name
         Copy_Structure (Model_Spec.Name, To_Sync_Spec.Name);
         To_Sync_Spec.Name.Hard_Replace;
      end;

      declare
         New_Spec : constant Subprogram_Definition :=
           Get_Subprogram_At
             (To_Sync_Spec.Buffer, This.Kernel, To_Sync_Spec.Start_Loc.all);
      begin
         New_Spec.Start_Loc.Buffer.Indent
           (New_Spec.Start_Loc.all, New_Spec.End_Loc.all);
         This.State.To_Sync_Spec := Get_Subprogram_At
           (To_Sync_Spec.Buffer, This.Kernel, To_Sync_Spec.Start_Loc.all);
         This.State.Original_Spec := This.State.New_Spec;
      end;

      This.Ed_Buf.Current_View.Cursor_Goto (Mark.Location);
      This.State.Need_Sync := False;
      Mark.Delete;

      Unlock (Model_Spec.Buffer.File);
      Unlock (To_Sync_Spec.Buffer.File);

      This.Ed_Buf.Finish_Undo_Group;

--     exception
--        when others =>
--           Put_Line ("Exception happenned while syncing program specs");
--           This.Update_Specs_Barrier := False;
--           This.State.Need_Sync := False;

   end Sync_Specs;

   function Debug_Info (Subp_Def : Subprogram_Definition) return String
   is
   begin
      return "< Subprogram_Def at "
        & Str (Subp_Def.Start_Loc.all)
        & " with profile : " & Unparse (Subp_Def) & ">";
   end Debug_Info;

   procedure Update_Specs
     (This : in out Spec_Sync_Listener;
      Location : Editor_Location'Class)
   is
      Subp_Def : constant Subprogram_Definition
        := Get_Subprogram_At (This.Ed_Buf.all, This.Kernel, Location);
      Counterpart : Subprogram_Definition;
      use Args_Lists;
   begin
      if Subp_Def /= Null_Subprogram_Definition then

         Put_Line ("SUBP DEF => " & Unparse (Subp_Def));

         --  Exit early if we are not in the spec part at all
         if not Subp_Def.Contains (Location) then
            This.State := (others => <>);
            return;
         end if;

         Counterpart :=
           Subprogram_Definition (Subp_Def.Get_Counterpart (This.Factory));
      end if;

      if Subp_Def /= Null_Subprogram_Definition
        and then Counterpart /= Null_Subprogram_Definition
      then
         if This.State.Original_Spec = Null_Subprogram_Definition then
            This.State.Original_Spec := Subp_Def;
            This.State.To_Sync_Spec := Subprogram_Definition
              (Subp_Def.Get_Counterpart (This.Factory));

            This.State.Original_Spec_Start_Mark := new Editor_Mark'Class'
              (Subp_Def.Start_Loc.Create_Mark);
            This.State.Original_Spec_End_Mark := new Editor_Mark'Class'
              (Subp_Def.End_Loc.Create_Mark);
         elsif
           This.State.Original_Spec_Start_Mark.Location =
             Subp_Def.Start_Loc.all
         then
            if This.State.To_Sync_Spec /= Subp_Def then
               --  The spec has changed
               This.State.New_Spec := Subp_Def;

               --  Ultimately this is where you put the GUI notification
               --  indicating to the user that the spec has changed
               This.State.Need_Sync := True;
            end if;

         else
            --  The spec is another one
            --  TODO : Instanciate a f***in Unchecked_Deallocation here
            This.State := (others => <>);
            This.State.Original_Spec := Subp_Def;
            This.State.To_Sync_Spec := Subprogram_Definition
              (Subp_Def.Get_Counterpart (This.Factory));
            This.State.Original_Spec_Start_Mark := new Editor_Mark'Class'
              (Subp_Def.Start_Loc.Create_Mark);
            This.State.Original_Spec_End_Mark := new Editor_Mark'Class'
              (Subp_Def.End_Loc.Create_Mark);
         end if;
      else
         if not (This.State.Original_Spec /= Null_Subprogram_Definition
                 and then Contains
                   (This.State.Original_Spec_Start_Mark.Location,
                    This.State.Original_Spec_End_Mark.Location,
                    Location))
         then
            This.State := (others => <>);
         end if;
      end if;
   end Update_Specs;

   overriding procedure Before_Delete_Range
     (This           : in out Spec_Sync_Listener;
      Start_Location : Editor_Location'Class;
      End_Location   : Editor_Location'Class;
      Offset         : Integer;
      From_User      : Boolean)
   is
      pragma Unreferenced (Start_Location);
      pragma Unreferenced (Offset);
   begin
      if This.Is_Locked
        or else not From_User
      then
         return;
      end if;
      Update_Specs (This, End_Location);
   end Before_Delete_Range;

   overriding procedure After_Insert_Text
     (This : in out Spec_Sync_Listener;
      Cursor_Location : Editor_Location'Class;
      From_User      : Boolean) is
   begin
      if This.Is_Locked
        or else not From_User
      then
         return;
      end if;

      Update_Specs (This, Cursor_Location);
      This.Sync_Specs;
   end After_Insert_Text;

   overriding procedure After_Delete_Range
     (This : in out Spec_Sync_Listener;
      Cursor_Location : Editor_Location'Class;
      From_User      : Boolean) is
   begin
      if This.Is_Locked
        or else not From_User
      then
         return;
      end if;
      Update_Specs (This, Cursor_Location);
      This.Sync_Specs;
   end After_Delete_Range;

   function To_String (Ed_Loc : Editor_Location'Class) return String is
   begin
      return "<Line : " & Ed_Loc.Line'Img
        & ", Column : " & Ed_Loc.Column'Img & ">";
   end To_String;

   overriding procedure After_Cursor_Moved
     (This            : in out Spec_Sync_Listener;
      Cursor_Location : Editor_Location'Class;
      From_User      : Boolean) is
   begin
      if This.Is_Locked
        or else not From_User
      then
         return;
      end if;
      Update_Specs (This, Cursor_Location);
   end After_Cursor_Moved;

end Spec_Sync_Listener;
