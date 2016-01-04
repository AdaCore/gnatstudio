------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

--  This package provides a listener for editor buffers. Its purpose is to
--  track subprogram specification modifications, and to propose a
--  synchronization mechanism to the user, by the way of some graphical
--  feedback

with GPS.Editors; use GPS.Editors;
with GPS.Core_Kernels; use GPS.Core_Kernels;
with Ada.Finalization; use Ada.Finalization;
with GPS.Syntax_Tree; use GPS.Syntax_Tree;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with GNATCOLL.Tribooleans; use GNATCOLL.Tribooleans;
with GNATCOLL.VFS; use GNATCOLL.VFS;

package Spec_Sync_Listener is

   package VF_Sets is new Ada.Containers.Hashed_Sets
     (Virtual_File,
      Equivalent_Elements => "=",
      Hash => Full_Name_Hash);
   --  Used for a set of file currently being synced.

   package Comments_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String,
      Comments_Lists.List,
      Ada.Strings.Unbounded.Hash,
      Ada.Strings.Unbounded."=",
      Comments_Lists."=");
   --  Used for mapping subprogram arguments to comments
   --  when synchronizing subprogram specifications

   package Comments_Lists_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Comments_Lists.List, Comments_Lists."=");
   --  List of lists of comments, used as a set to check which comments
   --  are already used

   type Sync_State is record

      Original_Spec, -- The original spec that the user began to modify
      To_Sync_Spec, -- The original counterpart of Original_Spec
      New_Spec : Subprogram_Definition -- The new version of Original_Spec
        := Null_Subprogram_Definition;

      Need_Sync : Boolean := False;

      Comments_Map : Comments_Maps.Map;
      --  Map to keep track of arguments -> comments mapping across spec
      --  modifications

      Original_Spec_Start_Mark : access Editor_Mark'Class;
      Original_Spec_End_Mark : access Editor_Mark'Class;
      --  Bounds of the original spec

      Layout_Vertical : Triboolean := Indeterminate;
      Layout_Aligned  : Triboolean := Indeterminate;
      --  Variables to keep track of the layout of the original spec
   end record;
   --  Record representing the state of an in-process synchronization

   type Spec_Sync_Listener is new Editor_Listener with record
      Ed_Buf : access Editor_Buffer'Class;
      Factory : Editor_Buffer_Factory_Access;
      Kernel : Core_Kernel;
      State : Sync_State;
      Auto_Spec_Sync : Boolean := False;
   end record;
   --  Listener object record

   function Is_Locked (This : in out Spec_Sync_Listener) return Boolean;
   --  Returns true if the file that the listener is associated to is locked
   --  eg. if it is in the process of a spec synchronization

   overriding procedure Before_Insert_Text
     (This      : in out Spec_Sync_Listener;
      Location  : Editor_Location'Class;
      Text      : String := "";
      From_User : Boolean);

   overriding procedure Before_Delete_Range
     (This           : in out Spec_Sync_Listener;
      Start_Location : Editor_Location'Class;
      End_Location   : Editor_Location'Class;
      Offset         : Integer;
      From_User      : Boolean);

   overriding procedure After_Insert_Text
     (This            : in out Spec_Sync_Listener;
      Cursor_Location : Editor_Location'Class;
      From_User       : Boolean);

   overriding procedure After_Delete_Range
     (This            : in out Spec_Sync_Listener;
      Cursor_Location : Editor_Location'Class;
      From_User       : Boolean);

   overriding procedure After_Cursor_Moved
     (This            : in out Spec_Sync_Listener;
      Cursor_Location : Editor_Location'Class;
      From_User       : Boolean);

   procedure Sync_Specs
     (This : in out Spec_Sync_Listener);
   --  Synchronize the specs if there is reason to

   type Spec_Sync_Listener_Factory is
     new Editor_Listener_Factory with null record;

   overriding function Create
     (This : Spec_Sync_Listener_Factory;
      Editor : Editor_Buffer'Class;
      Factory : Editor_Buffer_Factory'Class;
      Kernel : Core_Kernel) return Editor_Listener_Access
   is
     (new Spec_Sync_Listener'
        (Controlled with
            Ed_Buf => new GPS.Editors.Editor_Buffer'Class'(Editor),
            Kernel => Kernel,
            Factory => new GPS.Editors.Editor_Buffer_Factory'Class'(Factory),
            Auto_Spec_Sync => False,
         State => <>));
   --  Constructor for the Listener, meant to be Gtk agnostic !

end Spec_Sync_Listener;
