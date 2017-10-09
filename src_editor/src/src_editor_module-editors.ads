------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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

with GPS.Editors;       use GPS.Editors;
with Src_Editor_Buffer; use Src_Editor_Buffer;

package Src_Editor_Module.Editors is

   type Src_Editor_Buffer_Factory is new GPS.Editors.Editor_Buffer_Factory
   with private;

   function Create (Kernel : Kernel_Handle) return Src_Editor_Buffer_Factory;
   --  Constructor

   procedure Destroy (X : in out Src_Editor_Buffer_Factory);
   --  Destructor

   overriding function Get
     (This        : Src_Editor_Buffer_Factory;
      File        : Virtual_File;
      Force       : Boolean := False;
      Open_Buffer : Boolean := False;
      Open_View   : Boolean := True;
      Focus       : Boolean := True) return Editor_Buffer'Class;
   overriding function Get_New
     (This : Src_Editor_Buffer_Factory) return Editor_Buffer'Class;
   overriding function New_Mark
     (This   : Src_Editor_Buffer_Factory;
      File   : Virtual_File := No_File;
      Line   : Integer;
      Column : Integer) return Editor_Mark'Class;
   overriding function Buffers
     (This   : Src_Editor_Buffer_Factory) return Buffer_Lists.List;
   overriding function Create_Marker
     (This    : Src_Editor_Buffer_Factory;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type := GNATCOLL.Projects.No_Project;
      Line    : Editable_Line_Type;
      Column  : Visible_Column_Type;
      Length  : Natural := 0) return Location_Marker;

   function Get
     (This   : Src_Editor_Buffer_Factory'Class;
      Buffer : access Source_Buffer_Record'Class)
      return Editor_Buffer'Class;
   --  Wrap a gtk+ buffer into an abstract representation. If Buffer is null,
   --  Nil_Editor_Buffer is returned

   ---------------
   -- Scripting --
   ---------------
   --  The following functions interface with scripting languages.
   --   * a Gtk_Text_Buffer (or Src_Editor_Buffer.Source_Buffer) contains user
   --     data pointing to the one instance in the various scripting languages
   --     that represent them.
   --   * A class_instance contains a pointer to the corresponding
   --     Gtk_Text_Buffer.
   --     These first two elements are automatically handled by GNATCOLL, with
   --     proper refcounting
   --   * An Editor_Buffer wrapper contains a pointer to Gtk_Text_Buffer. We
   --     can have multiple such wrappers for a given Gtk_Text_Buffer without
   --     trouble since there is no specific data associated with them. When
   --     the editor is closed, the wrapper becomes unusable.
   --   * An Editor_Mark remains valid even when the editor is closed
   --
   --  The functions *_From_Instance return a temporary wrapper around the gtk+
   --  object. They raise an Editor_Exception if no object is stored in the
   --  instance.
   --  The functions Instance_From_* create a permanent class_instance
   --  for the gtk+ object, that will always be reused as long as this object
   --  exists.
   --  The functions Set_Data are low-level, and do not need to be used in
   --  general. They will override systematically the view stored in a class
   --  instance, thus breaking any instance that might have been set before

   function Instance_From_Buffer
     (Script  : access Scripting_Language_Record'Class;
      Class   : Class_Type;
      Buffer  : Editor_Buffer'Class) return Class_Instance;
   overriding function Buffer_From_Instance
     (This       : Src_Editor_Buffer_Factory;
      Instance   : Class_Instance) return Editor_Buffer'Class;
   --  See comment above

   function Instance_From_View
     (Script  : access Scripting_Language_Record'Class;
      Class   : Class_Type;
      View    : Editor_View'Class) return Class_Instance;
   function View_From_Instance
     (This       : Src_Editor_Buffer_Factory;
      Instance   : Class_Instance) return Editor_View'Class;
   procedure Set_Data
     (Instance   : Class_Instance;
      View       : Editor_View'Class);
   --  See comment above

   function Instance_From_Overlay
     (Script  : access Scripting_Language_Record'Class;
      Class   : Class_Type;
      Overlay : Editor_Overlay'Class) return Class_Instance;
   function Overlay_From_Instance
     (Instance   : Class_Instance) return Editor_Overlay'Class;
   --  See comment above

   function Instance_From_Mark
     (Script  : access Scripting_Language_Record'Class;
      Mark    : Editor_Mark'Class) return Class_Instance;
   function Mark_From_Instance
     (This     : Src_Editor_Buffer_Factory;
      Instance : Class_Instance) return Editor_Mark'Class;
   --  See comment above

   type Editor_Location_Access is access all Editor_Location'Class;
   --  For efficiency reasons, the subprograms below return access to the
   --  actual values. This also makes it more flexible since we can have
   --  procedures returning these as out parameters, instead of systematically
   --  functions.
   --  You never need to deallocate these.

   procedure Set_Data
     (Instance   : Class_Instance;
      Class_Name : String;
      Location   : Editor_Location'Class);
   function Get_Data
     (Instance   : Class_Instance;
      Class_Name : String) return Editor_Location_Access;

private

   type Element is record
      Buf : Src_Editor_Buffer.Source_Buffer;
   end record;

   procedure Free (X : in out Element);

   No_Element : constant Element := (Buf => null);

   package Pure_Editors_Hash is new HTables.Simple_HTable
     (Header_Num   => Header_Num,
      Element      => Element,
      Free_Element => Free,
      No_Element   => No_Element,
      Key          => Virtual_File,
      Hash         => Hash,
      Equal        => Equal);
   --  ??? This is only updated for views created through this package, not
   --  any other mean

   type Table_Access is access Pure_Editors_Hash.Instance;

   type Src_Editor_Buffer_Factory is new GPS.Editors.Editor_Buffer_Factory
   with record
      Kernel : Kernel_Handle;

      Pure_Buffers : Table_Access;
      --  Pure_Buffers are editors which are not realized to an MDI widget.
      --  This is used to support editor APIs without having to create
      --  corresponding widgets.
   end record;

end Src_Editor_Module.Editors;
