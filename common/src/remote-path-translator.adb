-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;
pragma Warnings (On);

with Glib;         use Glib;
with Glib.Xml_Int; use Glib.Xml_Int;

with Filesystem;   use Filesystem;
with Traces;       use Traces;

package body Remote.Path.Translator is

   Me : constant Debug_Handle := Create ("Paths_Translator");

   use Mirror_List;

   ------------------
   -- Mirror_Paths --
   ------------------

   type Mirrors_List_Record;
   type Mirrors_List_Access is access all Mirrors_List_Record;
   type Mirrors_List_Record is record
      Nickname : Glib.String_Ptr;
      List     : Mirror_List_Access;
      Next     : Mirrors_List_Access;
   end record;

   Main_Paths_Table : Mirrors_List_Access := null;
   --  ??? Global variable, should get rid of it

   --------------
   -- Get_List --
   --------------

   function Get_List (Server : Server_Type) return Mirror_List_Access is
      Name : constant String := Get_Nickname (Server);
   begin
      return Get_List (Name);
   end Get_List;

   --------------
   -- Get_List --
   --------------

   function Get_List (Server : String) return Mirror_List_Access
   is
      List : Mirrors_List_Access := Main_Paths_Table;
   begin
      if Server = "" then
         return null;
      end if;

      while List /= null loop
         exit when List.Nickname.all = Server;
         List := List.Next;
      end loop;

      if List /= null then
         return List.List;
      end if;

      List := new Mirrors_List_Record'
        (Nickname => new String'(Server),
         List     => new Mirror_List.List,
         Next     => Main_Paths_Table);
      Main_Paths_Table := List;

      return List.List;
   end Get_List;

   ----------------------------
   -- Parse_Remote_Path_Node --
   ----------------------------

   procedure Parse_Remote_Path_Node
     (Node      : Glib.Xml_Int.Node_Ptr)
   is
      Nickname : constant String := Get_Attribute (Node, "server_name");
      List     : constant Mirror_List_Access := Get_List (Nickname);
      Path     : Mirror_Path;
      Child    : Node_Ptr := Node.Child;

   begin
      while Child /= null loop
         declare
            Local_Path  : constant String :=
                            Get_Attribute (Child, "local_path");
            Remote_Path : constant String :=
                            Get_Attribute (Child, "remote_path");
            Sync_Str    : constant String :=
                            Get_Attribute (Child, "sync", "never");
            Sync        : Synchronisation_Type;
         begin

            --  Retrieve Sync value from string.
            begin
               Sync := Synchronisation_Type'Value (Sync_Str);
            exception
               when Constraint_Error =>
                  Sync := Never;
            end;

            Path.Init (Local_Path      => Local_Path,
                       Remote_Path     => Remote_Path,
                       Synchronisation => Sync);
            List.Append (Path);
         end;

         Child := Child.Next;
      end loop;
   end Parse_Remote_Path_Node;

   ---------------------------
   -- Save_Remote_Path_Node --
   ---------------------------

   procedure Save_Remote_Path_Node
     (Server : String;
      Node   : in out Glib.Xml_Int.Node_Ptr)
   is
      List   : constant Mirror_List_Access := Get_List (Server);
      Cursor : Mirror_List.Cursor;
      Child  : Node_Ptr;
      Path   : Mirror_Path;
   begin
      Cursor := First (List.all);

      while Mirror_List.Has_Element (Cursor) loop
         Path := Mirror_List.Element (Cursor);

         if Path /= Null_Path then
            Child := new Glib.Xml_Int.Node;
            Child.Tag := new String'("mirror_path");
            Set_Attribute
              (Child, "sync",
               Synchronisation_Type'Image (Path.Get_Synchronisation));
            Set_Attribute (Child, "remote_path", Path.Get_Remote_Path);
            Set_Attribute (Child, "local_path", Path.Get_Local_Path);
            Add_Child (Node, Child, True);
         end if;

         Mirror_List.Next (Cursor);
      end loop;
   end Save_Remote_Path_Node;

   ------------------------
   -- To_Remote_Possible --
   ------------------------

   function To_Remote_Possible
     (Path     : String;
      To       : String) return Boolean
   is
      List   : constant Mirror_List_Access := Get_List (To);
      Cursor : Mirror_List.Cursor;
      Mirror : Mirror_Path;

   begin
      if To = "" then
         return True;
      elsif List.Is_Empty then
         return False;
      end if;

      Cursor := First (List.all);

      while Mirror_List.Has_Element (Cursor) loop
         Mirror := Mirror_List.Element (Cursor);

         if Get_Local_Filesystem.Is_Subtree (Mirror.Get_Local_Path, Path) then
            return True;
         end if;

         Cursor := Mirror_List.Next (Cursor);
      end loop;

      return False;
   end To_Remote_Possible;

   -----------------------
   -- To_Local_Possible --
   -----------------------

   function To_Local_Possible
     (Path : String;
      From : String) return Boolean
   is
      List   : constant Mirror_List_Access := Get_List (From);
      Cursor : Mirror_List.Cursor;
      Mirror : Mirror_Path;

   begin
      if From = "" then
         return True;
      elsif List.Is_Empty then
         return False;
      end if;

      Cursor := First (List.all);

      while Mirror_List.Has_Element (Cursor) loop
         Mirror := Mirror_List.Element (Cursor);

         if Get_Filesystem (From).Is_Subtree
           (Mirror.Get_Remote_Path, Path)
         then
            return True;
         end if;

         Mirror_List.Next (Cursor);
      end loop;

      return False;
   end To_Local_Possible;

   ---------------
   -- To_Remote --
   ---------------

   function To_Remote
     (Path       : String;
      To         : Server_Type;
      Unix_Style : Boolean := False) return String is
   begin
      return To_Remote (Path, Get_Nickname (To), Unix_Style);
   end To_Remote;

   ---------------
   -- To_Remote --
   ---------------

   function To_Remote
     (Path       : String;
      To         : String;
      Unix_Style : Boolean := False) return String
   is
      List      : constant Mirror_List_Access := Get_List (To);
      Cursor    : Mirror_List.Cursor;
      Mirror    : Mirror_Path := Null_Path;

   begin
      --  If From and To are the same machine (and no unix path translation is
      --  needed), just return Path

      if To = ""  or else List.Is_Empty then
         if Unix_Style then
            return Get_Local_Filesystem.To_Unix (Path);
         else
            return Path;
         end if;
      end if;

      Cursor := First (List.all);

      while Mirror_List.Has_Element (Cursor) loop
         Mirror := Mirror_List.Element (Cursor);

         exit when
           Get_Local_Filesystem.Is_Subtree (Mirror.Get_Local_Path, Path);

         Mirror_List.Next (Cursor);
      end loop;

      if not Mirror_List.Has_Element (Cursor) then
         --  Not configured mirror path
         Mirror := Null_Path;
      end if;

      declare
         Path_From  : constant String := Mirror.Get_Local_Path;
         Path_To    : constant String := Mirror.Get_Remote_Path;
         To_Filesystem : Filesystem_Record'Class := Get_Filesystem (To);
         U_Path     : constant String := To_Unix (Get_Local_Filesystem, Path);
         --  The input path in unix style
         U_Frompath : constant String := To_Unix (Get_Local_Filesystem,
                                                  Path_From);
         --  The local root dir, in unix style
         U_Subpath  : constant String :=
           U_Path (U_Path'First + U_Frompath'Length .. U_Path'Last);
      begin

         --  At this point, we have the from and to moint points. Let's
         --  translate the path

         if Unix_Style then
            return To_Unix (To_Filesystem, Path_To) & U_Subpath;

         else
            return Concat (To_Filesystem,
                           Path_To,
                           From_Unix (To_Filesystem, U_Subpath));
         end if;
      end;
   end To_Remote;

   --------------
   -- To_Local --
   --------------

   function To_Local (Path : String; From : Server_Type) return String is
   begin
      return To_Local (Path, Get_Nickname (From));
   end To_Local;

   --------------
   -- To_Local --
   --------------

   function To_Local (Path : String; From : String) return String is
      List      : constant Mirror_List_Access := Get_List (From);
      Cursor    : Mirror_List.Cursor;
      Mirror    : Mirror_Path := Null_Path;

   begin
      --  If From and To are the same machine, just return Path

      if From = "" then
         return Path;
      end if;

      if Active (Me) then
         Trace (Me, "To_Local: " & Path & " on server " & From);
      end if;

      --  Search for mirror path in 'From' config

      Cursor := First (List.all);

      while Has_Element (Cursor) loop
         Mirror := Mirror_List.Element (Cursor);

         exit when Is_Subtree
           (Get_Filesystem (From), Mirror.Get_Remote_Path, Path);

         Mirror_List.Next (Cursor);
      end loop;

      if not Has_Element (Cursor) then
         --  Not configured mirror path.
         Mirror := Null_Path;
      end if;

      --  At this point, we have the from and to moint points. Let's translate
      --  the path

      declare
         Path_From  : constant String := Mirror.Get_Remote_Path;
         Path_To    : constant String := Mirror.Get_Local_Path;
         FS         : Filesystem_Record'Class := Get_Filesystem (From);
         U_Path     : constant String := To_Unix (FS, Path);
         --  The input path in unix style
         U_Frompath : constant String := To_Unix (FS, Path_From);
         --  The local root dir, in unix style
         U_Subpath  : constant String :=
           U_Path (U_Path'First + U_Frompath'Length .. U_Path'Last);

      begin
         return Concat
           (Get_Local_Filesystem,
            Path_To,
            From_Unix (Get_Local_Filesystem, U_Subpath));
      end;
   end To_Local;

   ------------------
   -- To_Unix_Path --
   ------------------

   function To_Unix_Path
     (Path       : String;
      Server     : Server_Type;
      Use_Cygwin : Boolean := False) return String is
   begin
      return Get_Filesystem (Get_Nickname (Server)).To_Unix (Path, Use_Cygwin);
   end To_Unix_Path;

end Remote.Path.Translator;
