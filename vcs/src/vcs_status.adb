-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2006                      --
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

with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.OS_Lib;           use GNAT;
with GNAT.Strings;

with GNAT.Calendar.Time_IO; use GNAT.Calendar; use GNAT.Calendar.Time_IO;

with Glib.Xml_Int;          use Glib.Xml_Int;

with GPS.Kernel.Project;    use GPS.Kernel.Project;
with Projects;              use Projects;
with Projects.Registry;     use Projects.Registry;
with Traces;                use Traces;
with VCS_View;              use VCS_View;
with XML_Parsers;

package body VCS_Status is

   Me : constant Debug_Handle := Create ("VCS_Status");

   VCS_Cache_Filename : constant String := "vcs_cache.xml";

   Valid_Delay        : constant Duration := 60.0 * 60.0;
   --  The cache status is valid for 1 hour

   function Copy (X : Line_Record) return Line_Record;
   --  Return a deep copy of X

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Cache : Status_Cache) is
   begin
      Status_Hash.Reset (Cache.T.all);
   end Clear_Cache;

   ----------
   -- Copy --
   ----------

   function Copy (X : Line_Record) return Line_Record is
   begin
      return (Copy_File_Status (X.Status), X.Log);
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Internal_Record) is
   begin
      Free (X.LR.Status);
   end Free;

   ---------------
   -- Get_Cache --
   ---------------

   function Get_Cache
     (Cache : Status_Cache;
      File  : VFS.Virtual_File) return Line_Record is
   begin
      return Status_Hash.Get (Cache.T.all, File).LR;
   end Get_Cache;

   ----------
   -- Hash --
   ----------

   function Hash (F : Virtual_File) return Header_Num is
      function Hash is new HTables.Hash (Header_Num);
   begin
      return Hash (File_Key (F));
   end Hash;

   ---------------
   -- Set_Cache --
   ---------------

   procedure Set_Cache
     (Cache  : Status_Cache;
      File   : VFS.Virtual_File;
      Status : in out Line_Record) is
   begin
      Status := Copy (Status);
      Status_Hash.Set (Cache.T.all, File, (Status, Clock));
   end Set_Cache;

   ----------------
   -- Has_Status --
   ----------------

   function Has_Status
     (Cache  : Status_Cache;
      File   : VFS.Virtual_File;
      Ref    : VCS_Access;
      Status : Status_Id) return Boolean
   is
      S : constant File_Status := Get_File_Status (Ref, Status);
      L : constant Line_Record := Status_Hash.Get (Cache.T.all, File).LR;
   begin
      if L /= No_Data
        and then L.Status.Status.Stock_Id.all = S.Stock_Id.all
      then
         return True;
      else
         return False;
      end if;
   end Has_Status;

   ----------------
   -- Save_Cache --
   ----------------

   procedure Save_Cache
     (Kernel : access Kernel_Handle_Record'Class; Cache : Status_Cache)
   is
      Now      : constant Time := Clock;
      Filename : constant String :=
                   Get_Home_Dir (Kernel) & VCS_Cache_Filename;
      File     : Node_Ptr;
      F_Child  : Node_Ptr;
      Iter     : Status_Hash.Iterator;
      Item     : Internal_Record;
      Status   : Status_Id;

   begin
      File     := new Node;
      File.Tag := new String'("custom_section");

      F_Child     := new Node;
      F_Child.Tag := new String'("vcs_cache");
      Add_Child (File, F_Child);

      --  Iterate through all stored statuses

      Status_Hash.Get_First (Cache.T.all, Iter);

      loop
         Item := Status_Hash.Get_Element (Iter);

         exit when Item = No_I_Data;

         Status := Get_File_Status_Id (Item.LR.Status.Status);

         if Now - Item.Timestamp <= Valid_Delay
           and then Status /= Unknown_Id
         then
            declare
               procedure Add_Attribute
                 (Name : String; List : String_List.List);
               --  Add attribute Name into Child with value read from List

               File  : constant Virtual_File := Item.LR.Status.File;
               Child : Node_Ptr;

               -------------------
               -- Add_Attribute --
               -------------------

               procedure Add_Attribute
                 (Name : String; List : String_List.List) is
               begin
                  if not String_List.Is_Empty (List) then
                     Set_Attribute (Child, Name, String_List.Head (List));
                  end if;
               end Add_Attribute;

            begin
               Child     := new Node;
               Child.Tag := new String'("file_status");

               Set_Attribute (Child, "file", Full_Name (File).all);
               Set_Attribute
                 (Child, "date", Image (Item.Timestamp, "%Y-%m-%d %H:%M:%S"));
               Set_Attribute (Child, "log", Boolean'Image (Item.LR.Log));
               Set_Attribute (Child, "status", Status_Id'Image (Status));
               Add_Attribute ("wrev", Item.LR.Status.Working_Revision);
               Add_Attribute ("rrev", Item.LR.Status.Repository_Revision);
               Add_Attribute ("tags", Item.LR.Status.Tags);
               Add_Attribute ("users", Item.LR.Status.Users);

               Add_Child (F_Child, Child);
            end;
         end if;

         Status_Hash.Get_Next (Cache.T.all, Iter);
      end loop;

      Print (File, Filename);
      Free (File);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Save_Cache;

   ----------------
   -- Load_Cache --
   ----------------

   procedure Load_Cache
     (Kernel : access Kernel_Handle_Record'Class; Cache : out Status_Cache)
   is

      pragma Warnings (Off, Cache);

      Now         : constant Time := Clock;
      Filename    : constant String :=
                      Get_Home_Dir (Kernel) & VCS_Cache_Filename;
      File, Child : Node_Ptr;
      Err         : Strings.String_Access;

      procedure Parse_Status (Node : Node_Ptr);
      --  Parse a cache status node

      ------------------
      -- Parse_Status --
      ------------------

      procedure Parse_Status (Node : Node_Ptr) is
         File     : constant Virtual_File :=
                      Create (Get_Attribute (Node, "file"));
         Date_Str : constant String := Get_Attribute (Node, "date");
         F        : constant Positive := Date_Str'First;
         Date     : constant Time :=
                      Time_Of
                        (Year_Number'Value (Date_Str (F .. F + 3)),
                         Month_Number'Value (Date_Str (F + 5 .. F + 6)),
                         Day_Number'Value (Date_Str (F + 8 .. F + 9)),
                         Hour_Number'Value (Date_Str (F + 11 .. F + 12)),
                         Minute_Number'Value (Date_Str (F + 14 .. F + 15)),
                         Second_Number'Value (Date_Str (F + 17 .. F + 18)));
      begin
         if Now - Date <= Valid_Delay then
            declare
               procedure Add_Attribute
                 (Into : in out String_List.List; Name : String);
               --  Add attribute value for the given Name into Into

               -------------------
               -- Add_Attribute --
               -------------------

               procedure Add_Attribute
                 (Into : in out String_List.List; Name : String)
               is
                  Value : constant String := Get_Attribute (Node, Name);
               begin
                  if Value /= "" then
                     String_List.Append (Into, Value);
                  end if;
               end Add_Attribute;

               Project : constant Project_Type :=
                           Get_Project_From_File
                             (Get_Registry (Kernel).all, File);
               VCS     : constant VCS_Access :=
                           Get_VCS_From_Id
                             (Get_Attribute_Value
                                (Project, Vcs_Kind_Attribute));
               Status  : constant Status_Id :=
                           Status_Id'Value
                             (Get_Attribute (Node, "status", "unknown_id"));
               Item    : Line_Record;
            begin
               Item.Log := Boolean'Value
                 (Get_Attribute (Node, "log", "FALSE"));
               Item.Status.File := File;
               Item.Status.Status := Get_File_Status (VCS, Status);
               Add_Attribute (Item.Status.Working_Revision, "wrev");
               Add_Attribute (Item.Status.Repository_Revision, "rrev");
               Add_Attribute (Item.Status.Tags, "tags");
               Add_Attribute (Item.Status.Users, "users");
               Set_Cache (Cache, File, Item);
            end;
         end if;
      end Parse_Status;

   begin
      if OS_Lib.Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);

         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Trace (Me, Err.all);
            OS_Lib.Free (Err);

         else
            --  Get node custom_section

            Child := File.Child;

            --  Get node activities

            Child := Child.Child;

            while Child /= null loop
               if Child.Tag.all = "file_status" then
                  Parse_Status (Child);
               else
                  Trace (Exception_Handle,
                         "Unknown activity node " & Child.Tag.all);
               end if;
               Child := Child.Next;
            end loop;

            Free (File);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Load_Cache;

end VCS_Status;
