-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;                use GNAT;
with GNAT.Strings;
with GNAT.HTable;
with GNAT.Calendar.Time_IO;      use GNAT.Calendar.Time_IO;

with Glib.Xml_Int;               use Glib.Xml_Int;

with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with Log_Utils;                  use Log_Utils;
with Projects;                   use Projects;
with Projects.Registry;          use Projects.Registry;
with String_Hash;
with Traces;                     use Traces;
with VCS.Unknown_VCS;            use VCS.Unknown_VCS;
with VCS_View;                   use VCS_View;
with VCS_View_API;               use VCS_View_API;
with XML_Parsers;

package body VCS_Activities is

   Me : constant Debug_Handle := Create ("VCS_Activities");

   Activities_Filename : constant String := "activities.xml";

   Item_Tag            : constant String := "@TAG@";
   --  Used as the data for file keys

   procedure Unchecked_Free is
     new Ada.Unchecked_Deallocation (String, String_Access);

   package Key_Hash is new String_Hash (String_Access, Unchecked_Free, null);
   use Key_Hash;
   type Key_Hash_Access is access String_Hash_Table.HTable;

   type Activity_Record is record
      Project      : Virtual_File;
      Name         : String_Access;
      Instance     : Class_Instance;
      Id           : Activity_Id;
      VCS          : VCS_Access;
      Group_Commit : Boolean := False;
      Closed       : Boolean := False;
      Keys         : Key_Hash_Access;
      Files        : String_List.List;
      Sorted       : Boolean;
   end record;

   Empty_Activity : constant Activity_Record :=
                      (No_File, null, No_Class_Instance, No_Activity,
                       null, False, False, null, String_List.Null_List, False);

   subtype Hash_Header is Positive range 1 .. 123;

   function Hash (F : Activity_Id) return Hash_Header;

   package Activity_Table is new GNAT.HTable.Simple_HTable
     (Hash_Header, Activity_Record, Empty_Activity, Activity_Id, Hash, "=");
   use Activity_Table;

   function Hash is new HTable.Hash (Hash_Header);

   ----------
   -- Hash --
   ----------

   function Hash (F : Activity_Id) return Hash_Header is
   begin
      return Hash (String (F));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Activity : Activity_Id) return String is
   begin
      return String (Activity);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Str : String) return Activity_Id is
   begin
      if Str'Length = Activity_Id'Length
        and then Get (Activity_Id (Str)) /= Empty_Activity
      then
         return Activity_Id (Str);
      else
         return No_Activity;
      end if;
   end Value;

   ---------------------
   -- Load_Activities --
   ---------------------

   procedure Load_Activities (Kernel : access Kernel_Handle_Record'Class) is

      Filename : constant String :=
                   Get_Home_Dir (Kernel) & Activities_Filename;

      procedure Parse_Activity (Node : Node_Ptr);
      --  Parse an activity node

      --------------------
      -- Parse_Activity --
      --------------------

      procedure Parse_Activity (Node : Node_Ptr) is
         Id           : constant String := Get_Attribute (Node, "id");
         Project      : constant String := Get_Attribute (Node, "project");
         Name         : constant String := Get_Attribute (Node, "name");
         Group_Commit : constant Boolean :=
                          Boolean'Value
                            (Get_Attribute (Node, "group_commit", "false"));
         Committed    : constant Boolean :=
                          Boolean'Value
                            (Get_Attribute (Node, "committed", "false"));
         --  For compatibility with GPS version 3.2, the name has been renamed
         --  closed starting with GPS 4.0.
         Closed       : constant Boolean :=
                          Boolean'Value
                            (Get_Attribute (Node, "closed", "false"));
         Child        : Node_Ptr := Node.Child;
         Item         : Activity_Record;
      begin
         Item := (Create (Project),
                  new String'(Name), No_Class_Instance, Activity_Id (Id),
                  null, Group_Commit,
                  Committed or Closed,
                  new String_Hash_Table.HTable, String_List.Null_List, False);

         while Child /= null loop
            if Child.Tag.all = "file" then
               declare
                  File : constant Virtual_File := Create (Child.Value.all);
               begin
                  String_Hash_Table.Set
                    (Item.Keys.all, File_Key (File), new String'(Item_Tag));
                  String_List.Append (Item.Files, Child.Value.all);
                  --  Note that here we can't use Add_File. At this point the
                  --  project is not yet loaded and we can't compute the VCS
                  --  for each activities.
               end;
            end if;
            Child := Child.Next;
         end loop;

         Set (Activity_Id (Id), Item);
      end Parse_Activity;

      File, Child : Node_Ptr;
      Err         : Strings.String_Access;

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
               if Child.Tag.all = "activity" then
                  Parse_Activity (Child);
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
      when E : others => Trace (Exception_Handle, E);
   end Load_Activities;

   ---------------------
   -- Save_Activities --
   ---------------------

   procedure Save_Activities (Kernel : access Kernel_Handle_Record'Class) is

      Filename        : constant String :=
                          Get_Home_Dir (Kernel) & Activities_Filename;
      File, Ada_Child : Node_Ptr;
      Child, F_Child  : Node_Ptr;
      F_Iter          : String_List.List_Node;
      Item            : Activity_Record;
      Success         : Boolean;

   begin
      File     := new Node;
      File.Tag := new String'("custom_section");

      Ada_Child     := new Node;
      Ada_Child.Tag := new String'("activities");
      Add_Child (File, Ada_Child);

      Item := Get_First;

      while Item /= Empty_Activity loop
         Child     := new Node;
         Child.Tag := new String'("activity");

         Set_Attribute (Child, "name", Item.Name.all);
         Set_Attribute (Child, "project", Full_Name (Item.Project).all);
         Set_Attribute (Child, "id", String (Item.Id));
         Set_Attribute
           (Child, "group_commit", Boolean'Image (Item.Group_Commit));
         Set_Attribute
           (Child, "closed", Boolean'Image (Item.Closed));

         Add_Child (Ada_Child, Child);

         if not String_List.Is_Empty (Item.Files) then
            --  Append all files to this child

            F_Iter := String_List.First (Item.Files);

            for K in 1 .. String_List.Length (Item.Files) loop
               F_Child       := new Node;
               F_Child.Tag   := new String'("file");
               F_Child.Value := new String'(String_List.Data (F_Iter));

               Add_Child (Child, F_Child);

               F_Iter := String_List.Next (F_Iter);
            end loop;
         end if;

         Item := Get_Next;
      end loop;

      Trace (Me, "Saving " & Filename);
      Print (File, Filename, Success);
      Free (File);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Save_Activities;

   ------------------
   -- New_Activity --
   ------------------

   function New_Activity
     (Kernel : access Kernel_Handle_Record'Class) return Activity_Id is
   begin
      New_Id : loop
         declare
            UID : constant Activity_Id :=
                    Activity_Id
                      (Image (Clock, Picture_String'("%Y%m%d%H%M%S%i")));
         begin
            if Get (UID) = Empty_Activity then
               --  Retreive the current root project name
               Set (UID,
                 (Project_Path
                    (Get_Root_Project (Get_Registry (Kernel).all)),
                  new String'("New Activity"),
                  No_Class_Instance,
                  UID,
                  null,
                  False, False,
                  new String_Hash_Table.HTable,
                  String_List.Null_List, False));

               Save_Activities (Kernel);

               return UID;
            end if;
         end;
      end loop New_Id;
   end New_Activity;

   --------------------------
   -- Get_VCS_For_Activity --
   --------------------------

   function Get_VCS_For_Activity
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id) return VCS_Access
   is
      VCS   : VCS_Access := Get (Activity).VCS;
      Files : String_List.List;
   begin
      if VCS = null or else VCS.all in Unknown_VCS_Record then
         --  It is possible that the VCS is not yet known. This happen just
         --  after loading the activities XML registry. Compute it now, we know
         --  that all current files are using the same VCS otherwise they won't
         --  have been append.

         Files := Get (Activity).Files;

         if not String_List.Is_Empty (Files)then
            declare
               File    : constant Virtual_File :=
                           Create (String_List.Head (Files));
               Project : constant Project_Type :=
                           Get_Project_From_File
                             (Get_Registry (Kernel).all, File);
            begin
               if Project /= No_Project then
                  VCS := Get_Current_Ref (Kernel, Project);
                  declare
                     Item : Activity_Record := Get (Activity);
                  begin
                     Item.VCS := VCS;
                     Set (Activity, Item);
                  end;
               end if;
            end;
         end if;
      end if;

      return VCS;
   end Get_VCS_For_Activity;

   ----------------------
   -- Get_Project_Path --
   ----------------------

   function Get_Project_Path (Activity : Activity_Id) return Virtual_File is
   begin
      return Get (Activity).Project;
   end Get_Project_Path;

   ---------------------
   -- Delete_Activity --
   ---------------------

   procedure Delete_Activity
     (Kernel : access Kernel_Handle_Record'Class; Activity : Activity_Id)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (String_Hash_Table.HTable, Key_Hash_Access);

      Logs_Dir  : constant String := Get_Home_Dir (Kernel) & "log_files";
      File_Name : constant String :=
                    Logs_Dir & OS_Lib.Directory_Separator &
                    String (Activity) & "$log";
      Success   : Boolean;
      pragma Unreferenced (Success);
   begin
      declare
         Item : Activity_Record := Get (Activity);
      begin
         String_List.Free (Item.Files);
         String_Hash_Table.Reset (Item.Keys.all);
         Free (Item.Name);
         Free (Item.Keys);
      end;

      Remove (Activity);

      Save_Activities (Kernel);

      OS_Lib.Delete_File (File_Name, Success);
   end Delete_Activity;

   -------------
   -- Has_Log --
   -------------

   function Has_Log
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id) return Boolean
   is
      Logs_Dir  : constant String := Get_Home_Dir (Kernel) & "log_files";
      File_Name : constant String :=
                    Logs_Dir & OS_Lib.Directory_Separator &
                    String (Activity) & "$log";
      Log_File : constant Virtual_File :=
                    Create (Full_Filename => File_Name);
   begin
      return Is_Regular_File (Log_File);
   end Has_Log;

   ------------------
   -- Get_Log_File --
   ------------------

   function Get_Log_File
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id) return Virtual_File
   is
      Logs_Dir  : constant String := Get_Home_Dir (Kernel) & "log_files";
      File_Name : constant String :=
                    Logs_Dir & OS_Lib.Directory_Separator &
                    String (Activity) & "$log";
      File      : constant Virtual_File := Create (File_Name);
      F         : OS_Lib.File_Descriptor;
   begin
      if not Is_Regular_File (File) then
         F := OS_Lib.Create_New_File (File_Name, OS_Lib.Text);
         OS_Lib.Close (F);
         return File;

      else
         return File;
      end if;
   end Get_Log_File;

   -------------
   -- Get_Log --
   -------------

   function Get_Log
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id) return String
   is
      use type Strings.String_Access;
      File : constant Virtual_File := Get_Log_File (Kernel, Activity);
      R    : Strings.String_Access;
   begin
      R := Read_File (File);

      if R = null then
         return "";

      else
         declare
            S : constant String := R.all;
         begin
            OS_Lib.Free (R);
            return S;
         end;
      end if;
   end Get_Log;

   -----------
   -- First --
   -----------

   function First return Activity_Id is
   begin
      return Get_First.Id;
   end First;

   ----------
   -- Next --
   ----------

   function Next return Activity_Id is
   begin
      return Get_Next.Id;
   end Next;

   ------------------
   -- Set_Instance --
   ------------------

   procedure Set_Instance
     (Activity : Activity_Id; Instance : Class_Instance)
   is
      Item : Activity_Record := Get (Activity);
   begin
      Item.Instance := Instance;
      Set (Activity, Item);
   end Set_Instance;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance (Activity : Activity_Id) return Class_Instance is
   begin
      return Get (Activity).Instance;
   end Get_Instance;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Activity : Activity_Id) return String is
   begin
      if Activity = No_Activity then
         return "";
      else
         return Get (Activity).Name.all;
      end if;
   end Get_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      Name     : String)
   is
      Item : Activity_Record := Get (Activity);
   begin
      Free (Item.Name);
      Item.Name := new String'(Name);
      Set (Activity, Item);
      Save_Activities (Kernel);
   end Set_Name;

   -----------------------
   -- Get_File_Activity --
   -----------------------

   function Get_File_Activity (File : Virtual_File) return Activity_Id is
      Item : Activity_Record := Get_First;
   begin
      while Item /= Empty_Activity loop
         if not Item.Closed
           and then String_Hash_Table.Get
             (Item.Keys.all, File_Key (File)) /= null
         then
            return Item.Id;
         end if;

         Item := Get_Next;
      end loop;

      return No_Activity;
   end Get_File_Activity;

   ---------------------------
   -- Get_Files_In_Activity --
   ---------------------------

   function Get_Files_In_Activity
     (Activity : Activity_Id) return String_List.List
   is
      Item : Activity_Record := Get (Activity);
   begin
      if not Item.Sorted then
         String_List_Utils.Sort (Item.Files);
         Item.Sorted := True;
         Set (Activity, Item);
      end if;

      return Item.Files;
   end Get_Files_In_Activity;

   --------------
   -- Add_File --
   --------------

   procedure Add_File
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      File     : Virtual_File)
   is
      F_Activity : constant Activity_Id := Get_File_Activity (File);
      Project    : constant Project_Type :=
                     Get_Project_From_File (Get_Registry (Kernel).all, File);
      VCS        : constant VCS_Access := Get_Current_Ref (Kernel, Project);
      Item       : Activity_Record := Get (Activity);

      procedure Add (File : Virtual_File);
      --  Add Name (a file or directory) into the VCS Activities

      ---------
      -- Add --
      ---------

      procedure Add (File : Virtual_File) is
         Name : constant String := File_Key (File);
      begin
         if String_Hash_Table.Get (Item.Keys.all, Name) = null then

            if Item.VCS = null then
               --  This is the first file added into this activity, set the
               --  group commit if supported.
               Item.VCS := VCS;

               Item.Group_Commit := Absolute_Filenames_Supported (VCS)
                 and then Atomic_Commands_Supported (VCS);
            end if;

            String_Hash_Table.Set
              (Item.Keys.all, File_Key (File), new String'(Item_Tag));
            String_List.Append (Item.Files, Full_Name (File, False).all);
            Item.Sorted := False;

            Set (Activity, Item);

            Save_Activities (Kernel);
         end if;
      end Add;

   begin
      --  Check that the new file is using the same VCS. Also check that the
      --  file is not yet part of an open activity.

      if Item.VCS /= null and then VCS /= Item.VCS then
         Console.Insert
           (Kernel,
            "Cannot add " & Full_Name (File).all
            & " to activity, VCS not matching.");

      elsif F_Activity /= No_Activity and then not Is_Closed (F_Activity) then
         Console.Insert (Kernel, "Cannot add a file to a closed activity.");

      else
         Add (File);
      end if;
   end Add_File;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      File     : Virtual_File)
   is
      Item : Activity_Record := Get (Activity);
   begin
      String_Hash_Table.Remove (Item.Keys.all, File_Key (File));
      Remove_From_List (Item.Files, Full_Name (File, False).all);

      if String_List.Length (Item.Files) = 0 then
         --  No more file in this activity, clear the information
         Item.VCS := null;
         Item.Sorted := False;
         Item.Group_Commit := False;
      end if;

      Set (Activity, Item);
      Save_Activities (Kernel);
   end Remove_File;

   ----------------------
   -- Get_Group_Commit --
   ----------------------

   function Get_Group_Commit (Activity : Activity_Id) return Boolean is
   begin
      return Get (Activity).Group_Commit;
   end Get_Group_Commit;

   -------------------------
   -- Toggle_Group_Commit --
   -------------------------

   procedure Toggle_Group_Commit
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      Item : Activity_Record := Get (Activity);
   begin
      Item.Group_Commit := not Item.Group_Commit;
      Set (Activity, Item);
      Save_Activities (Kernel);
   end Toggle_Group_Commit;

   ----------------
   -- Set_Closed --
   ----------------

   procedure Set_Closed
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      To       : Boolean)
   is
      Item : Activity_Record := Get (Activity);
   begin
      Item.Closed := To;
      Set (Activity, Item);
      Save_Activities (Kernel);
   end Set_Closed;

   ---------------
   -- Is_Closed --
   ---------------

   function Is_Closed (Activity : Activity_Id) return Boolean is
   begin
      return Get (Activity).Closed;
   end Is_Closed;

   --------------------------
   -- Toggle_Closed_Status --
   --------------------------

   procedure Toggle_Closed_Status
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      Item : Activity_Record := Get (Activity);
   begin
      Item.Closed := not Item.Closed;
      Set (Activity, Item);
      Save_Activities (Kernel);
   end Toggle_Closed_Status;

   -------------------------------
   -- Get_Activity_Log_Template --
   -------------------------------

   function Get_Activity_Log_Template
     (Kernel : access Kernel_Handle_Record'Class) return String
   is
      Activity_Log_Template : constant String := "activity_log.tmplt";
      Home_Template         : constant String :=
                                Get_Home_Dir (Kernel) & Activity_Log_Template;
      Sys_Template          : constant String :=
                                Get_System_Dir (Kernel)
                                  & "share" & OS_Lib.Directory_Separator
                                  & "gps" & OS_Lib.Directory_Separator
                                  & Activity_Log_Template;
   begin
      if OS_Lib.Is_Regular_File (Home_Template) then
         return Home_Template;
      else
         return Sys_Template;
      end if;
   end Get_Activity_Log_Template;

   --------------------------------
   -- Get_Activity_Template_Tags --
   --------------------------------

   function Get_Activity_Template_Tags
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id) return Translate_Set
   is
      Files          : constant String_List.List :=
                         Get_Files_In_Activity (Activity);
      Iter           : String_List.List_Node;

      V_Files        : Tag; -- vector tag (string), file names
      V_Logs         : Tag; -- vector tag (string), corresponding log
      V_Is_Directory : Tag; -- vector tag (boolean), true if a directory
      T_Set          : Translate_Set;

   begin
      Insert (T_Set, Assoc ("ACTIVITY_LOG", Get_Log (Kernel, Activity)));
      Insert (T_Set, Assoc ("ACTIVITY_NAME", Get_Name (Activity)));
      Iter := String_List.First (Files);

      for K in 1 .. String_List.Length (Files) loop
         declare
            use type String_List.List_Node;
            Filename : constant String := String_List.Data (Iter);
            File     : constant Virtual_File := Create (Filename);
         begin
            if Is_Directory (File) then
               Append (V_Is_Directory, True);
            else
               Append (V_Is_Directory, False);
            end if;

            Append (V_Files, Base_Dir_Name (File));
            Append (V_Logs, Get_Log (Kernel, File));
            Iter := String_List.Next (Iter);
         end;
      end loop;

      Insert (T_Set, Assoc ("FILES", V_Files));
      Insert (T_Set, Assoc ("LOGS", V_Logs));
      Insert (T_Set, Assoc ("IS_DIRECTORY", V_Is_Directory));

      return T_Set;
   end Get_Activity_Template_Tags;

end VCS_Activities;
