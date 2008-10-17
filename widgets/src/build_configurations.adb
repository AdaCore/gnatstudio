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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Switches_Parser;
with String_Utils;    use String_Utils;

package body Build_Configurations is

   use GNAT.OS_Lib;
   use Target_List;

   ------------------------
   -- Local declarations --
   ------------------------

   procedure Add_Target
     (Registry : Build_Config_Registry_Access;
      Target   : Target_Access);
   --  Add Target to Registry

   function "-" (Msg : String) return String;
   --  Convenient shortcut to the Gettext function

   function Command_Line_To_XML
     (CL  : GNAT.OS_Lib.Argument_List;
      Tag : String) return Node_Ptr;
   function XML_To_Command_Line
     (N : Node_Ptr) return GNAT.OS_Lib.Argument_List;
   --  Convert between a command line to/from the following XML representation
   --          <command-line>
   --             <arg>COMMAND</arg>
   --             <arg>ARG1</arg>
   --                 ...
   --             <arg>ARGN</arg>
   --          </command-line>

   function Copy (T : Target_Access) return Target_Access;
   --  Allocate and return a deep copy of T.

   function Deep_Copy (CL : Argument_List_Access) return Argument_List_Access;
   --  Allocate and return a deep copy of CL.

   function Equals (T1 : Target_Access; T2 : Target_Access) return Boolean;
   --  Deep comparison between T1 and T2.

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy
     (CL : Argument_List_Access) return Argument_List_Access
   is
      CL2 : Argument_List_Access;
   begin
      if CL = null then
         return null;
      end if;

      CL2 := new Argument_List (CL'Range);
      for J in CL'Range loop
         if CL (J) /= null then
            CL2 (J) := new String'(CL (J).all);
         end if;
      end loop;
      return CL2;
   end Deep_Copy;

   ----------
   -- Copy --
   ----------

   function Copy (T : Target_Access) return Target_Access is
      T2 : Target_Access;
   begin
      T2 := new Target_Type;
      T2.Name := T.Name;
      T2.Model     := T.Model;
      T2.Command_Line := Deep_Copy (T.Command_Line);
      T2.Properties := T.Properties;
      return T2;
   end Copy;

   ------------
   -- Equals --
   ------------

   function Equals (T1 : Target_Access; T2 : Target_Access) return Boolean is
      function Equals (A, B : Argument_List_Access) return Boolean;
      --  Comparison function

      function Equals (A, B : Argument_List_Access) return Boolean is
      begin
         if A = null and then B = null then
            return True;
         end if;

         if        A = null
           or else B = null
           or else A'First /= B'First
           or else A'Last /= B'Last
         then
            return False;
         end if;

         for J in A'Range loop
            if not (A (J) = null and then B (J) = null) then
               if A (J) = null
                 or else B (J) = null
                 or else A (J).all /= B (J).all
               then
                  return False;
               end if;
            end if;
         end loop;
         return True;
      end Equals;

   begin
      if        T1.Name       /= T2.Name
        or else T1.Model      /= T2.Model
        or else T1.Properties /= T2.Properties
        or else not Equals (T1.Command_Line, T2.Command_Line)
      then
         return False;
      end if;

      return True;
   end Equals;

   ---------
   -- "-" --
   ---------

   function "-" (Msg : String) return String is
   begin
      --  ??? Provide implementation
      return Msg;
   end "-";

   ---------
   -- Log --
   ---------

   procedure Log
     (Registry : Build_Config_Registry_Access;
      Message  : String;
      Mode     : Message_Mode := Error) is
   begin
      if Registry.Logger /= null then
         Registry.Logger (Message, Mode);
      end if;
   end Log;

   ---------------------------
   -- Create_Model_From_XML --
   ---------------------------

   procedure Create_Model_From_XML
     (Registry : Build_Config_Registry_Access;
      XML      : Node_Ptr)
   is
      Model : Target_Model_Type;

      procedure Parse_Target_Model_Node (N : Node_Ptr);
      --  Parse a global target model node

      procedure Parse_Switches_Node (N : Node_Ptr);
      --  Parse switches node

      -----------------------------
      -- Parse_Target_Model_Node --
      -----------------------------

      procedure Parse_Target_Model_Node (N : Node_Ptr) is
         Name  : constant String := Get_Attribute (N, "name", "");
         Cat   : constant String := Get_Attribute (N, "category", "");
         Child : Node_Ptr;

         use type Glib.String_Ptr;
      begin
         if Name = "" then
            Log
              (Registry,
               -("target-model nodes must have non-empty" &
                 " ""name"" attribute"));
            return;
         end if;

         Model.Name := To_Unbounded_String (Name);
         Model.Category := To_Unbounded_String (Cat);

         Child := N.Child;

         while Child /= null loop
            if Child.Tag.all = "switches" then
               Parse_Switches_Node (Child);

            elsif Child.Tag.all = "description" then
               if Child.Value /= null then
                  Model.Description :=
                    To_Unbounded_String (Child.Value.all);
               end if;

            elsif Child.Tag.all = "command-line" then
               Model.Default_Command_Line :=
                 new GNAT.OS_Lib.Argument_List'(XML_To_Command_Line (Child));

            elsif Child.Tag.all = "icon" then
               if Child.Value /= null then
                  Model.Icon := To_Unbounded_String (Child.Value.all);
               end if;

            elsif Child.Tag.all = "server" then
               Model.Server := Server_Type'Value (Child.Value.all);

            else
               Log
                 (Registry,
                  (-"tag not recognized as child of ""target-model"" node:")
                  & Child.Tag.all);
            end if;

            Child := Child.Next;
         end loop;
      end Parse_Target_Model_Node;

      -------------------------
      -- Parse_Switches_Node --
      -------------------------

      procedure Parse_Switches_Node (N : Node_Ptr) is
         M        : Unbounded_String;
         Switches : Switches_Editor_Config;
      begin
         Switches_Parser.Parse_Switches_Node
           (Current_Tool_Name   => "",
            Current_Tool_Config => Switches,
            Error_Message       => M,
            Finder              => null,
            Node                => N);

         if M /= "" then
            Log (Registry, To_String (M));
         end if;

         Model.Switches := Switches;
      end Parse_Switches_Node;

      use type Glib.String_Ptr;
   begin
      if XML = null or else XML.Tag = null then
         Log (Registry, -"Error: empty XML passed to builder configuration");
         return;
      end if;

      if XML.Tag.all /= "target-model" then
         --  This means in fact a program error: we should never reach this
         --  procedure with a node that does not correspond to a target model
         Log (Registry, -"Error: invalid XML passed to builder configuration");
         return;
      end if;

      --  Parse the XML
      Parse_Target_Model_Node (XML);

      --  Detect whether the model exists

      if Registry.Models.Contains (Model.Name) then
         Log (Registry,
              (-"Error: a model is already registered with the name '")
              & To_String (Model.Name) & "'");
         return;
      end if;

      --  Register the model
      Registry.Models.Insert (Model.Name, new Target_Model_Type'(Model));
   end Create_Model_From_XML;

   -------------------
   -- Create_Target --
   -------------------

   procedure Create_Target
     (Registry     : Build_Config_Registry_Access;
      Name         : String;
      Category     : String;
      Model        : String)
   is
      Target    : Target_Access;
      The_Model : Target_Model_Access;
   begin
      --  Lookup the model

      if not Registry.Models.Contains (To_Unbounded_String (Model)) then
         Log
           (Registry,
            Name & (-": cannot create target: no model registered with name ")
            & Model);
         return;
      end if;

      if Name = "" then
         Log (Registry, -"Cannot create target with an empty name");
         return;
      end if;

      The_Model := Registry.Models.Element (To_Unbounded_String (Model));

      Target := new Target_Type;
      Target.Name := To_Unbounded_String (Strip_Single_Underscores (Name));
      Target.Properties.Menu_Name := To_Unbounded_String (Name);
      Target.Properties.Category := To_Unbounded_String (Category);
      Target.Model := The_Model;

      if The_Model.Default_Command_Line /= null then
         Set_Command_Line
           (Registry, Target, The_Model.Default_Command_Line.all);
      end if;

      Add_Target (Registry, Target);
   end Create_Target;

   ------------------
   -- Change_Model --
   ------------------

   procedure Change_Model
     (Registry : Build_Config_Registry_Access;
      Target   : String;
      Model    : String)
   is
      The_Target : Target_Access;
      The_Model  : Target_Model_Access;
      Empty      : constant Argument_List (1 .. 0) := (others => null);
   begin
      --  Lookup the model

      if not Registry.Models.Contains (To_Unbounded_String (Model)) then
         Log
           (Registry,
            (-"cannot change model: no model registered with name ") & Model);
         return;
      end if;

      The_Model := Registry.Models.Element (To_Unbounded_String (Model));
      The_Target := Get_Target_From_Name (Registry, Target);

      if The_Target = null then
         Log
           (Registry,
            (-"Cannot change model: no target registered with name ")
            & Target);
         return;
      end if;

      The_Target.Model := The_Model;

      if The_Model.Default_Command_Line = null then
         Set_Command_Line (Registry, The_Target, Empty);
      else
         Set_Command_Line
           (Registry, The_Target,
            The_Model.Default_Command_Line.all);
      end if;
   end Change_Model;

   ----------------------
   -- Duplicate_Target --
   ----------------------

   procedure Duplicate_Target
     (Registry     : Build_Config_Registry_Access;
      Src_Name     : String;
      New_Name     : String;
      New_Category : String)
   is
      Src : Target_Access;
   begin
      Src := Get_Target_From_Name (Registry, Src_Name);

      if Src = null then
         Log (Registry, -("Cannot duplicate: source target not found: ")
              & Src_Name);
         return;
      end if;

      if Get_Target_From_Name (Registry, New_Name) /= null then
         Log (Registry, -("Cannot duplicate: target already exists: ")
              & New_Name);
         return;
      end if;

      Create_Target (Registry => Registry,
                     Name     => New_Name,
                     Category => New_Category,
                     Model    => To_String (Src.Model.Name));
   end Duplicate_Target;

   ----------------------
   -- Set_Command_Line --
   ----------------------

   procedure Set_Command_Line
     (Registry     : Build_Config_Registry_Access;
      Target       : Target_Access;
      Command_Line : GNAT.OS_Lib.Argument_List)
   is
      pragma Unreferenced (Registry);

   begin
      if Target.Command_Line /= null then
         for J in Target.Command_Line'Range loop
            Free (Target.Command_Line (J));
         end loop;

         Unchecked_Free (Target.Command_Line);
      end if;

      Target.Command_Line := new GNAT.OS_Lib.Argument_List
        (Command_Line'Range);

      for J in Command_Line'Range loop
         Target.Command_Line (J) := new String'(Command_Line (J).all);
      end loop;
   end Set_Command_Line;

   --------------
   -- Contains --
   --------------

   function Contains
     (List : Target_List.List; Key : Unbounded_String) return Boolean
   is
      C : Cursor;
   begin
      C := List.First;

      while Has_Element (C) loop
         if Element (C).Name = Key then
            return True;
         end if;

         Next (C);
      end loop;

      return False;
   end Contains;

   ----------------
   -- Add_Target --
   ----------------

   procedure Add_Target
     (Registry : Build_Config_Registry_Access;
      Target   : Target_Access) is
   begin
      if Contains (Registry.Targets, Target.Name) then
         Log (Registry, -("Target with this name already exists: ")
              & To_String (Target.Name));
         return;
      end if;

      Log (Registry,
           "Creating target '" & To_String (Target.Name) & "'",
           Trace);
      Registry.Targets.Append (Target);
   end Add_Target;

   -------------------
   -- Remove_Target --
   -------------------

   procedure Remove_Target
     (Registry    : Build_Config_Registry_Access;
      Target_Name : String)
   is
      C : Cursor;
   begin
      C := Registry.Targets.First;

      while Has_Element (C) loop
         if To_String (Element (C).Name) = Target_Name then
            Registry.Targets.Delete (C);
            exit;
         end if;

         Next (C);
      end loop;
   end Remove_Target;

   --------------------------
   -- Get_Target_From_Name --
   --------------------------

   function Get_Target_From_Name
     (Registry : Build_Config_Registry_Access;
      Name     : String) return Target_Access
   is
      C : Cursor;
   begin
      C := Registry.Targets.First;

      while Has_Element (C) loop
         if To_String (Element (C).Name) = Name then
            return Element (C);
         end if;

         Next (C);
      end loop;

      return null;
   end Get_Target_From_Name;

   ---------------------------------
   -- Get_Command_Line_Unexpanded --
   ---------------------------------

   function Get_Command_Line_Unexpanded
     (Registry : Build_Config_Registry_Access;
      Mode     : String;
      Target   : Target_Access) return GNAT.OS_Lib.Argument_List
   is
      Current_Mode : Build_Mode_Access;
      Empty        : constant Argument_List (1 .. 0) := (others => null);
   begin
      --  ??? We should do macro expansion here!

      if Target = null
        or else Target.Command_Line = null
      then
         --  A target command line should at least contain the command to
         --  launch; if none can be found, return.
         return Empty;
      end if;

      if Registry.Modes.Contains (To_Unbounded_String (Mode)) then
         Current_Mode := Registry.Modes.Element (To_Unbounded_String (Mode));
      end if;

      if Current_Mode = null
        or else Current_Mode.Switches = null
        or else Current_Mode.Switches'Length = 0
      then
         --  There is no mode, or the mode brings no switches
         return Target.Command_Line.all;
      else
         return Target.Command_Line.all
           & Current_Mode.Switches.all;
      end if;
   end Get_Command_Line_Unexpanded;

   ----------------------
   -- Get_Switch_Value --
   ----------------------

   function Get_Switch_Value
     (Target : Target_Access;
      Switch : String) return String is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return Get_Switch_Value (Target, Switch);
   end Get_Switch_Value;

   ----------
   -- Free --
   ----------

   procedure Free (Target : in out Target_Type) is
   begin
      GNAT.OS_Lib.Free (Target.Command_Line);
   end Free;

   -----------------
   -- Create_Mode --
   -----------------

   procedure Create_Mode
     (Registry : Build_Config_Registry_Access;
      Name     : String;
      Switches : GNAT.OS_Lib.Argument_List)
   is
      Mode : Build_Mode_Access;
   begin
      Mode := new Build_Mode;

      Mode.Name := To_Unbounded_String (Name);
      Mode.Switches := new GNAT.OS_Lib.Argument_List'(Switches);

      Registry.Modes.Insert (Mode.Name, Mode);
   end Create_Mode;

   ------------
   -- Create --
   ------------

   function Create
     (Logger : Logger_Type) return Build_Config_Registry_Access
   is
      Result : Build_Config_Registry_Access;
   begin
      Result := new Build_Config_Registry;
      Result.Logger := Logger;

      return Result;
   end Create;

   -------------------------
   -- Command_Line_To_XML --
   -------------------------

   function Command_Line_To_XML
     (CL   : GNAT.OS_Lib.Argument_List;
      Tag  : String) return Node_Ptr
   is
      N, Arg : Node_Ptr;
   begin
      N := new Node;
      N.Tag := new String'(Tag);

      if CL'Length <= 0 then
         return N;
      end if;

      N.Child := new Node;
      Arg := N.Child;

      for J in CL'Range loop
         Arg.Tag   := new String'("arg");
         Arg.Value := new String'(CL (J).all);

         if J /= CL'Last then
            Arg.Next := new Node;
            Arg := Arg.Next;
         end if;
      end loop;

      return N;
   end Command_Line_To_XML;

   -------------------------
   -- XML_To_Command_Line --
   -------------------------

   function XML_To_Command_Line
     (N : Node_Ptr) return GNAT.OS_Lib.Argument_List
   is
      Count : Natural := 0;
      Arg   : Node_Ptr;
      use type Glib.String_Ptr;
   begin
      Arg := N.Child;

      --  Count the arguments
      while Arg /= null loop
         Count := Count + 1;
         Arg := Arg.Next;
      end loop;

      --  Create the command line
      declare
         CL : GNAT.OS_Lib.Argument_List (1 .. Count);
      begin
         Arg := N.Child;
         for J in 1 .. Count loop
            if Arg.Value = null then
               CL (J) := new String'("");
            else
               CL (J) := new String'(Arg.Value.all);
            end if;

            Arg := Arg.Next;
         end loop;

         return CL;
      end;
   end XML_To_Command_Line;

   ------------------------
   -- Save_Target_To_XML --
   ------------------------

   function Save_Target_To_XML
     (Registry : Build_Config_Registry_Access;
      Target   : Target_Access) return Node_Ptr
   is
      pragma Unreferenced (Registry);
      N   : Node_Ptr;
      C   : Node_Ptr;
   begin
      N := new Node;
      N.Tag := new String'("target");

      --  Main node
      N.Attributes := new String'
        ("model="""
         & XML_Protect (To_String (Target.Model.Name)) & """ " &
         "category="""
         & XML_Protect (To_String (Target.Properties.Category)) & """ " &
         "name="""
         & XML_Protect (To_String (Target.Properties.Menu_Name)) & """");
      --  Insert a <icon> node if needed

      N.Child := new Node;
      C := N.Child;
      C.Tag := new String'("in-toolbar");
      C.Value := new String'(Target.Properties.In_Toolbar'Img);

      if Target.Properties.Icon /= "" then
         C.Next := new Node;
         C := C.Next;
         C.Tag := new String'("icon");
         C.Value := new String'(To_String (Target.Properties.Icon));
      end if;

      C.Next := new Node;
      C := C.Next;
      C.Tag := new String'("in-menu");
      C.Value := new String'(Target.Properties.In_Menu'Img);

      C.Next := new Node;
      C := C.Next;
      C.Tag := new String'("launch-mode");
      C.Value := new String'(Target.Properties.Launch_Mode'Img);

      C.Next := new Node;
      C := C.Next;
      C.Tag := new String'("read-only");
      C.Value := new String'(Target.Properties.Read_Only'Img);

      C.Next := new Node;
      C := C.Next;
      C.Tag := new String'("represents-mains");
      C.Value := new String'(Target.Properties.Represents_Mains'Img);

      C.Next := new Node;
      C := C.Next;
      C.Tag := new String'("key");
      C.Value := new String'(To_String (Target.Properties.Key));

      C.Next := new Node;
      C := C.Next;
      C.Tag := new String'("server");
      C.Value := new String'(Target.Properties.Server'Img);

      if Target.Command_Line /= null then
         C.Next := Command_Line_To_XML
           (Target.Command_Line.all, "command-line");
         C := C.Next;
      end if;

      return N;
   end Save_Target_To_XML;

   --------------------------
   -- Load_Target_From_XML --
   --------------------------

   function Load_Target_From_XML
     (Registry  : Build_Config_Registry_Access;
      XML       : Node_Ptr;
      From_User : Boolean) return Target_Access
   is
      Child  : Node_Ptr;
      Target : Target_Access;

      use type Glib.String_Ptr;
   begin
      if XML = null
        or else XML.Tag = null
      then
         Log (Registry, -"Error: empty XML passed to target builder");
         return null;
      end if;

      if XML.Tag.all /= "target" then
         Log (Registry, -"Error: wrong XML passed to target builder");
         return null;
      end if;

      --  Main node

      declare
         Menu_Name : constant String := (Get_Attribute (XML, "name", ""));
         Target_Name : constant String := Strip_Single_Underscores (Menu_Name);
         Category  : constant String := (Get_Attribute (XML, "category", ""));
         Model     : constant String := (Get_Attribute (XML, "model", ""));
      begin
         if Menu_Name = "" then
            Log (Registry,
                 -"Error: <target> node should have a ""name"" attribute");
            return null;
         end if;
         if Category = "" then
            Log (Registry,
                 -"Error: <target> node should have a ""category"" attribute");
            return null;
         end if;
         if Model = "" then
            Log (Registry,
                 -"Error: <target> node should have a ""model"" attribute");
            return null;
         end if;

         if not Registry.Models.Contains (To_Unbounded_String (Model)) then
            Log (Registry, (-"Error: unknown target model: ") & Model);
            return null;
         end if;

         if Contains (Registry.Targets, To_Unbounded_String (Target_Name)) then
            if From_User then
               --  In this case, it is OK to overwrite the data contained in
               --  the target.
               Target := Get_Target_From_Name (Registry, Target_Name);
            else
               Log (Registry, -"Error: target already registered: "
                    & Target_Name);
               return null;
            end if;

         else
            Target := new Target_Type;
            Target.Name  := To_Unbounded_String (Target_Name);
            Target.Properties.Menu_Name := To_Unbounded_String (Menu_Name);
            Target.Properties.Category := To_Unbounded_String (Category);
            Target.Model := Registry.Models.Element
              (To_Unbounded_String (Model));

            Add_Target (Registry, Target);
         end if;
      end;

      Child := XML.Child;

      while Child /= null loop
         if Child.Tag.all = "command-line" then
            Target.Command_Line := new GNAT.OS_Lib.Argument_List'
              (XML_To_Command_Line (Child));

         elsif Child.Value = null then
            Log (Registry, -"Warning: empty node in target: " & Child.Tag.all);

         elsif Child.Tag.all = "launch-mode" then
            Target.Properties.Launch_Mode := Launch_Mode_Type'Value
              (Child.Value.all);

         elsif Child.Tag.all = "icon" then
            Target.Properties.Icon := To_Unbounded_String (Child.Value.all);

         elsif Child.Tag.all = "in-toolbar" then
            Target.Properties.In_Toolbar := Boolean'Value (Child.Value.all);

         elsif Child.Tag.all = "in-menu" then
            Target.Properties.In_Menu := Boolean'Value (Child.Value.all);

         elsif Child.Tag.all = "key" then
            Target.Properties.Key := To_Unbounded_String (Child.Value.all);

         elsif Child.Tag.all = "read-only" then
            Target.Properties.Read_Only := Boolean'Value (Child.Value.all);

         elsif Child.Tag.all = "represents-mains" then
            Target.Properties.Represents_Mains :=
              Boolean'Value (Child.Value.all);

         elsif Child.Tag.all = "server" then
            Target.Properties.Server := Server_Type'Value (Child.Value.all);

         else
            Log (Registry, (-"Warning: invalid child to <target> node: ")
                 & Child.Tag.all);
         end if;

         Child := Child.Next;
      end loop;

      --  At this point, the target data has been updated. If this target is
      --  not from the user configuration, copy it to the original targets.

      Registry.Original_Targets.Append (Copy (Target));

      return Target;
   end Load_Target_From_XML;

   -----------------------------
   -- Save_All_Targets_To_XML --
   -----------------------------

   function Save_All_Targets_To_XML
     (Registry : Build_Config_Registry_Access) return Node_Ptr
   is
      N     : Node_Ptr;
      Child : Node_Ptr;
      C     : Cursor;
      C2    : Cursor;
      Name  : Unbounded_String;
      Equals_To_Original : Boolean;

   begin
      N := new Node;
      N.Tag := new String'("targets");

      C := Registry.Targets.First;

      while Has_Element (C) loop
         --  Check whether the target has been modified. If so, save it.

         Name := Element (C).Name;
         Equals_To_Original := False;
         C2 := Registry.Original_Targets.First;

         while Has_Element (C2) loop
            if Element (C2).Name = Name then
               if Equals (Element (C2), Element (C)) then
                  Equals_To_Original := True;
               end if;
               exit;
            end if;
            Next (C2);
         end loop;

         if not Equals_To_Original then
            if Child = null then
               N.Child := Save_Target_To_XML (Registry, Element (C));
               Child := N.Child;
            else
               Child.Next := Save_Target_To_XML (Registry, Element (C));
               Child := Child.Next;
            end if;
         end if;

         Next (C);
      end loop;

      return N;
   end Save_All_Targets_To_XML;

   -------------------------------
   -- Load_All_Targets_From_XML --
   -------------------------------

   procedure Load_All_Targets_From_XML
     (Registry : Build_Config_Registry_Access;
      XML      : Node_Ptr)
   is
      N : Node_Ptr;
      T : Target_Access;
      pragma Unreferenced (T);
      use type Glib.String_Ptr;
   begin
      if XML = null
        or else XML.Tag = null
        or else XML.Tag.all /= "targets"
      then
         Log (Registry, "Invalid XML found when loading multiple targets");
         return;
      end if;

      N := XML.Child;

      while N /= null loop
         T := Load_Target_From_XML
           (Registry => Registry, XML => N, From_User => True);
         N := N.Next;
      end loop;
   end Load_All_Targets_From_XML;

   --------------------
   -- Get_Properties --
   --------------------

   function Get_Properties (Target : Target_Access) return Target_Properties is
   begin
      return Target.Properties;
   end Get_Properties;

   ----------------------
   -- Get_First_Target --
   ----------------------

   function Get_First_Target
     (Registry : Build_Config_Registry_Access) return Target_Cursor is
   begin
      return Target_Cursor (Registry.Targets.First);
   end Get_First_Target;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target (Cursor : Target_Cursor) return Target_Access is
   begin
      if Has_Element (Cursor) then
         return Element (Cursor);
      else
         return null;
      end if;
   end Get_Target;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Cursor : in out Target_Cursor) is
   begin
      Target_List.Next (Target_List.Cursor (Cursor));
   end Next;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Target : Target_Access) return String is
   begin
      return To_String (Target.Name);
   end Get_Name;

   -------------------
   -- Get_Menu_Name --
   -------------------

   function Get_Menu_Name (Target : Target_Access) return String is
   begin
      return To_String (Target.Properties.Menu_Name);
   end Get_Menu_Name;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (Target : Target_Access) return String is
   begin
      return To_String (Target.Properties.Category);
   end Get_Category;

   ----------------
   -- Get_Server --
   ----------------

   function Get_Server (Target : Target_Access) return Server_Type is
   begin
      if Target.Properties.Server = GPS_Server then
         return Target.Model.Server;
      else
         return Target.Properties.Server;
      end if;
   end Get_Server;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon (Target : Target_Access) return String is
   begin
      if Target.Properties.Icon = "" then
         return To_String (Target.Model.Icon);
      else
         return To_String (Target.Properties.Icon);
      end if;
   end Get_Icon;

   -------------------------
   -- Is_Registered_Model --
   -------------------------

   function Is_Registered_Model
     (Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String) return Boolean is
   begin
      return Registry.Models.Contains (Name);
   end Is_Registered_Model;

   -------------------
   -- Revert_Target --
   -------------------

   procedure Revert_Target
     (Registry : Build_Config_Registry_Access;
      Target   : String)
   is
      O, C : Cursor;
   begin
      O := Registry.Targets.First;

      while Has_Element (O) loop
         if Element (O).Name = Target then
            exit;
         end if;

         Next (O);
      end loop;

      if not Has_Element (O) then
         Log (Registry, "Revert_Target: original target not found", Trace);
         return;
      end if;

      C := Registry.Original_Targets.First;

      while Has_Element (C) loop
         if Element (C).Name = Target then
            Registry.Targets.Replace_Element (O, Copy (Element (C)));
            exit;
         end if;

         Next (C);
      end loop;
   end Revert_Target;

end Build_Configurations;
