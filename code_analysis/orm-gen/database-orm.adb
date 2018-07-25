
pragma Warnings (Off);
with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;
pragma Warnings (On);
pragma Style_Checks (Off);

package body Database.Orm is
   pragma Warnings (Off);
   use Sessions.Pointers;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Entity_DDR, Entity_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Entity_Message_DDR, Entity_Message_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Message_DDR, Message_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Message_Property_DDR, Message_Property_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Property_DDR, Property_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Resource_DDR, Resource_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Resource_Message_DDR, Resource_Message_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Resource_Tree_DDR, Resource_Tree_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Rule_DDR, Rule_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Tool_DDR, Tool_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Entity'Class, Detached_Entity_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Message'Class, Detached_Message_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Property'Class, Detached_Property_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Resource'Class, Detached_Resource_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Rule'Class, Detached_Rule_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Tool'Class, Detached_Tool_Access);

   F_Entities_Id          : constant := 0;
   F_Entities_Name        : constant := 1;
   F_Entities_Line        : constant := 2;
   F_Entities_Col_Begin   : constant := 3;
   F_Entities_Col_End     : constant := 4;
   F_Entities_Resource_Id : constant := 5;
   Counts_Entities : constant Counts := ((6,6),(10,10),(10,10),(10,10));
   Upto_Entities_0 : constant Counts := ((6,6),(6,6),(6,6),(6,6));
   Alias_Entities : constant Alias_Array := (-1,2,-1);
   F_Entities_Messages_Id         : constant := 0;
   F_Entities_Messages_Entity_Id  : constant := 1;
   F_Entities_Messages_Message_Id : constant := 2;
   F_Entities_Messages_Line       : constant := 3;
   F_Entities_Messages_Col_Begin  : constant := 4;
   F_Entities_Messages_Col_End    : constant := 5;
   Upto_Entities_Messages_0 : constant Counts := ((6,6),(6,6),(6,6),(6,6));
   Upto_Entities_Messages_1 : constant Counts := ((6,6),(12,12),(16,16),(16,16));
   Alias_Entities_Messages : constant Alias_Array := (-1,3,6,-1,5,-1,-1,8,-1,10,-1);
   F_Messages_Id          : constant := 0;
   F_Messages_Rule_Id     : constant := 1;
   F_Messages_Data        : constant := 2;
   F_Messages_Ranking     : constant := 3;
   F_Messages_Tool_Msg_Id : constant := 4;
   Counts_Messages : constant Counts := ((5,5),(10,10),(12,12),(12,12));
   Upto_Messages_0 : constant Counts := ((5,5),(5,5),(5,5),(5,5));
   Alias_Messages : constant Alias_Array := (-1,2,-1,4,-1);
   F_Messages_Properties_Id          : constant := 0;
   F_Messages_Properties_Message_Id  : constant := 1;
   F_Messages_Properties_Property_Id : constant := 2;
   Upto_Messages_Properties_0 : constant Counts := ((3,3),(3,3),(3,3),(3,3));
   Upto_Messages_Properties_1 : constant Counts := ((3,3),(8,8),(13,13),(15,15));
   Alias_Messages_Properties : constant Alias_Array := (-1,3,8,-1,5,-1,7,-1,-1);
   F_Properties_Id         : constant := 0;
   F_Properties_Identifier : constant := 1;
   F_Properties_Name       : constant := 2;
   Counts_Properties : constant Counts := ((3,3),(3,3),(3,3),(3,3));
   Alias_Properties : constant Alias_Array := (0 => -1);
   F_Resource_Trees_Id        : constant := 0;
   F_Resource_Trees_Child_Id  : constant := 1;
   F_Resource_Trees_Parent_Id : constant := 2;
   Upto_Resource_Trees_0 : constant Counts := ((3,3),(3,3),(3,3),(3,3));
   Upto_Resource_Trees_1 : constant Counts := ((3,3),(3,7),(3,7),(3,7));
   Alias_Resource_Trees : constant Alias_Array := (-1,3,4,-1,0);
   F_Resources_Id        : constant := 0;
   F_Resources_Name      : constant := 1;
   F_Resources_Kind      : constant := 2;
   F_Resources_Timestamp : constant := 3;
   Counts_Resources : constant Counts := ((4,4),(4,4),(4,4),(4,4));
   Alias_Resources : constant Alias_Array := (0 => -1);
   F_Resources_Messages_Id          : constant := 0;
   F_Resources_Messages_Message_Id  : constant := 1;
   F_Resources_Messages_Resource_Id : constant := 2;
   F_Resources_Messages_Line        : constant := 3;
   F_Resources_Messages_Col_Begin   : constant := 4;
   F_Resources_Messages_Col_End     : constant := 5;
   Upto_Resources_Messages_0 : constant Counts := ((6,6),(6,6),(6,6),(6,6));
   Upto_Resources_Messages_1 : constant Counts := ((6,6),(11,11),(16,16),(18,18));
   Alias_Resources_Messages : constant Alias_Array := (-1,3,8,-1,5,-1,7,-1,-1);
   F_Rules_Id         : constant := 0;
   F_Rules_Name       : constant := 1;
   F_Rules_Identifier : constant := 2;
   F_Rules_Kind       : constant := 3;
   F_Rules_Tool_Id    : constant := 4;
   Counts_Rules : constant Counts := ((5,5),(7,7),(7,7),(7,7));
   Upto_Rules_0 : constant Counts := ((5,5),(5,5),(5,5),(5,5));
   Alias_Rules : constant Alias_Array := (-1,2,-1);
   F_Tools_Id   : constant := 0;
   F_Tools_Name : constant := 1;
   Counts_Tools : constant Counts := ((2,2),(2,2),(2,2),(2,2));
   Alias_Tools : constant Alias_Array := (0 => -1);

   pragma Warnings (On);
   function Detach_No_Lookup
     (Self    : Entity'Class;
      Session : Session_Type)
     return Detached_Entity'Class;
   function Detach_No_Lookup
     (Self    : Entity_Message'Class;
      Session : Session_Type)
     return Detached_Entity_Message'Class;
   function Detach_No_Lookup
     (Self    : Message'Class;
      Session : Session_Type)
     return Detached_Message'Class;
   function Detach_No_Lookup
     (Self    : Message_Property'Class;
      Session : Session_Type)
     return Detached_Message_Property'Class;
   function Detach_No_Lookup
     (Self    : Property'Class;
      Session : Session_Type)
     return Detached_Property'Class;
   function Detach_No_Lookup
     (Self    : Resource_Tree'Class;
      Session : Session_Type)
     return Detached_Resource_Tree'Class;
   function Detach_No_Lookup
     (Self    : Resource'Class;
      Session : Session_Type)
     return Detached_Resource'Class;
   function Detach_No_Lookup
     (Self    : Resource_Message'Class;
      Session : Session_Type)
     return Detached_Resource_Message'Class;
   function Detach_No_Lookup
     (Self    : Rule'Class;
      Session : Session_Type)
     return Detached_Rule'Class;
   function Detach_No_Lookup
     (Self    : Tool'Class;
      Session : Session_Type)
     return Detached_Tool'Class;
   --  Same as Detach, but does not check the session cache Same as Detach,
   --  but does not check the session cache Same as Detach, but does not check
   --  the session cache Same as Detach, but does not check the session cache
   --  Same as Detach, but does not check the session cache Same as Detach, but
   --  does not check the session cache Same as Detach, but does not check the
   --  session cache Same as Detach, but does not check the session cache Same
   --  as Detach, but does not check the session cache Same as Detach, but does
   --  not check the session cache

   procedure Do_Query_Entities
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Entities_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Messages_Properties
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Properties
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Resource_Trees
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Resources
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Resources_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Rules
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Tools
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Resource_Tree; Op2 : Resource_Tree) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Op1 : Detached_Resource_Tree;
      Op2 : Detached_Resource_Tree)
     return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Rule; Op2 : Rule) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_Rule; Op2 : Detached_Rule) return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Resource_Message; Op2 : Resource_Message) return Boolean
   is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Op1 : Detached_Resource_Message;
      Op2 : Detached_Resource_Message)
     return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Message; Op2 : Message) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_Message; Op2 : Detached_Message) return Boolean
   is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Entity; Op2 : Entity) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_Entity; Op2 : Detached_Entity) return Boolean
   is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Message_Property; Op2 : Message_Property) return Boolean
   is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Op1 : Detached_Message_Property;
      Op2 : Detached_Message_Property)
     return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Entity_Message; Op2 : Entity_Message) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Op1 : Detached_Entity_Message;
      Op2 : Detached_Entity_Message)
     return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Tool; Op2 : Tool) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_Tool; Op2 : Detached_Tool) return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Property; Op2 : Property) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Op1 : Detached_Property;
      Op2 : Detached_Property)
     return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Resource; Op2 : Resource) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Op1 : Detached_Resource;
      Op2 : Detached_Resource)
     return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   --------------
   -- Child_Id --
   --------------

   function Child_Id (Self : Resource_Tree) return Integer is
   begin
      return Integer_Value (Self, F_Resource_Trees_Child_Id);
   end Child_Id;

   --------------
   -- Child_Id --
   --------------

   function Child_Id (Self : Detached_Resource_Tree) return Integer is
   begin
      return Resource_Tree_Data (Self.Unchecked_Get).ORM_Child_Id;
   end Child_Id;

   --------------
   -- Child_Id --
   --------------

   function Child_Id (Self : Resource_Tree) return Resource'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 and then Self.Data.Follow_LJ then
         return I_Resources.Internal_Element
           (Self,
            Upto_Resource_Trees_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Child_Id";
         end if;

         return Filter (All_Resources, Id => Self.Child_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Child_Id;

   --------------
   -- Child_Id --
   --------------

   function Child_Id
     (Self : Detached_Resource_Tree)
     return Detached_Resource'Class
   is
      D : constant Resource_Tree_Data := Resource_Tree_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Child_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Child_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Child_Id := new Detached_Resource'Class'
           (Get_Resource (S, Id => D.ORM_Child_Id));
      end if;
      return D.ORM_FK_Child_Id.all;
   end Child_Id;

   ---------------
   -- Col_Begin --
   ---------------

   function Col_Begin (Self : Resource_Message) return Integer is
   begin
      return Integer_Value (Self, F_Resources_Messages_Col_Begin);
   end Col_Begin;

   ---------------
   -- Col_Begin --
   ---------------

   function Col_Begin (Self : Detached_Resource_Message) return Integer is
   begin
      return Resource_Message_Data (Self.Unchecked_Get).ORM_Col_Begin;
   end Col_Begin;

   ---------------
   -- Col_Begin --
   ---------------

   function Col_Begin (Self : Entity) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Col_Begin);
   end Col_Begin;

   ---------------
   -- Col_Begin --
   ---------------

   function Col_Begin (Self : Detached_Entity) return Integer is
   begin
      return Entity_Data (Self.Unchecked_Get).ORM_Col_Begin;
   end Col_Begin;

   ---------------
   -- Col_Begin --
   ---------------

   function Col_Begin (Self : Entity_Message) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Messages_Col_Begin);
   end Col_Begin;

   ---------------
   -- Col_Begin --
   ---------------

   function Col_Begin (Self : Detached_Entity_Message) return Integer is
   begin
      return Entity_Message_Data (Self.Unchecked_Get).ORM_Col_Begin;
   end Col_Begin;

   -------------
   -- Col_End --
   -------------

   function Col_End (Self : Resource_Message) return Integer is
   begin
      return Integer_Value (Self, F_Resources_Messages_Col_End);
   end Col_End;

   -------------
   -- Col_End --
   -------------

   function Col_End (Self : Detached_Resource_Message) return Integer is
   begin
      return Resource_Message_Data (Self.Unchecked_Get).ORM_Col_End;
   end Col_End;

   -------------
   -- Col_End --
   -------------

   function Col_End (Self : Entity) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Col_End);
   end Col_End;

   -------------
   -- Col_End --
   -------------

   function Col_End (Self : Detached_Entity) return Integer is
   begin
      return Entity_Data (Self.Unchecked_Get).ORM_Col_End;
   end Col_End;

   -------------
   -- Col_End --
   -------------

   function Col_End (Self : Entity_Message) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Messages_Col_End);
   end Col_End;

   -------------
   -- Col_End --
   -------------

   function Col_End (Self : Detached_Entity_Message) return Integer is
   begin
      return Entity_Message_Data (Self.Unchecked_Get).ORM_Col_End;
   end Col_End;

   ----------
   -- Data --
   ----------

   function Data (Self : Message) return String is
   begin
      return String_Value (Self, F_Messages_Data);
   end Data;

   ----------
   -- Data --
   ----------

   function Data (Self : Detached_Message) return String is
   begin
      return To_String (Message_Data (Self.Unchecked_Get).ORM_Data);
   end Data;

   ---------------
   -- Entity_Id --
   ---------------

   function Entity_Id (Self : Entity_Message) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Messages_Entity_Id);
   end Entity_Id;

   ---------------
   -- Entity_Id --
   ---------------

   function Entity_Id (Self : Detached_Entity_Message) return Integer is
   begin
      return Entity_Message_Data (Self.Unchecked_Get).ORM_Entity_Id;
   end Entity_Id;

   ---------------
   -- Entity_Id --
   ---------------

   function Entity_Id (Self : Entity_Message) return Entity'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Entities.Internal_Element
           (Self,
            Upto_Entities_Messages_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Entity_Id";
         end if;

         return Filter (All_Entities, Id => Self.Entity_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Entity_Id;

   ---------------
   -- Entity_Id --
   ---------------

   function Entity_Id
     (Self : Detached_Entity_Message)
     return Detached_Entity'Class
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Entity_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Entity_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Entity_Id := new Detached_Entity'Class'
           (Get_Entity (S, Id => D.ORM_Entity_Id));
      end if;
      return D.ORM_FK_Entity_Id.all;
   end Entity_Id;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message
     (Self : Message'Class)
     return Resources_Messages_Managers is
   begin
      return Filter (All_Resources_Messages, Message_Id => Self.Id);
   end Get_Message;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message
     (Self : Detached_Message'Class)
     return Resources_Messages_Managers is
   begin
      return Filter (All_Resources_Messages, Message_Id => Self.Id);
   end Get_Message;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message
     (Self : I_Messages_Managers'Class)
     return Resources_Messages_Managers
   is
      Q : constant SQL_Query := I_Messages.Build_Query(Self, +DBA.Messages.Id);
   begin
      return All_Resources_Messages.Filter
        (SQL_In(DBA.Resources_Messages.Message_Id, Q));
   end Get_Message;

   ------------------
   -- Get_Resource --
   ------------------

   function Get_Resource
     (Self : Resource'Class)
     return Resources_Messages_Managers is
   begin
      return Filter (All_Resources_Messages, Resource_Id => Self.Id);
   end Get_Resource;

   ------------------
   -- Get_Resource --
   ------------------

   function Get_Resource
     (Self : Detached_Resource'Class)
     return Resources_Messages_Managers is
   begin
      return Filter (All_Resources_Messages, Resource_Id => Self.Id);
   end Get_Resource;

   ------------------
   -- Get_Resource --
   ------------------

   function Get_Resource
     (Self : I_Resources_Managers'Class)
     return Resources_Messages_Managers
   is
      Q : constant SQL_Query := I_Resources.Build_Query(Self, +DBA.Resources.Id);
   begin
      return All_Resources_Messages.Filter
        (SQL_In(DBA.Resources_Messages.Resource_Id, Q));
   end Get_Resource;

   ------------------
   -- Get_Resource --
   ------------------

   function Get_Resource (Self : Resource'Class) return Entities_Managers is
   begin
      return Filter (All_Entities, Resource_Id => Self.Id);
   end Get_Resource;

   ------------------
   -- Get_Resource --
   ------------------

   function Get_Resource
     (Self : Detached_Resource'Class)
     return Entities_Managers is
   begin
      return Filter (All_Entities, Resource_Id => Self.Id);
   end Get_Resource;

   ------------------
   -- Get_Resource --
   ------------------

   function Get_Resource
     (Self : I_Resources_Managers'Class)
     return Entities_Managers
   is
      Q : constant SQL_Query := I_Resources.Build_Query(Self, +DBA.Resources.Id);
   begin
      return All_Entities.Filter
        (SQL_In(DBA.Entities.Resource_Id, Q));
   end Get_Resource;

   --------
   -- Id --
   --------

   function Id (Self : Resource_Tree) return Integer is
   begin
      return Integer_Value (Self, F_Resource_Trees_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Resource_Tree) return Integer is
   begin
      return Resource_Tree_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Rule) return Integer is
   begin
      return Integer_Value (Self, F_Rules_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Rule) return Integer is
   begin
      return Rule_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Resource_Message) return Integer is
   begin
      return Integer_Value (Self, F_Resources_Messages_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Resource_Message) return Integer is
   begin
      return Resource_Message_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Message) return Integer is
   begin
      return Integer_Value (Self, F_Messages_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Message) return Integer is
   begin
      return Message_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Entity) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Entity) return Integer is
   begin
      return Entity_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Message_Property) return Integer is
   begin
      return Integer_Value (Self, F_Messages_Properties_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Message_Property) return Integer is
   begin
      return Message_Property_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Entity_Message) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Messages_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Entity_Message) return Integer is
   begin
      return Entity_Message_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Tool) return Integer is
   begin
      return Integer_Value (Self, F_Tools_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Tool) return Integer is
   begin
      return Tool_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Property) return Integer is
   begin
      return Integer_Value (Self, F_Properties_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Property) return Integer is
   begin
      return Property_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Resource) return Integer is
   begin
      return Integer_Value (Self, F_Resources_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Resource) return Integer is
   begin
      return Resource_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Self : Rule) return String is
   begin
      return String_Value (Self, F_Rules_Identifier);
   end Identifier;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Self : Detached_Rule) return String is
   begin
      return To_String (Rule_Data (Self.Unchecked_Get).ORM_Identifier);
   end Identifier;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Self : Property) return String is
   begin
      return String_Value (Self, F_Properties_Identifier);
   end Identifier;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Self : Detached_Property) return String is
   begin
      return To_String (Property_Data (Self.Unchecked_Get).ORM_Identifier);
   end Identifier;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Rule) return Integer is
   begin
      return Integer_Value (Self, F_Rules_Kind);
   end Kind;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Detached_Rule) return Integer is
   begin
      return Rule_Data (Self.Unchecked_Get).ORM_Kind;
   end Kind;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Resource) return Integer is
   begin
      return Integer_Value (Self, F_Resources_Kind);
   end Kind;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Detached_Resource) return Integer is
   begin
      return Resource_Data (Self.Unchecked_Get).ORM_Kind;
   end Kind;

   ----------
   -- Line --
   ----------

   function Line (Self : Resource_Message) return Integer is
   begin
      return Integer_Value (Self, F_Resources_Messages_Line);
   end Line;

   ----------
   -- Line --
   ----------

   function Line (Self : Detached_Resource_Message) return Integer is
   begin
      return Resource_Message_Data (Self.Unchecked_Get).ORM_Line;
   end Line;

   ----------
   -- Line --
   ----------

   function Line (Self : Entity) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Line);
   end Line;

   ----------
   -- Line --
   ----------

   function Line (Self : Detached_Entity) return Integer is
   begin
      return Entity_Data (Self.Unchecked_Get).ORM_Line;
   end Line;

   ----------
   -- Line --
   ----------

   function Line (Self : Entity_Message) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Messages_Line);
   end Line;

   ----------
   -- Line --
   ----------

   function Line (Self : Detached_Entity_Message) return Integer is
   begin
      return Entity_Message_Data (Self.Unchecked_Get).ORM_Line;
   end Line;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id (Self : Resource_Message) return Integer is
   begin
      return Integer_Value (Self, F_Resources_Messages_Message_Id);
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id (Self : Detached_Resource_Message) return Integer is
   begin
      return Resource_Message_Data (Self.Unchecked_Get).ORM_Message_Id;
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id (Self : Resource_Message) return Message'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Messages.Internal_Element
           (Self,
            Upto_Resources_Messages_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Message_Id";
         end if;

         return Filter (All_Messages, Id => Self.Message_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id
     (Self : Detached_Resource_Message)
     return Detached_Message'Class
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Message_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Message_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Message_Id := new Detached_Message'Class'
           (Get_Message (S, Id => D.ORM_Message_Id));
      end if;
      return D.ORM_FK_Message_Id.all;
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id (Self : Message_Property) return Integer is
   begin
      return Integer_Value (Self, F_Messages_Properties_Message_Id);
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id (Self : Detached_Message_Property) return Integer is
   begin
      return Message_Property_Data (Self.Unchecked_Get).ORM_Message_Id;
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id (Self : Message_Property) return Message'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Messages.Internal_Element
           (Self,
            Upto_Messages_Properties_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Message_Id";
         end if;

         return Filter (All_Messages, Id => Self.Message_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id
     (Self : Detached_Message_Property)
     return Detached_Message'Class
   is
      D : constant Message_Property_Data := Message_Property_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Message_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Message_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Message_Id := new Detached_Message'Class'
           (Get_Message (S, Id => D.ORM_Message_Id));
      end if;
      return D.ORM_FK_Message_Id.all;
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id (Self : Entity_Message) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Messages_Message_Id);
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id (Self : Detached_Entity_Message) return Integer is
   begin
      return Entity_Message_Data (Self.Unchecked_Get).ORM_Message_Id;
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id (Self : Entity_Message) return Message'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Messages.Internal_Element
           (Self,
            Upto_Entities_Messages_1 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Message_Id";
         end if;

         return Filter (All_Messages, Id => Self.Message_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Message_Id;

   ----------------
   -- Message_Id --
   ----------------

   function Message_Id
     (Self : Detached_Entity_Message)
     return Detached_Message'Class
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Message_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Message_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Message_Id := new Detached_Message'Class'
           (Get_Message (S, Id => D.ORM_Message_Id));
      end if;
      return D.ORM_FK_Message_Id.all;
   end Message_Id;

   ----------
   -- Name --
   ----------

   function Name (Self : Rule) return String is
   begin
      return String_Value (Self, F_Rules_Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Detached_Rule) return String is
   begin
      return To_String (Rule_Data (Self.Unchecked_Get).ORM_Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Entity) return String is
   begin
      return String_Value (Self, F_Entities_Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Detached_Entity) return String is
   begin
      return To_String (Entity_Data (Self.Unchecked_Get).ORM_Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Tool) return String is
   begin
      return String_Value (Self, F_Tools_Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Detached_Tool) return String is
   begin
      return To_String (Tool_Data (Self.Unchecked_Get).ORM_Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Property) return String is
   begin
      return String_Value (Self, F_Properties_Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Detached_Property) return String is
   begin
      return To_String (Property_Data (Self.Unchecked_Get).ORM_Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Resource) return String is
   begin
      return String_Value (Self, F_Resources_Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Detached_Resource) return String is
   begin
      return To_String (Resource_Data (Self.Unchecked_Get).ORM_Name);
   end Name;

   ---------------
   -- Parent_Id --
   ---------------

   function Parent_Id (Self : Resource_Tree) return Integer is
   begin
      return Integer_Value (Self, F_Resource_Trees_Parent_Id);
   end Parent_Id;

   ---------------
   -- Parent_Id --
   ---------------

   function Parent_Id (Self : Detached_Resource_Tree) return Integer is
   begin
      return Resource_Tree_Data (Self.Unchecked_Get).ORM_Parent_Id;
   end Parent_Id;

   ---------------
   -- Parent_Id --
   ---------------

   function Parent_Id (Self : Resource_Tree) return Resource'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 and then Self.Data.Follow_LJ then
         return I_Resources.Internal_Element
           (Self,
            Upto_Resource_Trees_1 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Parent_Id";
         end if;

         return Filter (All_Resources, Id => Self.Parent_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Parent_Id;

   ---------------
   -- Parent_Id --
   ---------------

   function Parent_Id
     (Self : Detached_Resource_Tree)
     return Detached_Resource'Class
   is
      D : constant Resource_Tree_Data := Resource_Tree_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Parent_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Parent_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Parent_Id := new Detached_Resource'Class'
           (Get_Resource (S, Id => D.ORM_Parent_Id));
      end if;
      return D.ORM_FK_Parent_Id.all;
   end Parent_Id;

   -----------------
   -- Property_Id --
   -----------------

   function Property_Id (Self : Message_Property) return Integer is
   begin
      return Integer_Value (Self, F_Messages_Properties_Property_Id);
   end Property_Id;

   -----------------
   -- Property_Id --
   -----------------

   function Property_Id (Self : Detached_Message_Property) return Integer is
   begin
      return Message_Property_Data (Self.Unchecked_Get).ORM_Property_Id;
   end Property_Id;

   -----------------
   -- Property_Id --
   -----------------

   function Property_Id (Self : Message_Property) return Property'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Properties.Internal_Element
           (Self,
            Upto_Messages_Properties_1 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Property_Id";
         end if;

         return Filter (All_Properties, Id => Self.Property_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Property_Id;

   -----------------
   -- Property_Id --
   -----------------

   function Property_Id
     (Self : Detached_Message_Property)
     return Detached_Property'Class
   is
      D : constant Message_Property_Data := Message_Property_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Property_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Property_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Property_Id := new Detached_Property'Class'
           (Get_Property (S, Id => D.ORM_Property_Id));
      end if;
      return D.ORM_FK_Property_Id.all;
   end Property_Id;

   -------------
   -- Ranking --
   -------------

   function Ranking (Self : Message) return Integer is
   begin
      return Integer_Value (Self, F_Messages_Ranking);
   end Ranking;

   -------------
   -- Ranking --
   -------------

   function Ranking (Self : Detached_Message) return Integer is
   begin
      return Message_Data (Self.Unchecked_Get).ORM_Ranking;
   end Ranking;

   -----------------
   -- Resource_Id --
   -----------------

   function Resource_Id (Self : Resource_Message) return Integer is
   begin
      return Integer_Value (Self, F_Resources_Messages_Resource_Id);
   end Resource_Id;

   -----------------
   -- Resource_Id --
   -----------------

   function Resource_Id (Self : Detached_Resource_Message) return Integer is
   begin
      return Resource_Message_Data (Self.Unchecked_Get).ORM_Resource_Id;
   end Resource_Id;

   -----------------
   -- Resource_Id --
   -----------------

   function Resource_Id (Self : Resource_Message) return Resource'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Resources.Internal_Element
           (Self,
            Upto_Resources_Messages_1 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Resource_Id";
         end if;

         return Filter (All_Resources, Id => Self.Resource_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Resource_Id;

   -----------------
   -- Resource_Id --
   -----------------

   function Resource_Id
     (Self : Detached_Resource_Message)
     return Detached_Resource'Class
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Resource_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Resource_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Resource_Id := new Detached_Resource'Class'
           (Get_Resource (S, Id => D.ORM_Resource_Id));
      end if;
      return D.ORM_FK_Resource_Id.all;
   end Resource_Id;

   -----------------
   -- Resource_Id --
   -----------------

   function Resource_Id (Self : Entity) return Integer is
   begin
      return Integer_Value (Self, F_Entities_Resource_Id);
   end Resource_Id;

   -----------------
   -- Resource_Id --
   -----------------

   function Resource_Id (Self : Detached_Entity) return Integer is
   begin
      return Entity_Data (Self.Unchecked_Get).ORM_Resource_Id;
   end Resource_Id;

   -----------------
   -- Resource_Id --
   -----------------

   function Resource_Id (Self : Entity) return Resource'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Resources.Internal_Element
           (Self,
            Upto_Entities_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Resource_Id";
         end if;

         return Filter (All_Resources, Id => Self.Resource_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Resource_Id;

   -----------------
   -- Resource_Id --
   -----------------

   function Resource_Id (Self : Detached_Entity) return Detached_Resource'Class
   is
      D : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Resource_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Resource_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Resource_Id := new Detached_Resource'Class'
           (Get_Resource (S, Id => D.ORM_Resource_Id));
      end if;
      return D.ORM_FK_Resource_Id.all;
   end Resource_Id;

   -------------
   -- Rule_Id --
   -------------

   function Rule_Id (Self : Message) return Integer is
   begin
      return Integer_Value (Self, F_Messages_Rule_Id);
   end Rule_Id;

   -------------
   -- Rule_Id --
   -------------

   function Rule_Id (Self : Detached_Message) return Integer is
   begin
      return Message_Data (Self.Unchecked_Get).ORM_Rule_Id;
   end Rule_Id;

   -------------
   -- Rule_Id --
   -------------

   function Rule_Id (Self : Message) return Rule'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Rules.Internal_Element
           (Self,
            Upto_Messages_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Rule_Id";
         end if;

         return Filter (All_Rules, Id => Self.Rule_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Rule_Id;

   -------------
   -- Rule_Id --
   -------------

   function Rule_Id (Self : Detached_Message) return Detached_Rule'Class
   is
      D : constant Message_Data := Message_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Rule_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Rule_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Rule_Id := new Detached_Rule'Class'
           (Get_Rule (S, Id => D.ORM_Rule_Id));
      end if;
      return D.ORM_FK_Rule_Id.all;
   end Rule_Id;

   ---------------
   -- Timestamp --
   ---------------

   function Timestamp (Self : Resource) return Ada.Calendar.Time is
   begin
      return Time_Value (Self, F_Resources_Timestamp);
   end Timestamp;

   ---------------
   -- Timestamp --
   ---------------

   function Timestamp (Self : Detached_Resource) return Ada.Calendar.Time is
   begin
      return Resource_Data (Self.Unchecked_Get).ORM_Timestamp;
   end Timestamp;

   -------------
   -- Tool_Id --
   -------------

   function Tool_Id (Self : Rule) return Integer is
   begin
      return Integer_Value (Self, F_Rules_Tool_Id);
   end Tool_Id;

   -------------
   -- Tool_Id --
   -------------

   function Tool_Id (Self : Detached_Rule) return Integer is
   begin
      return Rule_Data (Self.Unchecked_Get).ORM_Tool_Id;
   end Tool_Id;

   -------------
   -- Tool_Id --
   -------------

   function Tool_Id (Self : Rule) return Tool'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Tools.Internal_Element
           (Self,
            Upto_Rules_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Tool_Id";
         end if;

         return Filter (All_Tools, Id => Self.Tool_Id)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Tool_Id;

   -------------
   -- Tool_Id --
   -------------

   function Tool_Id (Self : Detached_Rule) return Detached_Tool'Class
   is
      D : constant Rule_Data := Rule_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Tool_Id = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Tool_Id";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Tool_Id := new Detached_Tool'Class'
           (Get_Tool (S, Id => D.ORM_Tool_Id));
      end if;
      return D.ORM_FK_Tool_Id.all;
   end Tool_Id;

   -----------------
   -- Tool_Msg_Id --
   -----------------

   function Tool_Msg_Id (Self : Message) return Integer is
   begin
      return Integer_Value (Self, F_Messages_Tool_Msg_Id);
   end Tool_Msg_Id;

   -----------------
   -- Tool_Msg_Id --
   -----------------

   function Tool_Msg_Id (Self : Detached_Message) return Integer is
   begin
      return Message_Data (Self.Unchecked_Get).ORM_Tool_Msg_Id;
   end Tool_Msg_Id;

   ------------
   -- Detach --
   ------------

   function Detach
     (Self : Resource_Tree'Class)
     return Detached_Resource_Tree'Class
   is
      R : constant Detached_Resource_Tree'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Rule'Class) return Detached_Rule'Class
   is
      R : constant Detached_Rule'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach
     (Self : Resource_Message'Class)
     return Detached_Resource_Message'Class
   is
      R : constant Detached_Resource_Message'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Message'Class) return Detached_Message'Class
   is
      R : constant Detached_Message'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Entity'Class) return Detached_Entity'Class
   is
      R : constant Detached_Entity'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach
     (Self : Message_Property'Class)
     return Detached_Message_Property'Class
   is
      R : constant Detached_Message_Property'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach
     (Self : Entity_Message'Class)
     return Detached_Entity_Message'Class
   is
      R : constant Detached_Entity_Message'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Tool'Class) return Detached_Tool'Class
   is
      R : constant Detached_Tool'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Property'Class) return Detached_Property'Class
   is
      R : constant Detached_Property'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Resource'Class) return Detached_Resource'Class
   is
      R : constant Detached_Resource'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Entity'Class;
      Session : Session_Type)
     return Detached_Entity'Class
   is
      Default        : Detached_Entity;
      Result         : Detached_Entity'Class := Detached_Entity'Class (Session.Factory (Self, Default));
      Fk_Resource_Id : Detached_Resource_Access;
      Lj             : constant Boolean := Self.Data.Follow_LJ;
      Tmp            : Entity_Data;
   begin
      if Result.Is_Null then
         Result.Set (Entity_DDR'
              (Detached_Data with Field_Count => 7, others => <>));
      end if;

      Tmp := Entity_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         FK_Resource_Id := new Detached_Resource'Class'(
            I_Resources.Internal_Element
              (Self, Upto_Entities_0 (Self.Depth, LJ)).Detach);
      end if;

      Tmp.ORM_Col_Begin      := Integer_Value (Self, F_Entities_Col_Begin);
      Tmp.ORM_Col_End        := Integer_Value (Self, F_Entities_Col_End);
      Tmp.ORM_FK_Resource_Id := FK_Resource_Id;
      Tmp.ORM_Id             := Integer_Value (Self, F_Entities_Id);
      Tmp.ORM_Line           := Integer_Value (Self, F_Entities_Line);
      Tmp.ORM_Name           := To_Unbounded_String (String_Value (Self, F_Entities_Name));
      Tmp.ORM_Resource_Id    := Integer_Value (Self, F_Entities_Resource_Id);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Entity_Message'Class;
      Session : Session_Type)
     return Detached_Entity_Message'Class
   is
      Default       : Detached_Entity_Message;
      Result        : Detached_Entity_Message'Class := Detached_Entity_Message'Class (Session.Factory (Self, Default));
      Fk_Entity_Id  : Detached_Entity_Access;
      Fk_Message_Id : Detached_Message_Access;
      Lj            : constant Boolean := Self.Data.Follow_LJ;
      Tmp           : Entity_Message_Data;
   begin
      if Result.Is_Null then
         Result.Set (Entity_Message_DDR'
              (Detached_Data with Field_Count => 8, others => <>));
      end if;

      Tmp := Entity_Message_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         FK_Entity_Id := new Detached_Entity'Class'(
            I_Entities.Internal_Element
              (Self, Upto_Entities_Messages_0 (Self.Depth, LJ)).Detach);
         FK_Message_Id := new Detached_Message'Class'(
            I_Messages.Internal_Element
              (Self, Upto_Entities_Messages_1 (Self.Depth, LJ)).Detach);
      end if;

      Tmp.ORM_Col_Begin     := Integer_Value (Self, F_Entities_Messages_Col_Begin);
      Tmp.ORM_Col_End       := Integer_Value (Self, F_Entities_Messages_Col_End);
      Tmp.ORM_Entity_Id     := Integer_Value (Self, F_Entities_Messages_Entity_Id);
      Tmp.ORM_FK_Entity_Id  := FK_Entity_Id;
      Tmp.ORM_FK_Message_Id := FK_Message_Id;
      Tmp.ORM_Id            := Integer_Value (Self, F_Entities_Messages_Id);
      Tmp.ORM_Line          := Integer_Value (Self, F_Entities_Messages_Line);
      Tmp.ORM_Message_Id    := Integer_Value (Self, F_Entities_Messages_Message_Id);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Message'Class;
      Session : Session_Type)
     return Detached_Message'Class
   is
      Default    : Detached_Message;
      Result     : Detached_Message'Class := Detached_Message'Class (Session.Factory (Self, Default));
      Fk_Rule_Id : Detached_Rule_Access;
      Lj         : constant Boolean := Self.Data.Follow_LJ;
      Tmp        : Message_Data;
   begin
      if Result.Is_Null then
         Result.Set (Message_DDR'
              (Detached_Data with Field_Count => 6, others => <>));
      end if;

      Tmp := Message_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         FK_Rule_Id := new Detached_Rule'Class'(
            I_Rules.Internal_Element
              (Self, Upto_Messages_0 (Self.Depth, LJ)).Detach);
      end if;

      Tmp.ORM_Data           := To_Unbounded_String (String_Value (Self, F_Messages_Data));
      Tmp.ORM_FK_Rule_Id     := FK_Rule_Id;
      Tmp.ORM_Id             := Integer_Value (Self, F_Messages_Id);
      Tmp.ORM_Ranking        := Integer_Value (Self, F_Messages_Ranking);
      Tmp.ORM_Rule_Id        := Integer_Value (Self, F_Messages_Rule_Id);
      Tmp.ORM_Tool_Msg_Id    := Integer_Value (Self, F_Messages_Tool_Msg_Id);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Message_Property'Class;
      Session : Session_Type)
     return Detached_Message_Property'Class
   is
      Default        : Detached_Message_Property;
      Result         : Detached_Message_Property'Class := Detached_Message_Property'Class (Session.Factory (Self, Default));
      Fk_Message_Id  : Detached_Message_Access;
      Fk_Property_Id : Detached_Property_Access;
      Lj             : constant Boolean := Self.Data.Follow_LJ;
      Tmp            : Message_Property_Data;
   begin
      if Result.Is_Null then
         Result.Set (Message_Property_DDR'
              (Detached_Data with Field_Count => 5, others => <>));
      end if;

      Tmp := Message_Property_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         FK_Message_Id := new Detached_Message'Class'(
            I_Messages.Internal_Element
              (Self, Upto_Messages_Properties_0 (Self.Depth, LJ)).Detach);
         FK_Property_Id := new Detached_Property'Class'(
            I_Properties.Internal_Element
              (Self, Upto_Messages_Properties_1 (Self.Depth, LJ)).Detach);
      end if;

      Tmp.ORM_FK_Message_Id  := FK_Message_Id;
      Tmp.ORM_FK_Property_Id := FK_Property_Id;
      Tmp.ORM_Id             := Integer_Value (Self, F_Messages_Properties_Id);
      Tmp.ORM_Message_Id     := Integer_Value (Self, F_Messages_Properties_Message_Id);
      Tmp.ORM_Property_Id    := Integer_Value (Self, F_Messages_Properties_Property_Id);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Property'Class;
      Session : Session_Type)
     return Detached_Property'Class
   is
      Default : Detached_Property;
      Result  : Detached_Property'Class := Detached_Property'Class (Session.Factory (Self, Default));
      Tmp     : Property_Data;
   begin
      if Result.Is_Null then
         Result.Set (Property_DDR'
              (Detached_Data with Field_Count => 3, others => <>));
      end if;

      Tmp := Property_Data (Result.Unchecked_Get);

      Tmp.ORM_Id            := Integer_Value (Self, F_Properties_Id);
      Tmp.ORM_Identifier    := To_Unbounded_String (String_Value (Self, F_Properties_Identifier));
      Tmp.ORM_Name          := To_Unbounded_String (String_Value (Self, F_Properties_Name));
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Resource_Tree'Class;
      Session : Session_Type)
     return Detached_Resource_Tree'Class
   is
      Default      : Detached_Resource_Tree;
      Result       : Detached_Resource_Tree'Class := Detached_Resource_Tree'Class (Session.Factory (Self, Default));
      Fk_Child_Id  : Detached_Resource_Access;
      Fk_Parent_Id : Detached_Resource_Access;
      Lj           : constant Boolean := Self.Data.Follow_LJ;
      Tmp          : Resource_Tree_Data;
   begin
      if Result.Is_Null then
         Result.Set (Resource_Tree_DDR'
              (Detached_Data with Field_Count => 5, others => <>));
      end if;

      Tmp := Resource_Tree_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         if LJ then
            FK_Child_Id := new Detached_Resource'Class'(
               I_Resources.Internal_Element
                 (Self, Upto_Resource_Trees_0 (Self.Depth, LJ)).Detach);
         end if;

         if LJ then
            FK_Parent_Id := new Detached_Resource'Class'(
               I_Resources.Internal_Element
                 (Self, Upto_Resource_Trees_1 (Self.Depth, LJ)).Detach);
         end if;

      end if;

      Tmp.ORM_Child_Id     := Integer_Value (Self, F_Resource_Trees_Child_Id);
      Tmp.ORM_FK_Child_Id  := FK_Child_Id;
      Tmp.ORM_FK_Parent_Id := FK_Parent_Id;
      Tmp.ORM_Id           := Integer_Value (Self, F_Resource_Trees_Id);
      Tmp.ORM_Parent_Id    := Integer_Value (Self, F_Resource_Trees_Parent_Id);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Resource'Class;
      Session : Session_Type)
     return Detached_Resource'Class
   is
      Default : Detached_Resource;
      Result  : Detached_Resource'Class := Detached_Resource'Class (Session.Factory (Self, Default));
      Tmp     : Resource_Data;
   begin
      if Result.Is_Null then
         Result.Set (Resource_DDR'
              (Detached_Data with Field_Count => 4, others => <>));
      end if;

      Tmp := Resource_Data (Result.Unchecked_Get);

      Tmp.ORM_Id           := Integer_Value (Self, F_Resources_Id);
      Tmp.ORM_Kind         := Integer_Value (Self, F_Resources_Kind);
      Tmp.ORM_Name         := To_Unbounded_String (String_Value (Self, F_Resources_Name));
      Tmp.ORM_Timestamp    := Time_Value (Self, F_Resources_Timestamp);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Resource_Message'Class;
      Session : Session_Type)
     return Detached_Resource_Message'Class
   is
      Default        : Detached_Resource_Message;
      Result         : Detached_Resource_Message'Class := Detached_Resource_Message'Class (Session.Factory (Self, Default));
      Fk_Message_Id  : Detached_Message_Access;
      Fk_Resource_Id : Detached_Resource_Access;
      Lj             : constant Boolean := Self.Data.Follow_LJ;
      Tmp            : Resource_Message_Data;
   begin
      if Result.Is_Null then
         Result.Set (Resource_Message_DDR'
              (Detached_Data with Field_Count => 8, others => <>));
      end if;

      Tmp := Resource_Message_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         FK_Message_Id := new Detached_Message'Class'(
            I_Messages.Internal_Element
              (Self, Upto_Resources_Messages_0 (Self.Depth, LJ)).Detach);
         FK_Resource_Id := new Detached_Resource'Class'(
            I_Resources.Internal_Element
              (Self, Upto_Resources_Messages_1 (Self.Depth, LJ)).Detach);
      end if;

      Tmp.ORM_Col_Begin      := Integer_Value (Self, F_Resources_Messages_Col_Begin);
      Tmp.ORM_Col_End        := Integer_Value (Self, F_Resources_Messages_Col_End);
      Tmp.ORM_FK_Message_Id  := FK_Message_Id;
      Tmp.ORM_FK_Resource_Id := FK_Resource_Id;
      Tmp.ORM_Id             := Integer_Value (Self, F_Resources_Messages_Id);
      Tmp.ORM_Line           := Integer_Value (Self, F_Resources_Messages_Line);
      Tmp.ORM_Message_Id     := Integer_Value (Self, F_Resources_Messages_Message_Id);
      Tmp.ORM_Resource_Id    := Integer_Value (Self, F_Resources_Messages_Resource_Id);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Rule'Class;
      Session : Session_Type)
     return Detached_Rule'Class
   is
      Default    : Detached_Rule;
      Result     : Detached_Rule'Class := Detached_Rule'Class (Session.Factory (Self, Default));
      Fk_Tool_Id : Detached_Tool_Access;
      Lj         : constant Boolean := Self.Data.Follow_LJ;
      Tmp        : Rule_Data;
   begin
      if Result.Is_Null then
         Result.Set (Rule_DDR'
              (Detached_Data with Field_Count => 6, others => <>));
      end if;

      Tmp := Rule_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         FK_Tool_Id := new Detached_Tool'Class'(
            I_Tools.Internal_Element
              (Self, Upto_Rules_0 (Self.Depth, LJ)).Detach);
      end if;

      Tmp.ORM_FK_Tool_Id    := FK_Tool_Id;
      Tmp.ORM_Id            := Integer_Value (Self, F_Rules_Id);
      Tmp.ORM_Identifier    := To_Unbounded_String (String_Value (Self, F_Rules_Identifier));
      Tmp.ORM_Kind          := Integer_Value (Self, F_Rules_Kind);
      Tmp.ORM_Name          := To_Unbounded_String (String_Value (Self, F_Rules_Name));
      Tmp.ORM_Tool_Id       := Integer_Value (Self, F_Rules_Tool_Id);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Tool'Class;
      Session : Session_Type)
     return Detached_Tool'Class
   is
      Default : Detached_Tool;
      Result  : Detached_Tool'Class := Detached_Tool'Class (Session.Factory (Self, Default));
      Tmp     : Tool_Data;
   begin
      if Result.Is_Null then
         Result.Set (Tool_DDR'
              (Detached_Data with Field_Count => 2, others => <>));
      end if;

      Tmp := Tool_Data (Result.Unchecked_Get);

      Tmp.ORM_Id      := Integer_Value (Self, F_Tools_Id);
      Tmp.ORM_Name    := To_Unbounded_String (String_Value (Self, F_Tools_Name));
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   -----------------------
   -- Do_Query_Entities --
   -----------------------

   procedure Do_Query_Entities
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Entities(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Name
         & Table.Line
         & Table.Col_Begin
         & Table.Col_End
         & Table.Resource_Id;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Resources(Aliases(Aliases(Base + 1)));
         begin Criteria := Criteria
         and Table.Resource_Id = FK1.Id;
         From := +Table;
         C2 := No_Criteria;
         Do_Query_Resources(Fields, T, C2,Aliases(Base + 1),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;
      end;
   end if;
   end Do_Query_Entities;

   --------------------------------
   -- Do_Query_Entities_Messages --
   --------------------------------

   procedure Do_Query_Entities_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Entities_Messages(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Entity_Id
         & Table.Message_Id
         & Table.Line
         & Table.Col_Begin
         & Table.Col_End;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Entities(Aliases(Aliases(Base + 1)));
            FK2 : T_Numbered_Messages(Aliases(Aliases(Base + 2)));
         begin Criteria := Criteria
         and Table.Entity_Id = FK1.Id
         and Table.Message_Id = FK2.Id;
         From := +Table;
         C2 := No_Criteria;
         Do_Query_Entities(Fields, T, C2,Aliases(Base + 1),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;

         C2 := No_Criteria;
         Do_Query_Messages(Fields, T, C2,Aliases(Base + 2),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;
      end;
   end if;
   end Do_Query_Entities_Messages;

   -----------------------
   -- Do_Query_Messages --
   -----------------------

   procedure Do_Query_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Messages(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Rule_Id
         & Table.Data
         & Table.Ranking
         & Table.Tool_Msg_Id;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Rules(Aliases(Aliases(Base + 1)));
         begin Criteria := Criteria
         and Table.Rule_Id = FK1.Id;
         From := +Table;
         C2 := No_Criteria;
         Do_Query_Rules(Fields, T, C2,Aliases(Base + 1),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;
      end;
   end if;
   end Do_Query_Messages;

   ----------------------------------
   -- Do_Query_Messages_Properties --
   ----------------------------------

   procedure Do_Query_Messages_Properties
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Messages_Properties(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Message_Id
         & Table.Property_Id;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Messages(Aliases(Aliases(Base + 1)));
            FK2 : T_Numbered_Properties(Aliases(Aliases(Base + 2)));
         begin Criteria := Criteria
         and Table.Message_Id = FK1.Id
         and Table.Property_Id = FK2.Id;
         From := +Table;
         C2 := No_Criteria;
         Do_Query_Messages(Fields, T, C2,Aliases(Base + 1),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;

         C2 := No_Criteria;
         Do_Query_Properties(Fields, T, C2,Aliases(Base + 2),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;
      end;
   end if;
   end Do_Query_Messages_Properties;

   -------------------------
   -- Do_Query_Properties --
   -------------------------

   procedure Do_Query_Properties
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      pragma Unreferenced (Criteria, Depth, Follow_LJ);
      Table : T_Numbered_Properties(Aliases(Base));
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Identifier
         & Table.Name;
      end if;
      From := Empty_Table_List;
   end Do_Query_Properties;

   -----------------------------
   -- Do_Query_Resource_Trees --
   -----------------------------

   procedure Do_Query_Resource_Trees
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Resource_Trees(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Child_Id
         & Table.Parent_Id;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Resources(Aliases(Aliases(Base + 1)));
            FK2 : T_Numbered_Resources(Aliases(Aliases(Base + 2)));
         begin if Follow_LJ then
            From := +Left_Join(Left_Join(Table, FK1, Table.Child_Id=FK1.Id), FK2, Table.Parent_Id=FK2.Id);
         else
            From := +Table;
         end if;
         if Follow_LJ then
            C2 := No_Criteria;
            Do_Query_Resources(Fields, T, C2,Aliases(Base + 1),
               Aliases, Depth - 1, Follow_LJ);
            if Depth > 1 then
               Criteria := Criteria and C2;
            end if;
         end if;

         if Follow_LJ then
            C2 := No_Criteria;
            Do_Query_Resources(Fields, T, C2,Aliases(Base + 2),
               Aliases, Depth - 1, Follow_LJ);
            if Depth > 1 then
               Criteria := Criteria and C2;
            end if;
         end if;
      end;
   end if;
   end Do_Query_Resource_Trees;

   ------------------------
   -- Do_Query_Resources --
   ------------------------

   procedure Do_Query_Resources
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      pragma Unreferenced (Criteria, Depth, Follow_LJ);
      Table : T_Numbered_Resources(Aliases(Base));
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Name
         & Table.Kind
         & Table.Timestamp;
      end if;
      From := Empty_Table_List;
   end Do_Query_Resources;

   ---------------------------------
   -- Do_Query_Resources_Messages --
   ---------------------------------

   procedure Do_Query_Resources_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Resources_Messages(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Message_Id
         & Table.Resource_Id
         & Table.Line
         & Table.Col_Begin
         & Table.Col_End;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Messages(Aliases(Aliases(Base + 1)));
            FK2 : T_Numbered_Resources(Aliases(Aliases(Base + 2)));
         begin Criteria := Criteria
         and Table.Message_Id = FK1.Id
         and Table.Resource_Id = FK2.Id;
         From := +Table;
         C2 := No_Criteria;
         Do_Query_Messages(Fields, T, C2,Aliases(Base + 1),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;

         C2 := No_Criteria;
         Do_Query_Resources(Fields, T, C2,Aliases(Base + 2),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;
      end;
   end if;
   end Do_Query_Resources_Messages;

   --------------------
   -- Do_Query_Rules --
   --------------------

   procedure Do_Query_Rules
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Rules(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Name
         & Table.Identifier
         & Table.Kind
         & Table.Tool_Id;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Tools(Aliases(Aliases(Base + 1)));
         begin Criteria := Criteria
         and Table.Tool_Id = FK1.Id;
         From := +Table;
         C2 := No_Criteria;
         Do_Query_Tools(Fields, T, C2,Aliases(Base + 1),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;
      end;
   end if;
   end Do_Query_Rules;

   --------------------
   -- Do_Query_Tools --
   --------------------

   procedure Do_Query_Tools
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      pragma Unreferenced (Criteria, Depth, Follow_LJ);
      Table : T_Numbered_Tools(Aliases(Base));
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Name;
      end if;
      From := Empty_Table_List;
   end Do_Query_Tools;

   ---------------------
   -- Entity_Messages --
   ---------------------

   function Entity_Messages
     (Self : Entity'Class)
     return Entities_Messages_Managers is
   begin
      return Filter (All_Entities_Messages, Entity_Id => Self.Id);
   end Entity_Messages;

   ---------------------
   -- Entity_Messages --
   ---------------------

   function Entity_Messages
     (Self : Detached_Entity'Class)
     return Entities_Messages_Managers is
   begin
      return Filter (All_Entities_Messages, Entity_Id => Self.Id);
   end Entity_Messages;

   ---------------------
   -- Entity_Messages --
   ---------------------

   function Entity_Messages
     (Self : I_Entities_Managers'Class)
     return Entities_Messages_Managers
   is
      Q : constant SQL_Query := I_Entities.Build_Query(Self, +DBA.Entities.Id);
   begin
      return All_Entities_Messages.Filter
        (SQL_In(DBA.Entities_Messages.Entity_Id, Q));
   end Entity_Messages;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self      : Resource_Trees_Managers'Class;
      Id        : Integer := -1;
      Child_Id  : Integer := -1;
      Parent_Id : Integer := -1)
     return Resource_Trees_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Resource_Trees_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Resource_Trees.Id = Id;
      end if;
      if Child_Id /= -1 then
         C := C and DBA.Resource_Trees.Child_Id = Child_Id;
      end if;
      if Parent_Id /= -1 then
         C := C and DBA.Resource_Trees.Parent_Id = Parent_Id;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self       : Rules_Managers'Class;
      Id         : Integer := -1;
      Name       : String := No_Update;
      Identifier : String := No_Update;
      Kind       : Integer := -1;
      Tool_Id    : Integer := -1)
     return Rules_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Rules_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Rules.Id = Id;
      end if;
      if Name /= No_Update then
         C := C and DBA.Rules.Name = Name;
      end if;
      if Identifier /= No_Update then
         C := C and DBA.Rules.Identifier = Identifier;
      end if;
      if Kind /= -1 then
         C := C and DBA.Rules.Kind = Kind;
      end if;
      if Tool_Id /= -1 then
         C := C and DBA.Rules.Tool_Id = Tool_Id;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self        : Resources_Messages_Managers'Class;
      Id          : Integer := -1;
      Message_Id  : Integer := -1;
      Resource_Id : Integer := -1;
      Line        : Integer := -1;
      Col_Begin   : Integer := -1;
      Col_End     : Integer := -1)
     return Resources_Messages_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Resources_Messages_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Resources_Messages.Id = Id;
      end if;
      if Message_Id /= -1 then
         C := C and DBA.Resources_Messages.Message_Id = Message_Id;
      end if;
      if Resource_Id /= -1 then
         C := C and DBA.Resources_Messages.Resource_Id = Resource_Id;
      end if;
      if Line /= -1 then
         C := C and DBA.Resources_Messages.Line = Line;
      end if;
      if Col_Begin /= -1 then
         C := C and DBA.Resources_Messages.Col_Begin = Col_Begin;
      end if;
      if Col_End /= -1 then
         C := C and DBA.Resources_Messages.Col_End = Col_End;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self        : Messages_Managers'Class;
      Id          : Integer := -1;
      Rule_Id     : Integer := -1;
      Data        : String := No_Update;
      Ranking     : Integer := -1;
      Tool_Msg_Id : Integer := -1)
     return Messages_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Messages_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Messages.Id = Id;
      end if;
      if Rule_Id /= -1 then
         C := C and DBA.Messages.Rule_Id = Rule_Id;
      end if;
      if Data /= No_Update then
         C := C and DBA.Messages.Data = Data;
      end if;
      if Ranking /= -1 then
         C := C and DBA.Messages.Ranking = Ranking;
      end if;
      if Tool_Msg_Id /= -1 then
         C := C and DBA.Messages.Tool_Msg_Id = Tool_Msg_Id;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self        : Entities_Managers'Class;
      Id          : Integer := -1;
      Name        : String := No_Update;
      Line        : Integer := -1;
      Col_Begin   : Integer := -1;
      Col_End     : Integer := -1;
      Resource_Id : Integer := -1)
     return Entities_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Entities_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Entities.Id = Id;
      end if;
      if Name /= No_Update then
         C := C and DBA.Entities.Name = Name;
      end if;
      if Line /= -1 then
         C := C and DBA.Entities.Line = Line;
      end if;
      if Col_Begin /= -1 then
         C := C and DBA.Entities.Col_Begin = Col_Begin;
      end if;
      if Col_End /= -1 then
         C := C and DBA.Entities.Col_End = Col_End;
      end if;
      if Resource_Id /= -1 then
         C := C and DBA.Entities.Resource_Id = Resource_Id;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self        : Messages_Properties_Managers'Class;
      Id          : Integer := -1;
      Message_Id  : Integer := -1;
      Property_Id : Integer := -1)
     return Messages_Properties_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Messages_Properties_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Messages_Properties.Id = Id;
      end if;
      if Message_Id /= -1 then
         C := C and DBA.Messages_Properties.Message_Id = Message_Id;
      end if;
      if Property_Id /= -1 then
         C := C and DBA.Messages_Properties.Property_Id = Property_Id;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self       : Entities_Messages_Managers'Class;
      Id         : Integer := -1;
      Entity_Id  : Integer := -1;
      Message_Id : Integer := -1;
      Line       : Integer := -1;
      Col_Begin  : Integer := -1;
      Col_End    : Integer := -1)
     return Entities_Messages_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Entities_Messages_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Entities_Messages.Id = Id;
      end if;
      if Entity_Id /= -1 then
         C := C and DBA.Entities_Messages.Entity_Id = Entity_Id;
      end if;
      if Message_Id /= -1 then
         C := C and DBA.Entities_Messages.Message_Id = Message_Id;
      end if;
      if Line /= -1 then
         C := C and DBA.Entities_Messages.Line = Line;
      end if;
      if Col_Begin /= -1 then
         C := C and DBA.Entities_Messages.Col_Begin = Col_Begin;
      end if;
      if Col_End /= -1 then
         C := C and DBA.Entities_Messages.Col_End = Col_End;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self : Tools_Managers'Class;
      Id   : Integer := -1;
      Name : String := No_Update)
     return Tools_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Tools_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Tools.Id = Id;
      end if;
      if Name /= No_Update then
         C := C and DBA.Tools.Name = Name;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self       : Properties_Managers'Class;
      Id         : Integer := -1;
      Identifier : String := No_Update;
      Name       : String := No_Update)
     return Properties_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Properties_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Properties.Id = Id;
      end if;
      if Identifier /= No_Update then
         C := C and DBA.Properties.Identifier = Identifier;
      end if;
      if Name /= No_Update then
         C := C and DBA.Properties.Name = Name;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self      : Resources_Managers'Class;
      Id        : Integer := -1;
      Name      : String := No_Update;
      Kind      : Integer := -1;
      Timestamp : Ada.Calendar.Time := No_Time)
     return Resources_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Resources_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Resources.Id = Id;
      end if;
      if Name /= No_Update then
         C := C and DBA.Resources.Name = Name;
      end if;
      if Kind /= -1 then
         C := C and DBA.Resources.Kind = Kind;
      end if;
      if Timestamp /= No_Time then
         C := C and DBA.Resources.Timestamp = Timestamp;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Entity_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Resource_Id);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Entity_Message_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Entity_Id);
      Unchecked_Free (Self.ORM_FK_Message_Id);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Message_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Rule_Id);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Message_Property_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Message_Id);
      Unchecked_Free (Self.ORM_FK_Property_Id);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Property_Ddr) is
   begin
      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Resource_Tree_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Child_Id);
      Unchecked_Free (Self.ORM_FK_Parent_Id);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Resource_Ddr) is
   begin
      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Resource_Message_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Message_Id);
      Unchecked_Free (Self.ORM_FK_Resource_Id);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Rule_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Tool_Id);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Tool_Ddr) is
   begin
      Free (Detached_Data (Self));
   end Free;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Resource_Tree'Class is
   begin
      return Detached_Resource_Tree'Class (Session.From_Cache ((4000000, Id), No_Detached_Resource_Tree));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Rule'Class is
   begin
      return Detached_Rule'Class (Session.From_Cache ((1000000, Id), No_Detached_Rule));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Resource_Message'Class is
   begin
      return Detached_Resource_Message'Class (Session.From_Cache ((5000000, Id), No_Detached_Resource_Message));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Message'Class is
   begin
      return Detached_Message'Class (Session.From_Cache ((2000000, Id), No_Detached_Message));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Entity'Class is
   begin
      return Detached_Entity'Class (Session.From_Cache ((6000000, Id), No_Detached_Entity));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Message_Property'Class is
   begin
      return Detached_Message_Property'Class (Session.From_Cache ((9000000, Id), No_Detached_Message_Property));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Entity_Message'Class is
   begin
      return Detached_Entity_Message'Class (Session.From_Cache ((7000000, Id), No_Detached_Entity_Message));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Tool'Class is
   begin
      return Detached_Tool'Class (Session.From_Cache ((0, Id), No_Detached_Tool));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Property'Class is
   begin
      return Detached_Property'Class (Session.From_Cache ((8000000, Id), No_Detached_Property));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Resource'Class is
   begin
      return Detached_Resource'Class (Session.From_Cache ((3000000, Id), No_Detached_Resource));
   end From_Cache;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Entity'Class
   is
      R : constant Detached_Entity'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Entities_Managers := Filter
              (All_Entities,
               Id => Id);
            L : I_Entities.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Entity;
            else

               declare
                  E : constant Entity := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Entities.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Entity;

   ------------------------
   -- Get_Entity_Message --
   ------------------------

   function Get_Entity_Message
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Entity_Message'Class
   is
      R : constant Detached_Entity_Message'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Entities_Messages_Managers := Filter
              (All_Entities_Messages,
               Id => Id);
            L : I_Entities_Messages.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Entity_Message;
            else

               declare
                  E : constant Entity_Message := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Entities_Messages.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Entity_Message;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Message'Class
   is
      R : constant Detached_Message'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Messages_Managers := Filter
              (All_Messages,
               Id => Id);
            L : I_Messages.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Message;
            else

               declare
                  E : constant Message := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Messages.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Message;

   --------------------------
   -- Get_Message_Property --
   --------------------------

   function Get_Message_Property
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Message_Property'Class
   is
      R : constant Detached_Message_Property'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Messages_Properties_Managers := Filter
              (All_Messages_Properties,
               Id => Id);
            L : I_Messages_Properties.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Message_Property;
            else

               declare
                  E : constant Message_Property := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Messages_Properties.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Message_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Property'Class
   is
      R : constant Detached_Property'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Properties_Managers := Filter
              (All_Properties,
               Id => Id);
            L : I_Properties.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Property;
            else

               declare
                  E : constant Property := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Properties.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Property;

   ------------------
   -- Get_Resource --
   ------------------

   function Get_Resource
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Resource'Class
   is
      R : constant Detached_Resource'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Resources_Managers := Filter
              (All_Resources,
               Id => Id);
            L : I_Resources.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Resource;
            else

               declare
                  E : constant Resource := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Resources.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Resource;

   --------------------------
   -- Get_Resource_Message --
   --------------------------

   function Get_Resource_Message
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Resource_Message'Class
   is
      R : constant Detached_Resource_Message'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Resources_Messages_Managers := Filter
              (All_Resources_Messages,
               Id => Id);
            L : I_Resources_Messages.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Resource_Message;
            else

               declare
                  E : constant Resource_Message := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Resources_Messages.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Resource_Message;

   -----------------------
   -- Get_Resource_Tree --
   -----------------------

   function Get_Resource_Tree
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Resource_Tree'Class
   is
      R : constant Detached_Resource_Tree'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Resource_Trees_Managers := Filter
              (All_Resource_Trees,
               Id => Id);
            L : I_Resource_Trees.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Resource_Tree;
            else

               declare
                  E : constant Resource_Tree := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Resource_Trees.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Resource_Tree;

   --------------
   -- Get_Rule --
   --------------

   function Get_Rule
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Rule'Class
   is
      R : constant Detached_Rule'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Rules_Managers := Filter
              (All_Rules,
               Id => Id);
            L : I_Rules.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Rule;
            else

               declare
                  E : constant Rule := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Rules.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Rule;

   --------------
   -- Get_Tool --
   --------------

   function Get_Tool
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Tool'Class
   is
      R : constant Detached_Tool'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Tools_Managers := Filter
              (All_Tools,
               Id => Id);
            L : I_Tools.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Tool;
            else

               declare
                  E : constant Tool := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Tools.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Tool;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Entity;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Entities.Name = To_String (D.ORM_Name));
      end if;
      if Mask (3) then
         A := A & (DBA.Entities.Line = D.ORM_Line);
      end if;
      if Mask (4) then
         A := A & (DBA.Entities.Col_Begin = D.ORM_Col_Begin);
      end if;
      if Mask (5) then
         A := A & (DBA.Entities.Col_End = D.ORM_Col_End);
      end if;
      if Mask (6) then
         if D.ORM_Resource_Id /= -1 then
            A := A & (DBA.Entities.Resource_Id = D.ORM_Resource_Id);
         else

            declare
               D2 : constant Resource_Data :=
               Resource_data (D.ORM_FK_Resource_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Resource_Id.all);
               end if;

               A := A & (DBA.Entities.Resource_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Entities, A, DBA.Entities.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Entities.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Entity_Message;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         if D.ORM_Entity_Id /= -1 then
            A := A & (DBA.Entities_Messages.Entity_Id = D.ORM_Entity_Id);
         else

            declare
               D2 : constant Entity_Data :=
               Entity_data (D.ORM_FK_Entity_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Entity_Id.all);
               end if;

               A := A & (DBA.Entities_Messages.Entity_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (3) then
         if D.ORM_Message_Id /= -1 then
            A := A & (DBA.Entities_Messages.Message_Id = D.ORM_Message_Id);
         else

            declare
               D2 : constant Message_Data :=
               Message_data (D.ORM_FK_Message_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Message_Id.all);
               end if;

               A := A & (DBA.Entities_Messages.Message_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (4) then
         A := A & (DBA.Entities_Messages.Line = D.ORM_Line);
      end if;
      if Mask (5) then
         A := A & (DBA.Entities_Messages.Col_Begin = D.ORM_Col_Begin);
      end if;
      if Mask (6) then
         A := A & (DBA.Entities_Messages.Col_End = D.ORM_Col_End);
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Entities_Messages, A, DBA.Entities_Messages.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Entities_Messages.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Message;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Message_Data := Message_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         if D.ORM_Rule_Id /= -1 then
            A := A & (DBA.Messages.Rule_Id = D.ORM_Rule_Id);
         else

            declare
               D2 : constant Rule_Data :=
               Rule_data (D.ORM_FK_Rule_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Rule_Id.all);
               end if;

               A := A & (DBA.Messages.Rule_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (3) then
         A := A & (DBA.Messages.Data = To_String (D.ORM_Data));
      end if;
      if Mask (4) then
         A := A & (DBA.Messages.Ranking = D.ORM_Ranking);
      end if;
      if Mask (5) then
         A := A & (DBA.Messages.Tool_Msg_Id = D.ORM_Tool_Msg_Id);
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Messages, A, DBA.Messages.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Messages.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Message_Property;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Message_Property_Data := Message_Property_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         if D.ORM_Message_Id /= -1 then
            A := A & (DBA.Messages_Properties.Message_Id = D.ORM_Message_Id);
         else

            declare
               D2 : constant Message_Data :=
               Message_data (D.ORM_FK_Message_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Message_Id.all);
               end if;

               A := A & (DBA.Messages_Properties.Message_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (3) then
         if D.ORM_Property_Id /= -1 then
            A := A & (DBA.Messages_Properties.Property_Id = D.ORM_Property_Id);
         else

            declare
               D2 : constant Property_Data :=
               Property_data (D.ORM_FK_Property_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Property_Id.all);
               end if;

               A := A & (DBA.Messages_Properties.Property_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Messages_Properties, A, DBA.Messages_Properties.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Messages_Properties.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Property;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Property_Data := Property_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Properties.Identifier = To_String (D.ORM_Identifier));
      end if;
      if Mask (3) then
         A := A & (DBA.Properties.Name = To_String (D.ORM_Name));
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Properties, A, DBA.Properties.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Properties.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Resource_Tree;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Resource_Tree_Data := Resource_Tree_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         if D.ORM_Child_Id /= -1 then
            A := A & (DBA.Resource_Trees.Child_Id = D.ORM_Child_Id);
         else

            declare
               D2 : constant Resource_Data :=
               Resource_data (D.ORM_FK_Child_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Child_Id.all);
               end if;

               A := A & (DBA.Resource_Trees.Child_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (3) then
         if D.ORM_Parent_Id /= -1 then
            A := A & (DBA.Resource_Trees.Parent_Id = D.ORM_Parent_Id);
         else

            declare
               D2 : constant Resource_Data :=
               Resource_data (D.ORM_FK_Parent_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Parent_Id.all);
               end if;

               A := A & (DBA.Resource_Trees.Parent_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Resource_Trees, A, DBA.Resource_Trees.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Resource_Trees.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Resource;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Resource_Data := Resource_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Resources.Name = To_String (D.ORM_Name));
      end if;
      if Mask (3) then
         A := A & (DBA.Resources.Kind = D.ORM_Kind);
      end if;
      if Mask (4) then
         A := A & (DBA.Resources.Timestamp = D.ORM_Timestamp);
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Resources, A, DBA.Resources.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Resources.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Resource_Message;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         if D.ORM_Message_Id /= -1 then
            A := A & (DBA.Resources_Messages.Message_Id = D.ORM_Message_Id);
         else

            declare
               D2 : constant Message_Data :=
               Message_data (D.ORM_FK_Message_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Message_Id.all);
               end if;

               A := A & (DBA.Resources_Messages.Message_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (3) then
         if D.ORM_Resource_Id /= -1 then
            A := A & (DBA.Resources_Messages.Resource_Id = D.ORM_Resource_Id);
         else

            declare
               D2 : constant Resource_Data :=
               Resource_data (D.ORM_FK_Resource_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Resource_Id.all);
               end if;

               A := A & (DBA.Resources_Messages.Resource_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (4) then
         A := A & (DBA.Resources_Messages.Line = D.ORM_Line);
      end if;
      if Mask (5) then
         A := A & (DBA.Resources_Messages.Col_Begin = D.ORM_Col_Begin);
      end if;
      if Mask (6) then
         A := A & (DBA.Resources_Messages.Col_End = D.ORM_Col_End);
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Resources_Messages, A, DBA.Resources_Messages.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Resources_Messages.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Rule;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Rule_Data := Rule_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Rules.Name = To_String (D.ORM_Name));
      end if;
      if Mask (3) then
         A := A & (DBA.Rules.Identifier = To_String (D.ORM_Identifier));
      end if;
      if Mask (4) then
         A := A & (DBA.Rules.Kind = D.ORM_Kind);
      end if;
      if Mask (5) then
         if D.ORM_Tool_Id /= -1 then
            A := A & (DBA.Rules.Tool_Id = D.ORM_Tool_Id);
         else

            declare
               D2 : constant Tool_Data :=
               Tool_data (D.ORM_FK_Tool_Id.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Tool_Id.all);
               end if;

               A := A & (DBA.Rules.Tool_Id = D2.ORM_Id);
            end;
         end if;
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Rules, A, DBA.Rules.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Rules.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Tool;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Tool_Data := Tool_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Tools.Name = To_String (D.ORM_Name));
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Tools, A, DBA.Tools.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Tools.Id);
      end if;
   end Insert_Or_Update;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Entity)
   is
      D : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Entities, DBA.Entities.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Entity_Message)
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Entities_Messages, DBA.Entities_Messages.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Message)
   is
      D : constant Message_Data := Message_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Messages, DBA.Messages.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Message_Property)
   is
      D : constant Message_Property_Data := Message_Property_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Messages_Properties, DBA.Messages_Properties.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Property)
   is
      D : constant Property_Data := Property_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Properties, DBA.Properties.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Resource_Tree)
   is
      D : constant Resource_Tree_Data := Resource_Tree_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Resource_Trees, DBA.Resource_Trees.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Resource)
   is
      D : constant Resource_Data := Resource_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Resources, DBA.Resources.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Resource_Message)
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Resources_Messages, DBA.Resources_Messages.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Rule)
   is
      D : constant Rule_Data := Rule_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Rules, DBA.Rules.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Tool)
   is
      D : constant Tool_Data := Tool_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Tools, DBA.Tools.Id = D.ORM_Id));
   end Internal_Delete;

   -----------------------------
   -- Internal_Query_Entities --
   -----------------------------

   procedure Internal_Query_Entities
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Entities(Fields, From, Criteria,
         0, Alias_Entities, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Entities;

   --------------------------------------
   -- Internal_Query_Entities_Messages --
   --------------------------------------

   procedure Internal_Query_Entities_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Entities_Messages(Fields, From, Criteria,
         0, Alias_Entities_Messages, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Entities_Messages;

   -----------------------------
   -- Internal_Query_Messages --
   -----------------------------

   procedure Internal_Query_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Messages(Fields, From, Criteria,
         0, Alias_Messages, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Messages;

   ----------------------------------------
   -- Internal_Query_Messages_Properties --
   ----------------------------------------

   procedure Internal_Query_Messages_Properties
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Messages_Properties(Fields, From, Criteria,
         0, Alias_Messages_Properties, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Messages_Properties;

   -------------------------------
   -- Internal_Query_Properties --
   -------------------------------

   procedure Internal_Query_Properties
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Properties(Fields, From, Criteria,
         0, Alias_Properties, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Properties;

   -----------------------------------
   -- Internal_Query_Resource_Trees --
   -----------------------------------

   procedure Internal_Query_Resource_Trees
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Resource_Trees(Fields, From, Criteria,
         0, Alias_Resource_Trees, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Resource_Trees;

   ------------------------------
   -- Internal_Query_Resources --
   ------------------------------

   procedure Internal_Query_Resources
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Resources(Fields, From, Criteria,
         0, Alias_Resources, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Resources;

   ---------------------------------------
   -- Internal_Query_Resources_Messages --
   ---------------------------------------

   procedure Internal_Query_Resources_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Resources_Messages(Fields, From, Criteria,
         0, Alias_Resources_Messages, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Resources_Messages;

   --------------------------
   -- Internal_Query_Rules --
   --------------------------

   procedure Internal_Query_Rules
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Rules(Fields, From, Criteria,
         0, Alias_Rules, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Rules;

   --------------------------
   -- Internal_Query_Tools --
   --------------------------

   procedure Internal_Query_Tools
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Tools(Fields, From, Criteria,
         0, Alias_Tools, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Tools;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Entity_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (6000000, No_Primary_Key);
      else
         return (6000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Entity_Message_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (7000000, No_Primary_Key);
      else
         return (7000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Message_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (2000000, No_Primary_Key);
      else
         return (2000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Message_Property_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (9000000, No_Primary_Key);
      else
         return (9000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Property_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (8000000, No_Primary_Key);
      else
         return (8000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Resource_Tree_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (4000000, No_Primary_Key);
      else
         return (4000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Resource_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (3000000, No_Primary_Key);
      else
         return (3000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Resource_Message_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (5000000, No_Primary_Key);
      else
         return (5000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Rule_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (1000000, No_Primary_Key);
      else
         return (1000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Tool_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (0, No_Primary_Key);
      else
         return (0, Self.ORM_Id);
      end if;
   end Key;

   ----------------------
   -- Message_Entities --
   ----------------------

   function Message_Entities
     (Self : Message'Class)
     return Entities_Messages_Managers is
   begin
      return Filter (All_Entities_Messages, Message_Id => Self.Id);
   end Message_Entities;

   ----------------------
   -- Message_Entities --
   ----------------------

   function Message_Entities
     (Self : Detached_Message'Class)
     return Entities_Messages_Managers is
   begin
      return Filter (All_Entities_Messages, Message_Id => Self.Id);
   end Message_Entities;

   ----------------------
   -- Message_Entities --
   ----------------------

   function Message_Entities
     (Self : I_Messages_Managers'Class)
     return Entities_Messages_Managers
   is
      Q : constant SQL_Query := I_Messages.Build_Query(Self, +DBA.Messages.Id);
   begin
      return All_Entities_Messages.Filter
        (SQL_In(DBA.Entities_Messages.Message_Id, Q));
   end Message_Entities;

   ------------------------
   -- Message_Properties --
   ------------------------

   function Message_Properties
     (Self : Message'Class)
     return Messages_Properties_Managers is
   begin
      return Filter (All_Messages_Properties, Message_Id => Self.Id);
   end Message_Properties;

   ------------------------
   -- Message_Properties --
   ------------------------

   function Message_Properties
     (Self : Detached_Message'Class)
     return Messages_Properties_Managers is
   begin
      return Filter (All_Messages_Properties, Message_Id => Self.Id);
   end Message_Properties;

   ------------------------
   -- Message_Properties --
   ------------------------

   function Message_Properties
     (Self : I_Messages_Managers'Class)
     return Messages_Properties_Managers
   is
      Q : constant SQL_Query := I_Messages.Build_Query(Self, +DBA.Messages.Id);
   begin
      return All_Messages_Properties.Filter
        (SQL_In(DBA.Messages_Properties.Message_Id, Q));
   end Message_Properties;

   ----------------
   -- New_Entity --
   ----------------

   function New_Entity return Detached_Entity'Class
   is
      Result : Detached_Entity;
      Data   : Entity_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Entity;

   ------------------------
   -- New_Entity_Message --
   ------------------------

   function New_Entity_Message return Detached_Entity_Message'Class
   is
      Result : Detached_Entity_Message;
      Data   : Entity_Message_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Entity_Message;

   -----------------
   -- New_Message --
   -----------------

   function New_Message return Detached_Message'Class
   is
      Result : Detached_Message;
      Data   : Message_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Message;

   --------------------------
   -- New_Message_Property --
   --------------------------

   function New_Message_Property return Detached_Message_Property'Class
   is
      Result : Detached_Message_Property;
      Data   : Message_Property_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Message_Property;

   ------------------
   -- New_Property --
   ------------------

   function New_Property return Detached_Property'Class
   is
      Result : Detached_Property;
      Data   : Property_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Property;

   ------------------
   -- New_Resource --
   ------------------

   function New_Resource return Detached_Resource'Class
   is
      Result : Detached_Resource;
      Data   : Resource_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Resource;

   --------------------------
   -- New_Resource_Message --
   --------------------------

   function New_Resource_Message return Detached_Resource_Message'Class
   is
      Result : Detached_Resource_Message;
      Data   : Resource_Message_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Resource_Message;

   -----------------------
   -- New_Resource_Tree --
   -----------------------

   function New_Resource_Tree return Detached_Resource_Tree'Class
   is
      Result : Detached_Resource_Tree;
      Data   : Resource_Tree_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Resource_Tree;

   --------------
   -- New_Rule --
   --------------

   function New_Rule return Detached_Rule'Class
   is
      Result : Detached_Rule;
      Data   : Rule_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Rule;

   --------------
   -- New_Tool --
   --------------

   function New_Tool return Detached_Tool'Class
   is
      Result : Detached_Tool;
      Data   : Tool_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Tool;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Entity)
   is
      D : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Resource_Id /= null then
            Self.Session.Persist (D.ORM_FK_Resource_Id.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Entity_Message)
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Entity_Id /= null then
            Self.Session.Persist (D.ORM_FK_Entity_Id.all);
         end if;
         if D.ORM_FK_Message_Id /= null then
            Self.Session.Persist (D.ORM_FK_Message_Id.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Message)
   is
      D : constant Message_Data := Message_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Rule_Id /= null then
            Self.Session.Persist (D.ORM_FK_Rule_Id.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Message_Property)
   is
      D : constant Message_Property_Data := Message_Property_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Message_Id /= null then
            Self.Session.Persist (D.ORM_FK_Message_Id.all);
         end if;
         if D.ORM_FK_Property_Id /= null then
            Self.Session.Persist (D.ORM_FK_Property_Id.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Resource_Tree)
   is
      D : constant Resource_Tree_Data := Resource_Tree_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Child_Id /= null then
            Self.Session.Persist (D.ORM_FK_Child_Id.all);
         end if;
         if D.ORM_FK_Parent_Id /= null then
            Self.Session.Persist (D.ORM_FK_Parent_Id.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Resource_Message)
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Message_Id /= null then
            Self.Session.Persist (D.ORM_FK_Message_Id.all);
         end if;
         if D.ORM_FK_Resource_Id /= null then
            Self.Session.Persist (D.ORM_FK_Resource_Id.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Rule)
   is
      D : constant Rule_Data := Rule_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Tool_Id /= null then
            Self.Session.Persist (D.ORM_FK_Tool_Id.all);
         end if;
      end if;
   end On_Persist;

   -----------------------
   -- Property_Messages --
   -----------------------

   function Property_Messages
     (Self : Property'Class)
     return Messages_Properties_Managers is
   begin
      return Filter (All_Messages_Properties, Property_Id => Self.Id);
   end Property_Messages;

   -----------------------
   -- Property_Messages --
   -----------------------

   function Property_Messages
     (Self : Detached_Property'Class)
     return Messages_Properties_Managers is
   begin
      return Filter (All_Messages_Properties, Property_Id => Self.Id);
   end Property_Messages;

   -----------------------
   -- Property_Messages --
   -----------------------

   function Property_Messages
     (Self : I_Properties_Managers'Class)
     return Messages_Properties_Managers
   is
      Q : constant SQL_Query := I_Properties.Build_Query(Self, +DBA.Properties.Id);
   begin
      return All_Messages_Properties.Filter
        (SQL_In(DBA.Messages_Properties.Property_Id, Q));
   end Property_Messages;

   -----------------------
   -- Resource_Children --
   -----------------------

   function Resource_Children
     (Self : Resource'Class)
     return Resource_Trees_Managers is
   begin
      return Filter (All_Resource_Trees, Child_Id => Self.Id);
   end Resource_Children;

   -----------------------
   -- Resource_Children --
   -----------------------

   function Resource_Children
     (Self : Detached_Resource'Class)
     return Resource_Trees_Managers is
   begin
      return Filter (All_Resource_Trees, Child_Id => Self.Id);
   end Resource_Children;

   -----------------------
   -- Resource_Children --
   -----------------------

   function Resource_Children
     (Self : I_Resources_Managers'Class)
     return Resource_Trees_Managers
   is
      Q : constant SQL_Query := I_Resources.Build_Query(Self, +DBA.Resources.Id);
   begin
      return All_Resource_Trees.Filter
        (SQL_In(DBA.Resource_Trees.Child_Id, Q));
   end Resource_Children;

   ---------------------
   -- Resource_Parent --
   ---------------------

   function Resource_Parent
     (Self : Resource'Class)
     return Resource_Trees_Managers is
   begin
      return Filter (All_Resource_Trees, Parent_Id => Self.Id);
   end Resource_Parent;

   ---------------------
   -- Resource_Parent --
   ---------------------

   function Resource_Parent
     (Self : Detached_Resource'Class)
     return Resource_Trees_Managers is
   begin
      return Filter (All_Resource_Trees, Parent_Id => Self.Id);
   end Resource_Parent;

   ---------------------
   -- Resource_Parent --
   ---------------------

   function Resource_Parent
     (Self : I_Resources_Managers'Class)
     return Resource_Trees_Managers
   is
      Q : constant SQL_Query := I_Resources.Build_Query(Self, +DBA.Resources.Id);
   begin
      return All_Resource_Trees.Filter
        (SQL_In(DBA.Resource_Trees.Parent_Id, Q));
   end Resource_Parent;

   -------------------
   -- Rule_Messages --
   -------------------

   function Rule_Messages (Self : Rule'Class) return Messages_Managers is
   begin
      return Filter (All_Messages, Rule_Id => Self.Id);
   end Rule_Messages;

   -------------------
   -- Rule_Messages --
   -------------------

   function Rule_Messages (Self : Detached_Rule'Class) return Messages_Managers
   is
   begin
      return Filter (All_Messages, Rule_Id => Self.Id);
   end Rule_Messages;

   -------------------
   -- Rule_Messages --
   -------------------

   function Rule_Messages
     (Self : I_Rules_Managers'Class)
     return Messages_Managers
   is
      Q : constant SQL_Query := I_Rules.Build_Query(Self, +DBA.Rules.Id);
   begin
      return All_Messages.Filter
        (SQL_In(DBA.Messages.Rule_Id, Q));
   end Rule_Messages;

   ------------------
   -- Set_Child_Id --
   ------------------

   procedure Set_Child_Id (Self : Detached_Resource_Tree; Value : Integer)
   is
      D : constant Resource_Tree_Data := Resource_Tree_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Child_Id);
      D.ORM_Child_Id := Value;
      Self.Set_Modified (2);
   end Set_Child_Id;

   ------------------
   -- Set_Child_Id --
   ------------------

   procedure Set_Child_Id
     (Self  : Detached_Resource_Tree;
      Value : Detached_Resource'Class)
   is
      D : constant Resource_Tree_Data := Resource_Tree_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Child_Id);
      D.ORM_Child_Id := Value.Id;
      D.ORM_FK_Child_Id := new Detached_Resource'Class'(Value);

      Self.Set_Modified (2);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Child_Id.all);
      end if;
   end Set_Child_Id;

   -------------------
   -- Set_Col_Begin --
   -------------------

   procedure Set_Col_Begin (Self : Detached_Resource_Message; Value : Integer)
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
   begin
      D.ORM_Col_Begin := Value;
      Self.Set_Modified (5);
   end Set_Col_Begin;

   -------------------
   -- Set_Col_Begin --
   -------------------

   procedure Set_Col_Begin (Self : Detached_Entity; Value : Integer)
   is
      D : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
   begin
      D.ORM_Col_Begin := Value;
      Self.Set_Modified (4);
   end Set_Col_Begin;

   -------------------
   -- Set_Col_Begin --
   -------------------

   procedure Set_Col_Begin (Self : Detached_Entity_Message; Value : Integer)
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
   begin
      D.ORM_Col_Begin := Value;
      Self.Set_Modified (5);
   end Set_Col_Begin;

   -----------------
   -- Set_Col_End --
   -----------------

   procedure Set_Col_End (Self : Detached_Resource_Message; Value : Integer)
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
   begin
      D.ORM_Col_End := Value;
      Self.Set_Modified (6);
   end Set_Col_End;

   -----------------
   -- Set_Col_End --
   -----------------

   procedure Set_Col_End (Self : Detached_Entity; Value : Integer)
   is
      D : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
   begin
      D.ORM_Col_End := Value;
      Self.Set_Modified (5);
   end Set_Col_End;

   -----------------
   -- Set_Col_End --
   -----------------

   procedure Set_Col_End (Self : Detached_Entity_Message; Value : Integer)
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
   begin
      D.ORM_Col_End := Value;
      Self.Set_Modified (6);
   end Set_Col_End;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Self : Detached_Message; Value : String)
   is
      D : constant Message_Data := Message_Data (Self.Unchecked_Get);
   begin
      D.ORM_Data := To_Unbounded_String (Value);
      Self.Set_Modified (3);
   end Set_Data;

   -------------------
   -- Set_Entity_Id --
   -------------------

   procedure Set_Entity_Id (Self : Detached_Entity_Message; Value : Integer)
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Entity_Id);
      D.ORM_Entity_Id := Value;
      Self.Set_Modified (2);
   end Set_Entity_Id;

   -------------------
   -- Set_Entity_Id --
   -------------------

   procedure Set_Entity_Id
     (Self  : Detached_Entity_Message;
      Value : Detached_Entity'Class)
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Entity_Id);
      D.ORM_Entity_Id := Value.Id;
      D.ORM_FK_Entity_Id := new Detached_Entity'Class'(Value);

      Self.Set_Modified (2);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Entity_Id.all);
      end if;
   end Set_Entity_Id;

   --------------------
   -- Set_Identifier --
   --------------------

   procedure Set_Identifier (Self : Detached_Rule; Value : String)
   is
      D : constant Rule_Data := Rule_Data (Self.Unchecked_Get);
   begin
      D.ORM_Identifier := To_Unbounded_String (Value);
      Self.Set_Modified (3);
   end Set_Identifier;

   --------------------
   -- Set_Identifier --
   --------------------

   procedure Set_Identifier (Self : Detached_Property; Value : String)
   is
      D : constant Property_Data := Property_Data (Self.Unchecked_Get);
   begin
      D.ORM_Identifier := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_Identifier;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (Self : Detached_Rule; Value : Integer)
   is
      D : constant Rule_Data := Rule_Data (Self.Unchecked_Get);
   begin
      D.ORM_Kind := Value;
      Self.Set_Modified (4);
   end Set_Kind;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (Self : Detached_Resource; Value : Integer)
   is
      D : constant Resource_Data := Resource_Data (Self.Unchecked_Get);
   begin
      D.ORM_Kind := Value;
      Self.Set_Modified (3);
   end Set_Kind;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (Self : Detached_Resource_Message; Value : Integer)
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
   begin
      D.ORM_Line := Value;
      Self.Set_Modified (4);
   end Set_Line;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (Self : Detached_Entity; Value : Integer)
   is
      D : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
   begin
      D.ORM_Line := Value;
      Self.Set_Modified (3);
   end Set_Line;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (Self : Detached_Entity_Message; Value : Integer)
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
   begin
      D.ORM_Line := Value;
      Self.Set_Modified (4);
   end Set_Line;

   --------------------
   -- Set_Message_Id --
   --------------------

   procedure Set_Message_Id (Self : Detached_Resource_Message; Value : Integer)
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Message_Id);
      D.ORM_Message_Id := Value;
      Self.Set_Modified (2);
   end Set_Message_Id;

   --------------------
   -- Set_Message_Id --
   --------------------

   procedure Set_Message_Id
     (Self  : Detached_Resource_Message;
      Value : Detached_Message'Class)
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Message_Id);
      D.ORM_Message_Id := Value.Id;
      D.ORM_FK_Message_Id := new Detached_Message'Class'(Value);

      Self.Set_Modified (2);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Message_Id.all);
      end if;
   end Set_Message_Id;

   --------------------
   -- Set_Message_Id --
   --------------------

   procedure Set_Message_Id (Self : Detached_Message_Property; Value : Integer)
   is
      D : constant Message_Property_Data := Message_Property_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Message_Id);
      D.ORM_Message_Id := Value;
      Self.Set_Modified (2);
   end Set_Message_Id;

   --------------------
   -- Set_Message_Id --
   --------------------

   procedure Set_Message_Id
     (Self  : Detached_Message_Property;
      Value : Detached_Message'Class)
   is
      D : constant Message_Property_Data := Message_Property_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Message_Id);
      D.ORM_Message_Id := Value.Id;
      D.ORM_FK_Message_Id := new Detached_Message'Class'(Value);

      Self.Set_Modified (2);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Message_Id.all);
      end if;
   end Set_Message_Id;

   --------------------
   -- Set_Message_Id --
   --------------------

   procedure Set_Message_Id (Self : Detached_Entity_Message; Value : Integer)
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Message_Id);
      D.ORM_Message_Id := Value;
      Self.Set_Modified (3);
   end Set_Message_Id;

   --------------------
   -- Set_Message_Id --
   --------------------

   procedure Set_Message_Id
     (Self  : Detached_Entity_Message;
      Value : Detached_Message'Class)
   is
      D : constant Entity_Message_Data := Entity_Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Message_Id);
      D.ORM_Message_Id := Value.Id;
      D.ORM_FK_Message_Id := new Detached_Message'Class'(Value);

      Self.Set_Modified (3);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Message_Id.all);
      end if;
   end Set_Message_Id;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : Detached_Rule; Value : String)
   is
      D : constant Rule_Data := Rule_Data (Self.Unchecked_Get);
   begin
      D.ORM_Name := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : Detached_Entity; Value : String)
   is
      D : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
   begin
      D.ORM_Name := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : Detached_Tool; Value : String)
   is
      D : constant Tool_Data := Tool_Data (Self.Unchecked_Get);
   begin
      D.ORM_Name := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : Detached_Property; Value : String)
   is
      D : constant Property_Data := Property_Data (Self.Unchecked_Get);
   begin
      D.ORM_Name := To_Unbounded_String (Value);
      Self.Set_Modified (3);
   end Set_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : Detached_Resource; Value : String)
   is
      D : constant Resource_Data := Resource_Data (Self.Unchecked_Get);
   begin
      D.ORM_Name := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_Name;

   -------------------
   -- Set_Parent_Id --
   -------------------

   procedure Set_Parent_Id (Self : Detached_Resource_Tree; Value : Integer)
   is
      D : constant Resource_Tree_Data := Resource_Tree_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Parent_Id);
      D.ORM_Parent_Id := Value;
      Self.Set_Modified (3);
   end Set_Parent_Id;

   -------------------
   -- Set_Parent_Id --
   -------------------

   procedure Set_Parent_Id
     (Self  : Detached_Resource_Tree;
      Value : Detached_Resource'Class)
   is
      D : constant Resource_Tree_Data := Resource_Tree_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Parent_Id);
      D.ORM_Parent_Id := Value.Id;
      D.ORM_FK_Parent_Id := new Detached_Resource'Class'(Value);

      Self.Set_Modified (3);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Parent_Id.all);
      end if;
   end Set_Parent_Id;

   ---------------------
   -- Set_Property_Id --
   ---------------------

   procedure Set_Property_Id (Self : Detached_Message_Property; Value : Integer)
   is
      D : constant Message_Property_Data := Message_Property_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Property_Id);
      D.ORM_Property_Id := Value;
      Self.Set_Modified (3);
   end Set_Property_Id;

   ---------------------
   -- Set_Property_Id --
   ---------------------

   procedure Set_Property_Id
     (Self  : Detached_Message_Property;
      Value : Detached_Property'Class)
   is
      D : constant Message_Property_Data := Message_Property_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Property_Id);
      D.ORM_Property_Id := Value.Id;
      D.ORM_FK_Property_Id := new Detached_Property'Class'(Value);

      Self.Set_Modified (3);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Property_Id.all);
      end if;
   end Set_Property_Id;

   -----------------
   -- Set_Ranking --
   -----------------

   procedure Set_Ranking (Self : Detached_Message; Value : Integer)
   is
      D : constant Message_Data := Message_Data (Self.Unchecked_Get);
   begin
      D.ORM_Ranking := Value;
      Self.Set_Modified (4);
   end Set_Ranking;

   ---------------------
   -- Set_Resource_Id --
   ---------------------

   procedure Set_Resource_Id (Self : Detached_Resource_Message; Value : Integer)
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Resource_Id);
      D.ORM_Resource_Id := Value;
      Self.Set_Modified (3);
   end Set_Resource_Id;

   ---------------------
   -- Set_Resource_Id --
   ---------------------

   procedure Set_Resource_Id
     (Self  : Detached_Resource_Message;
      Value : Detached_Resource'Class)
   is
      D : constant Resource_Message_Data := Resource_Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Resource_Id);
      D.ORM_Resource_Id := Value.Id;
      D.ORM_FK_Resource_Id := new Detached_Resource'Class'(Value);

      Self.Set_Modified (3);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Resource_Id.all);
      end if;
   end Set_Resource_Id;

   ---------------------
   -- Set_Resource_Id --
   ---------------------

   procedure Set_Resource_Id (Self : Detached_Entity; Value : Integer)
   is
      D : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Resource_Id);
      D.ORM_Resource_Id := Value;
      Self.Set_Modified (6);
   end Set_Resource_Id;

   ---------------------
   -- Set_Resource_Id --
   ---------------------

   procedure Set_Resource_Id
     (Self  : Detached_Entity;
      Value : Detached_Resource'Class)
   is
      D : constant Entity_Data := Entity_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Resource_Id);
      D.ORM_Resource_Id := Value.Id;
      D.ORM_FK_Resource_Id := new Detached_Resource'Class'(Value);

      Self.Set_Modified (6);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Resource_Id.all);
      end if;
   end Set_Resource_Id;

   -----------------
   -- Set_Rule_Id --
   -----------------

   procedure Set_Rule_Id (Self : Detached_Message; Value : Integer)
   is
      D : constant Message_Data := Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Rule_Id);
      D.ORM_Rule_Id := Value;
      Self.Set_Modified (2);
   end Set_Rule_Id;

   -----------------
   -- Set_Rule_Id --
   -----------------

   procedure Set_Rule_Id (Self : Detached_Message; Value : Detached_Rule'Class)
   is
      D : constant Message_Data := Message_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Rule_Id);
      D.ORM_Rule_Id := Value.Id;
      D.ORM_FK_Rule_Id := new Detached_Rule'Class'(Value);

      Self.Set_Modified (2);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Rule_Id.all);
      end if;
   end Set_Rule_Id;

   -------------------
   -- Set_Timestamp --
   -------------------

   procedure Set_Timestamp (Self : Detached_Resource; Value : Ada.Calendar.Time)
   is
      D : constant Resource_Data := Resource_Data (Self.Unchecked_Get);
   begin
      D.ORM_Timestamp := Value;
      Self.Set_Modified (4);
   end Set_Timestamp;

   -----------------
   -- Set_Tool_Id --
   -----------------

   procedure Set_Tool_Id (Self : Detached_Rule; Value : Integer)
   is
      D : constant Rule_Data := Rule_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Tool_Id);
      D.ORM_Tool_Id := Value;
      Self.Set_Modified (5);
   end Set_Tool_Id;

   -----------------
   -- Set_Tool_Id --
   -----------------

   procedure Set_Tool_Id (Self : Detached_Rule; Value : Detached_Tool'Class)
   is
      D : constant Rule_Data := Rule_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Tool_Id);
      D.ORM_Tool_Id := Value.Id;
      D.ORM_FK_Tool_Id := new Detached_Tool'Class'(Value);

      Self.Set_Modified (5);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Tool_Id.all);
      end if;
   end Set_Tool_Id;

   ---------------------
   -- Set_Tool_Msg_Id --
   ---------------------

   procedure Set_Tool_Msg_Id (Self : Detached_Message; Value : Integer)
   is
      D : constant Message_Data := Message_Data (Self.Unchecked_Get);
   begin
      D.ORM_Tool_Msg_Id := Value;
      Self.Set_Modified (5);
   end Set_Tool_Msg_Id;

   ----------------
   -- Tool_Rules --
   ----------------

   function Tool_Rules (Self : Tool'Class) return Rules_Managers is
   begin
      return Filter (All_Rules, Tool_Id => Self.Id);
   end Tool_Rules;

   ----------------
   -- Tool_Rules --
   ----------------

   function Tool_Rules (Self : Detached_Tool'Class) return Rules_Managers is
   begin
      return Filter (All_Rules, Tool_Id => Self.Id);
   end Tool_Rules;

   ----------------
   -- Tool_Rules --
   ----------------

   function Tool_Rules (Self : I_Tools_Managers'Class) return Rules_Managers
   is
      Q : constant SQL_Query := I_Tools.Build_Query(Self, +DBA.Tools.Id);
   begin
      return All_Rules.Filter
        (SQL_In(DBA.Rules.Tool_Id, Q));
   end Tool_Rules;
end Database.Orm;

