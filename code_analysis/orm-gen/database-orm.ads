pragma Warnings (Off);
with Ada.Calendar; use Ada.Calendar;
with Ada.Finalization; use Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Database; use Database;
with GNAT.Calendar; use GNAT.Calendar;
with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.SQL; use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Orm; use GNATCOLL.SQL.Orm;
with GNATCOLL.SQL.Orm.Impl; use GNATCOLL.SQL.Orm.Impl;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL_Fields; use GNATCOLL.SQL_Fields;
with GNATCOLL.Tribooleans; use GNATCOLL.Tribooleans;
with System.Address_Image;
pragma Warnings (On);
pragma Style_Checks (Off);

package Database.Orm is
   package DBA renames Database;
   subtype Related_Depth is Integer range 0 .. 3;

   -----------
   -- Types --
   -----------
   --  Detached_* elements extract the value from the list and store them
    --  locally. As a result, they remain valid even if the list is modified,
    --  but require more memory to store.
    --
    --  Other elements are only valid while the list from which they are
    --  created is not modified(see Element below). As soon as you iterate the
    --  list this element becomes invalid.
    --
    --  Direct lists are stored in memory, and can be traversed in any order.
    --  Forward lists can only be iterated forward. With some database backends
    --  this is much more efficient since only the current element needs to be
    --  stored in memory(and retrieved from the server).

   type Entity is new Orm_Element with null record;
   type Entity_DDR is new Detached_Data (7) with private;
   type Detached_Entity is  --  Get() returns a Entity_DDR
   new Sessions.Detached_Element with private;
   type Detached_Entity_Access is access all Detached_Entity'Class;
   No_Detached_Entity : constant Detached_Entity;
   No_Entity : constant Entity;

   type Entity_Message is new Orm_Element with null record;
   type Entity_Message_DDR is new Detached_Data (8) with private;
   type Detached_Entity_Message is  --  Get() returns a Entity_Message_DDR
   new Sessions.Detached_Element with private;
   type Detached_Entity_Message_Access is access all Detached_Entity_Message'Class;
   No_Detached_Entity_Message : constant Detached_Entity_Message;
   No_Entity_Message : constant Entity_Message;

   type Message is new Orm_Element with null record;
   type Message_DDR is new Detached_Data (6) with private;
   type Detached_Message is  --  Get() returns a Message_DDR
   new Sessions.Detached_Element with private;
   type Detached_Message_Access is access all Detached_Message'Class;
   No_Detached_Message : constant Detached_Message;
   No_Message : constant Message;

   type Message_Property is new Orm_Element with null record;
   type Message_Property_DDR is new Detached_Data (5) with private;
   type Detached_Message_Property is  --  Get() returns a Message_Property_DDR
   new Sessions.Detached_Element with private;
   type Detached_Message_Property_Access is access all Detached_Message_Property'Class;
   No_Detached_Message_Property : constant Detached_Message_Property;
   No_Message_Property : constant Message_Property;

   type Property is new Orm_Element with null record;
   type Property_DDR is new Detached_Data (3) with private;
   type Detached_Property is  --  Get() returns a Property_DDR
   new Sessions.Detached_Element with private;
   type Detached_Property_Access is access all Detached_Property'Class;
   No_Detached_Property : constant Detached_Property;
   No_Property : constant Property;

   type Resource_Tree is new Orm_Element with null record;
   type Resource_Tree_DDR is new Detached_Data (5) with private;
   type Detached_Resource_Tree is  --  Get() returns a Resource_Tree_DDR
   new Sessions.Detached_Element with private;
   type Detached_Resource_Tree_Access is access all Detached_Resource_Tree'Class;
   No_Detached_Resource_Tree : constant Detached_Resource_Tree;
   No_Resource_Tree : constant Resource_Tree;

   type Resource is new Orm_Element with null record;
   type Resource_DDR is new Detached_Data (4) with private;
   type Detached_Resource is  --  Get() returns a Resource_DDR
   new Sessions.Detached_Element with private;
   type Detached_Resource_Access is access all Detached_Resource'Class;
   No_Detached_Resource : constant Detached_Resource;
   No_Resource : constant Resource;

   type Resource_Message is new Orm_Element with null record;
   type Resource_Message_DDR is new Detached_Data (8) with private;
   type Detached_Resource_Message is  --  Get() returns a Resource_Message_DDR
   new Sessions.Detached_Element with private;
   type Detached_Resource_Message_Access is access all Detached_Resource_Message'Class;
   No_Detached_Resource_Message : constant Detached_Resource_Message;
   No_Resource_Message : constant Resource_Message;

   type Rule is new Orm_Element with null record;
   type Rule_DDR is new Detached_Data (6) with private;
   type Detached_Rule is  --  Get() returns a Rule_DDR
   new Sessions.Detached_Element with private;
   type Detached_Rule_Access is access all Detached_Rule'Class;
   No_Detached_Rule : constant Detached_Rule;
   No_Rule : constant Rule;

   type Tool is new Orm_Element with null record;
   type Tool_DDR is new Detached_Data (2) with private;
   type Detached_Tool is  --  Get() returns a Tool_DDR
   new Sessions.Detached_Element with private;
   type Detached_Tool_Access is access all Detached_Tool'Class;
   No_Detached_Tool : constant Detached_Tool;
   No_Tool : constant Tool;


   ------------------------------
   -- Elements: Resource_Trees --
   ------------------------------

   function "=" (Op1 : Resource_Tree; Op2 : Resource_Tree) return Boolean;
   function "="
     (Op1 : Detached_Resource_Tree;
      Op2 : Detached_Resource_Tree)
     return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Child_Id (Self : Resource_Tree) return Integer;
   function Child_Id (Self : Detached_Resource_Tree) return Integer;
   procedure Set_Child_Id (Self : Detached_Resource_Tree; Value : Integer);
   function Child_Id (Self : Resource_Tree) return Resource'Class;
   function Child_Id
     (Self : Detached_Resource_Tree)
     return Detached_Resource'Class;
   procedure Set_Child_Id
     (Self  : Detached_Resource_Tree;
      Value : Detached_Resource'Class);
   --  Resources as a child

   function Id (Self : Resource_Tree) return Integer;
   function Id (Self : Detached_Resource_Tree) return Integer;
   --  Auto-generated id

   function Parent_Id (Self : Resource_Tree) return Integer;
   function Parent_Id (Self : Detached_Resource_Tree) return Integer;
   procedure Set_Parent_Id (Self : Detached_Resource_Tree; Value : Integer);
   function Parent_Id (Self : Resource_Tree) return Resource'Class;
   function Parent_Id
     (Self : Detached_Resource_Tree)
     return Detached_Resource'Class;
   procedure Set_Parent_Id
     (Self  : Detached_Resource_Tree;
      Value : Detached_Resource'Class);
   --  Resource as a parent

   function Detach
     (Self : Resource_Tree'Class)
     return Detached_Resource_Tree'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Resource_Tree'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Resource_Tree return Detached_Resource_Tree'Class;

   ---------------------
   -- Elements: Rules --
   ---------------------

   function "=" (Op1 : Rule; Op2 : Rule) return Boolean;
   function "=" (Op1 : Detached_Rule; Op2 : Detached_Rule) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Id (Self : Rule) return Integer;
   function Id (Self : Detached_Rule) return Integer;

   function Identifier (Self : Rule) return String;
   function Identifier (Self : Detached_Rule) return String;
   procedure Set_Identifier (Self : Detached_Rule; Value : String);
   --  Rule's unique identifier

   function Kind (Self : Rule) return Integer;
   function Kind (Self : Detached_Rule) return Integer;
   procedure Set_Kind (Self : Detached_Rule; Value : Integer);
   --  Whether it is a rule or a metric. 0 for rule, 1 for metric

   function Name (Self : Rule) return String;
   function Name (Self : Detached_Rule) return String;
   procedure Set_Name (Self : Detached_Rule; Value : String);
   --  Rule's name

   function Tool_Id (Self : Rule) return Integer;
   function Tool_Id (Self : Detached_Rule) return Integer;
   procedure Set_Tool_Id (Self : Detached_Rule; Value : Integer);
   function Tool_Id (Self : Rule) return Tool'Class;
   function Tool_Id (Self : Detached_Rule) return Detached_Tool'Class;
   procedure Set_Tool_Id (Self : Detached_Rule; Value : Detached_Tool'Class);
   --  Rule's related tool

   function Detach (Self : Rule'Class) return Detached_Rule'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Rule'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Rule return Detached_Rule'Class;

   ----------------------------------
   -- Elements: Resources_Messages --
   ----------------------------------

   function "=" (Op1 : Resource_Message; Op2 : Resource_Message) return Boolean;
   function "="
     (Op1 : Detached_Resource_Message;
      Op2 : Detached_Resource_Message)
     return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Col_Begin (Self : Resource_Message) return Integer;
   function Col_Begin (Self : Detached_Resource_Message) return Integer;
   procedure Set_Col_Begin (Self : Detached_Resource_Message; Value : Integer);
   --  Line's column begin

   function Col_End (Self : Resource_Message) return Integer;
   function Col_End (Self : Detached_Resource_Message) return Integer;
   procedure Set_Col_End (Self : Detached_Resource_Message; Value : Integer);
   --  Line's column end

   function Id (Self : Resource_Message) return Integer;
   function Id (Self : Detached_Resource_Message) return Integer;
   --  Auto-generated id

   function Line (Self : Resource_Message) return Integer;
   function Line (Self : Detached_Resource_Message) return Integer;
   procedure Set_Line (Self : Detached_Resource_Message; Value : Integer);
   --  Corresponding line for message - zero means not associated to a line

   function Message_Id (Self : Resource_Message) return Integer;
   function Message_Id (Self : Detached_Resource_Message) return Integer;
   procedure Set_Message_Id (Self : Detached_Resource_Message; Value : Integer);
   function Message_Id (Self : Resource_Message) return Message'Class;
   function Message_Id
     (Self : Detached_Resource_Message)
     return Detached_Message'Class;
   procedure Set_Message_Id
     (Self  : Detached_Resource_Message;
      Value : Detached_Message'Class);
   --  the associated message

   function Resource_Id (Self : Resource_Message) return Integer;
   function Resource_Id (Self : Detached_Resource_Message) return Integer;
   procedure Set_Resource_Id (Self : Detached_Resource_Message; Value : Integer);
   function Resource_Id (Self : Resource_Message) return Resource'Class;
   function Resource_Id
     (Self : Detached_Resource_Message)
     return Detached_Resource'Class;
   procedure Set_Resource_Id
     (Self  : Detached_Resource_Message;
      Value : Detached_Resource'Class);
   --  Corresponding resource for message

   function Detach
     (Self : Resource_Message'Class)
     return Detached_Resource_Message'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Resource_Message'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Resource_Message return Detached_Resource_Message'Class;

   ------------------------
   -- Elements: Messages --
   ------------------------

   function "=" (Op1 : Message; Op2 : Message) return Boolean;
   function "=" (Op1 : Detached_Message; Op2 : Detached_Message) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Data (Self : Message) return String;
   function Data (Self : Detached_Message) return String;
   procedure Set_Data (Self : Detached_Message; Value : String);
   --  Value associated with the message, possibly a numeric value for metrics

   function Id (Self : Message) return Integer;
   function Id (Self : Detached_Message) return Integer;
   --  Auto-generated id

   function Ranking (Self : Message) return Integer;
   function Ranking (Self : Detached_Message) return Integer;
   procedure Set_Ranking (Self : Detached_Message; Value : Integer);
   --  Values : 0-Annotation, 1-Unspecified, 2-Info, 3-Low, 4-Medium, 5-High

   function Rule_Id (Self : Message) return Integer;
   function Rule_Id (Self : Detached_Message) return Integer;
   procedure Set_Rule_Id (Self : Detached_Message; Value : Integer);
   function Rule_Id (Self : Message) return Rule'Class;
   function Rule_Id (Self : Detached_Message) return Detached_Rule'Class;
   procedure Set_Rule_Id (Self : Detached_Message; Value : Detached_Rule'Class);
   --  Messages' associated rule

   function Tool_Msg_Id (Self : Message) return Integer;
   function Tool_Msg_Id (Self : Detached_Message) return Integer;
   procedure Set_Tool_Msg_Id (Self : Detached_Message; Value : Integer);
   --  Stores original message id value issued from tools

   function Detach (Self : Message'Class) return Detached_Message'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Message'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Message return Detached_Message'Class;

   ------------------------
   -- Elements: Entities --
   ------------------------

   function "=" (Op1 : Entity; Op2 : Entity) return Boolean;
   function "=" (Op1 : Detached_Entity; Op2 : Detached_Entity) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Col_Begin (Self : Entity) return Integer;
   function Col_Begin (Self : Detached_Entity) return Integer;
   procedure Set_Col_Begin (Self : Detached_Entity; Value : Integer);
   --  Entitie's column begin

   function Col_End (Self : Entity) return Integer;
   function Col_End (Self : Detached_Entity) return Integer;
   procedure Set_Col_End (Self : Detached_Entity; Value : Integer);
   --  Entitie's column end

   function Id (Self : Entity) return Integer;
   function Id (Self : Detached_Entity) return Integer;
   --  Auto-generated id

   function Line (Self : Entity) return Integer;
   function Line (Self : Detached_Entity) return Integer;
   procedure Set_Line (Self : Detached_Entity; Value : Integer);
   --  Entitie's line begin

   function Name (Self : Entity) return String;
   function Name (Self : Detached_Entity) return String;
   procedure Set_Name (Self : Detached_Entity; Value : String);
   --  Entitie's name

   function Resource_Id (Self : Entity) return Integer;
   function Resource_Id (Self : Detached_Entity) return Integer;
   procedure Set_Resource_Id (Self : Detached_Entity; Value : Integer);
   function Resource_Id (Self : Entity) return Resource'Class;
   function Resource_Id (Self : Detached_Entity) return Detached_Resource'Class;
   procedure Set_Resource_Id
     (Self  : Detached_Entity;
      Value : Detached_Resource'Class);
   --  Entitie's associated ressource

   function Detach (Self : Entity'Class) return Detached_Entity'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Entity'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Entity return Detached_Entity'Class;

   -----------------------------------
   -- Elements: Messages_Properties --
   -----------------------------------

   function "=" (Op1 : Message_Property; Op2 : Message_Property) return Boolean;
   function "="
     (Op1 : Detached_Message_Property;
      Op2 : Detached_Message_Property)
     return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Id (Self : Message_Property) return Integer;
   function Id (Self : Detached_Message_Property) return Integer;
   --  Auto-generated id

   function Message_Id (Self : Message_Property) return Integer;
   function Message_Id (Self : Detached_Message_Property) return Integer;
   procedure Set_Message_Id (Self : Detached_Message_Property; Value : Integer);
   function Message_Id (Self : Message_Property) return Message'Class;
   function Message_Id
     (Self : Detached_Message_Property)
     return Detached_Message'Class;
   procedure Set_Message_Id
     (Self  : Detached_Message_Property;
      Value : Detached_Message'Class);
   --  Message's id

   function Property_Id (Self : Message_Property) return Integer;
   function Property_Id (Self : Detached_Message_Property) return Integer;
   procedure Set_Property_Id (Self : Detached_Message_Property; Value : Integer);
   function Property_Id (Self : Message_Property) return Property'Class;
   function Property_Id
     (Self : Detached_Message_Property)
     return Detached_Property'Class;
   procedure Set_Property_Id
     (Self  : Detached_Message_Property;
      Value : Detached_Property'Class);
   --  Propertie's id

   function Detach
     (Self : Message_Property'Class)
     return Detached_Message_Property'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Message_Property'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Message_Property return Detached_Message_Property'Class;

   ---------------------------------
   -- Elements: Entities_Messages --
   ---------------------------------

   function "=" (Op1 : Entity_Message; Op2 : Entity_Message) return Boolean;
   function "="
     (Op1 : Detached_Entity_Message;
      Op2 : Detached_Entity_Message)
     return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Col_Begin (Self : Entity_Message) return Integer;
   function Col_Begin (Self : Detached_Entity_Message) return Integer;
   procedure Set_Col_Begin (Self : Detached_Entity_Message; Value : Integer);
   --  Line's column begin

   function Col_End (Self : Entity_Message) return Integer;
   function Col_End (Self : Detached_Entity_Message) return Integer;
   procedure Set_Col_End (Self : Detached_Entity_Message; Value : Integer);
   --  Line's column end

   function Entity_Id (Self : Entity_Message) return Integer;
   function Entity_Id (Self : Detached_Entity_Message) return Integer;
   procedure Set_Entity_Id (Self : Detached_Entity_Message; Value : Integer);
   function Entity_Id (Self : Entity_Message) return Entity'Class;
   function Entity_Id
     (Self : Detached_Entity_Message)
     return Detached_Entity'Class;
   procedure Set_Entity_Id
     (Self  : Detached_Entity_Message;
      Value : Detached_Entity'Class);
   --  Entitie's id

   function Id (Self : Entity_Message) return Integer;
   function Id (Self : Detached_Entity_Message) return Integer;
   --  Auto-generated id

   function Line (Self : Entity_Message) return Integer;
   function Line (Self : Detached_Entity_Message) return Integer;
   procedure Set_Line (Self : Detached_Entity_Message; Value : Integer);
   --  Corresponding line for message - zero means not associated to a line

   function Message_Id (Self : Entity_Message) return Integer;
   function Message_Id (Self : Detached_Entity_Message) return Integer;
   procedure Set_Message_Id (Self : Detached_Entity_Message; Value : Integer);
   function Message_Id (Self : Entity_Message) return Message'Class;
   function Message_Id
     (Self : Detached_Entity_Message)
     return Detached_Message'Class;
   procedure Set_Message_Id
     (Self  : Detached_Entity_Message;
      Value : Detached_Message'Class);
   --  Message's id

   function Detach
     (Self : Entity_Message'Class)
     return Detached_Entity_Message'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Entity_Message'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Entity_Message return Detached_Entity_Message'Class;

   ---------------------
   -- Elements: Tools --
   ---------------------

   function "=" (Op1 : Tool; Op2 : Tool) return Boolean;
   function "=" (Op1 : Detached_Tool; Op2 : Detached_Tool) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Id (Self : Tool) return Integer;
   function Id (Self : Detached_Tool) return Integer;
   --  Auto-generated id

   function Name (Self : Tool) return String;
   function Name (Self : Detached_Tool) return String;
   procedure Set_Name (Self : Detached_Tool; Value : String);
   --  Tool's name

   function Detach (Self : Tool'Class) return Detached_Tool'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Tool'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Tool return Detached_Tool'Class;

   --------------------------
   -- Elements: Properties --
   --------------------------

   function "=" (Op1 : Property; Op2 : Property) return Boolean;
   function "="
     (Op1 : Detached_Property;
      Op2 : Detached_Property)
     return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Id (Self : Property) return Integer;
   function Id (Self : Detached_Property) return Integer;
   --  Auto-generated id

   function Identifier (Self : Property) return String;
   function Identifier (Self : Detached_Property) return String;
   procedure Set_Identifier (Self : Detached_Property; Value : String);
   --  Propertie's unique identifier

   function Name (Self : Property) return String;
   function Name (Self : Detached_Property) return String;
   procedure Set_Name (Self : Detached_Property; Value : String);
   --  Propertie's name

   function Detach (Self : Property'Class) return Detached_Property'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Property'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Property return Detached_Property'Class;

   -------------------------
   -- Elements: Resources --
   -------------------------

   function "=" (Op1 : Resource; Op2 : Resource) return Boolean;
   function "="
     (Op1 : Detached_Resource;
      Op2 : Detached_Resource)
     return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Id (Self : Resource) return Integer;
   function Id (Self : Detached_Resource) return Integer;
   --  Auto-generated id

   function Kind (Self : Resource) return Integer;
   function Kind (Self : Detached_Resource) return Integer;
   procedure Set_Kind (Self : Detached_Resource; Value : Integer);
   --  Resource's kind: project, directory or file

   function Name (Self : Resource) return String;
   function Name (Self : Detached_Resource) return String;
   procedure Set_Name (Self : Detached_Resource; Value : String);
   --  Resource's name

   function Timestamp (Self : Resource) return Ada.Calendar.Time;
   function Timestamp (Self : Detached_Resource) return Ada.Calendar.Time;
   procedure Set_Timestamp (Self : Detached_Resource; Value : Ada.Calendar.Time);
   --  Resource's timestamp

   function Detach (Self : Resource'Class) return Detached_Resource'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Resource'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Resource return Detached_Resource'Class;

   --------------------------------------
   -- Managers(Implementation Details) --
   --------------------------------------

   procedure Internal_Query_Entities
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Entities_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Messages_Properties
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Properties
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Resource_Trees
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Resources
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Resources_Messages
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Rules
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Tools
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   -------------------
   -- Manager Types --
   -------------------

   type I_Entities_Managers is abstract new Manager with null record;
   package I_Entities is new Generic_Managers
     (I_Entities_Managers, Entity, Related_Depth, DBA.Entities,
      Internal_Query_Entities);
   type Entities_Managers is new I_Entities.Manager with null record;
   subtype Entities_Stmt is I_Entities.ORM_Prepared_Statement;

   subtype Entity_List is I_Entities.List;
   subtype Direct_Entity_List is I_Entities.Direct_List;
   Empty_Entity_List : constant Entity_List := I_Entities.Empty_List;
   Empty_Direct_Entity_List : constant Direct_Entity_List :=
   I_Entities.Empty_Direct_List;

   type I_Entities_Messages_Managers is abstract new Manager with null record;
   package I_Entities_Messages is new Generic_Managers
     (I_Entities_Messages_Managers, Entity_Message, Related_Depth, DBA.Entities_Messages,
      Internal_Query_Entities_Messages);
   type Entities_Messages_Managers is new I_Entities_Messages.Manager with null record;
   subtype Entities_Messages_Stmt is I_Entities_Messages.ORM_Prepared_Statement;

   subtype Entity_Message_List is I_Entities_Messages.List;
   subtype Direct_Entity_Message_List is I_Entities_Messages.Direct_List;
   Empty_Entity_Message_List : constant Entity_Message_List := I_Entities_Messages.Empty_List;
   Empty_Direct_Entity_Message_List : constant Direct_Entity_Message_List :=
   I_Entities_Messages.Empty_Direct_List;

   type I_Messages_Managers is abstract new Manager with null record;
   package I_Messages is new Generic_Managers
     (I_Messages_Managers, Message, Related_Depth, DBA.Messages,
      Internal_Query_Messages);
   type Messages_Managers is new I_Messages.Manager with null record;
   subtype Messages_Stmt is I_Messages.ORM_Prepared_Statement;

   subtype Message_List is I_Messages.List;
   subtype Direct_Message_List is I_Messages.Direct_List;
   Empty_Message_List : constant Message_List := I_Messages.Empty_List;
   Empty_Direct_Message_List : constant Direct_Message_List :=
   I_Messages.Empty_Direct_List;

   type I_Messages_Properties_Managers is abstract new Manager with null record;
   package I_Messages_Properties is new Generic_Managers
     (I_Messages_Properties_Managers, Message_Property, Related_Depth, DBA.Messages_Properties,
      Internal_Query_Messages_Properties);
   type Messages_Properties_Managers is new I_Messages_Properties.Manager with null record;
   subtype Messages_Properties_Stmt is I_Messages_Properties.ORM_Prepared_Statement;

   subtype Message_Property_List is I_Messages_Properties.List;
   subtype Direct_Message_Property_List is I_Messages_Properties.Direct_List;
   Empty_Message_Property_List : constant Message_Property_List := I_Messages_Properties.Empty_List;
   Empty_Direct_Message_Property_List : constant Direct_Message_Property_List :=
   I_Messages_Properties.Empty_Direct_List;

   type I_Properties_Managers is abstract new Manager with null record;
   package I_Properties is new Generic_Managers
     (I_Properties_Managers, Property, Related_Depth, DBA.Properties,
      Internal_Query_Properties);
   type Properties_Managers is new I_Properties.Manager with null record;
   subtype Properties_Stmt is I_Properties.ORM_Prepared_Statement;

   subtype Property_List is I_Properties.List;
   subtype Direct_Property_List is I_Properties.Direct_List;
   Empty_Property_List : constant Property_List := I_Properties.Empty_List;
   Empty_Direct_Property_List : constant Direct_Property_List :=
   I_Properties.Empty_Direct_List;

   type I_Resource_Trees_Managers is abstract new Manager with null record;
   package I_Resource_Trees is new Generic_Managers
     (I_Resource_Trees_Managers, Resource_Tree, Related_Depth, DBA.Resource_Trees,
      Internal_Query_Resource_Trees);
   type Resource_Trees_Managers is new I_Resource_Trees.Manager with null record;
   subtype Resource_Trees_Stmt is I_Resource_Trees.ORM_Prepared_Statement;

   subtype Resource_Tree_List is I_Resource_Trees.List;
   subtype Direct_Resource_Tree_List is I_Resource_Trees.Direct_List;
   Empty_Resource_Tree_List : constant Resource_Tree_List := I_Resource_Trees.Empty_List;
   Empty_Direct_Resource_Tree_List : constant Direct_Resource_Tree_List :=
   I_Resource_Trees.Empty_Direct_List;

   type I_Resources_Managers is abstract new Manager with null record;
   package I_Resources is new Generic_Managers
     (I_Resources_Managers, Resource, Related_Depth, DBA.Resources,
      Internal_Query_Resources);
   type Resources_Managers is new I_Resources.Manager with null record;
   subtype Resources_Stmt is I_Resources.ORM_Prepared_Statement;

   subtype Resource_List is I_Resources.List;
   subtype Direct_Resource_List is I_Resources.Direct_List;
   Empty_Resource_List : constant Resource_List := I_Resources.Empty_List;
   Empty_Direct_Resource_List : constant Direct_Resource_List :=
   I_Resources.Empty_Direct_List;

   type I_Resources_Messages_Managers is abstract new Manager with null record;
   package I_Resources_Messages is new Generic_Managers
     (I_Resources_Messages_Managers, Resource_Message, Related_Depth, DBA.Resources_Messages,
      Internal_Query_Resources_Messages);
   type Resources_Messages_Managers is new I_Resources_Messages.Manager with null record;
   subtype Resources_Messages_Stmt is I_Resources_Messages.ORM_Prepared_Statement;

   subtype Resource_Message_List is I_Resources_Messages.List;
   subtype Direct_Resource_Message_List is I_Resources_Messages.Direct_List;
   Empty_Resource_Message_List : constant Resource_Message_List := I_Resources_Messages.Empty_List;
   Empty_Direct_Resource_Message_List : constant Direct_Resource_Message_List :=
   I_Resources_Messages.Empty_Direct_List;

   type I_Rules_Managers is abstract new Manager with null record;
   package I_Rules is new Generic_Managers
     (I_Rules_Managers, Rule, Related_Depth, DBA.Rules,
      Internal_Query_Rules);
   type Rules_Managers is new I_Rules.Manager with null record;
   subtype Rules_Stmt is I_Rules.ORM_Prepared_Statement;

   subtype Rule_List is I_Rules.List;
   subtype Direct_Rule_List is I_Rules.Direct_List;
   Empty_Rule_List : constant Rule_List := I_Rules.Empty_List;
   Empty_Direct_Rule_List : constant Direct_Rule_List :=
   I_Rules.Empty_Direct_List;

   type I_Tools_Managers is abstract new Manager with null record;
   package I_Tools is new Generic_Managers
     (I_Tools_Managers, Tool, Related_Depth, DBA.Tools,
      Internal_Query_Tools);
   type Tools_Managers is new I_Tools.Manager with null record;
   subtype Tools_Stmt is I_Tools.ORM_Prepared_Statement;

   subtype Tool_List is I_Tools.List;
   subtype Direct_Tool_List is I_Tools.Direct_List;
   Empty_Tool_List : constant Tool_List := I_Tools.Empty_List;
   Empty_Direct_Tool_List : constant Direct_Tool_List :=
   I_Tools.Empty_Direct_List;


   -----------------------------
   -- Manager: Resource_Trees --
   -----------------------------

   function Filter
     (Self      : Resource_Trees_Managers'Class;
      Id        : Integer := -1;
      Child_Id  : Integer := -1;
      Parent_Id : Integer := -1)
     return Resource_Trees_Managers;

   function Get_Resource_Tree
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Resource_Tree'Class;

   --------------------
   -- Manager: Rules --
   --------------------

   function Filter
     (Self       : Rules_Managers'Class;
      Id         : Integer := -1;
      Name       : String := No_Update;
      Identifier : String := No_Update;
      Kind       : Integer := -1;
      Tool_Id    : Integer := -1)
     return Rules_Managers;

   function Get_Rule
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Rule'Class;

   function Rule_Messages (Self : Rule'Class) return Messages_Managers;
   function Rule_Messages (Self : Detached_Rule'Class) return Messages_Managers;
   function Rule_Messages
     (Self : I_Rules_Managers'Class)
     return Messages_Managers;

   ---------------------------------
   -- Manager: Resources_Messages --
   ---------------------------------

   function Filter
     (Self        : Resources_Messages_Managers'Class;
      Id          : Integer := -1;
      Message_Id  : Integer := -1;
      Resource_Id : Integer := -1;
      Line        : Integer := -1;
      Col_Begin   : Integer := -1;
      Col_End     : Integer := -1)
     return Resources_Messages_Managers;

   function Get_Resource_Message
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Resource_Message'Class;

   -----------------------
   -- Manager: Messages --
   -----------------------

   function Get_Message
     (Self : Message'Class)
     return Resources_Messages_Managers;
   function Get_Message
     (Self : Detached_Message'Class)
     return Resources_Messages_Managers;
   function Get_Message
     (Self : I_Messages_Managers'Class)
     return Resources_Messages_Managers;

   function Filter
     (Self        : Messages_Managers'Class;
      Id          : Integer := -1;
      Rule_Id     : Integer := -1;
      Data        : String := No_Update;
      Ranking     : Integer := -1;
      Tool_Msg_Id : Integer := -1)
     return Messages_Managers;

   function Get_Message
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Message'Class;

   function Message_Entities
     (Self : Message'Class)
     return Entities_Messages_Managers;
   function Message_Entities
     (Self : Detached_Message'Class)
     return Entities_Messages_Managers;
   function Message_Entities
     (Self : I_Messages_Managers'Class)
     return Entities_Messages_Managers;

   function Message_Properties
     (Self : Message'Class)
     return Messages_Properties_Managers;
   function Message_Properties
     (Self : Detached_Message'Class)
     return Messages_Properties_Managers;
   function Message_Properties
     (Self : I_Messages_Managers'Class)
     return Messages_Properties_Managers;

   -----------------------
   -- Manager: Entities --
   -----------------------

   function Entity_Messages
     (Self : Entity'Class)
     return Entities_Messages_Managers;
   function Entity_Messages
     (Self : Detached_Entity'Class)
     return Entities_Messages_Managers;
   function Entity_Messages
     (Self : I_Entities_Managers'Class)
     return Entities_Messages_Managers;

   function Filter
     (Self        : Entities_Managers'Class;
      Id          : Integer := -1;
      Name        : String := No_Update;
      Line        : Integer := -1;
      Col_Begin   : Integer := -1;
      Col_End     : Integer := -1;
      Resource_Id : Integer := -1)
     return Entities_Managers;

   function Get_Entity
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Entity'Class;

   ----------------------------------
   -- Manager: Messages_Properties --
   ----------------------------------

   function Filter
     (Self        : Messages_Properties_Managers'Class;
      Id          : Integer := -1;
      Message_Id  : Integer := -1;
      Property_Id : Integer := -1)
     return Messages_Properties_Managers;

   function Get_Message_Property
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Message_Property'Class;

   --------------------------------
   -- Manager: Entities_Messages --
   --------------------------------

   function Filter
     (Self       : Entities_Messages_Managers'Class;
      Id         : Integer := -1;
      Entity_Id  : Integer := -1;
      Message_Id : Integer := -1;
      Line       : Integer := -1;
      Col_Begin  : Integer := -1;
      Col_End    : Integer := -1)
     return Entities_Messages_Managers;

   function Get_Entity_Message
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Entity_Message'Class;

   --------------------
   -- Manager: Tools --
   --------------------

   function Filter
     (Self : Tools_Managers'Class;
      Id   : Integer := -1;
      Name : String := No_Update)
     return Tools_Managers;

   function Get_Tool
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Tool'Class;

   function Tool_Rules (Self : Tool'Class) return Rules_Managers;
   function Tool_Rules (Self : Detached_Tool'Class) return Rules_Managers;
   function Tool_Rules (Self : I_Tools_Managers'Class) return Rules_Managers;

   -------------------------
   -- Manager: Properties --
   -------------------------

   function Filter
     (Self       : Properties_Managers'Class;
      Id         : Integer := -1;
      Identifier : String := No_Update;
      Name       : String := No_Update)
     return Properties_Managers;

   function Get_Property
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Property'Class;

   function Property_Messages
     (Self : Property'Class)
     return Messages_Properties_Managers;
   function Property_Messages
     (Self : Detached_Property'Class)
     return Messages_Properties_Managers;
   function Property_Messages
     (Self : I_Properties_Managers'Class)
     return Messages_Properties_Managers;

   ------------------------
   -- Manager: Resources --
   ------------------------

   function Get_Resource
     (Self : Resource'Class)
     return Resources_Messages_Managers;
   function Get_Resource
     (Self : Detached_Resource'Class)
     return Resources_Messages_Managers;
   function Get_Resource
     (Self : I_Resources_Managers'Class)
     return Resources_Messages_Managers;
   function Get_Resource (Self : Resource'Class) return Entities_Managers;
   function Get_Resource
     (Self : Detached_Resource'Class)
     return Entities_Managers;
   function Get_Resource
     (Self : I_Resources_Managers'Class)
     return Entities_Managers;

   function Filter
     (Self      : Resources_Managers'Class;
      Id        : Integer := -1;
      Name      : String := No_Update;
      Kind      : Integer := -1;
      Timestamp : Ada.Calendar.Time := No_Time)
     return Resources_Managers;

   function Get_Resource
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Resource'Class;

   function Resource_Children
     (Self : Resource'Class)
     return Resource_Trees_Managers;
   function Resource_Children
     (Self : Detached_Resource'Class)
     return Resource_Trees_Managers;
   function Resource_Children
     (Self : I_Resources_Managers'Class)
     return Resource_Trees_Managers;

   function Resource_Parent
     (Self : Resource'Class)
     return Resource_Trees_Managers;
   function Resource_Parent
     (Self : Detached_Resource'Class)
     return Resource_Trees_Managers;
   function Resource_Parent
     (Self : I_Resources_Managers'Class)
     return Resource_Trees_Managers;

   --------------
   -- Managers --
   --------------

   All_Entities : constant Entities_Managers :=
     (I_Entities.All_Managers with null record);

   All_Entities_Messages : constant Entities_Messages_Managers :=
     (I_Entities_Messages.All_Managers with null record);

   All_Messages : constant Messages_Managers :=
     (I_Messages.All_Managers with null record);

   All_Messages_Properties : constant Messages_Properties_Managers :=
     (I_Messages_Properties.All_Managers with null record);

   All_Properties : constant Properties_Managers :=
     (I_Properties.All_Managers with null record);

   All_Resource_Trees : constant Resource_Trees_Managers :=
     (I_Resource_Trees.All_Managers with null record);

   All_Resources : constant Resources_Managers :=
     (I_Resources.All_Managers with null record);

   All_Resources_Messages : constant Resources_Messages_Managers :=
     (I_Resources_Messages.All_Managers with null record);

   All_Rules : constant Rules_Managers :=
     (I_Rules.All_Managers with null record);

   All_Tools : constant Tools_Managers :=
     (I_Tools.All_Managers with null record);


   --------------
   -- Internal --
   --------------

   overriding procedure Free (Self : in out Entity_Ddr);
   overriding procedure Free (Self : in out Entity_Message_Ddr);
   overriding procedure Free (Self : in out Message_Ddr);
   overriding procedure Free (Self : in out Message_Property_Ddr);
   overriding procedure Free (Self : in out Property_Ddr);
   overriding procedure Free (Self : in out Resource_Tree_Ddr);
   overriding procedure Free (Self : in out Resource_Ddr);
   overriding procedure Free (Self : in out Resource_Message_Ddr);
   overriding procedure Free (Self : in out Rule_Ddr);
   overriding procedure Free (Self : in out Tool_Ddr);

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Entity;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Entity_Message;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Message;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Message_Property;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Property;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Resource_Tree;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Resource;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Resource_Message;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Rule;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Tool;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);

   overriding procedure Internal_Delete (Self : Detached_Entity);
   overriding procedure Internal_Delete (Self : Detached_Entity_Message);
   overriding procedure Internal_Delete (Self : Detached_Message);
   overriding procedure Internal_Delete (Self : Detached_Message_Property);
   overriding procedure Internal_Delete (Self : Detached_Property);
   overriding procedure Internal_Delete (Self : Detached_Resource_Tree);
   overriding procedure Internal_Delete (Self : Detached_Resource);
   overriding procedure Internal_Delete (Self : Detached_Resource_Message);
   overriding procedure Internal_Delete (Self : Detached_Rule);
   overriding procedure Internal_Delete (Self : Detached_Tool);

   overriding function Key (Self : Entity_Ddr) return Element_Key;
   overriding function Key (Self : Entity_Message_Ddr) return Element_Key;
   overriding function Key (Self : Message_Ddr) return Element_Key;
   overriding function Key (Self : Message_Property_Ddr) return Element_Key;
   overriding function Key (Self : Property_Ddr) return Element_Key;
   overriding function Key (Self : Resource_Tree_Ddr) return Element_Key;
   overriding function Key (Self : Resource_Ddr) return Element_Key;
   overriding function Key (Self : Resource_Message_Ddr) return Element_Key;
   overriding function Key (Self : Rule_Ddr) return Element_Key;
   overriding function Key (Self : Tool_Ddr) return Element_Key;

   overriding procedure On_Persist (Self : Detached_Entity);
   overriding procedure On_Persist (Self : Detached_Entity_Message);
   overriding procedure On_Persist (Self : Detached_Message);
   overriding procedure On_Persist (Self : Detached_Message_Property);
   overriding procedure On_Persist (Self : Detached_Resource_Tree);
   overriding procedure On_Persist (Self : Detached_Resource_Message);
   overriding procedure On_Persist (Self : Detached_Rule);

private

    type Entity_DDR is new Detached_Data (7) with record
       ORM_Col_Begin      : Integer := -1;
       ORM_Col_End        : Integer := -1;
       ORM_FK_Resource_Id : Detached_Resource_Access := null;
       ORM_Id             : Integer := -1;
       ORM_Line           : Integer := -1;
       ORM_Name           : Unbounded_String := Null_Unbounded_String;
       ORM_Resource_Id    : Integer := -1;
    end record;
    type Entity_Data is access all Entity_DDR;
    
    type Entity_Message_DDR is new Detached_Data (8) with record
       ORM_Col_Begin     : Integer := -1;
       ORM_Col_End       : Integer := -1;
       ORM_Entity_Id     : Integer := -1;
       ORM_FK_Entity_Id  : Detached_Entity_Access := null;
       ORM_FK_Message_Id : Detached_Message_Access := null;
       ORM_Id            : Integer := -1;
       ORM_Line          : Integer := -1;
       ORM_Message_Id    : Integer := -1;
    end record;
    type Entity_Message_Data is access all Entity_Message_DDR;
    
    type Message_DDR is new Detached_Data (6) with record
       ORM_Data           : Unbounded_String := Null_Unbounded_String;
       ORM_FK_Rule_Id     : Detached_Rule_Access := null;
       ORM_Id             : Integer := -1;
       ORM_Ranking        : Integer := 1;
       ORM_Rule_Id        : Integer := -1;
       ORM_Tool_Msg_Id    : Integer := 0;
    end record;
    type Message_Data is access all Message_DDR;
    
    type Message_Property_DDR is new Detached_Data (5) with record
       ORM_FK_Message_Id  : Detached_Message_Access := null;
       ORM_FK_Property_Id : Detached_Property_Access := null;
       ORM_Id             : Integer := -1;
       ORM_Message_Id     : Integer := -1;
       ORM_Property_Id    : Integer := -1;
    end record;
    type Message_Property_Data is access all Message_Property_DDR;
    
    type Property_DDR is new Detached_Data (3) with record
       ORM_Id            : Integer := -1;
       ORM_Identifier    : Unbounded_String := Null_Unbounded_String;
       ORM_Name          : Unbounded_String := Null_Unbounded_String;
    end record;
    type Property_Data is access all Property_DDR;
    
    type Resource_Tree_DDR is new Detached_Data (5) with record
       ORM_Child_Id     : Integer := -1;
       ORM_FK_Child_Id  : Detached_Resource_Access := null;
       ORM_FK_Parent_Id : Detached_Resource_Access := null;
       ORM_Id           : Integer := -1;
       ORM_Parent_Id    : Integer := -1;
    end record;
    type Resource_Tree_Data is access all Resource_Tree_DDR;
    
    type Resource_DDR is new Detached_Data (4) with record
       ORM_Id           : Integer := -1;
       ORM_Kind         : Integer := -1;
       ORM_Name         : Unbounded_String := Null_Unbounded_String;
       ORM_Timestamp    : Ada.Calendar.Time := No_Time;
    end record;
    type Resource_Data is access all Resource_DDR;
    
    type Resource_Message_DDR is new Detached_Data (8) with record
       ORM_Col_Begin      : Integer := -1;
       ORM_Col_End        : Integer := -1;
       ORM_FK_Message_Id  : Detached_Message_Access := null;
       ORM_FK_Resource_Id : Detached_Resource_Access := null;
       ORM_Id             : Integer := -1;
       ORM_Line           : Integer := -1;
       ORM_Message_Id     : Integer := -1;
       ORM_Resource_Id    : Integer := -1;
    end record;
    type Resource_Message_Data is access all Resource_Message_DDR;
    
    type Rule_DDR is new Detached_Data (6) with record
       ORM_FK_Tool_Id    : Detached_Tool_Access := null;
       ORM_Id            : Integer := -1;
       ORM_Identifier    : Unbounded_String := Null_Unbounded_String;
       ORM_Kind          : Integer := 0;
       ORM_Name          : Unbounded_String := Null_Unbounded_String;
       ORM_Tool_Id       : Integer := -1;
    end record;
    type Rule_Data is access all Rule_DDR;
    
    type Tool_DDR is new Detached_Data (2) with record
       ORM_Id      : Integer := -1;
       ORM_Name    : Unbounded_String := Null_Unbounded_String;
    end record;
    type Tool_Data is access all Tool_DDR;
    

    type Detached_Entity
       is new Sessions.Detached_Element with null record;
    No_Entity : constant Entity :=(No_Orm_Element with null record);
    No_Detached_Entity : constant Detached_Entity :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Entity_Message
       is new Sessions.Detached_Element with null record;
    No_Entity_Message : constant Entity_Message :=(No_Orm_Element with null record);
    No_Detached_Entity_Message : constant Detached_Entity_Message :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Message
       is new Sessions.Detached_Element with null record;
    No_Message : constant Message :=(No_Orm_Element with null record);
    No_Detached_Message : constant Detached_Message :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Message_Property
       is new Sessions.Detached_Element with null record;
    No_Message_Property : constant Message_Property :=(No_Orm_Element with null record);
    No_Detached_Message_Property : constant Detached_Message_Property :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Property
       is new Sessions.Detached_Element with null record;
    No_Property : constant Property :=(No_Orm_Element with null record);
    No_Detached_Property : constant Detached_Property :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Resource_Tree
       is new Sessions.Detached_Element with null record;
    No_Resource_Tree : constant Resource_Tree :=(No_Orm_Element with null record);
    No_Detached_Resource_Tree : constant Detached_Resource_Tree :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Resource
       is new Sessions.Detached_Element with null record;
    No_Resource : constant Resource :=(No_Orm_Element with null record);
    No_Detached_Resource : constant Detached_Resource :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Resource_Message
       is new Sessions.Detached_Element with null record;
    No_Resource_Message : constant Resource_Message :=(No_Orm_Element with null record);
    No_Detached_Resource_Message : constant Detached_Resource_Message :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Rule
       is new Sessions.Detached_Element with null record;
    No_Rule : constant Rule :=(No_Orm_Element with null record);
    No_Detached_Rule : constant Detached_Rule :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Tool
       is new Sessions.Detached_Element with null record;
    No_Tool : constant Tool :=(No_Orm_Element with null record);
    No_Detached_Tool : constant Detached_Tool :=
      (Sessions.Detached_Element with null record);
 
end Database.Orm;
