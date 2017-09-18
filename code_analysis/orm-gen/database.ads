with GNATCOLL.SQL; use GNATCOLL.SQL;
pragma Warnings (Off, "no entities of * are referenced");
pragma Warnings (Off, "use clause for package * has no effect");
with GNATCOLL.SQL_Fields; use GNATCOLL.SQL_Fields;
pragma Warnings (On, "no entities of * are referenced");
pragma Warnings (On, "use clause for package * has no effect");
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Abstract_Categories
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Categories, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Categories, Instance, N_Id, Index);
      --  Auto-generated id

      Label : SQL_Field_Text (Ta_Categories, Instance, N_Label, Index);
      --  Categorie's label

      On_Side : SQL_Field_Boolean (Ta_Categories, Instance, N_On_Side, Index);
      --  Whether messages belonging to this category should be displayed on
      --  the side

   end record;

   type T_Categories (Instance : Cst_String_Access)
      is new T_Abstract_Categories (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Categories (Index : Integer)
      is new T_Abstract_Categories (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Entities
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Entities, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Entities, Instance, N_Id, Index);
      --  Auto-generated id

      Name : SQL_Field_Text (Ta_Entities, Instance, N_Name, Index);
      --  Entitie's name

      Line : SQL_Field_Integer (Ta_Entities, Instance, N_Line, Index);
      --  Entitie's line begin

      Col_Begin : SQL_Field_Integer (Ta_Entities, Instance, N_Col_Begin, Index);
      --  Entitie's column begin

      Col_End : SQL_Field_Integer (Ta_Entities, Instance, N_Col_End, Index);
      --  Entitie's column end

   end record;

   type T_Entities (Instance : Cst_String_Access)
      is new T_Abstract_Entities (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Entities (Index : Integer)
      is new T_Abstract_Entities (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Entities_Messages
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Entities_Messages, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Entities_Messages, Instance, N_Id, Index);
      --  Auto-generated id

      Entity_Id : SQL_Field_Integer (Ta_Entities_Messages, Instance, N_Entity_Id, Index);
      --  Entitie's id

      Message_Id : SQL_Field_Integer (Ta_Entities_Messages, Instance, N_Message_Id, Index);
      --  Message's id

   end record;

   type T_Entities_Messages (Instance : Cst_String_Access)
      is new T_Abstract_Entities_Messages (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Entities_Messages (Index : Integer)
      is new T_Abstract_Entities_Messages (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Messages
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Messages, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Messages, Instance, N_Id, Index);
      --  Auto-generated id

      Rule_Id : SQL_Field_Integer (Ta_Messages, Instance, N_Rule_Id, Index);
      --  Messages' associated rule

      Data : SQL_Field_Text (Ta_Messages, Instance, N_Data, Index);
      --  Value associated with the message, possibly a numeric value for
      --  metrics

      Category_Id : SQL_Field_Integer (Ta_Messages, Instance, N_Category_Id, Index);
      --  Category of the rule

   end record;

   type T_Messages (Instance : Cst_String_Access)
      is new T_Abstract_Messages (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Messages (Index : Integer)
      is new T_Abstract_Messages (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Messages_Properties
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Messages_Properties, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Messages_Properties, Instance, N_Id, Index);
      --  Auto-generated id

      Message_Id : SQL_Field_Integer (Ta_Messages_Properties, Instance, N_Message_Id, Index);
      --  Message's id

      Property_Id : SQL_Field_Integer (Ta_Messages_Properties, Instance, N_Property_Id, Index);
      --  Propertie's id

   end record;

   type T_Messages_Properties (Instance : Cst_String_Access)
      is new T_Abstract_Messages_Properties (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Messages_Properties (Index : Integer)
      is new T_Abstract_Messages_Properties (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Properties
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Properties, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Properties, Instance, N_Id, Index);
      --  Auto-generated id

      Identifier : SQL_Field_Text (Ta_Properties, Instance, N_Identifier, Index);
      --  Propertie's unique identifier

      Name : SQL_Field_Text (Ta_Properties, Instance, N_Name, Index);
      --  Propertie's name

   end record;

   type T_Properties (Instance : Cst_String_Access)
      is new T_Abstract_Properties (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Properties (Index : Integer)
      is new T_Abstract_Properties (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Resource_Trees
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Resource_Trees, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Resource_Trees, Instance, N_Id, Index);
      --  Auto-generated id

      Child_Id : SQL_Field_Integer (Ta_Resource_Trees, Instance, N_Child_Id, Index);
      --  Resources as a child

      Parent_Id : SQL_Field_Integer (Ta_Resource_Trees, Instance, N_Parent_Id, Index);
      --  Resource as a parent

   end record;

   type T_Resource_Trees (Instance : Cst_String_Access)
      is new T_Abstract_Resource_Trees (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Resource_Trees (Index : Integer)
      is new T_Abstract_Resource_Trees (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Resources
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Resources, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Resources, Instance, N_Id, Index);
      --  Auto-generated id

      Name : SQL_Field_Text (Ta_Resources, Instance, N_Name, Index);
      --  Resource's name

      Kind : SQL_Field_Integer (Ta_Resources, Instance, N_Kind, Index);
      --  Resource's kind: project, directory or file

      Timestamp : SQL_Field_Time (Ta_Resources, Instance, N_Timestamp, Index);
      --  Resource's timestamp

   end record;

   type T_Resources (Instance : Cst_String_Access)
      is new T_Abstract_Resources (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Resources (Index : Integer)
      is new T_Abstract_Resources (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Resources_Messages
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Resources_Messages, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Resources_Messages, Instance, N_Id, Index);
      --  Auto-generated id

      Message_Id : SQL_Field_Integer (Ta_Resources_Messages, Instance, N_Message_Id, Index);
      --  the associated message

      Resource_Id : SQL_Field_Integer (Ta_Resources_Messages, Instance, N_Resource_Id, Index);
      --  Corresponding resource for message

      Line : SQL_Field_Integer (Ta_Resources_Messages, Instance, N_Line, Index);
      --  Corresponding line for message - zero means not associated to a line

      Col_Begin : SQL_Field_Integer (Ta_Resources_Messages, Instance, N_Col_Begin, Index);
      --  Line's column begin

      Col_End : SQL_Field_Integer (Ta_Resources_Messages, Instance, N_Col_End, Index);
      --  Line's column end

   end record;

   type T_Resources_Messages (Instance : Cst_String_Access)
      is new T_Abstract_Resources_Messages (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Resources_Messages (Index : Integer)
      is new T_Abstract_Resources_Messages (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Rules
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Rules, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Rules, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_Rules, Instance, N_Name, Index);
      --  Rule's name

      Identifier : SQL_Field_Text (Ta_Rules, Instance, N_Identifier, Index);
      --  Rule's unique identifier

      Kind : SQL_Field_Integer (Ta_Rules, Instance, N_Kind, Index);
      --  Whether it is a rule or a metric. 0 for rule, 1 for metric

      Tool_Id : SQL_Field_Integer (Ta_Rules, Instance, N_Tool_Id, Index);
      --  Rule's related tool

   end record;

   type T_Rules (Instance : Cst_String_Access)
      is new T_Abstract_Rules (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Rules (Index : Integer)
      is new T_Abstract_Rules (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Tools
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Tools, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Tools, Instance, N_Id, Index);
      --  Auto-generated id

      Name : SQL_Field_Text (Ta_Tools, Instance, N_Name, Index);
      --  Tool's name

   end record;

   type T_Tools (Instance : Cst_String_Access)
      is new T_Abstract_Tools (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Tools (Index : Integer)
      is new T_Abstract_Tools (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   function FK (Self : T_Entities_Messages'Class; Foreign : T_Entities'Class) return SQL_Criteria;
   function FK (Self : T_Entities_Messages'Class; Foreign : T_Messages'Class) return SQL_Criteria;
   function FK (Self : T_Messages'Class; Foreign : T_Rules'Class) return SQL_Criteria;
   function FK (Self : T_Messages'Class; Foreign : T_Categories'Class) return SQL_Criteria;
   function FK (Self : T_Messages_Properties'Class; Foreign : T_Messages'Class) return SQL_Criteria;
   function FK (Self : T_Messages_Properties'Class; Foreign : T_Properties'Class) return SQL_Criteria;
   function FK (Self : T_Resources_Messages'Class; Foreign : T_Messages'Class) return SQL_Criteria;
   function FK (Self : T_Resources_Messages'Class; Foreign : T_Resources'Class) return SQL_Criteria;
   function FK (Self : T_Rules'Class; Foreign : T_Tools'Class) return SQL_Criteria;
   Categories : T_Categories (null);
   Entities : T_Entities (null);
   Entities_Messages : T_Entities_Messages (null);
   Messages : T_Messages (null);
   Messages_Properties : T_Messages_Properties (null);
   Properties : T_Properties (null);
   Resource_Trees : T_Resource_Trees (null);
   Resources : T_Resources (null);
   Resources_Messages : T_Resources_Messages (null);
   Rules : T_Rules (null);
   Tools : T_Tools (null);
end Database;
