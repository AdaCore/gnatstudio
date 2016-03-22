package body Database is
   pragma Style_Checks (Off);
   use type Cst_String_Access;

   function FK (Self : T_Entities_Messages'Class; Foreign : T_Entities'Class) return SQL_Criteria is
   begin
      return Self.Entity_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Entities_Messages'Class; Foreign : T_Messages'Class) return SQL_Criteria is
   begin
      return Self.Message_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Messages'Class; Foreign : T_Rules'Class) return SQL_Criteria is
   begin
      return Self.Rule_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Messages'Class; Foreign : T_Categories'Class) return SQL_Criteria is
   begin
      return Self.Category_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Resources_Messages'Class; Foreign : T_Messages'Class) return SQL_Criteria is
   begin
      return Self.Message_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Resources_Messages'Class; Foreign : T_Resources'Class) return SQL_Criteria is
   begin
      return Self.Resource_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Rules'Class; Foreign : T_Tools'Class) return SQL_Criteria is
   begin
      return Self.Tool_Id = Foreign.Id;
   end FK;
end Database;
