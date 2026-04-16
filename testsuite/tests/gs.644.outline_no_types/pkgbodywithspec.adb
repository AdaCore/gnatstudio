package body PkgBodyWithSpec is

   type List_Item_Type is record
      Item : Integer := 0;
   end record;


   procedure one is
   begin null; end one;

   function two return boolean is
   begin return true; end two;

   procedure outline_issue is

      procedure typeDef_issue is
         type List_Type is array(Natural range <>) of List_Item_Type;
         List : List_Type(1 .. 20);
      begin

         for Index in List'range loop
            List(Index).Item := Index;
         end loop;

      end typeDef_issue;

   begin
      typeDef_issue;
   end outline_issue;

   procedure tree is
   begin null; end tree;

   function four return boolean is
   begin return true; end four;

end PkgBodyWithSpec;
