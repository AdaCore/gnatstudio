with SN,
     SN.DB_Structures,
     SN.Find_Fns,
     DB_API,
     Ada.Text_IO,
     Ada.Strings.Unbounded;

use  SN,
     SN.DB_Structures,
     SN.Find_Fns,
     DB_API,
     Ada.Text_IO,
     Ada.Strings.Unbounded;

procedure Find_Test is
   DB  : DB_File;
   Tab : GV_Table;
begin
   Open (DB, "db/test.gv");
   Tab := Find (DB, "a");
   Put_Line ("Found: " & Tab.Buffer (Tab.Name.First .. Tab.Name.Last) & ", "
      & Tab.Buffer (Tab.File_Name.First .. Tab.File_Name.Last));
   Tab := Find (DB, "a", Point'(1, 4));
   Put_Line ("Found: " & Tab.Buffer (Tab.Name.First .. Tab.Name.Last) & ", "
      & Tab.Buffer (Tab.File_Name.First .. Tab.File_Name.Last));
   Tab := Find (DB, "c");
   Close (DB);
end Find_Test;
