with SN.Browse;
use  SN.Browse;

procedure BwsTest is
begin
   Browse ("test.c", "sn_db_dir", "cbrowser", "../sn/bin");
   Browse ("test1.c", "sn_db_dir", "cbrowser", "../sn/bin");
   Generate_Xrefs ("sn_db_dir", "../sn/bin");
end BwsTest;
