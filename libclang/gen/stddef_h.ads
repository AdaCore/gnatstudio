pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stddef_h is

   --  unsupported macro: NULL __null
   subtype size_t is unsigned_long;  -- /export/work/reznik/ancr/sandbox/x86_64-linux/stable-gnat/install/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h:216

end stddef_h;
