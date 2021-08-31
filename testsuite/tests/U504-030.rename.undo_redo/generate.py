import sys
import os

spec = """
package {name} is
   procedure Hello;
end {name};
"""

body = """
with Refactor;

package body {name} is

   -----------
   -- Hello --
   -----------

   procedure Hello is
   begin
      Refactor.Hello (1, 2, "Hi");
   end Hello;
end {name};
"""

def main(nb, base_name, dir_name):
    if dir_name:
        try:
            os.mkdir(dir_name)
        except FileExistsError:
            pass
    for i in range(nb):
        pack_name = base_name + "_" + str(i)
        path = os.path.join(dir_name, pack_name + ".ads")
        with open(path, 'w') as f:
            f.write(spec.format(name=pack_name))
        path = os.path.join(dir_name, pack_name + ".adb")
        with open(path, 'w') as f:
            f.write(body.format(name=pack_name))

try:
    nb = int(sys.argv[1])
    l = len(sys.argv)
    base_name = "bar"
    dir_name = "generated"

    if l == 3:
        base_name = str(sys.argv[2])
    elif l == 4:
        base_name = str(sys.argv[2])
        dir_name = str(sys.argv[3])
except Exception as E:
    print(str(E))
    print("generate.py nb [name(bar)] [dir(src)]")

if dir_name != "protected":
    main(nb, base_name, dir_name)
