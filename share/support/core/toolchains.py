"""
Defines the standard toolchains used by the GPS project editor.

THIS PLUGIN MUST BE LOADED.
"""

import GPS

XML = r"""<?xml version="1.0" ?>
<GPS>
  <!-- Default toolchain: any toolchain will have those values by default,
       prefixed using their name (e.g. gnatmake will be transformed to
       name-gnatmake).

       Those can be explicitly overridden in the toolchain definition, or
       nullified by using an empty value in the corresponding tag
  -->
  <toolchain_default>
    <gnat_driver>gnat</gnat_driver>
    <gnat_list>gnatls</gnat_list>
    <debugger>gdb</debugger>
    <cpp_filt>c++filt</cpp_filt>
    <compiler lang="ada">gnatmake</compiler>
    <compiler lang="c">gcc</compiler>
    <compiler lang="c++">g++</compiler>
    <compiler lang="asm">gcc</compiler>
  </toolchain_default>

  <toolchain name="aarch64-elf"/>

  <toolchain name="aarch64-linux-gnu"/>

  <toolchain name="aarch64-nto-qnx">
    <debugger>ntoaarch64-gdb</debugger>
  </toolchain>

  <toolchain name="arm-apple-darwin10"/>

  <toolchain name="arm-linux-gnueabi"/>

  <toolchain name="arm-linux-androideabi"/>

  <toolchain name="arm-wrs-vxworks">
    <debugger>arm-wrs-vxworks6-gdb</debugger>
    <cpp_filt>c++filtarm</cpp_filt>
    <compiler lang="c">ccarm</compiler>
    <compiler lang="c++">c++arm</compiler>
  </toolchain>

  <toolchain name="aarch64-wrs-vxworks7">
    <debugger/>
  </toolchain>

  <toolchain name="arm-wrs-vxworks7">
    <debugger/>
  </toolchain>

  <toolchain name="powerpc-wrs-vxworks">
    <debugger>powerpc-wrs-vxworks6-gdb</debugger>
    <cpp_filt>c++filtppc</cpp_filt>
    <compiler lang="c">ccppc</compiler>
    <compiler lang="c++">c++ppc</compiler>
  </toolchain>

  <toolchain name="powerpc-wrs-vxworksae">
    <debugger>powerpc-wrs-vxworksae-gdb</debugger>
    <cpp_filt>c++filtppc</cpp_filt>
    <compiler lang="c">ccppc</compiler>
    <compiler lang="c++">c++ppc</compiler>
  </toolchain>

  <toolchain name="powerpc-wrs-vxworksmils">
    <debugger>powerpc-elf-gdb</debugger>
    <cpp_filt>c++filtppc</cpp_filt>
    <compiler lang="c">ccppc</compiler>
    <compiler lang="c++">c++ppc</compiler>
  </toolchain>

  <toolchain name="powerpc-wrs-vxworks7">
    <debugger/>
  </toolchain>

  <toolchain name="powerpc64-wrs-vxworks7">
    <debugger/>
  </toolchain>

  <toolchain name="powerpc64-generic-linux-gnu"/>

  <toolchain name="powerpc-generic-linux-gnu"/>

  <toolchain name="powerpc-wrs-linux"/>

  <toolchain name="powerpc-sysgo-pikeos"/>
  <toolchain name="i586-sysgo-pikeos"/>

  <toolchain name="powerpc-elf"/>

  <toolchain name="powerpc-eabispe"/>

  <toolchain name="i586-wrs-vxworks">
    <debugger>i586-wrs-vxworks6-gdb</debugger>
    <cpp_filt>c++filtpentium</cpp_filt>
    <compiler lang="c">ccpentium</compiler>
    <compiler lang="c++">c++pentium</compiler>
  </toolchain>

  <toolchain name="i586-wrs-vxworks7">
    <debugger/>
  </toolchain>

  <toolchain name="i586-wrs-linux"/>

  <toolchain name="e500v2-wrs-vxworks">
    <cpp_filt>c++filtppc</cpp_filt>
    <compiler lang="c">ccppc</compiler>
    <compiler lang="c++">c++ppc</compiler>
  </toolchain>

  <toolchain name="e500v2-wrs-vxworksae">
    <cpp_filt>c++filtppc</cpp_filt>
    <compiler lang="c">ccppc</compiler>
    <compiler lang="c++">c++ppc</compiler>
  </toolchain>

  <toolchain name="e500v2-wrs-vxworksmils">
    <cpp_filt>c++filtppc</cpp_filt>
    <compiler lang="c">ccppc</compiler>
    <compiler lang="c++">c++ppc</compiler>
  </toolchain>

  <toolchain name="e500v2-wrs-vxworks7">
    <debugger/>
  </toolchain>

  <toolchain name="e500v2-wrs-linux"/>

  <toolchain name="leon-wrs-vxworks">
    <cpp_filt>c++filtsparc</cpp_filt>
    <compiler lang="c">ccsparc</compiler>
    <compiler lang="c++">c++sparc</compiler>
  </toolchain>

  <toolchain name="powerpc-elf-lynxos"/>
  <toolchain name="powerpc-elf-lynxos178e"/>

  <toolchain name="powerpc-xcoff-lynxos"/>
  <toolchain name="powerpc-xcoff-lynxos178"/>

  <toolchain name="i586-elf-lynxos178e"/>

  <toolchain name="x86_64-wrs-vxworks7">
    <debugger/>
  </toolchain>

  <toolchain name="erc32-elf"/>

  <toolchain name="leon-elf"/>
  <toolchain name="leon3-elf"/>

  <toolchain name="arm-mentor-nucleus"/>

  <toolchain name="arm-eabi"/>

  <toolchain name="arm-sysgo-pikeos"/>

  <toolchain name="avr">
    <cpp_filt/>
    <compiler lang="c++"></compiler>
    <compiler lang="asm"></compiler>
  </toolchain>

  <toolchain name="dotnet">
    <debugger/>
    <cpp_filt/>
    <compiler lang="c"/>
    <compiler lang="c++"/>
    <compiler lang="asm"/>
  </toolchain>

  <toolchain name="aamp">
    <gnat_driver>gnaampcmd</gnat_driver>
    <gnat_list>gnaampls</gnat_list>
    <debugger/>
    <cpp_filt/>
    <compiler lang="ada">gnaampmake</compiler>
    <compiler lang="c"/>
    <compiler lang="c++"/>
    <compiler lang="asm"/>
  </toolchain>

  <toolchain name="jvm">
    <debugger/>
    <cpp_filt/>
    <compiler lang="c"/>
    <compiler lang="c++"/>
    <compiler lang="asm"/>
  </toolchain>

  <toolchain name="codepeer">
    <debugger/>
    <cpp_filt/>
    <compiler lang="c"/>
    <compiler lang="c++"/>
    <compiler lang="asm"/>
  </toolchain>

  <toolchain name="riscv64-elf"/>
</GPS>
"""

GPS.parse_xml(XML)
