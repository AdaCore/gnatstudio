# DAP types and I/O subprograms

The program `gen_json` generates the necessary types and procedures to
properly communicate on the DAP protocol.

It generates all the necessary code from a single JSON
specification file that can be found on the github repository of the DAP
protocol in the `gh-pages` branch:
https://github.com/Microsoft/debug-adapter-protocol/blob/gh-pages/debugAdapterProtocol.json

GNAT Studio uses a customized version of this `debugAdapterProtocol.json` file to allow some extensions
to the DAP protocol.

# Requirements

Have a working Ada environment, with `gnatchop` and `gnatpp` binaries (present
by default in GNAT toolchains) and `alr` available in your `PATH` (you can download Alire [here](https://alire.ada.dev/)).

Then you will need to build the `gen_json` tool from the `VSS` repository, and make it
available in your `PATH` environment varaible.
Here are the instructions to achieve that:

```shell
git clone https://github.com/AdaCore/VSS.git
cd VSS/tools/json_schema
alr build
export PATH=`pwd`:$PATH
```

# How to use

One have to be in the `script` directory and and have `gnatpp`, `gnatchop`, `gen_json` available in the `PATH`.

From there one can generate the code with a single `make` command.

It works with relative path and will put the generated code in the
`generated` directory that is `../generated` relative to `.` which is `script`.

# How to bump to a new protocol version (or produce code for a specific one)

One must download the desired JSON specification file (`debugAdapterProtocol.json`)
and put it in the `script` directory.

The live version of the specification can be found at this address:
https://raw.githubusercontent.com/microsoft/debug-adapter-protocol/gh-pages/debugAdapterProtocol.json

From there one only need to `make` and the code for the up to date version (or specific chosen one) will be generated.

# TODO

- It could be helpful to generate an interface type for each _base type_ and
  inherit base type and all descendants from this interface. This way we
  could have `base_type_interface'Class` hierarchy that includes all derived
  types.
