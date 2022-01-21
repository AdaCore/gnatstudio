# What is dap2ada

The python script `dap2ada` generates the necessary types and procedures to
properly communicate on the DAP protocol.

The script generates all the necessary code from a single JSON
specification file that can be found on the github repository of the DAP
protocol in the `gh-pages` branch:
https://github.com/Microsoft/debug-adapter-protocol/blob/gh-pages/debugAdapterProtocol.json

It produces `dap-tools.ads` and `dap-tools.adb`.

Most of the basic types are taken from `lsp-types.ads`
and `lsp-generic_optional.ads`

# Requirements

Have a working ada environment (at least `gnatpp` binary) and install the python
dependencies listed in `requirements.txt`

```shell
pip3 install -r requirements.txt
```

# How to use

One have to be in the `script` directory and inside an _ancr_ shell
(or at least have `gnatpp` in the path)

From there one can generate the code with a single `make` command.

`dap2ada` works with relative path and will put the generated code in the
`generated` directory that is `../generated` relative to `.` which is `script`.

# How to bump to a new protocol version (or produce code for a specific one)

One must download the desired JSON specification file, name it `dap.json`
and put it in the `script` directory.

The live version of the specification can be found at this address:
https://raw.githubusercontent.com/microsoft/debug-adapter-protocol/gh-pages/debugAdapterProtocol.json

From there one only need to `make` and the code for the up to date version
(or specific chosen one) will be generated.

# Fix the missing optionals

## I - Solve type dependencies
- create builtin optional types (integer, bool, string) (in predefined types)
- in `create_deg_list` if `required' then add `Optional_*` type
- create a `print_optionals` that will instantiate a new package for each
  optional types
- in `print_sub_*` add the `for *'Read/Write use Read/Write_*` after the type
- in `print_body_*` add the optional code logic and use `'Read and 'Write in
  place of Read_* and Write_*`

# TODO

- Add comments in the generated code to ease use of the protocol types
- Remove `Accesses` in preference for `in / out` parameters
- Investigate possible name clashing in the way we store _anonymous_ object body
    - adding `body_` prefix to selector might cause name clashing if there
      is already a `body_*` selector in the type
    - A more elegant solution might be to create a new type that is not
      present in the spec to have a named type in Ada in place of this
      anonymous one in the specification
- Add object structure to the script ?
- Add exception into the code ?
- Remove the third party dependency on `networkx` ?
