# Create our fake executables
gcc t.c -o t
for exec_name in git.exe gnatcov.exe gprbuild.exe ; do
   cp t.exe $exec_name
done

$GNATSTUDIO --load=python:test.py

# if there are any files called "*.was_called", raise an error
files=$(ls *.was_called 2>/dev/null)
if [ -n "$files" ]; then
    echo "Error: some execs have been found in the current directory:"
    for f in $files; do
        echo "  $f"
    done
    exit 1
fi