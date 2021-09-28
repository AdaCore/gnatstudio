($GPS -P test.gpr --load=python:test.py --traceon=GPS.LSP.ADA_SUPPORT.DIAGNOSTICS --traceon=GPS.SOURCE_EDITOR.BUFFER  --traceon=GPS.MAIN.GPS 2>&1 ) | tr -d '\r' > traces.out
# The last line should not be about destroying a buffer
tail -1 traces.out > last_line.txt
echo "   [GPS.MAIN.GPS] Shutdown" > expected.txt
diff last_line.txt expected.txt
