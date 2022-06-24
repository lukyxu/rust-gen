#!/bin/bash
OUTPUT=$(rustc -A warnings ownership.rs 2>&1 >/dev/null)

USED=$(echo $OUTPUT | exec grep -o "use of moved value: \`var_4664\`" | wc -l)
ERRORS=$(echo $OUTPUT |& exec grep -o "error" |& wc -l)

if [ $USED -eq 1 ] && [ $ERRORS -eq 4 ]
then
exit 0
fi

exit 1

# |& exec grep -o "use of moved value" | wc -l