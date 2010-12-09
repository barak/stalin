#!/usr/bin/awk -f
/^$/ && !f {print; f=1}
/./ {print; f=0}
