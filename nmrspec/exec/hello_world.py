#!/usr/bin/env python

import os
import sys
import time
 
if len(sys.argv) != 2:
    print('Usage: ./hello_worls.py filename')
    sys.exit(1)


print('Hello World! \nMacro-command file = %s' % sys.argv[1])

# Wait for 5 seconds
time.sleep(5)

num_words = num_lines = num_chars = 0  # chain assignment
 
with open(sys.argv[1]) as infile:
    for line in infile:
        num_lines += 1
        num_chars += len(line)
        line = line.strip()
        words = line.split()
        num_words += len(words)
 
# Print results
print('Number of Lines is %d' % num_lines)
print('Number of Words is %d' % num_words)
print('Number of Characters is %d' % num_chars)
