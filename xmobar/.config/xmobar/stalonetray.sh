#!/bin/bash


width=$(xprop -name stalonetray | grep 'program specified minimum size' | cut -d ' ' -f 5)

echo "<hspace=$width/>"
