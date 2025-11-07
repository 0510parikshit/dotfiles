#!/bin/bash

artist=$(playerctl -p spotify metadata artist)
title=$(playerctl -p spotify metadata title)

if [ $(playerctl -p spotify -l) = "spotify" ]; then
  if [ ${#title} -gt 10 ]; then
    title=${title:0:10}..
  else
    title=$title
  fi
  if [ ${#artist} -gt 10 ]; then
    artist=${artist:0:10}..
  else
    artist=$artist
  fi
else
  echo ""
fi

if [ $(playerctl -p spotify status) = "Paused" ]; then
  echo "󰏤 $artist · $title |"
else
  echo "$artist · $title |"
fi
