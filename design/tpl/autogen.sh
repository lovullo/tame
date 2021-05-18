#!/bin/sh
# Autoreconf runner

which autoreconf &>/dev/null || {
  echo "\`autoreconf' not found in PATH"
  exit 1
}

exec autoreconf -fvi

