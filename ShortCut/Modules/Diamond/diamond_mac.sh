#!/usr/bin/env bash
diamond version 2>&1 | head -n1 | cut -d'|' -f1
