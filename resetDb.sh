#!/bin/bash
sqlite3 stickers.db "delete from stickers;" && echo "Database reset successfully"