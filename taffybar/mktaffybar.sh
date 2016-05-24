#! /usr/bin/bash

kill -9 $(cat "/home/wes/.cache/taffybar.pid") && rm /home/wes/.cache/taffybar/taffybar-linux-x86_64 && runghc /home/wes/.config/taffybar/taffybar.hs &
