#! /bin/sh

tmux new-window
tmux send-keys 'cd ..; ./dist-newstyle/build/raft-demo-0.0.1/build/local-new/local-new' C-m
sleep 10

tmux new-window

# split off 1/3, then half of the remaining 2/3
tmux send-keys 'geth --preload prelude.js attach ipc:../gdata/geth1/geth.ipc' C-m
sleep 1
tmux send-keys C-l
tmux split-window -h -p 66

tmux send-keys 'geth --preload prelude.js attach ipc:../gdata/geth2/geth.ipc' C-m
sleep 1
tmux send-keys C-l
tmux split-window -h -p 50

tmux send-keys 'geth --preload prelude.js attach ipc:../gdata/geth3/geth.ipc' C-m
sleep 1
tmux send-keys C-l
