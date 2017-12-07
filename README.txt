Instructions / How to use

1) To start the program, type make repl.

2) Select a directory to sync after being prompted to do so.

3) After selection, the chosen directory will automatically be populated with a
   config directory (this directory will not be synced nor will any
   subdirectories in the root directory of the user’s choosing) with two files:
   a pubkey and a privkey. Exit the application.

4) Repeat steps 1-3 on the peer machine on the same network.

5) Add a copy of each peer machine’s pubkey to the other’s config directory
   with the name peerkey. Now, the two machines will be able to detect each
   other and sync automatically.

6) Start up the program on both machines again, and now the machines will be
   in sync with any files that are added to either directory!
