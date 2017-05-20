[comment]: <> -*- eval: (read-only-mode 1) -*-
Hack time in Emacs
==================

Hack time
---------

Hack time by typing

    M-x hack-time-mode

- Choose a date.  E.g. enter
  - "-1" for yesterday.  Time gets set to 11:55AM.
  - "-10 12:05" for 10 days ago at 12:05 PM"

After this action the current time is frozen to the chosen date.

Watch out for 'ht' in the modeline which indicates that hack-time-mode
is on.

Back to normal
--------------

Turn the mode off typing

    M-x hack-time-mode

again.

Use cases
========

- Mark Org-todo-items done at another date.
- View the Org agenda as if it was tomorrow.
- Bulk scatter (keys BS in Org agenda) items starting at a certain
  date.

Shortcomings
============

'hack-time-mode' has limitted control over time.  There are many time
sources in Emacs _not_ controlled by 'hack-time-mode'.

Watch out!

Dependencies
============

Currently 'hack-time-mode' depends on function 'org-read-date' of
Orgmode.

Vision
======

`hack-time-mode` gives the user full control about every aspect of
time in this universe and all parallel universes.
