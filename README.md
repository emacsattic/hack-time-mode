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

Use case
========

Mark Org-todo-items done at another day.

Dependencies
============

Currently 'hack-time-mode' depends on Orgmode.
