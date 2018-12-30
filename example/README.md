# pg×html usage example

Here is an example that implements a very basic bug reporting system:
users can just report, view, and list (search) the bugs.

First of all, a database should be designed: `bugs.sql` contains
definitions and comments.

A common template, `common.xsl`, includes error handling and some
shared HTML. One can choose to show error details to users, or to hide
them.

`view.xsl` is a basic template for bug viewing.

`list.xsl` includes report and search forms, and lists the bugs.

To quickly try it, run `pgxhtml --devlogging` in this directory,
with database connection environment variables set if needed.
