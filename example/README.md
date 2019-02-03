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

To quickly try it, run `spawn-fcgi -p 5152 /bin/env pgxhtml` in this
directory, with database connection environment variables set if
needed, an `127.0.0.1 pgxhtml-test` entry in `/etc/hosts`, and a nginx
config akin to the following:

```
server {
    listen localhost:80;
    server_name pgxhtml-test;

    location / {
      include fastcgi_params;
      fastcgi_param PATH_INFO $fastcgi_script_name;
      fastcgi_pass 127.0.0.1:5152;
    }
}
```
