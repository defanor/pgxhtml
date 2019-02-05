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

To quickly try the example after preparing a database, it can be
invoked directly in the `example` directory, e.g.:

```sh
echo | QUERY_STRING="t=list&q=select+bug_search('','',10,0)" pgxhtml
```

To try it with a web server, ensure that `fcgiwrap` is running (e.g.,
`fcgiwrap -s 'tcp:127.0.0.1:5152'`), database connection environment
variables are set if needed, an `127.0.0.1 pgxhtml-test` entry is in
`/etc/hosts`, and a nginx config akin to the following is set:

```
server {
    listen localhost:80;
    server_name pgxhtml-test;

    location / {
      include fastcgi_params;
      fastcgi_param SCRIPT_FILENAME /home/defanor/.cabal/bin/pgxhtml;
      fastcgi_param FCGI_CHDIR /home/defanor/proj/haskell/pgxhtml/example/;
      fastcgi_pass 127.0.0.1:5152;
    }
}
```

Then
[http://pgxhtml-test/?t=list&q=select+bug_search('','',10,0)](http://pgxhtml-test/?t=list&q=select%20bug_search(%27%27,%27%27,10,0))
should be available.
