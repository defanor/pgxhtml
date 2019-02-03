# pg×html

This is a tool to make custom web interfaces to PostgreSQL databases,
using simple and standard technologies:

- SQL for querying
- XSLT for templating (translation of XML query results into XHTML)
- HTML forms for user input
- Optional HTTP basic authentication for PostgreSQL authentication

URL query parameters are available for use from XSLTs. SQL query
templates also can use those, as well as HTML form data submitted with
the POST method.

Request timeouts are enforced and do cancel DB queries, but otherwise
it relies on PostgreSQL for access permissions and security policies,
as well as for any business logic that may be needed.

FastCGI is used.


## Usage

### Invocation

The used environment variables are:

- `TIMEOUT`: request timeout in seconds, 10 by default.
- `XSLT_DIR`: a directory to read XSLT files from, current working
  directory by default.

Regular [libpq environment
variables](https://www.postgresql.org/docs/current/libpq-envars.html)
are used for database connections.

### Templating

URL query parameters are made visible to XSLTs as `xsl:param`
parameters. The documents they get applied to are either the results
of SQL queries (which are expected to return a single XML document,
using `query_to_xml` or similar functions), or error documents (which
contain error details) in case of an error.

The XSLTs are taken from `XSLT_DIR`, using file name from the URL
query, with its extension changed to `xsl`.

### Querying

SQL queries provided in the `q` URL query parameter get executed, with
some substitutions to handle HTML forms:

- `f:<name>` for POST parameters
- `q:<name>` for GET parameters
- `:fields` for POST parameter names
- `:values` for POST parameter values (in the same order as the names)

`:fields` and `:values` are unnecessary, but are provided for
convenience of insert/upsert operations.

SQL queries get tokenized by splitting into words and reassembled
afterwards, hence some whitespace separation is needed.

### Authentication

Presence of `authorised` in the URL path requires HTTP basic
authentication, and the provided credentials are used directly for
PostgreSQL authentication.

### Web server

This is intended to be used with an HTTP server, which would take care
of encryption, compression, static files, redirects, and so on, while
pgxhtml only focuses on providing a web interface to a database.

### Security checklist

- Regular privileges (see
  [GRANT](https://www.postgresql.org/docs/current/sql-grant.html))
  should be set appropriately.

- [Row Security
  Policies](https://www.postgresql.org/docs/current/ddl-rowsecurity.html)
  should be defined and enabled.

- [Constraints](https://www.postgresql.org/docs/current/ddl-constraints.html)
  are useful for data validation.

- [REVOKE CREATE ON SCHEMA public FROM
  PUBLIC](https://www.postgresql.org/docs/current/ddl-schemas.html)
  would likely be desired (perhaps granting `CREATE` just to the
  database owner/administrators).

- Possibly web server rules (such as nginx rewrites), if it's desired
  to strictly limit available queries.

## See also

- [PostgREST](http://postgrest.org/), "a standalone web server that
  turns your PostgreSQL database directly into a RESTful API".

- [automatic-api](https://github.com/dbohdan/automatic-api/), a list
  of tools for automatic HTTP-based API generation.
