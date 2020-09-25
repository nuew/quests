# quests
*Quests* is an interactive online roleplaying software service.

It is currently in early development.

## Building
You need a recent version of [Stack][STACK]. Once installed, you can build by
running `stack build`. Installation to your path may be done using
`stack install`.

A recent version of [PostgreSQL][POSTGRESQL] is required, and [Sentry][SENTRY]
is (optionally) used for error aggregation.

## Usage
### Modules
Modules are specified at the command line. The default module is '', the empty
string, which runs the API server.

```sh
$ stack run <module>
```

| Module  | Action                                                                   |
|---------|--------------------------------------------------------------------------|
|         | Runs the API server.                                                     |
| layout  | Displays a graphical representation of the available API endpoints.      |
| migrate | Runs a database migration on the database specified in the configuration |

### Environment Variables
Environment variables may be specified either as *actual* environment variables,
or as `KEY=VALUE` pairs in the **.env** file in the application's working
directory. The environment variables listed here are in addition to any used for
configuration by [the C library][CVARS], [the linker][LINKERVARS], or by
[Haskell itself][HASKELLVARS].

Important variables are listed **`in bold`**.

| Environment Variable | Description                                                                                                             |  Default  |
|----------------------|-------------------------------------------------------------------------------------------------------------------------|-----------|
| **`PORT`**           | Port to listen on.                                                                                                      | `3000`    |
| **`HOST`**           | [Interface to bind to.][HOST_SYNTAX]                                                                                    | `*4`      |
| `TIMEOUT`            | Timeout value in seconds.                                                                                               | `30`      |
| `FD_CACHE_LENGTH`    | Cache duration time of file descriptors in seconds. `0` means that the cache mechanism is not used.                     | `0`       |
| `STAT_CACHE_LENGTH`  | Cache duration time of file information in seconds. `0` means that the cache mechanism is not used.                     | `0`       |
| `NO_PARSE_PATH`      | Skip parsing of the HTTP path and query. This is probably a bad idea, though may be useful for writing HTTP proxies.    | `False`   |
| `SERVER_NAME`        | Default server name to be sent as the "Server:" header. If an empty string is set, the "Server:" header is not sent.    | `Warp/`?  |
| `MAX_BODY_FLUSH`     | The maximum number of bytes to flush from an unconsumed request body.                                                   | `8192`    |
| `SLOWLORIS_SIZE`     | Size in bytes read to prevent [Slowloris attacks][SLOWLORIS].                                                           | `2048`    |
| `SHUTDOWN_TIMEOUT`   | Set the graceful shutdown timeout in seconds. A timeout of `Nothing` will wait indefinitely.                            | `Nothing` |
| **`DB_CONN`**        | The [database connection string][PQ-CONNSTRING] to use in order to connect to the database.                             |           |
| `DB_MAX_CONNS`       | Maximum number of connections to keep open per stripe. The smallest acceptable value is 1.                              | `16`      |
| `DB_STRIPES`         | The number of stripes (distinct sub-pools) to maintain. The smallest acceptable value is 1.                             | `2`       |
| `DB_TIMEOUT`         | Amount of time for which an unused connection is kept open. The smallest acceptable value is 0.5 seconds.               | `10`      |
| **`SECRET_KEY`**     | The (unique!) 32-byte secret key for this installation. This is used to encrypt cookies and various tokens.             | Required  |
| **`SENTRY_DSN`**     | Old-style/deprecated [Sentry][SENTRY] DSN.                                                                              | None      |
| `SOCKET`             | The name of a Unix socket to use instead of the default TCP socket.                                                     | None      |

[CVARS]: https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html
[HASKELLVARS]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#setting-rts-options-with-the-ghcrts-environment-variable
[HOST_SYNTAX]: https://hackage.haskell.org/package/warp-3.2.12/docs/Network-Wai-Handler-Warp.html#t:HostPreference
[LINKERVARS]: https://man7.org/linux/man-pages/man8/ld.so.8.html
[POSTGRESQL]: https://www.postgresql.org/
[PQ-CONNSTRING]: https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
[SENTRY]: https://sentry.io/
[SLOWLORIS]: https://en.wikipedia.org/wiki/Slowloris_(computer_security)
[STACK]: https://docs.haskellstack.org/
