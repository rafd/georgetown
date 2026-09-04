# Development

For the file and folder layout, see [CODE_ORGANIZATION.md](CODE_ORGANIZATION.md).

## Stack

- Backend: Clojure, [datalevin](https://github.com/juji-io/datalevin)
  (datomic-like graph db)
- Frontend: ClojureScript, reagent
- [omni](https://github.com/bloomventures/omni) — SPA framework wiring stuff together
- [tada](https://github.com/rafd/tada) — declarative queries and commands for the abstract 'system'

## Getting started

Install java and clojure, then clone the repo.

Create a `config.edn` at the repo root (gitignored):

```
{:http-port 9365
 :environment :dev
 :db-dir "data/datalevin"
 :website-base-url "http://localhost:9365"
 :auth-cookie-secret "0123456789123456"
 :auth-token-secret "0123456789123456"}
```

```
lein repl
(start!)      ;; from georgetown.core; boots omni (http server + cljs build)
```

`georgetown.dev.seed/seed!` (or `seed-plus!`) populates a fresh db with an
island.

## Tests

Tests use `hyperfiddle.rcf` , inline with the functions they test.

## Build

`lein uberjar`

## The core loop

1. `server/scheduler.clj` ticks each island once per second (`chime`).
2. On any transaction, `db/watchers` fire and `push/on-db-change!` recomputes
   the per island, user and player state, then sends diffs to the connected
   clients.
3. The client long-polls `GET /api/state` (`push/handler`) and mirrors the
   result into the atoms in `client/state.cljs`.
4. User actions call `state/exec!`, which POSTs to `/api/command`;
   `routes/dispatch-event!` runs the matching tada command.

