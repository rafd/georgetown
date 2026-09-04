# Code Organization

Two top-level concerns: `sim/` is the game model, `server/` is our choice to
serve that game as a web app, and `client/` is the browser UI.

```
src/
  georgetown/
    core.clj                ;; entry point; starts omni, the scheduler, push

    sim/                    ;; the game model
      types.cljc            ;; malli types
      blueprints.cljc       ;; improvements, offerables, their effects
      schema.cljc           ;; db attributes; installs the malli registry
      time.cljc             ;; ticks, shifts, years
      constants.cljc        ;; simulation tuning knobs
      citizen.cljc
      biome.cljc
      debt.cljc
      market.cljc           ;; one-sided market
      terrain.clj           ;; elevation, moisture, noise-driven generation
      island.clj            ;; assembles an island from terrain + citizens
      tick.clj              ;; the simulation loop
      api.clj               ;; game tada commands + :query/islands
      util/
        math.cljc
        noise.clj

    server/                 ;; serving the game as a web app
      config.clj            ;; declare + read vars to read from config.edn
      db.clj                ;; datalevin connection, transact, query, watchers
      state.clj             ;; db reads and permission predicates
      api.clj               ;; auth tada commands
      tada.clj              ;; the command registry and exec!
      routes.clj
      push.clj
      email.clj
      log.clj
      scheduler.clj         ;; drives tick! on a chime schedule
      omni_config.clj

    client/
      core.cljs
      state.cljs            ;; the reagent app state atom
      ui/
        pages/
          *
        *

dev-src/
  georgetown/
    dev/
      seed.clj
      *
```


## Tests

Tests use rcf, inline in the namespace they are testing.

## Layering

`server/` depends on `sim/` freely. The reverse is allowed (for now),
but deliberately narrow — only two namespaces reach back:

    sim/api.clj  -> server/db, server/state
    sim/tick.clj -> server/db
