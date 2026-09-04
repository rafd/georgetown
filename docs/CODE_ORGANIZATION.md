# Code Organization

Two top-level concerns: `sim/` is the game model, `server/` is our choice to
serve that game as a web app, and `client/` is the browser UI.

```
src/
  georgetown/
    core.clj                ;; entry point; starts omni, the scheduler, push

    sim/                    ;; the game model
      types.cljc            ;; malli types; the resource table
      blueprints.cljc       ;; improvements, offerables, their effects
      schema.cljc           ;; db attributes; installs the malli registry
      time.cljc             ;; ticks, shifts, years
      constants.cljc        ;; simulation tuning knobs
      citizen.cljc
      biome.cljc            ;; biome + color from elevation & moisture
      debt.cljc             ;; amortization + interest
      market.clj            ;; one-sided market
      terrain.clj           ;; elevation, moisture, noise-driven generation
      island.clj            ;; assembles an island from terrain + citizens
      tick.clj              ;; the simulation loop: markets, shift allocation,
                            ;;   maintenance, deaths, loans, taxes
      api.clj               ;; game tada commands and queries;
                            ;;   the whole game action surface
      util/
        math.cljc
        noise.clj           ;; wraps the java-src OpenSimplex2S

    server/                 ;; serving the game as a web app
      config.clj            ;; declare + read vars to read from config.edn
      db.clj                ;; datalevin connection, transact, query, watchers
      state.clj             ;; db reads and permission predicates
      api.clj               ;; auth tada commands
      tada.clj              ;; the command registry and exec!
      routes.clj            ;; /api/command, /api/state
      push.clj              ;; per island/user/player state; the long-poll
      email.clj             ;; magic-link auth email
      log.clj
      scheduler.clj         ;; drives tick! on a chime schedule
      omni_config.clj

    client/
      core.cljs
      state.cljs            ;; the reagent app state atom; exec!
      ui/
        app.cljs            ;; the routing root
        pages.cljs          ;; requires each page, so that it self-registers
        pages/
          *                 ;; one namespace per page
        map.cljs            ;; island + lot rendering
        iso.cljs            ;; isometric tiles
        common.cljs
        dataviz.cljs
        *

dev-src/                    ;; :dev profile only; REPL tooling
  georgetown/
    dev/
      core.clj              ;; enables rcf
      seed.clj              ;; populate a fresh db
      gen.clj               ;; regenerate an island's terrain
      migrations.clj        ;; one-off data migrations, run by hand
      misc.clj              ;; one-off data repairs
      *
```


## Layering

`server/` depends on `sim/` freely. The reverse is allowed (for now),
but deliberately narrow — only two namespaces reach back:

    sim/api.clj  -> server/db, server/state
    sim/tick.clj -> server/db
